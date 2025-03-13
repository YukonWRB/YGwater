discDataUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(
      HTML(
        "// Make the ...preparing download... notification stand out more
        .shiny-notification {
          font-size: 24px;
          font-weight: bold;
          background-color: #f9f9f9;  /* Light grey background */
          color: #333;  /* Dark grey text */
          padding: 15px;  /* Larger padding for more space */
          border-left: 5px solid #007BFF;  /* Blue left border */
          border-right: 5px solid #007BFF;  /* Blue right border */
          border-top: 5px solid #007BFF;  /* Blue top border */
          border-bottom: 5px solid #007BFF;  /* Blue bottom border */
          border-radius: 10px;  /* Rounded corners */
        }"
      )
    ),
    tags$script(
      HTML("
      // Handles tooltip updates outside of the datatable, binds tooltip properties to elements
      Shiny.addCustomMessageHandler('update-tooltip', function(message) {
        var selector = '#' + message.id;
        $(selector).attr('title', message.title)
        .tooltip('fixTitle').tooltip('hide');
      });
      
      // Handles tootip creation and update for the datatable headers
      $(document).ready(function() {
      // Initialize tooltips
      $('body').tooltip({
        selector: '[data-toggle=\"tooltip\"]',
        container: 'body'
      });
      // Reinitialize tooltips on table redraw
      $('#tbl').on('draw.dt', function() {
        $('.tooltip').remove();
        $('body').tooltip({
          selector: '[data-toggle=\"tooltip\"]',
          container: 'body'
        });
      });
    });"
      )
    ),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("sidebar")) # UI is rendered in the server function below so that it can use database information as well as language selections.
      ),
      mainPanel(
        uiOutput(ns("main"))
      )
    )
  )
}

discData <- function(id, language) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements within the server code
    
    # Functions and data fetch ################
    # Adjust multiple selection based on if 'all' is selected
    observeFilterInput <- function(inputId) {
      observeEvent(input[[inputId]], {
        # Check if 'all' is selected and adjust accordingly
        if (length(input[[inputId]]) > 1) { # If 'all' was selected last, remove all other selections
          if (input[[inputId]][length(input[[inputId]])] == "all") {
            updateSelectizeInput(session, inputId, selected = "all")
          } else if ("all" %in% input[[inputId]]) { # If 'all' is already selected and another option is selected, remove 'all'
            updateSelectizeInput(session, inputId, selected = input[[inputId]][length(input[[inputId]])])
          }
        }
      })
    }
    
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    moduleData <- reactiveValues(
      locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT loc.location_id, loc.name, loc.name_fr FROM locations AS loc INNER JOIN samples ON loc.location_id = samples.location_id ORDER BY loc.name ASC"),
      params = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id ORDER BY p.param_name ASC;"),
      media_types = DBI::dbGetQuery(session$userData$AquaCache,
                                    "SELECT DISTINCT m.* FROM media_types as m WHERE EXISTS (SELECT 1 FROM samples AS s WHERE m.media_id = s.media_id);"),
      parameter_relationships = DBI::dbGetQuery(session$userData$AquaCache,
                                                "SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id) ;"),
      range = DBI::dbGetQuery(session$userData$AquaCache, "SELECT MIN(datetime) AS min_date, MAX(datetime) AS max_date FROM samples;"),
      sample_types = DBI::dbGetQuery(session$userData$AquaCache, "SELECT st.sample_type_id, st.sample_type, COALESCE(st.sample_type_fr, st.sample_type) AS sample_type_fr FROM sample_types AS st WHERE EXISTS (SELECT 1 FROM samples AS s WHERE st.sample_type_id = s.sample_type);"),
      samples = DBI::dbGetQuery(session$userData$AquaCache, "SELECT sample_id, location_id, media_id, datetime, sample_type FROM samples;")
    )
    
    moduleData$locations_projects <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM locations_projects WHERE location_id IN (", paste(moduleData$locs$location_id, collapse = ", "), ");"))
    if (nrow(moduleData$locations_projects) > 0) {
      moduleData$projects <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT * FROM projects WHERE location_id IN (", paste(moduleData$locations_projects$project_id, collapse = ", "), ");"))
    } else {
      moduleData$locations_projects <- data.frame(location_id = numeric(), project_id = numeric())
      moduleData$projects <- data.frame(project_id = numeric(), name = character(), ame_fr = character())
    }
    
    moduleData$locations_networks <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM locations_networks WHERE location_id IN (", paste(moduleData$locs$location_id, collapse = ", "), ");"))
    if (nrow(moduleData$locations_networks) > 0) {
      moduleData$networks <-  DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT * FROM networks WHERE network_id IN (", paste(moduleData$locations_networks$network_id, collapse = ", "), ");"))
    }
    
    if (any(!is.na(moduleData$parameter_relationships$group_id))) {
      groups <- moduleData$parameter_relationships$group_id[!is.na(moduleData$parameter_relationships$group_id)]
      moduleData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache,
                                                 paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(groups, collapse = ", "), ");"))
    } else {
      moduleData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
    }
    if (any(!is.na(moduleData$parameter_relationships$subgroup_id))) {
      sub_groups <- moduleData$parameter_relationships$group_id[!is.na(moduleData$parameter_relationships$sub_group_id)]
    moduleData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache,
                                       paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
    } else {
      moduleData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
    }
    
    
    
    # Create UI elements ################
    # NOTE: output$sidebar is rendered at module load time, but also re-rendered whenever a change to the language is made or the reset button is pressed. If the user had selected a location, for example, and then changes the language, the location selection will be reset.
    output$sidebar <- renderUI({
      tagList(
        # start and end datetime
        dateRangeInput(ns("date_range"),
                       tr("date_range_select", language$language),
                       start = as.Date(moduleData$range$min_date),
                       end = as.Date(moduleData$range$max_date),
                       max = Sys.Date() + 1,
                       format = "yyyy-mm-dd"
                       ),
        # Selectize input for locations
        selectizeInput(ns("locations"),
                       label = tr("loc(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$locs$location_id),
                                         c(tr("all", language$language), moduleData$locs[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
                       ),
        # Selectize input for media type
        selectizeInput(ns("mType"),
                       label = tr("media_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$media_types$media_id),
                                         c(tr("all", language$language), moduleData$media_types[, tr("media_type_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
                       ),
        # Selectize input for sample types
        selectizeInput(ns("sample_types"),
                       label = tr("sample_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$sample_types$sample_type_id),
                                         c(tr("all", language$language), moduleData$sample_types[, tr("sample_type_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for networks
        selectizeInput(ns("networks"),
                       label = tr("network(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$networks$network_id),
                                         c(tr("all", language$language), moduleData$networks[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for projects
        selectizeInput(ns("projects"),
                       label = tr("project(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$projects$project_id),
                                         c(tr("all", language$language), moduleData$projects[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for parameter groups
        selectizeInput(ns("pGrps"),
                       label = tr("param_group(s)", language$language),
                       choices = stats::setNames(c("all", moduleData$param_groups$group_id),
                                                 c(tr("all", language$language), moduleData$param_groups[, tr("param_group_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # parameter sub-group
        selectizeInput(ns("pSubGrps"),
                       label = tr("param_sub_group(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$param_sub_groups$sub_group_id),
                                         c(tr("all", language$language), moduleData$param_sub_groups[, tr("param_sub_group_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for parameters
        selectizeInput(ns("params"),
                       label = tr("parameter(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$params$parameter_id),
                                         c(tr("all", language$language), moduleData$params[, tr("param_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        
        actionButton(ns("reset"),
                     label = tr("reset", language$language)
        )
      ) # End of tagList
    })  %>% # End of renderUI for sidebar
      bindEvent(language$language, input$reset)

    
    output$main <- renderUI({
      tagList(
        htmlOutput(ns("instructions")),
        tags$div(style = "height: 10px;"),
        DT::dataTableOutput(ns("tbl")), # Table with sample data, filtered by the sidebar inputs
        actionButton(ns("view_data"), "View Data"),  # Button will be hidden until a row is selected
        # The modal UI elements are created lower down
      ) # End of tagList
    }) %>% # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or moduleData changes
    
    output$instructions <- renderUI(translations[id == "view_data_instructions", get(language$language)][[1]])
    
    

    
    # Functional parts of server ################
    ## Run observeFilterInput for each selectize input where 'all' is an option ##### 
    observeFilterInput("locations")
    observeFilterInput("mType")
    observeFilterInput("pGrps")
    observeFilterInput("pSubGrps")
    observeFilterInput("params")
    observeFilterInput("networks")
    observeFilterInput("projects")
    observeFilterInput("sample_types")
    
    
    ## Cross-filtering of inputs based on the sidebar selections #####

    

    ## Create the datatable ################
    table_data <- reactive({
      
    })
    
  }) # End moduleServer
} # End discPlot function
