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
        selector: '[data-bs-toggle=\"tooltip\"]',
        container: 'body'
      });
      // Reinitialize tooltips on table redraw
      $('#tbl').on('draw.dt', function() {
        $('.tooltip').remove();
        $('body').tooltip({
          selector: '[data-bs-toggle=\"tooltip\"]',
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
    
    filteredData <- reactiveValues(
      locs = isolate(moduleData$locs),
      params = isolate(moduleData$params),
      media_types = isolate(moduleData$media_types),
      parameter_relationships = isolate(moduleData$parameter_relationships),
      range = isolate(moduleData$range),
      sample_types = isolate(moduleData$sample_types),
      samples = isolate(moduleData$samples),
      projects = isolate(moduleData$projects),
      networks = isolate(moduleData$networks),
      param_groups = isolate(moduleData$param_groups),
      param_sub_groups = isolate(moduleData$param_sub_groups)
    )
    
    
    # Create UI elements ################
    # NOTE: output$sidebar is rendered at module load time, but also re-rendered whenever a change to the language is made or the reset button is pressed. If the user had selected a location, for example, and then changes the language, the location selection will be reset.
    output$sidebar <- renderUI({
      req(moduleData)
      print("rendering UI sidebar")
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
      bindEvent(language$language)
    
    # flags to prevent running observeEvents when the reset button is pressed
    reset_triggered_params <- reactiveVal(FALSE)
    reset_triggered_other <- reactiveVal(FALSE)
    
    observeEvent(input$reset, {
      # Reset the filteredData to its original state
      filteredData <- reactiveValues(
        locs = isolate(moduleData$locs),
        params = isolate(moduleData$params),
        media_types = isolate(moduleData$media_types),
        parameter_relationships = isolate(moduleData$parameter_relationships),
        range = isolate(moduleData$range),
        sample_types = isolate(moduleData$sample_types),
        samples = isolate(moduleData$samples),
        projects = isolate(moduleData$projects),
        networks = isolate(moduleData$networks),
        param_groups = isolate(moduleData$param_groups),
        param_sub_groups = isolate(moduleData$param_sub_groups)
      )
      
      
      updateDateRangeInput(session, "date_range",
                           start = as.Date(moduleData$range$min_date),
                           end = as.Date(moduleData$range$max_date),
                           max = Sys.Date() + 1
      )
      updateSelectizeInput(session, "locations",
                           choices = stats::setNames(c("all", moduleData$locs$location_id),
                                                     c(tr("all", language$language), moduleData$locs[, tr("generic_name_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "mType",
                           choices = stats::setNames(c("all", moduleData$media_types$media_id),
                                                     c(tr("all", language$language), moduleData$media_types[, tr("media_type_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", moduleData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), moduleData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "networks",
                           choices = stats::setNames(c("all", moduleData$networks$network_id),
                                                     c(tr("all", language$language), moduleData$networks[, tr("generic_name_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "projects",
                           choices = stats::setNames(c("all", moduleData$projects$project_id),
                                                     c(tr("all", language$language), moduleData$projects[, tr("generic_name_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "pGrps",
                           choices = stats::setNames(c("all", moduleData$param_groups$group_id),
                                                     c(tr("all", language$language), moduleData$param_groups[, tr("param_group_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "pSubGrps",
                           choices = stats::setNames(c("all", moduleData$param_sub_groups$sub_group_id),
                                                     c(tr("all", language$language), moduleData$param_sub_groups[, tr("param_sub_group_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", moduleData$params$parameter_id),
                                                     c(tr("all", language$language), moduleData$params[, tr("param_name_col", language$language)])),
                           selected = "all")
      reset_triggered_params(TRUE)
      reset_triggered_other(TRUE)
    })
    
    output$main <- renderUI({
      tagList(
        htmlOutput(ns("instructions")),
        tags$div(style = "height: 10px;"),
        DT::dataTableOutput(ns("tbl")), # Table with sample data, filtered by the sidebar inputs
        actionButton(ns("view_data"), tr("view_data", language$language)),  # Button will be hidden until a row is selected
        # The modal UI elements are created lower down
      ) # End of tagList
    }) %>% # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or changes
    
    output$instructions <- renderUI(tr("view_data_instructions", language$language))
    
    
    
    
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
    
    
    
    # Filter available parameters, parameter groups, and parameter sub-groups based on the selected parameters. Only parameters are used to filter the samples later on.
    observeEvent(list(input$pGrps, input$pSubGrps, input$params), {
      if (reset_triggered_params()) {
        reset_triggered_params(FALSE)
        return()
      }
      req(input$pGrps, input$pSubGrps, input$params, filteredData$param_groups, filteredData$param_sub_groups, filteredData$params)
      print("Filtering parameters")
      # Filter parameters based on selected groups and sub-groups
      if (!("all" %in% input$pGrps)) {
        remain_params <- moduleData$parameter_relationships[moduleData$parameter_relationships$group_id %in% input$pGrps, ]
      } else {
        remain_params <- moduleData$parameter_relationships
      }
      if (!("all" %in% input$pSubGrps)) {
        remain_params <- moduleData$parameter_relationships[moduleData$parameter_relationships$subgroup_id %in% input$pSubGrps, ]
      }
      # Filter parameters based on selected parameters
      if (!("all" %in% input$params)) {
        remain_params <- moduleData$parameter_relationships[moduleData$parameter_relationships$parameter_id %in% input$params, ]
      }
      
      filteredData$param_groups <- moduleData$param_groups[moduleData$param_groups$group_id %in% remain_params$group_id, ]
      filteredData$param_sub_groups <- moduleData$param_sub_groups[moduleData$param_sub_groups$sub_group_id %in% remain_params$subgroup_id, ]
      filteredData$params <- moduleData$params[moduleData$params$parameter_id %in% remain_params$parameter_id, ]
      print(filteredData$params)
      print(filteredData$param_groups)
      print(filteredData$param_sub_groups)
      
      # Update the three selectize inputs
      # Keep the user's selections if they're still available; if not, set to 'all'
      pGrps_sel <- filteredData$param_groups[filteredData$param_groups$group_id %in% input$pGrps, "group_id"]
      updateSelectizeInput(session, "pGrps", 
                           choices = stats::setNames(c("all", filteredData$param_groups$group_id),
                                                     c(tr("all", language$language), filteredData$param_groups[filteredData$param_groups$group_id %in% remain_params$group_id, tr("param_group_col", language$language)])),
                           selected = if (length(pGrps_sel) > 0) pGrps_sel else "all"
      )
      pSubGrps_sel <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% input$pSubGrps, "sub_group_id"]
      updateSelectizeInput(session, "pSubGrps", 
                           choices = stats::setNames(c("all", filteredData$param_sub_groups$sub_group_id),
                                                     c(tr("all", language$language), filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% remain_params$subgroup_id, tr("param_sub_group_col", language$language)])),
                           selected = if (length(pSubGrps_sel) > 0) pSubGrps_sel else "all"
      )
      params_sel <- filteredData$params[filteredData$params$parameter_id %in% input$params, "parameter_id"]
      updateSelectizeInput(session, "params", 
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[filteredData$params$parameter_id %in% remain_params$parameter_id, tr("param_name_col", language$language)])),
                           selected = if (length(params_sel) > 0) params_sel else "all"
      )
    }, ignoreInit = TRUE)
    
    
    
    # Observe every filter input and recalculate/update filteredData
    observeEvent(list(input$date_range, input$locations, input$mType, input$sample_types, input$projects, input$networks, input$params), {
      if (reset_triggered_other()) {
        reset_triggered_other(FALSE)
        return()
      }
      req(moduleData$samples, filteredData, input$date_range, input$locations, input$mType, input$sample_types, input$projects, input$networks, input$params)
      print("Filtering filteredData")
      
      df <- moduleData$samples
      
      # Filter by date range
      if (!is.null(input$date_range)) {
        df <- df[df$datetime >= input$date_range[1] & df$datetime <= input$date_range[2], ]
      }
      # Filter by locations if "all" isn't selected
      if (!("all" %in% input$locations)) {
        df <- df[df$location_id %in% input$locations, ]
      }
      # Filter by media type if "all" isn't selected
      if (!("all" %in% input$mType)) {
        df <- df[df$media_id %in% input$mType, ]
      }
      # Filter by sample type if "all" isn't selected
      if (!("all" %in% input$sample_types)) {
        df <- df[df$sample_type %in% input$sample_types, ]
      }
      
      # Filter by parameters. parameters are already filtered by parameter groups and sub-groups in a separate observeEvent, so we can just use that filteredData.
      samples_param_filter <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT sample_id FROM results WHERE parameter_id IN (", paste(filteredData$params$parameter_id, collapse = ", "), ") AND sample_id IN (", paste0(df$sample_id, collapse = ", "), ");"))
      df <- df[df$sample_id %in% samples_param_filter$sample_id, ]
      
      # tricky: filter by networks and projects. This requires joining the 'locations_networks' and 'locations_projects' tables with the 'locations' table to get the location_ids associated with the selected networks and projects.
      if (!("all" %in% input$networks)) {
        locs <- moduleData$locations_networks[moduleData$locations_networks$network_id %in% input$networks, "location_id"]
        df <- df[df$location_id %in% locs, ]
      }
      if (!("all" %in% input$projects)) {
        locs <- moduleData$locations_projects[moduleData$locations_projects$project_id %in% input$projects, "location_id"]
        df <- df[df$location_id %in% locs, ]
      }
      # Find the remaining projects + locations associated with these locations and update the filteredData reactiveValues for both
      remain_projects <- moduleData$locations_projects[moduleData$locations_projects$location_id %in% df$location_id, ]
      remain_networks <- moduleData$locations_networks[moduleData$locations_networks$location_id %in% df$location_id, ]
      
      filteredData$samples <- df
      filteredData$locs <- moduleData$locs[moduleData$locs$location_id %in% df$location_id, ]
      filteredData$media_types <- moduleData$media_types[moduleData$media_types$media_id %in% df$media_id, ]
      filteredData$sample_types <- moduleData$sample_types[moduleData$sample_types$sample_type_id %in% df$sample_type, ]
      # filteredData$params is set in the observeEvent for parameters, parameter groups, and parameter sub-groups
      # filteredData$param_groups is set in the observeEvent for parameters, parameter groups, and parameter sub-groups
      # filteredData$param_sub_groups is set in the observeEvent for parameters, parameter groups, and parameter sub-groups
      # parameter_relationships, locations_projects, locations_networks are not necessary to update as they're not used directly anywhere
      filteredData$projects <- moduleData$projects[moduleData$projects$project_id %in% remain_projects$project_id, ]
      filteredData$networks <- moduleData$networks[moduleData$networks$network_id %in% remain_networks$network_id, ]
      
    })
    
    
    observeEvent(filteredData$locs, {
      req(filteredData$locs)
      print("Updating locations")
      locs_sel <- filteredData$locs[filteredData$locs$location_id %in% input$locations, "location_id"]
      
      updateSelectizeInput(session, "locations", 
                           choices = stats::setNames(c("all", filteredData$locs$location_id),
                                                     c(tr("all", language$language), filteredData$locs[, tr("generic_name_col", language$language)])),
                           selected = if (length(locs_sel) > 0) locs_sel else "all"
      )
    }, ignoreInit = TRUE)
    observeEvent(filteredData$media_types, {
      req(filteredData$media_types)
      print("Updating media types")
      media_sel <- filteredData$media_types[filteredData$media_types$media_id %in% input$mType, "media_id"]
      updateSelectizeInput(session, "mType", 
                           choices = stats::setNames(c("all", filteredData$media_types$media_id),
                                                                       c(tr("all", language$language), filteredData$media_types[, tr("media_type_col", language$language)])),
                           selected = if (length(media_sel) > 0) media_sel else "all"
      )
    }, ignoreInit = TRUE)
    observeEvent(filteredData$sample_types, {
      req(filteredData$sample_types)
      types_sel <- filteredData$sample_types[filteredData$sample_types$sample_type_id %in% input$sample_types, "sample_type_id"]
      updateSelectizeInput(session, "sample_types", 
                           choices = stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                                                              c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = if (length(types_sel) > 0) types_sel else "all"
      )
    }, ignoreInit = TRUE)
    observeEvent(filteredData$projects, {
      req(filteredData$projects)
      proj_sel <- filteredData$projects[filteredData$projects$project_id %in% input$projects, "project_id"]
      updateSelectizeInput(session, "projects", choices = stats::setNames(c("all", filteredData$projects$project_id),
                                                                          c(tr("all", language$language), filteredData$projects[, tr("generic_name_col", language$language)])),
                           selected = if (length(proj_sel) > 0) proj_sel else "all"
      )
    }, ignoreInit = TRUE)
    observeEvent(filteredData$networks, {
      req(filteredData$networks)
      net_sel <- filteredData$networks[filteredData$networks$network_id %in% input$networks, "network_id"]
      updateSelectizeInput(session, "networks", choices = stats::setNames(c("all", filteredData$networks$network_id),
                                                                          c(tr("all", language$language), filteredData$networks[, tr("generic_name_col", language$language)])),
                           selected = if (length(net_sel) > 0) net_sel else "all"
      )
    }, ignoreInit = TRUE)
    
    
    
    
    
    
    
    
    ## Create the datatable ################
    table_data <- reactive({
      
    })
    
  }) # End moduleServer
} # End discPlot function
