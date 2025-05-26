contDataUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(
      HTML(
        "/* Make the ...preparing download... notification stand out more */
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
    page_sidebar(
      sidebar = sidebar(
        title = NULL,
        width = 350,
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),
        uiOutput(ns("sidebar")) # UI is rendered in the server function below so that it can use database information as well as language selections.
      ),
      uiOutput(ns("main"))
    )
  )
}

contData <- function(id, language) {
  
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
      sub_locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT sl.sub_location_id, sl.sub_location_name, sl.sub_location_name_fr FROM sub_locations AS sl INNER JOIN locations ON sl.location_id = locations.location_id ORDER BY sl.sub_location_name ASC"),
      params = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id ORDER BY p.param_name ASC;"),
      media = DBI::dbGetQuery(session$userData$AquaCache,
                                    "SELECT DISTINCT m.* FROM media_types as m WHERE EXISTS (SELECT 1 FROM samples AS s WHERE m.media_id = s.media_id);"),
      aggregation_type_id = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT aggregation_type_id FROM timeseries;"),
      parameter_relationships = DBI::dbGetQuery(session$userData$AquaCache,
                                                "SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id) ;"),
      range = DBI::dbGetQuery(session$userData$AquaCache, "SELECT MIN(start_datetime) AS min_datetime, MAX(end_datetime) AS max_datetime FROM timeseries;"),
      timeseries = DBI::dbGetQuery(session$userData$AquaCache, "SELECT timeseries_id, location_id, sub_location_id, media_id, parameter_id, aggregation_type_id, record_rate, z, start_datetime, end_datetime FROM timeseries;")
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
    if (any(!is.na(moduleData$parameter_relationships$sub_group_id))) {
      sub_groups <- moduleData$parameter_relationships$sub_group_id[!is.na(moduleData$parameter_relationships$sub_group_id)]
      moduleData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache,
                                                     paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
    } else {
      moduleData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
    }
    
    # Create a function to create the filteredData reactiveValues object
    createFilteredData <- function() {
      data <-  reactiveValues(
        locs = isolate(moduleData$locs),
        sub_locs = isolate(moduleData$sub_locs),
        params = isolate(moduleData$params),
        media = isolate(moduleData$media),
        aggregation_type_ids = isolate(moduleData$aggregation_type_id),
        parameter_relationships = isolate(moduleData$parameter_relationships),
        range = isolate(moduleData$range),
        timeseries = isolate(moduleData$timeseries),
        projects = isolate(moduleData$projects),
        networks = isolate(moduleData$networks),
        locations_networks = isolate(moduleData$locations_networks),
        locations_projects = isolate(moduleData$locations_projects),
        param_groups = isolate(moduleData$param_groups),
        param_sub_groups = isolate(moduleData$param_sub_groups)
      )
      return(data)
    }
    
    filteredData <- createFilteredData()
    
    
    # Create UI elements and necessary helpers ################
    # NOTE: output$sidebar is rendered at module load time, but also re-rendered whenever a change to the language is made.
    
    # Reactive values to store the input values so that inputs can be reset if the user ends up narrowing their selections to 0 samples
    input_values <- reactiveValues(date_range = input$date_range, 
                                   locations = input$locations, 
                                   sub_locations = input$sub_locations,
                                   z = input$z,
                                   period = input$period,
                                   rate = input$rate,
                                   media = input$media, 
                                   params = input$params)
    
    # flags to prevent running observers when the sidebar is first rendered
    render_flags <- reactiveValues(date_range = FALSE,
                                   locations = FALSE,
                                   sub_locations = FALSE,
                                   z = FALSE,
                                   period = FALSE,
                                   rate = FALSE,
                                   media = FALSE,
                                   params = FALSE)
    
    output$sidebar <- renderUI({
      req(moduleData)
      
      render_flags$date_range <- TRUE
      render_flags$locations <- TRUE
      render_flags$sub_locations <- TRUE
      render_flags$z <- TRUE
      render_flags$period <- TRUE
      render_flags$rate <- TRUE
      render_flags$media <- TRUE
      render_flags$sample_types <- TRUE
      render_flags$params <- TRUE
      
      
      tagList(
        # start and end datetime
        dateRangeInput(ns("date_range"),
                       tr("date_range_select", language$language),
                       start = as.Date(moduleData$range$min_date),
                       end = as.Date(moduleData$range$max_date),
                       min = as.Date(moduleData$range$min_date),
                       max = as.Date(moduleData$range$max_date),
                       format = "yyyy-mm-dd"
        ),
        # Selectize input for locations
        selectizeInput(ns("locations"),
                       label = tr("loc(s)", language$language),
                       choices =  stats::setNames(c("all", moduleData$locs$location_id),
                                         c(tr("all", language$language), moduleData$locs[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Modal to let users filter locations by network or project
        actionButton(ns("loc_modal"),
                     label = tr("loc_modal", language$language),
                     width = "100%",
                     style = "font-size: 14px; margin-top: 5px;"
        ),
        selectizeInput(ns("sub_locations"),
                       label = tr("sub_loc(s)", language$language),
                       choices = stats::setNames(c("all", moduleData$sub_locs$sub_location_id),
                                         c(tr("all", language$language), moduleData$sub_locs[, tr("sub_location_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        selectizeInput(ns("z"), 
                       label = tr("z", language$language),
                       choices = stats::setNames(c("all", moduleData$sample_types$z),
                                                 c(tr("all", language$language), moduleData$sample_types$z)),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for media type
        selectizeInput(ns("media"),
                       label = tr("media_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$media$media_id),
                                         c(tr("all", language$language), moduleData$media[, tr("media_type_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for period types
        selectizeInput(ns("period"),
                       label = tr("sample_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$sample_types$sample_type_id),
                                         c(tr("all", language$language), moduleData$sample_types[, tr("sample_type_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for record rate
        selectizeInput(ns("rate"),
                       label = tr("rate", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$sample_types$rate),
                                         c(tr("all", language$language), moduleData$sample_types[, tr("rate_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        selectizeInput(ns("params"),
                       label = tr("parameter(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$params$parameter_id),
                                         c(tr("all", language$language), moduleData$params[, tr("param_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Modal button to let users filter parameters by group or sub-group
        actionButton(ns("param_modal"),
                     label = tr("param_modal", language$language),
                     width = "100%",
                     style = "font-size: 14px; margin-top: 5px;"
        ),
        # Divider
        tags$div(style = "height: 15px;"),
        
        # Side-by-side buttons
        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(ns("reset"),
                       label = tr("reset_filters", language$language),
                       width = "100%",
                       style = "font-size: 14px;"
          ),
          actionButton(ns("filter"),
                       label = tr("view_samples", language$language),
                       width = "100%",
                       style = "font-size: 14px;"
          )
        )
      ) # End of tagList
    })  %>% # End of renderUI for sidebar
      bindEvent(language$language)
    
    
    
    output$main <- renderUI({
      tagList(
        tags$p(HTML(tr("view_data_instructions", language$language))),
        tags$div(style = "height: 10px;"),
        DT::DTOutput(ns("tbl")), # Table with sample data, filtered by the sidebar inputs
        actionButton(ns("select_all"), tr("select_all", language$language), style = "display: none;"),  # Button will be hidden until a row is selected
        actionButton(ns("view_data"), tr("view_data1", language$language), style =  "display: none;"),  # Button will be hidden until a row is selected
        # The modal UI elements are created lower down
      ) # End of tagList
    }) %>% # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or changes
    
    
    # Functional parts of server ################
    ## Run observeFilterInput for each selectize input where 'all' is an option ##### 
    observeFilterInput("locations")
    observeFilterInput("sub_locations")
    observeFilterInput("media")
    observeFilterInput("pGrps")
    observeFilterInput("pSubGrps")
    observeFilterInput("params")
    observeFilterInput("networks")
    observeFilterInput("projects")
    

    # Flags to prevent running observers unecessarily when the 'reset' button is pressed
    reset_flags <- reactiveValues(date_range = FALSE,
                                  locations = FALSE,
                                  sub_locations = FALSE,
                                  media = FALSE,
                                  
                                  params = FALSE)
  }) # End moduleServer
} # End contData function
