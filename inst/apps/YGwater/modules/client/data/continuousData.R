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
      locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT loc.location_id, loc.name, loc.name_fr FROM locations AS loc INNER JOIN timeseries ON loc.location_id = timeseries.location_id ORDER BY loc.name ASC"),
      sub_locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT sub_location_id, sub_location_name, sub_location_name_fr FROM sub_locations WHERE location_id IN (SELECT DISTINCT location_id FROM timeseries) ORDER BY sub_location_name ASC;"),
      params = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT parameter_id, param_name, COALESCE(param_name_fr, param_name) AS param_name_fr, unit_default AS unit FROM parameters WHERE parameter_id IN (SELECT DISTINCT parameter_id FROM timeseries) ORDER BY param_name ASC;"),
      media = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT m.* FROM media_types as m WHERE EXISTS (SELECT 1 FROM timeseries AS t WHERE m.media_id = t.media_id);"),
      aggregation_types = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM aggregation_types WHERE aggregation_type_id IN (SELECT DISTINCT aggregation_type_id FROM timeseries);"),
      parameter_relationships = DBI::dbGetQuery(session$userData$AquaCache, "SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM timeseries AS t WHERE p.parameter_id = t.parameter_id) ;"),
      range = DBI::dbGetQuery(session$userData$AquaCache, "SELECT MIN(start_datetime) AS min_datetime, MAX(end_datetime) AS max_datetime FROM timeseries;"),
      timeseries = DBI::dbGetQuery(session$userData$AquaCache, "SELECT timeseries_id, location_id, sub_location_id, media_id, parameter_id, aggregation_type_id, EXTRACT(EPOCH FROM record_rate) AS record_rate, z, start_datetime, end_datetime FROM timeseries;")
    )
    
    moduleData$rates <- data.frame(seconds = unique(moduleData$timeseries$record_rate)[order(unique(moduleData$timeseries$record_rate))])
    moduleData$rates$period <- as.character(lubridate::seconds_to_period(moduleData$rates$seconds))
    
    moduleData$z <- unique(moduleData$timeseries$z[!is.na(moduleData$timeseries$z)])
    
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
      moduleData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(groups, collapse = ", "), ");"))
    } else {
      moduleData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
    }
    if (any(!is.na(moduleData$parameter_relationships$sub_group_id))) {
      sub_groups <- moduleData$parameter_relationships$sub_group_id[!is.na(moduleData$parameter_relationships$sub_group_id)]
      moduleData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
    } else {
      moduleData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
    }
    
    # Create a function to create the filteredData reactiveValues object
    createFilteredData <- function() {
      data <-  reactiveValues(
        locs = isolate(moduleData$locs),
        sub_locs = isolate(moduleData$sub_locs),
        z = isolate(moduleData$z),
        params = isolate(moduleData$params),
        media = isolate(moduleData$media),
        rates = isolate(moduleData$rates),
        aggregation_types = isolate(moduleData$aggregation_types),
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
    input_values <- reactiveValues(date_start = input$date_start,
                                   date_end = input$date_end,
                                   locations = input$locations, 
                                   sub_locations = input$sub_locations,
                                   z = input$z,
                                   aggregation = input$aggregation,
                                   rate = input$rate,
                                   media = input$media, 
                                   params = input$params)
    
    # flags to prevent running observers when the sidebar is first rendered
    render_flags <- reactiveValues(date_start = FALSE,
                                   date_end = FALSE,
                                   locations = FALSE,
                                   sub_locations = FALSE,
                                   z = FALSE,
                                   aggregation = FALSE,
                                   rate = FALSE,
                                   media = FALSE,
                                   params = FALSE)
    
    output$sidebar <- renderUI({
      req(moduleData)
      
      render_flags$date_start <- TRUE
      render_flags$date_end <- TRUE
      render_flags$locations <- TRUE
      render_flags$sub_locations <- TRUE
      render_flags$z <- TRUE
      render_flags$aggregation <- TRUE
      render_flags$rate <- TRUE
      render_flags$media <- TRUE
      render_flags$params <- TRUE

      tagList(
        # start and end datetime
        dateInput(ns("date_start"),
                       tr("date_start", language$language),
                       value = as.Date(moduleData$range$min_date),
                       min = as.Date(moduleData$range$min_date),
                       max = as.Date(moduleData$range$max_date),
                       format = "yyyy-mm-dd"
        ),
        # start and end datetime
        dateInput(ns("date_end"),
                       tr("date_end", language$language),
                       value = as.Date(moduleData$range$max_date),
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
        # Button for modal to let users filter locations by network or project
        actionButton(ns("loc_modal"),
                     label = tr("loc_modal", language$language),
                     width = "100%",
                     style = "font-size: 14px; margin-top: 5px;"
        ),
        # Selectize input for sub-locations
        selectizeInput(ns("sub_locations"),
                       label = tr("sub_loc(s)", language$language),
                       choices = stats::setNames(c("all", moduleData$sub_locs$sub_location_id),
                                         c(tr("all", language$language), moduleData$sub_locs[, tr("sub_location_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for depth/height
        selectizeInput(ns("z"), 
                       label = tr("z", language$language),
                       choices = stats::setNames(c("all", moduleData$z),
                                                 c(tr("all", language$language), moduleData$z)),
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
        # Selectize input for aggregation types
        selectizeInput(ns("aggregation"),
                       label = tr("aggregation_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$aggregation_types$aggregation_type_id),
                                         c(tr("all", language$language), moduleData$aggregation_types[, tr("aggregation_type_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for record rate
        selectizeInput(ns("rate"),
                       label = tr("nominal_rate", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$rates$seconds),
                                         c(tr("all", language$language), moduleData$rates[, "period"])),
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
                       label = tr("view_timeseries", language$language),
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
        DT::DTOutput(ns("tbl")), # Table with timeseries data, filtered by the sidebar inputs
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
    observeFilterInput("z")
    observeFilterInput("aggregation")
    observeFilterInput("rate")
    observeFilterInput("media")
    observeFilterInput("pGrps")
    observeFilterInput("pSubGrps")
    observeFilterInput("params")
    observeFilterInput("networks")
    observeFilterInput("projects")
    

    # Flags to prevent running observers unnecessarily when the 'reset' button is pressed
    reset_flags <- reactiveValues(date_start = FALSE,
                                  date_end = FALSE,
                                  locations = FALSE,
                                  sub_locations = FALSE,
                                  media = FALSE,
                                  z = FALSE,
                                  aggregation = FALSE,
                                  rate = FALSE,
                                  params = FALSE)
    
    ## Reset button observer ############
    observeEvent(input$reset, {
      # Reset the filteredData to its original state
      # pull back the originals
      newData <- createFilteredData()
      # overwrite each slot in the reactiveValues
      for (nm in names(newData)) {
        filteredData[[nm]] <- newData[[nm]]
      }
      
      # clear out the results table and hide the 'select all' button
      table_data(NULL)
      output$tbl <- DT::renderDT(data.frame())
      shinyjs::hide("select_all")
      # clear any rows still selected in the table
      DT::selectRows(proxy, NULL)
      
      # Reset the selectize inputs
      updateDateInput(session, "date_start",
                           value = as.Date(moduleData$range$min_date),
                           min = as.Date(moduleData$range$min_date),
                           max = as.Date(moduleData$range$max_date),
      )
      updateDateInput(session, "date_end",
                           value = as.Date(moduleData$range$max_date),
                           min = as.Date(moduleData$range$min_date),
                           max = as.Date(moduleData$range$max_date),
      )
      updateSelectizeInput(session, "locations",
                           choices = stats::setNames(c("all", moduleData$locs$location_id),
                                                     c(tr("all", language$language), moduleData$locs[, tr("generic_name_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "sub_locations",
                           choices = stats::setNames(c("all", moduleData$sub_locs$sub_location_id),
                                                     c(tr("all", language$language), moduleData$sub_locs[, tr("sub_location_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "media",
                           choices = stats::setNames(c("all", moduleData$media$media_id),
                                                     c(tr("all", language$language), moduleData$media[, tr("media_type_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "z",
                           choices = stats::setNames(c("all", moduleData$z),
                                                     c(tr("all", language$language), moduleData$z)),
                           selected = "all")
      updateSelectizeInput(session, "aggregation",
                           choices = stats::setNames(c("all", moduleData$aggregation_types$aggregation_type_id),
                                                     c(tr("all", language$language), moduleData$aggregation_types[, tr("aggregation_type_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "rate",
                           choices = stats::setNames(c("all", moduleData$rates$seconds),
                                                     c(tr("all", language$language), moduleData$rates[, "period"])),
                           selected = "all")
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", moduleData$params$parameter_id),
                                                     c(tr("all", language$language), moduleData$params[, tr("param_name_col", language$language)])),
                           selected = "all")
      
      reset_flags$date_start <- TRUE
      reset_flags$date_end <- TRUE
      reset_flags$locations <- TRUE
      reset_flags$sub_locations <- TRUE
      reset_flags$media <- TRUE
      reset_flags$z <- TRUE
      reset_flags$aggregation <- TRUE
      reset_flags$rate <- TRUE
      reset_flags$params <- TRUE
      
      # Reset the input values to their original state
      input_values$date_start <- input$date_start
      input_values$date_end <- input$date_end
      input_values$locations <- input$locations
      input_values$sub_locations <- input$sub_locations
      input_values$media <- input$media
      input_values$z <- input$z
      input_values$aggregation <- input$aggregation
      input_values$rate <- input$rate
      input_values$params <- input$params
      
    }, ignoreInit = TRUE)
    
    ## Modals to narrow selections ################
    # Observer for a modal that allows for location filtering based on projects and networks
    observeEvent(input$loc_modal, {
      showModal(modalDialog(
        title = tr("loc_modal", language$language),
        selectizeInput(ns("networks"),
                       label = tr("network(s)", language$language),
                       choices = stats::setNames(c("all", filteredData$networks$network_id),
                                                 c(tr("all", language$language), filteredData$networks[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        selectizeInput(ns("projects"),
                       label = tr("project(s)", language$language),
                       choices = stats::setNames(c("all", filteredData$projects$project_id),
                                                 c(tr("all", language$language), filteredData$projects[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        footer = tagList(
          actionButton(ns("loc_modal_filter"), tr("filter", language$language)),
          modalButton(tr("close", language$language))
        )
      ))
    })
    
    # Observer for the locations modal filter button
    observeEvent(input$loc_modal_filter, {
      # Filter the locations based on the selected projects and networks, update the locations selectizeInput
      req(input$projects, input$networks, filteredData$locations_projects, filteredData$locations_networks)
      
      if (!("all" %in% input$networks)) {
        remain_locs <- filteredData$locations_networks[filteredData$locations_networks$network_id %in% input$networks, ]
      } else {
        remain_locs <- filteredData$locations_networks
      }
      if (!("all" %in% input$projects)) {
        remain_locs <- filteredData$locations_projects[filteredData$locations_projects$project_id %in% input$projects, ]
      }
      remain_locs <- filteredData$locs[filteredData$locs$location_id %in% remain_locs$location_id, ]
      updateSelectizeInput(session, "locations",
                           choices = stats::setNames(c("all", remain_locs$location_id),
                                                     c(tr("all", language$language), remain_locs[, tr("generic_name_col", language$language)])),
                           selected = "all")
      removeModal()
    })
    
    
    # Observer for a modal that allows users to filter their parameters by group/sub-group
    observeEvent(input$param_modal, {
      showModal(modalDialog(
        title = tr("param_modal", language$language),
        selectizeInput(ns("pGrps"),
                       label = tr("param_group(s)", language$language),
                       choices = stats::setNames(c("all", filteredData$param_groups$group_id),
                                                 c(tr("all", language$language), filteredData$param_groups[, tr("param_group_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        selectizeInput(ns("pSubGrps"),
                       label = tr("param_sub_group(s)", language$language),
                       choices = stats::setNames(c("all", filteredData$param_sub_groups$sub_group_id),
                                                 c(tr("all", language$language), filteredData$param_sub_groups[, tr("param_sub_group_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        footer = tagList(
          actionButton(ns("param_modal_filter"), tr("filter", language$language)),
          modalButton(tr("close", language$language))
        )
      ))
    })
    
    # Observer for the parameters modal filter button
    observeEvent(input$param_modal_filter, {
      # Filter the parameters based on the selected groups and sub-groups, update the params selectizeInput
      req(input$pGrps, input$pSubGrps, filteredData$parameter_relationships, filteredData$params)
      
      if (!("all" %in% input$pGrps)) {
        remain_params <- filteredData$parameter_relationships[filteredData$parameter_relationships$group_id %in% input$pGrps, ]
      } else {
        remain_params <- filteredData$parameter_relationships
      }
      if (!("all" %in% input$pSubGrps)) {
        remain_params <- filteredData$parameter_relationships[filteredData$parameter_relationships$sub_group_id %in% input$pSubGrps, ]
      }
      remain_params <- filteredData$params[filteredData$params$parameter_id %in% remain_params$parameter_id, ]
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", remain_params$parameter_id),
                                                     c(tr("all", language$language), remain_params[, tr("param_name_col", language$language)])),
                           selected = "all")
      removeModal()
    })
    
    # Now apply filters top to bottom. If date range gets narrowed, the other filters will be applied to the narrowed date range. If locations gets narrowed, the other filters will be applied to the narrowed locations EXCEPT for date range, and so on down the line.
    
    ## Filters ###########
    ### date range inputs and filter ############
    observeEvent(list(input$date_start, input$date_end), {
      req(input$date_start, input$date_end, filteredData)
      
      # Flags to prevent running the observer when the date range is reset or initially rendered
      if (reset_flags$date_start) {
        reset_flags$date_start <- FALSE
        return()
      }
      if (render_flags$date_start) {
        render_flags$date_start <- FALSE
        return()
      }
      if (reset_flags$date_end) {
        reset_flags$date_end <- FALSE
        return()
      }
      if (render_flags$date_end) {
        render_flags$date_end <- FALSE
        return()
      }
      
      # Since this is the top-level filter, we reset the data.
      # There's no need to check for 0 timeseries here, because the date range is always valid.
      newData <- createFilteredData()
      # overwrite each slot in the reactiveValues
      for (nm in names(newData)) {
        filteredData[[nm]] <- newData[[nm]]
      }
      
      # clear out the results table
      table_data(NULL)
      
      # Filter the data based on the selected date range
      filteredData$range$min_date <- input$date_start
      filteredData$range$max_date <- input$date_end
      
      filteredData$timeseries <- filteredData$timeseries[filteredData$timeseries$start_datetime >= input$date_start & filteredData$timeseries$end_datetime <= input$date_end, ]
      filteredData$locs <- filteredData$locs[filteredData$locs$location_id %in% filteredData$timeseries$location_id, ]
      filteredData$sub_locs <- filteredData$sub_locs[filteredData$sub_locs$location_id %in% filteredData$locs$location_id, ]
      filteredData$z <- unique(filteredData$timeseries$z[!is.na(filteredData$timeseries$z)])
      filteredData$media <- filteredData$media[filteredData$media$media_id %in% filteredData$timeseries$media_id, ]
      filteredData$aggregation_types <- filteredData$aggregation_types[filteredData$aggregation_types$aggregation_type_id %in% filteredData$timeseries$aggregation_type_id, ]
      filteredData$rates <- filteredData$rates[filteredData$rates$seconds %in% filteredData$timeseries$record_rate, ]
      
      remain_projects <- filteredData$locations_projects[filteredData$locations_projects$location_id %in% filteredData$locs$location_id, ]
      filteredData$projects <- filteredData$projects[filteredData$projects$project_id %in% remain_projects$project_id, ]
      remain_networks <- filteredData$locations_networks[filteredData$locations_networks$location_id %in% filteredData$locs$location_id, ]
      filteredData$networks <- filteredData$networks[filteredData$networks$network_id %in% remain_networks$network_id, ]
      
      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% filteredData$timeseries$parameter_id, ]
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- filteredData$parameter_relationships[filteredData$parameter_relationships$parameter_id %in% filteredData$params$parameter_id, ]
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- filteredData$param_groups[filteredData$param_groups$group_id %in% filteredData$parameter_relationships$group_id, ]
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% sub_groups, ]
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      # Update the downstream selectize inputs with the filtered data
      updateSelectizeInput(session, "locations",
                           choices = stats::setNames(c("all", filteredData$locs$location_id),
                                                     c(tr("all", language$language), filteredData$locs[, tr("generic_name_col", language$language)])),
                           selected = if (input$locations %in% filteredData$locs$location_id) input$locations else "all"
      )
      updateSelectizeInput(session, "sub_locations",
                           choices = stats::setNames(c("all", filteredData$sub_locs$sub_location_id),
                                                     c(tr("all", language$language), filteredData$sub_locs[, tr("sub_location_col", language$language)])),
                           selected = if (input$sub_locations %in% filteredData$sub_locs$sub_location_id) input$sub_locations else "all"
      )
      updateSelectizeInput(session, "z",
                           choices = stats::setNames(c("all", filteredData$z),
                                                     c(tr("all", language$language), filteredData$z)),
                           selected = if (input$z %in% filteredData$z) input$z else "all"
      )
      updateSelectizeInput(session, "media",
                           choices = stats::setNames(c("all", filteredData$media$media_id),
                                                     c(tr("all", language$language), filteredData$media[, tr("media_type_col", language$language)])),
                           selected = if (input$media %in% filteredData$media$media_id) input$media else "all"
      )
      updateSelectizeInput(session, "aggregation",
                           choices = stats::setNames(c("all", filteredData$aggregation_types$aggregation_type_id),
                                                     c(tr("all", language$language), filteredData$aggregation_types[, tr("aggregation_type_col", language$language)])),
                           selected = if (input$aggregation %in% filteredData$aggregation_types$aggregation_type_id) input$aggregation else "all"
      )
      updateSelectizeInput(session, "rate",
                           choices = stats::setNames(c("all", filteredData$rates$seconds),
                                                     c(tr("all", language$language), filteredData$rates[, "period"])),
                           selected = if (input$rate %in% filteredData$rates$seconds) input$rate else "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )
      
      input_values$date_start <- input$date_start
      input_values$date_end <- input$date_end
    }, ignoreInit = TRUE)
    
    ### locations filter ############
    observeEvent(input$locations, {
      req(input$locations, filteredData)

      # Flags to prevent running the observer when the locations are reset or initially rendered
      if (reset_flags$locations) {
        reset_flags$locations <- FALSE
        return()
      }
      if (render_flags$locations) {
        render_flags$locations <- FALSE
        return()
      }

      # Filter the data based on the selected locations
      stash <- filteredData$timeseries
      filteredData$timeseries <- moduleData$timeseries[moduleData$timeseries$location_id %in% input$locations, ]
      if (nrow(filteredData$timeseries) == 0) {
        # If no timeseries are found, reset the timeseries to the original data, show a notification, and reset input$locations to its previous value
        filteredData$timeseries <- stash
        updateSelectizeInput(session, "locations",
                             selected = input_values$locations
        )
        showNotification(tr("no_ts_locs", language$language), type = "error", duration = 5)
        return()
      }

      filteredData$timeseries <- filteredData$timeseries[filteredData$timeseries$location_id %in% input$locations, ]
      filteredData$locs <- moduleData$locs[moduleData$locs$location_id %in% input$locations, ]
      filteredData$sub_locs <- moduleData$sub_locs[moduleData$sub_locs$location_id %in% filteredData$locs$location_id, ]
      filteredData$z <- unique(filteredData$timeseries$z[!is.na(filteredData$timeseries$z)])
      filteredData$media <- moduleData$media[moduleData$media$media_id %in% filteredData$timeseries$media_id, ]
      filteredData$aggregation_types <- filteredData$aggregation_types[filteredData$aggregation_types$aggregation_type_id %in% filteredData$timeseries$aggregation_type_id, ]
      filteredData$rates <- filteredData$rates[filteredData$rates$seconds %in% filteredData$timeseries$record_rate, ]
     
      remain_projects <- filteredData$locations_projects[filteredData$locations_projects$location_id %in% filteredData$locs$location_id, ]
      filteredData$projects <- filteredData$projects[filteredData$projects$project_id %in% remain_projects$project_id, ]
      remain_networks <- filteredData$locations_networks[filteredData$locations_networks$location_id %in% filteredData$locs$location_id, ]
      filteredData$networks <- filteredData$networks[filteredData$networks$network_id %in% remain_networks$network_id, ]

      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% filteredData$timeseries$parameter_id, ]
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- filteredData$parameter_relationships[filteredData$parameter_relationships$parameter_id %in% filteredData$params$parameter_id, ]
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- filteredData$param_groups[filteredData$param_groups$group_id %in% filteredData$parameter_relationships$group_id, ]
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% sub_groups, ]
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }

      # Update the downstream selectize inputs with the filtered data
      updateSelectizeInput(session, "sub_locations",
                           choices = stats::setNames(c("all", filteredData$sub_locs$sub_location_id),
                                                     c(tr("all", language$language), filteredData$sub_locs[, tr("sub_location_col", language$language)])),
                           selected = if (input$sub_locations %in% filteredData$sub_locs$sub_location_id) input$sub_locations else "all"
      )
      updateSelectizeInput(session, "media",
                           choices = stats::setNames(c("all", filteredData$media$media_id),
                                                     c(tr("all", language$language), filteredData$media[, tr("media_type_col", language$language)])),
                           selected = if (input$media %in% filteredData$media$media_id) input$media else "all"
      )
      updateSelectizeInput(session, "z",
                           choices = stats::setNames(c("all", filteredData$z),
                                                     c(tr("all", language$language), filteredData$z)),
                           selected = if (input$z %in% filteredData$z) input$z else "all"
      )
      updateSelectizeInput(session, "aggregation",
                           choices = stats::setNames(c("all", filteredData$aggregation_types$aggregation_type_id),
                                                     c(tr("all", language$language), filteredData$aggregation_types[, tr("aggregation_type_col", language$language)])),
                           selected = if (input$aggregation %in% filteredData$aggregation_types$aggregation_type_id) input$aggregation else "all"
      )
      updateSelectizeInput(session, "rate",
                           choices = stats::setNames(c("all", filteredData$rates$seconds),
                                                     c(tr("all", language$language), filteredData$rates[, "period"])),
                           selected = if (input$rate %in% filteredData$rates$seconds) input$rate else "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )

      # Reset the input values reactiveValues to the current selection
      input_values$locations <- input$locations
    }, ignoreInit = TRUE)

    ### sub_locations filter #########
    observeEvent(input$sub_locations, {
      req(input$sub_locations, filteredData)

      # Flags to prevent running the observer when the sub-locations are reset or initially rendered
      if (reset_flags$sub_locations) {
        reset_flags$sub_locations <- FALSE
        return()
      }
      if (render_flags$sub_locations) {
        render_flags$sub_locations <- FALSE
        return()
      }

      # Filter the data based on the selected sub-locations
      stash <- filteredData$timeseries
      filteredData$timeseries <- moduleData$timeseries[moduleData$timeseries$sub_location_id %in% input$sub_locations, ]
      if (nrow(filteredData$timeseries) == 0) {
        # If no timeseries are found, reset the timeseries to the original data, show a notification, and reset input$sub_locations to its previous value
        filteredData$timeseries <- stash
        updateSelectizeInput(session, "sub_locations",
                             selected = input_values$sub_locations
        )
        showNotification(tr("no_ts_sub_locs", language$language), type = "error", duration = 5)
        return()
      }

      filteredData$timeseries <- filteredData$timeseries[filteredData$timeseries$sub_location_id %in% input$sub_locations, ]
      filteredData$sub_locs <- moduleData$sub_locs[moduleData$sub_locs$sub_location_id %in% input$sub_locations, ]
      filteredData$z <- unique(filteredData$timeseries$z[!is.na(filteredData$timeseries$z)])
      filteredData$media <- moduleData$media[moduleData$media$media_id %in% filteredData$timeseries$media_id, ]
      filteredData$aggregation_types <- filteredData$aggregation_types[filteredData$aggregation_types$aggregation_type_id %in% filteredData$timeseries$aggregation_type_id, ]
      filteredData$rates <- filteredData$rates[filteredData$rates$seconds %in% filteredData$timeseries$record_rate, ]

      # No impact on projects or networks as these are location based and we're past that point

      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% filteredData$timeseries$parameter_id, ]
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- filteredData$parameter_relationships[filteredData$parameter_relationships$parameter_id %in% filteredData$params$parameter_id, ]
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- filteredData$param_groups[filteredData$param_groups$group_id %in% filteredData$parameter_relationships$group_id, ]
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% sub_groups, ]
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }

      # Update the downstream selectize inputs with the filtered data
      updateSelectizeInput(session, "media",
                           choices = stats::setNames(c("all", filteredData$media$media_id),
                                                     c(tr("all", language$language), filteredData$media[, tr("media_type_col", language$language)])),
                           selected = if (input$media %in% filteredData$media$media_id) input$media else "all"
      )
      updateSelectizeInput(session, "z",
                           choices = stats::setNames(c("all", filteredData$z),
                                                     c(tr("all", language$language), filteredData$z)),
                           selected = if (input$z %in% filteredData$z) input$z else "all"
      )
      updateSelectizeInput(session, "aggregation",
                           choices = stats::setNames(c("all", filteredData$aggregation_types$aggregation_type_id),
                                                     c(tr("all", language$language), filteredData$aggregation_types[, tr("aggregation_type_col", language$language)])),
                           selected = if (input$aggregation %in% filteredData$aggregation_types$aggregation_type_id) input$aggregation else "all"
      )
      updateSelectizeInput(session, "rate",
                           choices = stats::setNames(c("all", filteredData$rates$seconds),
                                                     c(tr("all", language$language), filteredData$rates[, "period"])),
                           selected = if (input$rate %in% filteredData$rates$seconds) input$rate else "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )

      # Reset the input values reactiveValues to the current selection
      input_values$sub_locations <- input$sub_locations
    }, ignoreInit = TRUE)
    
    ### z filter #########
    observeEvent(input$z, {
      req(input$z, filteredData)

      # Flags to prevent running the observer when the z values are reset or initially rendered
      if (reset_flags$z) {
        reset_flags$z <- FALSE
        return()
      }
      if (render_flags$z) {
        render_flags$z <- FALSE
        return()
      }

      # Filter the data based on the selected z values
      stash <- filteredData$timeseries
      filteredData$timeseries <- moduleData$timeseries[moduleData$timeseries$z %in% input$z, ]
      if (nrow(filteredData$timeseries) == 0) {
        # If no timeseries are found, reset the timeseries to the original data, show a notification, and reset input$z to its previous value
        filteredData$timeseries <- stash
        updateSelectizeInput(session, "z",
                             selected = input_values$z
        )
        showNotification(tr("no_ts_z", language$language), type = "error", duration = 5)
        return()
      }

      filteredData$timeseries <- filteredData$timeseries[filteredData$timeseries$z %in% input$z, ]
      filteredData$z <- unique(filteredData$timeseries$z[!is.na(filteredData$timeseries$z)])
      filteredData$media <- moduleData$media[moduleData$media$media_id %in% filteredData$timeseries$media_id, ]
      filteredData$aggregation_types <- filteredData$aggregation_types[filteredData$aggregation_types$aggregation_type_id %in% filteredData$timeseries$aggregation_type_id, ]
      filteredData$rates <- filteredData$rates[filteredData$rates$seconds %in% filteredData$timeseries$record_rate, ]

      # No impact on projects or networks as these are location based and we're past that point
      
      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% filteredData$timeseries$parameter_id, ]
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- filteredData$parameter_relationships[filteredData$parameter_relationships$parameter_id %in% filteredData$params$parameter_id, ]
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- filteredData$param_groups[filteredData$param_groups$group_id %in% filteredData$parameter_relationships$group_id, ]
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% sub_groups, ]
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      # Update the downstream selectize inputs with the filtered data
      updateSelectizeInput(session, "media",
                           choices = stats::setNames(c("all", filteredData$media$media_id),
                                                     c(tr("all", language$language), filteredData$media[, tr("media_type_col", language$language)])),
                           selected = if (input$media %in% filteredData$media$media_id) input$media else "all"
      )
      updateSelectizeInput(session, "aggregation",
                           choices = stats::setNames(c("all", filteredData$aggregation_types$aggregation_type_id),
                                                     c(tr("all", language$language), filteredData$aggregation_types[, tr("aggregation_type_col", language$language)])),
                           selected = if (input$aggregation %in% filteredData$aggregation_types$aggregation_type_id) input$aggregation else "all"
      )
      updateSelectizeInput(session, "rate",
                           choices = stats::setNames(c("all", filteredData$rates$seconds),
                                                     c(tr("all", language$language), filteredData$rates[, "period"])),
                           selected = if (input$rate %in% filteredData$rates$seconds) input$rate else "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )
      
      # Reset the input values reactiveValues to the current selection
      input_values$z <- input$z
    }, ignoreInit = TRUE)
      

    ### media filter #########
    observeEvent(input$media, {
      req(input$media, filteredData)

      # Flags to prevent running the observer when the media types are reset or initially rendered
      if (reset_flags$media) {
        reset_flags$media <- FALSE
        return()
      }
      if (render_flags$media) {
        render_flags$media <- FALSE
        return()
      }

      # Filter the data based on the selected media types
      stash <- filteredData$timeseries
      filteredData$timeseries <- moduleData$timeseries[moduleData$timeseries$media_id %in% input$media, ]
      if (nrow(filteredData$timeseries) == 0) {
        # If no timeseries are found, reset the timeseries to the original data, show a notification, and reset input$media to its previous value
        filteredData$timeseries <- stash
        updateSelectizeInput(session, "media",
                             selected = input_values$media
        )
        showNotification(tr("no_ts_medias", language$language), type = "error", duration = 5)
        return()
      }

      filteredData$timeseries <- filteredData$timeseries[filteredData$timeseries$media_id %in% input$media, ]
      filteredData$media <- moduleData$media[moduleData$media$media_id %in% input$media, ]
      filteredData$aggregation_types <- filteredData$aggregation_types[filteredData$aggregation_types$aggregation_type_id %in% filteredData$timeseries$aggregation_type_id, ]
      filteredData$rates <- filteredData$rates[filteredData$rates$seconds %in% filteredData$timeseries$record_rate, ]
      
      
      # No impact on projects or networks as we're no longer narrowing locations

      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% filteredData$timeseries$parameter_id, ]
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- filteredData$parameter_relationships[filteredData$parameter_relationships$parameter_id %in% filteredData$params$parameter_id, ]
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- filteredData$param_groups[filteredData$param_groups$group_id %in% filteredData$parameter_relationships$group_id, ]
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% sub_groups, ]
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }

      # Update the downstream selectize inputs with the filtered data
      updateSelectizeInput(session, "aggregation",
                           choices = stats::setNames(c("all", filteredData$aggregation_types$aggregation_type_id),
                                                     c(tr("all", language$language), filteredData$aggregation_types[, tr("aggregation_type_col", language$language)])),
                           selected = if (input$aggregation %in% filteredData$aggregation_types$aggregation_type_id) input$aggregation else "all"
      )
      updateSelectizeInput(session, "rate",
                           choices = stats::setNames(c("all", filteredData$rates$seconds),
                                                     c(tr("all", language$language), filteredData$rates[, "period"])),
                           selected = if (input$rate %in% filteredData$rates$seconds) input$rate else "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )

      # Reset the input values reactiveValues to the current selection
      input_values$media <- input$media
    }, ignoreInit = TRUE)

    ### aggregation type filter #########
    observeEvent(input$aggregation, {
      req(input$aggregation, filteredData)

      # Flags to prevent running the observer when the sample types are reset or initially rendered
      if (reset_flags$aggregation) {
        reset_flags$aggregation <- FALSE
        return()
      }
      if (render_flags$aggregation) {
        render_flags$aggregation <- FALSE
        return()
      }

      # Filter the data based on the selected aggregation types
      stash <- filteredData$timeseries
      filteredData$timeseries <- moduleData$timeseries[moduleData$timeseries$sample_type %in% input$aggregation, ]
      if (nrow(filteredData$timeseries) == 0) {
        # If no timeseries are found, reset the timeseries to the original data, show a notification, and reset input$aggregation to its previous value
        filteredData$timeseries <- stash
        updateSelectizeInput(session, "aggregation",
                             selected = input_values$aggregation
        )
        showNotification(tr("no_ts_agg", language$language), type = "error", duration = 5)
        return()
      }

      filteredData$timeseries <- filteredData$timeseries[filteredData$timeseries$aggregation_type_id %in% input$aggregation, ]
      filteredData$aggregation_types <- moduleData$aggregation_types[moduleData$aggregation_types$aggregation_type_id %in% input$aggregation, ]
      filteredData$rates <- filteredData$rates[filteredData$rates$seconds %in% filteredData$timeseries$record_rate, ]
      

      # No impact on projects or networks as we're no longer narrowing locations

      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% filteredData$timeseries$parameter_id, ]
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- filteredData$parameter_relationships[filteredData$parameter_relationships$parameter_id %in% filteredData$params$parameter_id, ]
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- filteredData$param_groups[filteredData$param_groups$group_id %in% filteredData$parameter_relationships$group_id, ]
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% sub_groups, ]
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }

      # Update the downstream selectize inputs with the filtered data
      updateSelectizeInput(session, "rate",
                           choices = stats::setNames(c("all", filteredData$rates$seconds),
                                                     c(tr("all", language$language), filteredData$rates[, "period"])),
                           selected = if (input$rate %in% filteredData$rates$seconds) input$rate else "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )

      # Reset the input values reactiveValues to the current selection
      input_values$aggregation <- input$aggregation
    }, ignoreInit = TRUE)
    
    ### rate filter #########
    observeEvent(input$rate, {
      req(input$rate, filteredData)

      # Flags to prevent running the observer when the rates are reset or initially rendered
      if (reset_flags$rate) {
        reset_flags$rate <- FALSE
        return()
      }
      if (render_flags$rate) {
        render_flags$rate <- FALSE
        return()
      }

      # Filter the data based on the selected rates
      stash <- filteredData$timeseries
      filteredData$timeseries <- moduleData$timeseries[moduleData$timeseries$record_rate %in% input$rate, ]
      if (nrow(filteredData$timeseries) == 0) {
        # If no timeseries are found, reset the timeseries to the original data, show a notification, and reset input$rate to its previous value
        filteredData$timeseries <- stash
        updateSelectizeInput(session, "rate",
                             selected = input_values$rate
        )
        showNotification(tr("no_ts_rates", language$language), type = "error", duration = 5)
        return()
      }

      filteredData$timeseries <- filteredData$timeseries[filteredData$timeseries$record_rate %in% input$rate, ]
      filteredData$rates <- moduleData$rates[moduleData$rates$seconds %in% input$rate, ]

      # No impact on projects or networks as we're no longer narrowing locations

      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% filteredData$timeseries$parameter_id, ]
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- filteredData$parameter_relationships[filteredData$parameter_relationships$parameter_id %in% filteredData$params$parameter_id, ]
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- filteredData$param_groups[filteredData$param_groups$group_id %in% filteredData$parameter_relationships$group_id, ]
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% sub_groups, ]
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
        
        # Update the downstream selectize inputs with the filtered data
        updateSelectizeInput(session, "params",
                             choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                       c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                             selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
        )
        
      # Reset the input values reactiveValues to the current selection
      input_values$rate <- input$rate
    }, ignoreInit = TRUE)

    ### params filter #########
    observeEvent(input$params, {
      req(input$params, filteredData)

      # Flags to prevent running the observer when the params are reset or initially rendered
      if (reset_flags$params) {
        reset_flags$params <- FALSE
        return()
      }
      if (render_flags$params) {
        render_flags$params <- FALSE
        return()
      }

      # Filter the data based on the selected params
      filteredData$timeseries <- moduleData$timeseries[moduleData$timeseries$parameter_id %in% input$params, ]
      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% input$params, ]

      # No impact on projects or networks as we're no longer narrowing locations
      # No impact on parameters as we're no longer narrowing parameters
      # No inputs to update as this is the last filter

      # Reset the input values reactiveValues to the current selection
      input_values$params <- input$params
    }, ignoreInit = TRUE)
    
    
    
    # Create the samples table and render it ###################################
    # The results table will be shown only if the user clicks on the 'view results' button in the modal
    table_data <- reactiveVal()
    # observeEvent(input$filter, {
    #   req(filteredData)
    #   samples <- dbGetQueryDT(
    #     session$userData$AquaCache, 
    #     paste0(
    #       "SELECT DISTINCT ON (s.sample_id)
    #       s.sample_id, l.location_id, l.name, sl.sub_location_name, s.z AS depth, s.datetime AS sample_datetime_utc, s.target_datetime AS target_datetime_utc, m.media_type, st.sample_type, cm.collection_method, s.sample_volume_ml, s.purge_volume_l, s.purge_time_min, s.flow_rate_l_min, s.wave_hgt_m, s.sample_grade, s.sample_approval, s.sample_qualifier, s.note FROM samples s 
    #        JOIN results r ON s.sample_id = r.sample_id
    #        JOIN locations l ON s.location_id = l.location_id
    #        LEFT JOIN sub_locations sl ON s.sub_location_id = sl.sub_location_id
    #        JOIN media_types m ON s.media_id = m.media_id
    #        JOIN sample_types st ON s.sample_type = st.sample_type_id
    #        JOIN parameters p ON r.parameter_id = p.parameter_id
    #        JOIN collection_methods cm ON s.collection_method = cm.collection_method_id
    #        LEFT JOIN organizations orgs ON s.owner = orgs.organization_id
    #        WHERE s.location_id IN (", 
    #       paste(filteredData$locs$location_id, collapse = ", "), 
    #       if (nrow(filteredData$sub_locs) > 0) 
    #         paste0(") AND s.sub_location_id IN (", paste(filteredData$sub_locs$sub_location_id, collapse = ", "), ")") 
    #       else 
    #         ")", 
    #       " AND s.sample_type IN (", paste(filteredData$sample_types$sample_type_id, collapse = ", "), ")",
    #       " AND s.media_id IN (", paste(filteredData$media$media_id, collapse = ", "), ")",
    #       " AND s.datetime BETWEEN '", filteredData$range$min_date, "' AND '", filteredData$range$max_date, "'",
    #       " AND r.parameter_id IN (", paste(filteredData$params$parameter_id, collapse = ", "), ")",
    #       ";"
    #     )
    #   )
    #   
    #   # Drop columns with all NA values
    #   na_cols <- names(samples)[sapply(samples, function(x) all(is.na(x)))]
    #   samples[, (na_cols) := NULL]      
    #   
    #   table_data(samples)
    # })
    # 
    # # 
    # observe({ # Render the table
    #   req(table_data())
    #   
    #   out_tbl <- DT::datatable(table_data(),
    #                            rownames = FALSE,
    #                            selection = "multiple",
    #                            filter  = "none",
    #                            options = list(
    #                              scrollX = TRUE,
    #                              initComplete = htmlwidgets::JS(
    #                                "function(settings, json) {",
    #                                "$(this.api().table().header()).css({",
    #                                "  'background-color': '#079',",
    #                                "  'color': '#fff',",
    #                                "  'font-size': '100%',",
    #                                # "  'font-family': 'montserrat'", # Unfortunately this isn't as readable as the default font. Left just in case it's needed later.
    #                                "});",
    #                                "$(this.api().table().body()).css({",
    #                                "  'font-size': '90%',",
    #                                # "  'font-family': 'nunito-sans'", # Unfortunately this isn't as readable as the default font. Left just in case it's needed later.
    #                                "});",
    #                                "}"
    #                              ),
    #                              columnDefs = list(
    #                                list(targets = c(0,1), visible = FALSE) #Hides the sample_id column. Column index numbers start at 0 here!!!
    #                              ),
    #                              language = list(
    #                                info = tr("tbl_info", language$language),
    #                                infoEmpty = tr("tbl_info_empty", language$language),
    #                                paginate = list(previous = "", `next` = ""),
    #                                search = tr("tbl_search", language$language),
    #                                lengthMenu = tr("tbl_length", language$language),
    #                                infoFiltered = tr("tbl_filtered", language$language),
    #                                zeroRecords = tr("tbl_zero", language$language)
    #                              ),
    #                              pageLength = 10
    #                            )
    #   )
    #   output$tbl <- DT::renderDT(out_tbl)
    #   
    #   
    #   if (nrow(table_data()) == 0) {
    #     shinyjs::hide("select_all")
    #   } else {
    #     shinyjs::show("select_all")
    #   }
    # }) # End of table render
    # # End of creating and rendering table section
    # 
    # # Create the proxy for datatable manipulations
    # proxy <- DT::dataTableProxy("tbl")
    # 
    # 
    # # Show/hide the view button based on if a row is selected #
    # observe({
    #   if (is.null(input$tbl_rows_selected)) {
    #     shinyjs::hide("view_data")
    #   } else {
    #     shinyjs::show("view_data")
    #     updateActionButton(session, "view_data", label = paste0(tr("view_data1", language$language), " ", length(input$tbl_rows_selected), " ", tr("view_data2", language$language)))
    #   }
    # })
    # 
    # # Select/deselect all rows in the table #
    # select_all <- reactiveVal(FALSE)
    # observeEvent(input$select_all, {
    #   if (select_all()) {
    #     DT::selectRows(proxy, NULL)
    #     select_all(FALSE)
    #   } else {
    #     DT::selectRows(proxy, seq_len(nrow(table_data())))
    #     select_all(TRUE)
    #   }
    # })
    # 
    # 
    # # Show a modal with the data when the view button is clicked ################
    # observeEvent(input$view_data, {
    #   # Get the timeseries_ids of the selected rows
    #   selected_sampleids <- table_data()[input$tbl_rows_selected, sample_id]
    #   selected_loc_ids <- table_data()[input$tbl_rows_selected, location_id]
    #   
    #   
    #   # Query will be for discrete or continuous data, depending on input$type
    #   # Show a modal with a subset (first 3 rows per sample_id) of the data. Below this, show a date range picker (with min/max preset based on the selected data), the number of rows that would be returned, and download and close buttons. The download button will give the user the entire dataset within the date range selected
    #   
    #   # Get the sample results
    #   # Construct a comma-separated string of sample IDs
    #   sample_ids_str <- paste(selected_sampleids, collapse = ",")
    #   
    #   # Single query using a window function to limit to 3 rows/results per sample_id
    #   query <- paste0(
    #     "SELECT * FROM (SELECT r.sample_id, r.result, p.param_name AS parameter, p.unit_default AS units, rs.result_speciation, rt.result_type, sf.sample_fraction, rc.result_condition, r.result_condition_value, rvt.result_value_type, pm.protocol_name, l.lab_name AS laboratory, r.analysis_datetime, ROW_NUMBER() OVER (PARTITION BY sample_id ORDER BY result_id) AS rn 
    #     FROM results r
    #     JOIN parameters p ON r.parameter_id = p.parameter_id
    #     JOIN result_types rt ON r.result_type = rt.result_type_id
    #     LEFT JOIN sample_fractions sf ON r.sample_fraction = sf.sample_fraction_id
    #     LEFT JOIN result_conditions rc ON r.result_condition = rc.result_condition_id
    #     LEFT JOIN result_value_types rvt ON r.result_value_type = rvt.result_value_type_id
    #     LEFT JOIN result_speciations rs ON r.result_speciation = rs.result_speciation_id
    #     LEFT JOIN protocols_methods pm ON r.protocol_method = pm.protocol_id
    #     LEFT JOIN laboratories l ON r.laboratory = l.lab_id
    #     WHERE sample_id IN (", sample_ids_str, ")) sub WHERE rn <= 3;"
    #   )
    #   
    #   subset <- dbGetQueryDT(session$userData$AquaCache, query)
    #   subset[, rn := NULL] # Drop column 'rn', left over from the window function
    #   
    #   output$modal_subset <- DT::renderDT({  # Create datatable for the measurements
    #     DT::datatable(subset,
    #                   rownames = FALSE,
    #                   selection = "none",
    #                   filter  = "none",
    #                   options = list(
    #                     scrollX = TRUE,
    #                     initComplete = htmlwidgets::JS(
    #                       "function(settings, json) {",
    #                       "$(this.api().table().header()).css({",
    #                       "  'background-color': '#079',",
    #                       "  'color': '#fff',",
    #                       "  'font-size': '100%',",
    #                       "});",
    #                       "$(this.api().table().body()).css({",
    #                       "  'font-size': '90%',",
    #                       "});",
    #                       "}"
    #                     ),
    #                     language = list(
    #                       info = "",
    #                       infoEmpty = tr("tbl_info_empty", language$language),
    #                       paginate = list(previous = "", `next` = ""),
    #                       search = tr("tbl_search", language$language),
    #                       infoFiltered = "",
    #                       zeroRecords = tr("tbl_zero", language$language)
    #                     ),
    #                     dom = 'rtip'
    #                   )
    #     )
    #   }) # End of function creating data subset datatable
    #   
    #   # Get location metadata
    #   location <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM ", if (language$abbrev == "fr") "location_metadata_fr" else "location_metadata_en", " WHERE location_id IN (", paste(selected_loc_ids, collapse = ", "), ") LIMIT 3;")) # Get the location metadata
    #   
    #   output$modal_location_metadata <- DT::renderDT({  # Create datatable for the locations
    #     DT::datatable(location,
    #                   rownames = FALSE,
    #                   selection = "none",
    #                   filter  = "none",
    #                   options = list(
    #                     initComplete = htmlwidgets::JS(
    #                       "function(settings, json) {",
    #                       "$(this.api().table().header()).css({",
    #                       "  'background-color': '#079',",
    #                       "  'color': '#fff',",
    #                       "  'font-size': '100%',",
    #                       "});",
    #                       "$(this.api().table().body()).css({",
    #                       "  'font-size': '90%',",
    #                       "});",
    #                       "}"
    #                     ),
    #                     columnDefs = list(
    #                       list(
    #                         targets = c(2, 6:9), # Column index numbers start at 0 here again!!!
    #                         render = htmlwidgets::JS( # Truncate long strings in the table
    #                           "function(data, type, row, meta) {",
    #                           "return type === 'display' && data !== null && data.length > 20 ?",
    #                           "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
    #                           "}")
    #                       )
    #                     ),
    #                     language = list(
    #                       info = "",
    #                       infoEmpty = tr("tbl_info_empty", language$language),
    #                       paginate = list(previous = "", `next` = ""),
    #                       search = "",
    #                       infoFiltered = "",
    #                       zeroRecords = tr("tbl_zero", language$language)
    #                     ),
    #                     dom = 'rt',
    #                     scrollX = TRUE
    #                   )
    #     ) %>%
    #       DT::formatRound(columns = c(4,5,5), digits = 3) # Round numbers, here index starts at 1 (not javascript)
    #   }) # End of function creating location metatadata datatable
    #   
    #   
    #   # Create the modal
    #   showModal(modalDialog(
    #     h4(tr("discrete_subset_msg", language$language)),
    #     DT::DTOutput(ns("modal_subset")),
    #     h4(tr("loc_meta_msg", language$language)),
    #     DT::DTOutput(ns("modal_location_metadata")),
    #     textOutput(ns("num_rows")),
    #     selectizeInput(ns("modal_format"), label = tr("dl_format", language$language), choices = stats::setNames(c("xlsx", "csv", "sqlite"), c(tr("dl_format_xlsx", language$language), tr("dl_format_csv", language$language), tr("dl_format_sqlite", language$language))), selected = "xlsx"),
    #     downloadButton(ns("download"), tr("dl_data", language$language)),
    #     size = "l"
    #   ))
    # })
    
    
    
    
    # # Updates to modal ########################################################
    # # Get the number of rows that will be returned based on the date range selected and update the subset table if necessary
    # observe({
    #   req(input$tbl_rows_selected, filteredData$params, table_data())
    #   selected_sampleids <- table_data()[input$tbl_rows_selected, sample_id]
    #   
    #   rows <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT COUNT(*) FROM results WHERE sample_id IN (", paste(selected_sampleids, collapse = ", "), ") AND parameter_id IN (", paste(filteredData$params$parameter_id, collapse = ", "), ");"))[[1]]
    #   
    #   # ouput message about number of rows
    #   output$num_rows <- renderText({ paste0(tr("dl_num_results", language$language), " ", rows) })
    #   
    #   # Update selectizeInput based on number of rows
    #   if (rows > 1000000) {
    #     updateSelectizeInput(session, "modal_format", label = tr("dl_format_no_xlsx", language$language),  choices = stats::setNames(c("csv", "sqlite"), c(tr("dl_format_csv", language$language), tr("dl_format_sqlite", language$language))), selected = "csv")
    #   } else {
    #     updateSelectizeInput(session, "modal_format", label = tr("dl_format", language$language), choices = stats::setNames(c("xlsx", "csv", "sqlite"), c(tr("dl_format_xlsx", language$language), tr("dl_format_csv", language$language), tr("dl_format_sqlite", language$language))), selected = "xlsx")
    #   }
    # }) # End of observe for number of rows
    
    
    # # Download handling #######################################################
    # output$download <- downloadHandler(
    #   filename = function() {
    #     paste0("data_", format(Sys.time(), "%Y%m%d_%H%M%S%Z"), ".", if (input$modal_format == "csv") "zip" else input$modal_format)
    #   },
    #   content = function(file) {
    #     
    #     showNotification(tr("dl_prep", language$language), id = "download_notification", duration = NULL, type = "message")
    #     
    #     # Get the data together
    #     selected_sampleids <- table_data()[input$tbl_rows_selected, sample_id]
    #     
    #     selected_loc_ids <- table_data()[input$tbl_rows_selected, location_id]
    #     
    #     data <- list()
    #     data$location_metadata <- dbGetQueryDT(session$userData$AquaCache, paste0("SELECT * FROM ", if (language$abbrev == "fr") "location_metadata_fr" else "location_metadata_en", " WHERE location_id IN (", paste(selected_loc_ids, collapse = ", "), ");")) # Get the location metadata
    #     data$samples <- table_data()
    #     data$results <- dbGetQueryDT(session$userData$AquaCache, paste0(
    #       "SELECT s.location_id, r.sample_id, s.datetime, s.target_datetime, r.result, p.param_name AS parameter, p.unit_default AS units, rs.result_speciation, rt.result_type, sf.sample_fraction, rc.result_condition, r.result_condition_value, rvt.result_value_type, pm.protocol_name, l.lab_name AS laboratory, r.analysis_datetime
    #     FROM results r
    #     JOIN samples s ON r.sample_id = s.sample_id
    #     JOIN parameters p ON r.parameter_id = p.parameter_id
    #     JOIN result_types rt ON r.result_type = rt.result_type_id
    #     LEFT JOIN sample_fractions sf ON r.sample_fraction = sf.sample_fraction_id
    #     LEFT JOIN result_conditions rc ON r.result_condition = rc.result_condition_id
    #     LEFT JOIN result_value_types rvt ON r.result_value_type = rvt.result_value_type_id
    #     LEFT JOIN result_speciations rs ON r.result_speciation = rs.result_speciation_id
    #     LEFT JOIN protocols_methods pm ON r.protocol_method = pm.protocol_id
    #     LEFT JOIN laboratories l ON r.laboratory = l.lab_id
    #     WHERE r.sample_id IN (", paste(selected_sampleids, collapse = ","), ");"
    #     ))
    #     
    #     if ("grade" %in% names(data$samples)) {
    #       data$grades <- dbGetQueryDT(session$userData$AquaCache, "SELECT * FROM grade_types;")
    #     }
    #     if ("approval" %in% names(data$samples)) {
    #       data$approvals <- dbGetQueryDT(session$userData$AquaCache, "SELECT * FROM approval_types;")
    #     }
    #     if ("qualifier" %in% names(data$samples)) {
    #       data$qualifiers <- dbGetQueryDT(session$userData$AquaCache, "SELECT * FROM qualifier_types;")
    #     }
    #     
    #     
    #     if (input$modal_format == "xlsx") {
    #       openxlsx::write.xlsx(data, file)
    #     } else if (input$modal_format == "csv") {
    #       # Temporary directory to store CSV files
    #       temp_dir <- tempdir()
    #       csv_files <- lapply(names(data), function(name) {
    #         file_name <- file.path(temp_dir, paste0(name, ".csv"))
    #         data.table::fwrite(data[[name]], file_name)
    #         return(file_name)
    #       })
    #       # Use zip to compress the files
    #       utils::zip(file, unlist(csv_files))
    #     } else if (input$modal_format == "sqlite") {
    #       # Create an sqlite database and write the data tables to it
    #       db <- DBI::dbConnect(RSQLite::SQLite(), dbname = file)
    #       lapply(names(data), function(name) {
    #         DBI::dbWriteTable(conn = db, name = name, value = data[[name]], overwrite = TRUE)
    #       })
    #       DBI::dbDisconnect(db)
    #     }
    #     removeNotification("download_notification")
    #   }) # End of downloadHandler
    
  }) # End moduleServer
} # End contData function
