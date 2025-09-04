discDataUI <- function(id) {
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
      ),
      HTML(sprintf("
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #E5F4F6;
        /* collapsed header */
        --bs-accordion-btn-bg:      #0097A9;
        /* expanded header */
        --bs-accordion-active-bg:   #0097A9;
      }
    ", ns("accordion")))
    ),
    uiOutput(ns("top")),
    page_sidebar(
      sidebar = sidebar(
        title = NULL,
        width = 400,
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),
        uiOutput(ns("sidebar")) # UI is rendered in the server function below so that it can use database information as well as language selections.
      ),
      uiOutput(ns("main"))
    )
  )
}

discData <- function(id, language, inputs) {
  
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
    if (session$userData$user_logged_in) {  # If logged in, get or create data that lives only with this session,
      cached <- disc_data_module_data(con = session$userData$AquaCache, env = session$userData$app_cache)
    } else {
      cached <- disc_data_module_data(con = session$userData$AquaCache)
    }
    
    moduleData <- reactiveValues(
      locs = cached$locs,
      sub_locs = cached$sub_locs,
      params = cached$params,
      media = cached$media,
      parameter_relationships = cached$parameter_relationships,
      range = cached$range,
      sample_types = cached$sample_types,
      samples = cached$samples,
      locations_projects = cached$locations_projects,
      projects = cached$projects,
      locations_networks = cached$locations_networks,
      networks = cached$networks,
      param_groups = cached$param_groups,
      param_sub_groups = cached$param_sub_groups
    )
    
    # Create a function to create the filteredData reactiveValues object
    createFilteredData <- function() {
      data <-  reactiveValues(
        locs = isolate(moduleData$locs),
        sub_locs = isolate(moduleData$sub_locs),
        params = isolate(moduleData$params),
        media = isolate(moduleData$media),
        parameter_relationships = isolate(moduleData$parameter_relationships),
        range = isolate(moduleData$range),
        sample_types = isolate(moduleData$sample_types),
        samples = isolate(moduleData$samples),
        projects = isolate(moduleData$projects),
        networks = isolate(moduleData$networks),
        locations_networks = isolate(moduleData$locations_networks),
        locations_projects = isolate(moduleData$locations_projects),
        param_groups = isolate(moduleData$param_groups),
        param_sub_groups = isolate(moduleData$param_sub_groups)
      )
      return(data)
    }
    
    # Assign the input value to a reactive right away as it's reset to NULL as soon as this module is loaded
    moduleInputs <- reactiveValues(location_id = if (!is.null(inputs$location_id)) as.numeric(inputs$location_id) else NULL)
    
    # If a location was provided from the map module, pre-filter the data, else create the full filteredData object
    if (!is.null(moduleInputs$location_id)) {
      filteredData <- createFilteredData()
      # If the location_id is not in the filteredData$locs, return early
      if (!moduleInputs$location_id %in% filteredData$locs$location_id) {
        moduleInputs$location_id <- NULL
        return()
      }
      
      loc_id <- moduleInputs$location_id
      filteredData$samples <- filteredData$samples[filteredData$samples$location_id %in% loc_id, ]
      filteredData$locs <- filteredData$locs[filteredData$locs$location_id %in% loc_id, ]
      filteredData$sub_locs <- filteredData$sub_locs[filteredData$sub_locs$location_id %in% loc_id, ]
      filteredData$media <- filteredData$media[filteredData$media$media_id %in% filteredData$samples$media_id, ]
      filteredData$sample_types <- filteredData$sample_types[filteredData$sample_types$sample_type_id %in% filteredData$samples$sample_type, ]
      
      remain_projects <- filteredData$locations_projects[filteredData$locations_projects$location_id %in% loc_id, ]
      filteredData$projects <- filteredData$projects[filteredData$projects$project_id %in% remain_projects$project_id, ]
      remain_networks <- filteredData$locations_networks[filteredData$locations_networks$location_id %in% loc_id, ]
      filteredData$networks <- filteredData$networks[filteredData$networks$network_id %in% remain_networks$network_id, ]
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache,
                                             paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (",
                                                    paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache,
                                                                paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (",
                                                                       paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache,
                                                       paste0("SELECT * FROM parameter_groups WHERE group_id IN (",
                                                              paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(),
                                                  group_name_fr = character(), description = character(),
                                                  description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache,
                                                           paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (",
                                                                  paste(sub_groups, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = character(),
                                                      sub_group_name_fr = character(), description = character(),
                                                      description_fr = character())
        }
      }
    } else {
      filteredData <- createFilteredData()
    }
    
    
    # Create UI elements and necessary helpers ################
    # NOTE: output$sidebar is rendered at module load time, but also re-rendered whenever a change to the language is made.
    
    # Reactive values to store the input values so that inputs can be reset if the user ends up narrowing their selections to 0 samples
    input_values <- reactiveValues()
    
    # flags to prevent running observers when the sidebar is first rendered
    render_flags <- reactiveValues(date_range = FALSE,
                                   locations = FALSE,
                                   sub_locations = FALSE,
                                   media = FALSE,
                                   sample_types = FALSE,
                                   params = FALSE)
    
    output$top <- renderUI({
      tagList(
        accordion(
          id = ns("accordion"),
          open = TRUE,
          accordion_panel(
            title = tr("instructions", language$language),
            tags$p(HTML(tr("view_data_instructions_discrete", language$language))),
            tags$div(style = "height: 10px;"),
          )
        )
      )
    }) |> # End of renderUI for instructions
      bindEvent(language$language)
    
    output$sidebar <- renderUI({
      req(filteredData, language)
      
      render_flags$date_range <- TRUE
      render_flags$locations <- TRUE
      render_flags$sub_locations <- TRUE
      render_flags$media <- TRUE
      render_flags$sample_types <- TRUE
      render_flags$params <- TRUE
      
      tags <- tagList(
        # start and end datetime
        dateRangeInput(ns("date_range"),
                       tr("date_range_lab", language$language),
                       start = as.Date(filteredData$range$min_date),
                       end = as.Date(filteredData$range$max_date),
                       min = as.Date(filteredData$range$min_date),
                       format = "yyyy-mm-dd",
                       language = language$abbrev,
                       separator = tr("date_sep", language$language)
        ),
        # Selectize input for locations
        selectizeInput(ns("locations"),
                       label = tr("loc(s)", language$language),
                       choices = 
                         stats::setNames(c("all", filteredData$locs$location_id),
                                         c(tr("all", language$language), filteredData$locs[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = if (!is.null(moduleInputs$location_id)) moduleInputs$location_id else "all"
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
                       choices = 
                         stats::setNames(c("all", filteredData$sub_locs$sub_location_id),
                                         c(tr("all", language$language), filteredData$sub_locs[, tr("sub_location_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for media type
        selectizeInput(ns("media"),
                       label = tr("media_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", filteredData$media$media_id),
                                         c(tr("all", language$language), filteredData$media[, tr("media_type_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        # Selectize input for sample types
        selectizeInput(ns("sample_types"),
                       label = tr("sample_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                         c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        selectizeInput(ns("params"),
                       label = tr("parameter(s)", language$language),
                       choices = 
                         stats::setNames(c("all", filteredData$params$parameter_id),
                                         c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
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
                       style = "font-size: 14px;", 
                       class = "btn btn-primary"
          ),
          actionButton(ns("filter"),
                       label = tr("view_samples", language$language),
                       width = "100%",
                       style = "font-size: 14px;", 
                       class = "btn btn-primary"
          )
        )
      ) # End of tagList
      
      input_values$date_range <- input$date_range
      input_values$locations <- input$locations
      input_values$sub_locations <- input$sub_locations
      input_values$media <- input$media
      input_values$sample_types <- input$sample_types
      input_values$params <- input$params
      
      return(tags)
    })  |> # End of renderUI for sidebar
      bindEvent(language$language)
    
    output$main <- renderUI({
      tagList(
        DT::DTOutput(ns("tbl")), # Table with sample data, filtered by the sidebar inputs
        actionButton(ns("select_all"), tr("select_all", language$language), style = "display: none;"),  # Button will be hidden until a row is selected
        actionButton(ns("view_data"), tr("view_data1", language$language), style =  "display: none;"),  # Button will be hidden until a row is selected
        # The modal UI elements are created lower down
      ) # End of tagList
    }) |> # End renderUI
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
    observeFilterInput("sample_types")
    
    # Flags to prevent running observers unecessarily when the 'reset' button is pressed
    reset_flags <- reactiveValues(date_range = FALSE,
                                  locations = FALSE,
                                  sub_locations = FALSE,
                                  media = FALSE,
                                  sample_types = FALSE,
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
      updateDateRangeInput(session, "date_range",
                           start = as.Date(moduleData$range$min_date),
                           end = as.Date(moduleData$range$max_date),
                           min = as.Date(moduleData$range$min_date),
                           max = as.Date(moduleData$range$max_date)
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
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", moduleData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), moduleData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", moduleData$params$parameter_id),
                                                     c(tr("all", language$language), moduleData$params[, tr("param_name_col", language$language)])),
                           selected = "all")
      
      reset_flags$date_range <- TRUE
      reset_flags$locations <- TRUE
      reset_flags$sub_locations <- TRUE
      reset_flags$media <- TRUE
      reset_flags$sample_types <- TRUE
      reset_flags$params <- TRUE
      
      # Reset the input values to their original state
      input_values$date_range <- input$date_range
      input_values$locations <- input$locations
      input_values$sub_locations <- input$sub_locations
      input_values$media <- input$media
      input_values$sample_types <- input$sample_types
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
      
      remain_locs <- filteredData$locs
      
      if (!("all" %in% input$networks)) {
        net_ids <- filteredData$locations_networks$location_id[
          filteredData$locations_networks$network_id %in% input$networks
        ]
        remain_locs <- remain_locs[remain_locs$location_id %in% net_ids, ]
      }
      
      if (!("all" %in% input$projects)) {
        proj_ids <- filteredData$locations_projects$location_id[
          filteredData$locations_projects$project_id %in% input$projects
        ]
        remain_locs <- remain_locs[remain_locs$location_id %in% proj_ids, ]
      }
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
      
      remain_params <- filteredData$parameter_relationships
      if (!("all" %in% input$pGrps)) {
        remain_params <- remain_params[remain_params$group_id %in% input$pGrps, ]
      }
      if (!("all" %in% input$pSubGrps)) {
        remain_params <- remain_params[remain_params$sub_group_id %in% input$pSubGrps, ]
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
    ### date_range application button and filter ############
    observeEvent(input$date_range, {
      req(input$date_range, filteredData)
      
      # Flags to prevent running the observer when the date range is reset or initially rendered
      if (reset_flags$date_range) {
        reset_flags$date_range <- FALSE
        return()
      }
      if (render_flags$date_range) {
        render_flags$date_range <- FALSE
        return()
      }
      
      # Since this is the top-level filter, we reset the data.
      # There's no need to check for 0 samples here, because the date range is always valid.
      newData <- createFilteredData()
      # overwrite each slot in the reactiveValues
      for (nm in names(newData)) {
        filteredData[[nm]] <- newData[[nm]]
      }
      
      # clear out the results table
      table_data(NULL)
      
      # Filter the data based on the selected date range
      if (is.na(input$date_range[1]) || is.na(input$date_range[2])) {
        return()
      }
      filteredData$range$min_date <- as.POSIXct(input$date_range[1], tz = "UTC")
      filteredData$range$max_date <- as.POSIXct(paste0(input$date_range[2], " 23:59:59"), tz = "UTC")
      
      filteredData$samples <- filteredData$samples[filteredData$samples$datetime >= input$date_range[1] & filteredData$samples$datetime <= input$date_range[2], ]
      filteredData$locs <- filteredData$locs[filteredData$locs$location_id %in% filteredData$samples$location_id, ]
      filteredData$sub_locs <- filteredData$sub_locs[filteredData$sub_locs$location_id %in% filteredData$locs$location_id, ]
      filteredData$media <- filteredData$media[filteredData$media$media_id %in% filteredData$samples$media_id, ]
      filteredData$sample_types <- filteredData$sample_types[filteredData$sample_types$sample_type_id %in% filteredData$samples$sample_type, ]
      
      remain_projects <- filteredData$locations_projects[filteredData$locations_projects$location_id %in% filteredData$locs$location_id, ]
      filteredData$projects <- filteredData$projects[filteredData$projects$project_id %in% remain_projects$project_id, ]
      remain_networks <- filteredData$locations_networks[filteredData$locations_networks$location_id %in% filteredData$locs$location_id, ]
      filteredData$networks <- filteredData$networks[filteredData$networks$network_id %in% remain_networks$network_id, ]
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = character(), sub_group_name_fr = character(), description = character(), description_fr = character())
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
      updateSelectizeInput(session, "media",
                           choices = stats::setNames(c("all", filteredData$media$media_id),
                                                     c(tr("all", language$language), filteredData$media[, tr("media_type_col", language$language)])),
                           selected = if (input$media %in% filteredData$media$media_id) input$media else "all"
      )
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = if (input$sample_types %in% filteredData$sample_types$sample_type_id) input$sample_types else "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )
      
      input_values$date_range <- input$date_range
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
      stash <- filteredData$samples
      filteredData$samples <- moduleData$samples[moduleData$samples$location_id %in% input$locations, ]
      if (nrow(filteredData$samples) == 0) {
        # If no samples are found, reset the samples to the original data, show a notification, and reset input$locations to its previous value
        filteredData$samples <- stash
        updateSelectizeInput(session, "locations",
                             selected = input_values$locations
        )
        showNotification(tr("no_samps_locs", language$language), type = "error", duration = 5)
        return()
      }
      
      filteredData$locs <- moduleData$locs[moduleData$locs$location_id %in% input$locations, ]
      filteredData$sub_locs <- moduleData$sub_locs[moduleData$sub_locs$location_id %in% filteredData$locs$location_id, ]
      filteredData$media <- moduleData$media[moduleData$media$media_id %in% filteredData$samples$media_id, ]
      filteredData$sample_types <- moduleData$sample_types[moduleData$sample_types$sample_type_id %in% filteredData$samples$sample_type, ]
      
      remain_projects <- moduleData$locations_projects[moduleData$locations_projects$location_id %in% input$locations, ]
      filteredData$projects <- moduleData$projects[moduleData$projects$project_id %in% remain_projects$project_id, ]
      remain_networks <- moduleData$locations_networks[moduleData$locations_networks$location_id %in% input$locations, ]
      filteredData$networks <- moduleData$networks[moduleData$networks$network_id %in% remain_networks$network_id, ]
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = character(), sub_group_name_fr = character(), description = character(), description_fr = character())
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
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = if (input$sample_types %in% filteredData$sample_types$sample_type_id) input$sample_types else "all"
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
      stash <- filteredData$samples
      filteredData$samples <- moduleData$samples[moduleData$samples$sub_location_id %in% input$sub_locations, ]
      if (nrow(filteredData$samples) == 0) {
        # If no samples are found, reset the samples to the original data, show a notification, and reset input$sub_locations to its previous value
        filteredData$samples <- stash
        updateSelectizeInput(session, "sub_locations",
                             selected = input_values$sub_locations
        )
        showNotification(tr("no_samps_sub_locs", language$language), type = "error", duration = 5)
        return()
      }
      
      filteredData$sub_locs <- moduleData$sub_locs[moduleData$sub_locs$sub_location_id %in% input$sub_locations, ]
      filteredData$media <- moduleData$media[moduleData$media$media_id %in% filteredData$samples$media_id, ]
      filteredData$sample_types <- moduleData$sample_types[moduleData$sample_types$sample_type_id %in% filteredData$samples$sample_type, ]
      
      # No impact on projects or networks as these are location based and we're past that point
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = character(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      # Update the downstream selectize inputs with the filtered data
      updateSelectizeInput(session, "media",
                           choices = stats::setNames(c("all", filteredData$media$media_id),
                                                     c(tr("all", language$language), filteredData$media[, tr("media_type_col", language$language)])),
                           selected = if (input$media %in% filteredData$media$media_id) input$media else "all"
      )
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = if (input$sample_types %in% filteredData$sample_types$sample_type_id) input$sample_types else "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )
      
      # Reset the input values reactiveValues to the current selection
      input_values$sub_locations <- input$sub_locations
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
      stash <- filteredData$samples
      filteredData$samples <- moduleData$samples[moduleData$samples$media_id %in% input$media, ]
      if (nrow(filteredData$samples) == 0) {
        # If no samples are found, reset the samples to the original data, show a notification, and reset input$media to its previous value
        filteredData$samples <- stash
        updateSelectizeInput(session, "media",
                             selected = input_values$media
        )
        showNotification(tr("no_samps_medias", language$language), type = "error", duration = 5)
        return()
      }
      
      filteredData$media <- moduleData$media[moduleData$media$media_id %in% input$media, ]
      filteredData$sample_types <- moduleData$sample_types[moduleData$sample_types$sample_type_id %in% filteredData$samples$sample_type, ]
      
      # No impact on projects or networks as we're no longer narrowing locations
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = character(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      # Update the downstream selectize inputs with the filtered data
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = if (input$sample_types %in% filteredData$sample_types$sample_type_id) input$sample_types else "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )
      
      # Reset the input values reactiveValues to the current selection
      input_values$media <- input$media
    }, ignoreInit = TRUE)
    
    ### sample_types filter #########
    observeEvent(input$sample_types, {
      req(input$sample_types, filteredData)
      
      # Flags to prevent running the observer when the sample types are reset or initially rendered
      if (reset_flags$sample_types) {
        reset_flags$sample_types <- FALSE
        return()
      }
      if (render_flags$sample_types) {
        render_flags$sample_types <- FALSE
        return()
      }
      
      # Filter the data based on the selected sample types
      stash <- filteredData$samples
      filteredData$samples <- moduleData$samples[moduleData$samples$sample_type %in% input$sample_types, ]
      if (nrow(filteredData$samples) == 0) {
        # If no samples are found, reset the samples to the original data, show a notification, and reset input$sample_types to its previous value
        filteredData$samples <- stash
        updateSelectizeInput(session, "sample_types",
                             selected = input_values$sample_types
        )
        showNotification(tr("no_samps_types", language$language), type = "error", duration = 5)
        return()
      }
      
      filteredData$sample_types <- moduleData$sample_types[moduleData$sample_types$sample_type_id %in% input$sample_types, ]
      
      # No impact on projects or networks as we're no longer narrowing locations
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(sub_groups, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = character(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      # Update the downstream selectize inputs with the filtered data
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = if (input$params %in% filteredData$params$parameter_id) input$params else "all"
      )
      
      # Reset the input values reactiveValues to the current selection
      input_values$sample_types <- input$sample_types
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
    observeEvent(input$filter, {
      req(filteredData, language$language)
      samples <- dbGetQueryDT(
        session$userData$AquaCache, 
        paste0(
          "SELECT DISTINCT ON (s.sample_id)
          s.sample_id, l.location_id, l.name, sl.sub_location_name, s.z AS depth, s.datetime AS sample_datetime_utc, s.target_datetime AS target_datetime_utc, m.media_type, st.sample_type, cm.collection_method, s.sample_volume_ml, s.purge_volume_l, s.purge_time_min, s.flow_rate_l_min, s.wave_hgt_m, s.sample_grade, s.sample_approval, s.sample_qualifier, s.note FROM samples s 
           JOIN results r ON s.sample_id = r.sample_id
           JOIN locations l ON s.location_id = l.location_id
           LEFT JOIN sub_locations sl ON s.sub_location_id = sl.sub_location_id
           JOIN media_types m ON s.media_id = m.media_id
           JOIN sample_types st ON s.sample_type = st.sample_type_id
           JOIN parameters p ON r.parameter_id = p.parameter_id
           JOIN collection_methods cm ON s.collection_method = cm.collection_method_id
           LEFT JOIN organizations orgs ON s.owner = orgs.organization_id
           WHERE s.location_id IN (", 
          paste(filteredData$locs$location_id, collapse = ", "), 
          if (nrow(filteredData$sub_locs) > 0) 
            paste0(") AND s.sub_location_id IN (", paste(filteredData$sub_locs$sub_location_id, collapse = ", "), ")") 
          else 
            ")", 
          " AND s.sample_type IN (", paste(filteredData$sample_types$sample_type_id, collapse = ", "), ")",
          " AND s.media_id IN (", paste(filteredData$media$media_id, collapse = ", "), ")",
          " AND s.datetime BETWEEN '", filteredData$range$min_date, "' AND '", filteredData$range$max_date, "'",
          " AND r.parameter_id IN (", paste(filteredData$params$parameter_id, collapse = ", "), ")",
          ";"
        )
      )
      
      # Drop columns with all NA values
      na_cols <- names(samples)[sapply(samples, function(x) all(is.na(x)))]
      samples[, (na_cols) := NULL]      
      
      table_data(samples)
    })
    
    # 
    observe({ # Render the table
      req(table_data())
      
      out_tbl <- DT::datatable(table_data(),
                               rownames = FALSE,
                               selection = "multiple",
                               filter  = "none",
                               options = list(
                                 scrollX = TRUE,
                                 initComplete = htmlwidgets::JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({",
                                   "  'background-color': '#079',",
                                   "  'color': '#fff',",
                                   "  'font-size': '100%',",
                                   # "  'font-family': 'montserrat'", # Unfortunately this isn't as readable as the default font. Left just in case it's needed later.
                                   "});",
                                   "$(this.api().table().body()).css({",
                                   "  'font-size': '90%',",
                                   # "  'font-family': 'nunito-sans'", # Unfortunately this isn't as readable as the default font. Left just in case it's needed later.
                                   "});",
                                   "}"
                                 ),
                                 columnDefs = list(
                                   list(targets = c(0,1), visible = FALSE) #Hides the sample_id and location_id columns. Column index numbers start at 0 here!!!
                                 ),
                                 language = list(
                                   info = tr("tbl_info", language$language),
                                   infoEmpty = tr("tbl_info_empty", language$language),
                                   paginate = list(previous = "", `next` = ""),
                                   search = tr("tbl_search", language$language),
                                   lengthMenu = tr("tbl_length", language$language),
                                   infoFiltered = tr("tbl_filtered", language$language),
                                   zeroRecords = tr("tbl_zero", language$language)
                                 ),
                                 pageLength = 10
                               )
      )
      output$tbl <- DT::renderDT(out_tbl)
      
      
      if (nrow(table_data()) == 0) {
        shinyjs::hide("select_all")
      } else {
        shinyjs::show("select_all")
      }
    }) # End of table render
    # End of creating and rendering table section
    
    # Create the proxy for datatable manipulations
    proxy <- DT::dataTableProxy("tbl")
    
    
    # Show/hide the view button based on if a row is selected #
    observe({
      if (is.null(input$tbl_rows_selected)) {
        shinyjs::hide("view_data")
      } else {
        shinyjs::show("view_data")
        updateActionButton(session, "view_data", label = paste0(tr("view_data1", language$language), " ", length(input$tbl_rows_selected), " ", tr("view_data2_discrete", language$language)))
      }
    })
    
    # Select/deselect all rows in the table #
    select_all <- reactiveVal(FALSE)
    observeEvent(input$select_all, {
      if (select_all()) {
        DT::selectRows(proxy, NULL)
        select_all(FALSE)
      } else {
        DT::selectRows(proxy, seq_len(nrow(table_data())))
        select_all(TRUE)
      }
    })
    
    
    # Show a modal with the data when the view button is clicked ################
    observeEvent(input$view_data, {
      # Get the timeseries_ids of the selected rows
      selected_sampleids <- table_data()[input$tbl_rows_selected, sample_id]
      selected_loc_ids <- table_data()[input$tbl_rows_selected, location_id]
      
      
      # Query will be for discrete or continuous data, depending on input$type
      # Show a modal with a subset (first 3 rows per sample_id) of the data. Below this, show a date range picker (with min/max preset based on the selected data), the number of rows that would be returned, and download and close buttons. The download button will give the user the entire dataset within the date range selected
      
      # Get the sample results
      # Construct a comma-separated string of sample IDs
      sample_ids_str <- paste(selected_sampleids, collapse = ",")
      
      # Single query using a window function to limit to 3 rows/results per sample_id
      query <- paste0(
        "SELECT * FROM (SELECT r.sample_id, r.result, p.param_name AS parameter, p.unit_default AS units, rs.result_speciation, rt.result_type, sf.sample_fraction, rc.result_condition, r.result_condition_value, rvt.result_value_type, pm.protocol_name, l.lab_name AS laboratory, r.analysis_datetime, ROW_NUMBER() OVER (PARTITION BY sample_id ORDER BY result_id) AS rn 
        FROM results r
        JOIN parameters p ON r.parameter_id = p.parameter_id
        JOIN result_types rt ON r.result_type = rt.result_type_id
        LEFT JOIN sample_fractions sf ON r.sample_fraction_id = sf.sample_fraction_id
        LEFT JOIN result_conditions rc ON r.result_condition = rc.result_condition_id
        LEFT JOIN result_value_types rvt ON r.result_value_type = rvt.result_value_type_id
        LEFT JOIN result_speciations rs ON r.result_speciation_id = rs.result_speciation_id
        LEFT JOIN protocols_methods pm ON r.protocol_method = pm.protocol_id
        LEFT JOIN laboratories l ON r.laboratory = l.lab_id
        WHERE sample_id IN (", sample_ids_str, ")) sub WHERE rn <= 3;"
      )
      
      subset <- dbGetQueryDT(session$userData$AquaCache, query)
      subset[, rn := NULL] # Drop column 'rn', left over from the window function
      
      output$modal_subset <- DT::renderDT({  # Create datatable for the measurements
        DT::datatable(subset,
                      rownames = FALSE,
                      selection = "none",
                      filter  = "none",
                      options = list(
                        scrollX = TRUE,
                        initComplete = htmlwidgets::JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({",
                          "  'background-color': '#079',",
                          "  'color': '#fff',",
                          "  'font-size': '100%',",
                          "});",
                          "$(this.api().table().body()).css({",
                          "  'font-size': '90%',",
                          "});",
                          "}"
                        ),
                        language = list(
                          info = "",
                          infoEmpty = tr("tbl_info_empty", language$language),
                          paginate = list(previous = "", `next` = ""),
                          search = tr("tbl_search", language$language),
                          infoFiltered = "",
                          zeroRecords = tr("tbl_zero", language$language)
                        ),
                        layout = list(
                          bottomStart = 'info',
                          bottomEnd   = 'paging'
                        )
                      )
        )
      }) # End of function creating data subset datatable
      
      # Get location metadata
      location <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM ", if (language$abbrev == "fr") "location_metadata_fr" else "location_metadata_en", " WHERE location_id IN (", paste(selected_loc_ids, collapse = ", "), ") LIMIT 3;")) # Get the location metadata
      
      output$modal_location_metadata <- DT::renderDT({  # Create datatable for the locations
        DT::datatable(location,
                      rownames = FALSE,
                      selection = "none",
                      filter  = "none",
                      options = list(
                        initComplete = htmlwidgets::JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({",
                          "  'background-color': '#079',",
                          "  'color': '#fff',",
                          "  'font-size': '100%',",
                          "});",
                          "$(this.api().table().body()).css({",
                          "  'font-size': '90%',",
                          "});",
                          "}"
                        ),
                        columnDefs = list(
                          list(
                            targets = c(2, 6:9), # Column index numbers start at 0 here again!!!
                            render = htmlwidgets::JS( # Truncate long strings in the table
                              "function(data, type, row, meta) {",
                              "return type === 'display' && data !== null && data.length > 20 ?",
                              "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                              "}")
                          )
                        ),
                        language = list(
                          info = "",
                          infoEmpty = tr("tbl_info_empty", language$language),
                          paginate = list(previous = "", `next` = ""),
                          search = "",
                          infoFiltered = "",
                          zeroRecords = tr("tbl_zero", language$language)
                        ),
                        scrollX = TRUE
                      )
        ) |>
          DT::formatRound(columns = c(4,5,5), digits = 3) # Round numbers, here index starts at 1 (not javascript)
      }) # End of function creating location metatadata datatable
      
      
      # Create the modal
      showModal(modalDialog(
        h4(tr("discrete_subset_msg", language$language)),
        DT::DTOutput(ns("modal_subset")),
        h4(tr("loc_meta_msg", language$language)),
        DT::DTOutput(ns("modal_location_metadata")),
        textOutput(ns("num_rows")),
        selectizeInput(ns("modal_format"), label = tr("dl_format", language$language), choices = stats::setNames(c("xlsx", "csv", "sqlite"), c(tr("dl_format_xlsx", language$language), tr("dl_format_csv", language$language), tr("dl_format_sqlite", language$language))), selected = "xlsx"),
        footer = tagList(
          modalButton(tr("close", language$language)),
          actionButton(ns("download"), tr("dl_data", language$language), icon = icon("download"))
        ),
        size = "xl"
      ))
    })
    
    
    
    
    # Updates to modal ########################################################
    # Get the number of rows that will be returned based on the date range selected and update the subset table if necessary
    observe({
      req(input$tbl_rows_selected, filteredData$params, table_data())
      selected_sampleids <- table_data()[input$tbl_rows_selected, sample_id]
      
      rows <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT COUNT(*) FROM results WHERE sample_id IN (", paste(selected_sampleids, collapse = ", "), ") AND parameter_id IN (", paste(filteredData$params$parameter_id, collapse = ", "), ");"))[[1]]
      
      # ouput message about number of rows
      output$num_rows <- renderText({ paste0(tr("dl_num_results", language$language), " ", rows) })
      
      # Update selectizeInput based on number of rows
      if (rows > 1000000) {
        updateSelectizeInput(session, "modal_format", label = tr("dl_format_no_xlsx", language$language),  choices = stats::setNames(c("csv", "sqlite"), c(tr("dl_format_csv", language$language), tr("dl_format_sqlite", language$language))), selected = "csv")
      } else {
        updateSelectizeInput(session, "modal_format", label = tr("dl_format", language$language), choices = stats::setNames(c("xlsx", "csv", "sqlite"), c(tr("dl_format_xlsx", language$language), tr("dl_format_csv", language$language), tr("dl_format_sqlite", language$language))), selected = "xlsx")
      }
    }) # End of observe for number of rows
    
    
    # Download handling #######################################################
    output$download <- downloadHandler(
      filename = function() {
        paste0("discreteData_", format(Sys.time(), "%Y%m%d_%H%M%S%Z"), ".", if (input$modal_format == "csv") "zip" else input$modal_format)
      },
      content = function(file) {
        
        showNotification(tr("dl_prep", language$language), id = "download_notification", duration = NULL, type = "message")
        
        # Get the data together
        selected_sampleids <- table_data()[input$tbl_rows_selected, sample_id]
        
        selected_loc_ids <- table_data()[input$tbl_rows_selected, location_id]
        
        data <- list()
        data$location_metadata <- dbGetQueryDT(session$userData$AquaCache, paste0("SELECT * FROM ", if (language$abbrev == "fr") "location_metadata_fr" else "location_metadata_en", " WHERE location_id IN (", paste(selected_loc_ids, collapse = ", "), ");")) # Get the location metadata
        data$samples <- table_data()
        data$results <- dbGetQueryDT(session$userData$AquaCache, paste0(
          "SELECT s.location_id, r.sample_id, s.datetime, s.target_datetime, r.result, p.param_name AS parameter, p.unit_default AS units, rs.result_speciation, rt.result_type, sf.sample_fraction, rc.result_condition, r.result_condition_value, rvt.result_value_type, pm.protocol_name, l.lab_name AS laboratory, r.analysis_datetime
        FROM results r
        JOIN samples s ON r.sample_id = s.sample_id
        JOIN parameters p ON r.parameter_id = p.parameter_id
        JOIN result_types rt ON r.result_type = rt.result_type_id
        LEFT JOIN sample_fractions sf ON r.sample_fraction_id = sf.sample_fraction_id
        LEFT JOIN result_conditions rc ON r.result_condition = rc.result_condition_id
        LEFT JOIN result_value_types rvt ON r.result_value_type = rvt.result_value_type_id
        LEFT JOIN result_speciations rs ON r.result_speciation_id = rs.result_speciation_id
        LEFT JOIN protocols_methods pm ON r.protocol_method = pm.protocol_id
        LEFT JOIN laboratories l ON r.laboratory = l.lab_id
        WHERE r.sample_id IN (", paste(selected_sampleids, collapse = ","), ");"
        ))
        
        if ("grade" %in% names(data$samples)) {
          data$grades <- dbGetQueryDT(session$userData$AquaCache, "SELECT * FROM grade_types;")
        }
        if ("approval" %in% names(data$samples)) {
          data$approvals <- dbGetQueryDT(session$userData$AquaCache, "SELECT * FROM approval_types;")
        }
        if ("qualifier" %in% names(data$samples)) {
          data$qualifiers <- dbGetQueryDT(session$userData$AquaCache, "SELECT * FROM qualifier_types;")
        }
        
        
        if (input$modal_format == "xlsx") {
          openxlsx::write.xlsx(data, file)
        } else if (input$modal_format == "csv") {
          # Temporary directory to store CSV files
          temp_dir <- tempdir()
          csv_files <- lapply(names(data), function(name) {
            file_name <- file.path(temp_dir, paste0(name, ".csv"))
            data.table::fwrite(data[[name]], file_name)
            return(file_name)
          })
          # Use zip to compress the files
          utils::zip(file, unlist(csv_files))
        } else if (input$modal_format == "sqlite") {
          # Create an sqlite database and write the data tables to it
          db <- DBI::dbConnect(RSQLite::SQLite(), dbname = file)
          lapply(names(data), function(name) {
            DBI::dbWriteTable(conn = db, name = name, value = data[[name]], overwrite = TRUE)
          })
          DBI::dbDisconnect(db)
        }
        removeNotification("download_notification")
      } # End of content function
    ) # End of downloadHandler
    
  }) # End moduleServer
} # End discData function

