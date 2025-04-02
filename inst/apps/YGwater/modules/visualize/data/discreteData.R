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
      sub_locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT sl.sub_location_id, sl.sub_location_name, sl.sub_location_name_fr FROM sub_locations AS sl INNER JOIN locations ON sl.location_id = locations.location_id ORDER BY sl.sub_location_name ASC"),
      params = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id ORDER BY p.param_name ASC;"),
      media_types = DBI::dbGetQuery(session$userData$AquaCache,
                                    "SELECT DISTINCT m.* FROM media_types as m WHERE EXISTS (SELECT 1 FROM samples AS s WHERE m.media_id = s.media_id);"),
      parameter_relationships = DBI::dbGetQuery(session$userData$AquaCache,
                                                "SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id) ;"),
      range = DBI::dbGetQuery(session$userData$AquaCache, "SELECT MIN(datetime) AS min_date, MAX(datetime) AS max_date FROM samples;"),
      sample_types = DBI::dbGetQuery(session$userData$AquaCache, "SELECT st.sample_type_id, st.sample_type, COALESCE(st.sample_type_fr, st.sample_type) AS sample_type_fr FROM sample_types AS st WHERE EXISTS (SELECT 1 FROM samples AS s WHERE st.sample_type_id = s.sample_type);"),
      samples = DBI::dbGetQuery(session$userData$AquaCache, "SELECT sample_id, location_id, sub_location_id, media_id, datetime, sample_type FROM samples;")
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
      sub_locs = isolate(moduleData$sub_locs),
      params = isolate(moduleData$params),
      media_types = isolate(moduleData$media_types),
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
    
    
    # Create UI elements ################
    # NOTE: output$sidebar is rendered at module load time, but also re-rendered whenever a change to the language is made.
    render_flags <- reactiveValues(date_range = FALSE,
                                   locations = FALSE,
                                   sub_locations = FALSE,
                                   media_types = FALSE,
                                   sample_types = FALSE,
                                   params = FALSE)
    
    output$sidebar <- renderUI({
      req(moduleData)
      
      render_flags$date_range <- TRUE
      render_flags$locations <- TRUE
      render_flags$sub_locations <- TRUE
      render_flags$media_types <- TRUE
      render_flags$sample_types <- TRUE
      render_flags$params <- TRUE
      
      tagList(
        # start and end datetime
        dateRangeInput(ns("date_range"),
                       tr("date_range_select", language$language),
                       start = as.Date(moduleData$range$min_date),
                       end = as.Date(moduleData$range$max_date),
                       max = Sys.Date() + 1,
                       format = "yyyy-mm-dd"
        ),
        uiOutput(ns("apply_date_range")),
        
        # Selectize input for locations
        selectizeInput(ns("locations"),
                       label = tr("loc(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$locs$location_id),
                                         c(tr("all", language$language), moduleData$locs[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        uiOutput(ns("apply_locations")),
        
        actionButton(ns("loc_modal"),
                     label = tr("loc_modal", language$language),
                     width = "100%",
                     style = "font-size: 14px; margin-top: -10px;"
        ),
        selectizeInput(ns("sub_locations"),
                       label = tr("sub_loc(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$sub_locs$sub_location_id),
                                         c(tr("all", language$language), moduleData$sub_locs[, tr("sub_location_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        uiOutput(ns("apply_sub_locations")),
        # Selectize input for media type
        selectizeInput(ns("media_types"),
                       label = tr("media_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$media_types$media_id),
                                         c(tr("all", language$language), moduleData$media_types[, tr("media_type_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        uiOutput(ns("apply_media_types")),
        # Selectize input for sample types
        selectizeInput(ns("sample_types"),
                       label = tr("sample_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$sample_types$sample_type_id),
                                         c(tr("all", language$language), moduleData$sample_types[, tr("sample_type_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        uiOutput(ns("apply_sample_types")),
        selectizeInput(ns("params"),
                       label = tr("parameter(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$params$parameter_id),
                                         c(tr("all", language$language), moduleData$params[, tr("param_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        uiOutput(ns("apply_params")),
        actionButton(ns("param_modal"),
                     label = tr("param_modal", language$language),
                     width = "100%",
                     style = "font-size: 14px; margin-top: -10px;"
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
        htmlOutput(ns("instructions")),
        tags$div(style = "height: 10px;"),
        DT::dataTableOutput(ns("tbl")), # Table with sample data, filtered by the sidebar inputs
        actionButton(ns("select_all"), tr("select_all", language$language), style = "display: none;"),  # Button will be hidden until a row is selected
        actionButton(ns("view_data"), "placeholder", style =  "display: none;"),  # Button will be hidden until a row is selected
        # The modal UI elements are created lower down
      ) # End of tagList
    }) %>% # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or changes
    
    output$instructions <- renderUI(tr("view_data_instructions", language$language))
    
    
    
    
    
    
    
    # Functional parts of server ################
    ## Run observeFilterInput for each selectize input where 'all' is an option ##### 
    observeFilterInput("locations")
    observeFilterInput("sub_locations")
    observeFilterInput("media_types")
    observeFilterInput("pGrps")
    observeFilterInput("pSubGrps")
    observeFilterInput("params")
    observeFilterInput("networks")
    observeFilterInput("projects")
    observeFilterInput("sample_types")
    
    reset_flags <- reactiveValues(date_range = FALSE,
                                  locations = FALSE,
                                  sub_locations = FALSE,
                                  media_types = FALSE,
                                  sample_types = FALSE,
                                  params = FALSE)
    
    ## Reset button observer ############
    observeEvent(input$reset, {
      # Reset the filteredData to its original state
      filteredData <- reactiveValues(
        locs = isolate(moduleData$locs),
        sub_locs = isolate(moduleData$sub_locs),
        params = isolate(moduleData$params),
        media_types = isolate(moduleData$media_types),
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
      
      updateDateRangeInput(session, "date_range",
                           start = as.Date(moduleData$range$min_date),
                           end = as.Date(moduleData$range$max_date),
                           max = Sys.Date() + 1
      )
      updateSelectizeInput(session, "locations",
                           choices = stats::setNames(c("all", moduleData$locs$location_id),
                                                     c(tr("all", language$language), moduleData$locs[, tr("generic_name_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "sub_locations",
                           choices = stats::setNames(c("all", moduleData$sub_locs$sub_location_id),
                                                     c(tr("all", language$language), moduleData$sub_locs[, tr("sub_location_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "media_types",
                           choices = stats::setNames(c("all", moduleData$media_types$media_id),
                                                     c(tr("all", language$language), moduleData$media_types[, tr("media_type_col", language$language)])),
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
      reset_flags$media_types <- TRUE
      reset_flags$sample_types <- TRUE
      reset_flags$params <- TRUE
      
      # re-enable the disabled inputs
      shinyjs::enable("date_range")
      shinyjs::enable("locations")
      shinyjs::enable("sub_locations")
      shinyjs::enable("media_types")
      shinyjs::enable("sample_types")
      
      shinyjs::show("loc_modal")
      shinyjs::show("param_modal")
      
      # Hide all visible 'apply' modals
      buttons_applied$apply_date_range <- FALSE
      shinyjs::hide("apply_date_range")
      buttons_applied$apply_locations <- FALSE
      shinyjs::hide("apply_locations")
      buttons_applied$apply_sub_locations <- FALSE
      shinyjs::hide("apply_sub_locations")
      buttons_applied$apply_media_types <- FALSE
      shinyjs::hide("apply_media_types")
      buttons_applied$apply_sample_types <- FALSE
      shinyjs::hide("apply_sample_types")
      buttons_applied$apply_params <- FALSE
      shinyjs::hide("apply_params")
      
      table_data <- reactiveVal()
      
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
                           selected = "all"
      )
      
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
        remain_params <- filteredData$parameter_relationships[filteredData$parameter_relationships$subgroup_id %in% input$pSubGrps, ]
      }
      remain_params <- filteredData$params[filteredData$params$parameter_id %in% remain_params$parameter_id, ]
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", remain_params$parameter_id),
                                                     c(tr("all", language$language), remain_params[, tr("param_name_col", language$language)])),
                           selected = "all"
      )
      removeModal()
    })
    
    
    # Now apply filters top to bottom. If date range gets narrowed, the other filters will be applied to the narrowed date range. If locations gets narrowed, the other filters will be applied to the narrowed locations EXCEPT for date range, and so on down the line.
    
    ## Filters ###########
    # Flags to determine if the apply buttons have been rendered
    buttons <- reactiveValues(apply_date_range = FALSE,
                              apply_locations = FALSE,
                              apply_sub_locations = FALSE,
                              apply_media_types = FALSE,
                              apply_sample_types = FALSE,
                              apply_params = FALSE)
    
    # Flags to determine if the apply buttons have been clicked
    buttons_applied <- reactiveValues(apply_date_range = FALSE,
                                      apply_locations = FALSE,
                                      apply_sub_locations = FALSE,
                                      apply_media_types = FALSE,
                                      apply_sample_types = FALSE,
                                      apply_params = FALSE)
    
    ### date_range application button and filter ############
    observeEvent(input$date_range, {
      if (reset_flags$date_range) {
        reset_flags$date_range <- FALSE
        return()
      }
      if (render_flags$date_range) {
        render_flags$date_range <- FALSE
        return()
      }
      if (buttons_applied$apply_date_range) {
        return()
      }
      if (buttons$apply_date_range) {
        shinyjs::show("apply_date_range")
      } else {
        output$apply_date_range <- renderUI({
          actionButton(ns("apply_date_range_btn"), tr("apply", language$language), style = "display: block; margin-left: auto; margin-right: 0;")
        })
        buttons$apply_date_range <- TRUE
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_date_range_btn, {
      req(input$date_range, filteredData)
      
      # disable the input
      shinyjs::disable("date_range")
      shinyjs::hide("apply_date_range")
      buttons_applied$apply_date_range <- TRUE
      
      filteredData$range$min_date <- input$date_range[1]
      filteredData$range$max_date <- input$date_range[2]
      
      filteredData$samples <- filteredData$samples[filteredData$samples$datetime >= input$date_range[1] & filteredData$samples$datetime <= input$date_range[2], ]
      filteredData$locs <- filteredData$locs[filteredData$locs$location_id %in% filteredData$samples$location_id, ]
      filteredData$sub_locs <- filteredData$sub_locs[filteredData$sub_locs$location_id %in% filteredData$locs$location_id, ]
      filteredData$media_types <- filteredData$media_types[filteredData$media_types$media_id %in% filteredData$samples$media_id, ]
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
        if (length(filteredData$parameter_relationships$subgroup_id) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(filteredData$parameter_relationships$subgroup_id, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      
      updateSelectizeInput(session, "locations",
                           choices = stats::setNames(c("all", filteredData$locs$location_id),
                                                     c(tr("all", language$language), filteredData$locs[, tr("generic_name_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "sub_locations",
                           choices = stats::setNames(c("all", filteredData$sub_locs$sub_location_id),
                                                     c(tr("all", language$language), filteredData$sub_locs[, tr("sub_location_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "media_types",
                           choices = stats::setNames(c("all", filteredData$media_types$media_id),
                                                     c(tr("all", language$language), filteredData$media_types[, tr("media_type_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = "all"
      )
    }, ignoreInit = TRUE)
    
    
    ### locations filter ############
    observeEvent(input$locations, {
      if (reset_flags$locations) {
        reset_flags$locations <- FALSE
        return()
      }
      if (render_flags$locations) {
        render_flags$locations <- FALSE
        return()
      }
      if (buttons_applied$apply_locations) {
        return()
      }
      if (buttons$apply_locations) {
        shinyjs::show("apply_locations")
      } else {
        output$apply_locations <- renderUI({
          actionButton(ns("apply_locations_btn"), tr("apply", language$language), style = "display: block; margin-left: auto; margin-right: 0; margin-top: -10px; margin-bottom: 15px;")
        })
        buttons$apply_locations <- TRUE
      }
      
      # Check if apply_date_range_btn was rendered and not clicked. If so, reset the date range to what's in the filteredData and hide the button
      if (!buttons_applied$apply_date_range) {
        buttons_applied$apply_date_range <- TRUE
        updateDateRangeInput(session, "date_range",
                             start = as.Date(filteredData$range$min_date),
                             end = as.Date(filteredData$range$max_date),
                             max = Sys.Date() + 1
        )
        shinyjs::hide("apply_date_range")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_locations_btn, {
      req(input$locations, filteredData)
      
      # Disable the inputs for date range and locations and hide the apply button
      shinyjs::disable("date_range")
      shinyjs::disable("locations")
      shinyjs::hide("apply_locations")
      buttons_applied$apply_locations <- TRUE
      
      shinyjs::hide("loc_modal")
      
      
      filteredData$samples <- filteredData$samples[filteredData$samples$location_id %in% input$locations, ]
      filteredData$locs <- filteredData$locs[filteredData$locs$location_id %in% input$locations, ]
      filteredData$sub_locs <- filteredData$sub_locs[filteredData$sub_locs$location_id %in% filteredData$locs$location_id, ]
      filteredData$media_types <- filteredData$media_types[filteredData$media_types$media_id %in% filteredData$samples$media_id, ]
      filteredData$sample_types <- filteredData$sample_types[filteredData$sample_types$sample_type_id %in% filteredData$samples$sample_type, ]
      
      remain_projects <- filteredData$locations_projects[filteredData$locations_projects$location_id %in% input$locations, ]
      filteredData$projects <- filteredData$projects[filteredData$projects$project_id %in% remain_projects$project_id, ]
      remain_networks <- filteredData$locations_networks[filteredData$locations_networks$location_id %in% input$locations, ]
      filteredData$networks <- filteredData$networks[filteredData$networks$network_id %in% remain_networks$network_id, ]
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        if (length(filteredData$parameter_relationships$subgroup_id) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(filteredData$parameter_relationships$subgroup_id, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      updateSelectizeInput(session, "sub_locations",
                           choices = stats::setNames(c("all", filteredData$sub_locs$sub_location_id),
                                                     c(tr("all", language$language), filteredData$sub_locs[, tr("sub_location_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "media_types",
                           choices = stats::setNames(c("all", filteredData$media_types$media_id),
                                                     c(tr("all", language$language), filteredData$media_types[, tr("media_type_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = "all"
      )
    }, ignoreInit = TRUE)
    
    ### sub_locations filter #########
    observeEvent(input$sub_locations, {
      if (reset_flags$sub_locations) {
        reset_flags$sub_locations <- FALSE
        return()
      }
      if (render_flags$sub_locations) {
        render_flags$sub_locations <- FALSE
        return()
      }
      if (buttons_applied$apply_sub_locations) {
        return()
      }
      if (buttons$apply_sub_locations) {
        shinyjs::show("apply_sub_locations")
      } else {
        output$apply_sub_locations <- renderUI({
          actionButton(ns("apply_sub_locations_btn"), tr("apply", language$language), style = "display: block; margin-left: auto; margin-right: 0;")
        })
        buttons$apply_sub_locations <- TRUE
      }
      
      # Check if apply_date_range_btn and apply_locations_btn were rendered and not clicked. If so, reset both and hide the buttons
      if (!buttons_applied$apply_date_range) {
        buttons_applied$apply_date_range <- TRUE
        updateDateRangeInput(session, "date_range",
                             start = as.Date(filteredData$range$min_date),
                             end = as.Date(filteredData$range$max_date),
                             max = Sys.Date() + 1
        )
        shinyjs::hide("apply_date_range")
      }
      if (!buttons_applied$apply_locations) {
        buttons_applied$apply_locations <- TRUE
        updateSelectizeInput(session, "locations",
                             selected = "all"
        )
        shinyjs::hide("apply_locations")
      }
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_sub_locations_btn, {
      req(input$sub_locations, filteredData)
      
      # Disable the inputs for date range, locations, and sub-locations and hide the apply button
      shinyjs::disable("date_range")
      shinyjs::disable("locations")
      shinyjs::disable("sub_locations")
      shinyjs::hide("apply_sub_locations")
      buttons_applied$apply_sub_locations <- TRUE
      
      filteredData$samples <- filteredData$samples[filteredData$samples$sub_location_id %in% input$sub_locations, ]
      filteredData$sub_locs <- filteredData$sub_locs[filteredData$sub_locs$sub_location_id %in% input$sub_locations, ]
      filteredData$media_types <- filteredData$media_types[filteredData$media_types$media_id %in% filteredData$samples$media_id, ]
      filteredData$sample_types <- filteredData$sample_types[filteredData$sample_types$sample_type_id %in% filteredData$samples$sample_type, ]
      
      # No impact on projects or networks as these are location based and we're past that point
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        if (length(filteredData$parameter_relationships$subgroup_id) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(filteredData$parameter_relationships$subgroup_id, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      updateSelectizeInput(session, "media_types",
                           choices = stats::setNames(c("all", filteredData$media_types$media_id),
                                                     c(tr("all", language$language), filteredData$media_types[, tr("media_type_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = "all"
      )
    }, ignoreInit = TRUE)
    
    
    ### media_types filter #########
    observeEvent(input$media_types, {
      if (reset_flags$media_types) {
        reset_flags$media_types <- FALSE
        return()
      }
      if (render_flags$media_types) {
        render_flags$media_types <- FALSE
        return()
      }
      if (buttons_applied$apply_media_types) {
        return()
      }
      if (buttons$apply_media_types) {
        shinyjs::show("apply_media_types")
      } else {
        output$apply_media_types <- renderUI({
          actionButton(ns("apply_media_types_btn"), tr("apply", language$language), style = "display: block; margin-left: auto; margin-right: 0;")
        })
        buttons$apply_media_types <- TRUE
      }
      
      # Check if apply_date_range_btn, apply_locations_btn, apply_sub_locations_btn were rendered and not clicked. If so, reset and hide the buttons
      if (!buttons_applied$apply_date_range) {
        buttons_applied$apply_date_range <- TRUE
        updateDateRangeInput(session, "date_range",
                             start = as.Date(filteredData$range$min_date),
                             end = as.Date(filteredData$range$max_date),
                             max = Sys.Date() + 1
        )
        shinyjs::hide("apply_date_range")
      }
      if (!buttons_applied$apply_locations) {
        buttons_applied$apply_locations <- TRUE
        updateSelectizeInput(session, "locations",
                             selected = "all"
        )
        shinyjs::hide("apply_locations")
      }
      if (!buttons_applied$apply_sub_locations) {
        buttons_applied$apply_sub_locations <- TRUE
        updateSelectizeInput(session, "sub_locations",
                             selected = "all"
        )
        shinyjs::hide("apply_sub_locations")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_media_types_btn, {
      req(input$media_types, filteredData)
      
      # Disable the inputs for date range, locations, sub-locations, and media types and hide the apply button
      shinyjs::disable("date_range")
      shinyjs::disable("locations")
      shinyjs::disable("sub_locations")
      shinyjs::disable("media_types")
      shinyjs::hide("apply_media_types")
      buttons_applied$apply_media_types <- TRUE
      
      filteredData$samples <- filteredData$samples[filteredData$samples$media_id %in% input$media_types, ]
      filteredData$media_types <- filteredData$media_types[filteredData$media_types$media_id %in% input$media_types, ]
      filteredData$sample_types <- filteredData$sample_types[filteredData$sample_types$sample_type_id %in% filteredData$samples$sample_type, ]
      
      # No impact on projects or networks as we're no longer narrowing locations
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        if (length(filteredData$parameter_relationships$subgroup_id) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(filteredData$parameter_relationships$subgroup_id, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      updateSelectizeInput(session, "sample_types",
                           choices = stats::setNames(c("all", filteredData$sample_types$sample_type_id),
                                                     c(tr("all", language$language), filteredData$sample_types[, tr("sample_type_col", language$language)])),
                           selected = "all"
      )
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = "all"
      )
      
    }, ignoreInit = TRUE)
    
    ### sample_types filter #########
    observeEvent(input$sample_types, {
      if (reset_flags$sample_types) {
        reset_flags$sample_types <- FALSE
        return()
      }
      if (render_flags$sample_types) {
        render_flags$sample_types <- FALSE
        return()
      }
      if (buttons_applied$apply_sample_types) {
        return()
      }
      if (buttons$apply_sample_types) {
        shinyjs::show("apply_sample_types")
      } else {
        output$apply_sample_types <- renderUI({
          actionButton(ns("apply_sample_types_btn"), tr("apply", language$language), style = "display: block; margin-left: auto; margin-right: 0;")
        })
        buttons$apply_sample_types <- TRUE
      }
      
      # Check if apply_date_range_btn, apply_locations_btn, apply_sub_locations_btn, and apply_media_types_btn were rendered and not clicked. If so, reset and hide the buttons
      if (!buttons_applied$apply_date_range) {
        buttons_applied$apply_date_range <- TRUE
        updateDateRangeInput(session, "date_range",
                             start = as.Date(filteredData$range$min_date),
                             end = as.Date(filteredData$range$max_date),
                             max = Sys.Date() + 1
        )
        shinyjs::hide("apply_date_range")
      }
      if (!buttons_applied$apply_locations) {
        buttons_applied$apply_locations <- TRUE
        updateSelectizeInput(session, "locations",
                             selected = "all"
        )
        shinyjs::hide("apply_locations")
      }
      if (!buttons_applied$apply_sub_locations) {
        buttons_applied$apply_sub_locations <- TRUE
        updateSelectizeInput(session, "sub_locations",
                             selected = "all"
        )
        shinyjs::hide("apply_sub_locations")
      }
      if (!buttons_applied$apply_media_types) {
        buttons_applied$apply_media_types <- TRUE
        updateSelectizeInput(session, "media_types",
                             selected = "all"
        )
        shinyjs::hide("apply_media_types")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_sample_types_btn, {
      req(input$sample_types, filteredData)
      
      # Disable the inputs for date range, locations, sub-locations, media types, and sample types and hide the apply button
      shinyjs::disable("date_range")
      shinyjs::disable("locations")
      shinyjs::disable("sub_locations")
      shinyjs::disable("media_types")
      shinyjs::disable("sample_types")
      shinyjs::hide("apply_sample_types")
      buttons_applied$apply_sample_types <- TRUE
      
      filteredData$samples <- filteredData$samples[filteredData$samples$sample_type %in% input$sample_types, ]
      filteredData$sample_types <- filteredData$sample_types[filteredData$sample_types$sample_type_id %in% input$sample_types, ]
      
      # No impact on projects or networks as we're no longer narrowing locations
      
      filteredData$params <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id WHERE r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), ");"))
      
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT p.* FROM parameter_relationships AS p WHERE EXISTS (SELECT 1 FROM results AS r WHERE p.parameter_id = r.parameter_id AND r.sample_id IN (", paste(filteredData$samples$sample_id, collapse = ", "), "));"))
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_groups WHERE group_id IN (", paste(filteredData$parameter_relationships$group_id, collapse = ", "), ");"))
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        if (length(filteredData$parameter_relationships$subgroup_id) > 0) {
          filteredData$param_sub_groups <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM parameter_sub_groups WHERE sub_group_id IN (", paste(filteredData$parameter_relationships$subgroup_id, collapse = ", "), ");"))
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", filteredData$params$parameter_id),
                                                     c(tr("all", language$language), filteredData$params[, tr("param_name_col", language$language)])),
                           selected = "all"
      )
    }, ignoreInit = TRUE)
    
    
    ### params filter #########
    observeEvent(input$params, {
      if (reset_flags$params) {
        reset_flags$params <- FALSE
        return()
      }
      if (render_flags$params) {
        render_flags$params <- FALSE
        return()
      }
      if (buttons_applied$apply_params) {
        return()
      }
      if (buttons$apply_params) {
        shinyjs::show("apply_params")
      } else {
        output$apply_params <- renderUI({
          actionButton(ns("apply_params_btn"), tr("apply", language$language), style = "display: block; margin-left: auto; margin-right: 0;")
        })
        buttons$apply_params <- TRUE
      }
      
      # Check if apply_date_range_btn, apply_locations_btn, apply_sub_locations_btn, apply_media_types_btn, and apply_sample_types_btn were rendered and not clicked. If so, reset and hide the buttons
      if (!buttons_applied$apply_date_range) {
        buttons_applied$apply_date_range <- TRUE
        updateDateRangeInput(session, "date_range",
                             start = as.Date(filteredData$range$min_date),
                             end = as.Date(filteredData$range$max_date),
                             max = Sys.Date() + 1
        )
        shinyjs::hide("apply_date_range")
        
      }
      if (!buttons_applied$apply_locations) {
        buttons_applied$apply_locations <- TRUE
        updateSelectizeInput(session, "locations",
                             selected = "all"
        )
        shinyjs::hide("apply_locations")
      }
      if (!buttons_applied$apply_sub_locations) {
        buttons_applied$apply_sub_locations <- TRUE
        updateSelectizeInput(session, "sub_locations",
                             selected = "all"
        )
        shinyjs::hide("apply_sub_locations")
      }
      if (!buttons_applied$apply_media_types) {
        buttons_applied$apply_media_types <- TRUE
        updateSelectizeInput(session, "media_types",
                             selected = "all"
        )
        shinyjs::hide("apply_media_types")
      }
      if (!buttons_applied$apply_sample_types) {
        buttons_applied$apply_sample_types <- TRUE
        updateSelectizeInput(session, "sample_types",
                             selected = "all"
        )
        shinyjs::hide("apply_sample_types")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_params_btn, {
      req(input$params, filteredData)
      
      # Disable the inputs for date range, locations, sub-locations, media types, sample types, and params and hide the apply button
      shinyjs::disable("date_range")
      shinyjs::disable("locations")
      shinyjs::disable("sub_locations")
      shinyjs::disable("media_types")
      shinyjs::disable("sample_types")
      shinyjs::disable("params")
      shinyjs::hide("apply_params")
      buttons_applied$apply_params <- TRUE
      
      shinyjs::hide("param_modal")
      
      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% input$params, ]
      
      # No impact on projects or networks as we're no longer narrowing locations
      
      # No impact on parameters as we're no longer narrowing parameters
      
    }, ignoreInit = TRUE)
    
    
    
    
    # Create the samples table and render it ###################################
    # The results table will be shown only if the user clicks on the 'view results' button in the modal
    table_data <- reactiveVal()
    observeEvent(input$filter, {
      req(filteredData)
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
          " AND s.media_id IN (", paste(filteredData$media_types$media_id, collapse = ", "), ")",
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
                                   list(targets = c(0,1), visible = FALSE) #Hides the sample_id column. Column index numbers start at 0 here!!!
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
      output$tbl <- DT::renderDataTable(out_tbl)
      
      
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
        updateActionButton(session, "view_data", label = paste0(tr("view_data1", language$language), " ", length(input$tbl_rows_selected), " ", tr("view_data2", language$language)))
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
        LEFT JOIN sample_fractions sf ON r.sample_fraction = sf.sample_fraction_id
        LEFT JOIN result_conditions rc ON r.result_condition = rc.result_condition_id
        LEFT JOIN result_value_types rvt ON r.result_value_type = rvt.result_value_type_id
        LEFT JOIN result_speciations rs ON r.result_speciation = rs.result_speciation_id
        LEFT JOIN protocols_methods pm ON r.protocol_method = pm.protocol_id
        LEFT JOIN laboratories l ON r.laboratory = l.lab_id
        WHERE sample_id IN (", sample_ids_str, ")) sub WHERE rn <= 3;"
      )
      
      subset <- dbGetQueryDT(session$userData$AquaCache, query)
      subset[, rn := NULL] # Drop column 'rn', left over from the window function
      
      output$modal_subset <- DT::renderDataTable({  # Create datatable for the measurements
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
                        dom = 'rtip'
                      )
        )
      }) # End of function creating data subset datatable
      
      
      
      # Get location metadata
      
      location <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM ", if (language$abbrev == "fr") "location_metadata_fr" else "location_metadata_en", " WHERE location_id IN (", paste(selected_loc_ids, collapse = ", "), ") LIMIT 3;")) # Get the location metadata
      
      output$modal_location_metadata <- DT::renderDataTable({  # Create datatable for the locations
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
                        dom = 'rt',
                        scrollX = TRUE
                      )
        ) %>%
          DT::formatRound(columns = c(4,5,5), digits = 3) # Round numbers, here index starts at 1 (not javascript)
      }) # End of function creating location metatadata datatable
      
      
      # Create the modal
      showModal(modalDialog(
        h4(tr("discrete_subset_msg", language$language)),
        DT::dataTableOutput(ns("modal_subset")),
        h4(tr("loc_meta_msg", language$language)),
        DT::dataTableOutput(ns("modal_location_metadata")),
        textOutput(ns("num_rows")),
        selectizeInput(ns("modal_format"), label = tr("dl_format", language$language), choices = stats::setNames(c("xlsx", "csv", "sqlite"), c(tr("dl_format_xlsx", language$language), tr("dl_format_csv", language$language), tr("dl_format_sqlite", language$language))), selected = "xlsx"),
        downloadButton(ns("download"), tr("dl_data", language$language)),
        size = "l"
      ))
    })
    
    
    
    
    # Updates to modal ########################################################
    # Get the number of rows that will be returned based on the date range selected and update the subset table if necessary
    observe({
      req(input$tbl_rows_selected, filteredData$params)
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
        paste0("data_", format(Sys.time(), "%Y%m%d_%H%M%S%Z"), ".", if (input$modal_format == "csv") "zip" else input$modal_format)
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
        LEFT JOIN sample_fractions sf ON r.sample_fraction = sf.sample_fraction_id
        LEFT JOIN result_conditions rc ON r.result_condition = rc.result_condition_id
        LEFT JOIN result_value_types rvt ON r.result_value_type = rvt.result_value_type_id
        LEFT JOIN result_speciations rs ON r.result_speciation = rs.result_speciation_id
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
      }) # End of downloadHandler
    
  }) # End moduleServer
} # End discPlot function

