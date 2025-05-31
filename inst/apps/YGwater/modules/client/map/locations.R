mapLocsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$script(
      HTML("
      // Function to change tabs based on namespace and input target, utilized in map popups
      function changeTab(namespace, target_input, locationId) {
        Shiny.setInputValue(namespace + target_input, locationId, {priority: 'event'});
      }
      // Resets Shiny input values to null, preventing unwanted reactive triggers
      shinyjs.resetInput = function(params) {
            Shiny.setInputValue(params.name, null, {priority: 'event'});
        }
    ")
    ),
    # All UI elements rendered in server function to allow multi-language functionality
    page_fluid(
      uiOutput(ns("sidebar_page"))
    )
  )
}

mapLocs <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    
    # Server setup ####
    setBookmarkExclude(c("reset", "map_bounds", "map_center", "map_zoom", "map_marker_mouseover", "map_marker_mouseout", "map_marker_click", "clicked_view_plots_discrete", "clicked_view_plots_continuous", "clicked_dl_data_discrete", "clicked_dl_data_continuous"))
    
    ns <- session$ns
    
    outputs <- reactiveValues()  # This allows the module to pass values back to the main server
    
    moduleData <- reactiveValues(
      locations = dbGetQueryDT(session$userData$AquaCache, "SELECT location, name, name_fr, latitude, longitude, location_id, geom_id, visibility_public, location_type FROM locations"),
      timeseries = dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT ts.timeseries_id, ts.location_id, p.param_name, p.param_name_fr, m.media_type, ts.media_id, ts.parameter_id, ts.aggregation_type_id, ts.start_datetime, ts.end_datetime, ts.z, 'continuous' AS data_type
           FROM continuous.timeseries AS ts
           LEFT JOIN public.parameters AS p ON ts.parameter_id = p.parameter_id
           LEFT JOIN public.media_types AS m ON ts.media_id = m.media_id
         UNION ALL
         SELECT MIN(r.result_id) AS timeseries_id, s.location_id, p.param_name, p.param_name_fr, m.media_type, s.media_id, r.parameter_id, NULL AS aggregation_type_id,
                MIN(s.datetime) AS start_datetime, MAX(s.datetime) AS end_datetime, MIN(s.z) AS z, 'discrete' AS data_type
           FROM discrete.results r
           JOIN discrete.samples s ON r.sample_id = s.sample_id
           LEFT JOIN public.parameters p ON r.parameter_id = p.parameter_id
           LEFT JOIN public.media_types m ON s.media_id = m.media_id
          GROUP BY s.location_id, p.param_name, p.param_name_fr, m.media_type, s.media_id, r.parameter_id"
      ),
      projects = dbGetQueryDT(session$userData$AquaCache, "SELECT p.* FROM projects AS p WHERE EXISTS (SELECT 1 FROM locations_projects lp WHERE lp.project_id = p.project_id);"),
      networks =  dbGetQueryDT(session$userData$AquaCache, "SELECT n.* FROM networks AS n WHERE EXISTS (SELECT 1 FROM locations_networks ln WHERE ln.network_id = n.network_id);"),
      locations_projects = dbGetQueryDT(session$userData$AquaCache, "SELECT * FROM locations_projects;"),
      locations_networks = dbGetQueryDT(session$userData$AquaCache, "SELECT * FROM locations_networks;"),
      media_types = dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT mt.* FROM public.media_types mt WHERE mt.media_id IN (
            SELECT DISTINCT media_id FROM continuous.timeseries
            UNION
            SELECT DISTINCT media_id FROM discrete.samples)") ,
      parameters = dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT DISTINCT p.parameter_id, p.param_name, p.param_name_fr, p.unit_default, pr.group_id, pr.sub_group_id
           FROM public.parameters AS p
           LEFT JOIN public.parameter_relationships AS pr ON p.parameter_id = pr.parameter_id
          WHERE p.parameter_id IN (
            SELECT DISTINCT parameter_id FROM continuous.timeseries
            UNION
            SELECT DISTINCT parameter_id FROM discrete.results)") ,
      parameter_groups = dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT DISTINCT pg.group_id, pg.group_name, pg.group_name_fr
           FROM public.parameter_groups AS pg
           LEFT JOIN public.parameter_relationships AS pr ON pg.group_id = pr.group_id
          WHERE pr.parameter_id IN (
            SELECT DISTINCT parameter_id FROM continuous.timeseries
            UNION
            SELECT DISTINCT parameter_id FROM discrete.results)") ,
      parameter_sub_groups = dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT psg.sub_group_id, psg.sub_group_name, psg.sub_group_name_fr
           FROM public.parameter_sub_groups AS psg
           LEFT JOIN public.parameter_relationships AS pr ON psg.sub_group_id = pr.sub_group_id
          WHERE pr.parameter_id IN (
            SELECT DISTINCT parameter_id FROM continuous.timeseries
            UNION
            SELECT DISTINCT parameter_id FROM discrete.results)")
      # has_image_series = dbGetQueryDT(session$userData$AquaCache, "SELECT DISTINCT location_id FROM image_series;"),
      # has_documents = dbGetQueryDT(session$userData$AquaCache, "SELECT DISTINCT locations.location_id FROM locations JOIN documents_spatial ON locations.geom_id = documents_spatial.geom_id JOIN documents ON documents_spatial.document_id = documents.document_id;")
    )
    
    
    # Adjust filter selections based on if 'all' is selected (remove selections other than 'all')
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
    observeFilterInput("type")
    observeFilterInput("pType")
    observeFilterInput("pGrp")
    observeFilterInput("param")
    observeFilterInput("proj")
    observeFilterInput("net")
    
    # Create UI elements #####
    output$sidebar_page <- renderUI({
      req(moduleData, language)
      page_sidebar(
        sidebar = sidebar(
          title = NULL,
          bg = config$sidebar_bg, # Set in globals file'
          tagList(
            selectizeInput(
              ns("type"),
              label = tr("data_type", language$language),
              choices = stats::setNames(
                c("all", "discrete", "continuous"),
                c(
                  tr("all", language$language),
                  titleCase(
                    c(
                      tr("discrete", language$language),
                      tr("continuous", language$language)
                    ),
                    language$abbrev
                  )
                )
              ),
              multiple = TRUE
            ),
            selectizeInput(
              ns("pType"),
              label = tr("type", language$language),
              choices = stats::setNames(
                c("all", moduleData$media_types$media_id),
                c(
                  tr("all", language$language),
                  titleCase(moduleData$media_types[[tr("media_type_col", language$language)]], language$abbrev)
                )
              ),
              multiple = TRUE
            ),
            selectizeInput(
              ns("pGrp"),
              label = tr("param_group", language$language),
              choices = stats::setNames(
                c("all", moduleData$parameter_groups$group_id),
                c(
                  tr("all", language$language),
                  titleCase(moduleData$parameter_groups[[tr("param_group_col", language$language)]], language$abbrev)
                )
              ),
              multiple = TRUE
            ),
            selectizeInput(
              ns("param"),
              label = tr("parameter", language$language),
              choices = stats::setNames(
                c("all", moduleData$parameters$parameter_id),
                c(
                  tr("all", language$language),
                  titleCase(moduleData$parameters[[tr("param_name_col", language$language)]], language$abbrev)
                )
              ),
              multiple = TRUE
            ),
            selectizeInput(
              ns("proj"),
              label = tr("project", language$language),
              choices = stats::setNames(
                c("all", moduleData$projects$project_id),
                c(
                  tr("all", language$language),
                  titleCase(moduleData$projects[[tr("generic_name_col", language$language)]], language$abbrev)
                )
              ),
              multiple = TRUE
            ),
            selectizeInput(
              ns("net"),
              label = tr("network", language$language),
              choices = stats::setNames(
                c("all", moduleData$networks$network_id),
                c(
                  tr("all", language$language),
                  titleCase(moduleData$networks[[tr("generic_name_col", language$language)]], language$abbrev)
                )
              ),
              multiple = TRUE
            ),
            sliderInput(
              ns("yrs"),
              label = tr("year_filter", language$language),
              min = lubridate::year(min(moduleData$timeseries$start_datetime)),
              max = lubridate::year(max(moduleData$timeseries$end_datetime)),
              value = lubridate::year(c(min(moduleData$timeseries$start_datetime), max(moduleData$timeseries$end_datetime))),
              step = 1,
              sep = ""
            ),
            actionButton(ns("reset"), tr("reset", language$language))
          ) # End sidebar tagList
        ),
        leaflet::leafletOutput(ns("map"), height = '80vh'),
      )
    }) |> bindEvent(moduleData, language$language)
    
    
    # Reset all filters when reset button pressed ##################################
    observeEvent(input$reset, {
      updateSelectizeInput(session, 
                           "type",
                           choices = stats::setNames(c("all", "discrete", "continuous"),
                                                     c(tr("all", language$language), titleCase(c(tr("discrete", language$language), tr("continuous", language$language)), language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pType",
                           choices = stats::setNames(c("all", moduleData$media_types$media_id),
                                                     c(tr("all", language$language), titleCase(moduleData$media_types[[tr("media_type_col", language$language)]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pGrp",
                           choices = stats::setNames(c("all", moduleData$parameter_groups$group_id),
                                                     c(tr("all", language$language), titleCase(moduleData$parameter_groups[[tr("param_group_col", language$language)]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "param",
                           choices = stats::setNames(c("all", moduleData$parameters$parameter_id),
                                                     c(tr("all", language$language), titleCase(moduleData$parameters[[tr("param_name_col", language$language)]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "proj",
                           choices = stats::setNames(c("all", moduleData$projects$project_id),
                                                     c(tr("all", language$language), titleCase(moduleData$projects[[tr("generic_name_col", language$language)]], language$abbrev))
                           ),
      )
      updateSelectizeInput(session,
                           "net",
                           choices = stats::setNames(c("all", moduleData$networks$network_id),
                                                     c(tr("all", language$language), titleCase(moduleData$networks[[tr("generic_name_col", language$language)]], language$abbrev))
                           )
      )
      updateSliderInput(session,
                        "yrs",
                        min = lubridate::year(min(moduleData$timeseries$start_datetime)),
                        max = lubridate::year(max(moduleData$timeseries$end_datetime)),
                        value = lubridate::year(c(min(moduleData$timeseries$start_datetime), max(moduleData$timeseries$end_datetime)))
      )
    }) # End of observeEvent for reset filters button
    
    # Update map popup based on language ###########################################
    popupData <- reactive({
      # Create popup text for each location. This is a bit slow when first loading the tab, but it doesn't need to be run again when the user modifies a filter.
      # Get location names
      popup_names <- moduleData$locations[ , .(location_id, popup_name = get(tr("generic_name_col", language$language)))]
      popup_names[, popup_name := titleCase(popup_name, language$abbrev)]
      # Aggregate time range for each location
      time_range <- moduleData$timeseries[, .(
        start_time = min(start_datetime),
        end_time = max(end_datetime)
      ), by = location_id]
      # Get parameters per location
      param_name_col <- tr("param_name_col", language$language)
      to_text <- tr("to", language$language)
      tmp <- moduleData$timeseries
      tmp[, formatted_param := paste(titleCase(get(param_name_col), language$abbrev), " (", 
                                     format(as.Date(start_datetime), "%Y-%m-%d"), 
                                     " ", to_text, " ", 
                                     format(as.Date(end_datetime), "%Y-%m-%d"), ")", sep = "")]
      location_parameters <- tmp[, .(parameters = paste(formatted_param, collapse = "<br/>")), by = location_id]
      # Get networks per location
      network_col <- tr("generic_name_col", language$language)
      tmp <- moduleData$locations_networks[moduleData$networks, on = "network_id", allow.cartesian = TRUE]
      tmp[, formatted_network := titleCase(get(network_col), language$abbrev)]
      location_networks <- tmp[, .(networks = paste(formatted_network, collapse = "<br/>")), by = location_id]
      # Get projects per location
      projects_col <- tr("generic_name_col", language$language)
      tmp <- moduleData$locations_projects[moduleData$projects, on = "project_id", allow.cartesian = TRUE]
      tmp[, formatted_project := titleCase(get(projects_col), language$abbrev)]
      location_projects <- tmp[, .(projects = paste(formatted_project, collapse = "<br/>")), by = location_id]
      
      # Combine all the data
      tmp <- data.table::copy(moduleData$locations)[, "location_id"]  # Use copy to avoid modifying the original data table
      tmp[popup_names, on = .(location_id), popup_name := popup_name]  # Join popup_name
      tmp[time_range, on = .(location_id), c("start_time", "end_time") := .(start_time, end_time)]  # Join time_range
      tmp[location_parameters, on = .(location_id), parameters := parameters]  # Join location_parameters
      tmp[location_networks, on = .(location_id), networks := networks]  # Join location_networks
      tmp[location_projects, on = .(location_id), projects := projects]  # Join location_projects

      # Determine available data types for each location
      loc_data_types <- moduleData$timeseries[, .(
        has_discrete = any(data_type == "discrete"),
        has_continuous = any(data_type == "continuous")
      ), by = location_id]
      tmp[loc_data_types, on = .(location_id), `:=`(
        has_discrete = i.has_discrete,
        has_continuous = i.has_continuous
      )]

      tmp[, popup_links := {
        links <- character(0)
        if (isTRUE(has_discrete)) {
          links <- c(
            links,
            sprintf(
              "<a href='#' onclick='changeTab(\"mapLocs-\", \"clicked_dl_data_discrete\", \"%s\"); return false;'>%s</a>",
              location_id,
              tr("dl_data_discrete", language$language)
            ),
            sprintf(
              "<a href='#' onclick='changeTab(\"mapLocs-\", \"clicked_view_plots_discrete\", \"%s\"); return false;'>%s</a>",
              location_id,
              tr("view_plots_discrete", language$language)
            )
          )
        }
        if (isTRUE(has_continuous)) {
          links <- c(
            links,
            sprintf(
              "<a href='#' onclick='changeTab(\"mapLocs-\", \"clicked_dl_data_continuous\", \"%s\"); return false;'>%s</a>",
              location_id,
              tr("dl_data_continuous", language$language)
            ),
            sprintf(
              "<a href='#' onclick='changeTab(\"mapLocs-\", \"clicked_view_plots_continuous\", \"%s\"); return false;'>%s</a>",
              location_id,
              tr("view_plots_continuous", language$language)
            )
          )
        }
        if (length(links) > 0) {
          paste0("<br/>", paste(links, collapse = "<br/>"), "<br/>")
        } else {
          ""
        }
      }, by = location_id]

      tmp[, popup_html := paste0(
        "<strong>", popup_name, "</strong><br/>",
        substr(start_time, 1, 10), " ", tr("to", language$language), " ", substr(end_time, 1, 10), "<br/><br/>",
        "<strong>", tr("parameter(s)", language$language), ":</strong><br/>",
        "<div style='max-height:100px; overflow-y:auto;'><i>", parameters, "</i></div><br/>",  # Supposed to be scrollable
        "<strong>", tr("network(s)", language$language), ":</strong><br/><i>", networks, "</i><br/>",
        "<strong>", tr("project(s)", language$language), ":</strong><br/><i>", ifelse(is.na(projects), "N/A", paste(projects, collapse = "<br/>")), "</i>",
        popup_links
      )]
      
      tmp
    })
    
    
    # Create the basic map ###########################################################
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 15)) %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addLayersControl(baseGroups = c("Topographic", "Satellite")) %>%
        leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE)) %>%
        leaflet::setView(lng = -135.05, lat = 64.00, zoom = 5) %>% # Center on Yukon
        htmlwidgets::onRender(
          "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }")
    })
    
    
    # Filter the map data based on user's selection and add points to map ############################
    observe({
      popup_data <- popupData()
      if (!is.null(input$type)) {
        if (length(input$type) > 1) {
          timeseries.sub <- moduleData$timeseries[moduleData$timeseries$data_type %in% input$type, ]
        } else {
          if (input$type == "all") {
            timeseries.sub <- moduleData$timeseries
          } else {
            timeseries.sub <- moduleData$timeseries[moduleData$timeseries$data_type == input$type, ]
          }
        }
      } else {
        timeseries.sub <- moduleData$timeseries
      }
      
      if (!is.null(input$pType)) {
        if (length(input$pType) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$media_id %in% input$pType, ]
        } else {
          if (input$pType != "all") {
            timeseries.sub <- timeseries.sub[timeseries.sub$media_id == input$pType, ]
          }
        }
      }
      
      if (!is.null(input$pGrp)) {
        if (length(input$pGrp) > 1) {
          select.params <- moduleData$parameters[moduleData$parameters$group_id %in% input$pGrp, "parameter_id"]$parameter_id
          timeseries.sub <- timeseries.sub[parameter_id %in% select.params, ]
        } else {
          if (input$pGrp != "all") {
            select.params <- moduleData$parameters[moduleData$parameters$group_id == input$pGrp, "parameter_id"]$parameter_id
            if (length(select.params) > 1) {
              timeseries.sub <- timeseries.sub[parameter_id %in% select.params, ]
            } else {
              timeseries.sub <- timeseries.sub[parameter_id == select.params, ] 
            }
          }
        }
      }
      
      if (!is.null(input$param)) {
        if (length(input$param) > 1) {
          timeseries.sub <- timeseries.sub[parameter_id %in% input$param, ]
        } else {
          if (input$param != "all") {
            timeseries.sub <- timeseries.sub[parameter_id == input$param, ]
          }
        }
      }
      
      if (!is.null(input$proj)) {
        if (length(input$proj) > 1) {
          ids <- moduleData$locations_projects[moduleData$locations_projects$project_id %in% input$proj, "location_id"]$location_id
          if (length(ids) > 1) {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% ids, ]
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id == ids, ]
          }
        } else {
          if (input$proj != "all") {
            ids <- moduleData$locations_projects[moduleData$locations_projects$project_id == input$proj, "location_id"]$location_id
            if (length(ids) > 1) {
              timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% ids, ]
            } else {
              timeseries.sub <- timeseries.sub[timeseries.sub$location_id == ids, ]
            }
          }
        }
      }
      
      if (!is.null(input$net)) {
        if (length(input$net) > 1) {
          ids <- moduleData$locations_networks[moduleData$locations_networks$network_id %in% input$net, "location_id"]$location_id
          if (length(ids) > 1) {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% ids, ] 
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id == ids, ]
          }
        } else {
          if (input$net != "all") {
            ids <- moduleData$locations_networks[moduleData$locations_networks$network_id == input$net, "location_id"]$location_id
            if (length(ids) > 1) {
              timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% ids, ]
            } else {
              timeseries.sub <- timeseries.sub[timeseries.sub$location_id == ids, ] 
            }
          }
        }
      }
      if (!is.null(input$yrs)) {
        timeseries.sub <- timeseries.sub[timeseries.sub$start_datetime <= as.POSIXct(paste0(input$yrs[2], "-12-31 23:59:59"), tz = "UTC") & timeseries.sub$end_datetime >= as.POSIXct(paste0(input$yrs[1], "-01-01 00:00"), tz = "UTC"),]
      }
      
      
      loc.sub <- moduleData$locations[moduleData$locations$location_id %in% timeseries.sub$location_id, ]
      loc.sub <- loc.sub[popup_data, on = .(location_id), popup_html := popup_html]
      
      leaflet::leafletProxy("map", session = session) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::addMarkers(data = loc.sub,
                            lng = ~longitude,
                            lat = ~latitude,
                            popup = ~popup_html,
                            clusterOptions = leaflet::markerClusterOptions())
      
    }) # End of observe for map filters and rendering location points
    
    
    # Pass a message to the main map server when a location link is clicked ############################
    # Listen for a click in the popup
    observeEvent(input$clicked_dl_data_discrete, {
      if (!is.null(input$clicked_dl_data_discrete)) {
        outputs$change_tab <- "discData"
        outputs$location_id <- input$clicked_dl_data_discrete
        shinyjs::runjs(sprintf("shinyjs.resetInput({name: 'map-clicked_dl_data_discrete'})"))  # Reset the value to NULL to prevent an endless loop
      }
    })
    observeEvent(input$clicked_dl_data_continuous, {
      if (!is.null(input$clicked_dl_data_continuous)) {
        outputs$change_tab <- "contData"
        outputs$location_id <- input$clicked_dl_data_continuous
        shinyjs::runjs(sprintf("shinyjs.resetInput({name: 'map-clicked_dl_data_continuous'})"))  # Reset the value to NULL to prevent an endless loop
      }
    })
    observeEvent(input$clicked_view_plots_discrete, {
      if (!is.null(input$clicked_view_plots_discrete)) {
        outputs$change_tab <- "discrete"
        outputs$location_id <- input$clicked_view_plots_discrete
        shinyjs::runjs(sprintf("shinyjs.resetInput({name: 'map-clicked_view_plots_discrete'})"))  # Reset the value to NULL to prevent an endless loop
      }
    })
    observeEvent(input$clicked_view_plots_continuous, {
      if (!is.null(input$clicked_view_plots_continuous)) {
        outputs$change_tab <- "continuous"
        outputs$location_id <- input$clicked_view_plots_continuous
        shinyjs::runjs(sprintf("shinyjs.resetInput({name: 'map-clicked_view_plots_continuous'})"))  # Reset the value to NULL to prevent an endless loop
      }
    })
    return(outputs)  # Sends values back to the main server function
  })
}



