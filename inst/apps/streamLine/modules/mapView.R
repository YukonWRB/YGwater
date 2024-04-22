
mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        HTML("
      .leaflet-left .leaflet-control{
        visibility: hidden;
      }"
        ))
    ),
    tags$script(
      HTML("
            Shiny.addCustomMessageHandler('update-tooltip', function(message) {
                var selector = '#' + message.id;
                $(selector).attr('title', message.title)
                .tooltip('fixTitle').tooltip('hide');
            });
        ")
    ),
    leaflet::leafletOutput(ns("map"), height = '80vh'),
    absolutePanel(id = ns("controls"), class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 135, left = "auto", width = "250px",
                  # Panel content
                  span(
                    id = ns("infoIcon"),
                    `data-toggle` = "tooltip",
                    `data-placement` = "right",
                    `data-trigger` = "click hover",
                    title = "Placeholder",
                    icon("info-circle", style = "font-size: 150%;")),
                  selectizeInput(ns("typeFlt"), "Data Type", choices = c("All" = "All"), multiple = TRUE), # choices and labels are updated in the server module
                  selectizeInput(ns("paramTypeFlt"), "Parameter Type", choices = c("All" = "All"), multiple = TRUE),
                  selectizeInput(ns("paramGrpFlt"), "Parameter Group", choices = c("All" = "All"), multiple = TRUE),
                  selectizeInput(ns("paramFlt"), "Parameter", choices = c("All" = "All"), multiple = TRUE),
                  selectizeInput(ns("projFlt"), "Project", choices = c("All" = "All"), multiple = TRUE),
                  selectizeInput(ns("netFlt"), "Network", choices = c("All" = "All"), multiple = TRUE),
                  sliderInput(ns("yrFlt"), "With data between...", sep = "", min = 1897, max = lubridate::year(Sys.Date()), value = c(1897, lubridate::year(Sys.Date())), step = 1),
                  actionButton(ns("reset"), "Reset Filters"),
                  style = "opacity: 1; z-index: 400;") # Adjust styling
  )
}


map <- function(id, con, language, restoring) {
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c("reset", "map_bounds", "map_center", "map_zoom", "map_marker_mouseover", "map_marker_mouseout"))
    ns <- session$ns
    
    # Get data from database ##################################################
    locations <- DBI::dbGetQuery(con, "SELECT location, location_id, name, latitude, longitude, geom_id, name_fr FROM locations;")
    timeseries <- DBI::dbGetQuery(con, "SELECT timeseries_id, location_id, parameter, param_type, period_type, category, start_datetime, end_datetime FROM timeseries;")
    locations_projects <- DBI::dbGetQuery(con, "SELECT * FROM locations_projects;")
    locations_networks <- DBI::dbGetQuery(con, "SELECT * FROM locations_networks;")
    param_types <- DBI::dbGetQuery(con, "SELECT p.* FROM param_types AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.param_type = p.param_type_code);")
    param_groups <- DBI::dbGetQuery(con, "SELECT DISTINCT p.group, p.group_fr FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter = p.param_code);")
    parameters <- DBI::dbGetQuery(con, "SELECT p.param_code, p.param_name, p.param_name_fr, p.group FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter = p.param_code);")
    projects <- DBI::dbGetQuery(con, "SELECT p.* FROM projects AS p WHERE EXISTS (SELECT 1 FROM locations_projects lp WHERE lp.project_id = p.project_id);")
    networks <-  DBI::dbGetQuery(con, "SELECT n.* FROM networks AS n WHERE EXISTS (SELECT 1 FROM locations_networks ln WHERE ln.network_id = n.network_id);")
    has_images <- DBI::dbGetQuery(con, "SELECT DISTINCT location_id FROM images_index;")
    has_documents <- DBI::dbGetQuery(con, "SELECT DISTINCT locations.location_id FROM locations JOIN documents_spatial ON locations.geom_id = documents_spatial.geom_id JOIN documents ON documents_spatial.document_id = documents.document_id;")
    
    # Adjust multiple selection based on if 'All' is selected ################
    observeFilterInput <- function(inputId) {
      observeEvent(input[[inputId]], {
        # Check if 'All' is selected and adjust accordingly
          if (length(input[[inputId]]) > 1) {
            if ("All" %in% input[[inputId]]) {
              updateSelectizeInput(session, inputId, selected = "All")
          }
        }
      })
    }
    observeFilterInput("typeFlt")
    observeFilterInput("paramTypeFlt")
    observeFilterInput("paramGrpFlt")
    observeFilterInput("paramFlt")
    observeFilterInput("projFlt")
    observeFilterInput("netFlt")
    
    # Create reactives to filter based on selections ############################
    #TODO: This portion is not functional yet
    filteredYears <- reactive({
      timeseries[timeseries$start_datetime >= as.POSIXct(paste0(input$yrFlt[1], "-01-01 00:00"), tz = "UTC") & timeseries$end_datetime <= as.POSIXct(paste0(input$yrFlt[2], "-12-31 23:59:59"), tz = "UTC"), ]
    })
    filteredTypes <- reactive({
      if (input$typeFlt == "All") {
        timeseries$category
      } else {
        timeseries[timeseries$category %in% input$typeFlt,  ]
      }
    })
    filteredProjects <- reactive({
      if (input$projFlt == "All") {
        projects
      } else {
        projects[projects[translations[translations$id == "generic_name_col", ..lang][[1]]] %in% tolower(input$projFlt),  ]
      }
    })
    filteredNetworks <- reactive({
      if (input$netFlt == "All") {
        networks
      } else {
        networks[networks[translations[translations$id == "generic_name_col", ..lang][[1]]] %in% tolower(input$netFlt), ]
      }
    })
    filteredParamTypes <- reactive({
      if (input$paramTypeFlt == "All") {
        param_types
      } else {
        param_types[param_types[translations[translations$id == "param_type_col", ..lang][[1]]] %in% tolower(input$paramTypeFlt), ]
      }
    })
    filteredParamGroup <- reactive({
      if (input$paramGrpFlt == "All") {
        param_groups
      } else {
        param_groups[param_groups[translations[translations$id == "param_group_col", ..lang][[1]]] %in% tolower(input$paramGrpFlt),  ]
      }
    })
    filteredParameters <- reactive({
      if (input$paramFlt == "All") {
        parameters
      } else {
        parameters[parameters[translations[translations$id == "param_name_col", ..lang][[1]]] %in% tolower(input$paramFlt), ]
      }
    })
    
    # Update text based on language ###########################################
    observe({
      lang <- language()
      abbrev <- translations[translations$id == "titleCase", ..lang][[1]]
      
      # Update the tooltip's text
      tooltipText <- translations[translations$id == "map_tooltip", ..lang][[1]]
      session$sendCustomMessage(type = 'update-tooltip', message = list(id = ns("infoIcon"), title = tooltipText))
      
      # Update selectizeInputs
      updateSelectizeInput(session, 
                           "typeFlt",
                           label = translations[translations$id == "data_type", ..lang][[1]],
                           choices = stats::setNames(c("All", "discrete", "continuous"),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(c(translations[translations$id == "discrete", ..lang][[1]], translations[translations$id == "continuous", ..lang][[1]]), abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "paramTypeFlt",
                           label = translations[translations$id == "param_type", ..lang][[1]],
                           choices = stats::setNames(c("All", param_types$param_type_code),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(param_types[[translations[translations$id == "param_type_col", ..lang][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "paramGrpFlt",
                           label = translations[translations$id == "param_group", ..lang][[1]],
                           choices = stats::setNames(c("All", param_groups$group),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(param_groups[[translations[translations$id == "param_group_col", ..lang][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "paramFlt",
                           label = translations[translations$id == "parameter", ..lang][[1]],
                           choices = stats::setNames(c("All", parameters$param_code),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(parameters[[translations[translations$id == "param_name_col", ..lang][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "projFlt",
                           label = translations[translations$id == "project", ..lang][[1]],
                           choices = stats::setNames(c("All", projects$project_id),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(projects[[translations[translations$id == "generic_name_col", ..lang][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "netFlt",
                           label = translations[translations$id == "network", ..lang][[1]],
                           choices = stats::setNames(c("All", networks$network_id),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(networks[[translations[translations$id == "generic_name_col", ..lang][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSliderInput(session,
                        "yrFlt",
                        label = translations[translations$id == "year_filter", ..lang][[1]],
                        min = lubridate::year(min(timeseries$start_datetime)),
                        max = lubridate::year(max(timeseries$end_datetime)),
                        value = lubridate::year(c(min(timeseries$start_datetime), max(timeseries$end_datetime)))
      )
      updateActionButton(session,
                         "reset",
                         label = translations[translations$id == "reset", ..lang][[1]]
      )
    })
    
    
    # Create the basic map ###########################################################
    
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 12)) %>%
        leaflet::addTiles() %>% 
        leaflet::addProviderTiles("Esri.WorldTopoMap") %>%
        leaflet::setView(lng = -135.05, lat = 65.00, zoom = 5)  %>% # Center on Yukon
        htmlwidgets::onRender(
          "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }")
    })
    
    # Filter the map data based on user's selection and add points ############################
    # Function to create popups dynamically
    popupContent <- function(loc, language, abbreviation) {
      paste0(
        "<strong>", loc$name, "</strong><br/>",
        substr(loc$start_time, 1, 10), " to ", substr(loc$end_time, 1, 10), "<br/><br/>",
        "<strong>", translations[translations$id == "parameter(s)", ..language][[1]], ":</strong><br/><i>", loc$parameters, "</i><br/>",
        "<strong>", translations[translations$id == "network(s)", ..language][[1]], ":</strong><br/><i>", loc$networks, "</i><br/>",
        "<strong>", translations[translations$id == "project(s)", ..language][[1]], ":</strong><br/><i>", ifelse(is.na(loc$projects), "N/A", paste(loc$projects, collapse = "<br/>")), "</i><br/>",
        "<a href='#' onclick='changeTabAndSetInput(\"", loc$location_id, "\"); return false;'>View Data</a>"
      )
    }
    
    observe({
      lang <- language()
      abbrev <- translations[translations$id == "titleCase", ..lang][[1]]
      
      if (!is.null(input$typeFlt)) {
        if (length(input$typeFlt) > 1) {
          timeseries.sub <- timeseries[timeseries$category %in% input$typeFlt, ]
        } else {
          if (input$typeFlt == "All") {
            timeseries.sub <- timeseries
          } else {
            timeseries.sub <- timeseries[timeseries$category == input$typeFlt, ]
          }
        }
      } else {
        timeseries.sub <- timeseries
      }
      
      if (!is.null(input$paramFlt)) {
        if (length(input$paramFlt) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$parameter %in% input$paramFlt, ]
        } else {
          if (input$paramFlt == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$parameter == input$paramFlt, ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$paramTypeFlt)) {
        if (length(input$paramTypeFlt) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$param_type %in% input$paramTypeFlt, ]
        } else {
          if (input$paramTypeFlt == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$param_type == input$paramTypeFlt, ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$paramGrpFlt)) {
        if (length(input$paramGrpFlt) > 1) {
          select.params <- parameters[parameters$group %in% input$paramGrpFlt, "param_code"]
          timeseries.sub <- timeseries.sub[timeseries.sub$parameter %in% select.params, ]
        } else {
          if (input$paramGrpFlt == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            select.params <- parameters[parameters$group == input$paramGrpFlt, "param_code"]
            timeseries.sub <- timeseries.sub[timeseries.sub$parameter %in% select.params, ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$projFlt)) {
        if (length(input$projFlt) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% locations_projects[locations_projects$project_id %in% input$projFlt, "location_id"], ]
        } else {
          if (input$projFlt == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% locations_projects[locations_projects$project_id == input$projFlt, "location_id"], ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$netFlt)) {
        if (length(input$netFlt) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% locations_networks[locations_networks$network_id %in% input$netFlt, "location_id"], ]
        } else {
          if (input$netFlt == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% locations_networks[locations_networks$network_id == input$netFlt, "location_id"], ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      timeseries.sub <- timeseries.sub[timeseries.sub$start_datetime <= as.POSIXct(paste0(input$yrFlt[2], "-12-31 23:59:59"), tz = "UTC") & timeseries.sub$end_datetime >= as.POSIXct(paste0(input$yrFlt[1], "-01-01 00:00"), tz = "UTC"),]
      
      loc.sub <- locations[locations$location_id %in% timeseries.sub$location_id, ]
      
      # Create popup text
      # Aggregating time range for each location
      time_range <- timeseries.sub %>%
        dplyr::group_by(location_id) %>%
        dplyr::summarize(
          start_time = min(start_datetime),
          end_time = max(end_datetime)
        )
      # Get parameters per location
      param_name_col <- translations[translations$id == "param_name_col", ..lang][[1]]
      location_parameters <- timeseries.sub %>%
        dplyr::left_join(parameters, by = c("parameter" = "param_code")) %>%
        dplyr::group_by(location_id) %>%
        dplyr::summarize(parameters = paste(titleCase(get(param_name_col), abbrev), " (", substr(start_datetime, 1, 10), " to ", substr(end_datetime, 1, 10), ") ", collapse = "<br/>", sep = ""), .groups = 'drop')
      # Get networks per location
      network_col <- translations[translations$id == "generic_name_col", ..lang][[1]]
      location_networks <- locations_networks %>%
        dplyr::left_join(networks, by = "network_id") %>%
        dplyr::group_by(location_id) %>%
        dplyr::summarize(networks = paste(titleCase(get(network_col), abbrev), collapse = "<br/>"), .groups = 'drop')
      
      # If you have project data, repeat a similar process for projects
      projects_col <- translations[translations$id == "generic_name_col", ..lang][[1]]
      location_projects <- locations_projects %>%
        dplyr::left_join(projects, by = "project_id") %>%
        dplyr::group_by(location_id) %>%
        dplyr::summarize(projects = paste(titleCase(get(projects_col), abbrev), collapse = "<br/>"), .groups = 'drop')
      
      # Combine all the data
      location_info <- loc.sub %>%
        dplyr::left_join(time_range, by = "location_id") %>%
        dplyr::left_join(location_parameters, by = "location_id") %>%
        dplyr::left_join(location_networks, by = "location_id") %>%
        dplyr::left_join(location_projects, by = "location_id")
      
        location_info$popup_html <- unname(sapply(
        split(location_info, seq(nrow(location_info))),
        function(x) popupContent(x, language = lang, abbreviation = abbrev)
      ))
      
      
      leaflet::leafletProxy("map", session = session) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::addMarkers(data = location_info, 
                            lng = ~longitude, 
                            lat = ~latitude,
                            popup = ~popup_html,
                            clusterOptions = leaflet::markerClusterOptions())
    })
    
    
    # Reset all filters when button pressed ##################################
    observeEvent(input$reset, {
      lang <- language()
      abbrev <- translations[translations$id == "titleCase", ..lang][[1]]
      
      updateSelectizeInput(session, 
                           "typeFlt",
                           choices = stats::setNames(c("All", "discrete", "continuous"),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(c(translations[translations$id == "discrete", ..lang][[1]], translations[translations$id == "continuous", ..lang][[1]]), abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "paramTypeFlt",
                           choices = stats::setNames(c("All", param_types$param_type_code),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(param_types[[translations[translations$id == "param_type_col", ..lang][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "paramGrpFlt",
                           choices = stats::setNames(c("All", param_groups$group),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(param_groups[[translations[translations$id == "param_group_col", ..lang][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "paramFlt",
                           choices = stats::setNames(c("All", parameters$param_code),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(parameters[[translations[translations$id == "param_name_col", ..lang][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "projFlt",
                           choices = stats::setNames(c("All", projects$project_id),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(projects[[translations[translations$id == "generic_name_col", ..lang][[1]]]], abbrev))
                           )
      )
      updateSelectizeInput(session,
                           "netFlt",
                           choices = stats::setNames(c("All", networks$network_id),
                                                     c(translations[translations$id == "all", ..lang][[1]], titleCase(networks[[translations[translations$id == "generic_name_col", ..lang][[1]]]], abbrev))
                           )
      )
      updateSliderInput(session,
                        "yrFlt",
                        label = translations[translations$id == "year_filter", ..lang][[1]],
                        min = lubridate::year(min(timeseries$start_datetime)),
                        max = lubridate::year(max(timeseries$end_datetime)),
                        value = lubridate::year(c(min(timeseries$start_datetime), max(timeseries$end_datetime)))
      )
    })
    
  })
}
