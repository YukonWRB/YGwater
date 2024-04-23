
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
        "),
      HTML("
      function changeTabAndSetInput(locationId, namespace) {
        Shiny.setInputValue(namespace + 'clicked_location_id', locationId, {priority: 'event'});
      }
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
    
    setBookmarkExclude(c("reset", "map_bounds", "map_center", "map_zoom", "map_marker_mouseover", "map_marker_mouseout", "map_marker_click"))
    ns <- session$ns
    
    outputs <- reactiveValues()
    
    # Define functions ##########################################################
    # Function to create popups dynamically
    popupContent <- function(df, language, abbreviation, namespace) {
      paste0(
        "<strong>", df$popup_name, "</strong><br/>",
        substr(df$start_time, 1, 10), " ", translations[id == "to", get(language)][[1]], " ", substr(df$end_time, 1, 10), "<br/><br/>",
        "<strong>", translations[id == "parameter(s)", get(language)][[1]], ":</strong><br/><i>", df$parameters, "</i><br/>",
        "<strong>", translations[id == "network(s)", get(language)][[1]], ":</strong><br/><i>", df$networks, "</i><br/>",
        "<strong>", translations[id == "project(s)", get(language)][[1]], ":</strong><br/><i>", ifelse(is.na(df$projects), "N/A", paste(df$projects, collapse = "<br/>")), "</i><br/>",
        "<a href='#' onclick='changeTabAndSetInput(\"", df$location_id, "\", \"", namespace, "\"); return false;'>View Data</a>"
      )
    }
    
    # Get data from database ##################################################
    
    data <- reactiveValues(
      locations = dbGetQueryDT(con, "SELECT location, location_id, name, latitude, longitude, geom_id, name_fr FROM locations;"),
      timeseries = dbGetQueryDT(con, "SELECT timeseries_id, location_id, parameter, param_type, period_type, category, start_datetime, end_datetime FROM timeseries;"),
      locations_projects = dbGetQueryDT(con, "SELECT * FROM locations_projects;"),
      locations_networks = dbGetQueryDT(con, "SELECT * FROM locations_networks;"),
      param_types = dbGetQueryDT(con, "SELECT p.* FROM param_types AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.param_type = p.param_type_code);"),
      param_groups = dbGetQueryDT(con, "SELECT DISTINCT p.group, p.group_fr FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter = p.param_code);"),
      parameters = dbGetQueryDT(con, "SELECT p.param_code, p.param_name, p.param_name_fr, p.group FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter = p.param_code);"),
      projects = dbGetQueryDT(con, "SELECT p.* FROM projects AS p WHERE EXISTS (SELECT 1 FROM locations_projects lp WHERE lp.project_id = p.project_id);"),
      networks =  dbGetQueryDT(con, "SELECT n.* FROM networks AS n WHERE EXISTS (SELECT 1 FROM locations_networks ln WHERE ln.network_id = n.network_id);"),
      has_images = dbGetQueryDT(con, "SELECT DISTINCT location_id FROM images_index;"),
      has_documents = dbGetQueryDT(con, "SELECT DISTINCT locations.location_id FROM locations JOIN documents_spatial ON locations.geom_id = documents_spatial.geom_id JOIN documents ON documents_spatial.document_id = documents.document_id;")
    )
    
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
    #TODO: This portion is not functional yet and needs to be adapted to use data.tables
    # filteredYears <- reactive({
    #   data$timeseries[data$timeseries$start_datetime >= as.POSIXct(paste0(input$yrFlt[1], "-01-01 00:00"), tz = "UTC") & data$timeseries$end_datetime <= as.POSIXct(paste0(input$yrFlt[2], "-12-31 23:59:59"), tz = "UTC"), ]
    # })
    # filteredTypes <- reactive({
    #   if (input$typeFlt == "All") {
    #     data$timeseries$category
    #   } else {
    #     data$timeseries[data$timeseries$category %in% input$typeFlt,  ]
    #   }
    # })
    # filteredProjects <- reactive({
    #   if (input$projFlt == "All") {
    #     data$projects
    #   } else {
    #     data$projects[data$projects[translations[id == "generic_name_col", ..lang][[1]]] %in% tolower(input$projFlt),  ]
    #   }
    # })
    # filteredNetworks <- reactive({
    #   if (input$netFlt == "All") {
    #     data$networks
    #   } else {
    #     data$networks[data$networks[translations[id == "generic_name_col", ..lang][[1]]] %in% tolower(input$netFlt), ]
    #   }
    # })
    # filteredParamTypes <- reactive({
    #   if (input$paramTypeFlt == "All") {
    #     data$param_types
    #   } else {
    #     data$param_types[data$param_types[translations[id == "param_type_col", ..lang][[1]]] %in% tolower(input$paramTypeFlt), ]
    #   }
    # })
    # filteredParamGroup <- reactive({
    #   if (input$paramGrpFlt == "All") {
    #     data$param_groups
    #   } else {
    #     data$param_groups[data$param_groups[translations[id == "param_group_col", ..lang][[1]]] %in% tolower(input$paramGrpFlt),  ]
    #   }
    # })
    # filteredParameters <- reactive({
    #   if (input$paramFlt == "All") {
    #     data$parameters
    #   } else {
    #     data$parameters[data$parameters[translations[id == "param_name_col", ..lang][[1]]] %in% tolower(input$paramFlt), ]
    #   }
    # })
    
    # Update text based on language ###########################################
    observe({
      lang <- language()
      abbrev <- translations[id == "titleCase", get(lang)][[1]]
      
      # Create popup text for each location. This is a bit slow when first loading the tab, but it doesn't need to be run again when the user modifies a filter.
      # Get location names
      popup_names <- data$locations[, .(location_id, popup_name = get(translations[id == "generic_name_col", get(lang)][[1]]))]
      popup_names[, popup_name := titleCase(popup_name, abbrev)]
      # Aggregate time range for each location
      time_range <- data$timeseries[, .(
        start_time = min(start_datetime),
        end_time = max(end_datetime)
      ), by = location_id]
      # Get parameters per location
      param_name_col <- translations[id == "param_name_col", get(lang)][[1]]
      to_text <- translations[id == "to", get(lang)][[1]]
      tmp <- data$timeseries[data$parameters, on = .(parameter = param_code), allow.cartesian = TRUE]
      tmp[, formatted_param := paste(titleCase(get(param_name_col), abbrev), " (", 
                                                 format(as.Date(start_datetime), "%Y-%m-%d"), 
                                                 " ", to_text, " ", 
                                                 format(as.Date(end_datetime), "%Y-%m-%d"), ")", sep = "")]
      location_parameters <- tmp[, .(parameters = paste(formatted_param, collapse = "<br/>")), by = location_id]
      # Get networks per location
      network_col <- translations[id == "generic_name_col", get(lang)][[1]]
      tmp <- data$locations_networks[data$networks, on = "network_id", allow.cartesian = TRUE]
      tmp[, formatted_network := titleCase(get(network_col), abbrev)]
      location_networks <- tmp[, .(networks = paste(formatted_network, collapse = "<br/>")), by = location_id]
      # Get projects per location
      projects_col <- translations[id == "generic_name_col", get(lang)][[1]]
      tmp <- data$locations_projects[data$projects, on = "project_id", allow.cartesian = TRUE]
      tmp[, formatted_project := titleCase(get(projects_col), abbrev)]
      location_projects <- tmp[, .(projects = paste(formatted_project, collapse = "<br/>")), by = location_id]
      
      # Combine all the data
      tmp <- data$locations[, "location_id"] %>%
        dplyr::left_join(popup_names, by = "location_id") %>%
        dplyr::left_join(time_range, by = "location_id") %>%
        dplyr::left_join(location_parameters, by = "location_id") %>%
        dplyr::left_join(location_networks, by = "location_id") %>%
        dplyr::left_join(location_projects, by = "location_id")
      
      tmp <- data.table::copy(data$locations)[, "location_id"]  # Use copy to avoid modifying the original data table
      tmp[popup_names, on = .(location_id), popup_name := popup_name]  # Join popup_name
      tmp[time_range, on = .(location_id), c("start_time", "end_time") := .(start_time, end_time)]  # Join time_range
      tmp[location_parameters, on = .(location_id), parameters := parameters]  # Join location_parameters
      tmp[location_networks, on = .(location_id), networks := networks]  # Join location_networks
      tmp[location_projects, on = .(location_id), projects := projects]  # Join location_projects
      
      # Use the popupContent function to create the popup text
      data$locations$popup_html <- unname(sapply(
        split(tmp, seq(nrow(tmp))),
        function(x) popupContent(x, language = lang, abbreviation = abbrev, namespace = "map-")
      ))
      
      
      # Update the tooltip's text
      tooltipText <- translations[id == "map_tooltip", get(lang)][[1]]
      session$sendCustomMessage(type = 'update-tooltip', message = list(id = ns("infoIcon"), title = tooltipText))
      
      # Update selectizeInputs
      updateSelectizeInput(session, 
                           "typeFlt",
                           label = translations[id == "data_type", get(lang)][[1]],
                           choices = stats::setNames(c("All", "discrete", "continuous"),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(c(translations[id == "discrete", get(lang)][[1]], translations[id == "continuous", get(lang)][[1]]), abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "paramTypeFlt",
                           label = translations[id == "param_type", get(lang)][[1]],
                           choices = stats::setNames(c("All", data$param_types$param_type_code),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(data$param_types[[translations[id == "param_type_col", get(lang)][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "paramGrpFlt",
                           label = translations[id == "param_group", get(lang)][[1]],
                           choices = stats::setNames(c("All", data$param_groups$group),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(data$param_groups[[translations[id == "param_group_col", get(lang)][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "paramFlt",
                           label = translations[id == "parameter", get(lang)][[1]],
                           choices = stats::setNames(c("All", data$parameters$param_code),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(data$parameters[[translations[id == "param_name_col", get(lang)][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "projFlt",
                           label = translations[id == "project", get(lang)][[1]],
                           choices = stats::setNames(c("All", data$projects$project_id),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(data$projects[[translations[id == "generic_name_col", get(lang)][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "netFlt",
                           label = translations[id == "network", get(lang)][[1]],
                           choices = stats::setNames(c("All", data$networks$network_id),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(data$networks[[translations[id == "generic_name_col", get(lang)][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSliderInput(session,
                        "yrFlt",
                        label = translations[id == "year_filter", get(lang)][[1]],
                        min = lubridate::year(min(data$timeseries$start_datetime)),
                        max = lubridate::year(max(data$timeseries$end_datetime)),
                        value = lubridate::year(c(min(data$timeseries$start_datetime), max(data$timeseries$end_datetime)))
      )
      updateActionButton(session,
                         "reset",
                         label = translations[id == "reset", get(lang)][[1]]
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
    observe({
      lang <- language()
      abbrev <- translations[id == "titleCase", get(lang)][[1]]
      
      if (!is.null(input$typeFlt)) {
        if (length(input$typeFlt) > 1) {
          timeseries.sub <- data$timeseries[data$timeseries$category %in% input$typeFlt, ]
        } else {
          if (input$typeFlt == "All") {
            timeseries.sub <- data$timeseries
          } else {
            timeseries.sub <- data$timeseries[data$timeseries$category == input$typeFlt, ]
          }
        }
      } else {
        timeseries.sub <- data$timeseries
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
          select.params <- data$parameters[data$parameters$group %in% input$paramGrpFlt, "param_code"]
          timeseries.sub <- timeseries.sub[timeseries.sub$parameter %in% select.params, ]
        } else {
          if (input$paramGrpFlt == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            select.params <- data$parameters[data$parameters$group == input$paramGrpFlt, "param_code"]
            timeseries.sub <- timeseries.sub[timeseries.sub$parameter %in% select.params, ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$projFlt)) {
        if (length(input$projFlt) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% data$locations_projects[data$locations_projects$project_id %in% input$projFlt, "location_id"], ]
        } else {
          if (input$projFlt == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% data$locations_projects[data$locations_projects$project_id == input$projFlt, "location_id"], ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$netFlt)) {
        if (length(input$netFlt) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% data$locations_networks[data$locations_networks$network_id %in% input$netFlt, "location_id"], ]
        } else {
          if (input$netFlt == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% data$locations_networks[data$locations_networks$network_id == input$netFlt, "location_id"], ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      timeseries.sub <- timeseries.sub[timeseries.sub$start_datetime <= as.POSIXct(paste0(input$yrFlt[2], "-12-31 23:59:59"), tz = "UTC") & timeseries.sub$end_datetime >= as.POSIXct(paste0(input$yrFlt[1], "-01-01 00:00"), tz = "UTC"),]
      
      loc.sub <- data$locations[data$locations$location_id %in% timeseries.sub$location_id, ]
      
      leaflet::leafletProxy("map", session = session) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::addMarkers(data = loc.sub, 
                            lng = ~longitude, 
                            lat = ~latitude,
                            popup = ~popup_html,
                            clusterOptions = leaflet::markerClusterOptions())
    })
    
    # Reset all filters when button pressed ##################################
    observeEvent(input$reset, {
      lang <- language()
      abbrev <- translations[id == "titleCase", get(lang)][[1]]
      
      updateSelectizeInput(session, 
                           "typeFlt",
                           choices = stats::setNames(c("All", "discrete", "continuous"),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(c(translations[id == "discrete", get(lang)][[1]], translations[id == "continuous", get(lang)][[1]]), abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "paramTypeFlt",
                           choices = stats::setNames(c("All", param_types$param_type_code),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(param_types[[translations[id == "param_type_col", get(lang)][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "paramGrpFlt",
                           choices = stats::setNames(c("All", param_groups$group),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(param_groups[[translations[id == "param_group_col", get(lang)][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "paramFlt",
                           choices = stats::setNames(c("All", parameters$param_code),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(parameters[[translations[id == "param_name_col", get(lang)][[1]]]], abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "projFlt",
                           choices = stats::setNames(c("All", projects$project_id),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(projects[[translations[id == "generic_name_col", get(lang)][[1]]]], abbrev))
                           )
      )
      updateSelectizeInput(session,
                           "netFlt",
                           choices = stats::setNames(c("All", networks$network_id),
                                                     c(translations[id == "all", get(lang)][[1]], titleCase(networks[[translations[id == "generic_name_col", get(lang)][[1]]]], abbrev))
                           )
      )
      updateSliderInput(session,
                        "yrFlt",
                        label = translations[id == "year_filter", get(lang)][[1]],
                        min = lubridate::year(min(timeseries$start_datetime)),
                        max = lubridate::year(max(timeseries$end_datetime)),
                        value = lubridate::year(c(min(timeseries$start_datetime), max(timeseries$end_datetime)))
      )
    })
    
    # Update the navbar when a location is clicked ############################
    # Listen for a click
    observeEvent(input$clicked_location_id, {
      outputs$change_tab <- "data"
      outputs$location_id <- input$clicked_location_id

    })
    
    return(outputs)
  })
}
