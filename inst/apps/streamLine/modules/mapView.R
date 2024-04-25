
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
       // Handles custom tooltips updates, binds tooltip properties to elements
            Shiny.addCustomMessageHandler('update-tooltip', function(message) {
                var selector = '#' + message.id;
                $(selector).attr('title', message.title)
                .tooltip('fixTitle').tooltip('hide');
            });
            
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
                  selectizeInput(ns("type"), "Data Type", choices = c("All" = "All"), multiple = TRUE), # choices and labels are updated in the server module
                  selectizeInput(ns("pType"), "Parameter Type", choices = c("All" = "All"), multiple = TRUE),
                  selectizeInput(ns("pGrp"), "Parameter Group", choices = c("All" = "All"), multiple = TRUE),
                  selectizeInput(ns("param"), "Parameter", choices = c("All" = "All"), multiple = TRUE),
                  selectizeInput(ns("proj"), "Project", choices = c("All" = "All"), multiple = TRUE),
                  selectizeInput(ns("net"), "Network", choices = c("All" = "All"), multiple = TRUE),
                  sliderInput(ns("yrs"), "With data between...", sep = "", min = 1897, max = lubridate::year(Sys.Date()), value = c(1897, lubridate::year(Sys.Date())), step = 1),
                  actionButton(ns("reset"), "Reset Filters"),
                  style = "opacity: 1; z-index: 400;"  # Adjust styling
    ) # End of absolutePanel
  ) # End of tagList
} # End of mapUI


map <- function(id, language, restoring, data) {
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c("reset", "map_bounds", "map_center", "map_zoom", "map_marker_mouseover", "map_marker_mouseout", "map_marker_click"))
    ns <- session$ns
    
    outputs <- reactiveValues()  # This allows the module to pass values back to the main server
    
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
    observeFilterInput("type")
    observeFilterInput("pType")
    observeFilterInput("pGrp")
    observeFilterInput("param")
    observeFilterInput("proj")
    observeFilterInput("net")
    
    # Create reactives to filter based on selections ############################
    #TODO: This portion is not functional yet and needs to be adapted to use data.tables
    # filteredYears <- reactive({
    #   data$timeseries[data$timeseries$start_datetime >= as.POSIXct(paste0(input$yrs[1], "-01-01 00:00"), tz = "UTC") & data$timeseries$end_datetime <= as.POSIXct(paste0(input$yrs[2], "-12-31 23:59:59"), tz = "UTC"), ]
    # })
    # filteredTypes <- reactive({
    #   if (input$type == "All") {
    #     data$timeseries$category
    #   } else {
    #     data$timeseries[data$timeseries$category %in% input$type,  ]
    #   }
    # })
    # filteredProjects <- reactive({
    #   if (input$proj == "All") {
    #     data$projects
    #   } else {
    #     data$projects[data$projects[translations[id == "generic_name_col", ..lang][[1]]] %in% tolower(input$proj),  ]
    #   }
    # })
    # filteredNetworks <- reactive({
    #   if (input$net == "All") {
    #     data$networks
    #   } else {
    #     data$networks[data$networks[translations[id == "generic_name_col", ..lang][[1]]] %in% tolower(input$net), ]
    #   }
    # })
    # filteredParamTypes <- reactive({
    #   if (input$pType == "All") {
    #     data$param_types
    #   } else {
    #     data$param_types[data$param_types[translations[id == "param_type_col", ..lang][[1]]] %in% tolower(input$pType), ]
    #   }
    # })
    # filteredParamGroup <- reactive({
    #   if (input$pGrp == "All") {
    #     data$param_groups
    #   } else {
    #     data$param_groups[data$param_groups[translations[id == "param_group_col", ..lang][[1]]] %in% tolower(input$pGrp),  ]
    #   }
    # })
    # filteredParameters <- reactive({
    #   if (input$param == "All") {
    #     data$parameters
    #   } else {
    #     data$parameters[data$parameters[translations[id == "param_name_col", ..lang][[1]]] %in% tolower(input$param), ]
    #   }
    # })
    
    # Update text (including map popup) based on language ###########################################
    observe({
      
      # Create popup text for each location. This is a bit slow when first loading the tab, but it doesn't need to be run again when the user modifies a filter.
      # Get location names
      popup_names <- data$locations[, .(location_id, popup_name = get(translations[id == "generic_name_col", get(language$language)][[1]]))]
      popup_names[, popup_name := titleCase(popup_name, language$abbrev)]
      # Aggregate time range for each location
      time_range <- data$timeseries[, .(
        start_time = min(start_datetime),
        end_time = max(end_datetime)
      ), by = location_id]
      # Get parameters per location
      param_name_col <- translations[id == "param_name_col", get(language$language)][[1]]
      to_text <- translations[id == "to", get(language$language)][[1]]
      tmp <- data$timeseries[data$parameters, on = .(parameter = param_code), allow.cartesian = TRUE]
      tmp[, formatted_param := paste(titleCase(get(param_name_col), language$abbrev), " (", 
                                                 format(as.Date(start_datetime), "%Y-%m-%d"), 
                                                 " ", to_text, " ", 
                                                 format(as.Date(end_datetime), "%Y-%m-%d"), ")", sep = "")]
      location_parameters <- tmp[, .(parameters = paste(formatted_param, collapse = "<br/>")), by = location_id]
      # Get networks per location
      network_col <- translations[id == "generic_name_col", get(language$language)][[1]]
      tmp <- data$locations_networks[data$networks, on = "network_id", allow.cartesian = TRUE]
      tmp[, formatted_network := titleCase(get(network_col), language$abbrev)]
      location_networks <- tmp[, .(networks = paste(formatted_network, collapse = "<br/>")), by = location_id]
      # Get projects per location
      projects_col <- translations[id == "generic_name_col", get(language$language)][[1]]
      tmp <- data$locations_projects[data$projects, on = "project_id", allow.cartesian = TRUE]
      tmp[, formatted_project := titleCase(get(projects_col), language$abbrev)]
      location_projects <- tmp[, .(projects = paste(formatted_project, collapse = "<br/>")), by = location_id]
      
      # Combine all the data
      tmp <- data.table::copy(data$locations)[, "location_id"]  # Use copy to avoid modifying the original data table
      tmp[popup_names, on = .(location_id), popup_name := popup_name]  # Join popup_name
      tmp[time_range, on = .(location_id), c("start_time", "end_time") := .(start_time, end_time)]  # Join time_range
      tmp[location_parameters, on = .(location_id), parameters := parameters]  # Join location_parameters
      tmp[location_networks, on = .(location_id), networks := networks]  # Join location_networks
      tmp[location_projects, on = .(location_id), projects := projects]  # Join location_projects
      

      data$locations$popup_html <- paste0(
        "<strong>", tmp$popup_name, "</strong><br/>",
        substr(tmp$start_time, 1, 10), " ", translations[id == "to", get(language$language)][[1]], " ", substr(tmp$end_time, 1, 10), "<br/><br/>",
        "<strong>", translations[id == "parameter(s)", get(language$language)][[1]], ":</strong><br/><i>", tmp$parameters, "</i><br/>",
        "<strong>", translations[id == "network(s)", get(language$language)][[1]], ":</strong><br/><i>", tmp$networks, "</i><br/>",
        "<strong>", translations[id == "project(s)", get(language$language)][[1]], ":</strong><br/><i>", ifelse(is.na(tmp$projects), "N/A", paste(tmp$projects, collapse = "<br/>")), "</i><br/>",
        "<br/><a href='#' onclick='changeTab(\"map-\", \"clicked_view_data\", \"", tmp$location_id, "\"); return false;'>View Data</a><br/>",
        "<a href='#' onclick='changeTab(\"map-\", \"clicked_view_plots\", \"", tmp$location_id, "\"); return false;'>View Plots</a>"
      )

      
      # Update the tooltip's text
      tooltipText <- translations[id == "tooltip_reset", get(language$language)][[1]]
      session$sendCustomMessage(type = 'update-tooltip', message = list(id = ns("infoIcon"), title = tooltipText))
      
      # Update selectizeInputs
      updateSelectizeInput(session, 
                           "type",
                           label = translations[id == "data_type", get(language$language)][[1]],
                           choices = stats::setNames(c("All", "discrete", "continuous"),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(c(translations[id == "discrete", get(language$language)][[1]], translations[id == "continuous", get(language$language)][[1]]), language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pType",
                           label = translations[id == "param_type", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$param_types$param_type_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_types[[translations[id == "param_type_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pGrp",
                           label = translations[id == "param_group", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$param_groups$group),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_groups[[translations[id == "param_group_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "param",
                           label = translations[id == "parameter", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$parameters$param_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "proj",
                           label = translations[id == "project", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$projects$project_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$projects[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "net",
                           label = translations[id == "network", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$networks$network_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$networks[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSliderInput(session,
                        "yrs",
                        label = translations[id == "year_filter", get(language$language)][[1]],
                        min = lubridate::year(min(data$timeseries$start_datetime)),
                        max = lubridate::year(max(data$timeseries$end_datetime)),
                        value = lubridate::year(c(min(data$timeseries$start_datetime), max(data$timeseries$end_datetime)))
      )
      updateActionButton(session,
                         "reset",
                         label = translations[id == "reset", get(language$language)][[1]]
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
      
      if (!is.null(input$type)) {
        if (length(input$type) > 1) {
          timeseries.sub <- data$timeseries[data$timeseries$category %in% input$type, ]
        } else {
          if (input$type == "All") {
            timeseries.sub <- data$timeseries
          } else {
            timeseries.sub <- data$timeseries[data$timeseries$category == input$type, ]
          }
        }
      } else {
        timeseries.sub <- data$timeseries
      }
      
      if (!is.null(input$param)) {
        if (length(input$param) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$parameter %in% input$param, ]
        } else {
          if (input$param == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$parameter == input$param, ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$pType)) {
        if (length(input$pType) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$param_type %in% input$pType, ]
        } else {
          if (input$pType == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$param_type == input$pType, ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$pGrp)) {
        if (length(input$pGrp) > 1) {
          select.params <- data$parameters[data$parameters$group %in% input$pGrp, "param_code"]
          timeseries.sub <- timeseries.sub[timeseries.sub$parameter %in% select.params, ]
        } else {
          if (input$pGrp == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            select.params <- data$parameters[data$parameters$group == input$pGrp, "param_code"]
            timeseries.sub <- timeseries.sub[timeseries.sub$parameter %in% select.params, ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$proj)) {
        if (length(input$proj) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% data$locations_projects[data$locations_projects$project_id %in% input$proj, "location_id"], ]
        } else {
          if (input$proj == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% data$locations_projects[data$locations_projects$project_id == input$proj, "location_id"], ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      if (!is.null(input$net)) {
        if (length(input$net) > 1) {
          timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% data$locations_networks[data$locations_networks$network_id %in% input$net, "location_id"], ]
        } else {
          if (input$net == "All") {
            timeseries.sub <- timeseries.sub
          } else {
            timeseries.sub <- timeseries.sub[timeseries.sub$location_id %in% data$locations_networks[data$locations_networks$network_id == input$net, "location_id"], ]
          }
        }
      } else {
        timeseries.sub <- timeseries.sub
      }
      
      timeseries.sub <- timeseries.sub[timeseries.sub$start_datetime <= as.POSIXct(paste0(input$yrs[2], "-12-31 23:59:59"), tz = "UTC") & timeseries.sub$end_datetime >= as.POSIXct(paste0(input$yrs[1], "-01-01 00:00"), tz = "UTC"),]
      
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
      
      updateSelectizeInput(session, 
                           "type",
                           choices = stats::setNames(c("All", "discrete", "continuous"),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(c(translations[id == "discrete", get(language$language)][[1]], translations[id == "continuous", get(language$language)][[1]]), language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pType",
                           choices = stats::setNames(c("All", param_types$param_type_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(param_types[[translations[id == "param_type_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pGrp",
                           choices = stats::setNames(c("All", param_groups$group),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(param_groups[[translations[id == "param_group_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "param",
                           choices = stats::setNames(c("All", parameters$param_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "proj",
                           choices = stats::setNames(c("All", projects$project_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(projects[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev))
                           )
      )
      updateSelectizeInput(session,
                           "net",
                           choices = stats::setNames(c("All", networks$network_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(networks[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev))
                           )
      )
      updateSliderInput(session,
                        "yrs",
                        label = translations[id == "year_filter", get(language$language)][[1]],
                        min = lubridate::year(min(timeseries$start_datetime)),
                        max = lubridate::year(max(timeseries$end_datetime)),
                        value = lubridate::year(c(min(timeseries$start_datetime), max(timeseries$end_datetime)))
      )
    })
    
    # Update the navbar when a location is clicked ############################
    # Listen for a click int he popup
    observeEvent(input$clicked_view_data, {
      if (!is.null(input$clicked_view_data)) {
        outputs$change_tab <- "data"
        outputs$location_id <- input$clicked_view_data
        shinyjs::runjs(sprintf("shinyjs.resetInput({name: 'map-clicked_view_data'})"))  # Reset the value to NULL to prevent an endless loop
      }
    })
    observeEvent(input$clicked_view_plots, {
      if (!is.null(input$clicked_view_plots)) {
        outputs$change_tab <- "plot"
        outputs$location_id <- input$clicked_view_plots
        shinyjs::runjs(sprintf("shinyjs.resetInput({name: 'map-clicked_view_plots'})"))  # Reset the value to NULL to prevent an endless loop
      }
    })
    
    return(outputs)  # Sends values back to the main server function
  })
}
