mapLocsUI <- function(id) {
  ns <- NS(id)

  tagList(
    # styling for custom legend
    tags$style(HTML(
      "
  /* Remove leaflet control background and border for custom legend */
  .leaflet-control.custom-legend {
    background: transparent !important;
    border: none !important;
    box-shadow: none !important;
  }
"
    )),
    # Custom JavaScript to create pie chart style cluster icons
    tags$script(HTML(
      "
    function pieClusterIcon(cluster) {
      var children = cluster.getAllChildMarkers();
      var counts = {}; // key: 'type|hex'
      children.forEach(function(m){
        var cls = (m.options.icon && m.options.icon.options.className) || '';
        var type = (cls.match(/loc-type-([^\\s]+)/) || [,'unknown'])[1];
        var col  = (cls.match(/loc-col-([0-9A-Fa-f]+)/) || [,'777777'])[1];
        var key = type + '|' + col;
        counts[key] = (counts[key] || 0) + 1;
      });
  
      var total = children.length;
      var keys = Object.keys(counts);
      var cx=24, cy=24, r=22;
  
      var ring = '';    // outer ring svg content
      var stroke = '#0c4e7a';
  
      if (keys.length === 1) {
        // FULL ring: one category only
        var hex = '#'+keys[0].split('|')[1];
        ring = '<circle cx=\"'+cx+'\" cy=\"'+cy+'\" r=\"'+r+'\" fill=\"'+hex+'\" stroke=\"'+stroke+'\" stroke-width=\"1\" />';
      } else {
        // Pie slices
        var start = 0;
        keys.forEach(function(k){
          var hex = '#'+k.split('|')[1];
          var val = counts[k];
          var theta = 2*Math.PI*val/total;
          // guard tiny float issues
          if (theta <= 0) return;
          var x1 = cx + r*Math.cos(start);
          var y1 = cy + r*Math.sin(start);
          var x2 = cx + r*Math.cos(start+theta);
          var y2 = cy + r*Math.sin(start+theta);
          var large = (theta > Math.PI) ? 1 : 0;
          var d = 'M '+cx+' '+cy+' L '+x1+' '+y1+' A '+r+' '+r+' 0 '+large+' 1 '+x2+' '+y2+' Z';
          ring += '<path d=\"'+d+'\" fill=\"'+hex+'\" stroke=\"'+stroke+'\" stroke-width=\"1\" />';
          start += theta;
        });
      }
  
      var html = '<div class=\"cluster-pie\">' +
        '<svg width=\"48\" height=\"48\" viewBox=\"0 0 48 48\">' +
          ring +
          '<circle cx=\"'+cx+'\" cy=\"'+cy+'\" r=\"14\" fill=\"white\" />' +
          '<text x=\"'+cx+'\" y=\"27\" text-anchor=\"middle\" font-size=\"12\" font-weight=\"600\">'+ total +'</text>' +
        '</svg></div>';
  
      return L.divIcon({
        html: html,
        className: 'marker-cluster marker-cluster-pie',
        iconSize: L.point(48,48)
      });
    }
  "
    )),
    # Function to change tabs from map popups
    tags$script(
      HTML(
        "
      // Function to change tabs based on namespace and input target, utilized in map popups
      function changeTab(namespace, target_input, locationId) {
        Shiny.setInputValue(namespace + target_input, locationId, {priority: 'event'});
      }
      // Resets Shiny input values to null, preventing unwanted reactive triggers
      shinyjs.resetInput = function(params) {
            Shiny.setInputValue(params.name, null, {priority: 'event'});
        }
    "
      )
    ),
    # All UI elements rendered in server function to allow multi-language functionality
    uiOutput(ns("banner")),
    uiOutput(ns("sidebar_page"))
  )
}

mapLocs <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    # Server setup ####

    ns <- session$ns

    outputs <- reactiveValues() # This allows the module to pass values back to the main server

    if (session$userData$user_logged_in) {
      cached <- map_location_module_data(
        con = session$userData$AquaCache,
        env = session$userData$app_cache
      )
    } else {
      cached <- map_location_module_data(con = session$userData$AquaCache)
    }

    moduleData <- reactiveValues(
      locations = cached$locations,
      timeseries = cached$timeseries,
      projects = cached$projects,
      networks = cached$networks,
      locations_projects = cached$locations_projects,
      locations_networks = cached$locations_networks,
      media_types = cached$media_types,
      parameters = cached$parameters,
      parameter_groups = cached$parameter_groups,
      parameter_sub_groups = cached$parameter_sub_groups
    )

    # Adjust filter selections based on if 'all' is selected (remove selections other than 'all')
    observeFilterInput <- function(inputId) {
      observeEvent(input[[inputId]], {
        # Check if 'all' is selected and adjust accordingly
        if (length(input[[inputId]]) > 1) {
          # If 'all' was selected last, remove all other selections
          if (input[[inputId]][length(input[[inputId]])] == "all") {
            updateSelectizeInput(session, inputId, selected = "all")
          } else if ("all" %in% input[[inputId]]) {
            # If 'all' is already selected and another option is selected, remove 'all'
            updateSelectizeInput(
              session,
              inputId,
              selected = input[[inputId]][length(input[[inputId]])]
            )
          }
        }
      })
    }
    observeFilterInput("data_type")
    observeFilterInput("media_type")
    observeFilterInput("param_group")
    observeFilterInput("param")
    observeFilterInput("project")
    observeFilterInput("network")

    # Create UI elements #####
    output$banner <- renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "mapLocs"
      )
    })

    output$sidebar_page <- renderUI({
      req(moduleData, language)
      page_sidebar(
        sidebar = sidebar(
          title = NULL,
          bg = config$sidebar_bg, # Set in globals file'
          open = list(mobile = "always-above"),
          tagList(
            checkboxInput(
              ns("cluster_points"),
              label = tr("cluster_points_label", language$language),
              value = TRUE
            ),
            selectizeInput(
              ns("data_type"),
              label = tooltip(
                trigger = list(
                  tr("data_format", language$language),
                  bsicons::bs_icon("info-circle-fill")
                ),
                tr("tooltip_discrete_continuous", language$language),
              ),
              choices = stats::setNames(
                c("all", "discrete", "continuous"),
                c(
                  tr("all_m", language$language),
                  c(
                    tr("discrete", language$language),
                    tr("continuous", language$language)
                  )
                )
              ),
              selected = "all",
              multiple = TRUE
            ),
            selectizeInput(
              ns("media_type"),
              label = tooltip(
                trigger = list(
                  tr("media", language$language),
                  bsicons::bs_icon("info-circle-fill")
                ),
                tr("tooltip_sample_media", language$language),
              ),
              choices = stats::setNames(
                c("all", moduleData$media_types$media_id),
                c(
                  tr("all_m", language$language),
                  moduleData$media_types[[tr(
                    "media_type_col",
                    language$language
                  )]]
                )
              ),
              selected = "all",
              multiple = TRUE
            ),
            selectizeInput(
              ns("param_group"),
              label = tr("param_group", language$language),
              choices = stats::setNames(
                c("all", moduleData$parameter_groups$group_id),
                c(
                  tr("all_m", language$language),
                  moduleData$parameter_groups[[tr(
                    "param_group_col",
                    language$language
                  )]]
                )
              ),
              selected = "all",
              multiple = TRUE
            ),
            selectizeInput(
              ns("param"),
              label = tr("parameter", language$language),
              choices = stats::setNames(
                c("all", moduleData$parameters$parameter_id),
                c(
                  tr("all_m", language$language),
                  moduleData$parameters[[tr(
                    "param_name_col",
                    language$language
                  )]]
                )
              ),
              selected = "all",
              multiple = TRUE
            ),
            selectizeInput(
              ns("project"),
              label = tr("project", language$language),
              choices = stats::setNames(
                c("all", moduleData$projects$project_id),
                c(
                  tr("all_m", language$language),
                  moduleData$projects[[tr(
                    "generic_name_col",
                    language$language
                  )]]
                )
              ),
              selected = "all",
              multiple = TRUE
            ),
            selectizeInput(
              ns("network"),
              label = tr("network", language$language),
              choices = stats::setNames(
                c("all", moduleData$networks$network_id),
                c(
                  tr("all_m", language$language),
                  moduleData$networks[[tr(
                    "generic_name_col",
                    language$language
                  )]]
                )
              ),
              selected = "all",
              multiple = TRUE
            ),
            sliderInput(
              ns("yrs"),
              label = tr("year_filter", language$language),
              min = lubridate::year(min(
                moduleData$timeseries$start_datetime,
                na.rm = TRUE
              )),
              max = lubridate::year(max(
                moduleData$timeseries$end_datetime,
                na.rm = TRUE
              )),
              value = lubridate::year(c(
                min(moduleData$timeseries$start_datetime, na.rm = TRUE),
                max(moduleData$timeseries$end_datetime, na.rm = TRUE)
              )),
              step = 1,
              sep = ""
            ),
            actionButton(
              ns("reset"),
              tr("reset", language$language),
              class = "btn btn-primary"
            )
          ) # End sidebar tagList
        ),
        leaflet::leafletOutput(ns("map"), height = '80vh'),
      )
    }) |>
      bindEvent(moduleData, language$language)

    # Reset all filters when reset button pressed ##################################
    observeEvent(input$reset, {
      req(moduleData)
      updateSelectizeInput(
        session,
        "data_type",
        choices = stats::setNames(
          c("all", "discrete", "continuous"),
          c(
            tr("all_m", language$language),
            c(
              tr("discrete", language$language),
              tr("continuous", language$language)
            )
          )
        ),
        selected = "all"
      )
      updateSelectizeInput(
        session,
        "media_type",
        choices = stats::setNames(
          c("all", moduleData$media_types$media_id),
          c(
            tr("all_m", language$language),
            moduleData$media_types[[tr("media_type_col", language$language)]]
          )
        ),
        selected = "all"
      )
      updateSelectizeInput(
        session,
        "param_group",
        choices = stats::setNames(
          c("all", moduleData$parameter_groups$group_id),
          c(
            tr("all_m", language$language),
            moduleData$parameter_groups[[tr(
              "param_group_col",
              language$language
            )]]
          )
        ),
        selected = "all"
      )
      updateSelectizeInput(
        session,
        "param",
        choices = stats::setNames(
          c("all", moduleData$parameters$parameter_id),
          c(
            tr("all_m", language$language),
            moduleData$parameters[[tr("param_name_col", language$language)]]
          )
        ),
        selected = "all"
      )
      updateSelectizeInput(
        session,
        "project",
        choices = stats::setNames(
          c("all", moduleData$projects$project_id),
          c(
            tr("all_m", language$language),
            moduleData$projects[[tr("generic_name_col", language$language)]]
          )
        ),
        selected = "all"
      )
      updateSelectizeInput(
        session,
        "network",
        choices = stats::setNames(
          c("all", moduleData$networks$network_id),
          c(
            tr("all_m", language$language),
            moduleData$networks[[tr("generic_name_col", language$language)]]
          )
        ),
        selected = "all"
      )
      updateSliderInput(
        session,
        "yrs",
        min = lubridate::year(min(
          moduleData$timeseries$start_datetime,
          na.rm = TRUE
        )),
        max = lubridate::year(max(
          moduleData$timeseries$end_datetime,
          na.rm = TRUE
        )),
        value = lubridate::year(c(
          min(moduleData$timeseries$start_datetime, na.rm = TRUE),
          max(moduleData$timeseries$end_datetime, na.rm = TRUE)
        ))
      )
    }) # End of observeEvent for reset filters button

    # Update map popup based on language ###########################################
    popupData <- reactive({
      get_cached(
        key = if (language$abbrev == "fr") {
          "map_popup_data_fr"
        } else {
          "map_popup_data_en"
        },
        env = if (session$userData$user_logged_in) {
          session$userData$app_cache
        } else {
          .GlobalEnv
        },
        fetch_fun = function() {
          # Create popup text for each location. This is a bit slow when first loading the tab, but it doesn't need to be run again when the user modifies a filter.
          # Get location names
          popup_names <- moduleData$locations[, .(
            location_id,
            popup_name = get(tr("generic_name_col", language$language))
          )]
          # Aggregate time range for each location
          time_range <- moduleData$timeseries[,
            .(
              start_time = min(start_datetime),
              end_time = max(end_datetime)
            ),
            by = location_id
          ]
          # Get parameters per location
          param_name_col <- tr("param_name_col", language$language)
          to_text <- tr("to", language$language)
          tmp <- moduleData$timeseries
          tmp[,
            formatted_param := paste(
              get(param_name_col),
              " (",
              format(as.Date(start_datetime), "%Y-%m-%d"),
              " ",
              to_text,
              " ",
              format(as.Date(end_datetime), "%Y-%m-%d"),
              ")",
              sep = ""
            )
          ]
          location_parameters <- tmp[,
            .(parameters = paste(formatted_param, collapse = "<br/>")),
            by = location_id
          ]
          # Get networks per location
          network_col <- tr("generic_name_col", language$language)
          tmp <- moduleData$locations_networks[
            moduleData$networks,
            on = "network_id",
            allow.cartesian = TRUE
          ]
          tmp[,
            formatted_network := get(network_col)
          ]
          location_networks <- tmp[,
            .(networks = paste(formatted_network, collapse = "<br/>")),
            by = location_id
          ]
          # Get projects per location
          projects_col <- tr("generic_name_col", language$language)
          tmp <- moduleData$locations_projects[
            moduleData$projects,
            on = "project_id",
            allow.cartesian = TRUE
          ]
          tmp[,
            formatted_project := get(projects_col)
          ]
          location_projects <- tmp[,
            .(projects = paste(formatted_project, collapse = "<br/>")),
            by = location_id
          ]

          # Combine all the data
          tmp <- data.table::copy(popup_names) # Use copy to avoid modifying the original data table
          tmp[
            time_range,
            on = .(location_id),
            c("start_time", "end_time") := .(start_time, end_time)
          ] # Join time_range
          tmp[
            location_parameters,
            on = .(location_id),
            parameters := parameters
          ] # Join location_parameters
          tmp[location_networks, on = .(location_id), networks := networks] # Join location_networks
          tmp[location_projects, on = .(location_id), projects := projects] # Join location_projects

          # Determine available data types for each location
          loc_data_types <- moduleData$timeseries[,
            .(
              has_discrete = any(data_type == "discrete"),
              has_continuous = any(data_type == "continuous")
            ),
            by = location_id
          ]
          tmp[
            loc_data_types,
            on = .(location_id),
            `:=`(
              has_discrete = i.has_discrete,
              has_continuous = i.has_continuous
            )
          ]

          tmp[,
            popup_links := {
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
            },
            by = location_id
          ]

          tmp[,
            popup_html := paste0(
              "<strong>",
              popup_name,
              "</strong><br/>",
              substr(start_time, 1, 10),
              " ",
              tr("to", language$language),
              " ",
              substr(end_time, 1, 10),
              "<br/><br/>",
              "<strong>",
              tr("parameter(s)", language$language),
              ":</strong><br/>",
              "<div style='max-height:100px; overflow-y:auto;'><i>",
              parameters,
              "</i></div><br/>", # Supposed to be scrollable
              "<strong>",
              tr("network(s)", language$language),
              ":</strong><br/><i>",
              networks,
              "</i><br/>",
              "<strong>",
              tr("project(s)", language$language),
              ":</strong><br/><i>",
              data.table::fifelse(
                is.na(projects),
                "N/A",
                paste(projects, collapse = "<br/>")
              ),
              "</i>",
              popup_links
            )
          ]

          tmp
        },
        ttl = 60 * 60
      ) # Cache for 1 hours
    })

    # Create the basic map ###########################################################
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(
        options = leaflet::leafletOptions(
          maxZoom = 15,
          zoomSnap = 0.5,
          zoomDelta = 0.5,
          zoomPxPerZoomLevel = 120
        )
      ) %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles(
          "Esri.WorldTopoMap",
          group = "Topographic"
        ) %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addLayersControl(
          baseGroups = c("Topographic", "Satellite")
        ) %>%
        leaflet::addScaleBar(
          position = "bottomleft",
          options = leaflet::scaleBarOptions(imperial = FALSE)
        ) %>%
        leaflet::setView(lng = -135.05, lat = 64.00, zoom = 5) %>% # Center on Yukon
        htmlwidgets::onRender(
          "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }"
        )
    }) |>
      bindEvent(language$language)

    # Filter the map data based on user's selection and add points to map ############################

    # ---- helper: build a data-URI SVG icon ----
    svg_data_uri <- function(
      shape = c("circle", "square", "diamond"),
      fill = "#2C7FB8",
      size = 20,
      stroke = "#0c4e7aff",
      stroke_width = 1
    ) {
      shape <- match.arg(shape)
      s <- size
      svg <- switch(
        shape,
        circle = sprintf(
          '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d">
         <circle cx="%d" cy="%d" r="%d" fill="%s" stroke="%s" stroke-width="%d"/>
       </svg>',
          s,
          s,
          s / 2,
          s / 2,
          floor((s - 2 * stroke_width) / 2),
          fill,
          stroke,
          stroke_width
        ),
        square = sprintf(
          '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d">
         <rect x="%d" y="%d" width="%d" height="%d" fill="%s" stroke="%s" stroke-width="%d" />
       </svg>',
          s,
          s,
          stroke_width,
          stroke_width,
          s - 2 * stroke_width,
          s - 2 * stroke_width,
          fill,
          stroke,
          stroke_width
        ),
        diamond = sprintf(
          '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %1$d %1$d">
         <polygon points="%d,%d %d,%d %d,%d %d,%d" fill="%s" stroke="%s" stroke-width="%d"/>
       </svg>',
          s,
          s,
          s / 2,
          stroke_width,
          s - stroke_width,
          s / 2,
          s / 2,
          s - stroke_width,
          stroke_width,
          s / 2,
          fill,
          stroke,
          stroke_width
        )
      )
      paste0(
        "data:image/svg+xml;base64,",
        base64enc::base64encode(charToRaw(svg))
      )
    }

    # Helpers to build legend HTML (default leaflet legend doesn't support shapes)
    build_symbol_legend <- function(type_map, title, stroke = "#244C5A") {
      rows <- mapply(
        function(shape, fill, label) {
          icon <- svg_data_uri(
            shape = shape,
            fill = fill,
            size = 14,
            stroke = stroke,
            stroke_width = 1
          )
          sprintf(
            "<div style='display:flex;align-items:center;margin:2px 0;'>
           <img src='%s' style='width:14px;height:14px;margin-right:6px;'/>
           <span>%s</span>
         </div>",
            icon,
            htmltools::htmlEscape(label)
          )
        },
        type_map$shape,
        type_map$color_hex,
        type_map$type_label,
        USE.NAMES = FALSE
      )

      htmltools::HTML(sprintf(
        "<div style='background: rgba(255,255,255,0.9);
                 padding:8px 10px; border-radius:6px;
                 box-shadow: 0 1px 4px rgba(0,0,0,0.25);
                 font: 12px/1.2 sans-serif;'>
       <div style='font-weight:600; margin-bottom:6px;'>%s</div>
       %s
     </div>",
        htmltools::htmlEscape(title),
        paste(rows, collapse = "")
      ))
    }

    observe({
      req(input$map_zoom, popupData(), language$language)
      popup_data <- popupData()
      if (!is.null(input$data_type)) {
        if (length(input$data_type) > 1) {
          timeseries.sub <- moduleData$timeseries[
            moduleData$timeseries$data_type %in% input$data_type,
          ]
        } else {
          if (input$data_type == "all") {
            timeseries.sub <- moduleData$timeseries
          } else {
            timeseries.sub <- moduleData$timeseries[
              moduleData$timeseries$data_type == input$data_type,
            ]
          }
        }
      } else {
        timeseries.sub <- moduleData$timeseries
      }

      if (!is.null(input$media_type)) {
        if (length(input$media_type) > 1) {
          timeseries.sub <- timeseries.sub[
            timeseries.sub$media_id %in% input$media_type,
          ]
        } else {
          if (input$media_type != "all") {
            timeseries.sub <- timeseries.sub[
              timeseries.sub$media_id == input$media_type,
            ]
          }
        }
      }

      if (!is.null(input$param_group)) {
        if (length(input$param_group) > 1) {
          select.params <- moduleData$parameters[
            moduleData$parameters$group_id %in% input$param_group,
            "parameter_id"
          ]$parameter_id
          timeseries.sub <- timeseries.sub[parameter_id %in% select.params, ]
        } else {
          if (input$param_group != "all") {
            select.params <- moduleData$parameters[
              moduleData$parameters$group_id == input$param_group,
              "parameter_id"
            ]$parameter_id
            if (length(select.params) > 1) {
              timeseries.sub <- timeseries.sub[
                parameter_id %in% select.params,
              ]
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

      if (!is.null(input$project)) {
        if (length(input$project) > 1) {
          ids <- moduleData$locations_projects[
            moduleData$locations_projects$project_id %in% input$project,
            "location_id"
          ]$location_id
          if (length(ids) > 1) {
            timeseries.sub <- timeseries.sub[
              timeseries.sub$location_id %in% ids,
            ]
          } else {
            timeseries.sub <- timeseries.sub[
              timeseries.sub$location_id == ids,
            ]
          }
        } else {
          if (input$project != "all") {
            ids <- moduleData$locations_projects[
              moduleData$locations_projects$project_id == input$project,
              "location_id"
            ]$location_id
            if (length(ids) > 1) {
              timeseries.sub <- timeseries.sub[
                timeseries.sub$location_id %in% ids,
              ]
            } else {
              timeseries.sub <- timeseries.sub[
                timeseries.sub$location_id == ids,
              ]
            }
          }
        }
      }

      if (!is.null(input$network)) {
        if (length(input$network) > 1) {
          ids <- moduleData$locations_networks[
            moduleData$locations_networks$network_id %in% input$network,
            "location_id"
          ]$location_id
          if (length(ids) > 1) {
            timeseries.sub <- timeseries.sub[
              timeseries.sub$location_id %in% ids,
            ]
          } else {
            timeseries.sub <- timeseries.sub[
              timeseries.sub$location_id == ids,
            ]
          }
        } else {
          if (input$network != "all") {
            ids <- moduleData$locations_networks[
              moduleData$locations_networks$network_id == input$network,
              "location_id"
            ]$location_id
            if (length(ids) > 1) {
              timeseries.sub <- timeseries.sub[
                timeseries.sub$location_id %in% ids,
              ]
            } else {
              timeseries.sub <- timeseries.sub[
                timeseries.sub$location_id == ids,
              ]
            }
          }
        }
      }
      if (!is.null(input$yrs)) {
        timeseries.sub <- timeseries.sub[
          timeseries.sub$start_datetime <=
            as.POSIXct(paste0(input$yrs[2], "-12-31 23:59:59"), tz = "UTC") &
            timeseries.sub$end_datetime >=
              as.POSIXct(paste0(input$yrs[1], "-01-01 00:00"), tz = "UTC"),
        ]
      }

      loc.sub <- moduleData$locations[
        moduleData$locations$location_id %in% timeseries.sub$location_id,
      ]
      loc.sub <- loc.sub[
        popup_data,
        on = .(location_id),
        popup_html := popup_html
      ]

      type_col <- if (language$abbrev == "fr") "type_fr" else "type"
      unknown_label <- tr("unknown", language$language)
      loc.sub[, type_label := get(type_col)]
      loc.sub[
        is.na(type_label) | trimws(type_label) == "",
        type_label := unknown_label
      ]
      loc.sub[, type_label := type_label]

      loc_types <- sort(unique(loc.sub$type_label))

      shape_choices <- c("circle", "square", "diamond")

      # YG primary colors
      # color_hex_choices <- c(
      #   "#0097A9",
      #   "#F2A900",
      #   "#DC4405",
      #   "#244C5A",
      #   "#512A44",
      #   "#7A9A01",
      #   "#F781BF"
      # )

      # YG primary colors plus some complementary colors
      color_hex_choices <- c(
        "#0097A9",
        "#DC4405",
        "#F2A900",
        "#244C5A",
        "#512A44",
        "#7A9A01",
        "#00BFC4",
        "#FFD24D",
        "#F76C5E",
        "#005F73",
        "#8E6C8A",
        "#A3C72D",
        "#B8B8B8",
        "#5E5E5E",
        "#C83E8A"
      )

      # Create a mapping of location types to colors and shapes
      type_map <- data.table::data.table(
        type_label = loc_types,
        color_hex = color_hex_choices[
          ((seq_along(loc_types) - 1) %% length(color_hex_choices)) + 1
        ],
        shape = shape_choices[
          ((seq_along(loc_types) - 1) %% length(shape_choices)) + 1
        ]
      )

      loc.sub[
        type_map,
        on = .(type_label),
        `:=`(
          color_hex = i.color_hex,
          shape = i.shape
        )
      ]

      map_proxy <- leaflet::leafletProxy("map", session = session) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::removeControl("location_type_legend")

      if (nrow(loc.sub) > 0) {
        # Build per-row SVG data URIs (matches nrow(loc.sub))
        icon_urls <- mapply(
          svg_data_uri,
          shape = loc.sub$shape,
          fill = loc.sub$color_hex,
          MoreArgs = list(size = 20, stroke = "#244C5A", stroke_width = 1),
          USE.NAMES = FALSE
        )

        # Create icons with custom class names, used for pie chart cluster icons
        slug <- function(x) gsub("[^A-Za-z0-9_-]", "_", x)
        icons <- leaflet::icons(
          iconUrl = icon_urls,
          iconWidth = 15,
          iconHeight = 15,
          className = paste0(
            "loc-type-",
            slug(loc.sub$type_label),
            " loc-col-",
            gsub("#", "", loc.sub$color_hex)
          )
        )

        map_proxy <- map_proxy %>%
          leaflet::addMarkers(
            data = loc.sub,
            lng = ~longitude,
            lat = ~latitude,
            popup = ~popup_html,
            icon = icons,
            clusterOptions = if (isTRUE(input$cluster_points)) {
              leaflet::markerClusterOptions(
                iconCreateFunction = htmlwidgets::JS("pieClusterIcon"), # pieClusterIcon defined in tags$script above
                maxClusterRadius = 80, # cluster radius in pixels
                spiderfyOnMaxZoom = TRUE
              )
            } else {
              NULL
            }
          ) %>%
          leaflet::addControl(
            build_symbol_legend(
              type_map,
              title = tr("location_type_legend", language$language),
              stroke = "#244C5A" # same stroke as for markers
            ),
            position = "bottomright",
            layerId = "location_type_legend",
            className = "custom-legend"
          )
      }
      map_proxy
    }) # End of observe for map filters and rendering location points

    # Pass a message to the main map server when a location link is clicked ############################
    # Listen for a click in the popup
    observeEvent(input$clicked_dl_data_discrete, {
      if (!is.null(input$clicked_dl_data_discrete)) {
        outputs$change_tab <- "discData"
        outputs$location_id <- input$clicked_dl_data_discrete
        shinyjs::runjs(sprintf(
          "shinyjs.resetInput({name: '%s'})",
          session$ns("clicked_dl_data_discrete")
        )) # Reset the value to NULL to prevent an endless loop
      }
    })
    observeEvent(input$clicked_dl_data_continuous, {
      if (!is.null(input$clicked_dl_data_continuous)) {
        outputs$change_tab <- "contData"
        outputs$location_id <- input$clicked_dl_data_continuous
        shinyjs::runjs(sprintf(
          "shinyjs.resetInput({name: '%s'})",
          session$ns("clicked_dl_data_continuous")
        )) # Reset the value to NULL to prevent an endless loop
      }
    })
    observeEvent(input$clicked_view_plots_discrete, {
      if (!is.null(input$clicked_view_plots_discrete)) {
        outputs$change_tab <- "discPlot"
        outputs$location_id <- input$clicked_view_plots_discrete
        shinyjs::runjs(sprintf(
          "shinyjs.resetInput({name: '%s'})",
          session$ns("clicked_view_plots_discrete")
        )) # Reset the value to NULL to prevent an endless loop
      }
    })
    observeEvent(input$clicked_view_plots_continuous, {
      if (!is.null(input$clicked_view_plots_continuous)) {
        outputs$change_tab <- "contPlot"
        outputs$location_id <- input$clicked_view_plots_continuous
        shinyjs::runjs(sprintf(
          "shinyjs.resetInput({name: '%s'})",
          session$ns("clicked_view_plots_continuous")
        )) # Reset the value to NULL to prevent an endless loop
      }
    })
    return(outputs) # Sends values back to the main server function
  })
}
