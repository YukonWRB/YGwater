# Dev notes:

# Keep same map feel/look as monitoring locations. Include pie chart markers, spider on max zoom
# Keep sidebar layout, fewer filters than locations map

wellRegistryUI <- function(id) {
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
    # All UI elements rendered in server function to allow multi-language functionality
    bslib::page_fluid(
      uiOutput(ns("sidebar_page"))
    )
  )
}

# mapLocs <- function(id, language) {
wellRegistry <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    # Server setup ####

    ns <- session$ns

    outputs <- reactiveValues() # This allows the module to pass values back to the main server

    if (session$userData$user_logged_in) {
      cached <- wwr_module_data(
        con = session$userData$AquaCache,
        env = session$userData$app_cache
      )
    } else {
      cached <- wwr_module_data(con = session$userData$AquaCache)
    }

    moduleData <- reactiveValues(
      wells = cached$wells,
      casing_materials = cached$casing_materials,
      boreholes_docs = cached$boreholes_docs,
      purposes = cached$purposes
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
    # TODO: change based on new filter names
    observeFilterInput("purpose")
    # observeFilterInput("media_type")
    # observeFilterInput("param_group")
    # observeFilterInput("param")

    # Create UI elements #####
    output$sidebar_page <- renderUI({
      req(moduleData, language)
      page_sidebar(
        sidebar = sidebar(
          title = NULL,
          bg = config$sidebar_bg, # Set in globals file'
          open = list(mobile = "always-above"),
          tagList(
            selectizeInput(
              ns("purpose"),
              label = tr("well_purpose", language$language),
              choices = stats::setNames(
                c("all", moduleData$purposes$borehole_well_purpose_id),
                c(
                  tr("all", language$language),
                  # TODO add column, reference in translation file
                  moduleData$purposes$purpose_name
                )
              ),
              multiple = TRUE
            ),
            sliderInput(
              ns("yrs"),
              label = tr("well_completion_yr", language$language),
              min = lubridate::year(min(
                moduleData$wells$completion_date,
                na.rm = TRUE
              )),
              max = lubridate::year(max(
                moduleData$wells$completion_date,
                na.rm = TRUE
              )),
              value = lubridate::year(c(
                min(moduleData$wells$completion_date, na.rm = TRUE),
                max(moduleData$wells$completion_date, na.rm = TRUE)
              )),
              step = 1,
              sep = ""
            ),
            # Add checkboxInput for wells with no known completion date
            checkboxInput(
              ns("include_unknown_completion"),
              label = tr("include_unknown_completion", language$language),
              value = TRUE
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
        "purpose",
        selected = "all"
      )
      updateSliderInput(
        session,
        "yrs",
        value = lubridate::year(c(
          min(moduleData$wells$completion_date, na.rm = TRUE),
          max(moduleData$wells$completion_date, na.rm = TRUE)
        ))
      )
      updateCheckboxInput(
        session,
        "include_unknown_completion",
        value = TRUE
      )
    }) # End of observeEvent for reset filters button

    # Update map popup based on language ###########################################
    popupData <- reactive({
      get_cached(
        key = if (language$abbrev == "fr") {
          "wwr_popup_data_fr"
        } else {
          "wwr_popup_data_en"
        },
        env = if (session$userData$user_logged_in) {
          session$userData$app_cache
        } else {
          .GlobalEnv
        },
        fetch_fun = function() {
          # Create popup text for each well. This is a bit slow when first loading the tab, but it doesn't need to be run again when the user modifies a filter.
          # Get well names
          popup_names <- moduleData$wells[, .(
            borehole_id,
            popup_name = data.table::fifelse(
              is.na(borehole_name),
              tr("borehole_unnamed", language$language),
              borehole_name
            )
          )]
          # drill date for each well
          drill_date <- moduleData$wells[, .(
            borehole_id,
            completion_date = data.table::fifelse(
              is.na(completion_date),
              tr("unknown", language$language),
              as.character(completion_date)
            )
          )]
          docs_count <- moduleData$boreholes_docs[,
            .(
              document_count = .N
            ),
            by = borehole_id
          ]

          # Combine all the data
          tmp <- data.table::copy(popup_names) # Use copy to avoid modifying the original data table
          tmp[
            drill_date,
            on = .(borehole_id),
            drill_date := completion_date
          ] # Join drill_date
          tmp[
            docs_count,
            on = .(borehole_id),
            document_count := i.document_count
          ]
          tmp[is.na(document_count), document_count := 0]

          tmp[,
            popup_html := paste0(
              "<strong>",
              popup_name,
              "</strong><br/>",
              substr(drill_date, 1, 10),
              "<br/>",
              "Documents: ",
              document_count
            )
          ]

          tmp
        },
        ttl = 60 * 60 * 24
      ) # Cache for 24 hours
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

    # Observe to filter and render location points on the map ############################
    observe({
      req(input$map_zoom, popupData(), language$language)
      popup_data <- popupData()

      wells_sub <- data.table::copy(moduleData$wells)
      wells_sub[
        moduleData$purposes,
        on = .(well_purpose_id = borehole_well_purpose_id),
        purpose_name := i.purpose_name
      ]

      if (!is.null(input$purpose)) {
        selected_purposes <- suppressWarnings(as.numeric(input$purpose))
        if (length(input$purpose) > 1) {
          wells_sub <- wells_sub[
            well_purpose_id %in% selected_purposes
          ]
        } else if (input$purpose != "all") {
          wells_sub <- wells_sub[
            well_purpose_id == selected_purposes
          ]
        }
      }

      if (!is.null(input$yrs)) {
        wells_sub[, completion_year := lubridate::year(completion_date)]
        if (isTRUE(input$include_unknown_completion)) {
          wells_sub <- wells_sub[
            is.na(completion_year) |
              (completion_year >= input$yrs[1] &
                completion_year <= input$yrs[2])
          ]
        } else {
          wells_sub <- wells_sub[
            !is.na(completion_year) &
              completion_year >= input$yrs[1] &
              completion_year <= input$yrs[2]
          ]
        }
      }

      wells_sub <- wells_sub[!is.na(latitude) & !is.na(longitude)]

      wells_sub <- wells_sub[
        popup_data,
        on = .(borehole_id),
        popup_html := popup_html
      ]

      unknown_label <- tr("unknown", language$language)
      wells_sub[, type_label := purpose_name]
      wells_sub[
        is.na(type_label) | trimws(type_label) == "",
        type_label := unknown_label
      ]

      loc_types <- sort(unique(wells_sub$type_label))

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

      wells_sub[
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
        leaflet::removeControl("well_purpose_legend")

      if (nrow(wells_sub) > 0) {
        # Build per-row SVG data URIs (matches nrow(wells_sub))
        icon_urls <- mapply(
          svg_data_uri,
          shape = wells_sub$shape,
          fill = wells_sub$color_hex,
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
            slug(wells_sub$type_label),
            " loc-col-",
            gsub("#", "", wells_sub$color_hex)
          )
        )

        map_proxy <- map_proxy %>%
          leaflet::addMarkers(
            data = wells_sub,
            lng = ~longitude,
            lat = ~latitude,
            popup = ~popup_html,
            icon = icons,
            clusterOptions = leaflet::markerClusterOptions(
              iconCreateFunction = htmlwidgets::JS("pieClusterIcon"), # pieClusterIcon defined in tags$script above
              maxClusterRadius = 80, # cluster radius in pixels
              spiderfyOnMaxZoom = TRUE
            )
          ) %>%
          leaflet::addControl(
            build_symbol_legend(
              type_map,
              title = tr("well_purpose", language$language),
              stroke = "#244C5A" # same stroke as for markers
            ),
            position = "bottomright",
            layerId = "well_purpose_legend",
            className = "custom-legend"
          )
      }
      map_proxy
    }) # End of observe for map filters and rendering location points
  })
}
