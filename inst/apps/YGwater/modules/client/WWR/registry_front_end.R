wellRegistryUI <- function(id) {
  ns <- NS(id)

  tagList(
    # styling for custom legend
    tags$style(
      HTML(
        "
  /* Remove leaflet control background and border for custom legend */
  .leaflet-control.custom-legend {
    background: transparent !important;
    border: none !important;
    box-shadow: none !important;
  }
"
      ),
      # Make chekboxes tighter together in the sidebar
      HTML(
        "
    .compact-checkboxes .checkbox {
      margin-top: -5px;
      margin-bottom: -5px;
    }
  "
      )
    ),
    # Custom JavaScript to create pie chart style cluster icons
    tags$script(HTML(
      "
    function pieClusterIcon(cluster) {
      var children = cluster.getAllChildMarkers();
      var counts = {}; // key: 'purpose|hex|fill style'
      children.forEach(function(m){
        var cls = (m.options.icon && m.options.icon.options.className) || '';
        var type = (cls.match(/loc-type-([^\\s]+)/) || [,'unknown'])[1];
        var col  = (cls.match(/loc-col-([0-9A-Fa-f]+)/) || [,'777777'])[1];
        var fillStyle = (cls.match(/loc-fill-([^\\s]+)/) || [,'filled'])[1];
        var key = type + '|' + col + '|' + fillStyle;
        counts[key] = (counts[key] || 0) + 1;
      });
  
      var total = children.length;
      var keys = Object.keys(counts);
      var cx=24, cy=24, r=22;
  
      var ring = '';    // outer ring svg content
      var stroke = '#0c4e7a';
  
      if (keys.length === 1) {
        // FULL ring: one category only
        var parts = keys[0].split('|');
        var hex = '#'+parts[1];
        var hollow = parts[2] === 'hollow';
        var ringFill = hollow ? 'white' : hex;
        var ringStroke = hollow ? hex : stroke;
        var ringStrokeWidth = hollow ? 5 : 1;
        ring = '<circle cx=\"'+cx+'\" cy=\"'+cy+'\" r=\"'+r+'\" fill=\"'+ringFill+'\" stroke=\"'+ringStroke+'\" stroke-width=\"'+ringStrokeWidth+'\" />';
      } else {
        // Pie slices
        var start = 0;
        keys.forEach(function(k){
          var parts = k.split('|');
          var hex = '#'+parts[1];
          var hollow = parts[2] === 'hollow';
          var sliceFill = hollow ? 'white' : hex;
          var sliceStroke = hollow ? hex : stroke;
          var sliceStrokeWidth = hollow ? 5 : 1;
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
          ring += '<path d=\"'+d+'\" fill=\"'+sliceFill+'\" stroke=\"'+sliceStroke+'\" stroke-width=\"'+sliceStrokeWidth+'\" />';
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
      uiOutput(ns("banner")),
      page_sidebar(
        sidebar = sidebar(
          title = NULL,
          bg = config$sidebar_bg,
          open = list(mobile = "always-above"),
          uiOutput(ns("sidebar_controls"))
        ),
        leaflet::leafletOutput(ns("map"), height = '80vh')
      )
    )
  )
}

# mapLocs <- function(id, language) {
wellRegistry <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    # Server setup ####

    ns <- session$ns

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
      boreholes_docs = cached$boreholes_docs,
      documents = cached$documents,
      purposes = cached$purposes
    )

    completion_year_range <- function() {
      years <- moduleData$wells$completion_year
      years <- years[!is.na(years) & is.finite(years)]
      if (!length(years)) {
        current_year <- lubridate::year(Sys.Date())
        return(c(current_year - 1L, current_year))
      }
      year_range <- range(years)
      if (year_range[1] == year_range[2]) {
        year_range <- year_range + c(-1L, 1L)
      }
      year_range
    }

    # Adjust filter selections based on if 'all' is selected (remove selections other than 'all')
    observeFilterInput <- function(inputId) {
      observeEvent(
        input[[inputId]],
        {
          values <- input[[inputId]]
          if (is.null(values) || length(values) == 0) {
            updateSelectizeInput(session, inputId, selected = "all")
            return()
          }
          values <- as.character(values)
          if (length(values) > 1 && "all" %in% values) {
            selected <- if (identical(values[[length(values)]], "all")) {
              "all"
            } else {
              setdiff(values, "all")
            }
            updateSelectizeInput(session, inputId, selected = selected)
          }
        },
        ignoreNULL = FALSE
      )
    }
    observeFilterInput("purpose")

    purpose_label_for_scope <- function(
      registry_scope = input$borehole_well_scope %||% "with_wells"
    ) {
      switch(
        registry_scope,
        without_wells = tr("borehole_purpose", language$language),
        all = tr("well_or_borehole_purpose", language$language),
        tr("well_purpose", language$language)
      )
    }

    purpose_choices_for_scope <- function(
      registry_scope = input$borehole_well_scope %||% "with_wells"
    ) {
      purpose_ids <- switch(
        registry_scope,
        without_wells = moduleData$wells[
          has_well == FALSE,
          unique(borehole_purpose_id)
        ],
        all = unique(c(
          moduleData$wells[has_well == TRUE, well_purpose_id],
          moduleData$wells[has_well == FALSE, borehole_purpose_id]
        )),
        moduleData$wells[has_well == TRUE, unique(well_purpose_id)]
      )
      purpose_ids <- purpose_ids[!is.na(purpose_ids)]
      purpose_column <- tr(
        "borehole_well_purpose_col",
        language$language
      )
      purposes_sorted <- moduleData$purposes[
        borehole_well_purpose_id %in% purpose_ids
      ][order(get(purpose_column))]

      stats::setNames(
        c("all", purposes_sorted$borehole_well_purpose_id),
        c(
          tr("all_m", language$language),
          purposes_sorted[[purpose_column]]
        )
      )
    }

    # Create UI elements #####

    output$banner <- renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "wellRegistry"
      )
    })

    output$sidebar_controls <- renderUI({
      req(moduleData, language)

      # If there are no boreholes, return a message instead of the filters
      if (nrow(moduleData$wells) == 0) {
        return(
          div(
            class = "p-3",
            "There are no wells in the database yet!"
          )
        )
      }
      year_range <- completion_year_range()
      tagList(
        checkboxInput(
          ns("cluster_points"),
          label = tr("cluster_points_label", language$language),
          value = TRUE
        ),
        selectizeInput(
          ns("purpose"),
          label = purpose_label_for_scope(),
          choices = purpose_choices_for_scope(),
          multiple = TRUE,
          selected = "all"
        ),
        textInput(
          ns("well_name_search"),
          label = tr("well_name_contains", language$language),
          value = "",
          placeholder = tr(
            "well_name_contains_placeholder",
            language$language
          )
        ),
        div(
          class = "compact-checkboxes",
          style = "margin-top: 10px;", # Tiny space to separate the text input from the checkboxes
          checkboxInput(
            ns("well_name_starts_with"),
            label = tr("starts_with", language$language),
            value = FALSE
          ),
          checkboxInput(
            ns("well_name_ends_with"),
            label = tr("ends_with", language$language),
            value = FALSE
          ),
          checkboxInput(
            ns("well_name_case_sensitive"),
            label = tr("case_sensitive", language$language),
            value = FALSE
          )
        ),
        sliderInput(
          ns("yrs"),
          label = tr("well_completion_yr", language$language),
          min = year_range[1],
          max = year_range[2],
          value = year_range,
          step = 1,
          sep = ""
        ),
        div(
          class = "compact-checkboxes",
          # Add checkboxInput for wells with no known completion date
          checkboxInput(
            ns("include_unknown_completion"),
            label = tr(
              "include_unknown_well_completion",
              language$language
            ),
            value = TRUE
          ),
          # Checkbox for missing well depth
          checkboxInput(
            ns("include_missing_depth"),
            label = tr("include_missing_well_depth", language$language),
            value = TRUE
          ),
          # Checkbox for wells with missing depth to water
          checkboxInput(
            ns("include_missing_depth_to_water"),
            label = tr(
              "include_missing_well_depth_to_water",
              language$language
            ),
            value = TRUE
          )
        ),
        selectizeInput(
          ns("borehole_well_scope"),
          label = tr("borehole_well_scope", language$language),
          choices = stats::setNames(
            c("with_wells", "without_wells", "all"),
            c(
              tr("show_boreholes_with_wells", language$language),
              tr("show_boreholes_without_wells", language$language),
              tr("show_all_boreholes", language$language)
            )
          ),
          selected = "with_wells"
        ),
        # # Checkbox for wells with missing depth to bedrock
        # checkboxInput(
        #   ns("include_missing_depth_to_bedrock"),
        #   label = tr(
        #     "include_missing_well_depth_to_bedrock",
        #     language$language
        #   ),
        #   value = TRUE
        # ),
        actionButton(
          ns("reset"),
          tr("reset", language$language),
          class = "btn btn-primary"
        )
      )
    }) |>
      bindEvent(moduleData, language$language)

    observeEvent(
      list(input$borehole_well_scope, language$language),
      {
        purpose_choices <- purpose_choices_for_scope()
        selected_purposes <- as.character(input$purpose %||% "all")
        available_purposes <- as.character(unname(purpose_choices))
        if (!all(selected_purposes %in% available_purposes)) {
          selected_purposes <- "all"
        }
        updateSelectizeInput(
          session,
          "purpose",
          label = purpose_label_for_scope(),
          choices = purpose_choices,
          selected = selected_purposes
        )
      },
      ignoreInit = FALSE
    )

    # Reset all filters when reset button pressed ##################################
    observeEvent(input$reset, {
      req(moduleData)
      year_range <- completion_year_range()
      updateSelectizeInput(
        session,
        "purpose",
        selected = "all"
      )
      updateSliderInput(
        session,
        "yrs",
        value = year_range
      )
      updateCheckboxInput(
        session,
        "include_unknown_completion",
        value = TRUE
      )
      updateCheckboxInput(
        session,
        "include_missing_depth",
        value = TRUE
      )
      updateCheckboxInput(
        session,
        "include_missing_depth_to_water",
        value = TRUE
      )
      updateTextInput(
        session,
        "well_name_search",
        value = ""
      )
      updateCheckboxInput(
        session,
        "well_name_starts_with",
        value = FALSE
      )
      updateCheckboxInput(
        session,
        "well_name_ends_with",
        value = FALSE
      )
      updateCheckboxInput(
        session,
        "well_name_case_sensitive",
        value = FALSE
      )
      updateSelectizeInput(
        session,
        "borehole_well_scope",
        selected = "with_wells"
      )
      # updateCheckboxInput(
      #   session,
      #   "include_missing_depth_to_bedrock",
      #   value = TRUE
      # )
    }) # End of observeEvent for reset filters button

    # Build one popup per registry row. Borehole-level fields and documents are
    # repeated for associated wells, while borehole-only rows remain distinct.
    popupData <- reactive({
      unknown_label <- tr("unknown", language$language)
      bedrock_not_reached_label <- if (identical(language$abbrev, "fr")) {
        "Non atteint"
      } else {
        "Not reached"
      }
      well_label <- if (identical(language$abbrev, "fr")) "Puits" else "Well"
      borehole_label <- if (identical(language$abbrev, "fr")) {
        "Trou de forage"
      } else {
        "Borehole"
      }

      docs_by_borehole <- data.table::copy(moduleData$boreholes_docs)
      docs_by_borehole[
        moduleData$documents,
        on = .(document_id),
        `:=`(
          document_name = i.name,
          document_format = i.format
        )
      ]
      registry_documents <- merge(
        moduleData$wells[, .(registry_id, borehole_id)],
        docs_by_borehole,
        by = "borehole_id",
        all = FALSE,
        sort = FALSE,
        allow.cartesian = TRUE
      )
      registry_documents[,
        download_id := paste0(
          "download_document_",
          registry_id,
          "_",
          document_id
        )
      ]
      doc_links <- registry_documents[,
        .(
          document_count = .N,
          document_links = paste0(
            "<li>",
            vapply(
              seq_len(.N),
              function(idx) {
                doc_label <- document_name[idx]
                if (is.na(doc_label) || !nzchar(doc_label)) {
                  doc_label <- paste(
                    tr("document_download", language$language),
                    document_id[idx]
                  )
                }
                if (
                  !is.na(document_format[idx]) &&
                    nzchar(document_format[idx])
                ) {
                  doc_label <- paste0(
                    doc_label,
                    " (",
                    document_format[idx],
                    ")"
                  )
                }
                as.character(shiny::downloadLink(
                  ns(download_id[idx]),
                  label = htmltools::htmlEscape(doc_label)
                ))
              },
              character(1)
            ),
            "</li>",
            collapse = ""
          )
        ),
        by = registry_id
      ]

      purposes_lookup <- moduleData$purposes[, .(
        borehole_well_purpose_id,
        purpose_name = YGwater:::escape_html_text(
          get(tr("borehole_well_purpose_col", language$language))
        )
      )]

      tmp <- data.table::copy(moduleData$wells)
      tmp[
        purposes_lookup,
        on = .(well_purpose_id = borehole_well_purpose_id),
        purpose_name := i.purpose_name
      ]
      tmp[
        doc_links,
        on = .(registry_id),
        `:=`(
          document_count = i.document_count,
          document_links = i.document_links
        )
      ]
      tmp[is.na(document_count), document_count := 0L]
      tmp[,
        `:=`(
          well_display_name = data.table::fcase(
            !is.na(well_name) & nzchar(trimws(well_name)),
            YGwater:::escape_html_text(well_name),
            !is.na(borehole_name) & nzchar(trimws(borehole_name)),
            YGwater:::escape_html_text(borehole_name),
            default = unknown_label
          ),
          borehole_display_name = data.table::fcase(
            !is.na(borehole_name) & nzchar(trimws(borehole_name)),
            YGwater:::escape_html_text(borehole_name),
            default = tr("borehole_unnamed", language$language)
          ),
          drill_date = data.table::fifelse(
            is.na(completion_date),
            unknown_label,
            as.character(completion_date)
          ),
          depth_to_bedrock_display = data.table::fcase(
            !is.na(depth_to_bedrock_m),
            as.character(round(depth_to_bedrock_m, 4)),
            !is.na(bedrock_reached) & !bedrock_reached,
            bedrock_not_reached_label,
            default = unknown_label
          )
        )
      ]

      tmp[,
        popup_html := paste0(
          data.table::fifelse(
            has_well,
            paste0(
              "<strong>",
              well_label,
              ": ",
              well_display_name,
              "</strong><br/>",
              data.table::fifelse(
                well_display_name != borehole_display_name,
                paste0(
                  "<div><strong>",
                  borehole_label,
                  ":</strong> ",
                  borehole_display_name,
                  "</div>"
                ),
                ""
              )
            ),
            paste0(
              "<strong>",
              borehole_label,
              ": ",
              borehole_display_name,
              "</strong><br/>"
            )
          ),
          "<div><strong>Year drilled:</strong> ",
          drill_date,
          "</div>",
          "<div><strong>Depth (m):</strong> ",
          data.table::fifelse(
            is.na(depth_m),
            unknown_label,
            as.character(round(depth_m, 4))
          ),
          "</div>",
          "<div><strong>Depth to bedrock (m):</strong> ",
          depth_to_bedrock_display,
          "</div>",
          data.table::fifelse(
            has_well,
            paste0(
              "<div><strong>Well purpose:</strong> ",
              data.table::fifelse(
                is.na(purpose_name) | !nzchar(purpose_name),
                unknown_label,
                purpose_name
              ),
              "</div>",
              "<div><strong>Static water level (m):</strong> ",
              data.table::fifelse(
                is.na(static_water_level_m),
                unknown_label,
                as.character(round(static_water_level_m, 4))
              ),
              "</div>",
              "<div><strong>Estimated yield (L/s):</strong> ",
              data.table::fifelse(
                is.na(estimated_yield_lps),
                unknown_label,
                as.character(round(estimated_yield_lps, 2))
              ),
              "</div>",
              "<div><strong>Casing diameter (mm):</strong> ",
              data.table::fifelse(
                is.na(casing_diameter_mm),
                unknown_label,
                as.character(round(casing_diameter_mm, 1))
              ),
              "</div>",
              "<div><strong>Stick-up height (m):</strong> ",
              data.table::fifelse(
                is.na(stick_up_height_m),
                unknown_label,
                as.character(round(stick_up_height_m, 4))
              ),
              "</div>",
              "<div><strong>Seal material:</strong> ",
              data.table::fifelse(
                is.na(seal_material) | !nzchar(seal_material),
                unknown_label,
                YGwater:::escape_html_text(seal_material)
              ),
              "</div>",
              "<div><strong>Seal diameter (mm):</strong> ",
              data.table::fifelse(
                is.na(seal_diameter_mm),
                unknown_label,
                as.character(round(seal_diameter_mm, 1))
              ),
              "</div>",
              "<div><strong>Seal depth from (m):</strong> ",
              data.table::fifelse(
                is.na(seal_depth_from_m),
                unknown_label,
                as.character(round(seal_depth_from_m, 4))
              ),
              "</div>",
              "<div><strong>Seal depth to (m):</strong> ",
              data.table::fifelse(
                is.na(seal_depth_to_m),
                unknown_label,
                as.character(round(seal_depth_to_m, 4))
              ),
              "</div>",
              "<div><strong>Screen material:</strong> ",
              data.table::fifelse(
                is.na(screen_material) | !nzchar(screen_material),
                unknown_label,
                YGwater:::escape_html_text(screen_material)
              ),
              "</div>",
              "<div><strong>Screen type:</strong> ",
              data.table::fifelse(
                is.na(screen_type) | !nzchar(screen_type),
                unknown_label,
                YGwater:::escape_html_text(screen_type)
              ),
              "</div>",
              "<div><strong>Screen top (m):</strong> ",
              data.table::fifelse(
                is.na(screen_top_depth_m),
                unknown_label,
                as.character(round(screen_top_depth_m, 4))
              ),
              "</div>",
              "<div><strong>Screen bottom (m):</strong> ",
              data.table::fifelse(
                is.na(screen_bottom_depth_m),
                unknown_label,
                as.character(round(screen_bottom_depth_m, 4))
              ),
              "</div>",
              "<div><strong>Total screen interval (m):</strong> ",
              data.table::fifelse(
                is.na(screen_top_depth_m) | is.na(screen_bottom_depth_m),
                unknown_label,
                as.character(round(
                  pmax(screen_bottom_depth_m - screen_top_depth_m, 0),
                  4
                ))
              ),
              "</div>"
            ),
            ""
          ),
          "<div><strong>Latitude:</strong> ",
          round(latitude, 4),
          "</div>",
          "<div><strong>Longitude:</strong> ",
          round(longitude, 4),
          "</div>",
          "<div><strong>",
          tr("documents", language$language),
          ":</strong> ",
          document_count,
          "</div>",
          data.table::fifelse(
            document_count > 0,
            paste0("<ul>", document_links, "</ul>"),
            ""
          )
        )
      ]
      tmp[, .(registry_id, popup_html)]
    })

    # Create the basic map ###########################################################
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(
        options = leaflet::leafletOptions(
          maxZoom = 17,
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
          "
        function(el, x) {
          var map = this;
          L.control.zoom({position:'bottomright'}).addTo(map);
  
          map.on('popupopen', function(e) {
            if (window.Shiny && e.popup && e.popup.getElement) {
              Shiny.bindAll(e.popup.getElement());
            }
          });
        }
      "
        )
    })

    # Filter the map data based on user's selection and add points to map ############################

    # build SVG icons
    svg_data_uri <- function(
      shape = c("circle", "square", "diamond"),
      fill = "#2C7FB8",
      size = 20,
      stroke = "#0c4e7aff",
      stroke_width = 1,
      hollow = FALSE
    ) {
      shape <- match.arg(shape)
      if (isTRUE(hollow)) {
        stroke <- fill
        fill <- "white"
        stroke_width <- max(stroke_width, 4)
      }
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
    build_symbol_legend <- function(legend_map, title, group_titles = NULL) {
      render_rows <- function(rows_data) {
        if (!nrow(rows_data)) {
          return("")
        }
        rows_data <- rows_data[order(type_label)]
        paste0(
          mapply(
            function(label, icon_url) {
              sprintf(
                "<div style='display:flex;align-items:center;margin:2px 0;'>
                 <img src='%s' style='width:14px;height:14px;margin-right:6px;'/>
                 <span>%s</span>
               </div>",
                icon_url,
                htmltools::htmlEscape(label)
              )
            },
            rows_data$type_label,
            rows_data$icon_url,
            USE.NAMES = FALSE
          ),
          collapse = ""
        )
      }

      legend_body <- if (is.null(group_titles)) {
        render_rows(legend_map)
      } else {
        sections <- vapply(
          names(group_titles),
          function(group_key) {
            group_value <- identical(group_key, "TRUE")
            group_rows <- legend_map[has_well == group_value]
            if (!nrow(group_rows)) {
              return("")
            }
            paste0(
              "<div style='font-weight:600;margin:6px 0 3px;'>",
              htmltools::htmlEscape(group_titles[[group_key]]),
              "</div>",
              render_rows(group_rows)
            )
          },
          character(1)
        )
        paste0(sections, collapse = "")
      }

      htmltools::HTML(sprintf(
        "<div style='background: rgba(255,255,255,0.9);
                 padding:8px 10px; border-radius:6px;
                 box-shadow: 0 1px 4px rgba(0,0,0,0.25);
                 font: 12px/1.2 sans-serif;'>
       <div style='font-weight:600; margin-bottom:6px;'>%s</div>
       %s
     </div>",
        htmltools::htmlEscape(title),
        legend_body
      ))
    }

    # Observe to filter and render location points on the map ############################
    observe({
      req(input$map_zoom, popupData(), language$language)
      popup_data <- popupData()

      wells_sub <- data.table::copy(moduleData$wells)
      purpose_column <- tr(
        "borehole_well_purpose_col",
        language$language
      )
      wells_sub[
        moduleData$purposes,
        on = .(well_purpose_id = borehole_well_purpose_id),
        well_purpose_name := get(paste0("i.", purpose_column))
      ]
      wells_sub[
        moduleData$purposes,
        on = .(borehole_purpose_id = borehole_well_purpose_id),
        borehole_purpose_name := get(paste0("i.", purpose_column))
      ]
      wells_sub[,
        `:=`(
          display_purpose_id = data.table::fifelse(
            has_well,
            well_purpose_id,
            borehole_purpose_id
          ),
          type_label = data.table::fifelse(
            has_well,
            well_purpose_name,
            borehole_purpose_name
          )
        )
      ]

      registry_scope <- input$borehole_well_scope %||% "with_wells"
      if (identical(registry_scope, "with_wells")) {
        wells_sub <- wells_sub[has_well == TRUE]
      } else if (identical(registry_scope, "without_wells")) {
        wells_sub <- wells_sub[has_well == FALSE]
      }

      if (!is.null(input$purpose)) {
        selected_purposes <- suppressWarnings(as.numeric(input$purpose))
        if (length(input$purpose) > 1) {
          wells_sub <- wells_sub[
            display_purpose_id %in% selected_purposes
          ]
        } else if (input$purpose != "all") {
          wells_sub <- wells_sub[
            display_purpose_id == selected_purposes
          ]
        }
      }

      if (!is.null(input$yrs)) {
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

      if (isFALSE(input$include_missing_depth)) {
        wells_sub <- wells_sub[!is.na(depth_m)]
      }
      if (isFALSE(input$include_missing_depth_to_water)) {
        wells_sub <- wells_sub[!is.na(static_water_level_m)]
      }

      # Filter based on well name search
      search_term <- trimws(input$well_name_search %||% "")
      if (nzchar(search_term)) {
        search_for_match <- if (isTRUE(input$well_name_case_sensitive)) {
          search_term
        } else {
          tolower(search_term)
        }

        matches_name <- function(values) {
          valid <- !is.na(values) & nzchar(trimws(values))
          values <- if (isTRUE(input$well_name_case_sensitive)) {
            values
          } else {
            tolower(values)
          }
          matched <- rep(FALSE, length(values))
          if (isTRUE(input$well_name_starts_with)) {
            matched[valid] <- startsWith(values[valid], search_for_match)
          } else if (isTRUE(input$well_name_ends_with)) {
            matched[valid] <- endsWith(values[valid], search_for_match)
          } else {
            matched[valid] <- grepl(
              search_for_match,
              values[valid],
              fixed = TRUE
            )
          }
          matched
        }
        wells_sub <- wells_sub[
          matches_name(well_name) | matches_name(borehole_name)
        ]
      }

      # if (isFALSE(input$include_missing_depth_to_bedrock)) {
      #   wells_sub <- wells_sub[!is.na(depth_to_bedrock_m)]
      # }

      wells_sub <- wells_sub[!is.na(latitude) & !is.na(longitude)]

      wells_sub <- wells_sub[
        popup_data,
        on = .(registry_id),
        popup_html := i.popup_html
      ]
      wells_sub[,
        marker_label := data.table::fcase(
          !is.na(well_name) & nzchar(trimws(well_name)),
          well_name,
          !is.na(borehole_name) & nzchar(trimws(borehole_name)),
          borehole_name,
          default = tr("unknown", language$language)
        )
      ]
      wells_sub[
        is.na(type_label) | trimws(type_label) == "",
        type_label := tr("unknown", language$language)
      ]

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

      # Assign each purpose a stable color and shape. Borehole-only records use
      # the same purpose symbol as wells, but render it hollow.
      purpose_map <- data.table::copy(moduleData$purposes)[
        order(borehole_well_purpose_id),
        .(display_purpose_id = borehole_well_purpose_id)
      ]
      purpose_map[,
        `:=`(
          color_hex = color_hex_choices[
            ((seq_len(.N) - 1) %% length(color_hex_choices)) + 1
          ],
          shape = shape_choices[
            ((seq_len(.N) - 1) %% length(shape_choices)) + 1
          ]
        )
      ]
      purpose_map <- data.table::rbindlist(
        list(
          purpose_map,
          data.table::data.table(
            display_purpose_id = NA_integer_,
            color_hex = "#5E5E5E",
            shape = "circle"
          )
        ),
        use.names = TRUE
      )

      wells_sub[
        purpose_map,
        on = .(display_purpose_id),
        `:=`(
          color_hex = i.color_hex,
          shape = i.shape
        )
      ]
      wells_sub[,
        `:=`(
          symbol_fill = data.table::fifelse(
            has_well,
            "filled",
            "hollow"
          ),
          purpose_key = data.table::fifelse(
            is.na(display_purpose_id),
            "unknown",
            as.character(display_purpose_id)
          )
        )
      ]
      wells_sub[,
        icon_url := vapply(
          seq_len(.N),
          function(idx) {
            svg_data_uri(
              shape = shape[idx],
              fill = color_hex[idx],
              size = 20,
              stroke = "#244C5A",
              stroke_width = 1,
              hollow = !has_well[idx]
            )
          },
          character(1)
        )
      ]

      legend_map <- unique(wells_sub[, .(
        has_well,
        display_purpose_id,
        type_label,
        color_hex,
        shape,
        icon_url
      )])
      legend_title <- switch(
        registry_scope,
        without_wells = tr("borehole_purpose", language$language),
        all = tr("well_or_borehole_purpose", language$language),
        tr("well_purpose", language$language)
      )
      legend_group_titles <- if (identical(registry_scope, "all")) {
        stats::setNames(
          c(
            tr("well_purpose", language$language),
            tr("borehole_purpose", language$language)
          ),
          c("TRUE", "FALSE")
        )
      } else {
        NULL
      }

      map_proxy <- leaflet::leafletProxy("map", session = session) %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::removeControl("well_purpose_legend")

      if (nrow(wells_sub) > 0) {
        # Create icons with custom class names, used for pie chart cluster icons
        slug <- function(x) gsub("[^A-Za-z0-9_-]", "_", x)
        icons <- leaflet::icons(
          iconUrl = wells_sub$icon_url,
          iconWidth = 15,
          iconHeight = 15,
          className = paste0(
            "loc-type-",
            slug(wells_sub$purpose_key),
            " loc-col-",
            gsub("#", "", wells_sub$color_hex),
            " loc-fill-",
            wells_sub$symbol_fill
          )
        )

        map_proxy <- map_proxy %>%
          leaflet::addMarkers(
            data = wells_sub,
            lng = ~longitude,
            lat = ~latitude,
            layerId = ~registry_id,
            label = ~htmltools::htmlEscape(marker_label),
            popup = ~popup_html,
            icon = icons,
            clusterOptions = if (isTRUE(input$cluster_points)) {
              leaflet::markerClusterOptions(
                iconCreateFunction = htmlwidgets::JS("pieClusterIcon"), # pieClusterIcon defined in tags$script above
                maxClusterRadius = 80, # cluster radius in pixels
                spiderfyOnMaxZoom = TRUE
                # chunkedLoading = TRUE,
                # chunkInterval = 75,
                # chunkDelay = 10
              )
            } else {
              NULL
            }
          ) %>%
          leaflet::addControl(
            build_symbol_legend(
              legend_map,
              title = legend_title,
              group_titles = legend_group_titles
            ),
            position = "bottomright",
            layerId = "well_purpose_legend",
            className = "custom-legend"
          )
      }
      map_proxy
    }) # End of observe for map filters and rendering location points

    # Create document download handlers ############################
    observeEvent(
      list(moduleData$documents, moduleData$boreholes_docs),
      {
        req(moduleData$documents, moduleData$boreholes_docs)
        registry_documents <- merge(
          moduleData$wells[, .(registry_id, borehole_id)],
          moduleData$boreholes_docs[, .(borehole_id, document_id)],
          by = "borehole_id",
          all = FALSE,
          sort = FALSE,
          allow.cartesian = TRUE
        )
        registry_documents <- unique(
          registry_documents[, .(registry_id, document_id)]
        )
        if (!nrow(registry_documents)) {
          return()
        }
        for (row_index in seq_len(nrow(registry_documents))) {
          local({
            registry_id_local <- registry_documents$registry_id[[row_index]]
            doc_id_local <- registry_documents$document_id[[row_index]]
            output[[paste0(
              "download_document_",
              registry_id_local,
              "_",
              doc_id_local
            )]] <-
              downloadHandler(
                filename = function() {
                  doc <- DBI::dbGetQuery(
                    session$userData$AquaCache,
                    "SELECT name, format
                       FROM files.documents
                      WHERE document_id = $1;",
                    params = list(doc_id_local)
                  )
                  if (nrow(doc) != 1) {
                    return("document")
                  }
                  name <- gsub(
                    "[^A-Za-z0-9_-]",
                    "_",
                    doc$name[[1]]
                  )
                  if (is.na(name) || !nzchar(name)) {
                    name <- paste0("document_", doc_id_local)
                  }
                  format <- sub("^\\.", "", doc$format[[1]])
                  if (is.na(format) || !nzchar(format)) {
                    return(name)
                  }
                  paste0(name, ".", format)
                },
                content = function(file) {
                  doc <- DBI::dbGetQuery(
                    session$userData$AquaCache,
                    "SELECT document
                       FROM files.documents
                      WHERE document_id = $1;",
                    params = list(doc_id_local)
                  )
                  if (nrow(doc) != 1) {
                    return(NULL)
                  }
                  writeBin(doc$document[[1]], file)
                }
              )
          })
        }
      },
      ignoreInit = FALSE
    )
  }) # End of moduleServer
}
