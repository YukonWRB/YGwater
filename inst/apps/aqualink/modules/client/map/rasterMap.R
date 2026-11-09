mapRasterUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fluid(
    shiny::uiOutput(ns("banner")),
    shiny::uiOutput(ns("sidebar_page")) # <-- add ns() here
  )
}

mapRaster <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    getRasterRefID <- function(ref_table, datetime) {
      ref_id <- ref_table$reference_id[which.min(abs(
        as.numeric(ref_table$datetime - as.POSIXct(datetime, tz = "UTC"))
      ))]
      return(ref_id)
    }

    getRasterLayer <- function(con, reference_id, parameter) {
      r <- getRaster(
        con = con,
        tbl_name = c("spatial", "rasters"),
        clauses = sprintf("WHERE reference_id = %d", reference_id),
        bands = 1
      )
      terra::crs(r) <- "EPSG:4326"
      # SWE rasters are stored in metres; the map convention is millimetres.
      if (identical(parameter, "snow water equivalent")) {
        r <- r * 1000
      }
      return(r)
    }

    getPillowSWE <- function(con, stations_sf, date) {
      stations_sf$swe <- NA_real_

      ts_query <- sprintf(
        "SELECT DISTINCT ON (timeseries_id) timeseries_id, date, value 
           FROM continuous.measurements_calculated_daily
           WHERE timeseries_id IN (%s) AND value IS NOT NULL AND date = '%s'
           ORDER BY timeseries_id, date DESC",
        paste(stations_sf$timeseries_id, collapse = ","),
        as.Date(date)
      )

      ts_data <- DBI::dbGetQuery(con, ts_query)
      # Match ts_data with stations_sf based on timeseries_id
      for (i in seq_len(nrow(stations_sf))) {
        match_idx <- which(
          ts_data$timeseries_id == stations_sf$timeseries_id[i]
        )
        if (length(match_idx) > 0) {
          stations_sf$swe[i] <- ts_data$value[match_idx[1]]
        }
      }
      return(stations_sf)
    }

    getSurveySWE <- function(con, stations_sf, date) {
      stations_sf$swe <- NA_real_
      ts_query <- sprintf(
        "SELECT s.target_datetime, r.result as value, s.location_id
           FROM discrete.samples s
           JOIN discrete.results r ON s.sample_id = r.sample_id
           WHERE s.location_id IN (%s)
           AND r.parameter_id = (SELECT parameter_id FROM public.parameters
                       WHERE param_name = 'snow water equivalent')
           AND r.result IS NOT NULL
           AND DATE(s.target_datetime) = '%s'
           ORDER BY s.location_id, s.target_datetime DESC",
        paste(stations_sf$location_id, collapse = ","),
        as.Date(date)
      )

      ts_data <- DBI::dbGetQuery(con, ts_query)
      # Match ts_data with stations_sf based on location_id
      for (i in seq_len(nrow(stations_sf))) {
        match_idx <- which(
          ts_data$location_id == stations_sf$location_id[i]
        )
        if (length(match_idx) > 0) {
          stations_sf$swe[i] <- ts_data$value[match_idx[1]]
        }
      }
      return(stations_sf)
    }

    communities <- YGwater:::download_spatial_layer(
      con = session$userData$AquaCache,
      layer_name = "Communities"
    )

    swe_pillow_stations <- download_continuous_ts_locations(
      con = session$userData$AquaCache,
      param_name = "snow water equivalent"
    )

    swe_survey_stations <- download_discrete_ts_locations(
      con = session$userData$AquaCache,
      param_name = "snow water equivalent"
    )

    raster_reference_query <- "
    SELECT 
        r.reference_id,
        rsi.model,
        rsi.parameter,
        rr.units,
        rr.valid_from as datetime
    FROM spatial.raster_series_index rsi
    JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
    JOIN spatial.rasters r ON r.reference_id = rr.reference_id
    WHERE rsi.parameter IS NOT NULL
    ORDER BY rr.valid_from, r.reference_id"

    raster_reference_list <- DBI::dbGetQuery(
      session$userData$AquaCache,
      raster_reference_query
    )

    raster_reference_list$datetime <- as.POSIXct(raster_reference_list$datetime)
    variables <- sort(unique(raster_reference_list$parameter))
    initial_variable <- if ("snow water equivalent" %in% variables) {
      "snow water equivalent"
    } else {
      variables[1]
    }
    models <- sort(unique(raster_reference_list$model[
      raster_reference_list$parameter == initial_variable
    ]))
    initial_model <- models[1]
    initial_model_references <- raster_reference_list[
      raster_reference_list$parameter == initial_variable &
        raster_reference_list$model == initial_model,
      ,
      drop = FALSE
    ]

    colour_palettes <- list(
      "Snow bulletin" = c("#EBB966", "#EEE383", "#C1FB80", "#6CFC88", "#8CEFE1", "#85B4F8", "#6772F7"),
      Viridis = c("#440154", "#443983", "#31688E", "#21918C", "#35B779", "#90D743", "#FDE725"),
      Blues = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C"),
      "Yellow-green-blue" = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8"),
      Plasma = c("#0D0887", "#5B02A3", "#9A179B", "#CB4679", "#ED7953", "#FDB32E", "#F0F921")
    )

    selected_raster_references <- shiny::reactive({
      shiny::req(input$raster_variable, input$raster_type)
      raster_reference_list[
        raster_reference_list$parameter == input$raster_variable &
          raster_reference_list$model == input$raster_type,
        ,
        drop = FALSE
      ]
    })

    updating_raster_selector <- shiny::reactiveVal(FALSE)

    update_raster_date <- function(refs) {
      shiny::req(nrow(refs) > 0)
      shiny::freezeReactiveValue(input, "date")
      shiny::updateDateInput(
        session,
        "date",
        value = as.Date(max(refs$datetime)),
        min = as.Date(min(refs$datetime)),
        max = as.Date(max(refs$datetime))
      )
    }

    shiny::observeEvent(input$raster_variable, {
      available_models <- sort(unique(raster_reference_list$model[
        raster_reference_list$parameter == input$raster_variable
      ]))
      shiny::req(length(available_models) > 0)
      selected_model <- available_models[1]
      refs <- raster_reference_list[
        raster_reference_list$parameter == input$raster_variable &
          raster_reference_list$model == selected_model,
        ,
        drop = FALSE
      ]
      updating_raster_selector(TRUE)
      shiny::freezeReactiveValue(input, "raster_type")
      shiny::updateSelectInput(session, "raster_type",
        choices = available_models, selected = selected_model
      )
      update_raster_date(refs)
    }, ignoreInit = TRUE)

    # Each model can have a different period of record. Keep the date control
    # within the selected model's range and default to its newest raster.
    shiny::observeEvent(input$raster_type, {
      if (isTRUE(updating_raster_selector())) {
        updating_raster_selector(FALSE)
        return(invisible())
      }
      refs <- selected_raster_references()
      update_raster_date(refs)
    }, ignoreInit = TRUE)

    output$banner <- shiny::renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "mapRaster"
      )
    })

    output$sidebar_page <- shiny::renderUI({
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput(
            ns("raster_variable"),
            "Variable",
            choices = variables,
            selected = initial_variable
          ),
          shiny::selectInput(
            ns("raster_type"),
            tr("maps_raster", language$language),
            choices = models,
            selected = models[1]
          ),
          shiny::dateInput(
            ns("date"), # already namespaced
            "Date",
            value = as.Date(max(initial_model_references$datetime)),
            min = as.Date(min(initial_model_references$datetime)),
            max = as.Date(max(initial_model_references$datetime)),
            language = language$abbrev
          ),
          shiny::selectInput(
            ns("shapefile"), # already namespaced
            tr("gen_snowBul_basins", language$language),
            choices = c(
              "swe_basins" = "swe_basins"
            ),
            selected = "swe_basins"
          ),
          shiny::tags$details(
            shiny::tags$summary("Advanced display options"),
            shiny::selectInput(
              ns("raster_colour_palette"),
              "Colour palette",
              choices = names(colour_palettes),
              selected = "Snow bulletin"
            ),
            shiny::checkboxInput(
              ns("reverse_raster_colours"),
              "Reverse colour palette",
              value = FALSE
            )
          ),
          shiny::tags$details(
            shiny::tags$summary(tr("snowbull_details", language$language)),
            shiny::uiOutput(ns("map_details"))
          )
        ),
        leaflet::leafletOutput(ns("swe_map"), height = 800) # already namespaced
      )
    })

    basins_shp <- shiny::reactive({
      shiny::req(length(input$shapefile) == 1L, nzchar(input$shapefile))
      if (identical(input$shapefile, "swe_basins")) {
        # input IDs are already namespaced in modules
        shp <- sf::st_read(
          system.file(
            "snow_survey/swe_basins/swe_basins.shp",
            package = "YGwater",
            mustWork = TRUE
          ),
          quiet = TRUE
        )
        sf::st_transform(shp, crs = 4326)
      } else {
        NULL
      }
    })

    # Permanent snow mask. It is calculated per model because the reference
    # rasters may have distinct extents, resolutions, or periods of record.
    permenent_snow_date <- as.Date("2025-09-01")
    swe_mask <- shiny::reactive({
      refs <- selected_raster_references()
      shiny::req(nrow(refs) > 0)
      ref_id_perm <- getRasterRefID(refs, permenent_snow_date)
      r_db_perm <- getRasterLayer(
        session$userData$AquaCache,
        ref_id_perm,
        "snow water equivalent"
      )
      !is.na(r_db_perm) & (r_db_perm <= 10)
    })

    # Raster references in a series are not guaranteed to share a grid.  The
    # permanent-snow mask is stored on the September 2025 grid, so align it to
    # each selected raster before calling `mask()`.
    align_swe_mask <- function(mask_raster, target_raster) {
      same_grid <- terra::compareGeom(
        mask_raster,
        target_raster,
        crs = TRUE,
        ext = TRUE,
        rowcol = TRUE,
        res = TRUE,
        stopOnError = FALSE
      )

      if (isTRUE(same_grid)) {
        return(mask_raster)
      }

      # A logical mask is categorical, so nearest-neighbour resampling avoids
      # inventing partial mask values along cell boundaries.
      terra::resample(mask_raster, target_raster, method = "near")
    }

    swe_map_data <- shiny::bindEvent(shiny::reactive({
      shiny::req(input$date, input$raster_variable, input$raster_type)
      selected_refs <- selected_raster_references()
      shiny::req(nrow(selected_refs) > 0)
      is_swe <- identical(input$raster_variable, "snow water equivalent")

      swe_pillows <- if (is_swe) {
        getPillowSWE(session$userData$AquaCache, swe_pillow_stations, input$date)
      } else {
        empty_pillows <- swe_pillow_stations[0, , drop = FALSE]
        empty_pillows$swe <- numeric(0)
        empty_pillows
      }
      swe_surveys <- if (is_swe) {
        getSurveySWE(session$userData$AquaCache, swe_survey_stations, input$date)
      } else {
        empty_surveys <- swe_survey_stations[0, , drop = FALSE]
        empty_surveys$swe <- numeric(0)
        empty_surveys
      }

      # Query raster for selected date
      ref_id <- getRasterRefID(selected_refs, input$date)
      r_db <- getRasterLayer(session$userData$AquaCache, ref_id, input$raster_variable)
      if (is_swe) {
        r_db <- terra::mask(
          r_db,
          align_swe_mask(swe_mask(), r_db),
          maskvalue = FALSE
        )
      }
      # Basins
      basins <- basins_shp()
      if (is.null(basins)) {
        return(NULL)
      }
      basin_means <- terra::extract(r_db, basins, fun = mean, na.rm = TRUE)
      basins$mean_raster <- basin_means[, 2]

      values_r <- terra::values(r_db, mat = FALSE)
      finite_values <- values_r[is.finite(values_r)]
      shiny::req(length(finite_values) > 0)

      # SWE uses the established bulletin scale. Other variables use their
      # displayed data range, so e.g. temperature and precipitation are not
      # compressed into a 0--250 SWE legend.
      if (is_swe) {
        colorbar_min <- 0
        colorbar_max <- 250
      } else {
        colorbar_min <- unname(stats::quantile(finite_values, 0.02, na.rm = TRUE))
        colorbar_max <- unname(stats::quantile(finite_values, 0.98, na.rm = TRUE))
        if (!is.finite(colorbar_min) || !is.finite(colorbar_max) ||
            identical(colorbar_min, colorbar_max)) {
          colorbar_min <- min(finite_values)
          colorbar_max <- max(finite_values)
        }
        if (identical(colorbar_min, colorbar_max)) {
          colorbar_min <- colorbar_min - 0.5
          colorbar_max <- colorbar_max + 0.5
        }
      }

      # Contours
      contour_levels <- pretty(c(colorbar_min, colorbar_max), n = 10)
      contours <- terra::as.contour(r_db, levels = contour_levels)
      contours_sf <- sf::st_as_sf(contours)

      raster_units <- unique(stats::na.omit(selected_refs$units))
      raster_units <- if (is_swe) "mm" else if (length(raster_units)) raster_units[1] else ""

      palette_name <- input$raster_colour_palette
      if (length(palette_name) != 1L || is.na(palette_name) ||
          !palette_name %in% names(colour_palettes)) {
        palette_name <- "Snow bulletin"
      }
      palette_values <- colour_palettes[[palette_name]]
      if (isTRUE(input$reverse_raster_colours)) {
        palette_values <- rev(palette_values)
      }

      pal <- leaflet::colorNumeric(
        palette = palette_values,
        domain = c(colorbar_min, colorbar_max),
        na.color = "transparent"
      )
      # Saturate values outside the displayed scale instead of converting
      # them to NA (the default colorNumeric behaviour).
      raster_pal <- function(x) {
        pal(pmin(pmax(x, colorbar_min), colorbar_max))
      }
      contours_sf$col <- raster_pal(contours_sf$level)

      list(
        r_db = r_db,
        basins = basins,
        contours_sf = contours_sf,
        pal = pal,
        raster_pal = raster_pal,
        swe_pillows = swe_pillows,
        swe_surveys = swe_surveys,
        colorbar_min = colorbar_min,
        colorbar_max = colorbar_max,
        parameter = input$raster_variable,
        units = raster_units
      )
    }),
    input$date,
    input$raster_colour_palette,
    input$reverse_raster_colours,
    ignoreInit = FALSE
    )

    output$swe_map <- leaflet::renderLeaflet({
      map_data <- swe_map_data()
      shiny::req(map_data)
      r_db <- map_data$r_db
      basins_shp <- map_data$basins
      contours_sf <- map_data$contours_sf
      pillows <- map_data$swe_pillows
      surveys <- map_data$swe_surveys
      pal <- map_data$pal
      raster_pal <- map_data$raster_pal
      colorbar_min <- map_data$colorbar_min
      colorbar_max <- map_data$colorbar_max
      parameter <- map_data$parameter
      raster_units <- map_data$units

      map <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addRasterImage(
          r_db,
          colors = raster_pal,
          opacity = 0.7,
          project = TRUE,
          group = tr("snowbull_raster", language$language)
        ) %>%
        leaflet::addPolylines(
          data = contours_sf,
          color = ~col,
          weight = 3,
          opacity = 0.85,
          group = tr("isocontours", language$language)
        ) %>%
        leaflet::addPolygons(
          data = basins_shp,
          fillColor = "transparent",
          color = "black",
          weight = 2,
          fillOpacity = 0.6,
          group = tr("gen_snowBul_basins", language$language)
        ) %>%
        leaflet::addPolygons(
          data = basins_shp,
          fillColor = ~ raster_pal(mean_raster),
          color = "black",
          weight = 2,
          fillOpacity = 0.6,
          label = ~ paste0(
            "Mean Value: ",
            ifelse(
              mean_raster > colorbar_max,
              paste0(colorbar_max, "+"),
              round(mean_raster, 2)
            )
          ),
          group = tr("snowbull_swe_basin", language$language)
        ) %>%
        leaflet::addLabelOnlyMarkers(
          data = basins_shp,
          lng = sf::st_coordinates(sf::st_centroid(basins_shp))[, 1],
          lat = sf::st_coordinates(sf::st_centroid(basins_shp))[, 2],
          label = ~ ifelse(
            mean_raster > colorbar_max,
            paste0(colorbar_max, "+"),
            round(mean_raster, 1)
          ),
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "font-weight" = "bold",
              "color" = "black",
              "font-size" = "12px"
            )
          ),
          group = tr("snowbull_swe_basin", language$language)
        ) %>%
        leaflet::addCircleMarkers(
          data = communities,
          lng = ~ sf::st_coordinates(geometry)[, 1],
          lat = ~ sf::st_coordinates(geometry)[, 2],
          radius = 2,
          color = "black",
          fillColor = "black",
          fillOpacity = 1,
          opacity = 1,
          label = ~feature_name,
          group = tr("communities", language$language)
        ) %>%
        leaflet::addLabelOnlyMarkers(
          data = communities,
          lng = ~ sf::st_coordinates(geometry)[, 1],
          lat = ~ sf::st_coordinates(geometry)[, 2],
          label = ~feature_name,
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            style = list(
              "font-weight" = "bold",
              "color" = "black",
              "font-size" = "10px"
            )
          ),
          group = tr("communities", language$language)
        )

      # These stations only measure SWE. Do not add their layers for another
      # raster variable: Leaflet evaluates formula fields even with zero rows.
      if (identical(parameter, "snow water equivalent")) {
        map <- map %>%
          leaflet::addCircleMarkers(
            data = pillows[!is.na(pillows$swe), ],
            lng = ~longitude,
            lat = ~latitude,
            fillColor = ~raster_pal(swe),
            color = "black", weight = 2, fillOpacity = 1, opacity = 1, radius = 8,
            label = ~paste0(name, ": ", round(swe, 1), " mm"),
            group = tr("snowbull_snow_pillow", language$language)
          ) %>%
          leaflet::addCircleMarkers(
            data = surveys[!is.na(surveys$swe), ],
            lng = ~longitude,
            lat = ~latitude,
            fillColor = ~raster_pal(swe),
            color = "blue", weight = 2, fillOpacity = 1, opacity = 1, radius = 6,
            label = ~paste0(name, ": ", round(swe, 1), " mm"),
            group = tr("snow_survey", language$language)
          )
      }

      map <- map %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = c(colorbar_min, colorbar_max),
          title = paste0(parameter, if (nzchar(raster_units)) paste0(" (", raster_units, ")")),
          opacity = 1,
          labFormat = function(type, cuts, p) {
            cuts <- round(cuts)
            labels <- as.character(cuts)
            labels[length(labels)] <- paste0(colorbar_max, "+")
            labels
          }
        ) %>%
        leaflet::addLayersControl(
          overlayGroups = c(
            tr("snowbull_raster", language$language),
            tr("snowbull_snow_pillow", language$language),
            tr("snow_survey", language$language),
            tr("gen_snowBul_basins", language$language),
            tr("snowbull_swe_basin", language$language),
            tr("communities", language$language),
            tr("isocontours", language$language)
          ),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::hideGroup(tr("snowbull_swe_basin", language$language)) %>%
        leaflet::hideGroup(tr("isocontours", language$language))
      map
    })

    # --- Render selected date text with details ---
    output$map_details <- shiny::renderUI({
      shiny::HTML(paste0(
        "<b>",
        tr("app_version", language$language),
        "</b> ",
        as.character(utils::packageVersion("YGwater")),
        "<br/>",
        "<b>",
        "Dislaimer:</b> ",
        tr("snowbull_raster_disclaimer", language$language),
        "<br/>"
      ))
    })
  })
}
