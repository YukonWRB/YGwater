# library(shiny)
# library(leaflet)
# library(raster)
# library(YGwater)
# library(terra)
# library(sf)

# con <- AquaCache::AquaConnect(
#   name = "aquacache",
#   host = Sys.getenv("aquacacheHostProd"),
#   port = Sys.getenv("aquacachePortProd"),
#   user = Sys.getenv("aquacacheUserProd"),
#   password = Sys.getenv("aquacachePassProd")
# )

download_spatial_layer <- function(
  con,
  layer_name,
  additional_query = NULL
) {
  query <- sprintf(
    "SELECT *, ST_AsText(ST_Transform(geom, 4326)) as geom_4326 
         FROM spatial.vectors 
         WHERE layer_name = %s",
    DBI::dbQuoteString(con, layer_name)
  )

  if (!is.null(additional_query) && nzchar(additional_query)) {
    query <- paste(query, additional_query)
  }

  data <- DBI::dbGetQuery(con, query)
  if (nrow(data) == 0) {
    warning(sprintf("No data found for layer: %s", layer_name))
    return(NULL)
  }

  geom <- sf::st_as_sfc(data$geom_4326, crs = 4326)
  sf::st_sf(data, geometry = geom, crs = 4326)
}

getRasterRefID <- function(ref_table, datetime) {
  ref_id <- ref_table$reference_id[which.min(abs(
    as.numeric(ref_table$datetime - as.POSIXct(datetime, tz = "UTC"))
  ))]
  return(ref_id)
}

getRasterSWE <- function(con, reference_id) {
  r <- getRaster(
    con = con,
    tbl_name = c("spatial", "rasters"),
    clauses = sprintf("WHERE reference_id = %d", reference_id),
    bands = 1
  )
  terra::crs(r) <- "EPSG:4326"
  r <- r * 1000 # Convert from meters to mm
  return(r)
}

getPillowSWE <- function(con, stations_sf, date) {
  stations_sf$swe <- NA_real_

  ts_query <- sprintf(
    "SELECT DISTINCT ON (timeseries_id) timeseries_id, date, value 
       FROM continuous.measurements_calculated_daily_corrected
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


# UI using bslib::page_fluid and dynamic sidebar via uiOutput

mapRasterUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fluid(
    uiOutput(ns("banner")),
    shiny::uiOutput(ns("sidebar_page")) # <-- add ns() here
  )
}

mapRaster <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns # ensure ns is available

    communities <- download_spatial_layer(
      con = session$userData$AquaCache,
      layer_name = "Communities"
    )

    swe_pillow_stations <- download_continuous_ts_locations(
      con = session$userData$AquaCache,
      param_name_long = "snow water equivalent"
    )

    swe_survey_stations <- download_discrete_ts_locations(
      con = session$userData$AquaCache,
      param_name_long = "snow water equivalent"
    )

    era5_query <- "
    SELECT 
        r.reference_id,
        rr.valid_from as datetime
    FROM spatial.raster_series_index rsi
    JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
    JOIN spatial.rasters r ON r.reference_id = rr.reference_id
    WHERE rsi.model = 'ERA5_land'
    ORDER BY rr.valid_from, r.reference_id"
    era5_raster_data <- DBI::dbGetQuery(session$userData$AquaCache, era5_query)
    era5_raster_data$datetime <- as.POSIXct(era5_raster_data$datetime)

    output$banner <- renderUI({
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
          shiny::dateInput(
            ns("date"), # already namespaced
            "Date",
            value = max(era5_raster_data$datetime),
            min = min(era5_raster_data$datetime),
            max = max(era5_raster_data$datetime),
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
            tags$summary(tr("snowbull_details", language$language)),
            shiny::uiOutput(ns("map_details"))
          )
        ),
        leaflet::leafletOutput(ns("swe_map"), height = 800) # already namespaced
      )
    })

    basins_shp <- shiny::reactive({
      if (input$shapefile == "swe_basins") {
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

    # Permanent snow mask
    permenent_snow_date <- as.Date("2025-09-01")
    ref_id_perm <- getRasterRefID(era5_raster_data, permenent_snow_date)
    r_db_perm <- getRasterSWE(session$userData$AquaCache, ref_id_perm)
    swe_mask <- !is.na(r_db_perm) & (r_db_perm <= 10)

    swe_map_data <- shiny::reactive({
      shiny::req(input$date)

      swe_pillows <- getPillowSWE(
        con = session$userData$AquaCache,
        stations_sf = swe_pillow_stations,
        date = input$date
      )

      swe_surveys <- getSurveySWE(
        con = session$userData$AquaCache,
        stations_sf = swe_survey_stations,
        date = input$date
      )

      # Query raster for selected date
      ref_id <- getRasterRefID(era5_raster_data, input$date)
      r_db <- getRasterSWE(session$userData$AquaCache, ref_id)
      r_db <- terra::mask(r_db, swe_mask, maskvalue = FALSE)
      # Basins
      basins <- basins_shp()
      if (is.null(basins)) {
        return(NULL)
      }
      basin_means <- terra::extract(r_db, basins, fun = mean, na.rm = TRUE)
      basins$mean_raster <- basin_means[, 2]

      # Contours
      contour_levels <- seq(0, 1000, by = 50)
      contours <- terra::as.contour(r_db, levels = contour_levels)
      contours_sf <- sf::st_as_sf(contours)

      # Set colorbar limits
      colorbar_min <- 0
      colorbar_max <- 250

      # Mask raster values above 250 for colorbar/legend
      values_r <- terra::values(r_db)
      values_r[values_r > colorbar_max] <- colorbar_max

      pal <- leaflet::colorNumeric(
        palette = rev(c(
          "#6772F7", # Blue (extremely high)
          "#85B4F8", # Light blue (very high)
          "#8CEFE1", # Cyan (high)
          "#6CFC88", # Green (well above normal)
          "#C1FB80", # Light green (above normal)
          "#EEE383", # Yellow (normal)
          "#EBB966" # Orange (near normal)
        )),
        domain = c(colorbar_min, colorbar_max),
        na.color = "transparent"
      )
      contours_sf$col <- pal(pmin(contours_sf$level, colorbar_max))

      list(
        r_db = r_db,
        basins = basins,
        contours_sf = contours_sf,
        pal = pal,
        swe_pillows = swe_pillows,
        swe_surveys = swe_surveys,
        colorbar_min = colorbar_min,
        colorbar_max = colorbar_max
      )
    })

    output$swe_map <- leaflet::renderLeaflet({
      map_data <- swe_map_data()
      shiny::req(map_data)
      r_db <- map_data$r_db
      basins_shp <- map_data$basins
      contours_sf <- map_data$contours_sf
      pillows <- map_data$swe_pillows
      surveys <- map_data$swe_surveys
      pal <- map_data$pal
      colorbar_min <- map_data$colorbar_min
      colorbar_max <- map_data$colorbar_max

      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addRasterImage(
          r_db,
          colors = pal,
          opacity = 0.7,
          project = TRUE,
          group = tr("snowbull_raster", language$language)
        ) %>%
        leaflet::addPolylines(
          data = contours_sf,
          color = ~col,
          weight = 3,
          opacity = 0.85,
          group = tr("snowbull_isocontours", language$language)
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
          fillColor = ~ pal(pmin(mean_raster, colorbar_max)),
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
          group = tr("snowbull_communities", language$language)
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
          group = tr("snowbull_communities", language$language)
        ) %>%
        leaflet::addCircleMarkers(
          data = pillows[!is.na(pillows$swe), ],
          lng = ~longitude,
          lat = ~latitude,
          fillColor = ~ pal(pmin(swe, colorbar_max)),
          color = "black",
          weight = 2,
          fillOpacity = 1,
          opacity = 1,
          radius = 8,
          label = ~ paste0(
            name,
            ": ",
            ifelse(
              swe > colorbar_max,
              paste0(colorbar_max, "+"),
              round(swe, 1)
            ),
            " mm"
          ),
          group = tr("snowbull_snow_pillow", language$language)
        ) %>%
        leaflet::addCircleMarkers(
          data = surveys[!is.na(surveys$swe), ],
          lng = ~longitude,
          lat = ~latitude,
          fillColor = ~ pal(pmin(swe, colorbar_max)),
          color = "blue",
          weight = 2,
          fillOpacity = 1,
          opacity = 1,
          radius = 6,
          label = ~ paste0(
            name,
            ": ",
            ifelse(
              swe > colorbar_max,
              paste0(colorbar_max, "+"),
              round(swe, 1)
            ),
            " mm"
          ),
          group = tr("snowbull_snow_survey", language$language)
        ) %>%
        # --- Add colorbar legend for raster ---
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = c(colorbar_min, colorbar_max),
          title = "SWE (mm)",
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
            tr("snowbull_snow_survey", language$language),
            tr("gen_snowBul_basins", language$language),
            tr("snowbull_swe_basin", language$language),
            tr("snowbull_communities", language$language),
            tr("snowbull_isocontours", language$language)
          ),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::hideGroup(tr("snowbull_swe_basin", language$language)) %>%
        leaflet::hideGroup(tr("snowbull_isocontours", language$language))
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
