library(shiny)
library(leaflet)
library(raster)
library(YGwater)
library(terra)
library(sf)

con <- AquaCache::AquaConnect(
    name = "aquacache",
    host = Sys.getenv("aquacacheHostProd"),
    port = Sys.getenv("aquacachePortProd"),
    user = Sys.getenv("aquacacheUserProd"),
    password = Sys.getenv("aquacachePassProd")
)

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

communities <- download_spatial_layer(
    con = con,
    layer_name = "Communities"
)

community_points <- if (!is.null(communities) && nrow(communities) > 0) {
    community_coords <- sf::st_coordinates(communities)
    data.frame(
        sf::st_drop_geometry(communities),
        lng = community_coords[, 1],
        lat = community_coords[, 2],
        stringsAsFactors = FALSE
    )
} else {
    data.frame(
        feature_name = character(),
        lng = numeric(),
        lat = numeric(),
        stringsAsFactors = FALSE
    )
}

era5_query <- "
    SELECT 
        r.reference_id,
        rr.valid_from as datetime
    FROM spatial.raster_series_index rsi
    JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
    JOIN spatial.rasters r ON r.reference_id = rr.reference_id
    WHERE rsi.model = 'ERA5_land'
    ORDER BY rr.valid_from, r.reference_id"
era5_raster_data <- DBI::dbGetQuery(con, era5_query)
era5_raster_data$datetime <- as.POSIXct(era5_raster_data$datetime)

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
    crs(r) <- "+init=epsg:4326"
    r <- r * 1000 # Convert from meters to mm
    return(r)
}

load_demo_swe_basins <- function() {
    shp <- sf::st_read(
        system.file(
            "snow_survey/swe_basins/swe_basins.shp",
            package = "YGwater",
            mustWork = TRUE
        ),
        quiet = TRUE
    )
    sf::st_transform(shp, crs = 4326)
}

available_shapefiles <- list(
    swe_basins = load_demo_swe_basins()
)

ui <- bslib::page_sidebar(
    sidebar = bslib::sidebar(
        dateInput(
            "date",
            "Date",
            value = as.Date("2026-01-01"),
            min = min(era5_raster_data$datetime),
            max = max(era5_raster_data$datetime)
        ),
        selectInput(
            "shapefile",
            "Shapefile",
            choices = c(
                "swe_basins" = "swe_basins"
            ),
            selected = "swe_basins"
        )
    ),
    leafletOutput("swe_map", height = 800)
)

server <- function(input, output, session) {
    basins_shp <- reactive({
        req(input$shapefile)
        available_shapefiles[[input$shapefile]]
    })

    # Permanent snow mask
    permenent_snow_date <- as.Date("2024-09-01")
    ref_id_perm <- getRasterRefID(era5_raster_data, permenent_snow_date)
    r_db_perm <- getRasterSWE(con, ref_id_perm)
    swe_mask <- !is.na(r_db_perm) & (r_db_perm <= 10)

    swe_map_data <- reactive({
        req(input$date)

        # Query raster for selected date
        ref_id <- getRasterRefID(era5_raster_data, input$date)
        r_db <- getRasterSWE(con, ref_id)
        r_db <- terra::mask(r_db, swe_mask, maskvalue = FALSE)

        # Basins
        basins <- basins_shp()
        if (is.null(basins)) {
            return(NULL)
        }
        basins <- basins
        basin_means <- terra::extract(r_db, basins, fun = mean, na.rm = TRUE)
        basins$mean_raster <- basin_means[, 2]
        basin_palette <- leaflet::colorNumeric(
            palette = "viridis",
            domain = basins$mean_raster,
            na.color = "transparent"
        )
        basins$fill_color <- basin_palette(basins$mean_raster)
        basin_centroids <- suppressWarnings(sf::st_centroid(basins))
        basin_centroid_coords <- sf::st_coordinates(basin_centroids)
        basin_labels <- data.frame(
            sf::st_drop_geometry(basins),
            lng = basin_centroid_coords[, 1],
            lat = basin_centroid_coords[, 2],
            stringsAsFactors = FALSE
        )

        # Contours
        contour_levels <- seq(0, 1000, by = 50)
        contours <- terra::as.contour(r_db, levels = contour_levels)
        contours_sf <- sf::st_as_sf(contours)

        values_r <- terra::values(r_db, mat = FALSE)
        values_r <- values_r[is.finite(values_r)]
        pal <- colorNumeric(
            domain = values_r,
            palette = "viridis",
            na.color = "transparent"
        )
        contours_sf$col <- pal(contours_sf$level)

        list(
            r_db = r_db,
            basins = basins,
            basin_labels = basin_labels,
            contours_sf = contours_sf,
            pal = pal
        )
    }) %>%
        shiny::bindEvent(input$date, input$shapefile, ignoreInit = FALSE)

    output$swe_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(
                data = community_points,
                lng = ~lng,
                lat = ~lat,
                radius = 2,
                color = "black",
                fillColor = "black",
                fillOpacity = 1,
                opacity = 1,
                label = ~feature_name,
                group = "Communities"
            ) %>%
            addLabelOnlyMarkers(
                data = community_points,
                lng = ~lng,
                lat = ~lat,
                label = ~feature_name,
                labelOptions = labelOptions(
                    noHide = TRUE,
                    direction = "top",
                    textOnly = TRUE,
                    style = list(
                        "font-weight" = "bold",
                        "color" = "black",
                        "font-size" = "10px"
                    )
                ),
                group = "Communities"
            ) %>%
            addLayersControl(
                overlayGroups = c(
                    "ERA5 Raster",
                    "Basin averages",
                    "Communities",
                    "Isocontours"
                ),
                options = layersControlOptions(collapsed = FALSE)
            )
    })

    observe({
        map_data <- swe_map_data()
        req(map_data)

        leafletProxy("swe_map", session = session) %>%
            clearGroup("ERA5 Raster") %>%
            clearGroup("Isocontours") %>%
            clearGroup("Basin averages") %>%
            addRasterImage(
                map_data$r_db,
                colors = map_data$pal,
                opacity = 0.7,
                project = TRUE,
                group = "ERA5 Raster"
            ) %>%
            addPolylines(
                data = map_data$contours_sf,
                color = ~col,
                weight = 3,
                opacity = 0.85,
                group = "Isocontours"
            ) %>%
            addPolygons(
                data = map_data$basins,
                fillColor = ~fill_color,
                color = "black",
                weight = 2,
                fillOpacity = 0.6,
                label = ~ paste0("Mean Value: ", round(mean_raster, 2)),
                group = "Basin averages"
            ) %>%
            addLabelOnlyMarkers(
                data = map_data$basin_labels,
                lng = ~lng,
                lat = ~lat,
                label = ~ round(mean_raster, 1),
                labelOptions = labelOptions(
                    noHide = TRUE,
                    direction = "center",
                    textOnly = TRUE,
                    style = list(
                        "font-weight" = "bold",
                        "color" = "black",
                        "font-size" = "12px"
                    )
                ),
                group = "Basin averages"
            )
    })
}

shinyApp(ui, server)
