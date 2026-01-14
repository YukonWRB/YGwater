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
# UI using bslib::page_fluid and dynamic sidebar via uiOutput
ui <- bslib::page_fluid(
    uiOutput("sidebar_page")
)

server <- function(input, output, session) {
    # Place this in your server function to generate the sidebar layout dynamically
    output$sidebar_page <- renderUI({
        bslib::page_sidebar(
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
    })

    basins_shp <- reactive({
        if (input$shapefile == "swe_basins") {
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
        basin_means <- terra::extract(r_db, basins, fun = mean, na.rm = TRUE)
        basins$mean_raster <- basin_means[, 2]

        # Contours
        contour_levels <- seq(0, 1000, by = 50)
        contours <- terra::as.contour(r_db, levels = contour_levels)
        contours_sf <- sf::st_as_sf(contours)

        values_r <- values(r_db)
        pal <- colorNumeric(
            domain = values_r,
            palette = "viridis",
            na.color = "transparent"
        )
        contours_sf$col <- pal(contours_sf$level)

        list(
            r_db = r_db,
            basins = basins,
            contours_sf = contours_sf,
            pal = pal
        )
    })

    output$swe_map <- renderLeaflet({
        map_data <- swe_map_data()
        req(map_data)
        r_db <- map_data$r_db
        basins_shp <- map_data$basins
        contours_sf <- map_data$contours_sf
        pal <- map_data$pal

        leaflet() %>%
            addTiles() %>%
            addRasterImage(
                r_db,
                colors = pal,
                opacity = 0.7,
                project = TRUE,
                group = "ERA5 Raster"
            ) %>%
            addPolylines(
                data = contours_sf,
                color = ~col,
                weight = 3,
                opacity = 0.85,
                group = "Isocontours"
            ) %>%
            addPolygons(
                data = basins_shp,
                fillColor = ~ colorNumeric(
                    "viridis",
                    domain = basins_shp$mean_raster
                )(mean_raster),
                color = "black",
                weight = 2,
                fillOpacity = 0.6,
                label = ~ paste0("Mean Value: ", round(mean_raster, 2)),
                group = "Basin averages"
            ) %>%
            addLabelOnlyMarkers(
                data = basins_shp,
                lng = sf::st_coordinates(sf::st_centroid(basins_shp))[, 1],
                lat = sf::st_coordinates(sf::st_centroid(basins_shp))[, 2],
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
            ) %>%
            addCircleMarkers(
                data = communities,
                lng = ~ sf::st_coordinates(geometry)[, 1],
                lat = ~ sf::st_coordinates(geometry)[, 2],
                radius = 2,
                color = "black",
                fillColor = "black",
                fillOpacity = 1,
                opacity = 1,
                label = ~feature_name,
                group = "Communities"
            ) %>%
            addLabelOnlyMarkers(
                data = communities,
                lng = ~ sf::st_coordinates(geometry)[, 1],
                lat = ~ sf::st_coordinates(geometry)[, 2],
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
}

shinyApp(ui, server)
