load_all()

con <- YGwater::AquaConnect(
    name = "aquacache",
    host = Sys.getenv("aquacacheHostProd"),
    port = Sys.getenv("aquacachePortProd"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"),
)

# load_all()
snowBulletin(
    year = 2026,
    month = 5,
    save_path = "C:\\Users\\esniede\\Documents\\github\\YGwater\\dev\\swe",
    con = con,
    language = "english",
)

# load_all()
dat <- load_bulletin_timeseries(
    con = con,
    load_swe = FALSE,
    load_temp = FALSE,
    load_precip = FALSE,
    load_streamflow = TRUE,
    start_year_historical = 1991,
    end_year_historical = 2020,
    october_start = TRUE,
    epsg = 3579
)

make_snowbull_map(
    year = 2026,
    month = 4,
    statistic = "relative_to_med",
    language = "English",
    format = "ggplot",
    param_name = "streamflow",
    snowbull_timeseries = dat,
    con = con,
    filename = "map.png"
)

# ggplotOverlap(
#     location = "",
#     continuous_data = timeseries,
#     parameter = "swe",
#     snowbulletin = TRUE,
#     start_year_historical = 2010,
#     end_year_historical = 2020
# )

df <- DBI::dbGetQuery(
    con,
    sprintf(
        "
        SELECT ts.timeseries_id, l.name AS location_name
        FROM timeseries ts
        INNER JOIN locations l ON ts.location_id = l.location_id
        INNER JOIN parameters p ON ts.parameter_id = p.parameter_id
        WHERE l.name IN ('Old Crow ECCC met', 'Teslin ECCC Met', 'Mayo ECCC Met', 'Watson Lake ECCC Met', 'Dawson City ECCC Met')
            AND ts.record_rate = '%s'
            AND p.param_name = '%s'
        ",
        "1 day",
        "precipitation, total"
    )
)

bulletin_month <- 5
bulletin_year <- 2026
bulletin_scale <- 1
language <- list(language = "English", abbrev = "en")
start_year_historical <- 1991
end_year_historical <- 2020


month <- bulletin_month
month_param <- bulletin_month
year_param <- bulletin_year
year <- bulletin_year
scale <- 1
scale_param <- 1
param_name <- "snow water equivalent"

epsg <- "EPSG:3005"


plot(dat$swe$surveys$timeseries$data$datetime, )


length(unique(year(dat$swe$surveys$timeseries$data$datetime[
    !is.na(dat$swe$surveys$timeseries$data[, '45'])
])))

plot(dat$fdd$timeseries$data[, 'datetime'], dat$fdd$timeseries$data[, '489'])


View(p)
plot(1:10, rnorm(10), main = "Example Plot")


get_state_as_shp(
    con = con,
    year = 2025,
    month = 3,
    parameter_name = "swe",
    statistic = "relative_to_med"
)

swe$relative_to_norm

timeseries <- dat$swe$basins$timeseries$data

timeseries_key_to_name <- function(timeseries, metadata) {
    station_ids <- names(timeseries)
    # Exclude datetime column if present
    station_ids <- setdiff(station_ids, "datetime")
    # Create a named vector for renaming
    rename_map <- stats::setNames(metadata$location_id, metadata$key)
    # Only rename columns that exist in both timeseries and metadata
    common_ids <- intersect(station_ids, names(rename_map))
    names(timeseries)[match(common_ids, names(timeseries))] <- rename_map[
        common_ids
    ]
    return(timeseries)
}


timeseries <- timeseries_key_to_name(
    timeseries = dat$fdd$timeseries$data,
    metadata = dat$fdd$metadata
)


ok <- get_normalized_bulletin_values(
    bulletin_month = 3,
    bulletin_year = 2025,
    ts = dat$swe$basins$timeseries$data,
    parameter = "swe",
    norms = dat$swe$basins$norms
)


swe_stats <- as.data.frame(t(do.call(rbind, ok)))
swe_stats$name <- rownames(swe_stats)
rownames(swe_stats) <- NULL
swe_stats <- swe_stats[, c("name", setdiff(names(swe_stats), "name"))]


df$description <- description

# on.exit(DBI::dbDisconnect(con), add = TRUE)

query <- "SELECT locations.name AS location_name, locations.location AS location_id, datetime, value_corrected AS value FROM measurements_continuous_corrected INNER JOIN timeseries ON measurements_continuous_corrected.timeseries_id = timeseries.timeseries_id INNER JOIN locations ON timeseries.location = locations.location\n                                           WHERE measurements_continuous_corrected.timeseries_id IN ('663', '665', '666', '668', '664', '671', '667')"


DBI::dbGetQuery(con, query)


# download_spatial_layer

ret <- make_snowbull_map(
    con = con,
    year = 2025,
    month = 4,
    format = "ggplot",
    parameter_name = "temperature",
    statistic = "anomalies",
    language = "English",
    filename = "dev\\swe\\exports\\swe_bulletin_apr2025.png"
)


library(sf)
library(ggplot2)

# Download Yukon boundary from a GeoJSON URL (no rnaturalearth)
yukon_url <- "https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/yukon.geojson"
yukon_boundary <- sf::st_read(yukon_url, quiet = TRUE)

# Download Canada boundary from a GeoJSON URL
canada_url <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries/CAN.geo.json"
canada <- sf::st_read(canada_url, quiet = TRUE)

# Plot Yukon boundary with terrain basemap
library(ggspatial)
library(ggmap)

# Get terrain basemap from Stamen Maps

ggmap::register_stadiamaps("774f7648-cc81-4dc5-a1f6-1d18e74e2f10")

terrain_map <- get_stadiamap(
    bbox = c(left = -142, bottom = 59, right = -123, top = 69),
    zoom = 6,
    maptype = "stamen_terrain_background",
    color = 'bw',
    force = TRUE
)


ggmap(terrain_map)
+geom_sf(data = yukon_boundary, fill = NA, color = "red", inherit.aes = FALSE) +
    theme_minimal() +
    labs(title = "Yukon Boundary Map with Terrain Basemap")
library(data.table)

flow_stations <- c(
    "Nordenskiold River Below Rowlinson Creek",
    "Pelly River At Pelly Crossing",
    "Stewart River At The Mouth",
    "White River At Km 1881.6 Alaska Highway",
    "Yukon River Above White River",
    "Klondike River Above Bonanza Creek",
    "Porcupine River Near International Boundary",
    "Peel River Above Canyon Creek",
    "Liard River At Upper Crossing",
    "Alsek River Above Bates River"
)

level_stations <- c(
    "Marsh Lake Near Whitehorse",
    "Lake Laberge Near Whitehorse",
    "Teslin Lake At Teslin"
)

station_dt <- data.table(
    parameter = c(
        rep("water flow", length(flow_stations)),
        rep("water level", length(level_stations))
    ),
    name = c(flow_stations, level_stations),
    longitude = NA_real_,
    latitude = NA_real_,
    percentile = NA_real_,
    date = as.Date(NA),
    value = NA_real_,
    historical_median = NA_real_
)
# Query to get daily corrected measurements for a single station and parameter
station_name <- "Marsh Lake Near Whitehorse" # Change as needed
param_name <- "water level"


# Get today's date

# Query all historical values for the station/parameter on the same day-of-year as today

# Use EXTRACT(DOY FROM m.date) for PostgreSQL, or adapt as needed for your DBMS
get_today_percentile <- function(
    param_name,
    station_name,
    con,
    start_year = 1991,
    end_year = 2020,
    query_date = Sys.Date()
) {
    today_doy <- lubridate::yday(query_date)

    query_hist <- sprintf(
        "
        SELECT
            m.value,
            EXTRACT(DOY FROM m.date) AS doy,
            m.date
        FROM measurements_calculated_daily_corrected m
            INNER JOIN timeseries ts ON m.timeseries_id = ts.timeseries_id
            INNER JOIN locations l ON ts.location_id = l.location_id
            INNER JOIN parameters p ON ts.parameter_id = p.parameter_id
        WHERE l.name = '%s'
          AND p.param_name = '%s'
          AND m.date >= '%d-01-01'
          AND m.date <= '%d-12-31'
          AND m.value IS NOT NULL
          AND EXTRACT(DOY FROM m.date) = %d
          AND NOT (EXTRACT(MONTH FROM m.date) = 2 AND EXTRACT(DAY FROM m.date) = 29)
        ",
        station_name,
        param_name,
        start_year,
        end_year,
        today_doy
    )
    hist_df <- DBI::dbGetQuery(con, query_hist)
    hist_values <- hist_df$value
    # Get the most recent available value up to and including query_date
    query_today <- sprintf(
        "
        SELECT m.value, l.longitude, l.latitude, l.location_code, m.date
        FROM measurements_calculated_daily_corrected m
            INNER JOIN timeseries ts ON m.timeseries_id = ts.timeseries_id
            INNER JOIN locations l ON ts.location_id = l.location_id
            INNER JOIN parameters p ON ts.parameter_id = p.parameter_id
        WHERE l.name = '%s'
          AND p.param_name = '%s'
          AND m.value IS NOT NULL
          AND m.date <= '%s'
        ORDER BY m.date DESC
        LIMIT 1
        ",
        station_name,
        param_name,
        query_date
    )
    today_res <- DBI::dbGetQuery(con, query_today)

    percentile <- NA
    if (length(today_res$value) == 1 && length(hist_values) > 0) {
        percentile <- ecdf(hist_values)(today_res$value) * 100
    }
    list(
        station_name = station_name,
        param_name = param_name,
        today_value = today_res$value,
        percentile = percentile,
        hist_values = hist_values,
        date = today_res$date,
        longitude = today_res$longitude,
        latitude = today_res$latitude,
        location_code = today_res$location_code
    )
}

for (i in seq_len(nrow(station_dt))) {
    res <- get_today_percentile(
        param_name = station_dt$parameter[i],
        station_name = station_dt$name[i],
        con = con,
        query_date = "2026-04-01"
    )
    station_dt$date[i] <- res$date
    station_dt$percentile[i] <- res$percentile
    station_dt$value[i] <- if (
        !is.null(res$today_value) && length(res$today_value) > 0
    ) {
        res$today_value
    } else {
        NA
    }
    station_dt$longitude[i] <- if (!is.null(res$longitude)) {
        res$longitude
    } else {
        NA
    }
    station_dt$latitude[i] <- if (!is.null(res$latitude)) res$latitude else NA
    station_dt$historical_median[i] <- if (
        !is.null(res$hist_values) && length(res$hist_values) > 0
    ) {
        median(res$hist_values, na.rm = TRUE)
    } else {
        NA
    }
}

library(ggplot2)
library(sf)

# Remove rows with missing coordinates
station_dt_plot <- station_dt[!is.na(longitude) & !is.na(latitude), ]

# Convert to sf object (EPSG:4326)
station_sf <- st_as_sf(
    station_dt_plot,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
)

stations_sf_transformed <- st_transform(station_sf, crs = 3579)
stations_sf_transformed$x <- st_coordinates(stations_sf_transformed)[, 1]
stations_sf_transformed$y <- st_coordinates(stations_sf_transformed)[, 2]
# Plot with symbol based on parameter

shps <- load_bulletin_shapefiles(con = con, epsg = 3579)
shp

library(scales)

# Create a custom color ramp from blue to yellow to red
cmap <- colorRampPalette(c(
    "#EBB966", # Orange (near normal)
    "#EEE383", # Yellow (normal)
    "#C1FB80", # Light green (above normal)
    "#6CFC88", # Green (well above normal)
    "#8CEFE1", # Cyan (high)
    "#85B4F8", # Light blue (very high)
    "#6772F7" # Blue (extremely high)
))

# Calculate color for each station based on percentile using the custom cmap
station_dt_plot$color <- cmap(100)[
    as.numeric(cut(
        station_dt_plot$percentile,
        breaks = seq(0, 100, length.out = 101),
        include.lowest = TRUE
    ))
]


p <- ggplot2::ggplot() +
    ggplot2::theme_void()


if (!is.null(shps$waterbodies)) {
    p <- p +
        ggplot2::geom_sf(
            data = shps$waterbodies,
            fill = "lightblue",
            color = "lightblue",
            alpha = 0.5
        )
}


if (!is.null(shps$yukon)) {
    p <- p +
        ggplot2::geom_sf(
            data = shps$yukon,
            fill = NA,
            color = "black",
            alpha = 0.5
        )
}

if (!is.null(shps$basins)) {
    p <- p +
        ggplot2::geom_sf(
            data = shps$basins,
            fill = NA,
            color = "black",
            alpha = 0.5
        )
}


# Add roads (below stations)
if (!is.null(shps$roads)) {
    p <- p +
        ggplot2::geom_sf(
            data = shps$roads,
            color = '#411a1a',
            linewidth = 1
        )
}


stations_sf_transformed$fill_colour <- station_dt_plot$color

# Plot water level and water flow separately, each with a different shape
p <- p +
    ggplot2::geom_point(
        data = stations_sf_transformed[
            stations_sf_transformed$parameter == "water flow",
        ],
        ggplot2::aes(
            x = .data$x,
            y = .data$y
        ),
        fill = stations_sf_transformed$fill_colour[
            stations_sf_transformed$parameter == "water flow"
        ],
        color = "black",
        shape = 21, # circle for water flow
        size = 6,
        stroke = 1
    ) +
    ggplot2::geom_point(
        data = stations_sf_transformed[
            stations_sf_transformed$parameter == "water level",
        ],
        ggplot2::aes(
            x = .data$x,
            y = .data$y
        ),
        fill = stations_sf_transformed$fill_colour[
            stations_sf_transformed$parameter == "water level"
        ],
        color = "black",
        shape = 22, # square for water level
        size = 6,
        stroke = 1
    )
# ) +
# labs(
#     title = "Stations by Parameter (colored by Percentile, shape by Parameter)"
# )

# labels <- stations_sf_transformed$percentile
# labels <- paste0("(", round(stations_sf_transformed$percentile), ")")
# stations_sf_transformed$labels <- labels

# p <- p +
#     shadowtext::geom_shadowtext(
#         data = stations_sf_transformed,
#         ggplot2::aes(
#             x = .data$x,
#             y = .data$y,
#             label = .data$labels
#         ),
#         size = 3.5,
#         color = "black",
#         bg.color = "white",
#         bg.r = 0.15,
#         vjust = 0.5,
#         hjust = 1,
#         nudge_x = -13500,
#         nudge_y = -6000
#     )

if (!is.null(shps$communities)) {
    comm_coords <- sf::st_coordinates(shps$communities)
    communities_df <- data.frame(
        x = comm_coords[, 1],
        y = comm_coords[, 2],
        name = shps$communities$feature_name,
        annotation = gsub(
            "<br>",
            "\n",
            shps$communities$annotation
        ),
        x_adjust = shps$communities$x_adjusted,
        y_adjust = shps$communities$y_adjusted
    )

    p <- p +
        ggplot2::geom_point(
            data = communities_df,
            ggplot2::aes(x = .data$x, y = .data$y),
            fill = "black",
            size = static_style_elements$communities$iconWidth / 6,
            shape = 18
        ) +
        shadowtext::geom_shadowtext(
            data = communities_df,
            ggplot2::aes(
                x = .data$x_adjust,
                y = .data$y_adjust,
                label = .data$annotation
            ),
            size = static_style_elements$communities$label$textShadowSize,
            fontface = "bold.italic",
            color = static_style_elements$communities$label$color,
            bg.color = "white",
            bg.r = 0.1,
            vjust = -0.5,
            hjust = 0.5,
            family = "serif"
        )
}


# Add colorbar legend for percentiles

# Create a data frame for colorbar legend
colorbar_df <- data.frame(
    percentile = seq(0, 100, length.out = 100),
    color = cmap(100),
    stringsAsFactors = FALSE
)

# Create bins and labels for the legend (e.g., 0-10, 10-20, ..., 90-100)
breaks <- seq(0, 100, by = 10)
labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
color_bins <- cmap(length(breaks) - 1)

legend_df <- data.frame(
    color = color_bins,
    label = labels,
    stringsAsFactors = FALSE
)
# Reverse order for legend
legend_df$label <- factor(legend_df$label, levels = rev(labels))
legend_df$color <- rev(legend_df$color)


subtitle <- paste0(
    "Flow and water level\n",
    "percentiles as of April 1, 2026\n",
    "Reference years: 1991-2020"
)
# Add a dummy geom for the colorbar legend
p <- p +
    # ggplot2::labs(
    #     title = title,
    #     subtitle = subtitle
    # ) +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            title = subtitle,
            override.aes = list(fill = legend_df$color),
            label.position = "right"
        )
    ) +
    ggplot2::theme(
        legend.title = ggplot2::element_text(size = 10, face = "bold"),
        legend.text = ggplot2::element_text(size = 9),
        legend.position = c(0.9, 0.9), # top right inside plot
        legend.justification = c("right", "top")
    )


# Save plot bounds before adding dummy legend geom

# Add a dummy invisible geom for legend

p <- p +
    ggplot2::geom_point(
        data = legend_df,
        ggplot2::aes(x = 99, y = 99, fill = .data$label),
        shape = 21,
        size = 5,
        show.legend = TRUE
    ) +
    ggplot2::scale_fill_manual(
        name = subtitle,
        values = stats::setNames(legend_df$color, levels(legend_df$label)),
        drop = FALSE,
        guide = ggplot2::guide_legend(
            override.aes = list(shape = 21, size = 5)
        )
    )

ggplot2::ggsave(
    "stations_percentile_map.jpg",
    plot = p,
    width = 10,
    height = 12,
    dpi = 300
)


loc_name <- "Watson"
param_name <- "precipitation, total"
query <- "
SELECT
l.location_id,
l.location_code,
l.name AS location_name,
ts.timeseries_id,
ts.record_rate,
p.param_name
FROM locations l
INNER JOIN timeseries ts
ON ts.location_id = l.location_id
INNER JOIN parameters p
ON p.parameter_id = ts.parameter_id
WHERE l.name ILIKE $1
AND p.param_name ILIKE $2
ORDER BY l.name, ts.timeseries_id;
"

res <- DBI::dbGetQuery(
    con,
    query,
    params = list(
        paste0("%", loc_name, "%"),
        paste0("%", param_name, "%")
    )
)

View(res)
