# =============================================================================
# Snow Water Equivalent (SWE) Basin Visualization
# =============================================================================
# This script creates an interactive leaflet map showing:
# - SWE basins colored by elevation
# - Continuous SWE monitoring stations colored by mean values
# - Communities, roads, and geographic boundaries
#
# Data Sources:
# - SWE basins: Local shapefile
# - Stations/Communities/Roads: PostgreSQL spatial database (AquaCache)
# - Timeseries data: Continuous measurements database
# =============================================================================

# Load required libraries
library(shiny)
library(AquaCache)
library(YGwater)
library(DBI)
library(plotly)
library(leaflet)
library(sf)
library(base64enc)

# =============================================================================
# DATABASE CONFIGURATION
# =============================================================================

# Configure production database connection
config <- list(
    dbName = "aquacache",
    dbHost = Sys.getenv("aquacacheHostProd"),
    dbPort = Sys.getenv("aquacachePortProd"),
    dbUser = Sys.getenv("aquacacheUserProd"),
    dbPass = Sys.getenv("aquacachePassProd")
)

# Establish production database connection for spatial data
con2 <- AquaConnect(
    name = config$dbName,
    host = config$dbHost,
    port = config$dbPort,
    user = config$dbUser,
    pass = config$dbPass
)

# Configure development database connection for timeseries data
config_dev <- list(
    dbName = "aquacache",
    dbHost = Sys.getenv("aquacacheHost"),
    dbPort = Sys.getenv("aquacachePort"),
    dbUser = Sys.getenv("aquacacheAdminUser"),
    dbPass = Sys.getenv("aquacacheAdminPass")
)

# Establish development database connection for continuous measurements
con <- AquaConnect(
    name = config_dev$dbName,
    host = config_dev$dbHost,
    port = config_dev$dbPort,
    user = config_dev$dbUser,
    pass = config_dev$dbPass
)


# Query continuous timeseries for the first location in locations_with_swe
record_cutoff_years <- 5
year <- 2025
doy <- 60

start_date <- sprintf("%d-01-01", 1950)
end_date <- sprintf("%d-12-31", 3000)

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Load and process snowcourse factors for discrete SWE stations
#'
#' Reads the snowcourse factors CSV, removes duplicates, and returns a weight matrix
#' for locations present in the provided discrete metadata.
#'
#' @param metadata_discrete sf object with discrete station metadata (must have 'location' column)
#' @param csv_path Path to snowcourse_factors.csv
#' @return data.frame weight matrix for matching locations
load_snowcourse_factors <- function(
    metadata_discrete,
    csv_path = "data-raw/snowcourse_factors.csv"
) {
    snowcourse_factors <- read.csv(
        csv_path,
        stringsAsFactors = FALSE
    )
    # Remove duplicate Hyland River value
    snowcourse_factors <- subset(snowcourse_factors, location_id != "10AD-SC01")
    # Drop location_name column
    snowcourse_factors <- snowcourse_factors[,
        !colnames(snowcourse_factors) %in% "location_name"
    ]
    # Get snowcourse factors for locations in metadata_discrete$location

    return(snowcourse_factors)
}


#' Get the most recent non-NaN value from a timeseries data.frame
#'
#' @param ts data.frame with a 'datetime' column and a 'value' column
#' @return The most recent non-NaN numeric value, or NA if none found
get_most_recent_date <- function(ts) {
    if (
        is.null(ts) ||
            nrow(ts) == 0 ||
            !"datetime" %in% names(ts) ||
            !"value" %in% names(ts)
    ) {
        return(NA_real_)
    }
    # Ensure datetime is POSIXct
    ts$datetime <- as.POSIXct(ts$datetime)
    # Filter out NaN and NA values
    valid_idx <- which(!is.na(ts$value) & !is.nan(ts$value))
    if (length(valid_idx) == 0) {
        return(NA_real_)
    }
    # Find the most recent datetime among valid values
    latest_time <- max(ts$datetime[valid_idx], na.rm = TRUE)
    return(latest_time)
}

# Example usage:

#' Load spatial data from database with coordinate transformation
#' @param con Database connection
#' @param layer_name Name of the spatial layer
#' @param description_filter Optional filter for description field
#' @return sf object in WGS84 (EPSG:4326)
#'
#'

load_spatial_layer <- function(con, layer_name, description_filter = NULL) {
    query <- sprintf(
        "SELECT *, ST_AsText(ST_Transform(geom, 4326)) as geom_4326 
         FROM spatial.vectors 
         WHERE layer_name = '%s'",
        layer_name
    )

    # Add description filter if provided
    if (!is.null(description_filter)) {
        filter_str <- paste0("'", description_filter, "'", collapse = ", ")
        query <- paste(query, sprintf("AND description IN (%s)", filter_str))
    }

    # Execute query and create sf object
    data <- DBI::dbGetQuery(con, query)
    if (nrow(data) == 0) {
        warning(sprintf("No data found for layer: %s", layer_name))
        return(NULL)
    }

    geom <- sf::st_as_sfc(data$geom_4326, crs = 4326)
    sf::st_sf(data, geometry = geom, crs = 4326)
}

#' Retrieve continuous SWE timeseries and station metadata
#'
#' This function fetches metadata for continuous Snow Water Equivalent (SWE)
#' timeseries and the associated measurement values. It returns a list with:
#' - timeseries: a named list where each element is a numeric vector of values for a timeseries_id
#' - metadata: an sf object (WGS84) containing station metadata and a mean_value column
#'
#' Note: start_date and end_date filter timeseries metadata based on the timeseries
#' start_date / end_date fields (if present in the continuous.timeseries table).
#'
#' @param con DBI database connection
#' @param start_date Optional character date (YYYY-MM-DD) to filter timeseries by t.start_date >= start_date
#' @param end_date Optional character date (YYYY-MM-DD) to filter timeseries by t.end_date <= end_date
#' @return A list with components:
#'   - timeseries: named list of numeric vectors (names are timeseries_id)
#'   - metadata: sf object of station metadata with geometry and mean_value column
#' @examples
#' \dontrun{
#' res <- get_swe_timeseries(con)
#' res$timeseries[["123"]]
#' plot(res$metadata)
#' }
#'

get_continuous_timeseries <- function(
    con,
    start_date = sprintf("%d-01-01", 1950),
    end_date = sprintf("%d-01-01", 3000),
    resolution = "daily"
) {
    # Build metadata query for continuous SWE timeseries
    md_query <- paste0(
        "SELECT 
            t.timeseries_id,
            t.location_id,
            l.location,
            l.name,
            l.latitude,
            l.longitude
         FROM continuous.timeseries t
         JOIN public.locations l ON t.location_id = l.location_id
         WHERE t.parameter_id = (SELECT parameter_id FROM public.parameters 
                                 WHERE param_name = 'snow water equivalent')"
    )

    md_continuous_df <- DBI::dbGetQuery(con, md_query)

    if (nrow(md_continuous_df) == 0) {
        warning("No continuous SWE stations found")
        return(list(timeseries = list(), metadata = NULL))
    }

    # Convert metadata to sf (WGS84)
    md_continuous <- sf::st_as_sf(
        md_continuous_df,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )

    ts_ids <- unique(md_continuous$timeseries_id)
    # Initialize list to store timeseries for each station
    ts_list <- vector("list", length(ts_ids))
    names(ts_list) <- as.character(ts_ids)
    station_values <- numeric(length(ts_ids))
    names(station_values) <- as.character(ts_ids)

    cat("Processing", length(ts_ids), "timeseries...\n")

    for (i in seq_along(ts_ids)) {
        ts_id <- ts_ids[i]

        # Retrieve all non-NULL measurement values for this timeseries
        # (no date filtering on measurements to avoid assuming timestamp column name)
        ts_query <- sprintf(
            "SELECT datetime, value FROM continuous.measurements_continuous 
             WHERE timeseries_id = %d AND value IS NOT NULL",
            as.integer(ts_id)
        )

        # Optionally filter metadata by timeseries start/end dates if provided
        if (!is.null(start_date)) {
            ts_query <- paste0(
                ts_query,
                " AND datetime >= ",
                DBI::dbQuoteString(con, start_date)
            )
        }
        if (!is.null(end_date)) {
            ts_query <- paste0(
                ts_query,
                " AND datetime <= ",
                DBI::dbQuoteString(con, end_date)
            )
        }

        ts_data <- DBI::dbGetQuery(con, ts_query)
        ts_data$datetime <- as.POSIXct(ts_data$datetime, tz = "UTC")
        ts_data$datetime <- ts_data$datetime[order(ts_data$datetime)]

        # Resample to daily if requested
        if (resolution == "daily" && nrow(ts_data) > 0) {
            ts_data$day <- as.Date(ts_data$datetime)
            ts_data <- aggregate(
                value ~ day,
                data = ts_data,
                FUN = mean,
                na.rm = TRUE
            )
            names(ts_data) <- c("datetime", "value")
            ts_data$datetime <- as.POSIXct(ts_data$datetime)
        } else if (resolution == "monthly" && nrow(ts_data) > 0) {
            ts_data$month <- format(ts_data$datetime, "%Y-%m")
            ts_data <- aggregate(
                value ~ month,
                data = ts_data,
                FUN = mean,
                na.rm = TRUE
            )
            names(ts_data) <- c("datetime", "value")
            ts_data$datetime <- as.POSIXct(
                paste0(ts_data$datetime, "-01"),
                tz = "UTC"
            )
        } else if (nrow(ts_data) == 0) {
            warning(
                sprintf("No data for timeseries_id: %s", ts_id)
            )
        } else {
            warning(
                sprintf(
                    "Resolution not 'daily'/'monthly' for timeseries_id: %s",
                    ts_id
                )
            )
        }

        ts_list[[as.character(ts_id)]] <- ts_data

        if (nrow(ts_data) > 0) {
            # Get the value nearest to end_date (if end_date is provided)
            if (!is.null(end_date) && "datetime" %in% names(ts_data)) {
                # Convert end_date and timestamps to POSIXct
                target_time <- as.POSIXct(end_date, tz = "UTC")
                # Find the row with timestamp closest to end_date
                idx <- which.min(abs(difftime(
                    ts_data$datetime,
                    target_time,
                    units = "secs"
                )))
                station_values[i] <- ts_data$value[idx]
            } else {
                # Fallback: use mean if no timestamp or end_date
                station_values[i] <- mean(ts_data$value, na.rm = TRUE)
            }
        } else {
            station_values[i] <- NA_real_
        }
    }

    station_latest_dates <- vapply(
        names(ts_list),
        function(id) {
            ts <- ts_list[[id]]
            get_most_recent_date(ts)
        },
        FUN.VALUE = as.Date(NA)
    )

    station_means_df <- data.frame(
        timeseries_id = as.integer(names(station_latest_dates)),
        latest_date = as.POSIXct(station_latest_dates),
        stringsAsFactors = FALSE
    )

    md_no_geom <- sf::st_drop_geometry(md_continuous)
    merged_df <- merge(
        md_no_geom,
        station_means_df,
        by = "timeseries_id",
        all.x = TRUE,
        sort = FALSE
    )

    # Reattach geometry in the order of merged_df
    geom_match <- match(merged_df$timeseries_id, md_continuous$timeseries_id)
    geom <- sf::st_geometry(md_continuous)[geom_match]

    metadata_sf <- sf::st_sf(
        merged_df,
        geometry = geom,
        crs = sf::st_crs(md_continuous)
    )

    return(list(timeseries = ts_list, metadata = metadata_sf))
}

#' Retrieve discrete SWE timeseries and station metadata
#'
#' This function fetches metadata for discrete Snow Water Equivalent (SWE)
#' timeseries and the associated measurement values. It returns a list with:
#' - timeseries: a named list where each element is a data.frame of values for a timeseries_id
#' - metadata: an sf object (WGS84) containing station metadata and a mean_value column
#'
#' @param con DBI database connection
#' @param start_date Optional character date (YYYY-MM-DD) to filter measurements by datetime >= start_date
#' @param end_date Optional character date (YYYY-MM-DD) to filter measurements by datetime <= end_date
#' @return A list with components:
#'   - timeseries: named list of data.frames (names are timeseries_id)
#'   - metadata: sf object of station metadata with geometry and mean_value column
get_discrete_timeseries <- function(
    con,
    start_date = sprintf("%d-01-01", 1950),
    end_date = sprintf("%d-01-01", 2025)
) {
    # Build metadata query for discrete SWE timeseries
    md_discrete <- DBI::dbGetQuery(
        con,
        "SELECT DISTINCT
            s.location_id,
            l.latitude,
            l.longitude,
            l.location,
            l.name
        FROM discrete.results r 
        JOIN discrete.samples s ON r.sample_id = s.sample_id 
        JOIN public.locations l ON s.location_id = l.location_id
        WHERE r.parameter_id = (SELECT parameter_id FROM public.parameters 
                               WHERE param_name = 'snow water equivalent')"
    )

    if (nrow(md_discrete) == 0) {
        warning("No discrete SWE stations found")
        return(list(timeseries = list(), metadata = NULL))
    }

    # Convert metadata to sf (WGS84)
    md_discrete <- sf::st_as_sf(
        md_discrete,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )

    ts_ids <- unique(md_discrete$location_id)
    ts_list <- vector("list", length(ts_ids))
    names(ts_list) <- as.character(ts_ids)
    station_values <- numeric(length(ts_ids))
    names(station_values) <- as.character(ts_ids)

    for (loc_id in ts_ids) {
        ts <- DBI::dbGetQuery(
            con,
            paste0(
                "SELECT s.datetime, s.target_datetime, r.result ",
                " FROM discrete.samples s ",
                "JOIN discrete.results r ON s.sample_id = r.sample_id ",
                "WHERE s.location_id = ",
                loc_id,
                " AND r.parameter_id = (SELECT parameter_id FROM public.parameters ",
                "WHERE param_name = 'snow water equivalent')"
            )
        )
        # if target data is NULL, use datetime
        ts$target_datetime[is.na(ts$target_datetime)] <- ts$datetime[is.na(
            ts$target_datetime
        )]

        # overwrite datetime with target_datetime as posixct, then remove target_datetime column
        ts$datetime <- as.POSIXct(ts$target_datetime)
        ts$target_datetime <- NULL
        ts <- ts[order(ts$datetime), ]

        # Rename 'result' column to 'value' if it exists
        if ("result" %in% names(ts)) {
            names(ts)[names(ts) == "result"] <- "value"
        }
        ts_list[[as.character(loc_id)]] <- ts
    }

    # Compute most recent non-NaN value for each discrete timeseries and
    # attach to metadata
    station_latest_dates <- vapply(
        names(ts_list),
        function(id) {
            ts <- ts_list[[id]]
            get_most_recent_date(ts)
        },
        FUN.VALUE = numeric(1)
    )

    station_means_df <- data.frame(
        location_id = as.integer(names(station_latest_dates)),
        latest_date = as.POSIXct(station_latest_dates),
        stringsAsFactors = FALSE
    )

    md_no_geom <- sf::st_drop_geometry(md_discrete)
    merged_df <- merge(
        md_no_geom,
        station_means_df,
        by = "location_id",
        all.x = TRUE,
        sort = FALSE
    )

    # Reattach geometry in original order
    geom_match <- match(merged_df$location_id, md_discrete$location_id)
    geom <- sf::st_geometry(md_discrete)[geom_match]

    md_discrete <- sf::st_sf(
        merged_df,
        geometry = geom,
        crs = sf::st_crs(md_discrete)
    )

    return(list(timeseries = ts_list, metadata = md_discrete))
}
# =============================================================================

# =============================================================================
# LOAD AND PROCESS SPATIAL DATA
# =============================================================================

# Load timeseries data
data_continuous <- get_continuous_timeseries(
    con,
    start_date = start_date,
    end_date = end_date
)

data_discrete <- get_discrete_timeseries(
    con,
    start_date = start_date,
    end_date = end_date
)

calculate_historic_daily_median <- function(ts, lookback_years = 100) {
    # For each row in ts, calculate the median for the same day-of-year using only data from years < year(ts$datetime)
    ts$doy <- as.integer(strftime(ts$datetime, format = "%j"))
    ts$year <- as.integer(strftime(ts$datetime, format = "%Y"))

    # Snap day-of-year to nearest key date if within 2 days
    key_dates <- c(32, 60, 91, 121)
    for (i in seq_len(nrow(ts))) {
        diffs <- abs(ts$doy[i] - key_dates)
        if (any(diffs <= 2)) {
            ts$doy[i] <- key_dates[which.min(diffs)]
        }
    }
    # Precompute for speed: for each year, for each day-of-year, the median of all previous years
    historic_median <- numeric(nrow(ts))
    relative_change <- numeric(nrow(ts))
    for (i in seq_len(nrow(ts))) {
        this_year <- ts$year[i]
        this_doy <- ts$doy[i]
        idx <- which(
            ts$year < this_year &
                ts$doy == this_doy &
                (this_year - ts$year) <= lookback_years
        )
        if (length(idx) > 0) {
            historic_median[i] <- if (
                length(idx) > 0 && is.numeric(ts$value[idx])
            ) {
                median(ts$value[idx], na.rm = TRUE)
            } else {
                NA_real_
            }
        } else {
            historic_median[i] <- NA_real_
        }
        # Relative change: (current value - historic median) / historic median
        if (
            !is.na(historic_median[i]) &&
                !is.na(ts$value[i]) &&
                historic_median[i] != 0
        ) {
            relative_change[i] <- 100 *
                ts$value[i] /
                historic_median[i]
        } else {
            relative_change[i] <- NA_real_
        }
    }
    ts$historic_median <- historic_median
    ts$relative_change <- relative_change
    return(ts)
}

for (station in names(data_continuous$timeseries)) {
    ts <- data_continuous$timeseries[[station]]
    ts <- calculate_historic_daily_median(ts)
    data_continuous$timeseries[[station]] <- ts
}

for (station in names(data_discrete$timeseries)) {
    ts <- data_discrete$timeseries[[station]]
    ts <- calculate_historic_daily_median(ts)
    data_discrete$timeseries[[station]] <- ts
}


cat("Loading spatial data...\n")

# Load SWE basins from local shapefile
if (!file.exists("dev/era5/.data/shapes/swe_basins_ExportFeatures.zip")) {
    stop(
        "Shapefile 'dev/era5/.data/shapes/swe_basins_ExportFeatures.zip' does not exist."
    )
}

swe_basins_shp <- sf::st_read(
    "dev/era5/.data/shapes/swe_basins_ExportFeatures/swe_basins_ExportFeatures.shp",
    quiet = TRUE
)

# Process SWE basin names for better display
swe_col <- "Label"
if (swe_col %in% names(swe_basins_shp)) {
    # Replace underscores with spaces
    swe_basins_shp[[swe_col]] <- gsub(
        "_",
        " ",
        as.character(swe_basins_shp[[swe_col]])
    )

    # Add line breaks for long basin names (after 5+ characters followed by space)
    swe_basins_shp[[swe_col]] <- vapply(
        swe_basins_shp[[swe_col]],
        FUN.VALUE = character(1),
        FUN = function(x) {
            if (is.na(x) || x == "") {
                return(x)
            }
            sub("^(\\S{5,})\\s+", "\\1<br>", x, perl = TRUE)
        }
    )
} else {
    warning("No 'SWE_Basin' column found in shapefile")
}

# Transform to WGS84 if needed
if (sf::st_crs(swe_basins_shp)$epsg != 4326) {
    swe_basins_shp <- sf::st_transform(swe_basins_shp, 4326)
}


# Load and process snowcourse factors
weight_matrix <- load_snowcourse_factors(data_discrete$metadata)


swe_basins <- swe_basins_shp$SWE_Basin

# reorder columns to match swe_basins
#weight_matrix <- weight_matrix[, swe_basins]

# Store mean SWE for each location at the specified year and day-of-year in a named list
discrete_swe_by_location <- setNames(
    lapply(data_discrete$metadata$location_id, function(location) {
        ts <- data_discrete$timeseries[[as.character(location)]]
        values <- ts$relative_change[which(
            as.integer(format(ts$datetime, "%Y")) == year &
                as.integer(format(ts$datetime, "%j")) == doy
        )]
        if (length(values) == 0) {
            NA_real_
        } else {
            mean(values, na.rm = TRUE)
        }
    }),
    as.character(data_discrete$metadata$location)
)


# Add discrete_swe_by_location values to weight_matrix as a new column
weight_matrix$discrete_swe <- vapply(
    weight_matrix$location,
    function(loc_name) {
        if (loc_name %in% names(discrete_swe_by_location)) {
            discrete_swe_by_location[[loc_name]]
        } else {
            NA_real_
        }
    },
    FUN.VALUE = numeric(1)
)

weight_matrix <- weight_matrix[
    !is.nan(weight_matrix$discrete_swe) & !is.na(weight_matrix$discrete_swe),
]
weight_matrix[, swe_basins]

weight_matrix[, swe_basins] <- weight_matrix[, swe_basins] /
    colSums(weight_matrix[, swe_basins], na.rm = TRUE)

# Calculate weighted SWE for each basin: sum of (weight * discrete_swe) for each basin
weighted_swe <- colSums(
    as.matrix(weight_matrix[, swe_basins]) * weight_matrix$discrete_swe,
    na.rm = TRUE
)

data_at_basins <- data.frame(
    basin = names(weighted_swe),
    relative_change = as.numeric(weighted_swe),
    stringsAsFactors = FALSE
)
# Add geometry from swe_basins_shp
data_at_basins$geometry <- swe_basins_shp$geometry[match(
    data_at_basins$basin,
    swe_basins_shp$SWE_Basin
)]
data_at_basins <- sf::st_as_sf(data_at_basins, crs = sf::st_crs(swe_basins_shp))

# Load administrative boundaries and filter for Yukon
cat("Loading administrative boundaries...\n")
prov_sf <- load_spatial_layer(con2, "Provincial/Territorial Boundaries")
yukon_sf <- prov_sf[prov_sf$feature_name == "Yukon", ]

# Create mask for areas outside Yukon
bbox_extent <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = -170, xmax = -90, ymin = 50, ymax = 90),
    crs = 4326
))
inverted_yukon_sf <- sf::st_difference(bbox_extent, sf::st_union(yukon_sf))

# Load communities (filtered by importance)
cat("Loading communities...\n")
place_types <- c("City", "Town", "Village") # Removed "Settlement" for cleaner map
communities_sf <- load_spatial_layer(con2, "Communities", place_types)

# Load major roads only
cat("Loading roads...\n")
road_types <- c("Primary Highway", "Secondary Highway") # Removed minor roads for cleaner map
roads_sf <- load_spatial_layer(con2, "Roads", road_types)


# Load SWE monitoring stations with calculated means
cat("Loading SWE monitoring stations...\n")

# Generate popup content for SWE monitoring stations using ggplotOverlap

# Function to create base64-encoded ggplotOverlap plot for a station
create_continuous_plot_popup <- function(ts_id, station_name) {
    ts_query <- sprintf(
        "SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = %d",
        as.integer(ts_id)
    )
    ts <- DBI::dbGetQuery(con, ts_query)
    ts$day <- as.Date(ts$datetime)
    ts <- aggregate(value ~ day, data = ts, FUN = mean, na.rm = TRUE)
    names(ts) <- c("datetime", "value")

    plot <- ggplotOverlap(
        parameter = "snow water equivalent",
        startDay = 240,
        endDay = 239,
        years = c(2025),
        returns = "none",
        custom_title = station_name,
        line_scale = 1 * 0.9,
        axis_scale = 1 * 0.9,
        legend_scale = 1 * 0.9,
        snowbulletin = TRUE,
        lang = "en",
        gridx = FALSE,
        gridy = FALSE,
        historic_range = "last",
        filter = c(100, 100),
        continuous_data = ts
    )

    # Save the ggplotOverlap plot as a PNG tempfile
    plot_file <- tempfile(fileext = ".png")
    png(plot_file, width = 800, height = 600, res = 100)
    print(plot)
    dev.off()

    # Encode PNG to base64
    plot_data <- readBin(plot_file, "raw", file.info(plot_file)$size)
    plot_base64 <- base64enc::base64encode(plot_data)
    unlink(plot_file)

    # HTML for popup
    popup_html <- sprintf(
        "<b>%s</b><br><img src='data:image/png;base64,%s' width='380'/>",
        htmltools::htmlEscape(station_name),
        plot_base64
    )
    return(popup_html)
}


create_discrete_plot_popup <- function(ts, station_name) {
    #plot <- hydrometDiscrete(
    #    parameter = "SWE",
    #    startDay = 240,
    #    year = 2025,
    #    title = TRUE,
    #    discrete_data = ts,
    #    custom_title = station_name,
    #    lang = "en",
    #    plot_type = "linedbox"
    #)

    library(ggplot2)
    plot <- ggplot(
        ts,
        aes(x = as.Date(paste(year, month, "01", sep = "-")), y = value)
    ) +
        geom_line() +
        labs(
            title = station_name,
            x = "Date",
            y = "SWE (mm)"
        ) +
        theme_minimal()

    # Save the ggplotOverlap plot as a PNG tempfile
    plot_file <- tempfile(fileext = ".png")
    png(plot_file, width = 800, height = 600, res = 100)
    print(plot)
    dev.off()

    # Encode PNG to base64
    plot_data <- readBin(plot_file, "raw", file.info(plot_file)$size)
    plot_base64 <- base64enc::base64encode(plot_data)
    unlink(plot_file)

    # HTML for popup
    popup_html <- sprintf(
        "<b>%s</b><br><img src='data:image/png;base64,%s' width='380'/>",
        htmltools::htmlEscape(station_name),
        plot_base64
    )
    return(popup_html)
}

get_datetime <- function(year, doy) {
    # year: integer year, e.g. 2025
    # doy: integer day of year (1-366)
    # Returns POSIXct date for the given year and day-of-year
    as.POSIXct(as.Date(paste0(year, "-01-01")) + (doy - 1), tz = "UTC")
}


get_data_at_points <- function(
    data,
    year,
    doy,
    key,
    column = "relative_change",
    cutoff = 5
) {
    point_source_data <- data$metadata

    # Filter out points where the most recent record is older than cutoff years
    # Calculate difference in years between latest_date and target date
    target_date <- get_datetime(year, doy)
    timeseries_names <- names(data$timeseries)

    date_diff_years <- as.numeric(difftime(
        target_date,
        point_source_data$latest_date,
        units = "days"
    )) /
        365.25
    keep_idx <- which(!is.na(date_diff_years) & date_diff_years <= cutoff)

    point_source_data <- point_source_data[keep_idx, , drop = FALSE]
    timeseries_names <- timeseries_names[keep_idx]

    # Assert that 'key' is present in metadata
    stopifnot(key %in% colnames(point_source_data))

    # Assert that 'column' is present in all timeseries data frames
    missing_column <- any(
        vapply(
            data$timeseries,
            function(ts) !(column %in% names(ts)),
            logical(1)
        )
    )
    if (missing_column) {
        stop(sprintf(
            "Column '%s' not found in one or more timeseries.",
            column
        ))
    }

    # For each point, query the value at the target date (year/doy) using row index instead of key
    target_date <- get_datetime(year, doy)

    point_source_data$value <- vapply(
        timeseries_names,
        function(i) {
            ts <- data$timeseries[[i]]
            if (is.null(ts) || nrow(ts) == 0) {
                return(NA_real_)
            }
            diffs <- abs(difftime(ts$datetime, target_date, units = "weeks"))

            min_diff <- min(diffs)
            if (min_diff > 30 * 24 * 3600) {
                # more than 30 days in seconds
                return(NA_real_)
            }
            idx <- which.min(diffs)

            if (length(idx) == 0 || !(column %in% names(ts))) {
                return(NA_real_)
            }
            ts[[column]][idx[which.max(ts$datetime[idx])]]
        },
        FUN.VALUE = numeric(1)
    )
    return(point_source_data)
}

data_at_points <- get_data_at_points(
    data_continuous,
    year,
    doy,
    key = "timeseries_id",
    cutoff = record_cutoff_years
)

data_at_points_discrete <- get_data_at_points(
    data_discrete,
    year,
    doy,
    key = "location_id",
    cutoff = record_cutoff_years
)

# Add popup_content column to data_at_points
data_at_points$popup_content <- vapply(
    seq_len(nrow(data_at_points)),
    function(i) {
        create_continuous_plot_popup(
            data_at_points$timeseries_id[i],
            data_at_points$name[i]
        )
    },
    FUN.VALUE = character(1)
)

data_at_points_discrete$popup_content <- vapply(
    seq_len(nrow(data_at_points_discrete)),
    function(i) {
        ts <- data_discrete$timeseries[[as.character(data_at_points_discrete$location_id[
            i
        ])]]

        ts$month <- format(ts$datetime, "%m")
        ts$year <- format(ts$datetime, "%Y")
        ts$units <- "mm"
        ts <- ts[, c("month", "year", "value", "units")]
        create_discrete_plot_popup(
            ts,
            data_at_points_discrete$name[i]
        )
    },
    FUN.VALUE = character(1)
)

# =============================================================================
# MAP VISUALIZATION SETUP
# =============================================================================

cat("Setting up map visualization...\n")


station_bins <- c(-Inf, -2, -1, 0, 50, 70, 90, 110, 130, 150, Inf) # Define your bins (mm SWE)
station_colors <- c(
    "#a5a4a2",
    "#096409",
    "#7313a0",
    "#D78527",
    "#ddbe36",
    "#a6eb4c",
    "#2dc973",
    "#60e1eb",
    "#3182a8",
    "#2351ce"
) # Custom colors for bins

station_pal <- leaflet::colorBin(
    palette = station_colors,
    bins = station_bins,
    domain = if (!is.null(data_at_points)) {
        data_at_points$value
    } else {
        NULL
    },
    na.color = "gray"
)

# Create color palettes
swe_pal <- leaflet::colorBin(
    palette = station_colors, # Use the same palette as stations
    bins = station_bins,
    domain = data_at_basins$relative_change,
    na.color = "gray"
)

# Create discrete station palette (same bins and colors for consistency)
discrete_pal <- leaflet::colorBin(
    palette = station_colors,
    bins = station_bins,
    domain = if (!is.null(data_at_points_discrete)) {
        data_at_points_discrete$value
    } else {
        NULL
    },
    na.color = "gray"
)

# Create custom community icon (diamond shape)
communities_icon <- leaflet::makeIcon(
    iconUrl = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'><polygon points='8,0 16,8 8,16 0,8' fill='black' stroke='white' stroke-width='2'/></svg>",
    iconWidth = 16,
    iconHeight = 16,
    iconAnchorX = 8,
    iconAnchorY = 8
)

# Set map extent to Yukon boundary
bbox <- sf::st_bbox(yukon_sf)

# =============================================================================
# CREATE INTERACTIVE LEAFLET MAP
# =============================================================================

cat("Creating interactive map...\n")

map <- leaflet::leaflet() %>%
    # Base layer: Satellite imagery without labels
    leaflet::addProviderTiles("Esri.WorldImagery") %>%

    # Set initial view to Yukon extent
    leaflet::fitBounds(
        as.numeric(bbox["xmin"]),
        as.numeric(bbox["ymin"]),
        as.numeric(bbox["xmax"]),
        as.numeric(bbox["ymax"])
    ) %>%
    # SWE basins with SWE value coloring
    leaflet::addPolygons(
        data = data_at_basins,
        fillColor = ~ swe_pal(relative_change),
        color = "white", # White boundaries for contrast
        weight = 2,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~ lapply(
            paste0(
                data_at_basins$basin,
                "<br>",
                "Mean SWE: ",
                round(data_at_basins$relative_change, 1),
                " mm"
            ),
            htmltools::HTML
        ),
        labelOptions = leaflet::labelOptions(
            direction = "auto",
            style = list("font-weight" = "bold")
        )
    ) %>%

    # Mask areas outside Yukon
    leaflet::addPolygons(
        data = inverted_yukon_sf,
        fillColor = "#ffffff",
        fillOpacity = 0.5,
        color = NA,
        weight = 0,
        label = "Outside Yukon"
    ) %>%

    # Yukon boundary
    leaflet::addPolygons(
        data = yukon_sf,
        fill = FALSE,
        color = "black",
        weight = 3,
        opacity = 1,
        label = "Yukon Territory Boundary",
        highlightOptions = leaflet::highlightOptions(
            color = "yellow",
            weight = 8,
            opacity = 0.8
        ),
        options = leaflet::pathOptions(zIndex = 999)
    ) %>%

    # Major roads
    {
        if (!is.null(roads_sf) && nrow(roads_sf) > 0) {
            leaflet::addPolylines(
                .,
                data = roads_sf,
                color = "darkred",
                weight = 2,
                opacity = 0.8,
                label = ~ ifelse(
                    "feature_name" %in% names(roads_sf),
                    paste0("Road: ", feature_name),
                    "Road"
                ),
                highlightOptions = leaflet::highlightOptions(
                    color = "orange",
                    weight = 4,
                    opacity = 1
                ),
                options = leaflet::pathOptions(zIndex = 800),
                group = "Roads"
            )
        } else {
            .
        }
    } %>%

    # Continuous SWE monitoring stations (clickable for timeseries)
    {
        if (!is.null(data_at_points) && nrow(data_at_points) > 0) {
            leaflet::addCircleMarkers(
                .,
                data = data_at_points,
                radius = 8,
                color = "black", # Black outline for visibility
                fillColor = ~ station_pal(value),
                weight = 2,
                opacity = 1,
                fillOpacity = 0.8,
                label = ~ paste0(
                    "Continuous Station: ",
                    name,
                    "<br>",
                    "Relative Change: ",
                    round(value, 2),
                    "<br>",
                    "Timeseries ID: ",
                    timeseries_id,
                    "<br>",
                    "<i>Click to view historic data</i>"
                ),
                labelOptions = leaflet::labelOptions(
                    direction = "auto",
                    style = list("font-weight" = "bold")
                ),
                popup = ~popup_content,
                popupOptions = leaflet::popupOptions(
                    maxWidth = 800,
                    minWidth = 300,
                    minHeight = 300,
                    maxHeight = 800,
                    closeButton = TRUE,
                    autoClose = TRUE,
                    keepInView = TRUE
                ),
                options = leaflet::markerOptions(zIndex = 900),
                group = "Continuous Stations"
            )
        } else {
            .
        }
    } %>%

    # Discrete SWE monitoring stations (clickable for timeseries)
    {
        if (
            !is.null(data_at_points_discrete) &&
                nrow(data_at_points_discrete) > 0
        ) {
            leaflet::addCircleMarkers(
                .,
                data = data_at_points_discrete,
                radius = 8,
                color = "black", # White outline to distinguish from continuous
                fillColor = ~ discrete_pal(value),
                weight = 2,
                opacity = 1,
                fillOpacity = 0.8,
                label = ~ paste0(
                    "Discrete Station: ",
                    name,
                    "<br>",
                    "Relative Change: ",
                    round(value, 2),
                    "<br>",
                    "Location ID: ",
                    location_id,
                    "<br>",
                    "<i>Click to view historic data</i>"
                ),
                labelOptions = leaflet::labelOptions(
                    direction = "auto",
                    style = list("font-weight" = "bold")
                ),
                popup = ~popup_content,
                popupOptions = leaflet::popupOptions(
                    maxWidth = 800,
                    minWidth = 300,
                    minHeight = 300,
                    maxHeight = 800,
                    closeButton = TRUE,
                    autoClose = TRUE,
                    keepInView = TRUE
                ),
                options = leaflet::markerOptions(zIndex = 850),
                group = "Discrete Stations"
            )
        } else {
            .
        }
    } %>%

    # Community markers
    {
        if (!is.null(communities_sf) && nrow(communities_sf) > 0) {
            leaflet::addMarkers(
                .,
                data = communities_sf,
                icon = communities_icon,
                label = communities_sf$feature_name,
                labelOptions = leaflet::labelOptions(
                    direction = "auto",
                    style = list("font-weight" = "bold", "color" = "black"),
                    noHide = FALSE
                ),
                options = leaflet::markerOptions(zIndex = 1000),
                group = "Communities"
            )
        } else {
            .
        }
    } %>%

    # Community labels
    {
        if (!is.null(communities_sf) && nrow(communities_sf) > 0) {
            leaflet::addLabelOnlyMarkers(
                .,
                data = communities_sf,
                label = communities_sf$feature_name,
                labelOptions = leaflet::labelOptions(
                    noHide = TRUE,
                    direction = "top",
                    textOnly = TRUE,
                    style = list(
                        "color" = "black",
                        "font-weight" = "bold",
                        "font-size" = "12px",
                        "font-style" = "italic",
                        "font-family" = "'Times New Roman', Times, serif"
                    ),
                    offset = c(0, -10)
                ),
                group = "Communities"
            )
        } else {
            .
        }
    } %>%

    # Add layer controls for toggling different marker types
    leaflet::addLayersControl(
        overlayGroups = c(
            "Continuous Stations",
            "Discrete Stations",
            "Communities",
            "Roads"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE),
        position = "topleft"
    ) %>%

    # Add combined legend for both station types
    leaflet::addLegend(
        pal = station_pal,
        values = c(
            if (!is.null(data_at_points)) data_at_points$value,
            if (!is.null(data_at_points_discrete)) data_at_points_discrete$value
        ),
        title = "Relative Change",
        position = "bottomleft"
    ) %>%

    # SWE basin legend
    leaflet::addLegend(
        pal = swe_pal,
        values = data_at_basins$relative_change,
        title = "Basin SWE",
        position = "bottomright"
    )

# Clean up database connections
#DBI::dbDisconnect(con)
#DBI::dbDisconnect(con2)

cat("Map creation completed!\n")

# Display the map
map
