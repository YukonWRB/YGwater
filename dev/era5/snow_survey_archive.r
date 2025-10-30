# =============================================================================
# Snow Water Equivalent (SWE) Basin Visualization - Shiny App
# =============================================================================
# This Shiny app creates an interactive leaflet map showing:
# - SWE basins colored by weighted SWE values from discrete stations
# - Continuous SWE monitoring stations colored by relative change
# - Discrete SWE monitoring stations colored by relative change
# - Communities, roads, and geographic boundaries
#
# The map displays snow water equivalent data as a percentage of historic normal
# for a specific date (year and day-of-year). Basin values are calculated using
# weighted averages from discrete snow course stations.
#
# Data Sources:
# - SWE basins: Local shapefile (swe_basins_ExportFeatures.shp)
# - Stations/Communities/Roads: PostgreSQL spatial database (AquaCache)
# - Timeseries data: Continuous and discrete measurements database
# - Snowcourse factors: CSV file with basin weighting factors
# =============================================================================

# Load required libraries
library(shiny) # For reactive web applications
library(AquaCache) # For database connections and water data functions
library(YGwater) # For Yukon water-specific functions
library(DBI) # For database interface
library(plotly) # For interactive plots
library(leaflet) # For interactive maps
library(sf) # For spatial data handling
library(base64enc) # For encoding plots in popups
library(ggplot2) # For creating plots

# =============================================================================
# CONFIGURATION
# =============================================================================

# Analysis parameters - these control what data is displayed
record_cutoff_years <- 5 # Only show stations with data within this many years

# Date range for loading historical data
start_date <- sprintf("%d-01-01", 1950) # Start of historical period
end_date <- sprintf("%d-01-01", 3000) # End date (far future to get all data)

# Color scheme and visualization parameters
# Bins represent percentage of normal SWE (relative_change values)
station_bins <- c(-2, -1, 0, 50, 70, 90, 110, 130, 150, Inf)
station_colors <- c(
    "#27A62C", # Green (much below normal)
    "#B200DD", # Purple (below normal)
    "#EBB966", # Orange (near normal)
    "#EEE383", # Yellow (normal)
    "#C1FB80", # Light green (above normal)
    "#6CFC88", # Green (well above normal)
    "#8CEFE1", # Cyan (high)
    "#85B4F8", # Light blue (very high)
    "#6772F7" # Blue (extremely high)
)

# Absolute value bins and colors for SWE (in mm)
absolute_bins <- c(0, 50, 100, 150, 200, 250, 300, 400, 500, Inf)
absolute_colors <- c(
    "#f7f7f7", # Very light gray (no snow)
    "#cccccc", # Light gray (minimal)
    "#a1dab4", # Light green
    "#41b6c4", # Light blue
    "#2c7fb8", # Medium blue
    "#253494", # Dark blue
    "#762a83", # Purple
    "#5d1a86", # Dark purple
    "#2d004b" # Very dark purple
)

# Icon definitions
communities_icon_svg <- "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'><polygon points='8,0 16,8 8,16 0,8' fill='black' stroke='white' stroke-width='2'/></svg>"

# Basemap options for the leaflet map
basemap_providers <- list(
    "Satellite" = "Esri.WorldImagery",
    "Topographic" = "Esri.WorldTopoMap",
    "Street Map" = "OpenStreetMap",
    "Terrain" = "Stamen.Terrain",
    "Watercolor" = "Stamen.Watercolor",
    "Dark" = "CartoDB.DarkMatter",
    "Light" = "CartoDB.Positron"
)

# Database configurations for production and development environments
config <- list(
    dbName = "aquacache",
    dbHost = Sys.getenv("aquacacheHostProd"),
    dbPort = Sys.getenv("aquacachePortProd"),
    dbUser = Sys.getenv("aquacacheUserProd"),
    dbPass = Sys.getenv("aquacachePassProd")
)

config_dev <- list(
    dbName = "aquacache",
    dbHost = Sys.getenv("aquacacheHost"),
    dbPort = Sys.getenv("aquacachePort"),
    dbUser = Sys.getenv("aquacacheAdminUser"),
    dbPass = Sys.getenv("aquacacheAdminPass")
)

# =============================================================================
# DATABASE CONNECTIONS
# =============================================================================

# Establish database connections
# con2: Production database for spatial layers (communities, roads, boundaries)
con2 <- AquaConnect(
    name = config$dbName,
    host = config$dbHost,
    port = config$dbPort,
    user = config$dbUser,
    pass = config$dbPass
)

# con: Development database for timeseries data (measurements)
con <- AquaConnect(
    name = config_dev$dbName,
    host = config_dev$dbHost,
    port = config_dev$dbPort,
    user = config_dev$dbUser,
    pass = config_dev$dbPass
)

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Get the most recent non-NaN value date from a timeseries data.frame
#'
#' @param ts data.frame with 'datetime' and 'value' columns
#' @return POSIXct datetime of the most recent valid measurement, or NA
#' @details This function finds the latest date in a timeseries that has a valid
#'   (non-NA, non-NaN) value. Used to filter out stations with stale data.
get_most_recent_date <- function(ts) {
    if (
        is.null(ts) ||
            nrow(ts) == 0 ||
            !"datetime" %in% names(ts) ||
            !"value" %in% names(ts)
    ) {
        return(NA_real_)
    }
    ts$datetime <- as.POSIXct(ts$datetime)
    valid_idx <- which(!is.na(ts$value) & !is.nan(ts$value))
    if (length(valid_idx) == 0) {
        return(NA_real_)
    }
    latest_time <- max(ts$datetime[valid_idx], na.rm = TRUE)
    return(latest_time)
}

#' Convert year and day-of-year to POSIXct datetime
#'
#' @param year integer year (e.g., 2025)
#' @param doy integer day of year (1-366)
#' @return POSIXct datetime object in UTC timezone
#' @examples
#' get_datetime(2025, 60)  # Returns Feb 29, 2025 (or Feb 28 in non-leap year)
get_datetime <- function(year, doy) {
    as.POSIXct(as.Date(paste0(year, "-01-01")) + (doy - 1), tz = "UTC")
}

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Load spatial data from database with coordinate transformation
#'
#' @param con DBI database connection object
#' @param layer_name character string of the spatial layer name in the database
#' @param description_filter optional character vector to filter by description field
#' @return sf object in WGS84 (EPSG:4326) coordinate system, or NULL if no data found
#' @details Queries the spatial.vectors table and transforms geometries to WGS84.
#'   Used for loading administrative boundaries, communities, roads, etc.
load_spatial_layer <- function(con, layer_name, description_filter = NULL) {
    query <- sprintf(
        "SELECT *, ST_AsText(ST_Transform(geom, 4326)) as geom_4326 
         FROM spatial.vectors 
         WHERE layer_name = '%s'",
        layer_name
    )

    if (!is.null(description_filter)) {
        filter_str <- paste0("'", description_filter, "'", collapse = ", ")
        query <- paste(query, sprintf("AND description IN (%s)", filter_str))
    }

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
#' @param con DBI database connection object
#' @param start_date character date string (YYYY-MM-DD) for filtering data
#' @param end_date character date string (YYYY-MM-DD) for filtering data
#' @param resolution character, either "daily" or "monthly" for data aggregation
#' @return list with two elements:
#'   - timeseries: named list of data.frames (names are timeseries_id)
#'   - metadata: sf object with station locations and latest_date column
#' @details Loads continuous snow water equivalent measurements from the database.
#'   Data is aggregated to daily resolution by default. Metadata includes the
#'   most recent measurement date for each station.
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
#' @param con DBI database connection object
#' @param start_date character date string (YYYY-MM-DD) for filtering data
#' @param end_date character date string (YYYY-MM-DD) for filtering data
#' @return list with two elements:
#'   - timeseries: named list of data.frames (names are location_id)
#'   - metadata: sf object with station locations and latest_date column
#' @details Loads discrete (manual) snow water equivalent measurements from snow
#'   courses. Uses target_datetime when available, falls back to datetime.
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

#' Load and process snowcourse factors for discrete SWE stations
#'
#' @param metadata_discrete sf object with discrete station metadata
#' @param csv_path character path to the snowcourse factors CSV file
#' @return data.frame with location names and basin weighting factors
#' @details Reads the CSV file containing weights for how much each snow course
#'   station contributes to each SWE basin calculation. Removes duplicates and
#'   unnecessary columns.
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

    return(snowcourse_factors)
}

# =============================================================================
# DATA PROCESSING FUNCTIONS
# =============================================================================

#' Calculate historic daily median and relative change for timeseries
#'
#' @param ts data.frame with 'datetime' and 'value' columns
#' @param lookback_year year to start lookback period (e.g., 1980)
#' @param lookback_length integer number of years to look back from each measurement
#' @param start_year integer starting year for analysis (currently unused)
#' @return data.frame with additional columns:
#'   - doy: day of year
#'   - year: year extracted from datetime
#'   - historic_median: median value for same day-of-year in previous years
#'   - relative_change: current value as percentage of historic median
#' @details For each measurement, calculates the historic median for the same day
#'   of year using data from previous years within the lookback period. Day-of-year
#'   values are snapped to key dates (32, 60, 91, 121) if within 2 days.
calculate_historic_daily_median <- function(
    ts,
    lookback_length = NULL,
    lookback_year = NULL
) {
    if (is.null(lookback_year) && is.null(lookback_length)) {
        lookback_year <- 1980
    } else if (!is.null(lookback_year) && !is.null(lookback_length)) {
        stop("Specify either lookback_year or lookback_length, not both.")
    }

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

    historic_median <- numeric(nrow(ts))
    relative_change <- numeric(nrow(ts))

    for (i in seq_len(nrow(ts))) {
        this_year <- ts$year[i]
        this_doy <- ts$doy[i]
        if (!is.null(lookback_year)) {
            lookback_filter <- ts$year >= lookback_year
        } else if (!is.null(lookback_length)) {
            lookback_filter <- (this_year - ts$year) <= lookback_length
        }

        idx <- which(
            ts$year < this_year &
                ts$doy == this_doy &
                lookback_filter
        )

        if (length(idx) > 0) {
            historic_median[i] <- median(ts$value[idx], na.rm = TRUE)
        } else {
            historic_median[i] <- NA_real_
        }

        if (
            !is.na(historic_median[i]) &&
                !is.na(ts$value[i]) &&
                historic_median[i] != 0
        ) {
            relative_change[i] <- 100 * ts$value[i] / historic_median[i]
        } else {
            relative_change[i] <- NA_real_
        }
    }

    ts$historic_median <- historic_median
    ts$relative_change <- relative_change
    return(ts)
}

#' Extract data at specific points for a given year and day-of-year
#'
#' @param data list containing timeseries and metadata from get_*_timeseries functions
#' @param year integer target year for data extraction
#' @param doy integer target day of year for data extraction
#' @param key character name of the key column in metadata (e.g., "timeseries_id")
#' @param column character name of the column to extract from timeseries (default "relative_change")
#' @param cutoff numeric maximum years between target date and latest available data
#' @return data.frame subset of metadata with additional 'value' column
#' @details Filters stations to only include those with recent data (within cutoff years),
#'   then extracts the specified column value for the target date. Uses nearest
#'   available date within 30 days if exact date not found.
get_swe_state <- function(
    timeseries,
    metadata,
    year,
    doy,
    key,
    cutoff = 5
) {
    point_source_data <- metadata

    # Filter out points where the most recent record is older than cutoff years
    target_date <- get_datetime(year, doy)
    timeseries_names <- names(timeseries)

    date_diff_years <- as.numeric(difftime(
        target_date,
        point_source_data$latest_date,
        units = "days"
    )) /
        365.25

    keep_idx <- which(!is.na(date_diff_years) & date_diff_years <= cutoff)
    point_source_data <- point_source_data[keep_idx, , drop = FALSE]
    timeseries_names <- timeseries_names[keep_idx]

    stopifnot(key %in% colnames(point_source_data))

    # Extract both relative_change and value for flexibility
    # Consolidated nearest-value lookup for both relative_change and value
    vals_mat <- vapply(
        timeseries_names,
        function(i) {
            ts <- data$timeseries[[i]]
            if (is.null(ts) || nrow(ts) == 0 || !"datetime" %in% names(ts)) {
                return(c(NA_real_, NA_real_))
            }

            # compute absolute difference in seconds to the target_date
            diffs_secs <- abs(as.numeric(difftime(
                ts$datetime,
                target_date,
                units = "secs"
            )))

            # if no valid diffs or the nearest point is > 30 days (in seconds), return NA
            if (all(is.na(diffs_secs))) {
                return(c(NA_real_, NA_real_))
            }
            min_diff <- min(diffs_secs, na.rm = TRUE)
            if (is.na(min_diff) || min_diff > 30 * 24 * 3600) {
                return(c(NA_real_, NA_real_))
            }

            # choose the timestamp with the minimum diff; if ties, pick the most recent timestamp
            idxs <- which(diffs_secs == min_diff)
            chosen_idx <- idxs[which.max(ts$datetime[idxs])]

            rel_val <- if ("relative_change" %in% names(ts)) {
                ts$relative_change[chosen_idx]
            } else {
                NA_real_
            }
            abs_val <- if ("value" %in% names(ts)) {
                ts$value[chosen_idx]
            } else {
                NA_real_
            }

            c(rel_val, abs_val)
        },
        FUN.VALUE = c(NA_real_, NA_real_)
    )

    # vapply returns a 2 x N matrix: row 1 = relative, row 2 = absolute
    point_source_data$relative_value <- as.numeric(vals_mat[1, ])
    point_source_data$absolute_value <- as.numeric(vals_mat[2, ])

    return(point_source_data)
}

# =============================================================================
# POPUP CREATION FUNCTIONS
# =============================================================================

#' Create base64-encoded ggplotOverlap plot for continuous stations
#'
#' @param ts_id integer timeseries ID for database query
#' @param station_name character station name for plot title
#' @return character string containing HTML with embedded base64 image
#' @details Creates a YGwater ggplotOverlap plot showing current year data
#'   compared to historical range. Plot is saved as PNG, encoded to base64,
#'   and embedded in HTML for use in leaflet popups.
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

    # Save the ggplotOverlap plot as a PNG tempfile with smaller dimensions
    plot_file <- tempfile(fileext = ".png")
    png(plot_file, width = 800, height = 600, res = 120)
    print(plot)
    dev.off()

    # Encode PNG to base64
    plot_data <- readBin(plot_file, "raw", file.info(plot_file)$size)
    plot_base64 <- base64enc::base64encode(plot_data)
    unlink(plot_file)

    # HTML for popup with smaller image and click-to-open functionality
    popup_html <- sprintf(
        "<div style='text-align: center; width: 500px; max-width: none;'>
            <b>%s</b><br>
            <img src='data:image/png;base64,%s' 
                 width='500' 
                 style='cursor: pointer; border: 1px solid #ccc; margin-top: 10px; max-width: none;' 
                 onclick='window.open(\"data:image/png;base64,%s\", \"_blank\", \"width=800,height=600\");'
                 title='Click to open full size in new window' />
            <br><small style='color: #666;'>Click image to open full size</small>
        </div>",
        htmltools::htmlEscape(station_name),
        plot_base64,
        plot_base64
    )
    return(popup_html)
}

#' Create base64-encoded plot for discrete stations
#'
#' @param ts data.frame with timeseries data formatted for plotting
#' @param station_name character station name for plot title
#' @return character string containing HTML with embedded base64 image
#' @details Creates a simple ggplot2 line plot of discrete measurements over time.
#'   Plot is saved as PNG, encoded to base64, and embedded in HTML for leaflet popups.
create_discrete_plot_popup <- function(ts, station_name) {
    # Clean and validate the data before plotting
    ts_clean <- ts[
        !is.na(ts$value) & !is.na(ts$datetime) & is.finite(ts$value),
    ]

    if (nrow(ts_clean) == 0) {
        # Create a simple message plot if no data
        plot <- ggplot() +
            annotate(
                "text",
                x = 0.5,
                y = 0.5,
                label = "No data available",
                size = 6
            ) +
            xlim(0, 1) +
            ylim(0, 1) +
            labs(title = station_name) +
            theme_minimal() +
            theme(axis.text = element_blank(), axis.ticks = element_blank())
    } else {
        # Use hydrometDiscrete with cleaned data
        plot <- tryCatch(
            {
                hydrometDiscrete(
                    parameter = "SWE",
                    location = station_name,
                    startDay = 1,
                    discrete_data = ts_clean,
                    plot_type = "linedbox"
                )
            },
            error = function(e) {
                # Fallback to simple ggplot if hydrometDiscrete fails
                ggplot(ts_clean, aes(x = datetime, y = value)) +
                    geom_line() +
                    geom_point() +
                    labs(
                        title = station_name,
                        x = "Date",
                        y = "SWE (mm)"
                    ) +
                    theme_minimal()
            }
        )
    }

    # Save the plot as a PNG tempfile with smaller dimensions
    plot_file <- tempfile(fileext = ".png")

    # Suppress warnings during PNG creation to avoid the geom_line warning
    suppressWarnings({
        png(plot_file, width = 800, height = 600, res = 120)
        print(plot)
        dev.off()
    })

    # Encode PNG to base64
    plot_data <- readBin(plot_file, "raw", file.info(plot_file)$size)
    plot_base64 <- base64enc::base64encode(plot_data)
    unlink(plot_file)

    # HTML for popup with smaller image and click-to-open functionality
    popup_html <- sprintf(
        "<div style='text-align: center; width: 500px; max-width: none;'>
            <b>%s</b><br>
            <img src='data:image/png;base64,%s' 
                 width='500' 
                 style='cursor: pointer; border: 1px solid #ccc; margin-top: 10px; max-width: none;' 
                 onclick='window.open(\"data:image/png;base64,%s\", \"_blank\", \"width=800,height=600\");'
                 title='Click to open full size in new window' />
            <br><small style='color: #666;'>Click image to open full size</small>
        </div>",
        htmltools::htmlEscape(station_name),
        plot_base64,
        plot_base64
    )
    return(popup_html)
}


#' Load all base data for the application
#'
#' @return list containing all loaded base data including timeseries, spatial layers, etc.
#' @details This function loads all static data that doesn't change based on user inputs.
#'   It includes timeseries data, spatial layers, and processed metadata.
load_base_data <- function(con, con2) {
    cat("Loading base data...\n")

    # Initialize base data list
    base_data <- list()

    cat("Loading timeseries data...\n")

    # Load timeseries data from both continuous and discrete sources
    base_data$data_continuous <- get_continuous_timeseries(
        con,
        start_date,
        end_date
    )
    base_data$data_discrete <- get_discrete_timeseries(
        con,
        start_date,
        end_date
    )

    # Calculate historic medians and relative changes for all stations
    # This adds historic_median and relative_change columns to each timeseries
    for (station in names(base_data$data_continuous$timeseries)) {
        ts <- base_data$data_continuous$timeseries[[station]]
        ts <- calculate_historic_daily_median(ts, lookback_year = 1980)
        base_data$data_continuous$timeseries[[station]] <- ts
    }

    for (station in names(base_data$data_discrete$timeseries)) {
        ts <- base_data$data_discrete$timeseries[[station]]
        ts <- calculate_historic_daily_median(ts, lookback_year = 1980)
        base_data$data_discrete$timeseries[[station]] <- ts
    }

    # Ensure SWE basins shapefile is transformed to WGS84
    base_data$swe_basins_shp <- sf::st_read(
        "dev/era5/.data/shapes/swe_basins_ExportFeatures/swe_basins_ExportFeatures.shp",
        quiet = TRUE
    )

    if (!is.null(base_data$swe_basins_shp)) {
        if (sf::st_crs(base_data$swe_basins_shp)$epsg != 4326) {
            base_data$swe_basins_shp <- sf::st_transform(
                base_data$swe_basins_shp,
                crs = 4326
            )
        }
    }

    # Ensure Yukon boundaries are transformed to WGS84
    prov_sf <- load_spatial_layer(con2, "Provincial/Territorial Boundaries")
    if (!is.null(prov_sf)) {
        if (sf::st_crs(prov_sf)$epsg != 4326) {
            prov_sf <- sf::st_transform(prov_sf, crs = 4326)
        }
    }
    base_data$yukon_sf <- prov_sf[prov_sf$feature_name == "Yukon", ]

    # Ensure inverted Yukon mask is transformed to WGS84
    if (!is.null(base_data$yukon_sf)) {
        bbox_extent <- sf::st_as_sfc(sf::st_bbox(
            c(xmin = -170, xmax = -90, ymin = 50, ymax = 90),
            crs = 4326
        ))
        base_data$inverted_yukon_sf <- sf::st_difference(
            bbox_extent,
            sf::st_union(base_data$yukon_sf)
        )
    }

    # Ensure communities and roads layers are transformed to WGS84
    if (!is.null(base_data$communities_sf)) {
        if (sf::st_crs(base_data$communities_sf)$epsg != 4326) {
            base_data$communities_sf <- sf::st_transform(
                base_data$communities_sf,
                crs = 4326
            )
        }
    }

    if (!is.null(base_data$roads_sf)) {
        if (sf::st_crs(base_data$roads_sf)$epsg != 4326) {
            base_data$roads_sf <- sf::st_transform(
                base_data$roads_sf,
                crs = 4326
            )
        }
    }

    # Load SWE basins from shapefile
    if (!file.exists("dev/era5/.data/shapes/swe_basins_ExportFeatures.zip")) {
        stop(
            "Shapefile 'dev/era5/.data/shapes/swe_basins_ExportFeatures.zip' does not exist."
        )
    }

    # Process basin names for better display on map
    # Replace underscores with spaces and add line breaks for long names
    base_data$swe_basins_shp$annotation <- base_data$swe_basins_shp$Label
    base_data$swe_basins_shp$annotation <- gsub(
        "_",
        " ",
        as.character(base_data$swe_basins_shp$annotation)
    )
    base_data$swe_basins_shp$annotation <- vapply(
        base_data$swe_basins_shp$annotation,
        FUN.VALUE = character(1),
        FUN = function(x) {
            if (is.na(x) || x == "") {
                return(x)
            }
            # Add line break after first word if it's 5+ characters
            sub("^(\\S{5,})\\s+", "\\1<br>", x, perl = TRUE)
        }
    )

    # Transform to WGS84 coordinate system if needed for web mapping
    if (sf::st_crs(base_data$swe_basins_shp)$epsg != 4326) {
        base_data$swe_basins_shp <- sf::st_transform(
            base_data$swe_basins_shp,
            4326
        )
    }

    # Calculate weighted basin values using snow course data
    # Load the weighting factors that determine how much each station contributes to each basin
    base_data$weight_matrix <- load_snowcourse_factors(
        base_data$data_discrete$metadata
    )
    base_data$swe_basins <- base_data$swe_basins_shp$SWE_Basin

    # Load adjusted basin centroids from GeoJSON
    if (file.exists("data-raw/swe_basin_centroids_adjusted.geojson")) {
        base_data$swe_at_basins_labels <- sf::st_read(
            "data-raw/swe_basin_centroids_adjusted.geojson",
            quiet = TRUE
        )
    } else {
        warning(
            "GeoJSON file 'data-raw/swe_basin_centroids_adjusted.geojson' not found."
        )
        base_data$swe_at_basins_labels <- NULL
    }

    # Load additional spatial layers for context
    cat("Loading administrative boundaries...\n")
    prov_sf <- load_spatial_layer(con2, "Provincial/Territorial Boundaries")
    base_data$yukon_sf <- prov_sf[prov_sf$feature_name == "Yukon", ]

    # Create mask for areas outside Yukon (will be shown as white overlay)
    if (!is.null(base_data$yukon_sf)) {
        bbox_extent <- sf::st_as_sfc(sf::st_bbox(
            c(xmin = -170, xmax = -90, ymin = 50, ymax = 90),
            crs = 4326
        ))
        base_data$inverted_yukon_sf <- sf::st_difference(
            bbox_extent,
            sf::st_union(base_data$yukon_sf)
        )
    } else {
        base_data$inverted_yukon_sf <- NULL
    }

    cat("Loading communities...\n")
    place_types <- c("City", "Town", "Village") # Filter to major communities only
    base_data$communities_sf <- load_spatial_layer(
        con2,
        "Communities",
        place_types
    )

    cat("Loading roads...\n")
    road_types <- c("Primary Highway", "Secondary Highway") # Major roads only
    base_data$roads_sf <- load_spatial_layer(con2, "Roads", road_types)

    cat("Base data loading completed!\n")

    return(base_data)
}


# =============================================================================
# DATA LOADING AND PROCESSING
# =============================================================================

# Load all base data once at startup
base_data <- load_base_data(con, con2)

# =============================================================================
# SHINY APP UI
# =============================================================================

ui <- fluidPage(
    tags$head(
        tags$style(HTML(
            "
            body, html {
                height: 100%;
                margin: 0;
                padding: 0;
                overflow: hidden;
            }
            .container-fluid {
                height: 100vh;
                padding: 0 !important;
                margin: 0 !important;
            }
            .main-container {
                height: 100vh;
                padding: 0 !important;
                margin: 0 !important;
                position: relative;
            }
            .control-accordion {
                position: absolute;
                top: 10px;
                right: 10px;
                z-index: 1000;
                background: rgba(248, 249, 250, 0.95);
                border: 1px solid #dee2e6;
                border-radius: 5px;
                box-shadow: 0 2px 10px rgba(0,0,0,0.1);
                min-width: 300px;
                max-width: 400px;
            }
            .control-header {
                background: #e9ecef;
                padding: 12px 15px;
                cursor: pointer;
                user-select: none;
                display: flex;
                justify-content: space-between;
                align-items: center;
                border-bottom: 1px solid #dee2e6;
                border-radius: 4px 4px 0 0;
            }
            .control-header:hover {
                background: #dde2e6;
            }
            .control-header h4 {
                margin: 0;
                font-size: 16px;
                font-weight: 600;
                color: #495057;
            }
            .control-toggle {
                font-size: 14px;
                color: #6c757d;
                transition: transform 0.2s ease;
            }
            .control-content {
                padding: 15px;
                display: block;
                transition: max-height 0.3s ease-out, padding 0.3s ease-out;
                max-height: 200px;
            }
            .control-content.collapsed {
                max-height: 0;
                padding: 0 15px;
                overflow: hidden;
            }
            .slider-row {
                display: flex;
                align-items: center;
                margin-bottom: 15px;
            }
            .slider-row:last-child {
                margin-bottom: 0;
            }
            .slider-label {
                min-width: 80px;
                font-weight: bold;
                margin-right: 15px;
                font-size: 14px;
            }
            .slider-input {
                flex: 1;
                max-width: 200px;
            }
            .date-display {
                margin-left: 15px;
                font-style: italic;
                color: #6c757d;
                font-size: 13px;
            }
            #map {
                position: absolute !important;
                top: 0 !important;
                left: 0 !important;
                height: 100vh !important;
                width: 100vw !important;
                margin: 0 !important;
                padding: 0 !important;
                z-index: 1 !important;
            }
            .leaflet-control-layers {
                z-index: 800;
            }
        "
        ))
    ),

    # Main container with full viewport
    div(
        class = "main-container",

        # Full screen map (positioned absolutely to fill entire viewport)
        leafletOutput("map"),

        # Floating control accordion (positioned absolutely over the map)
        div(
            class = "control-accordion",

            # Accordion header
            div(
                class = "control-header",
                onclick = "toggleControls()",
                h4("SWE Analysis Controls"),
                span(class = "control-toggle", id = "control-toggle", "▼")
            ),

            # Accordion content
            div(
                class = "control-content",
                id = "control-content",

                div(
                    class = "slider-row",
                    div(class = "slider-label", "Year:"),
                    div(
                        class = "slider-input",
                        sliderInput(
                            "year",
                            label = NULL,
                            min = 2010,
                            max = 2025,
                            value = 2025,
                            step = 1,
                            sep = ""
                        )
                    )
                ),

                div(
                    class = "slider-row",
                    div(class = "slider-label", "Month:"),
                    div(class = "slider-input", {
                        months_map <- stats::setNames(
                            c(32, 60, 91, 121),
                            c("February", "March", "April", "May")
                        )
                        choices <- stats::setNames(
                            as.character(months_map),
                            names(months_map)
                        )
                        tagList(
                            selectInput(
                                "doy",
                                label = NULL,
                                choices = choices,
                                selected = as.character(months_map[2]),
                                width = "100%"
                            )
                        )
                    })
                ),

                div(
                    class = "slider-row",
                    div(class = "slider-label", "Values:"),
                    div(
                        class = "slider-input",
                        radioButtons(
                            "value_type",
                            label = NULL,
                            choices = list(
                                "% of Normal" = "relative",
                                "Absolute (mm)" = "absolute"
                            ),
                            selected = "relative",
                            inline = TRUE
                        )
                    )
                )
            )
        )
    ),

    # JavaScript for collapsible functionality and plot generation
    tags$script(HTML(
        "
        function toggleControls() {
            var content = document.getElementById('control-content');
            var toggle = document.getElementById('control-toggle');
            
            if (content.classList.contains('collapsed')) {
                content.classList.remove('collapsed');
                toggle.innerHTML = '▼';
                toggle.style.transform = 'rotate(0deg)';
            } else {
                content.classList.add('collapsed');
                toggle.innerHTML = '▶';
                toggle.style.transform = 'rotate(-90deg)';
            }
        }
        
        function generatePlot(type, stationId, stationName) {
            // Show loading message
            var popup = document.querySelector('.leaflet-popup-content');
            if (popup) {
                popup.innerHTML = '<div style=\"text-align: center; padding: 20px;\"><b>' + 
                    stationName + '</b><br><br>Loading plot...<br>' + 
                    '<div style=\"margin-top: 10px;\">⏳</div></div>';
            }
            
            // Send request to Shiny to generate plot
            Shiny.setInputValue('generate_plot', {
                type: type,
                station_id: stationId,
                station_name: stationName,
                timestamp: Date.now()
            });
        }
        
        // Listen for custom messages from Shiny to update popup content
        Shiny.addCustomMessageHandler('updatePopup', function(data) {
            var popup = document.querySelector('.leaflet-popup-content');
            if (popup) {
                popup.innerHTML = data.html;
            }
        });
    "
    ))
)

# =============================================================================
# SHINY APP SERVER
# =============================================================================

server <- function(input, output, session) {
    # Reactive expression for selected date display
    output$selected_date <- renderText({
        req(input$year, input$doy)
        # Convert DOY from character to numeric
        doy_numeric <- as.numeric(input$doy)
        date_obj <- get_datetime(input$year, doy_numeric)
        format(date_obj, "%B %d, %Y")
    })

    # Reactive data processing based on user inputs
    processed_data <- reactive({
        req(input$year, input$doy)

        # Convert DOY from character to numeric
        doy_numeric <- as.numeric(input$doy)

        # Extract data at points for the selected date - determine column to use
        value_column <- if (input$value_type == "relative") {
            "relative_change"
        } else {
            "value"
        }

        # Extract data at points for the selected date
        swe_at_pillows <- get_swe_state(
            base_data$data_continuous,
            input$year,
            doy_numeric,
            key = "timeseries_id",
            cutoff = record_cutoff_years
        )

        swe_at_surveys <- get_swe_state(
            base_data$data_discrete,
            input$year,
            doy_numeric,
            key = "location_id",
            cutoff = record_cutoff_years
        )

        # Set the value column based on user selection
        if (nrow(swe_at_pillows) > 0) {
            swe_at_pillows$value <- if (input$value_type == "relative") {
                swe_at_pillows$relative_value
            } else {
                swe_at_pillows$absolute_value
            }
        }

        if (nrow(swe_at_surveys) > 0) {
            swe_at_surveys$value <- if (input$value_type == "relative") {
                swe_at_surveys$relative_value
            } else {
                swe_at_surveys$absolute_value
            }
        }

        # Add location names to discrete data
        if (nrow(swe_at_surveys) > 0) {
            swe_at_surveys$location <- base_data$data_discrete$metadata$location[
                match(
                    swe_at_surveys$location_id,
                    base_data$data_discrete$metadata$location_id
                )
            ]
        }

        # Create popup content
        if (nrow(swe_at_pillows) > 0) {
            swe_at_pillows$popup_content <- vapply(
                seq_len(nrow(swe_at_pillows)),
                function(i) {
                    abs_value <- if (!is.na(swe_at_pillows$absolute_value[i])) {
                        paste0(
                            round(swe_at_pillows$absolute_value[i], 1),
                            " mm"
                        )
                    } else {
                        "No data"
                    }

                    rel_value <- if (!is.na(swe_at_pillows$relative_value[i])) {
                        paste0(
                            round(swe_at_pillows$relative_value[i], 1),
                            "% of normal"
                        )
                    } else {
                        "No data"
                    }

                    sprintf(
                        "<div style='text-align: center; padding: 10px;'>
                            <b>%s</b><br>
                            <span style='font-size: 12px; color: #666;'>ID: %s</span><br>
                            <span style='font-size: 14px;'><b>Absolute SWE:</b> %s</span><br>
                            <span style='font-size: 14px;'><b>Relative SWE:</b> %s</span><br>
                            <button onclick='generatePlot(\"continuous\", %d, \"%s\")' 
                                    style='margin-top: 8px; padding: 5px 10px; cursor: pointer;'>
                                Show Plot
                            </button>
                        </div>",
                        htmltools::htmlEscape(swe_at_pillows$name[i]),
                        htmltools::htmlEscape(swe_at_pillows$location[i]),
                        abs_value,
                        rel_value,
                        swe_at_pillows$timeseries_id[i],
                        htmltools::htmlEscape(swe_at_pillows$name[i])
                    )
                },
                FUN.VALUE = character(1)
            )
        } else {
            swe_at_pillows$popup_content <- character(0)
        }

        if (nrow(swe_at_surveys) > 0) {
            swe_at_surveys$popup_content <- vapply(
                seq_len(nrow(swe_at_surveys)),
                function(i) {
                    abs_value <- if (!is.na(swe_at_surveys$absolute_value[i])) {
                        paste0(
                            round(swe_at_surveys$absolute_value[i], 1),
                            " mm"
                        )
                    } else {
                        "No data"
                    }

                    rel_value <- if (!is.na(swe_at_surveys$relative_value[i])) {
                        paste0(
                            round(swe_at_surveys$relative_value[i], 1),
                            "% of normal"
                        )
                    } else {
                        "No data"
                    }

                    sprintf(
                        "<div style='text-align: center; padding: 10px;'>
                            <b>%s</b><br>
                            <span style='font-size: 12px; color: #666;'>ID: %s</span><br>
                            <span style='font-size: 14px;'><b>Absolute SWE:</b> %s</span><br>
                            <span style='font-size: 14px;'><b>Relative SWE:</b> %s</span><br>
                            <button onclick='generatePlot(\"discrete\", %d, \"%s\")' 
                                    style='margin-top: 8px; padding: 5px 10px; cursor: pointer;'>
                                Show Plot
                            </button>
                        </div>",
                        htmltools::htmlEscape(swe_at_surveys$name[i]),
                        htmltools::htmlEscape(swe_at_surveys$location[i]),
                        abs_value,
                        rel_value,
                        swe_at_surveys$location_id[i],
                        htmltools::htmlEscape(swe_at_surveys$name[i])
                    )
                },
                FUN.VALUE = character(1)
            )
        } else {
            swe_at_surveys$popup_content <- character(0)
        }

        # Calculate basin values
        weight_matrix <- base_data$weight_matrix
        if (nrow(swe_at_surveys) > 0) {
            weight_matrix$value <- vapply(
                weight_matrix$location,
                function(loc_name) {
                    idx <- which(swe_at_surveys$location == loc_name)
                    if (length(idx) > 0) {
                        swe_at_surveys$value[idx[1]]
                    } else {
                        NA_real_
                    }
                },
                FUN.VALUE = numeric(1)
            )
        } else {
            weight_matrix$value <- rep(NA_real_, nrow(weight_matrix))
        }

        # Filter and calculate weighted SWE
        weight_matrix_filtered <- weight_matrix[
            !is.na(weight_matrix$value) & !is.nan(weight_matrix$value),
        ]

        if (nrow(weight_matrix_filtered) > 0) {
            # normalize SWE weights (weights should add up to 10 but in cases there is missing data at weighted stations, we normalize)
            weight_matrix_filtered[,
                base_data$swe_basins
            ] <- weight_matrix_filtered[, base_data$swe_basins] /
                colSums(
                    weight_matrix_filtered[, base_data$swe_basins],
                    na.rm = TRUE
                )
            weighted_swe <- colSums(
                as.matrix(weight_matrix_filtered[, base_data$swe_basins]) *
                    weight_matrix_filtered$value,
                na.rm = TRUE
            )
        } else {
            weighted_swe <- rep(0, length(base_data$swe_basins))
            names(weighted_swe) <- base_data$swe_basins
        }

        # Create basin data
        swe_at_basins <- data.frame(
            basin = names(weighted_swe),
            relative_change = as.numeric(weighted_swe),
            stringsAsFactors = FALSE
        )
        swe_at_basins$geometry <- base_data$swe_basins_shp$geometry[match(
            swe_at_basins$basin,
            base_data$swe_basins_shp$SWE_Basin
        )]
        swe_at_basins$annotation <- base_data$swe_basins_shp$annotation[match(
            swe_at_basins$basin,
            base_data$swe_basins_shp$SWE_Basin
        )]
        swe_at_basins$annotation <- paste0(
            swe_at_basins$annotation,
            "<br>(",
            round(swe_at_basins$relative_change, 1),
            "%)"
        )
        swe_at_basins <- sf::st_as_sf(
            swe_at_basins,
            crs = sf::st_crs(base_data$swe_basins_shp)
        )

        # Update basin points
        swe_at_basins_labels <- base_data$swe_at_basins_labels
        if (!is.null(swe_at_basins_labels)) {
            swe_at_basins_labels$annotation <- swe_at_basins$annotation[
                match(swe_at_basins_labels$SWE_Basin, swe_at_basins$basin)
            ]
        }

        # Process communities
        communities_sf <- base_data$communities_sf
        if (!is.null(communities_sf) && nrow(communities_sf) > 0) {
            communities_sf$basin <- NA_character_
            communities_sf$value <- NA_real_
            overlap_idx <- sf::st_intersects(communities_sf, swe_at_basins)

            for (i in seq_along(overlap_idx)) {
                if (length(overlap_idx[[i]]) > 0) {
                    basin_idx <- overlap_idx[[i]][1]
                    communities_sf$basin[i] <- swe_at_basins$basin[basin_idx]
                    communities_sf$value[i] <- swe_at_basins$relative_change[
                        basin_idx
                    ]
                }
            }
        }

        list(
            swe_at_pillows = swe_at_pillows,
            swe_at_surveys = swe_at_surveys,
            swe_at_basins = swe_at_basins,
            swe_at_basins_labels = swe_at_basins_labels,
            communities_sf = communities_sf
        )
    })

    # Initialize map - This creates the map as a server output
    output$map <- renderLeaflet({
        # Set default bounds to Yukon if available, otherwise use fallback
        if (!is.null(base_data$yukon_sf)) {
            bbox <- sf::st_bbox(base_data$yukon_sf)
        } else {
            # Fallback bounds for Yukon territory
            bbox <- c(xmin = -141, ymin = 60, xmax = -123, ymax = 69.6)
        }

        leaflet(
            options = leafletOptions(
                zoomDelta = 0.5,
                zoomSnap = 0.25,
                wheelPxPerZoomLevel = 120
            )
        ) %>%
            addProviderTiles(
                providers$USGS.USTopo,
                group = "USGS Imagery Topo"
            ) %>%
            fitBounds(
                as.numeric(bbox["xmin"]),
                as.numeric(bbox["ymin"]),
                as.numeric(bbox["xmax"]),
                as.numeric(bbox["ymax"])
            ) %>%
            {
                # Add inverted Yukon mask if available
                if (!is.null(base_data$inverted_yukon_sf)) {
                    addPolygons(
                        .,
                        data = base_data$inverted_yukon_sf,
                        fillColor = "#ffffff",
                        fillOpacity = 0.8,
                        color = NA,
                        weight = 0,
                        options = pathOptions(zIndex = 998)
                    )
                } else {
                    .
                }
            } %>%
            addLayersControl(
                overlayGroups = c(
                    "Basins averages",
                    "Snow pillow stations (continuous)",
                    "Snow surveys (discrete)",
                    "Communities",
                    "Roads",
                    "Territory Boundary"
                ),
                options = layersControlOptions(collapsed = FALSE),
                position = "topleft"
            )
    })

    # Update map when data changes - The sliders ARE connected and working
    observe({
        req(input$year, input$doy, input$value_type)
        data <- processed_data() # This uses input$year and input$doy

        # Create color palettes based on value type
        if (input$value_type == "relative") {
            station_pal <- colorBin(
                palette = station_colors,
                bins = station_bins,
                right = FALSE,
                domain = c(
                    if (nrow(data$swe_at_pillows) > 0) {
                        data$swe_at_pillows$value
                    } else {
                        NULL
                    },
                    if (nrow(data$swe_at_surveys) > 0) {
                        data$swe_at_surveys$value
                    } else {
                        NULL
                    }
                ),
                na.color = "gray"
            )

            swe_pal <- colorBin(
                palette = station_colors,
                bins = station_bins,
                domain = data$swe_at_basins$relative_change,
                na.color = "gray"
            )

            legend_title <- "Snow Water Equivalent<br>(Percent of Historic Median)"
        } else {
            station_pal <- colorBin(
                palette = absolute_colors,
                bins = absolute_bins,
                right = FALSE,
                domain = c(
                    if (nrow(data$swe_at_pillows) > 0) {
                        data$swe_at_pillows$value
                    } else {
                        NULL
                    },
                    if (nrow(data$swe_at_surveys) > 0) {
                        data$swe_at_surveys$value
                    } else {
                        NULL
                    }
                ),
                na.color = "gray"
            )

            swe_pal <- colorBin(
                palette = station_colors,
                bins = station_bins,
                domain = data$swe_at_basins$relative_change,
                na.color = "gray"
            )

            legend_title <- "Snow Water Equivalent<br>(mm)"
        }

        leafletProxy("map") %>%
            clearGroup("Basins averages") %>%
            clearGroup("Snow pillow stations (continuous)") %>%
            clearGroup("Snow surveys (discrete)") %>%
            clearGroup("Communities") %>%
            clearGroup("Roads") %>%
            clearControls() %>%

            # Add basins
            addPolygons(
                data = data$swe_at_basins,
                fillColor = ~ swe_pal(relative_change),
                color = "white",
                weight = 2,
                opacity = 1,
                fillOpacity = 0.7,
                label = ~ lapply(
                    paste0(
                        basin,
                        "<br>SWE ratio: ",
                        round(relative_change, 1),
                        "%"
                    ),
                    htmltools::HTML
                ),
                group = "Basins averages"
            ) %>%

            # Add basin labels
            {
                if (!is.null(data$swe_at_basins_labels)) {
                    addLabelOnlyMarkers(
                        .,
                        data = data$swe_at_basins_labels,
                        label = ~ lapply(annotation, htmltools::HTML),
                        group = "Basins averages",
                        labelOptions = labelOptions(
                            noHide = TRUE,
                            direction = "center",
                            textOnly = TRUE,
                            style = list(
                                "color" = "black",
                                "font-weight" = "bold",
                                "font-size" = "14px",
                                "text-shadow" = "-1px 0 0 #FFFFFF, 1px 0 0 #FFFFFF, 0 -1px 0 #FFFFFF, 0 1px 0 #FFFFFF",
                                "background" = "none",
                                "pointer-events" = "none",
                                "text-align" = "center"
                            )
                        )
                    )
                } else {
                    .
                }
            } %>%

            # Add stations
            {
                if (nrow(data$swe_at_pillows) > 0) {
                    addCircleMarkers(
                        .,
                        data = data$swe_at_pillows,
                        radius = 8,
                        color = "black",
                        fillColor = ~ station_pal(value),
                        weight = 2,
                        opacity = 1,
                        fillOpacity = 1,
                        popup = ~popup_content,
                        group = "Snow pillow stations (continuous)"
                    )
                } else {
                    .
                }
            } %>%
            {
                if (nrow(data$swe_at_surveys) > 0) {
                    addCircleMarkers(
                        .,
                        data = data$swe_at_surveys,
                        radius = 8,
                        color = "black",
                        fillColor = ~ station_pal(value),
                        weight = 2,
                        opacity = 1,
                        fillOpacity = 1,
                        popup = ~popup_content,
                        group = "Snow surveys (discrete)"
                    )
                } else {
                    .
                }
            } %>%

            # Add roads
            {
                if (
                    !is.null(base_data$roads_sf) && nrow(base_data$roads_sf) > 0
                ) {
                    addPolylines(
                        .,
                        data = base_data$roads_sf,
                        color = "darkred",
                        weight = 2,
                        opacity = 0.8,
                        group = "Roads",
                        options = pathOptions(zIndex = 400)
                    )
                } else {
                    .
                }
            } %>%

            # Add communities
            {
                if (
                    !is.null(data$communities_sf) &&
                        nrow(data$communities_sf) > 0
                ) {
                    addMarkers(
                        .,
                        data = data$communities_sf,
                        icon = makeIcon(
                            iconUrl = communities_icon_svg,
                            iconWidth = 16,
                            iconHeight = 16,
                            iconAnchorX = 8,
                            iconAnchorY = 8
                        ),
                        group = "Communities"
                    ) %>%
                        {
                            map_obj <- .
                            for (i in seq_len(nrow(data$communities_sf))) {
                                map_obj <- addLabelOnlyMarkers(
                                    map_obj,
                                    data = data$communities_sf[i, ],
                                    label = data$communities_sf$feature_name[i],
                                    group = "Communities",
                                    labelOptions = labelOptions(
                                        noHide = TRUE,
                                        direction = "top",
                                        textOnly = TRUE,
                                        style = list(
                                            "color" = "black",
                                            "font-weight" = "bold",
                                            "font-size" = "12px",
                                            "font-style" = "italic",
                                            "text-align" = "center",
                                            "margin-left" = ifelse(
                                                data$communities_sf$annotation_align[
                                                    i
                                                ] ==
                                                    "left",
                                                "20px",
                                                "-20px"
                                            ),
                                            "margin-right" = ifelse(
                                                data$communities_sf$annotation_align[
                                                    i
                                                ] ==
                                                    "right",
                                                "20px",
                                                "-20px"
                                            ),
                                            "text-shadow" = "none"
                                        )
                                    )
                                )
                            }
                            map_obj
                        }
                } else {
                    .
                }
            } %>%

            # Add legend
            addLegend(
                pal = station_pal,
                values = c(
                    if (nrow(data$swe_at_pillows) > 0) {
                        data$swe_at_pillows$value
                    } else {
                        NULL
                    },
                    if (nrow(data$swe_at_surveys) > 0) {
                        data$swe_at_surveys$value
                    } else {
                        NULL
                    }
                ),
                title = legend_title,
                position = "bottomleft"
            ) %>%

            # Add Yukon territory boundary on top of everything
            {
                if (!is.null(base_data$yukon_sf)) {
                    addPolygons(
                        .,
                        data = base_data$yukon_sf,
                        fill = FALSE,
                        color = "black",
                        weight = 3,
                        opacity = 1,
                        group = "Territory Boundary",
                        options = pathOptions(zIndex = 1000)
                    )
                } else {
                    .
                }
            }
    })

    # Reactive for plot generation on demand
    observeEvent(input$generate_plot, {
        req(input$generate_plot)

        plot_info <- input$generate_plot

        if (plot_info$type == "continuous") {
            plot_html <- create_continuous_plot_popup(
                plot_info$station_id,
                plot_info$station_name
            )
        } else {
            # Get discrete station data
            ts <- base_data$data_discrete$timeseries[[as.character(
                plot_info$station_id
            )]]
            if (!is.null(ts)) {
                ts$month <- format(ts$datetime, "%m")
                ts$year <- format(ts$datetime, "%Y")
                ts$units <- "mm"
                ts <- ts[, c("month", "year", "value", "units", "datetime")]
                plot_html <- create_discrete_plot_popup(
                    ts,
                    plot_info$station_name
                )
            } else {
                plot_html <- sprintf(
                    "<div style='text-align: center; padding: 20px;'>
                        <b>%s</b><br><br>No data available for this station.
                    </div>",
                    htmltools::htmlEscape(plot_info$station_name)
                )
            }
        }

        # Send plot HTML back to update popup
        session$sendCustomMessage("updatePopup", list(html = plot_html))
    })
}

# =============================================================================
# RUN SHINY APP
# =============================================================================

shiny::shinyApp(ui = ui, server = server)
