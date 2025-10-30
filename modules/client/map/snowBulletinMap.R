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
# Bins represent percentage of normal SWE (relative_swe values)
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
    "#e6f5ff", # Very light sky blue
    "#b3e2ff", # Light blue
    "#80c7e6", # Cyan/teal
    "#41b6c4", # Light blue (existing)
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
get_datetime <- function(year, month) {
    as.POSIXct(as.Date(paste0(year, "-", month, "-01")), tz = "UTC")
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
download_spatial_layer <- function(con, layer_name, additional_query = NULL) {
    # Assert that the requested layer_name exists in the database
    exists_query <- sprintf(
        "SELECT EXISTS (SELECT 1 FROM spatial.vectors WHERE layer_name = %s)",
        DBI::dbQuoteString(con, layer_name)
    )
    exists_res <- DBI::dbGetQuery(con, exists_query)
    if (nrow(exists_res) == 0 || !isTRUE(as.logical(exists_res[[1]]))) {
        stop(sprintf(
            "Layer '%s' not found in spatial.vectors table",
            layer_name
        ))
    }

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
download_continuous_ts <- function(
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
        return(list(
            timeseries = data.frame(datetime = as.POSIXct(character(0))),
            metadata = NULL
        ))
    }

    # Convert metadata to sf (WGS84)
    md_continuous <- sf::st_as_sf(
        md_continuous_df,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )

    ts_ids <- unique(md_continuous$timeseries_id)
    # Temporary list to hold per-station data.frames for latest-date calculation
    ts_list_temp <- vector("list", length(ts_ids))
    names(ts_list_temp) <- as.character(ts_ids)

    # Master timeseries dataframe (will contain datetime + one column per station)
    master_df <- NULL

    cat("Processing", length(ts_ids), "timeseries...\n")

    for (i in seq_along(ts_ids)) {
        ts_id <- ts_ids[i]

        # Retrieve measurements for this timeseries
        ts_query <- sprintf(
            "SELECT date, value FROM continuous.measurements_calculated_daily_corrected
             WHERE timeseries_id = %d AND value IS NOT NULL",
            as.integer(ts_id)
        )

        if (!is.null(start_date)) {
            ts_query <- paste0(
                ts_query,
                " AND date >= ",
                DBI::dbQuoteString(con, start_date)
            )
        }
        if (!is.null(end_date)) {
            ts_query <- paste0(
                ts_query,
                " AND date <= ",
                DBI::dbQuoteString(con, end_date)
            )
        }

        ts_data <- DBI::dbGetQuery(con, ts_query)

        if (nrow(ts_data) == 0) {
            # store empty df for latest-date processing
            ts_list_temp[[as.character(ts_id)]] <- data.frame(
                datetime = as.POSIXct(character(0)),
                value = numeric(0)
            )
            next
        }
        # Rename 'date' column to 'datetime'
        if ("date" %in% names(ts_data)) {
            names(ts_data)[names(ts_data) == "date"] <- "datetime"
        }
        # Parse datetimes and sort
        ts_data$datetime <- as.POSIXct(ts_data$datetime, tz = "UTC")
        ts_data <- ts_data[order(ts_data$datetime), , drop = FALSE]

        # Resample to daily or monthly as requested
        if (resolution == "daily") {
            ts_data$day <- as.Date(ts_data$datetime)
            ts_daily <- aggregate(
                value ~ day,
                data = ts_data,
                FUN = mean,
                na.rm = TRUE
            )
            names(ts_daily) <- c("datetime", "value")
            ts_daily$datetime <- as.POSIXct(ts_daily$datetime, tz = "UTC")
            ts_out <- ts_daily
        } else if (resolution == "monthly") {
            ts_data$month <- format(ts_data$datetime, "%Y-%m")
            ts_monthly <- aggregate(
                value ~ month,
                data = ts_data,
                FUN = mean,
                na.rm = TRUE
            )
            names(ts_monthly) <- c("datetime", "value")
            # set to first day of month
            ts_monthly$datetime <- as.POSIXct(
                paste0(ts_monthly$datetime, "-01"),
                tz = "UTC"
            )
            ts_out <- ts_monthly
        } else {
            warning(sprintf(
                "Unknown resolution '%s' for timeseries_id: %s",
                resolution,
                ts_id
            ))
            ts_out <- ts_data[, c("datetime", "value"), drop = FALSE]
        }

        # Keep a copy for latest-date calculation
        ts_list_temp[[as.character(ts_id)]] <- ts_out

        # Merge into master dataframe (wide format)
        # rename value column to station id
        station_col <- as.character(ts_id)
        ts_wide <- ts_out
        names(ts_wide)[names(ts_wide) == "value"] <- station_col

        if (is.null(master_df)) {
            master_df <- ts_wide
        } else {
            # base merge by datetime, keep all dates
            master_df <- merge(
                master_df,
                ts_wide,
                by = "datetime",
                all = TRUE,
                sort = TRUE
            )
        }
    }

    if (is.null(master_df)) {
        # No data found for any station
        master_df <- data.frame(datetime = as.POSIXct(character(0)))
    } else {
        # Ensure master_df sorted by datetime
        master_df <- master_df[order(master_df$datetime), , drop = FALSE]
        # convert columns other than datetime to numeric (they may be list factors)
        non_dt_cols <- setdiff(names(master_df), "datetime")
        for (col in non_dt_cols) {
            master_df[[col]] <- as.numeric(master_df[[col]])
        }
    }

    # Compute most recent non-NaN date per station using ts_list_temp
    station_latest_dates <- vapply(
        names(ts_list_temp),
        function(id) {
            ts <- ts_list_temp[[id]]
            get_most_recent_date(ts)
        },
        FUN.VALUE = as.POSIXct(NA)
    )

    station_means_df <- data.frame(
        timeseries_id = as.integer(names(station_latest_dates)),
        latest_date = as.POSIXct(
            station_latest_dates,
            origin = "1970-01-01",
            tz = "UTC"
        ),
        latest_observation = format(
            as.POSIXct(
                station_latest_dates,
                origin = "1970-01-01",
                tz = "UTC"
            ),
            "%Y-%m-%d"
        ),
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

    return(list(timeseries = list(swe = master_df), metadata = metadata_sf))
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
download_discrete_ts <- function(
    con,
    start_date = sprintf("%d-01-01", 1950),
    end_date = sprintf("%d-01-01", 3000)
) {
    # Build metadata query for discrete SWE timeseries
    md_discrete_df <- DBI::dbGetQuery(
        con,
        "SELECT DISTINCT
            l.location_id,
            l.latitude,
            l.longitude,
            l.location,
            l.name
         FROM discrete.samples s
         JOIN discrete.results r ON s.sample_id = r.sample_id
         JOIN public.locations l ON s.location_id = l.location_id
         WHERE r.parameter_id = (SELECT parameter_id FROM public.parameters
                                 WHERE param_name = 'snow water equivalent')"
    )

    if (nrow(md_discrete_df) == 0) {
        warning("No discrete SWE stations found")
        return(list(
            timeseries = list(
                swe = data.frame(datetime = as.POSIXct(character(0)))
            ),
            metadata = NULL
        ))
    }

    # Convert metadata to sf (WGS84)
    md_discrete <- sf::st_as_sf(
        md_discrete_df,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )

    ts_ids <- unique(md_discrete$location_id)
    ts_list_temp <- vector("list", length(ts_ids))
    names(ts_list_temp) <- as.character(ts_ids)

    master_df <- NULL

    cat("Processing", length(ts_ids), "discrete timeseries...\n")

    for (i in seq_along(ts_ids)) {
        loc_id <- ts_ids[i]

        ts_query <- sprintf(
            "SELECT s.datetime, s.target_datetime, r.result
             FROM discrete.samples s
             JOIN discrete.results r ON s.sample_id = r.sample_id
             WHERE s.location_id = %s
               AND r.parameter_id = (SELECT parameter_id FROM public.parameters
                                     WHERE param_name = 'snow water equivalent')
               AND r.result IS NOT NULL",
            DBI::dbQuoteLiteral(con, loc_id)
        )

        if (!is.null(start_date)) {
            ts_query <- paste0(
                ts_query,
                " AND (COALESCE(s.target_datetime, s.datetime) >= ",
                DBI::dbQuoteString(con, start_date),
                ")"
            )
        }
        if (!is.null(end_date)) {
            ts_query <- paste0(
                ts_query,
                " AND (COALESCE(s.target_datetime, s.datetime) <= ",
                DBI::dbQuoteString(con, end_date),
                ")"
            )
        }

        ts <- DBI::dbGetQuery(con, ts_query)

        if (nrow(ts) == 0) {
            ts_list_temp[[as.character(loc_id)]] <- data.frame(
                datetime = as.POSIXct(character(0)),
                value = numeric(0)
            )
            next
        }

        # prefer target_datetime when present
        if ("target_datetime" %in% names(ts)) {
            ts$target_datetime[is.na(ts$target_datetime)] <- ts$datetime[is.na(
                ts$target_datetime
            )]
            ts$datetime <- as.POSIXct(ts$target_datetime, tz = "UTC")
            ts$target_datetime <- NULL
        } else {
            ts$datetime <- as.POSIXct(ts$datetime, tz = "UTC")
        }

        # Convert datetime to just the date (day resolution)
        ts$datetime <- as.Date(ts$datetime)

        # rename result -> value
        if ("result" %in% names(ts)) {
            names(ts)[names(ts) == "result"] <- "value"
        }

        ts <- ts[order(ts$datetime), , drop = FALSE]

        # store temporary for latest-date calc
        ts_list_temp[[as.character(loc_id)]] <- ts[,
            c("datetime", "value"),
            drop = FALSE
        ]

        # merge into master_df (wide)
        station_col <- as.character(loc_id)
        ts_wide <- ts[, c("datetime", "value"), drop = FALSE]
        names(ts_wide)[names(ts_wide) == "value"] <- station_col

        if (is.null(master_df)) {
            master_df <- ts_wide
        } else {
            master_df <- merge(
                master_df,
                ts_wide,
                by = "datetime",
                all = TRUE,
                sort = TRUE
            )
        }
    }

    if (is.null(master_df)) {
        master_df <- data.frame(datetime = as.POSIXct(character(0)))
    } else {
        master_df <- master_df[order(master_df$datetime), , drop = FALSE]
        non_dt_cols <- setdiff(names(master_df), "datetime")
        for (col in non_dt_cols) {
            master_df[[col]] <- as.numeric(master_df[[col]])
        }
    }

    # Compute most recent non-NaN date per station using ts_list_temp
    station_latest_dates <- vapply(
        names(ts_list_temp),
        function(id) {
            ts <- ts_list_temp[[id]]
            get_most_recent_date(ts)
        },
        FUN.VALUE = as.POSIXct(NA)
    )

    station_means_df <- data.frame(
        location_id = as.integer(names(station_latest_dates)),
        latest_date = as.POSIXct(
            station_latest_dates,
            origin = "1970-01-01",
            tz = "UTC"
        ),
        latest_observation = format(
            as.POSIXct(
                station_latest_dates,
                origin = "1970-01-01",
                tz = "UTC"
            ),
            "%Y-%m-%d"
        ),
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

    metadata_sf <- sf::st_sf(
        merged_df,
        geometry = geom,
        crs = sf::st_crs(md_discrete)
    )

    return(list(timeseries = list(swe = master_df), metadata = metadata_sf))
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
    metadata_discrete
) {
    snowcourse_factors <- read.csv(
        system.file(
            "snow_survey/snowcourse_factors.csv",
            package = "YGwater"
        ),
        stringsAsFactors = FALSE
    )

    # Remove duplicate Hyland River value
    snowcourse_factors <- subset(snowcourse_factors, location != "10AD-SC01")
    # Drop location_name column
    snowcourse_factors <- snowcourse_factors[,
        !colnames(snowcourse_factors) %in% "location_name"
    ]

    if (!is.null(metadata_discrete)) {
        # Replace 'location' in snowcourse_factors with 'location_id' from metadata_discrete
        # Assume metadata_discrete has columns 'location' and 'location_id'
        if (
            "location" %in%
                colnames(snowcourse_factors) &&
                "location" %in% colnames(metadata_discrete) &&
                "location_id" %in% colnames(metadata_discrete)
        ) {
            snowcourse_factors <- merge(
                snowcourse_factors,
                metadata_discrete[, c("location", "location_id")],
                by = "location",
                all.x = TRUE,
                sort = FALSE
            )
            # Remove old 'location' column if desired, keep only 'location_id'
            snowcourse_factors$location <- NULL
        }
    }

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
#'   - relative_swe: current value as percentage of historic median
#' @details For each measurement, calculates the historic median for the same day
#'   of year using data from previous years within the lookback period. Day-of-year
#'   values are snapped to key dates (32, 60, 91, 121) if within 2 days.
calculate_historic_daily_median <- function(
    ts,
    lookback_length = NULL,
    lookback_year = NULL
) {
    # Default behaviour if neither provided
    if (is.null(lookback_year) && is.null(lookback_length)) {
        lookback_year <- 1980
    } else if (!is.null(lookback_year) && !is.null(lookback_length)) {
        stop("Specify either lookback_year or lookback_length, not both.")
    }

    # For each unique year/month, if no value exists exactly on the 1st, grab the nearest value within 5 days
    if ("datetime" %in% names(ts)) {
        ts$year <- as.integer(format(ts$datetime, "%Y"))
        ts$month <- as.integer(format(ts$datetime, "%m"))
        ts$day <- as.integer(format(ts$datetime, "%d"))

        # Only keep Feb-May
        ts <- ts[ts$month %in% 2:5, , drop = FALSE]

        # For each year/month, ensure a value exists for the 1st (or nearest within 5 days)
        keep_rows <- logical(nrow(ts))
        # Track which rows are snapped (not on the 1st)
        snapped_rows <- logical(nrow(ts))
        unique_ym <- unique(ts[, c("year", "month")])
        for (i in seq_len(nrow(unique_ym))) {
            y <- unique_ym$year[i]
            m <- unique_ym$month[i]
            idx <- which(ts$year == y & ts$month == m)
            # Prefer day==1
            idx1 <- idx[ts$day[idx] == 1]
            if (length(idx1) > 0) {
                keep_rows[idx1] <- TRUE
            } else {
                # Find nearest to 1st within 5 days
                days_from_first <- abs(ts$day[idx] - 1)
                min_diff <- min(days_from_first)
                if (min_diff <= 5) {
                    nearest_idx <- idx[which.min(days_from_first)]
                    keep_rows[nearest_idx] <- TRUE
                    snapped_rows[nearest_idx] <- TRUE
                }
            }
        }
        # Snap datetime to 1st of month for snapped rows
        if (any(snapped_rows)) {
            ts$datetime[snapped_rows] <- as.POSIXct(
                sprintf(
                    "%d-%02d-01",
                    ts$year[snapped_rows],
                    ts$month[snapped_rows]
                ),
                tz = "UTC"
            )
            ts$day[snapped_rows] <- 1
        }
        ts <- ts[keep_rows, , drop = FALSE]
        # Remove extra columns
        ts$year <- NULL
        ts$month <- NULL
        ts$day <- NULL
        ts$date_first_of_month <- NULL
        ts$date_diff_days <- NULL
    }

    # Wide timeseries: datetime + one column per station (stations as columns)
    if ("datetime" %in% names(ts)) {
        ts$datetime <- as.POSIXct(ts$datetime, tz = "UTC")
        n <- nrow(ts)
        day <- as.integer(format(ts$datetime, "%d"))
        month <- as.integer(format(ts$datetime, "%m"))
        year <- as.integer(format(ts$datetime, "%Y"))

        #doy <- snap_doy(doy)

        # station columns are everything except datetime (and any preexisting doy/year)
        station_cols <- setdiff(names(ts), c("datetime", "doy", "year"))
        # prepare output data.frames
        hist_df <- data.frame(datetime = ts$datetime, stringsAsFactors = FALSE)
        rel_df <- data.frame(datetime = ts$datetime, stringsAsFactors = FALSE)
        # Vectorized and grouped computation for speed
        p <- length(station_cols)
        if (p > 0) {
            vals_mat <- as.matrix(ts[, station_cols, drop = FALSE]) # n x p
            hist_mat <- matrix(NA_real_, nrow = n, ncol = p)
            rel_mat <- matrix(NA_real_, nrow = n, ncol = p)

            # Group by month-day (usually day==1 after snapping), compute per group
            grp <- paste(month, day, sep = "-")
            ug <- unique(grp)

            for (g in ug) {
                idx_g <- which(grp == g)
                # Ensure chronological order within group
                idx_g <- idx_g[order(year[idx_g])]

                # Precompute years vector for the group
                yg <- year[idx_g]

                for (j_idx in seq_along(idx_g)) {
                    j <- idx_g[j_idx]

                    if (!is.null(lookback_year)) {
                        prev_idx <- idx_g[which(
                            yg < yg[j_idx] & yg >= lookback_year
                        )]
                    } else {
                        prev_idx <- idx_g[which(
                            yg < yg[j_idx] & yg >= (yg[j_idx] - lookback_length)
                        )]
                    }

                    if (length(prev_idx) > 0) {
                        prev_block <- vals_mat[prev_idx, , drop = FALSE]
                        # Fast column medians when matrixStats is available
                        if (requireNamespace("matrixStats", quietly = TRUE)) {
                            hist_mat[j, ] <- matrixStats::colMedians(
                                prev_block,
                                na.rm = TRUE
                            )
                        } else {
                            hist_mat[j, ] <- apply(
                                prev_block,
                                2,
                                stats::median,
                                na.rm = TRUE
                            )
                        }
                    }
                }
            }

            # Compute relative SWE in a fully vectorized way
            cur <- vals_mat
            hist <- hist_mat

            # Case 1: standard percentage where historic median != 0
            mask1 <- !is.na(hist) & !is.na(cur) & (hist != 0)
            rel_mat[mask1] <- 100 * cur[mask1] / hist[mask1]

            # Case 2: both zero => -2
            mask2 <- !is.na(cur) & (cur == 0) & !is.na(hist) & (hist == 0)
            rel_mat[mask2] <- -2

            # Case 3: current > 0, historic == 0 => -1
            mask3 <- !is.na(cur) & (cur > 0) & !is.na(hist) & (hist == 0)
            rel_mat[mask3] <- -1

            colnames(hist_mat) <- station_cols
            colnames(rel_mat) <- station_cols

            hist_df[station_cols] <- as.data.frame(
                hist_mat,
                stringsAsFactors = FALSE
            )
            rel_df[station_cols] <- as.data.frame(
                rel_mat,
                stringsAsFactors = FALSE
            )
        }

        # ensure ordering by datetime
        hist_df <- hist_df[order(hist_df$datetime), , drop = FALSE]
        rel_df <- rel_df[order(rel_df$datetime), , drop = FALSE]

        return(list(
            historic_median = hist_df,
            relative_swe = rel_df
        ))
    }

    stop(
        "Input timeseries must contain a 'datetime' column (and either 'value' for single-station or station columns for wide format)."
    )
}

#' Extract data at specific points for a given year and day-of-year
#'
#' Modified to work with reorganized timeseries structure where data is organized
#' by parameter type (absolute_swe, relative_swe, historic_median) with
#' stations as columns rather than by station with parameters as columns.
#'
#' @param data list containing timeseries and metadata from get_*_timeseries functions
#' @param year integer target year for data extraction
#' @param month integer target month for data extraction
#' @param key character name of the key column in metadata (e.g., "timeseries_id")
#' @param cutoff numeric maximum years between target date and latest available data
#' @return data.frame subset of metadata with additional value columns
#' @details Filters stations to only include those with recent data (within cutoff years),
#'   then extracts values for the target date from the reorganized timeseries structure.
get_swe_state <- function(
    data,
    year,
    month,
    key,
    cutoff = 5
) {
    # Assert that data contains timeseries and metadata
    stopifnot(is.list(data))
    stopifnot("timeseries" %in% names(data))
    stopifnot("metadata" %in% names(data))
    stopifnot(all(
        c("swe", "historic_median", "relative_swe") %in%
            names(data$timeseries)
    ))
    point_source_data <- data$metadata
    target_date <- get_datetime(year, month)

    # Validate that key column exists in metadata
    stopifnot(key %in% colnames(point_source_data))

    # Helper to extract value for each station at the closest matching date
    extract_at_date <- function(ts_df, col_key, target_date) {
        vals <- rep(NA_real_, nrow(point_source_data))
        if (!is.null(ts_df) && "datetime" %in% names(ts_df)) {
            for (i in seq_len(nrow(point_source_data))) {
                station_id <- as.character(point_source_data[[col_key]][i])
                if (!is.null(station_id) && station_id %in% names(ts_df)) {
                    # Find row in ts_df with closest date to target_date
                    dt <- as.Date(ts_df$datetime)
                    idx <- which(dt == as.Date(target_date))
                    if (length(idx) == 1) {
                        vals[i] <- as.numeric(ts_df[[station_id]][idx])
                    }
                }
            }
        }
        vals
    }

    point_source_data$swe <- extract_at_date(
        data$timeseries$swe,
        key,
        target_date
    )
    point_source_data$relative_swe <- extract_at_date(
        data$timeseries$relative_swe,
        key,
        target_date
    )
    point_source_data$historic_median <- extract_at_date(
        data$timeseries$historic_median,
        key,
        target_date
    )

    return(point_source_data)

    # For backward compatibility, also set $value to match selected value_type if needed
    # (optional, depending on how downstream code expects it)
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
create_continuous_plot_popup <- function(timeseries, year) {
    station_name <- names(timeseries)[2]
    names(timeseries) <- c("datetime", "value")

    timeseries <- timeseries[
        !is.na(timeseries$value) & !is.nan(timeseries$value),
    ]
    plot <- ggplotOverlap(
        parameter = "Snow Water Equivalent",
        startDay = 240,
        endDay = 239,
        years = c(year - 1),
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
        continuous_data = timeseries
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
create_discrete_plot_popup <- function(timeseries) {
    # Clean and validate the data before plotting
    station_name <- names(timeseries)[2]
    names(timeseries) <- c("datetime", "value")

    timeseries$month <- as.integer(format(timeseries$datetime, "%m"))
    timeseries$year <- as.integer(format(timeseries$datetime, "%Y"))
    timeseries$units <- "mm"

    plot <- hydrometDiscrete(
        parameter = "SWE",
        location = station_name,
        startDay = 1,
        discrete_data = timeseries,
        plot_type = "linedbox"
    )

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
#' Modified to reorganize timeseries data by parameter type with stations as columns
#' and to precompute basin-averaged SWE timeseries at each timestep.
#'
#' @return list containing all loaded base data including reorganized timeseries, spatial layers, etc.
#' @details This function loads all static data and reorganizes timeseries structure
#'   for more efficient access patterns in the application. Basin-averaged SWE is
#'   calculated for each timestep using discrete station data and weighting factors.
load_base_data <- function(con, con2) {
    cat("Loading base data...\n")

    # Initialize base data list
    base_data <- list(
        pillows = list(),
        surveys = list(),
        basins = list(),
        shapefiles = list()
    )

    cat("Loading timeseries data...\n")

    # Load timeseries data from both continuous and discrete sources

    continuous_data <- download_continuous_ts(con, start_date, end_date)

    ret <- calculate_historic_daily_median(
        continuous_data$timeseries$swe,
        lookback_year = 1980
    )
    continuous_data$timeseries$historic_median <- ret$historic_median
    continuous_data$timeseries$relative_swe <- ret$relative_swe
    base_data$pillows <- continuous_data

    discrete_data <- download_discrete_ts(con, start_date, end_date)

    ret <- calculate_historic_daily_median(
        discrete_data$timeseries$swe,
        lookback_year = 1980
    )
    discrete_data$timeseries$historic_median <- ret$historic_median
    discrete_data$timeseries$relative_swe <- ret$relative_swe
    base_data$surveys <- discrete_data

    # Ensure discrete SWE wide timeseries is available

    # Load or infer weight matrix from snowcourse factors CSV using discrete metadata
    weights_df <- load_snowcourse_factors(
        metadata_discrete = discrete_data$metadata
    )

    # If we can read basin polygons from shapefile now (so basin names exist), prefer them
    basins_shp <- sf::st_read(
        system.file(
            "snow_survey/swe_basins/swe_basins.shp",
            package = "YGwater",
            mustWork = TRUE
        ),
        quiet = TRUE
    )

    if ("SWE_Basin" %in% names(basins_shp)) {
        names(basins_shp)[names(basins_shp) == "SWE_Basin"] <- "name"
    }

    # Ensure CRS is WGS84 for leaflet plotting
    if (sf::st_crs(basins_shp)$epsg != 4326) {
        basins_shp <- sf::st_transform(basins_shp, 4326)
    }

    # Prepare dates and station list from discrete wide timeseries
    basin_dates <- base_data$surveys$timeseries$swe$datetime

    # Convert station_cols from location_id to location (name) using discrete metadata
    basin_names <- basins_shp$name
    basin_swe_mat <- matrix(
        NA_real_,
        nrow = length(basin_dates),
        ncol = length(basin_names)
    )

    for (i in seq_along(basin_dates)) {
        basin_swe_mat[i, ] <- NA_real_

        weight_matrix <- weights_df[,
            c("location_id", basin_names),
            drop = FALSE
        ]

        swe_samples <- as.numeric(discrete_data$timeseries$swe[
            i,
            as.character(weight_matrix$location_id),
            drop = TRUE
        ])

        nan_samples <- is.na(swe_samples)

        swe_samples <- swe_samples[!nan_samples]
        weight_matrix <- weight_matrix[!nan_samples, ]
        # Drop location_id column before normalization and multiplication
        weight_matrix <- weight_matrix[, -1, drop = FALSE]

        # Normalize columns so that each basin's weights sum to 1
        weight_matrix <- sweep(
            weight_matrix,
            2,
            colSums(weight_matrix, na.rm = TRUE),
            FUN = "/"
        )

        # Remove the location_id column for multiplication
        basin_vals <- as.numeric(
            t(matrix(swe_samples, ncol = 1)) %*% as.matrix(weight_matrix)
        )

        basin_swe_mat[i, ] <- basin_vals
    }

    # Build timeseries data.frame (datetime + basin columns)
    basin_timeseries <- data.frame(
        datetime = basin_dates,
        stringsAsFactors = FALSE
    )

    basin_timeseries[, basin_names] <- NA_real_
    basin_timeseries[, basin_names] <- basin_swe_mat

    # Compute historic median and relative change for basins (wide format)
    ret <- calculate_historic_daily_median(
        basin_timeseries,
        lookback_year = 1980
    )

    base_data$basins$timeseries <- list(
        swe = basin_timeseries,
        historic_median = ret$historic_median,
        relative_swe = ret$relative_swe
    )

    # Load adjusted basin centroids from GeoJSON
    centroids <- sf::st_read(
        system.file(
            "snow_survey/swe_basin_labels.geojson",
            package = "YGwater",
            mustWork = TRUE
        ),
        quiet = TRUE
    )

    # Ensure CRS is WGS84 for leaflet
    if (sf::st_crs(centroids)$epsg != 4326) {
        centroids <- sf::st_transform(centroids, 4326)
    }
    base_data$basins$centroids <- centroids

    prov_sf <- download_spatial_layer(
        con2,
        "Provincial/Territorial Boundaries",
        additional_query = "AND feature_name = 'Yukon'"
    )
    # Ensure CRS is WGS84 for leaflet
    if (!is.null(prov_sf) && sf::st_crs(prov_sf)$epsg != 4326) {
        prov_sf <- sf::st_transform(prov_sf, 4326)
    }
    base_data$shapefiles$yukon <- prov_sf

    bbox_extent <- sf::st_as_sfc(sf::st_bbox(
        c(xmin = -170, xmax = -90, ymin = 50, ymax = 90),
        crs = 4326
    ))
    # Ensure CRS match for st_difference
    if (
        !is.null(base_data$shapefiles$yukon) &&
            sf::st_crs(bbox_extent) != sf::st_crs(base_data$shapefiles$yukon)
    ) {
        base_data$shapefiles$yukon <- sf::st_transform(
            base_data$shapefiles$yukon,
            sf::st_crs(bbox_extent)
        )
    }
    base_data$shapefiles$inverted_yukon <- sf::st_difference(
        bbox_extent,
        sf::st_union(base_data$shapefiles$yukon)
    )

    cat("Loading communities...\n")
    place_types <- c("City", "Town", "Village") # Filter to major communities only
    communities <- download_spatial_layer(
        con2,
        "Communities",
        additional_query = sprintf(
            "AND (description IN (%s) OR feature_name = 'Old Crow')",
            paste(
                sapply(
                    place_types,
                    function(x) DBI::dbQuoteString(con2, x)
                ),
                collapse = ", "
            )
        )
    )
    # Ensure CRS is WGS84 for leaflet
    if (!is.null(communities) && sf::st_crs(communities)$epsg != 4326) {
        communities <- sf::st_transform(communities, 4326)
    }
    base_data$shapefiles$communities <- communities

    cat("Loading roads...\n")
    road_types <- c("Primary Highway", "Secondary Highway") # Major roads only
    roads <- download_spatial_layer(
        con2,
        "Roads",
        additional_query = sprintf(
            "AND Description IN (%s)",
            paste(
                sapply(
                    road_types,
                    function(x) DBI::dbQuoteString(con2, x)
                ),
                collapse = ", "
            )
        )
    )
    # Ensure CRS is WGS84 for leaflet
    if (!is.null(roads) && sf::st_crs(roads)$epsg != 4326) {
        roads <- sf::st_transform(roads, 4326)
    }
    base_data$shapefiles$roads <- roads

    # Process basin names for better display on map

    base_data$basins$metadata <- basins_shp
    base_data$basins$metadata$annotation <- base_data$basins$metadata$Label
    base_data$basins$metadata$annotation <- gsub(
        "_",
        " ",
        as.character(base_data$basins$metadata$annotation)
    )
    base_data$basins$metadata$annotation <- vapply(
        base_data$basins$metadata$annotation,
        FUN.VALUE = character(1),
        FUN = function(x) {
            if (is.na(x) || x == "") {
                return(x)
            }
            # Add line break after first word if it's 5+ characters
            sub("^(\\S{5,})\\s+", "\\1<br>", x, perl = TRUE)
        }
    )

    #base_data$basins$metadata$latest_observation <- format(
    #    Sys.Date(),
    #    "%Y-%m-%d"
    #)

    return(base_data)
}

# =============================================================================
# DATA LOADING AND PROCESSING
# =============================================================================

# Load all base data once at startup

# =============================================================================
# SHINY APP UI
# =============================================================================

mapSnowbullUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        # Main container with full viewport
        div(
            style = "height: 100vh; width: 100vw; position: relative; margin: 0; padding: 0;",
            leafletOutput(ns("map"), width = "100vw", height = "100vh"),
            # Floating controls
            div(
                style = "
          position: absolute; top: 10px; left: 50%; transform: translateX(-50%);
          z-index: 1000; background: rgba(255,255,255,0.95); border: 1px solid #ccc;
          border-radius: 5px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); min-width: 400px;
          padding: 10px 20px; display: flex; gap: 20px; align-items: center;",
                # Year slider
                div(
                    style = "display: flex; align-items: center; gap: 5px;",
                    span("Year:"),
                    sliderInput(
                        ns("year"),
                        label = NULL,
                        min = 2010,
                        max = 2025,
                        value = 2025,
                        step = 1,
                        sep = ""
                    )
                ),
                # Month select
                div(
                    style = "display: flex; align-items: center; gap: 5px;",
                    span("Month:"),
                    selectInput(
                        ns("month"),
                        label = NULL,
                        choices = setNames(
                            as.character(c(2, 3, 4, 5)),
                            c("February", "March", "April", "May")
                        ),
                        selected = "3",
                        width = "100px"
                    )
                ),
                # Value type radio
                div(
                    style = "display: flex; align-items: center; gap: 5px;",
                    span("Values:"),
                    radioButtons(
                        ns("value_type"),
                        label = NULL,
                        choices = list(
                            "% of Normal" = "relative_swe",
                            "Absolute (mm)" = "swe"
                        ),
                        selected = "relative_swe",
                        inline = TRUE
                    )
                )
            )
        ),
        # JS for plot generation
        tags$script(HTML(sprintf(
            "
        function generatePlot(type, stationId, stationName) {
          var popup = document.querySelector('.leaflet-popup-content');
          if (popup) {
            popup.innerHTML = '<div style=\"text-align: center; padding: 20px;\"><b>' + 
              stationName + '</b><br><br>Loading plot...<br>' + 
              '<div style=\"margin-top: 10px;\">⏳</div></div>';
          }
          Shiny.setInputValue('%s', {
            type: type,
            station_id: stationId,
            station_name: stationName,
            timestamp: Date.now()
          });
        }
      ",
            ns("generate_plot")
        )))
    )
}


mapSnowbull <- function(id, language) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Server setup ####

        base_data <- load_base_data(con, con2)
        print("Base data loaded successfully")
        print(sprintf(
            "Number of pillows: %d",
            nrow(base_data$pillows$metadata)
        ))
        print(sprintf(
            "Number of surveys: %d",
            nrow(base_data$surveys$metadata)
        ))
        print(sprintf("Number of basins: %d", nrow(base_data$basins$metadata)))

        # Render UI elements ####
        # (placeholder for now)
        # ---- End color palette functions ----
        # Reactive expression for selected date display
        output$selected_date <- renderText({
            req(input$year, input$month)
            # Convert DOY from character to numeric
            date_obj <- get_datetime(input$year, input$month)
            format(date_obj, "%B %d, %Y")
        })

        # Reactive data processing based on user inputs
        processed_data <- reactive({
            print(sprintf(
                "Processing data for year: %d, month: %s",
                input$year,
                input$month
            ))

            req(input$year, input$month)

            # Extract data at points for the selected date
            swe_at_basins <- get_swe_state(
                data = base_data$basins,
                year = input$year,
                month = input$month,
                key = "name",
                cutoff = record_cutoff_years
            )
            print(sprintf(
                "Processed basins: %d with data",
                sum(!is.na(swe_at_basins$swe))
            ))

            swe_at_surveys <- get_swe_state(
                data = base_data$surveys,
                year = input$year,
                month = input$month,
                key = "location_id",
                cutoff = record_cutoff_years
            )

            swe_at_pillows <- get_swe_state(
                data = base_data$pillows,
                year = input$year,
                month = input$month,
                key = "timeseries_id",
                cutoff = record_cutoff_years
            )

            # Ensure data is not empty before proceeding
            if (is.null(swe_at_basins)) {
                stop("Basin data is empty.")
            }

            if (is.null(swe_at_surveys)) {
                stop("Survey data is empty.")
            }

            if (is.null(swe_at_pillows)) {
                stop("Pillow data is empty.")
            }

            # Filter out stations/basins with stale data (latest_date > cutoff years from selected date)
            input_date <- get_datetime(input$year, input$month)

            # Helper function to filter by latest_date within cutoff
            filter_by_latest_date <- function(df, input_date, cutoff_days) {
                df[
                    is.na(df$latest_date) |
                        as.numeric(difftime(
                            input_date,
                            df$latest_date,
                            units = "days"
                        )) <=
                            cutoff_days,
                    ,
                    drop = FALSE
                ]
            }
            #swe_at_basins <- filter_by_latest_date(
            #    swe_at_basins,
            #    input_date,
            #    366
            #)
            swe_at_surveys <- filter_by_latest_date(
                swe_at_surveys,
                input_date,
                366
            )
            swe_at_pillows <- filter_by_latest_date(
                swe_at_pillows,
                input_date,
                366
            )

            swe_at_basins$annotation <- paste0(
                swe_at_basins$annotation,
                "<br>(",
                round(swe_at_basins$relative_swe, 1),
                "%)"
            )
            # Consolidated popup generation function
            generate_popup_content <- function(
                type,
                swe,
                relative_swe,
                historic_median,
                name,
                location = NULL,
                id = NULL
            ) {
                type_label <- switch(
                    type,
                    "basin" = "",
                    "survey" = "<b>Type:</b> Discrete (Snow Course)<br>",
                    "pillow" = "<b>Type:</b> Continuous (Pillow)<br>",
                    ""
                )

                # Add generate plot button if applicable
                plot_button <- if (!is.null(id)) {
                    sprintf(
                        "<button onclick='generatePlot(\"%s\", \"%s\", \"%s\")' style='margin-top: 10px;'>Generate Plot</button>",
                        type,
                        id,
                        name
                    )
                } else {
                    ""
                }

                location_html <- if (!is.null(location)) {
                    paste0(
                        "<span style='font-size: 12px; color: #666;'>",
                        location,
                        "</span><br><br>"
                    )
                } else {
                    "<br>"
                }

                paste0(
                    "<div style='text-align: left; padding: 10px; width: 300px;'>",
                    "<b style='font-size: 16px;'>",
                    name,
                    "</b><br>",
                    location_html,
                    "<b>SWE Value (mm):</b> ",
                    if (!is.na(swe)) {
                        paste0(round(swe, 1), " mm")
                    } else {
                        "No data"
                    },
                    "<br>",
                    "<b>SWE Value (%):</b> ",
                    if (!is.na(relative_swe)) {
                        paste0(round(relative_swe, 1), "% of normal")
                    } else {
                        "No data"
                    },
                    "<br>",
                    "<b>Historic Median:</b> ",
                    if (!is.na(historic_median)) {
                        paste0(round(historic_median, 1), " mm")
                    } else {
                        "No data"
                    },
                    "<br>",
                    type_label,
                    plot_button,
                    "</div>"
                )
            }

            # Create popup content for basins
            swe_at_basins$popup_content <- mapply(
                function(swe, relative_swe, historic_median, name) {
                    generate_popup_content(
                        "basin",
                        swe,
                        relative_swe,
                        historic_median,
                        name,
                        location = NULL,
                        id = name # Use basin name as ID
                    )
                },
                swe_at_basins$swe,
                swe_at_basins$relative_swe,
                swe_at_basins$historic_median,
                swe_at_basins$name,
                SIMPLIFY = FALSE
            )

            # Create popup content for surveys with ID
            swe_at_surveys$popup_content <- mapply(
                function(
                    swe,
                    relative_swe,
                    historic_median,
                    name,
                    location,
                    id
                ) {
                    generate_popup_content(
                        "survey",
                        swe,
                        relative_swe,
                        historic_median,
                        name,
                        location,
                        id
                    )
                },
                swe_at_surveys$swe,
                swe_at_surveys$relative_swe,
                swe_at_surveys$historic_median,
                swe_at_surveys$name,
                swe_at_surveys$location,
                swe_at_surveys$location_id,
                SIMPLIFY = FALSE
            )

            # Create popup content for pillows with ID
            swe_at_pillows$popup_content <- mapply(
                function(
                    swe,
                    relative_swe,
                    historic_median,
                    name,
                    location,
                    id
                ) {
                    generate_popup_content(
                        "pillow",
                        swe,
                        relative_swe,
                        historic_median,
                        name,
                        location,
                        id
                    )
                },
                swe_at_pillows$swe,
                swe_at_pillows$relative_swe,
                swe_at_pillows$historic_median,
                swe_at_pillows$name,
                swe_at_pillows$location,
                swe_at_pillows$timeseries_id,
                SIMPLIFY = FALSE
            )

            list(
                swe_at_basins = swe_at_basins,
                swe_at_surveys = swe_at_surveys,
                swe_at_pillows = swe_at_pillows
            )
        })

        # Initialize map - This creates the map as a server output
        output[[ns("map")]] <- renderLeaflet({
            # Set default bounds to Yukon if available, otherwise use fallback
            if (!is.null(base_data$shapefiles$yukon)) {
                bbox <- sf::st_bbox(base_data$shapefiles$yukon)
            } else {
                bbox <- c(xmin = -141, ymin = 60, xmax = -123, ymax = 69.6)
            }

            leaflet::leaflet(
                options = leaflet::leafletOptions(
                    zoomDelta = 0.5,
                    zoomSnap = 0.25,
                    wheelPxPerZoomLevel = 120
                )
            ) %>%
                leaflet::addProviderTiles(
                    leaflet::providers$Esri.WorldTopoMap,
                    group = "Topographic"
                ) %>%
                leaflet::fitBounds(
                    as.numeric(bbox["xmin"]),
                    as.numeric(bbox["ymin"]),
                    as.numeric(bbox["xmax"]),
                    as.numeric(bbox["ymax"])
                ) %>%
                {
                    if (!is.null(base_data$shapefiles$yukon)) {
                        leaflet::addPolygons(
                            .,
                            data = base_data$shapefiles$yukon,
                            color = "#222222",
                            weight = 3,
                            fill = FALSE,
                            group = "Boundary"
                        )
                    } else {
                        .
                    }
                } %>%
                {
                    if (!is.null(base_data$shapefiles$roads)) {
                        leaflet::addPolylines(
                            .,
                            data = base_data$shapefiles$roads,
                            color = "#8B0000",
                            weight = 2,
                            opacity = 0.8,
                            group = "Roads"
                        )
                    } else {
                        .
                    }
                } %>%
                {
                    if (!is.null(base_data$shapefiles$communities)) {
                        leaflet::addMarkers(
                            .,
                            data = base_data$shapefiles$communities,
                            icon = leaflet::icons(
                                iconUrl = communities_icon_svg,
                                iconWidth = 16,
                                iconHeight = 16
                            ),
                            group = "Communities"
                        )
                    } else {
                        .
                    }
                } %>%
                leaflet::addLayersControl(
                    baseGroups = "Topographic",
                    overlayGroups = c("Boundary", "Roads", "Communities"),
                    options = leaflet::layersControlOptions(collapsed = TRUE)
                ) %>%
                leaflet::hideGroup("Roads")
        })

        # Update map when data changes
        observe({
            print(sprintf(
                "Updating map - Year: %d, Month: %s, Value type: %s",
                input$year,
                input$month,
                input$value_type
            ))

            req(input$year, input$month, input$value_type)
            data <- processed_data()

            print(sprintf(
                "Map update - Basins: %d, Surveys: %d, Pillows: %d",
                nrow(data$swe_at_basins),
                nrow(data$swe_at_surveys),
                nrow(data$swe_at_pillows)
            ))

            # Select value column based on input$value_type
            value_col <- if (input$value_type == "relative_swe") {
                "relative_swe"
            } else {
                "swe"
            }

            # Create appropriate color palette based on value type
            if (input$value_type == "relative_swe") {
                swe_pal <- colorBin(
                    palette = station_colors,
                    bins = station_bins,
                    domain = c(
                        data$swe_at_basins[[value_col]],
                        data$swe_at_surveys[[value_col]],
                        data$swe_at_pillows[[value_col]]
                    ),
                    na.color = "gray"
                )
            } else {
                swe_pal <- colorBin(
                    palette = absolute_colors,
                    bins = absolute_bins,
                    domain = c(
                        data$swe_at_basins[[value_col]],
                        data$swe_at_surveys[[value_col]],
                        data$swe_at_pillows[[value_col]]
                    ),
                    na.color = "gray"
                )
            }

            leafletProxy("map") %>%
                clearGroup("Basins averages") %>%
                clearGroup("Snow surveys (discrete)") %>%
                clearGroup("Snow pillows (continuous)") %>%
                addPolygons(
                    data = data$swe_at_basins,
                    fillColor = ~ swe_pal(get(value_col)),
                    color = "white",
                    weight = 2,
                    opacity = 1,
                    fillOpacity = 0.7,
                    label = ~ lapply(
                        paste0(
                            name,
                            "<br>SWE: ",
                            if (input$value_type == "relative_swe") {
                                paste0(round(get(value_col), 1), "% of normal")
                            } else {
                                paste0(round(get(value_col), 1), " mm")
                            }
                        ),
                        htmltools::HTML
                    ),
                    popup = ~ lapply(popup_content, htmltools::HTML),
                    group = "Basins averages"
                ) %>%
                addCircleMarkers(
                    data = data$swe_at_surveys,
                    radius = 8,
                    color = "black",
                    fillColor = ~ swe_pal(get(value_col)),
                    weight = 2,
                    opacity = 1,
                    fillOpacity = 1,
                    label = ~ lapply(
                        paste0(name, "<br>", location),
                        htmltools::HTML
                    ),
                    popup = ~ lapply(popup_content, htmltools::HTML),
                    group = "Snow surveys (discrete)"
                ) %>%
                addCircleMarkers(
                    data = data$swe_at_pillows,
                    radius = 8,
                    color = "blue",
                    fillColor = ~ swe_pal(get(value_col)),
                    weight = 2,
                    opacity = 1,
                    fillOpacity = 1,
                    label = ~ lapply(
                        paste0(name, "<br>", location),
                        htmltools::HTML
                    ),
                    popup = ~ lapply(popup_content, htmltools::HTML),
                    group = "Snow pillows (continuous)"
                ) %>%
                # Clear and update legend based on selected value type
                clearControls()
            # %>%
            # addLegend(
            #   position = "bottomleft",
            #   title = if (input$value_type == "relative_swe") {
            #     "SWE (% of Normal)"
            #   } else {
            #     "SWE (mm)"
            #   },
            #   pal = swe_pal,
            #   values = if (input$value_type == "relative_swe") {
            #     station_bins[-length(station_bins)]
            #   } else {
            #     absolute_bins[-length(absolute_bins)]
            #   },
            #   bins = if (input$value_type == "relative_swe") {
            #     station_bins
            #   } else {
            #     absolute_bins
            #   },
            #   labels = if (input$value_type == "relative_swe") {
            #     c(
            #       "No data",
            #       "Below Historic",
            #       "Near Historic",
            #       "50-70%",
            #       "70-90%",
            #       "90-110%",
            #       "110-130%",
            #       "130-150%",
            #       ">150%"
            #     )
            #   } else {
            #     c(
            #       "0-50",
            #       "50-100",
            #       "100-150",
            #       "150-200",
            #       "200-250",
            #       "250-300",
            #       "300-400",
            #       "400-500",
            #       ">500"
            #     )
            #   },
            #   opacity = 0.7
            # )
        })

        # Add observer for plot generation requests
        observeEvent(input$generate_plot, {
            print(sprintf(
                "Generating plot - Type: %s, Station: %s",
                input$generate_plot$type,
                input$generate_plot$station_name
            ))

            req(
                input$generate_plot$type,
                input$generate_plot$station_id,
                input$generate_plot$station_name
            )

            plot_html <- if (input$generate_plot$type == "pillow") {
                create_continuous_plot_popup(
                    timeseries = base_data$pillows$timeseries$swe[, c(
                        "datetime",
                        as.character(input$generate_plot$station_id)
                    )],
                    year = input$year
                )
            } else if (input$generate_plot$type == "survey") {
                create_discrete_plot_popup(
                    timeseries = base_data$surveys$timeseries$swe[, c(
                        "datetime",
                        as.character(input$generate_plot$station_id)
                    )]
                )
            } else if (input$generate_plot$type == "basin") {
                # Get basin data and use discrete plot style
                create_discrete_plot_popup(
                    timeseries = base_data$basins$timeseries$swe[, c(
                        "datetime",
                        input$generate_plot$station_id
                    )]
                )
            }

            # Send the plot HTML back to update the popup
            session$sendCustomMessage("updatePopup", list(html = plot_html))
            print("Plot generated successfully")
        })
    }) # End of moduleServer
} # End of mapLocs function
