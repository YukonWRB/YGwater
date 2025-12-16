# ============================================================================
#
# SWE bulletin utilities for loading data and setting up map visualizations
# Date created: Nov 2025
# Author: esnieder

# Main mapping function (date, parameter, statistic)
# -- 'load_snowbull_shapefiles' Load static spatial layers from database
# -- 'load_snowbull_timeseries' Load hydromet timeseries data from database
# --
# -- 'get_display_data' Process timeseries for selected date, parameter, and statistic
# ---- 'get_state_as_shp' Get shapefile with data fields for selected date (all statistics, to create popups)

# ===========================================================================

#' Create string of month names
#'
#' @description
#' Generates a character vector of month names or abbreviations.
#'
#' @param month Optional integer vector of month numbers (1-12). If NULL, returns all months.
#' @param short Logical indicating whether to return abbreviated month names (TRUE) or full names (FALSE). Default is FALSE.
#'
#' @return A character vector of month names or abbreviations
#'
#' @details
#' The relative SWE bins are designed to highlight significant departures from normal
#'
#' @noRd
#'
snowbull_months <- function(month = NULL, short = FALSE) {
    months = c(
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
    )
    if (short) {
        months = tolower(substr(months, 1, 3))
    }

    if (!is.null(month)) {
        months = months[month]
    }

    return(months)
}

#' Initialize visualization parameters
#'
#' @description
#' Sets up color schemes, bins, and other visualization parameters used across
#' both leaflet and ggplot2 mapping functions.
#'
#' @return A nested list containing all plotting options for basins, stations, roads, boundaries, labels, etc.
#'
#' @details
#' The relative SWE bins are designed to highlight significant departures from normal
#'
#' @noRd

get_static_style_elements <- function() {
    # SVG icon for communities
    communities_icon_svg <- "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'><polygon points='8,0 16,8 8,16 0,8' fill='black' stroke='white' stroke-width='2'/></svg>"
    pillows_icon_svg <- "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'><polygon points='8,2 14,8 8,14 2,8' fill='blue' stroke='white' stroke-width='2'/></svg>"

    static_style_elements <- list(
        basins = list(
            geom_type = "polygon",
            fillColor = "blue",
            color = "white",
            weight = 3,
            opacity = 1,
            fillOpacity = 0.7,
            label = list(
                color = "#222",
                fontSize = "14px",
                fontWeight = "bold",
                textShadow = "1px 1px 1px #fff, -1px -1px 1px #fff, 1px -1px 1px #fff, -1px 1px 1px #fff"
            ),
            popupOptions = list(
                maxWidth = 320,
                closeButton = TRUE,
                autoPan = TRUE
            ),
            highlightOptions = list(
                color = "#FF6600",
                weight = 4,
                opacity = 1,
                bringToFront = TRUE,
                sendToBack = FALSE
            )
        ),
        pillows = list(
            geom_type = "point",
            color = "black",
            icon = pillows_icon_svg,
            iconWidth = 16,
            iconHeight = 16,
            radius = 8,
            weight = 2,
            opacity = 1,
            fillOpacity = 1,
            highlightOptions = list(
                color = "#00FFFF",
                weight = 5,
                opacity = 1,
                bringToFront = TRUE,
                sendToBack = FALSE
            )
        ),
        surveys = list(
            geom_type = "point",
            color = "black",
            radius = 8,
            weight = 2,
            opacity = 1,
            fillOpacity = 1,
            highlightOptions = list(
                color = "#00FFFF",
                weight = 5,
                opacity = 1,
                bringToFront = TRUE,
                sendToBack = FALSE
            )
        ),
        precip = list(
            geom_type = "point",
            color = "blue",
            radius = 6,
            weight = 2,
            opacity = 1,
            fillOpacity = 1,
            highlightOptions = list(
                color = "#00FFFF",
                weight = 5,
                opacity = 1,
                bringToFront = TRUE,
                sendToBack = FALSE
            )
        ),
        temp = list(
            geom_type = "point",
            color = "red",
            radius = 6,
            weight = 2,
            opacity = 1,
            fillOpacity = 1,
            highlightOptions = list(
                color = "#00FFFF",
                weight = 5,
                opacity = 1,
                bringToFront = TRUE,
                sendToBack = FALSE
            )
        ),
        roads = list(
            geom_type = "line",
            color = "#8B0000",
            weight = 2,
            opacity = 0.8,
            label = list(
                color = "#8B0000"
            )
        ),
        boundary = list(
            geom_type = "polygon",
            color = "#222222",
            weight = 3,
            fill = FALSE
        ),
        communities = list(
            geom_type = "point",
            iconUrl = communities_icon_svg,
            iconWidth = 16,
            iconHeight = 16,
            labelColor = "#222",
            labelFontSize = "13px",
            labelFontWeight = "bold",
            labelFontStyle = "italic",
            labelTextShadow = "1px 1px 1px #fff, -1px -1px 1px #fff, 1px -1px 1px #fff, -1px 1px 1px #fff"
        ),

        leaflet = list(),
        zoomDelta = 0.5,
        zoomSnap = 0.25,
        wheelPxPerZoomLevel = 120
    )

    # create leaflet style elements to minimize redundancy
    static_style_elements$communities$icon <- leaflet::icons(
        iconUrl = static_style_elements$communities$iconUrl,
        iconWidth = static_style_elements$communities$iconWidth,
        iconHeight = static_style_elements$communities$iconHeight
    )

    static_style_elements$communities$labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        direction = "top",
        textOnly = TRUE,
        style = list(
            color = static_style_elements$communities$labelColor,
            fontSize = static_style_elements$communities$labelFontSize,
            fontWeight = static_style_elements$communities$labelFontWeight,
            fontStyle = static_style_elements$communities$labelFontStyle,
            textShadow = static_style_elements$communities$labelTextShadow
        )
    )

    return(static_style_elements)
}


get_dynamic_style_elements <- function(
    statistic = NULL,
    language = "English"
) {
    # VALUE_COL_CHOICES = c("relative_to_med", "absolute", "percentile")
    # if (!(value_col %in% VALUE_COL_CHOICES)) {
    #     stop(
    #         paste0(
    #             "Invalid value_col specified; must be one of ",
    #             paste(VALUE_COL_CHOICES, collapse = ", ")
    #         )
    #     )
    # }

    if (is.null(statistic)) {
        statistic <- "relative_to_med"
    }

    # Color scheme and visualization parameters
    # Bins represent percentage of normal SWE (relative_to_med values)
    relative_bins <- c(-2, -1, 0, 50, 70, 90, 110, 130, 150, Inf)
    relative_colors <- c(
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

    # Percentile bins and colors (Spectral palette, 0-100)
    percentile_bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    percentile_colors <- c(
        "#9e0142", # Very low (0-10)
        "#d53e4f", # Low (10-20)
        "#f46d43", # Below normal (20-30)
        "#fdae61", # Below normal (30-40)
        "#fee090", # Near normal (40-50)
        "#e6f598", # Near normal (50-60)
        "#abdda4", # Above normal (60-70)
        "#66c2a5", # Above normal (70-80)
        "#3288bd", # High (80-90)
        "#5e4fa2" # Very high (90-100)
    )

    # Custom legend labels for each value type
    relative_labels = c(
        tr("snowbull_no_snow", language), #"No snow present where<br>historical median is zero",
        tr("snowbull_some_snow", language), #"Snow present where<br>historical median is zero",
        "< 50%",
        "50 - 69%",
        "70 - 89%",
        "90 - 109%",
        "110 - 129%",
        "130 - 149%",
        ">= 150%"
    )
    absolute_labels = c(
        "0-50 mm",
        "50-100 mm",
        "100-150 mm",
        "150-200 mm",
        "200-250 mm",
        "250-300 mm",
        "300-400 mm",
        "400-500 mm",
        ">500 mm"
    )
    percentile_labels = c(
        paste("0-10", tr("snowbull_percentile_suffix", language)),
        paste("10-20", tr("snowbull_percentile_suffix", language)),
        paste("20-30", tr("snowbull_percentile_suffix", language)),
        paste("30-40", tr("snowbull_percentile_suffix", language)),
        paste("40-50", tr("snowbull_percentile_suffix", language)),
        paste("50-60", tr("snowbull_percentile_suffix", language)),
        paste("60-70", tr("snowbull_percentile_suffix", language)),
        paste("70-80", tr("snowbull_percentile_suffix", language)),
        paste("80-90", tr("snowbull_percentile_suffix", language)),
        paste("90-100", tr("snowbull_percentile_suffix", language))
    )

    style_choices = list(
        relative_to_med = list(
            bins = relative_bins,
            colors = relative_colors,
            labels = relative_labels
        ),
        data = list(
            bins = absolute_bins,
            colors = absolute_colors,
            labels = absolute_labels
        ),
        percentile = list(
            bins = percentile_bins,
            colors = percentile_colors,
            labels = percentile_labels
        )
    )

    return(style_choices[[statistic]])
}


#' Create color mapping for SWE values
#'
#' @description
#' Creates a color mapping for SWE values based on predefined bins and colors.
#'
#' @param values Numeric vector of SWE values
#' @return Character vector of color hex codes
#' @noRd

# --- Helper function for color mapping ---
get_state_style_elements <- function(values, style_elements) {
    # style_elements must be a *list*, not a reactive expression
    # If passed as a reactive, call it: style_elements <- style_elements()
    if (is.function(style_elements)) {
        style_elements <- style_elements()
    }
    cut_values <- cut(
        values,
        breaks = style_elements$bins,
        include.lowest = TRUE,
        right = FALSE
    )
    colors <- style_elements$colors[as.numeric(cut_values)]
    colors[is.na(colors)] <- "gray"
    return(colors)
}


# HEADING 1 ####

#' Get the most recent non-missing value from a timeseries
#'
#' @param ts A data.frame with 'datetime' and 'value' columns
#' @return POSIXct datetime of the most recent valid measurement, or \code{NA}
#'
#' @description
#' This function finds the latest date in a timeseries that has a valid
#' (non-NA, non-NaN) value. Used to filter out stations with stale data.
#'
#' @details
#' The function validates input structure and searches backwards from the most
#' recent datetime to find the last valid measurement. This is used to determine
#' data currency for station filtering.
#'
#' @examples
#' \dontrun{
#' ts_data <- data.frame(
#'   datetime = as.POSIXct(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   value = c(100, NA, 150)
#' )
#' get_most_recent_date(ts_data)  # Returns 2024-01-03
#' }
#' @noRd
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

#' Standardize parameter names for consistent database queries
#'
#' @param parameter Character string of parameter name to standardize
#' @return Character string of standardized parameter name
#' @description
#' Converts various parameter name variations to standardized database values.
#' Handles common abbreviations and full names for snow, precipitation, and temperature.
#'
#' @details
#' Standardizes parameter names as follows:
#' \itemize{
#'   \item Snow variations ('snow', 'swe', 'snow water equivalent') -> 'snow water equivalent'
#'   \item Precipitation variations ('precip', 'precipitation', 'rain') -> 'precipitation, total'
#'   \item Temperature variations ('temp', 'temperature', 'air temp') -> 'temperature, air'
#' }
#'
#' @examples
#' \dontrun{
#' standardize_parameter_name("snow")  # Returns "snow water equivalent"
#' standardize_parameter_name("swe")   # Returns "snow water equivalent"
#' standardize_parameter_name("precip") # Returns "precipitation, total"
#' standardize_parameter_name("temp")   # Returns "temperature, air"
#' }
#' @noRd
standardize_parameter_name <- function(parameter) {
    if (is.null(parameter) || is.na(parameter) || parameter == "") {
        return("snow water equivalent") # Default to SWE
    }

    # Convert to lowercase and trim whitespace for comparison
    param_clean <- tolower(trimws(parameter))

    # Snow water equivalent variations
    snow_variants <- c(
        "snow",
        "swe",
        "snow water equivalent",
        "snow_water_equivalent",
        "snowpack"
    )

    # Precipitation variations
    precip_variants <- c(
        "precip",
        "precipitation",
        "precipitation, total",
        "precip, total"
    )

    # Temperature variations
    temp_variants <- c(
        "temp",
        "temperature",
        "air temp",
        "air temperature",
        "temperature, air"
    )

    # Check each category and return standardized name
    if (param_clean %in% snow_variants) {
        return("snow water equivalent")
    } else if (param_clean %in% precip_variants) {
        return("precipitation, total")
    } else if (param_clean %in% temp_variants) {
        return("temperature, air")
    } else {
        # Return original parameter if no match found
        warning(sprintf("Unknown parameter '%s', returning as-is", parameter))
        return(parameter)
    }
}

#' Convert year and month to POSIXct datetime
#'
#' @param year Integer year (e.g., 2025)
#' @param month Integer month (1-12)
#' @return POSIXct datetime object in UTC timezone, set to first day of month
#' @description
#' Creates a standardized datetime for the first day of the specified month and year.
#' Used throughout the application for consistent date handling.
#'
#' @examples
#' \dontrun{
#' get_datetime(2025, 3)  # Returns 2025-03-01 00:00:00 UTC
#' get_datetime(2024, 12) # Returns 2024-12-01 00:00:00 UTC
#' }
#' @noRd

get_datetime <- function(year, month) {
    as.POSIXct(as.Date(paste0(year, "-", month, "-01")), tz = "UTC")
}

#' Resample timeseries data by aggregation function and frequency
#'
#' @param ts_data data.frame with datetime column and station value columns
#' @param frequency Character string: "daily", "monthly", or "yearly"
#' @param func Character string: aggregation function ("sum", "mean", "max", "min")
#' @return data.frame with resampled timeseries data
#' @noRd

resample_timeseries <- function(ts_data, frequency = "monthly", func = "sum") {
    if (is.null(ts_data) || nrow(ts_data) == 0) {
        return(ts_data)
    }

    # Validate inputs
    frequency <- match.arg(frequency, choices = c("daily", "monthly", "yearly"))
    func <- match.arg(func, choices = c("sum", "mean", "max", "min"))

    # Ensure datetime column exists and is POSIXct
    if (!"datetime" %in% names(ts_data)) {
        warning("No datetime column found, returning original data")
        return(ts_data)
    }

    ts_data$datetime <- as.POSIXct(ts_data$datetime, tz = "UTC")

    # Create grouping variable based on frequency
    if (frequency == "daily") {
        ts_data$group_var <- as.Date(ts_data$datetime)
    } else if (frequency == "monthly") {
        ts_data$group_var <- format(ts_data$datetime, "%Y-%m")
    } else if (frequency == "yearly") {
        ts_data$group_var <- format(ts_data$datetime, "%Y")
    }

    # Get station columns (exclude datetime and group_var)
    station_cols <- setdiff(names(ts_data), c("datetime", "group_var"))

    if (length(station_cols) == 0) {
        warning("No station columns found, returning original data")
        return(ts_data[, !names(ts_data) %in% "group_var", drop = FALSE])
    }

    # Select aggregation function
    agg_func <- switch(
        func,
        "sum" = function(x) {
            if (all(is.na(x))) {
                return(as.numeric(NA))
            }
            sum(x, na.rm = TRUE)
        },
        "mean" = function(x) {
            if (all(is.na(x))) {
                return(as.numeric(NA))
            }
            mean(x, na.rm = TRUE)
        },
        "max" = function(x) {
            if (all(is.na(x))) {
                return(as.numeric(NA))
            }
            max(x, na.rm = TRUE)
        },
        "min" = function(x) {
            if (all(is.na(x))) {
                return(as.numeric(NA))
            }
            min(x, na.rm = TRUE)
        }
    )

    # Create result dataframe with unique groups
    resampled_data <- data.frame(
        group_var = unique(ts_data$group_var),
        stringsAsFactors = FALSE
    )

    # Aggregate each station column
    for (col in station_cols) {
        agg_result <- stats::aggregate(
            ts_data[[col]],
            by = list(ts_data$group_var),
            FUN = agg_func
        )
        names(agg_result) <- c("group_var", col)
        resampled_data <- merge(
            resampled_data,
            agg_result,
            by = "group_var",
            all = TRUE
        )
    }

    # Convert group_var to datetime and apply left window shift
    if (frequency == "daily") {
        resampled_data$datetime <- as.POSIXct(
            resampled_data$group_var,
            tz = "UTC"
        )
    } else if (frequency == "monthly") {
        # Convert to first day of month, then shift forward by 1 month
        base_datetime <- as.POSIXct(
            paste0(resampled_data$group_var, "-01"),
            tz = "UTC"
        )
        # Add one month to each date
        shifted_dates <- as.Date(base_datetime)
        for (i in seq_along(shifted_dates)) {
            shifted_dates[i] <- seq(
                shifted_dates[i],
                by = "month",
                length.out = 2
            )[2]
        }
        resampled_data$datetime <- as.POSIXct(shifted_dates, tz = "UTC")
    } else if (frequency == "yearly") {
        # Convert to first day of year, then shift forward by 1 year
        base_datetime <- as.POSIXct(
            paste0(resampled_data$group_var, "-01-01"),
            tz = "UTC"
        )
        # Add one year to each date
        shifted_dates <- as.Date(base_datetime)
        for (i in seq_along(shifted_dates)) {
            shifted_dates[i] <- seq(
                shifted_dates[i],
                by = "year",
                length.out = 2
            )[2]
        }
        resampled_data$datetime <- as.POSIXct(shifted_dates, tz = "UTC")
    }

    # Remove group_var column and reorder with datetime first
    resampled_data$group_var <- NULL
    resampled_data <- resampled_data[, c("datetime", station_cols)]

    # Sort by datetime
    resampled_data <- resampled_data[
        order(resampled_data$datetime),
        ,
        drop = FALSE
    ]

    return(resampled_data)
}

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Load spatial data from database with coordinate transformation
#'
#' @param con DBI database connection object
#' @param layer_name Character string of the spatial layer name in the database
#' @param additional_query Optional character string for additional WHERE conditions
#' @return sf object in WGS84 (EPSG:4326) coordinate system, or \code{NULL} if no data found
#'
#' @description
#' Queries the spatial.vectors table and transforms geometries to WGS84.
#' Used for loading administrative boundaries, communities, roads, etc.
#' @details
#' The function first verifies that the requested layer exists in the database,
#' then downloads and transforms the geometry to WGS84 for consistent mapping.
#' Additional SQL WHERE conditions can be specified for filtering.
#'
#' @examples
#' \dontrun{
#' # Load all communities
#' communities <- download_spatial_layer(con, "Communities")
#'
#' # Load only major communities
#' major_communities <- download_spatial_layer(
#'   con,
#'   "Communities",
#'   "AND description IN ('City', 'Town')"
#' )
#' }
#' @noRd

download_spatial_layer <- function(con, layer_name, additional_query = NULL) {
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

#' Retrieve continuous SWE timeseries and station metadata
#'
#' @param con DBI database connection object
#' @param start_date Character date string (YYYY-MM-DD) for filtering data
#' @param end_date Character date string (YYYY-MM-DD) for filtering data
#' @param resolution Character, either "daily" or "monthly" for data aggregation
#' @param parameter_name Character string of the parameter name to retrieve
#' @return A list with two elements:
#' \describe{
#'   \item{timeseries}{Named list containing a 'swe' data.frame with datetime and station columns}
#'   \item{metadata}{sf object with station locations and latest_date column}
#' }
#'
#' @description
#' Loads continuous snow water equivalent measurements from snow pillow stations.
#' Data is aggregated to daily resolution by default. Metadata includes the
#' most recent measurement date for each station.
#'
#' @details
#' The function queries the continuous.timeseries and continuous.measurements_calculated_daily_corrected
#' tables to retrieve SWE data. Each station becomes a column in the wide-format
#' timeseries data.frame, facilitating efficient historical analysis.
#'
#' @examples
#' \dontrun{
#' continuous_data <- download_continuous_ts(con)
#' print(names(continuous_data$timeseries$data))  # Shows datetime + station columns
#' print(nrow(continuous_data$metadata))         # Number of stations
#' }

download_continuous_ts <- function(
    con,
    start_date = sprintf("%d-01-01", 1950),
    end_date = sprintf("%d-01-01", 2100),
    resolution = "daily",
    parameter_name = "snow water equivalent"
) {
    parameter_name <- standardize_parameter_name(parameter_name)

    print(parameter_name)

    # Verify parameter exists in public.parameters table
    if (!check_parameter_exists(con, parameter_name)) {
        warning(sprintf(
            "Parameter '%s' not found in public.parameters table",
            parameter_name
        ))
        return(list(
            timeseries = list(
                data = data.frame(datetime = as.POSIXct(character(0)))
            ),
            metadata = NULL
        ))
    }

    # Build metadata query for continuous SWE timeseries
    md_query <- paste0(
        "SELECT
            t.timeseries_id,
            t.location_id,
            l.location,
            l.name,
            l.latitude,
            l.longitude,
            dc.conversion_m
         FROM continuous.timeseries t
         JOIN public.locations l ON t.location_id = l.location_id
         LEFT JOIN datum_conversions dc ON l.location_id = dc.location_id
         WHERE t.parameter_id = (SELECT parameter_id FROM public.parameters
                                 WHERE param_name = ",
        DBI::dbQuoteString(con, parameter_name),
        ")"
    )

    md_continuous_df <- DBI::dbGetQuery(con, md_query)

    if (nrow(md_continuous_df) == 0) {
        warning("No continuous data stations found")
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
            ts_daily <- stats::aggregate(
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
            ts_monthly <- stats::aggregate(
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

    # add a key column to match the timeseries columns (this key is different for each datatype)
    metadata_sf$key <- metadata_sf$timeseries_id

    # Get coordinates for stations
    surveys_coords <- sf::st_coordinates(metadata_sf)
    metadata_sf$x <- surveys_coords[, 1]
    metadata_sf$y <- surveys_coords[, 2]

    return(list(
        timeseries = list(data = master_df),
        metadata = metadata_sf,
        geom = "point",
        continuity = "continuous",
        label = "snow_pillow",
        parameter = "swe"
    ))
}

#' Check if parameter exists in the database
#'
#' @param con DBI database connection object
#' @param parameter_name Character string of the parameter name to check
#' @return Logical indicating whether the parameter exists
#' @noRd

check_parameter_exists <- function(con, parameter_name) {
    param_check_query <- sprintf(
        "SELECT COUNT(*) as count FROM public.parameters WHERE param_name = %s",
        DBI::dbQuoteString(con, parameter_name)
    )
    param_exists <- DBI::dbGetQuery(con, param_check_query)$count > 0
    return(param_exists)
}

#' Retrieve discrete SWE timeseries and station metadata
#'
#' @param con DBI database connection object
#' @param start_date Character date string (YYYY-MM-DD) for filtering data
#' @param end_date Character date string (YYYY-MM-DD) for filtering data
#' @param parameter_name Character string of the parameter name to retrieve
#' @return A list with two elements:
#' \describe{
#'   \item{timeseries}{Named list containing a 'swe' data.frame with datetime and station columns}
#'   \item{metadata}{sf object with station locations and latest_date column}
#' }
#'
#' @description
#' Loads discrete (manual) snow water equivalent measurements from snow
#' courses. Uses target_datetime when available, falls back to datetime.
#'
#' @details
#' Manual snow course measurements are typically taken monthly during winter.
#' The function prefers target_datetime (scheduled measurement date) over
#' actual datetime when available for consistent temporal analysis.
#'
#' @examples
#' \dontrun{
#' discrete_data <- download_discrete_ts(con)
#' # Check data currency
#' recent_stations <- discrete_data$metadata[
#'   !is.na(discrete_data$metadata$latest_date),
#' ]
#' }

download_discrete_ts <- function(
    con,
    start_date = sprintf("%d-01-01", 1950),
    end_date = sprintf("%d-01-01", 2100),
    parameter_name = "snow water equivalent"
) {
    # Verify parameter exists in public.parameters table
    if (!check_parameter_exists(con, parameter_name)) {
        warning(sprintf(
            "Parameter '%s' not found in public.parameters table",
            parameter_name
        ))
        return(list(
            timeseries = list(
                swe = data.frame(datetime = as.POSIXct(character(0)))
            ),
            metadata = NULL
        ))
    }

    # Build metadata query for discrete SWE timeseries
    md_discrete_df <- DBI::dbGetQuery(
        con,
        sprintf(
            "SELECT DISTINCT
            l.location_id,
            l.latitude,
            l.longitude,
            l.location,
            l.name,
            dc.conversion_m
             FROM discrete.samples s
             JOIN discrete.results r ON s.sample_id = r.sample_id
             JOIN public.locations l ON s.location_id = l.location_id
             LEFT JOIN datum_conversions dc ON l.location_id = dc.location_id
             WHERE r.parameter_id = (SELECT parameter_id FROM public.parameters
                         WHERE param_name = %s)",
            DBI::dbQuoteString(con, parameter_name)
        )
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

    # Compute most recent non-NaN value per station using ts_list_temp
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

    # add a key column to match the timeseries columns (this key is different for each datatype)
    metadata_sf$key <- metadata_sf$location_id

    # Get coordinates for stations
    surveys_coords <- sf::st_coordinates(metadata_sf)
    metadata_sf$x <- surveys_coords[, 1]
    metadata_sf$y <- surveys_coords[, 2]

    return(list(
        timeseries = list(data = master_df),
        metadata = metadata_sf,
        geom = "point",
        continuity = "discrete",
        label = "snow_survey",
        parameter = "swe"
    ))
}

#' Load and process snowcourse factors for discrete SWE stations
#'
#' @param metadata_discrete sf object with discrete station metadata
#' @return data.frame with location IDs and basin weighting factors
#'
#' @description
#' Reads the CSV file containing weights for how much each snow course
#' station contributes to each SWE basin calculation. Removes duplicates and
#' merges with station location IDs.
#'
#' @details
#' The snowcourse factors define how measurements from individual snow course
#' stations are weighted when calculating basin-average SWE values. Each row
#' represents a station, with columns for each basin showing the contribution weight.
#'
#' @examples
#' \dontrun{
#' weights <- load_snowcourse_factors(discrete_metadata)
#' # Check which basins a station contributes to
#' station_weights <- weights[weights$location_id == 123, ]
#' }

load_snowcourse_factors <- function(
    metadata_discrete
) {
    snowcourse_factors <- data$snowcourse_factors

    # Remove duplicate Hyland River value
    snowcourse_factors <- snowcourse_factors[
        snowcourse_factors$location != "10AD-SC01",
    ]
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
#' @param ts Wide-format data.frame with 'datetime' column and station columns
#' @param lookback_length Integer number of years to look back from each measurement (optional)
#' @param lookback_start Integer year to start lookback period from (e.g., 1980) (optional)
#' @param lookback_end Integer year to end lookback period (optional, defaults to current year - 1)
#' @return A list with two elements:
#' \describe{
#'   \item{historic_median}{data.frame with historic median values for each date/station}
#'   \item{relative_to_med}{data.frame with current values as percentage of historic median}
#' }
#'
#' @description
#' For each measurement, calculates the historic median for the same day-of-year
#' using data from previous years within the lookback period. Supports both
#' fixed lookback periods and rolling windows.
#'
#' @details
#' The function handles special cases for relative SWE calculation:
#' \itemize{
#'   \item Standard percentage when historic median > 0
#'   \item Value of -2 when both current and historic are zero
#'   \item Value of -1 when current > 0 but historic is zero
#' }
#'
#' Data is filtered to February-May and snapped to the 1st of each month for
#' consistency with snow bulletin reporting periods.
#'
#' @examples
#' \dontrun{
#' # Calculate using fixed lookback year
#' result <- calculate_historic_daily_median(ts_data, lookback_start = 1980)
#'
#' # Calculate using rolling 30-year window
#' result <- calculate_historic_daily_median(ts_data, lookback_length = 30)
#' }

calculate_historic_daily_median <- function(
    ts,
    lookback_length = NULL,
    lookback_start = NULL,
    lookback_end = NULL
) {
    # Default behaviour if neither provided
    if (is.null(lookback_start) && is.null(lookback_length)) {
        lookback_start <- 1980
    } else if (!is.null(lookback_start) && !is.null(lookback_length)) {
        stop("Specify either lookback_start or lookback_length, not both.")
    }

    # Warning if lookback_end is not provided when using lookback_start
    if (!is.null(lookback_start) && is.null(lookback_end)) {
        warning(
            "lookback_end not provided when using lookback_start. Defaulting to most recent historical value (current year - 1)."
        )
        lookback_end <- as.integer(format(Sys.Date(), "%Y")) - 1
    }

    if (!"datetime" %in% names(ts)) {
        stop(sprintf(
            "Input timeseries must contain 'datetime' column. Found columns: %s",
            paste(names(ts), collapse = ", ")
        ))
    }

    if (lookback_start >= lookback_end && !is.null(lookback_end)) {
        stop("lookback_start must be less than lookback_end.")
    }

    if (!is.null(lookback_start) && !is.null(lookback_end)) {
        if (
            lookback_start < 1800 ||
                lookback_end < 1800 ||
                lookback_start > as.integer(format(Sys.Date(), "%Y")) ||
                lookback_end > as.integer(format(Sys.Date(), "%Y"))
        ) {
            stop(
                "lookback_start and lookback_end must be valid years (>= 1800 and <= current year)."
            )
        }
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
        perc_df <- data.frame(datetime = ts$datetime, stringsAsFactors = FALSE)

        # Vectorized and grouped computation for speed
        p <- length(station_cols)
        if (p > 0) {
            vals_mat <- as.matrix(ts[, station_cols, drop = FALSE]) # n x p
            hist_mat <- matrix(NA_real_, nrow = n, ncol = p)
            rel_mat <- matrix(NA_real_, nrow = n, ncol = p)
            perc_mat <- matrix(NA_real_, nrow = n, ncol = p)

            # Group by month-day (usually day==1 after snapping), compute per group
            grp <- paste(month, day, sep = "-")
            ug <- unique(grp)
            for (group_month_day in ug) {
                group_indices <- which(grp == group_month_day)
                # Ensure chronological order within group
                group_indices <- group_indices[order(year[group_indices])]

                # Precompute years vector for the group
                group_years <- year[group_indices]

                for (current_idx in seq_along(group_indices)) {
                    current_row <- group_indices[current_idx]

                    # if lookback_start is provided, use that as fixed start year
                    # default to the current year - 1 as lookback_end
                    if (!is.null(lookback_start)) {
                        historical_indices <- group_indices[which(
                            group_years < group_years[current_idx] &
                                group_years >= lookback_start &
                                group_years <= lookback_end
                        )]

                        # # if lookback_end is provided, apply that
                        # if (!is.null(lookback_end)) {
                        #     historical_indices <- historical_indices[which(
                        #         group_years[historical_indices] <= lookback_end
                        #     )]
                        # }

                        # if lookback_length is provided, apply that. Note, this is secondary to lookback_start
                    } else {
                        historical_indices <- group_indices[which(
                            group_years < group_years[current_idx] &
                                group_years >=
                                    (group_years[current_idx] - lookback_length)
                        )]
                    }

                    if (length(historical_indices) > 0) {
                        historical_values <- vals_mat[
                            historical_indices,
                            ,
                            drop = FALSE
                        ]
                        hist_mat[current_row, ] <- apply(
                            historical_values,
                            2,
                            stats::median,
                            na.rm = TRUE
                        )

                        # Calculate percentile: what % of historical values are <= current value
                        if (length(historical_indices) >= 4) {
                            current_values <- vals_mat[current_row, ]
                            if (all(is.na(current_values))) {
                                perc_mat[current_row, ] <- NA_real_
                            } else {
                                perc_mat[current_row, ] <- (colSums(
                                    historical_values <=
                                        matrix(
                                            current_values,
                                            nrow = nrow(historical_values),
                                            ncol = ncol(historical_values),
                                            byrow = TRUE
                                        ),
                                    na.rm = TRUE
                                ) /
                                    colSums(
                                        !is.na(historical_values),
                                        na.rm = TRUE
                                    )) *
                                    100
                            }
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

            colnames(perc_mat) <- station_cols
            perc_df[station_cols] <- as.data.frame(
                perc_mat,
                stringsAsFactors = FALSE
            )
        }

        # ensure ordering by datetime
        hist_df <- hist_df[order(hist_df$datetime), , drop = FALSE]
        rel_df <- rel_df[order(rel_df$datetime), , drop = FALSE]
        perc_df <- perc_df[order(perc_df$datetime), , drop = FALSE]

        return(list(
            historic_median = hist_df,
            relative_to_med = rel_df,
            percentile = perc_df
        ))
    }

    stop(
        "Input timeseries must contain a 'datetime' column (and either 'value' for single-station or station columns for wide format)."
    )
}

split_communities <- function(communities) {
    # Assume input is a comma-separated string of community names
    large_types <- c("City", "Town")
    small_types <- c("Settlement", "Village")
    large <- communities[communities$description %in% large_types, ]
    small <- communities[communities$description %in% small_types, ]

    list(large = large, small = small)
}

#' Extract SWE data at specific points for a given year and month
#'
#' @param data List containing timeseries and metadata from download functions
#' @param year Integer target year for data extraction
#' @param month Integer target month for data extraction
#' @param key Character name of the key column in metadata (e.g., "timeseries_id", "location_id")
#' @return data.frame subset of metadata with additional SWE value columns:
#' \describe{
#'   \item{swe}{Absolute SWE value in mm}
#'   \item{relative_to_med}{SWE as percentage of historic median}
#'   \item{historic_median}{Historic median SWE value for this date}
#' }
#'
#' @description
#' Extracts values for the target date from the reorganized timeseries structure
#' where data is organized by parameter type with stations as columns.
#'
#' @details
#' The function matches the target date (first of specified month/year) with
#' available timeseries data and extracts the relevant values for each station.
#' Missing data is represented as NA.
#'
#' @examples
#' \dontrun{
#' # Get pillow data for March 2025
#' pillow_data <- get_parameter_states(
#'   snowbull_data$swe$pillows,
#'   year = 2025,
#'   month = 3,
#'   key = "timeseries_id"
#' )
#'
#' # Check data availability
#' valid_stations <- pillow_data[!is.na(pillow_data$swe), ]
#' }

get_state_as_shp <- function(
    data,
    year,
    month
) {
    # Assert that data contains timeseries and metadata
    stopifnot(is.list(data))
    stopifnot("timeseries" %in% names(data))
    stopifnot("metadata" %in% names(data))
    stopifnot(all(
        c("data", "historic_median", "relative_to_med") %in%
            names(data$timeseries)
    ))
    point_source_data <- data$metadata
    target_date <- get_datetime(year, month)

    # Helper to extract value for each station at the closest matching date
    extract_at_date <- function(ts_df, target_date) {
        vals <- rep(NA_real_, nrow(point_source_data))
        if (!is.null(ts_df) && "datetime" %in% names(ts_df)) {
            for (i in seq_len(nrow(point_source_data))) {
                station_id <- as.character(point_source_data[["key"]][i])
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

    # get the statistics at the target date
    # this code is a bit clunky and could be optimized, but works for now; should not need to call extract_at_date every time
    point_source_data$data <- extract_at_date(
        data$timeseries$data,
        target_date
    )
    point_source_data$relative_to_med <- extract_at_date(
        data$timeseries$relative_to_med,
        target_date
    )
    point_source_data$historic_median <- extract_at_date(
        data$timeseries$historic_median,
        target_date
    )

    point_source_data$percentile <- extract_at_date(
        data$timeseries$percentile,
        target_date
    )

    return(point_source_data)
}

# =============================================================================
# POPUP CREATION FUNCTIONS
# =============================================================================

#' Create base64-encoded ggplotOverlap plot for continuous stations
#'
#' @param timeseries data.frame with 'datetime' and station value columns
#' @param year Integer year for plot focus
#' @param con Database connection for historical data access
#' @param station_name Optional character string for station name in plot title
#' @param language Character string for language. Defaults to "English".
#' @return Character string containing HTML with embedded base64 image
#'
#' @description
#' Creates a YGwater ggplotOverlap plot showing current year data
#' compared to historical range. Plot is saved as PNG, encoded to base64,
#' and embedded in HTML for use in leaflet popups.
#'
#' @details
#' The plot shows:
#' \itemize{
#'   \item Historical range (min/max) as shaded area
#'   \item Historical median as dashed line
#'   \item Current year data as solid line
#'   \item Click-to-expand functionality for full-size viewing
#' }
#'
#' @examples
#' \dontrun{
#' station_ts <- snowbull_data$swe$pillows$timeseries$data[, c("datetime", "123")]
#' popup_html <- create_continuous_plot_popup(station_ts, 2025, con)
#' }

create_continuous_plot_popup <- function(
    timeseries,
    year,
    con,
    station_name,
    language = "English"
) {
    lang <- shortenLanguage(language)

    # Validate timeseries structure
    if (!is.data.frame(timeseries) || ncol(timeseries) < 2) {
        stop("Timeseries must have columns datetime and data")
    }

    names(timeseries) <- c("datetime", "value")

    timeseries <- timeseries[
        !is.na(timeseries$value) & !is.nan(timeseries$value),
    ]

    tryCatch(
        {
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
                lang = lang,
                gridx = FALSE,
                gridy = FALSE,
                filter = c(100, 100),
                continuous_data = timeseries,
                con = con
            )
        },
        error = function(e) {
            return(
                "<div style='text-align: center; padding: 20px;'><p style='color: #d9534f;'>Error generating plot</p></div>"
            )
        }
    )

    # Save the plot as a PNG tempfile with smaller dimensions
    plot_file <- tempfile(fileext = ".png")

    # Suppress warnings during PNG creation to avoid the geom_line warning
    suppressWarnings({
        grDevices::png(plot_file, width = 800, height = 600, res = 120)
        print(plot)
        grDevices::dev.off()
    })

    # Encode PNG to base64
    plot_data <- readBin(plot_file, "raw", file.info(plot_file)$size)
    plot_base64 <- base64enc::base64encode(plot_data)
    unlink(plot_file)

    # HTML for popup with smaller image and click-to-open functionality
    popup_html <- sprintf(
        paste0(
            "<div style='text-align: center; max-width: 660px; width: 100%%;'>",
            "<b>%s</b><br>",
            "<img src='data:image/png;base64,%s' ",
            "style='width: 100%%; height: auto; cursor: pointer; border: 1px solid #ccc; margin-top: 10px;' ",
            "onclick='window.open(\"data:image/png;base64,%s\", \"_blank\", \"width=800,height=600\");' ",
            "title='Click to open full size in new window' />",
            "<br><small style='color: #666;'>Click image to open full size</small>",
            "</div>"
        ),
        htmltools::htmlEscape(station_name),
        plot_base64,
        plot_base64
    )
    return(popup_html)
}

#' Create base64-encoded plot for discrete stations
#'
#' @param timeseries data.frame with timeseries data formatted for plotting
#' @param station_name Character string for station name in plot title
#' @param language Character string for language. Defaults to "English".
#' @return Character string containing HTML with embedded base64 image
#'
#' @description
#' Creates a YGwater hydrometDiscrete plot of discrete measurements over time.
#' Plot is saved as PNG, encoded to base64, and embedded in HTML for leaflet popups.
#'
#' @details
#' The discrete plot shows monthly snow course measurements as:
#' \itemize{
#'   \item Line plot connecting measurements within each year
#'   \item Box plots showing distribution across years for each month
#'   \item Current year highlighted if available
#' }
#'
#' @examples
#' \dontrun{
#' station_ts <- snowbull_data$swe$surveys$timeseries$data[, c("datetime", "456")]
#' popup_html <- create_discrete_plot_popup(station_ts)
#' }

create_discrete_plot_popup <- function(
    timeseries,
    station_name,
    language = "English"
) {
    # Clean and validate the data before plotting
    names(timeseries) <- c("datetime", "value")

    lang <- shortenLanguage(language)

    timeseries$month <- as.integer(format(timeseries$datetime, "%m"))
    timeseries$year <- as.integer(format(timeseries$datetime, "%Y"))
    timeseries$units <- "mm"
    tryCatch(
        {
            plot <- hydrometDiscrete(
                parameter = "SWE",
                location = station_name,
                startDay = 1,
                discrete_data = timeseries,
                years = c(max(timeseries$year, na.rm = TRUE)),
                plot_type = "boxplot",
                lang = lang
            )
        },
        error = function(e) {
            return(
                "<div style='text-align: center; padding: 20px;'><p style='color: #d9534f;'>Error generating plot</p></div>"
            )
        }
    )

    # Save the plot as a PNG tempfile with smaller dimensions
    plot_file <- tempfile(fileext = ".png")

    # Suppress warnings during PNG creation to avoid the geom_line warning
    suppressWarnings({
        grDevices::png(plot_file, width = 800, height = 600, res = 120)
        print(plot)
        grDevices::dev.off()
    })

    # Encode PNG to base64
    plot_data <- readBin(plot_file, "raw", file.info(plot_file)$size)
    plot_base64 <- base64enc::base64encode(plot_data)
    unlink(plot_file)

    # HTML for popup with smaller image and click-to-open functionality
    popup_html <- sprintf(
        paste0(
            "<div style='text-align: center; max-width: 660px; width: 100%%;'>",
            "<b>%s</b><br>",
            "<img src='data:image/png;base64,%s' ",
            "style='width: 100%%; height: auto; cursor: pointer; border: 1px solid #ccc; margin-top: 10px;' ",
            "onclick='window.open(\"data:image/png;base64,%s\", \"_blank\", \"width=800,height=600\");' ",
            "title='Click to open full size in new window' />",
            "<br><small style='color: #666;'>Click image to open full size</small>",
            "</div>"
        ),
        htmltools::htmlEscape(station_name),
        plot_base64,
        plot_base64
    )
    return(popup_html)
}

#' Load all base data for the SWE mapping application
#'
#' @param con DBI database connection object
#' @param load_swe Logical indicating whether to load SWE data (default TRUE)
#' @param load_precip Logical indicating whether to load precipitation data (default FALSE)
#' @param load_temp Logical indicating whether to load temperature data (default FALSE)
#' @return A list containing all loaded base data:
#' \describe{
#'   \item{pillows}{List with continuous station timeseries and metadata}
#'   \item{surveys}{List with discrete station timeseries and metadata}
#'   \item{basins}{List with basin-averaged timeseries and metadata}
#'   \item{shapefiles}{List with spatial data (yukon, communities, roads)}
#'   \item{precipitation}{List with precipitation timeseries and metadata (if loaded)}
#'   \item{temperature}{List with temperature timeseries and metadata (if loaded)}
#' }
#'
#' @description
#' Loads all static data and reorganizes timeseries structure for efficient access.
#' Basin-averaged SWE is calculated for each timestep using discrete station data
#' and weighting factors.
#'
#' @details
#' This function performs several key operations:
#' \enumerate{
#'   \item Loads continuous and discrete SWE timeseries
#'   \item Calculates historical medians and relative values
#'   \item Computes basin-averaged SWE using weighting factors
#'   \item Loads spatial layers (boundaries, communities, roads)
#'   \item Applies label position adjustments for optimal display
#' }
#'
#' The resulting data structure is optimized for both interactive (Shiny) and
#' static (export) mapping applications.
#'
#' @examples
#' \dontrun{
#' con <- AquaCache::AquaConnect(...)
#' snowbull_data <- load_bulletin_data(con)
#'
#' # Check data availability
#' print(sprintf("Loaded %d pillow stations", nrow(snowbull_data$swe$pillows$metadata)))
#' print(sprintf("Loaded %d survey stations", nrow(snowbull_data$swe$surveys$metadata)))
#' print(sprintf("Loaded %d basins", nrow(snowbull_data$swe$basins$metadata)))
#' }
#'

load_bulletin_timeseries <- function(
    con,
    load_swe = TRUE,
    load_precip = FALSE,
    load_temp = FALSE
) {
    # Initialize output structure
    # SWE consists of pillows (continuous), surveys (discrete), and basins (weighted average)
    # Precipitation and temperature are only continuous for now
    # Each data entry has timeseries and metadata
    # Each timeseries entry has data, historic_median, relative_to_med, percentile
    snowbull_timeseries <- list(
        swe = list(),
        precipitation = list(),
        temperature = list()
    )

    if (load_swe) {
        # load basin shapefiles for basin averaging
        basins_shp <- sf::st_read(
            system.file(
                "snow_survey/swe_basins/swe_basins.shp",
                package = "YGwater",
                mustWork = TRUE
            ),
            quiet = TRUE
        )

        # rename basin name column if needed
        if ("SWE_Basin" %in% names(basins_shp)) {
            names(basins_shp)[names(basins_shp) == "SWE_Basin"] <- "name"
        }

        # convert area to km2
        basins_shp$area_km2 <- sf::st_area(basins_shp) |> as.numeric() * 1e-6

        # load swe data from continuous source
        continuous_data <- download_continuous_ts(con, parameter_name = "swe")

        # process norms and relative values
        ret <- calculate_historic_daily_median(
            continuous_data$timeseries$data,
            lookback_start = 1980,
            lookback_end = 2020
        )
        continuous_data$timeseries$historic_median <- ret$historic_median
        continuous_data$timeseries$relative_to_med <- ret$relative_to_med
        continuous_data$timeseries$percentile <- ret$percentile

        # store continuous pillow data
        snowbull_timeseries$swe$pillows <- continuous_data

        # load swe data from discrete source
        discrete_data <- download_discrete_ts(con)

        ret <- calculate_historic_daily_median(
            discrete_data$timeseries$data,
            lookback_start = 1980,
            lookback_end = 2020
        )
        discrete_data$timeseries$historic_median <- ret$historic_median
        discrete_data$timeseries$relative_to_med <- ret$relative_to_med
        discrete_data$timeseries$percentile <- ret$percentile

        # store discrete survey data
        snowbull_timeseries$swe$surveys <- discrete_data

        # Load or infer weight matrix from snowcourse factors CSV using discrete metadata
        weights_df <- load_snowcourse_factors(
            metadata_discrete = discrete_data$metadata
        )

        # Ensure basin CRS is WGS84
        if (sf::st_crs(basins_shp)$epsg != 4326) {
            basins_shp <- sf::st_transform(basins_shp, 4326)
        }

        # Prepare dates and station list from discrete wide timeseries
        basin_dates <- snowbull_timeseries$swe$surveys$timeseries$data$datetime

        # Convert station_cols from location_id to location (name) using discrete metadata
        basin_names <- basins_shp$name
        basin_swe_mat <- matrix(
            NA_real_,
            nrow = length(basin_dates),
            ncol = length(basin_names)
        )

        # for each survey date
        for (i in seq_along(basin_dates)) {
            basin_swe_mat[i, ] <- NA_real_

            # get the weight matrix for available stations on that date
            weight_matrix <- weights_df[,
                c("location_id", basin_names),
                drop = FALSE
            ]

            swe_samples <- as.numeric(discrete_data$timeseries$data[
                i,
                as.character(weight_matrix$location_id),
                drop = TRUE
            ])

            # Remove NaN samples and corresponding rows in weight matrix
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

        # Compute historic median and relative change for basins
        ret <- calculate_historic_daily_median(
            basin_timeseries,
            lookback_start = 1980,
            lookback_end = 2020
        )

        # store basin-averaged data
        snowbull_timeseries$swe$basins$timeseries <- list(
            data = basin_timeseries,
            historic_median = ret$historic_median,
            relative_to_med = ret$relative_to_med,
            percentile = ret$percentile
        )

        snowbull_timeseries$swe$basins$geom <- "poly"
        snowbull_timeseries$swe$basins$continuity <- "discrete"
        snowbull_timeseries$swe$basins$parameter <- "swe"
        snowbull_timeseries$swe$basins$label <- "snow_basin"

        # Process basin names for better display on map
        snowbull_timeseries$swe$basins$metadata <- basins_shp
        snowbull_timeseries$swe$basins$metadata$key <- snowbull_timeseries$swe$basins$metadata$name
        snowbull_timeseries$swe$basins$metadata$annotation <- snowbull_timeseries$swe$basins$metadata$Label
        snowbull_timeseries$swe$basins$metadata$annotation <- gsub(
            "_",
            " ",
            as.character(snowbull_timeseries$swe$basins$metadata$annotation)
        )
        snowbull_timeseries$swe$basins$metadata$annotation <- vapply(
            snowbull_timeseries$swe$basins$metadata$annotation,
            FUN.VALUE = character(1),
            FUN = function(x) {
                if (is.na(x) || x == "") {
                    return(x)
                }
                # Add line break after first word if it's 5+ characters
                sub("^(\\S{5,})\\s+", "\\1<br>", x, perl = TRUE)
            }
        )

        # Create named list of label position adjustments for each basin
        basin_adjustments <- stats::setNames(
            lapply(
                snowbull_timeseries$swe$basins$metadata$name,
                function(name) {
                    list(x = 0, y = 0) # Default: no offset
                }
            ),
            snowbull_timeseries$swe$basins$metadata$name
        )

        # Customize specific basins
        basin_adjustments[["Lower_Yukon"]] <- list(x = -100, y = 40)
        basin_adjustments[["Liard"]] <- list(x = -50, y = 60)
        basin_adjustments[["Upper_Yukon"]] <- list(x = -75, y = -10)
        basin_adjustments[["Pelly"]] <- list(x = 0, y = 20)
        basin_adjustments[["Porcupine"]] <- list(x = 120, y = -30)
        basin_adjustments[["Stewart"]] <- list(x = 0, y = 40)
        basin_adjustments[["Teslin_Big_Salmon"]] <- list(x = -70, y = 90)
        basin_adjustments[["Central_Yukon"]] <- list(x = -115, y = 30)
        basin_adjustments[["White"]] <- list(x = 10, y = -20)
        basin_adjustments[["Alsek"]] <- list(x = 0, y = -80)

        # Get basin centroids and apply adjustments
        basin_centroids <- suppressWarnings(sf::st_centroid(
            snowbull_timeseries$swe$basins$metadata
        ))
        basin_coords <- sf::st_coordinates(basin_centroids)
        snowbull_timeseries$swe$basins$metadata$x <- basin_coords[, 1]
        snowbull_timeseries$swe$basins$metadata$y <- basin_coords[, 2]

        snowbull_timeseries$swe$basins$metadata$x_adjusted <- snowbull_timeseries$swe$basins$metadata$x +
            vapply(
                snowbull_timeseries$swe$basins$metadata$name,
                function(n) {
                    if (n %in% names(basin_adjustments)) {
                        basin_adjustments[[n]]$x / 111.32 # convert km -> degrees (approx)
                    } else {
                        0
                    }
                },
                numeric(1)
            )
        snowbull_timeseries$swe$basins$metadata$y_adjusted <- snowbull_timeseries$swe$basins$metadata$y +
            vapply(
                snowbull_timeseries$swe$basins$metadata$name,
                function(n) {
                    if (n %in% names(basin_adjustments)) {
                        basin_adjustments[[n]]$y / 111.32 # convert km -> degrees (approx)
                    } else {
                        0
                    }
                },
                numeric(1)
            )
    } # end load_swe

    if (load_precip) {
        precip_data <- download_continuous_ts(
            con,
            parameter_name = "precipitation, total",
            start_date = "1980-01-01"
        )

        # aggregate to monthly totals (data on 1st of month of sum of previous month)
        monthly_precip <- resample_timeseries(
            precip_data$timeseries$data,
            frequency = "monthly",
            func = "sum"
        )

        ret = calculate_historic_daily_median(
            monthly_precip,
            lookback_start = 1980,
            lookback_end = 2020
        )

        precip_data$timeseries$historic_median <- ret$historic_median
        precip_data$timeseries$relative_to_med <- ret$relative_to_med
        precip_data$timeseries$percentile <- ret$percentile

        snowbull_timeseries$precipitation <- precip_data
    } # end load_precip

    return(snowbull_timeseries)
}

load_bulletin_shapefiles <- function(con) {
    snowbull_shapefiles <- list()

    # If we can read basin polygons from shapefile now (so basin names exist), prefer them
    basins_shp <- sf::st_read(
        system.file(
            "snow_survey/swe_basins/swe_basins.shp",
            package = "YGwater",
            mustWork = TRUE
        ),
        quiet = TRUE
    )

    # Calculate basin areas in square kilometers
    basins_shp$area_km2 <- sf::st_area(basins_shp) |> as.numeric() * 1e-6

    # rename basin name column if needed
    if ("SWE_Basin" %in% names(basins_shp)) {
        names(basins_shp)[names(basins_shp) == "SWE_Basin"] <- "name"
    }

    snowbull_shapefiles$basins <- basins_shp

    prov_sf <- download_spatial_layer(
        con,
        "Provincial/Territorial Boundaries",
        additional_query = "AND feature_name = 'Yukon'"
    )
    # Ensure CRS is WGS84 for leaflet
    if (!is.null(prov_sf) && sf::st_crs(prov_sf)$epsg != 4326) {
        prov_sf <- sf::st_transform(prov_sf, 4326)
    }
    snowbull_shapefiles$yukon <- prov_sf

    bbox_extent <- sf::st_as_sfc(sf::st_bbox(
        c(xmin = -170, xmax = -90, ymin = 50, ymax = 90),
        crs = 4326
    ))
    # Ensure CRS match for st_difference
    if (
        !is.null(snowbull_shapefiles$yukon) &&
            sf::st_crs(bbox_extent) != sf::st_crs(snowbull_shapefiles$yukon)
    ) {
        snowbull_shapefiles$yukon <- sf::st_transform(
            snowbull_shapefiles$yukon,
            sf::st_crs(bbox_extent)
        )
    }
    # Guard against NULL Yukon geometry
    if (!is.null(snowbull_shapefiles$yukon)) {
        snowbull_shapefiles$inverted_yukon <- sf::st_difference(
            bbox_extent,
            sf::st_union(snowbull_shapefiles$yukon)
        )
    } else {
        snowbull_shapefiles$inverted_yukon <- bbox_extent
    }

    roads <- download_spatial_layer(
        con = con,
        layer_name = "Roads",
        additional_query = "AND description IN ('Primary Highway', 'Secondary Highway')"
    )
    # Ensure CRS is WGS84 for leaflet
    if (!is.null(roads) && sf::st_crs(roads)$epsg != 4326) {
        roads <- sf::st_transform(roads, 4326)
    }

    # Clip roads to basin boundaries if both exist
    # Ensure both have the same CRS
    if (sf::st_crs(roads) != sf::st_crs(basins_shp)) {
        roads <- sf::st_transform(roads, sf::st_crs(basins_shp))
    }

    # Buffer basin boundaries by 10km and clip roads to buffered area
    basins_buffered <- sf::st_buffer(sf::st_union(basins_shp), dist = 100000) # 10km in meters
    roads <- sf::st_intersection(roads, basins_buffered)

    snowbull_shapefiles$roads <- roads

    #place_types <- c("City", "Town", "Village") # Filter to major communities only
    keep_communities <- c(
        "Whitehorse",
        "Dawson City",
        "Watson Lake",
        "Haines Junction",
        "Carmacks",
        "Mayo",
        "Pelly Crossing",
        "Ross River",
        "Teslin",
        "Beaver Creek",
        "Carcross",
        "Faro",
        "Old Crow"
    )
    communities <- download_spatial_layer(
        con = con,
        layer_name = "Communities"
    )
    # Ensure CRS is WGS84 for leaflet
    if (!is.null(communities) && sf::st_crs(communities)$epsg != 4326) {
        communities <- sf::st_transform(communities, 4326)
    }

    # Add popup, annotate, and annotation columns to communities
    if (!is.null(communities)) {
        communities$popup <- sprintf(
            "<div style='text-align: left; padding: 10px;'><b>%s</b><br><span style='font-size: 12px; color: #666;'>%s</span></div>",
            htmltools::htmlEscape(communities$feature_name),
            htmltools::htmlEscape(communities$description)
        )
        communities <- communities[
            communities$feature_name %in% keep_communities,
        ]
        #communities$annotate <- communities$description %in% place_types
        communities$annotation <- communities$feature_name

        communities$annotation <- vapply(
            communities$annotation,
            FUN.VALUE = character(1),
            FUN = function(x) {
                if (is.na(x) || x == "") {
                    return(x)
                }
                # Add line break after first word if it's 5+ characters
                sub("^(\\S{5,})\\s+", "\\1<br>", x, perl = TRUE)
            }
        )

        # Create named list of label position adjustments for each community
        community_adjustments <- stats::setNames(
            lapply(communities$feature_name, function(name) {
                list(x = 0, y = 0) # Default: no offset
            }),
            communities$feature_name
        )

        # Customize specific communities
        community_adjustments[["Whitehorse"]] <- list(x = 0, y = 10)
        community_adjustments[["Dawson City"]] <- list(x = 0, y = 0)
        community_adjustments[["Watson Lake"]] <- list(x = 60, y = -55)
        community_adjustments[["Haines Junction"]] <- list(x = -60, y = -70)
        community_adjustments[["Carmacks"]] <- list(x = 20, y = -40)
        community_adjustments[["Mayo"]] <- list(x = 0, y = -40)
        community_adjustments[["Pelly Crossing"]] <- list(x = 65, y = -40)
        community_adjustments[["Ross River"]] <- list(x = 70, y = -10)
        community_adjustments[["Teslin"]] <- list(x = 60, y = 10)
        community_adjustments[["Beaver Creek"]] <- list(x = 80, y = -35)
        community_adjustments[["Burwash Landing"]] <- list(x = 0, y = 0)
        community_adjustments[["Carcross"]] <- list(x = 45, y = -40)
        community_adjustments[["Faro"]] <- list(x = 50, y = -10)
        community_adjustments[["Old Crow"]] <- list(x = 0, y = 10)
        community_adjustments[["Inuvik"]] <- list(x = 0, y = 0)

        # Get coordinates and apply adjustments
        comm_coords <- sf::st_coordinates(communities)

        communities$x <- comm_coords[, 1]
        communities$y <- comm_coords[, 2]
        communities$x_adjusted <- communities$x +
            vapply(
                communities$feature_name,
                function(n) {
                    if (n %in% names(community_adjustments)) {
                        community_adjustments[[n]]$x / 111.32 # convert km -> degrees (approx)
                    } else {
                        0
                    }
                },
                numeric(1)
            )
        communities$y_adjusted <- communities$y +
            vapply(
                communities$feature_name,
                function(n) {
                    if (n %in% names(community_adjustments)) {
                        community_adjustments[[n]]$y / 111.32 # convert km -> degrees (approx)
                    } else {
                        0
                    }
                },
                numeric(1)
            )
    }

    snowbull_shapefiles$communities <- communities

    return(snowbull_shapefiles)
}

# ' Generate HTML content for SWE map popups
#'' @param type Character string indicating station type ("basin", "survey", "pillow")
#' @param swe Numeric SWE value at the station
#' @param relative_to_med Numeric percent of median SWE value
#' @param historic_median Numeric historical median SWE value
#' @param name Character string for station or basin name
#' @param location Optional character string for station location description
#' @param id Optional character string for station ID (used for plot generation)
#' @param percentile Optional numeric percentile value for the station
#' @return Character string containing HTML content for the popup
#' @description
#' Generates HTML content for leaflet map popups displaying SWE information
#' at basins, discrete survey stations, or continuous pillow stations.
#' Includes SWE value, percent of median, historical median, and optional plot button.
#' #'
#' @noRd
generate_popup_content <- function(
    swe,
    relative_to_med = NA,
    historic_median = NA,
    name = "",
    location = "",
    id = "",
    percentile = NA,
    language = "English",
    shiny = TRUE,
    datatype = ""
) {
    # match the datatype label to translation key
    datatype_tr_key <- switch(
        datatype,
        "snow_basin" = "snowbull_swe_basin",
        "snow_survey" = "snowbull_snow_survey",
        "snow_pillow" = "snowbull_snow_pillow",
        "unknown"
    )
    datatype_label <- paste0(
        "<b>Type:</b>",
        tr(datatype_tr_key, language),
        "<br>"
    )

    name <- gsub("_", " ", name)

    # if it's shiny, add a plot button (plot functions are located in the Shiny script)
    plot_button <- if (shiny) {
        paste(
            "<button onclick='generatePlot(\"",
            datatype,
            "\", \"",
            id,
            "\", \"",
            name,
            "\")' style='margin-top: 12px; font-size: 16px; padding: 10px 18px; border-radius: 6px; background-color: #337ab7; color: #fff; border: none; cursor: pointer; display: flex; align-items: center;'>",
            # SVG icon of a bar chart
            "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20' style='margin-right: 8px; vertical-align: middle;' viewBox='0 0 20 20'><rect x='3' y='10' width='3' height='7' fill='#fff' stroke='#337ab7' stroke-width='1'/><rect x='8' y='6' width='3' height='11' fill='#fff' stroke='#337ab7' stroke-width='1'/><rect x='13' y='2' width='3' height='15' fill='#fff' stroke='#337ab7' stroke-width='1'/></svg>",
            "</button>",
            sep = ""
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
        datatype_label,
        "<br>",
        "<b>SWE Value:</b> ",
        if (!is.na(swe)) paste0(round(swe, 1), " mm") else "No data",
        "<br>",
        "<b>",
        tr("snowbull_relative_median", language),
        ":</b> ",
        if (!is.na(relative_to_med)) {
            paste0(round(relative_to_med, 1), "% of normal")
        } else {
            "No data"
        },
        "<br>",
        "<b>",
        tr("snowbull_historical_median", language),
        ":</b> ",
        if (!is.na(historic_median)) {
            paste0(round(historic_median, 1), " mm")
        } else {
            "No data"
        },
        "<br>",
        "<b>",
        tr("snowbull_percentile", language),
        ":</b> ",
        if (!is.null(percentile) && !is.na(percentile)) {
            paste0(round(percentile, 1), "th percentile")
        } else {
            "No data"
        },
        "<br>",
        plot_button,
        "</div>"
    )
}


# Filter out stations with no recent data (more than 1 year old)
filter_stations_by_latest_date <- function(df, input_date, cutoff_days) {
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


#' Get SWE values for inputted type at basins, surveys, and pillows for a given year and month. Updates data fields and popup content.
#'
#' @param year Integer year for data extraction
#' @param month Integer month for data extraction
#' @param snowbull_timeseries List containing all loaded base data
#' @param shiny Logical indicating if running in Shiny app context (default TRUE)
#' @return A list containing processed SWE data at basins, surveys, and pillows
#'
#' @description
#' Extracts SWE data at basins, discrete survey stations, and continuous pillow stations
#' for the specified year and month. Formats data for mapping, including
#' popup content generation.
#'
#' @noRd
get_display_data <- function(
    dataset,
    year,
    month,
    statistic,
    shiny = TRUE,
    language = "English"
) {
    lang <- shortenLanguage(language)

    input_date <- get_datetime(year, month)

    # convert from [timeseries, shp] to shp with timeseries data for a given year/month
    dataset_state <- get_state_as_shp(
        data = dataset,
        year = year,
        month = month
    )

    # generate popup content for each station/basin
    dataset_state$popup_content <- mapply(
        function(
            data,
            relative_to_med,
            historic_median,
            percentile
        ) {
            generate_popup_content(
                swe = data,
                relative_to_med = relative_to_med,
                historic_median = historic_median,
                percentile = percentile,
                language = language,
                shiny = shiny,
                datatype = dataset$label
            )
        },
        dataset_state$data,
        dataset_state$relative_to_med,
        dataset_state$historic_median,
        dataset_state$percentile,
        SIMPLIFY = FALSE
    )

    # if point-source data, remove stations with no recent data
    if (dataset$geom == "point") {
        dataset_state <- filter_stations_by_latest_date(
            dataset_state,
            input_date,
            366
        )
    }

    dataset_state$value_to_show <- dataset_state[[statistic]]

    # Ensure all swe_at_* columns are numeric for each data frame
    to_numeric_cols <- function(df, cols) {
        for (col in cols) {
            if (col %in% names(df)) {
                df[[col]] <- as.numeric(df[[col]])
            }
        }
        df
    }

    num_cols <- c("data", "relative_to_med", "historic_median", "percentile")
    dataset_state <- to_numeric_cols(dataset_state, num_cols)

    # update the annotations to display the value for the selected type
    dataset_state$annotation <- paste0(
        dataset_state$annotation,
        "<br>(",
        round(dataset_state[[statistic]], 0),
        "%)"
    )
    return(dataset_state)
}


# ' Create a Leaflet map visualizing SWE data
#' @param data List containing processed SWE data at basins, surveys, and pillows
#' @param statistic Character string indicating which SWE value to visualize
#' (e.g., "data", "relative_to_med", "percentile")
#' @param language Character string indicating the language for labels and legends. Default is "English".
#' @param snowbull_timeseries List containing all loaded base data, returned from load_bulletin_data()
#' @param snowbull_shapefiles List containing all loaded shapefiles, returned from load_bulletin_shapefiles()
#' @param month Integer month for map title
#' @param year Integer year for map title
#' @return A Leaflet map object visualizing the SWE data'
#' @description
#' Creates a Leaflet map visualizing SWE data at basins, discrete survey stations,
#' and continuous pillow stations. The map includes styled polygons and markers,
#' labels, popups, and a dynamic legend based on the selected value type and date.
#' @noRd
make_leaflet_map <- function(
    point_data = NULL,
    poly_data = NULL,
    point_data_secondary = NULL,
    snowbull_shapefiles,
    statistic = NULL,
    language = "English",
    month = NULL,
    year = NULL,
    filename = NULL
) {
    month_str <- snowbull_months(month = month, short = TRUE)

    # processed data is values for one point in time, based on bulletin_data
    static_style_elements <- get_static_style_elements()

    # load style elements based on value_type
    # language used for legend text
    dynamic_style_elements <- get_dynamic_style_elements(
        statistic = statistic,
        language = language
    )

    # legend created dynamically based on inputs
    legend_title <- paste0(
        "<b>",
        tr("maps_snowbull", language),
        "</b><br>",
        switch(
            statistic,
            "relative_to_med" = tr("snowbull_relative_median", language),
            "data" = tr("snowbull_swe", language),
            "percentile" = tr("snowbull_percentile", language),
            ""
        ),
        "<br>",
        tr(month_str, language),
        " ",
        year
    )

    # bbox for leaflet plotting extent
    bbox <- c(xmin = -141, ymin = 60, xmax = -123, ymax = 69.6)

    # --- Main leaflet map output ---
    # some objects are plotted several times for aesthetics (have a white border, black border, and separate labels)
    # similarly, communities are split into two shapefiles, to accomodate different zoom level visibility
    # labels are plotted seperately since their positions are adjusted based on hardcoded offsets
    m <- leaflet::leaflet(
        options = leaflet::leafletOptions(
            zoomDelta = static_style_elements$leaflet$zoomDelta %||% 0.5,
            zoomSnap = static_style_elements$leaflet$zoomSnap %||% 0.25,
            wheelPxPerZoomLevel = static_style_elements$leaflet$wheelPxPerZoomLevel %||%
                120
        )
    ) %>%
        leaflet::addProviderTiles(
            leaflet::providers$Esri.WorldImagery,
            group = "Topographic"
        ) %>%
        leaflet::fitBounds(
            as.numeric(bbox["xmin"]),
            as.numeric(bbox["ymin"]),
            as.numeric(bbox["xmax"]),
            as.numeric(bbox["ymax"])
        )

    if (!is.null(poly_data)) {
        m <- m %>%
            leaflet::addPolygons(
                data = poly_data,
                fillColor = ~ get_state_style_elements(
                    value_to_show,
                    dynamic_style_elements
                ),
                color = static_style_elements$basins$color,
                weight = 2 * static_style_elements$basins$weight,
                opacity = static_style_elements$basins$opacity,
                fillOpacity = static_style_elements$basins$fillOpacity,
                label = ~ lapply(annotation, htmltools::HTML),
                popup = ~ lapply(popup_content, htmltools::HTML),
                popupOptions = leaflet::popupOptions(
                    maxWidth = static_style_elements$basins$popupOptions$maxWidth,
                    closeButton = static_style_elements$basins$popupOptions$closeButton,
                    autoPan = static_style_elements$basins$popupOptions$autoPan
                ),
                group = "Basins averages"
            ) %>%
            leaflet::addLabelOnlyMarkers(
                data = data$states_basins,
                lng = data$states_basins$x_adjusted,
                lat = data$states_basins$y_adjusted,
                label = lapply(data$states_basins$annotation, htmltools::HTML),
                labelOptions = leaflet::labelOptions(
                    noHide = static_style_elements$basins$labelOptions$noHide %||%
                        TRUE,
                    direction = static_style_elements$basins$labelOptions$direction %||%
                        "center",
                    textOnly = static_style_elements$basins$labelOptions$textOnly %||%
                        TRUE,
                    style = static_style_elements$basins$label
                ),
                group = "Basins averages"
            )
    }
    leaflet::addPolygons(
        data = snowbull_shapefiles$basins,
        fill = FALSE,
        color = "black",
        weight = 0.5 * static_style_elements$basins$weight,
        opacity = static_style_elements$basins$opacity,
        fillOpacity = 0,
        label = NULL,
        popup = NULL,
        group = "Basins averages"
    )

    if (!is.null(point_data)) {
        m <- m %>%
            leaflet::addCircleMarkers(
                data = point_data,
                radius = static_style_elements$surveys$radius,
                color = static_style_elements$surveys$color,
                fillColor = ~ get_state_style_elements(
                    value_to_show,
                    dynamic_style_elements
                ),
                weight = static_style_elements$surveys$weight,
                opacity = static_style_elements$surveys$opacity,
                fillOpacity = static_style_elements$surveys$fillOpacity,
                label = ~ lapply(
                    paste0(name, "<br>", location),
                    htmltools::HTML
                ),
                popup = ~ lapply(popup_content, htmltools::HTML),
                popupOptions = leaflet::popupOptions(
                    maxWidth = static_style_elements$basins$popupOptions$maxWidth,
                    closeButton = static_style_elements$basins$popupOptions$closeButton,
                    autoPan = static_style_elements$basins$popupOptions$autoPan
                ),
                group = "Snow surveys (discrete)"
            )
    }
    {
        if (!is.null(snowbull_shapefiles$roads)) {
            m <- m %>%
                leaflet::addPolylines(
                    .,
                    data = snowbull_shapefiles$roads,
                    color = static_style_elements$roads$color,
                    weight = static_style_elements$roads$weight,
                    opacity = static_style_elements$roads$opacity,
                    group = "Roads"
                )
        } else {
            .
        }
    }
    if (!is.null(point_data_secondary)) {
        m <- m %>%
            leaflet::addMarkers(
                data = point_data_secondary,
                icon = leaflet::icons(
                    iconUrl = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16'><rect x='2' y='2' width='12' height='12' fill='none' stroke='black' stroke-width='2'/></svg>",
                    iconWidth = 2.7 * static_style_elements$pillows$radius,
                    iconHeight = 2.7 * static_style_elements$pillows$radius
                ),
                label = ~ lapply(
                    paste0(name, "<br>", location),
                    htmltools::HTML
                ),
                popup = ~ lapply(popup_content, htmltools::HTML),
                popupOptions = do.call(
                    leaflet::popupOptions,
                    static_style_elements$basins$popupOptions
                ),
                group = "Snow pillows (continuous)"
            ) %>%
            leaflet::addCircleMarkers(
                data = point_data_secondary,
                radius = static_style_elements$pillows$radius,
                color = static_style_elements$pillows$color,
                fillColor = ~ get_state_style_elements(
                    value_to_show,
                    dynamic_style_elements
                ),
                weight = static_style_elements$pillows$weight,
                opacity = static_style_elements$pillows$opacity,
                fillOpacity = static_style_elements$pillows$fillOpacity,
                label = ~ lapply(
                    paste0(name, "<br>", location),
                    htmltools::HTML
                ),
                popup = ~ lapply(popup_content, htmltools::HTML),
                popupOptions = leaflet::popupOptions(
                    maxWidth = static_style_elements$basins$popupOptions$maxWidth,
                    closeButton = static_style_elements$basins$popupOptions$closeButton,
                    autoPan = static_style_elements$basins$popupOptions$autoPan
                ),
                group = "Snow pillows (continuous)"
            )
    }

    {
        if (!is.null(snowbull_shapefiles$yukon)) {
            m <- m %>%
                leaflet::addPolygons(
                    .,
                    data = snowbull_shapefiles$yukon,
                    color = static_style_elements$boundary$color,
                    weight = static_style_elements$boundary$weight,
                    fill = static_style_elements$boundary$fill,
                    group = "Boundary"
                )
        } else {
            .
        }
    } %>%
        {
            communities_split <- split_communities(
                snowbull_shapefiles$communities
            )

            . <- leaflet::addMarkers(
                .,
                data = communities_split$large,
                icon = static_style_elements$communities$icon,
                label = ~ lapply(annotation, htmltools::HTML),
                popup = ~ lapply(popup, htmltools::HTML),
                popupOptions = do.call(
                    leaflet::popupOptions,
                    static_style_elements$basins$popupOptions
                ),
                group = c("Communities_large")
            )
            . <- leaflet::addLabelOnlyMarkers(
                .,
                data = communities_split$large,
                lng = communities_split$large$x,
                lat = communities_split$large$y,
                label = ~ lapply(
                    communities_split$large$annotation,
                    htmltools::HTML
                ),
                labelOptions = static_style_elements$communities$labelOptions,
                group = c("Communities_large")
            )
            . <- leaflet::addMarkers(
                .,
                data = communities_split$small,
                icon = static_style_elements$communities$icon,
                label = ~ lapply(annotation, htmltools::HTML),
                popup = ~ lapply(popup, htmltools::HTML),
                popupOptions = do.call(
                    leaflet::popupOptions,
                    static_style_elements$basins$popupOptions
                ),
                group = c("Communities_small")
            )
            . <- leaflet::addLabelOnlyMarkers(
                .,
                data = communities_split$small,
                lng = communities_split$small$x,
                lat = communities_split$small$y,
                label = ~ lapply(
                    communities_split$small$annotation,
                    htmltools::HTML
                ),
                labelOptions = static_style_elements$communities$labelOptions,
                group = c("Communities_small")
            )
            .
        } %>%
        leaflet::groupOptions(
            "Communities_large",
            zoomLevels = seq(7, 18, 0.25)
        ) %>%
        leaflet::groupOptions(
            "Communities_small",
            zoomLevels = seq(8, 18, 0.25)
        ) %>%
        leaflet::addControl(
            # here we specify a dummy HTML legend since it's much easier than the alternative.
            # we grab some style elements from the static styles to keep it consistent, but this isn't possible for all cases
            html = paste0(
                "<div style='padding: 8px; border-radius: 6px; font-size: 13px; line-height: 1.4; min-width: 140px;'>",
                "<b>",
                tr("snowbull_symbols", language),
                "</b><br>",
                "<svg width='18' height='18' style='vertical-align:middle;'><circle cx='9' cy='9' r='7' fill='none' stroke='black' stroke-width='2'/></svg> ",
                tr("snowbull_snow_survey", language),
                "<br>",
                "<svg width='18' height='18' style='vertical-align:middle;'><rect x='3' y='3' width='12' height='12' fill='none' stroke='black' stroke-width='2'/><circle cx='9' cy='9' r='5' fill='none' stroke='black' stroke-width='2'/></svg> ",
                tr("snowbull_snow_pillow", language),
                "<br>",
                sprintf(
                    "<svg width='18' height='18' style='vertical-align:middle;'><line x1='2' y1='16' x2='16' y2='2' style='stroke:%s;stroke-width:%d'/></svg> ",
                    static_style_elements$roads$color,
                    static_style_elements$roads$weight
                ),
                tr("snowbull_roads", language),
                "<br>",
                "<svg width='18' height='18' style='vertical-align:middle;'><polygon points='9,2 16,9 9,16 2,9' fill='black' stroke='white' stroke-width='2'/></svg> ",
                tr("snowbull_communities", language),
                "<br>",
                "</div>"
            ),
            position = "bottomright"
        ) %>%
        leaflet::addLegend(
            position = "bottomright",
            colors = dynamic_style_elements$colors,
            title = legend_title,
            labels = dynamic_style_elements$labels,
            opacity = 1
        )

    if (!is.null(filename)) {
        cat(sprintf("Saving map to file: %s\n", filename))
        requireNamespace("pandoc")

        loc <- pandoc::pandoc_locate()
        if (is.null(loc)) {
            stop("Pandoc installation not found. Please install pandoc.")
        }

        htmlwidgets::saveWidget(
            m,
            file = filename,
            selfcontained = TRUE
        )
    }

    return(m)
}

make_ggplot_map <- function(
    point_data = NULL,
    poly_data = NULL,
    point_data_secondary = NULL,
    statistic = NULL,
    snowbull_shapefiles,
    language = "English",
    month = NULL,
    year = NULL,
    filename = NULL,
    dpi = 300,
    height = 8,
    width = 8
) {
    requireNamespace("ggplot2")
    requireNamespace("shadowtext")

    # processed data is values for one point in time, based on bulletin_data
    static_style_elements <- get_static_style_elements()

    # load style elements based on value_type
    # language used for legend text
    dynamic_style_elements <- get_dynamic_style_elements(
        statistic = statistic,
        language = language
    )

    # Create the base plot
    p <- ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::theme(
            legend.position = "right",
            plot.title = ggplot2::element_text(
                hjust = 0.5,
                size = 14,
                face = "bold"
            ),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
        )

    # Add Yukon boundary background (underneath everything except basins)
    if (!is.null(snowbull_shapefiles$yukon)) {
        p <- p +
            ggplot2::geom_sf(
                data = snowbull_shapefiles$yukon,
                fill = "#F5F5DC", # Beige terrain-like color
                color = "black",
                size = 1.5,
                alpha = 0.5 # 50% transparent background
            )
    }

    # Add basin labels using adjusted coordinates
    if (!is.null(poly_data)) {
        basin_labels_df <- data.frame(
            x = poly_data$x_adjusted,
            y = poly_data$y_adjusted,
            annotation = poly_data$annotation
        )
        basin_labels_df$annotation <- gsub(
            "<br>",
            "\n",
            basin_labels_df$annotation
        )

        p <- p +
            ggplot2::geom_sf(
                data = poly_data,
                fill = poly_data$fill_colour,
                size = static_style_elements$basins$weight * 0.25,
                alpha = static_style_elements$basins$fillOpacity
            )
    }

    # Add roads (below stations)
    if (!is.null(snowbull_shapefiles$roads)) {
        p <- p +
            ggplot2::geom_sf(
                data = snowbull_shapefiles$roads,
                color = static_style_elements$roads$color,
                size = static_style_elements$roads$weight * 0.15,
                alpha = static_style_elements$roads$opacity
            )
    }

    p <- p +
        ggplot2::geom_sf(
            data = snowbull_shapefiles$basins,
            color = "white",
            size = static_style_elements$basins$weight * 0.6
        )
    p <- p +
        ggplot2::geom_sf(
            data = snowbull_shapefiles$basins,
            color = "black",
            size = static_style_elements$basins$weight * 0.25
        )

    if (!is.null(poly_data)) {
        p <- p +
            shadowtext::geom_shadowtext(
                data = basin_labels_df,
                x = basin_labels_df$x,
                y = basin_labels_df$y,
                label = basin_labels_df$annotation,
                size = 2.25,
                fontface = "bold",
                color = static_style_elements$basins$label$color,
                bg.color = "white",
                bg.r = 0.2
            )
    }

    if (!is.null(point_data)) {
        p <- p +
            ggplot2::geom_point(
                data = point_data,
                ggplot2::aes(
                    x = .data$x,
                    y = .data$y
                ),
                fill = point_data$fill_colour,
                color = static_style_elements$surveys$color,
                size = static_style_elements$surveys$radius / 2.5,
                shape = 21,
                stroke = static_style_elements$surveys$weight * 0.5
            )
    }

    # if (!is.null(point_data_secondary)) {
    #     p <- p +
    #         ggplot2::geom_point(
    #             data = point_data_secondary,
    #             ggplot2::aes(
    #                 x = .data$x,
    #                 y = .data$y
    #             ),
    #             fill = point_data_secondary$fill_colour,
    #             color = static_style_elements$surveys$color,
    #             size = static_style_elements$surveys$radius / 2.5,
    #             shape = 22,
    #             stroke = static_style_elements$surveys$weight * 0.5
    #         )
    # }

    # Add communities using pre-calculated adjusted coordinates
    if (!is.null(snowbull_shapefiles$communities)) {
        comm_coords <- sf::st_coordinates(snowbull_shapefiles$communities)
        communities_df <- data.frame(
            x = comm_coords[, 1],
            y = comm_coords[, 2],
            name = snowbull_shapefiles$communities$feature_name,
            annotation = gsub(
                "<br>",
                "\n",
                snowbull_shapefiles$communities$annotation
            ),
            x_adjust = snowbull_shapefiles$communities$x_adjusted,
            y_adjust = snowbull_shapefiles$communities$y_adjusted
        )

        p <- p +
            ggplot2::geom_point(
                data = communities_df,
                ggplot2::aes(x = .data$x, y = .data$y),
                fill = "black",
                size = static_style_elements$communities$iconWidth / 8,
                shape = 18
            ) +
            shadowtext::geom_shadowtext(
                data = communities_df,
                ggplot2::aes(
                    x = .data$x_adjust,
                    y = .data$y_adjust,
                    label = .data$annotation
                ),
                size = 2,
                fontface = "bold.italic",
                color = static_style_elements$communities$labelColor,
                bg.color = "white",
                bg.r = 0.1,
                vjust = -0.5,
                hjust = 0.5,
                family = "serif"
            )
    }

    # Add colormap legend for SWE (relative_to_med)
    # Create legend data dynamically based on actual data bins
    bin_ranges <- static_style_elements$relative_bins[
        -length(static_style_elements$relative_bins)
    ]
    bin_labels <- character(length(bin_ranges))

    for (i in seq_along(bin_ranges)) {
        if (i == 1) {
            bin_labels[i] <- sprintf("< %d%%", bin_ranges[i + 1])
        } else if (i == length(bin_ranges)) {
            bin_labels[i] <- sprintf("> %d%%", bin_ranges[i])
        } else {
            bin_labels[i] <- sprintf(
                "%d-%d%%",
                bin_ranges[i],
                bin_ranges[i + 1]
            )
        }
    }

    # swe_legend_df <- data.frame(
    #     bin = seq_along(bin_ranges),
    #     color = static_style_elements$relative_colors,
    #     label = bin_labels,
    #     stringsAsFactors = FALSE
    # )
    month_name_short <- snowbull_months(
        month = as.numeric(month),
        short = TRUE
    )
    # Compose title and subtitle for ggplot (no HTML tags, no <br>, no <b>)
    title <- tr("maps_snowbull", language)
    subtitle <- paste0(
        switch(
            statistic,
            "relative_to_med" = tr("snowbull_relative_median", language),
            "data" = tr("snowbull_swe", language),
            "percentile" = tr("snowbull_percentile", language),
            ""
        ),
        " - ",
        tr(month_name_short, language),
        " ",
        year
    )

    # Add a dummy legend showing fill colour bins
    legend_df <- data.frame(
        color = dynamic_style_elements$colors,
        label = dynamic_style_elements$labels,
        stringsAsFactors = FALSE
    )
    # Ensure label is a factor with levels in the correct order
    legend_df$label <- factor(
        legend_df$label,
        levels = dynamic_style_elements$labels
    )

    p <- p +
        ggplot2::labs(
            title = title,
            subtitle = subtitle
        ) +
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
            legend.position = c(0.7, 0.8), # move legend to left and up
            legend.justification = c("left", "top")
        )

    # Add a dummy invisible geom for legend
    p <- p +
        ggplot2::geom_point(
            data = legend_df,
            ggplot2::aes(x = Inf, y = Inf, fill = label),
            shape = 21,
            size = 5,
            show.legend = TRUE
        ) +
        ggplot2::scale_fill_manual(
            name = subtitle,
            values = setNames(legend_df$color, levels(legend_df$label)),
            drop = FALSE,
            guide = ggplot2::guide_legend(
                override.aes = list(shape = 21, size = 5)
            )
        )
    # # Add coordinate system
    # # Calculate basin extents with 50km buffer
    # basin_bbox <- sf::st_bbox(snowbull_shapefiles$basins)
    # yukon_bbox <- sf::st_bbox(snowbull_shapefiles$yukon)
    # basin_bbox <- sf::st_bbox(c(
    #     xmin = min(basin_bbox["xmin"], yukon_bbox["xmin"]),
    #     xmax = max(basin_bbox["xmax"], yukon_bbox["xmax"]),
    #     ymin = min(basin_bbox["ymin"], yukon_bbox["ymin"]),
    #     ymax = max(basin_bbox["ymax"], yukon_bbox["ymax"])
    # ))

    # buffer_degrees <- 50 / 111.32 # Convert 50km to degrees (approx 111.32 km per degree)

    # p <- p +
    #     ggplot2::coord_sf(
    #         crs = 4326,
    #         xlim = c(
    #             basin_bbox["xmin"] - buffer_degrees,
    #             basin_bbox["xmax"] + buffer_degrees
    #         ),
    #         ylim = c(
    #             basin_bbox["ymin"] - buffer_degrees,
    #             basin_bbox["ymax"] + buffer_degrees
    #         )
    #     )

    if (!is.null(filename)) {
        cat(sprintf("Saving ggplot map to to file: %s\n", filename))

        ggplot2::ggsave(
            filename = filename,
            plot = p,
            width = width,
            height = height,
            units = "in",
            dpi = dpi
        )
    }
    return(p)
}


#' Create a static ggplot2 map for SWE basins and stations
#'
#' @param year Integer year (e.g., 2025)
#' @param month Integer month (e.g., 3 for March)
#' @param snowbull_data Optional preloaded snowbull_data from \code{load_bulletin_data()}, otherwise loads from default connection
#' @param width Numeric width of the plot in inches (default: 12)
#' @param height Numeric height of the plot in inches (default: 8)
#' @param filename Optional character string for PNG output file path
#' @param dpi Numeric resolution in dots per inch (default: 300)
#' @param parameter_name Character, parameter to plot (default: "swe")
#' @param statistic Character, "absolute", "relative", or "percentile" (default: "relative")
#' @param language Character string indicating the language for labels and legends. Default is "English".
#' (default: "English")
#' @return A ggplot2 object with SWE basins and stations
#'
#' @description
#' Creates a publication-ready static map showing SWE conditions across the Yukon Territory.
#' Optimized for high-resolution output and professional presentation.
#'
#' @details
#' The static map features:
#' \itemize{
#'   \item High-quality terrain background
#'   \item SWE basins with color-coded values and labels
#'   \item Station markers with consistent symbology
#'   \item Communities with optimally positioned labels
#'   \item Roads and territorial boundaries
#'   \item Professional typography and layout
#'   \item Proper coordinate system and extent
#' }
#'
#' Label positions are pre-calculated and adjusted to minimize overlap and
#' maximize readability across different map extents.
#'
#' @export

make_snowbull_map <- function(
    year,
    month,
    filename = NULL,
    snowbull_shapefiles = NULL,
    snowbull_timeseries = NULL,
    width = 12,
    height = 8,
    dpi = 300,
    parameter_name = "swe",
    statistic = "relative_to_med",
    language = "English",
    con = NULL,
    format = "ggplot"
) {
    language <- lengthenLanguage(language)

    # Load required packages
    requireNamespace("sf")
    requireNamespace("stats")

    parameter_name <- standardize_parameter_name(parameter_name)

    STATISTICS <- c("data", "relative_to_med", "percentile")
    statistic <- match.arg(
        statistic,
        choices = STATISTICS
    )

    FORMATS <- c("ggplot", "leaflet", "shiny")
    format <- match.arg(
        format,
        choices = FORMATS
    )

    dynamic_style_elements <- get_dynamic_style_elements(
        statistic = statistic,
        language = language
    )

    static_style_elements <- get_static_style_elements()

    if (is.null(con)) {
        con <- AquaCache::AquaConnect(
            name = "aquacache",
            host = "10.250.12.154",
            port = 5432,
            user = "public_reader",
            password = "aquacache"
        )
        on.exit(DBI::dbDisconnect(con))
    }

    # Load snowbull_data if not provided
    if (is.null(snowbull_timeseries)) {
        snowbull_timeseries <- load_bulletin_timeseries(
            con,
            load_swe = parameter_name == "snow water equivalent",
            load_precip = parameter_name == "precipitation, total",
            load_temp = parameter_name == "air temperature"
        )
    }

    # Load snowbull_data if not provided
    if (is.null(snowbull_shapefiles)) {
        snowbull_shapefiles <- load_bulletin_shapefiles(
            con
        )
    }

    switch(
        parameter_name,
        "snow water equivalent" = {
            timeseries_data <- list(
                poly_data = snowbull_timeseries$swe$basins,
                point_data = snowbull_timeseries$swe$surveys,
                point_data_secondary = snowbull_timeseries$swe$pillows
            )
        },
        "precipitation, total" = {
            timeseries_data <- list(
                poly_data = NULL,
                point_data = snowbull_timeseries$precipitation,
                point_data_secondary = NULL
            )
        },
        "air temperature" = {
            timeseries_data <- list(
                poly_data = NULL,
                point_data = snowbull_shapefiles$surveys_temp,
                point_data_secondary = NULL
            )
        },
        stop("Unsupported parameter_name: ", parameter_name)
    )

    # get the 'current' data for the specified date, and create the popup data
    # returns list of sf objects with data columns
    map_data <- list(
        point_data = NULL,
        point_data_secondary = NULL,
        poly_data = NULL
    )
    for (data_type in names(map_data)) {
        if (!is.null(timeseries_data[[data_type]])) {
            map_data[[data_type]] <- get_display_data(
                year = year,
                month = month,
                dataset = timeseries_data[[data_type]],
                shiny = FALSE,
                statistic = statistic,
                language = language
            )

            map_data[[data_type]]$fill_colour <- get_state_style_elements(
                map_data[[data_type]]$value_to_show,
                style_elements = dynamic_style_elements
            )
        }
    }

    switch(
        format,
        "leaflet" = ,
        "shiny" = {
            return(make_leaflet_map(
                point_data = map_data$point_data,
                poly_data = map_data$poly_data,
                point_data_secondary = map_data$point_data_secondary,
                snowbull_shapefiles = snowbull_shapefiles,
                language = language,
                statistic = statistic,
                month = month, # month and year for title only; data is already good to go
                year = year,
                filename = filename
            ))
        },
        "ggplot" = {
            return(make_ggplot_map(
                point_data = map_data$point_data,
                poly_data = map_data$poly_data,
                point_data_secondary = map_data$point_data_secondary,
                snowbull_shapefiles = snowbull_shapefiles,
                language = language,
                statistic = statistic,
                month = month,
                year = year,
                filename = filename
            ))
        },
        stop("Unknown format: ", format)
    )
}

con <- AquaCache::AquaConnect(
    name = "aquacache",
    host = "10.250.12.154",
    port = 5432,
    user = "public_reader",
    password = "aquacache"
)

year <- 2025
month <- 3
language <- "English"
statistic <- "relative_to_med"
parameter_name <- "swe"

make_snowbull_map(
    year = 2025,
    month = 3,
    filename = "swe_map_march2025.html",
    format = "leaflet",
    parameter_name = "swe",
    statistic = "relative_to_med",
    language = "English"
)
