# ============================================================================
#
# SWE bulletin utilities for loading data and setting up map visualizations
# Date created: Nov 2025
# Author: esnieder

# Main mapping function (date, parameter, statistic)
# -- 'load_snowbull_shapefiles' Load static spatial layers from database
# -- 'load_snowbull_timeseries' Load hydromet timeseries data from database
# ---- 'get_norms()' Compute norms for each timeseries
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
#' @keywords internal
#' @noRd
#' @keywords internal
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
#' @keywords internal
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
            fillOpacity = 1,
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
            labelFontSize = "18px",
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


#' Generate dynamic style elements for visualization
#'
#' @description
#' Creates color schemes, bins, and labels for different SWE statistics based on
#' the selected statistic type and language.
#'
#' @param statistic Character string indicating the type of statistic to style.
#'   Options are "relative_to_med", "data", "percentile", or "anomalies".
#'   Defaults to "relative_to_med".
#' @param param_name Character string for parameter name (e.g., "snow water equivalent", "precipitation, total").
#' @param language Character string for language. Defaults to "English".
#'
#' @return A list containing bins, colors, and labels for the specified statistic:
#' \describe{
#'   \item{bins}{Numeric vector of bin boundaries}
#'   \item{colors}{Character vector of color hex codes}
#'   \item{labels}{Character vector of human-readable labels}
#' }
#'
#' @details
#' The function provides different styling schemes:
#' \itemize{
#'   \item relative_to_med: Percentage of normal SWE with special cases for zero values
#'   \item data: Absolute SWE values in millimeters
#'   \item percentile: Percentile rankings from 0-100
#'   \item anomalies: Deviation from normal in standard deviation units
#' }
#'
#' @keywords internal
#' @noRd

get_dynamic_style_elements <- function(
    statistic = NULL,
    param_name = "snow water equivalent",
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

    param_name <- standardize_param_name(param_name)

    # standardize parameter name for upcoming switch case
    # param_name <- standardize_param_name(param_name)

    if (is.null(statistic)) {
        statistic <- "relative_to_med"
    }

    # Color scheme and visualization parameters
    # Bins represent percentage of normal SWE (relative_to_med values)
    relative_bins <- c(-2, -1, 0, 50, 70, 90, 110, 130, 150, Inf)
    relative_colors <- c(
        "gray", # Gray (NA/NaN values)
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

    anomalies_bins <- c(-Inf, -5, -2, -0.4, 0.5, 2, 5, Inf)
    anomalies_colors <- c(
        "gray", # Gray (NA/NaN values)
        "#6772F7", # Blue (extremely low)
        "#85B4F8", # Light blue (very low)
        "#8CEFE1", # Cyan (low)
        "#6CFC88", # Green (well below normal)
        "#C1FB80", # Light green (below normal)
        "#EEE383", # Yellow (above normal)
        "#EBB966" # Orange (well above normal)
    )

    absolute_bins <- c(0, 50, 100, 150, 200, 250, 300, 400, 500, Inf)
    absolute_colors <- c(
        "gray", # Gray (NA/NaN values)
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
        "gray", # Gray (NA/NaN values)
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
    relative_labels <- c(
        tr("snowbull_no_data", language), # "No data"
        tr("snowbull_no_snow", language), # "No snow present where historical median is zero"
        tr("snowbull_some_snow", language), # "Snow present where historical median is zero"
        "x ≤ 50%",
        "50% < x ≤ 70%",
        "70% < x ≤ 90%",
        "90% < x ≤ 110%",
        "110% < x ≤ 130%",
        "130% < x ≤ 150%",
        "x > 150%"
    )

    anomalies_labels <- c(
        tr("snowbull_no_data", language), # "No data"
        "x ≤ -5.0",
        "-5.0 < x ≤ -2.0",
        "-2.0 < x ≤ -0.4",
        "-0.4 < x ≤ +0.5",
        "+0.5 < x ≤ +2.0",
        "+2.0 < x ≤ +5.0",
        "x > +5.0"
    )

    absolute_labels <- c(
        tr("snowbull_no_data", language), # "No data"
        "x ≤ 50",
        "50 < x ≤ 100",
        "100 < x ≤ 150 ",
        "150 < x ≤ 200",
        "200 < x ≤ 250",
        "250 < x ≤ 300",
        "300 < x ≤ 400",
        "400 < x ≤ 500",
        "x > 500"
    )
    percentile_labels <- c(
        tr("snowbull_no_data", language), # "No data"
        "x ≤ 10",
        "10 < x ≤ 20",
        "20 < x ≤ 30",
        "30 < x ≤ 40",
        "40 < x ≤ 50",
        "50 < x ≤ 60",
        "60 < x ≤ 70",
        "70 < x ≤ 80",
        "80 < x ≤ 90",
        "90 < x ≤ 100"
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
        ),
        anomalies = list(
            bins = anomalies_bins,
            colors = anomalies_colors,
            labels = anomalies_labels
        )
    )

    # for precip, remove the 'no snow' and 'snow present' edge cases
    if (param_name == "precipitation, total") {
        style_choices$relative_to_med <- list(
            bins = relative_bins[-c(2, 3)], # remove no snow / some snow bins
            colors = relative_colors[-c(2, 3)], # remove no snow / some snow colors
            labels = relative_labels[-c(2, 3)] # remove no snow / some snow labels
        )
    }

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


#' Standardize parameter name
#' @param epsg Integer or character EPSG code
#' @return Standardized integer EPSG code
#' @noRd
standardize_epsg <- function(epsg) {
    if (is.null(epsg)) {
        stop("epsg must be provided and non-null")
    }
    # If epsg is character and matches a number, extract the number
    if (is.character(epsg) && length(epsg) == 1) {
        # Extract number from string (e.g., "EPSG:4326" or "4326")
        m <- regmatches(epsg, regexpr("[0-9]{3,5}", epsg))
        if (length(m) == 1 && nzchar(m)) {
            epsg <- as.integer(m)
        } else {
            stop("Could not extract EPSG code from string")
        }
    }
    if (!is.numeric(epsg) || length(epsg) != 1 || is.na(epsg)) {
        stop("epsg must be a single integer or string containing an EPSG code")
    }
    return(epsg)
}


#' Standardize parameter name
#' @param param_name Character string of parameter name
#' @param con (Optional) AquaCache connection
#' @return parameter name
#' @noRd
standardize_param_name <- function(param_name, con = NULL) {
    if (is.null(param_name)) {
        stop("param_name must be provided and non-null")
    }
    if (!is.character(param_name) || length(param_name) != 1) {
        stop("param_name must be a single character string")
    }

    PARAM_NAME_CHOICES <- c(
        "snow water equivalent",
        "precipitation, total",
        "temperature, air",
        "water level",
        "water flow",
        "fdd",
        "snow depth"
    )

    # check that param_name is in available choices
    if (!(param_name %in% PARAM_NAME_CHOICES)) {
        stop(
            paste0(
                "Invalid param_name specified; must be one of ",
                paste(PARAM_NAME_CHOICES, collapse = ", ")
            )
        )
    }

    # except parameters that are not in AquaCache public.parameters table
    param_not_in_ac <- (param_name %in% c("fdd"))

    # if con is provided, validate con and query param_name existence
    if (!is.null(con) && !param_not_in_ac) {
        if (!DBI::dbIsValid(con)) {
            stop("Database connection is not valid.")
        }

        param_exists <- DBI::dbGetQuery(
            con,
            "SELECT COUNT(*) as count FROM public.parameters WHERE param_name = $1",
            params = list(param_name)
        )$count >
            0

        # Verify parameter exists in public.parameters table
        if (!param_exists) {
            warning(sprintf(
                "Parameter '%s' not found in public.parameters table",
                param_name
            ))
            stop("Please provide a valid parameter name.")
        }
    }

    return(param_name)
}

# #' Calculate historical norms for stations
# #'
# #' @param temperature_data data.frame with 'datetime' column and station columns
# #' @param percent_missing_threshold Numeric (0-1) minimum data completeness in cumulative FDD calculation
# #' @param water_year_start_month Integer month when water year starts (default October = 10)
# #' @return data.frame with cumulative FDD for each station
# #'
# calculate_fdd <- function(
#     temperature,
#     percent_missing_threshold = 0.8,
#     water_year_start_month = 10
# ) {
#     if ('datetime' %notin% colnames(temperature)) {
#         stop("Input temperature data must have a 'datetime' column")
#     }

#     # Get list of stations as all columns except 'datetime'
#     station_list <- setdiff(colnames(temperature), "datetime")

#     # Ensure datetime is POSIXct
#     temperature$datetime <- as.POSIXct(temperature$datetime)

#     # Calculate Freezing Degree Days (FDD) for each year and station, resetting at the start of each water year (Oct 1)
#     temperature$date_year <- lubridate::year(temperature$datetime)
#     temperature$date_month <- lubridate::month(temperature$datetime)
#     temperature$date_day <- lubridate::day(temperature$datetime)
#     temperature <- temperature[order(temperature$datetime), ]

#     # FDD: sum of degrees below 0°C per water year (Oct 1 - Jun 30)
#     temperature$date_water_year <- ifelse(
#         lubridate::month(temperature$datetime) >= water_year_start_month,
#         temperature$date_year + 1,
#         temperature$date_year
#     )

#     # Only keep data from October to June (exclude July and August)
#     temperature <- temperature[
#         lubridate::month(temperature$datetime) %in%
#             c(water_year_start_month:12, 1:6),
#     ]

#     # Initialize FDD and cumulative_nans data frames
#     fdd <- temperature
#     fdd[station_list] <- NA
#     cumulative_nans <- temperature
#     cumulative_nans[station_list] <- NA

#     below_zero <- temperature
#     # Set all values above 0 to 0 for each station
#     below_zero[station_list] <- lapply(below_zero[station_list], function(x) {
#         ifelse(x > 0, 0, x)
#     })

#     # Calculate cumulative FDD within each water_year
#     for (station_id in station_list) {
#         fdd[station_id] <- ave(
#             abs(below_zero[[station_id]]),
#             fdd$date_water_year,
#             FUN = function(x) cumsum(ifelse(is.na(x), 0, x))
#         )

#         cumulative_nans[station_id] <- ave(
#             is.na(temperature[[station_id]]),
#             temperature$date_water_year,
#             FUN = cumsum
#         )
#     }

#     # Add a column for number of days since start of water year as integer
#     temperature$days_from_start <- as.integer(
#         ave(
#             as.numeric(temperature$datetime),
#             temperature$date_water_year,
#             FUN = function(x) as.numeric(difftime(x, min(x), units = "days"))
#         )
#     ) +
#         1

#     # Normalize cumulative_nans by days_from_start to get proportion of missing data [0-1]
#     cumulative_nans$days_from_start <- temperature$days_from_start
#     for (station_id in station_list) {
#         cumulative_nans[[station_id]] <- ifelse(
#             cumulative_nans$days_from_start > 0,
#             cumulative_nans[[station_id]] / cumulative_nans$days_from_start,
#             NA
#         )
#     }

#     # Set FDD to NA where more than 80% of data is missing
#     for (station_id in station_list) {
#         nan_vector <- cumulative_nans[[station_id]]
#         fdd[[station_id]][nan_vector > percent_missing_threshold] <- NA
#     }

#     return(fdd)
# }

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
#' @description
#' Aggregates timeseries data to a specified temporal frequency using various
#' statistical functions. Handles missing data appropriately.
#'
#' @param ts_data data.frame with datetime column and station value columns
#' @param frequency Character string: "daily", "monthly", or "yearly"
#' @param func Character string: aggregation function ("sum", "mean", "max", "min")
#'
#' @return data.frame with resampled timeseries data with:
#' \describe{
#'   \item{datetime}{POSIXct timestamps for the aggregation periods}
#'   \item{...}{Aggregated values for each station column}
#' }
#'
#' @details
#' The function groups data by the specified frequency and applies the aggregation
#' function to each station column. For temporal aggregation, the datetime is
#' shifted to represent the end of the aggregation period (e.g., end of month
#' for monthly aggregation). Missing data (NA/NaN) is handled by returning NA
#' if all values in a period are missing, otherwise applying the function with
#' na.rm = TRUE.
#'
#' @examples
#' \dontrun{
#' # Convert daily to monthly means
#' monthly_data <- resample_timeseries(daily_ts, "monthly", "mean")
#'
#' # Convert hourly to daily sums
#' daily_sums <- resample_timeseries(hourly_ts, "daily", "sum")
#' }
#'
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
#' @keywords internal
download_spatial_layer <- function(
    con,
    layer_name,
    additional_query = NULL,
    epsg = 4326
) {
    query <- sprintf(
        "SELECT *, ST_AsText(ST_Transform(geom, %d)) as geom_wkt
         FROM spatial.vectors
         WHERE layer_name = %s",
        epsg,
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

    # Remove any existing geom column to avoid duplicate
    if ("geom" %in% names(data)) {
        data$geom <- NULL
    }

    geom <- sf::st_as_sfc(data$geom_wkt, crs = epsg)
    data$geom_wkt <- NULL

    sf::st_sf(data, geometry = geom, crs = epsg)
}

#' Download continuous timeseries station locations
#'
#' @description
#' Retrieves metadata for continuous monitoring stations from the database
#' and converts to spatial format.
#'
#' @param con DBI database connection object
#' @param param_name Character string of the parameter name to check
#' @param epsg Integer EPSG code for coordinate transformation (default 4326)
#'
#' @return sf object with station locations and metadata including:
#' \describe{
#'   \item{timeseries_id}{Unique identifier for the timeseries}
#'   \item{location_id}{Station location identifier}
#'   \item{location}{Station code}
#'   \item{name}{Station name}
#'   \item{latitude}{Station latitude}
#'   \item{longitude}{Station longitude}
#'   \item{conversion_m}{Datum conversion factor if available}
#' }
#'
#' @export
#'
#' @details
#' Queries the continuous.timeseries and public.locations tables to get station
#' metadata for the specified parameter. Returns NULL if no stations are found.
#'
download_continuous_ts_locations <- function(
    con,
    param_name,
    epsg = 4326
) {
    # mapping aggregation descriptions to parameter names
    aggregation_descriptions <- list(
        `temperature, air` = "(min+max)/2",
        `precipitation, total` = "sum",
        `snow water equivalent` = "instantaneous",
        `water level` = "instantaneous",
        `water flow` = "instantaneous"
    )

    # mapping record rates to parameter names
    record_rates <- list(
        `temperature, air` = "'1 day'",
        `precipitation, total` = "'1 day'",
        `snow water equivalent` = "'01:00:00'",
        # allow both 5 and 15 min
        `water level` = paste(
            "'00:05:00'",
            "'00:15:00'",
            "'01:00:00'",
            sep = ","
        ),
        `water flow` = paste(
            "'00:05:00'",
            "'00:15:00'",
            "'01:00:00'",
            sep = ","
        )
    )
    # Build metadata query for continuous SWE timeseries
    md_query <- sprintf(
        "SELECT
            t.timeseries_id,
            t.location_id,
            l.location_code,
            l.name,
            l.latitude,
            l.longitude,
            t.record_rate,
            t.start_datetime,
            p.param_name,
            p.parameter_id,
            t.aggregation_type_id
        FROM continuous.timeseries t
        JOIN public.locations l ON t.location_id = l.location_id
        JOIN public.parameters p ON t.parameter_id = p.parameter_id
        WHERE t.parameter_id = (SELECT parameter_id FROM public.parameters
            WHERE param_name = %s)
            AND t.aggregation_type_id = (
                SELECT aggregation_type_id FROM continuous.aggregation_types
                WHERE aggregation_type = %s LIMIT 1
            )
            AND t.record_rate IN (%s)
        ORDER BY t.location_id, t.start_datetime ASC",
        DBI::dbQuoteString(con, param_name),
        DBI::dbQuoteString(con, aggregation_descriptions[[param_name]]),
        record_rates[[param_name]]
    )

    md_continuous_df <- DBI::dbGetQuery(con, md_query)

    # Keep only the first (best) record_rate per unique location_id
    md_continuous_df <- md_continuous_df[
        !duplicated(md_continuous_df$location_id),
    ]

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

    md_continuous <- sf::st_transform(md_continuous, crs = epsg)

    return(md_continuous)
}

#' Download discrete timeseries station locations
#'
#' @description
#' Retrieves metadata for discrete monitoring stations (snow surveys) from the database
#' and converts to spatial format.
#'
#' @param con DBI database connection object
#' @param param_name Character string of the parameter name to check
#' @param epsg Integer EPSG code for coordinate transformation (default 4326)
#'
#' @return sf object with station locations and metadata including:
#' \describe{
#'   \item{location_id}{Station location identifier}
#'   \item{latitude}{Station latitude}
#'   \item{longitude}{Station longitude}
#'   \item{location}{Station code}
#'   \item{name}{Station name}
#'   \item{conversion_m}{Datum conversion factor if available}
#' }
#'
#' @export
#'
#' @details
#' Queries the discrete.samples, discrete.results, and public.locations tables
#' to get station metadata for the specified parameter. Returns NULL if no
#' stations are found.
#'
download_discrete_ts_locations <- function(con, param_name, epsg = 4326) {
    # Build metadata query for discrete SWE timeseries
    md_discrete_df <- DBI::dbGetQuery(
        con,
        "SELECT DISTINCT
        l.location_id,
        l.latitude,
        l.longitude,
        l.location_code,
        l.name,
        dc.conversion_m
        FROM discrete.samples s
        JOIN discrete.results r ON s.sample_id = r.sample_id
        JOIN public.locations l ON s.location_id = l.location_id
        LEFT JOIN datum_conversions dc ON l.location_id = dc.location_id
        WHERE r.parameter_id = 
            (SELECT parameter_id FROM public.parameters
             WHERE param_name = $1)",
        params = list(param_name)
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

    md_discrete <- sf::st_transform(md_discrete, crs = epsg)
    return(md_discrete)
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
#' @param param_name Character string of the parameter name to retrieve
#' @param epsg Integer EPSG code for coordinate transformation (default 4326)
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
    param_name,
    start_date = sprintf("%d-01-01", 1950),
    end_date = sprintf("%d-01-01", 2100),
    resolution = "daily",
    epsg = 4326
) {
    param_name <- standardize_param_name(
        con = con,
        param_name = param_name
    )

    md_continuous <- download_continuous_ts_locations(
        con = con,
        param_name = param_name,
        epsg = epsg
    )

    ts_ids <- unique(md_continuous$timeseries_id)

    # Get corresponding location_ids for each timeseries_id
    loc_ids <- md_continuous$location_id[match(
        ts_ids,
        md_continuous$timeseries_id
    )]

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

        #TODO: fix aggreation for other parameters

        aggr_fun <- get_aggr_fun(param_name = param_name)
        # Resample to daily or monthly as requested
        if (resolution == "daily") {
            ts_data$day <- as.Date(ts_data$datetime)
            ts_daily <- stats::aggregate(
                value ~ day,
                data = ts_data,
                FUN = aggr_fun,
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
        station_col <- as.character(loc_ids[i])
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
    metadata_sf$key <- metadata_sf$location_id

    # Get coordinates for stations
    surveys_coords <- sf::st_coordinates(metadata_sf)
    metadata_sf$x <- surveys_coords[, 1]
    metadata_sf$y <- surveys_coords[, 2]

    return(list(
        timeseries = list(data = master_df),
        metadata = metadata_sf,
        geom = "point",
        continuity = "continuous",
        param_name = param_name
    ))
}


#' Retrieve discrete SWE timeseries and station metadata
#'
#' @param con DBI database connection object
#' @param start_date Character date string (YYYY-MM-DD) for filtering data
#' @param end_date Character date string (YYYY-MM-DD) for filtering data
#' @param param_name Character string of the parameter name to retrieve
#' @param epsg Integer EPSG code for coordinate transformation (default 4326)
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
    param_name = NULL,
    epsg = 4326
) {
    param_name <- standardize_param_name(
        con = con,
        param_name = param_name
    )

    md_discrete <- download_discrete_ts_locations(
        con = con,
        param_name = param_name,
        epsg = epsg
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
                                     WHERE param_name = %s)
               AND r.result IS NOT NULL",
            DBI::dbQuoteLiteral(con, loc_id),
            DBI::dbQuoteString(con, param_name)
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
        param_name = param_name
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

# Helper: get aggregation function by parameter
get_aggr_fun <- function(param_name) {
    switch(
        param_name,
        'precipitation, total' = sum,
        'snow water equivalent' = mean,
        'temperature, air' = mean,
        'fdd' = mean,
        mean
    )
}

# Helper: get station names from ts
get_station_names <- function(ts) {
    setdiff(colnames(ts), "datetime")
}

#'  Helper: get start/end dates for a given parameter and year/month
#'
#' @param month bulletin month
#' @param year bulletin year
#' @param october_start Norm period starts in October of previous year regardless of bulletin month
#' @return start date and end date for norm calculation period
#' @noRd
get_period_dates <- function(year, month, october_start = FALSE) {
    # for the feb or mar bulletin, start from oct previous year
    # otherwise start from previous month
    if ((month == 2) || (month == 3) || october_start) {
        start_month <- 10
    } else {
        start_month <- month - 1
    }

    if (start_month == 10) {
        start_year <- year - 1
    } else {
        start_year <- year
    }

    start_date <- as.Date(sprintf("%d-%02d-01", start_year, start_month))
    end_date <- as.Date(sprintf("%d-%02d-01", year, month))

    list(start_date = start_date, end_date = end_date)
}

# Helper: get indices for a given parameter and period
# e.g., for SWE or FDD, just grab the SWE measurement on the period end_date
# for precipitation/temperature, grab all dates in the range
# precip gets summed over the period, temp gets averaged
get_indices <- function(param_name, ts, start_date, end_date) {
    switch(
        param_name,
        `precipitation, total` = which(
            ts$datetime >= start_date & ts$datetime < end_date
        ),
        `snow water equivalent` = which(ts$datetime == end_date),
        `temperature, air` = which(
            ts$datetime >= start_date & ts$datetime < end_date
        ),
        `fdd` = which(ts$datetime == end_date),
        `water level` = which(ts$datetime == end_date),
        `water flow` = which(ts$datetime == end_date),
        which(ts$datetime >= start_date & ts$datetime < end_date)
    )
}


#' Calculate historical norms for stations
#'
#' @param start_year_historical Integer start year for historical period
#' @param end_year_historical Integer end year for historical period
#' @param ts Wide-format data.frame with 'datetime' column and station columns
#' @param param_name Character string specifying the parameter name.
#' @param end_months_historical Integer vector of months to calculate norms for
#' @param completeness_per_aggr_period Numeric (0-1) minimum data completeness
#'   required for aggregation period (e.g., oct-feb for march bulletin)
#' @param completeness_per_norm_period Numeric (0-1) minimum data completeness
#'   required for norm period (e.g., 1991-2020)
#' @return A list with data.frames (station_norms, historical_distr, etc.)
#' @noRd
get_norms <- function(
    ts,
    param_name,
    start_year_historical = 1991,
    end_year_historical = 2020,
    end_months_historical = c(2, 3, 4, 5),
    october_start = FALSE,
    completeness_per_aggr_period = 0.5,
    completeness_per_norm_period = 0.2
) {
    aggr_fun <- get_aggr_fun(param_name)
    station_names <- get_station_names(ts)

    # Create a 3D array: years x months x stations
    historical_distr <- array(
        NA,
        dim = c(
            end_year_historical - start_year_historical + 1,
            length(end_months_historical),
            length(station_names)
        ),
        dimnames = list(
            year = as.character(start_year_historical:end_year_historical),
            month = as.character(end_months_historical),
            station = station_names
        )
    )

    # for each year (e.g., 1991-2020) and for each each month (e.g., 2,3,4,5)
    # get the 'norm' period, aggregate value
    # output is table [year x month x station]
    for (yr in start_year_historical:end_year_historical) {
        for (m in end_months_historical) {
            period <- get_period_dates(yr, m, october_start = october_start)
            idx <- get_indices(
                param_name,
                ts,
                period$start_date,
                period$end_date
            )
            for (station in station_names) {
                # check data completeness (for an individual year; eg Oct-Feb for March bulletin, or March for April bulletin)
                vals <- ts[idx, station]
                if (
                    sum(!is.na(vals)) >=
                        completeness_per_aggr_period * length(vals)
                ) {
                    aggr_value <- aggr_fun(vals, na.rm = TRUE)
                } else {
                    aggr_value <- NA
                }

                historical_distr[
                    as.character(yr),
                    as.character(m),
                    station
                ] <- aggr_value
            }
        }
    }

    # Check completeness across the historical period for each month/station combination
    # e.g., for a norm period from 1991-2020, need at least 24 non-NA values to meet 80% completeness
    for (m_idx in seq_along(end_months_historical)) {
        for (s_idx in seq_along(station_names)) {
            # Get all values for this month/station across all years
            values_for_month_station <- historical_distr[, m_idx, s_idx]

            # Calculate completeness (proportion of non-NA values)
            completeness <- sum(!is.na(values_for_month_station)) /
                length(values_for_month_station)

            # If completeness is below threshold, set entire dimension to NA
            if (completeness < completeness_per_norm_period) {
                historical_distr[, m_idx, s_idx] <- NA
            }
        }
    }

    # Calculate median (norm) for each station and month
    station_norms <- apply(
        historical_distr,
        c(2, 3),
        stats::median,
        na.rm = TRUE
    )
    # station_norms: months x stations

    list(
        station_norms = station_norms,
        historical_distr = historical_distr
    )
}


#' Get bulletin value for each station
#' @param bulletin_month Integer bulletin month
#' @param bulletin_year Integer bulletin year
#' @param ts Wide-format data.frame with 'datetime' column and station columns
#' @param param_name Character string specifying the parameter name.
#' @return Named numeric vector of bulletin values for each station
#' @noRd
get_bulletin_value <- function(
    bulletin_month,
    bulletin_year,
    ts,
    param_name
) {
    aggr_fun <- get_aggr_fun(param_name)
    station_names <- get_station_names(ts)

    if (param_name %in% c("fdd")) {
        october_start <- TRUE
    } else {
        october_start <- FALSE
    }

    period <- get_period_dates(
        bulletin_year,
        bulletin_month,
        october_start = october_start
    )
    idx <- get_indices(param_name, ts, period$start_date, period$end_date)

    # Calculate aggregated value for each station over the period
    station_current <- stats::setNames(
        sapply(station_names, function(station) {
            aggr_fun(ts[idx, station], na.rm = TRUE)
        }),
        station_names
    )

    station_current <- as.numeric(station_current)
    station_current[is.nan(station_current)] <- NA
    names(station_current) <- station_names

    return(station_current)
}


get_normalized_bulletin_values <- function(
    bulletin_month,
    bulletin_year,
    ts,
    norms,
    param_name,
    as_table = FALSE
) {
    bulletin_values <- get_bulletin_value(
        bulletin_month,
        bulletin_year,
        ts,
        param_name
    )

    last_year_values <- get_bulletin_value(
        bulletin_month,
        bulletin_year - 1,
        ts,
        param_name
    )

    station_names <- get_station_names(ts)

    # Extract norms for the bulletin month
    norms_for_month <- norms$station_norms[
        rownames(norms$station_norms) == as.character(bulletin_month),
    ]
    # Calculate the ratio of current_aggr to station_norms for each station
    relative_to_norm <- 100 *
        (bulletin_values /
            norms_for_month)

    # Handle special cases for SWE
    if (param_name == "snow water equivalent") {
        snow_present <- !is.na(bulletin_values) & bulletin_values > 0
        snow_not_present <- !is.na(bulletin_values) & bulletin_values == 0

        median_is_zero <- !is.na(norms_for_month) & norms_for_month == 0
        # median_is_nonzero <- !is.na(norms_for_month) & norms_for_month > 0

        relative_to_norm[median_is_zero & snow_not_present] <- -2
        relative_to_norm[median_is_zero & snow_present] <- -1
    }

    # Calculate anomalies (current - norm)
    anomalies <- bulletin_values - norms_for_month

    # Calculate the percentile of bulletin_values within historical values for each station
    station_percentiles <- sapply(station_names, function(station) {
        hist_values <- norms$historical_distr[,
            rownames(norms$station_norms) == bulletin_month,
            station
        ]
        # Remove NA values
        hist_values <- hist_values[!is.na(hist_values)]

        # Percentile: proportion of historical values less than or equal to current value
        mean(hist_values <= bulletin_values[station]) * 100
    })

    list(
        current = bulletin_values,
        relative_to_norm = relative_to_norm,
        norm = norms_for_month,
        percentiles = station_percentiles,
        anomalies = anomalies,
        last_year = last_year_values
    )
}


# #' Calculate historic daily median and relative change for timeseries
# #'
# #' @param ts Wide-format data.frame with 'datetime' column and station columns
# #' @param lookback_length Integer number of years to look back from each measurement (optional)
# #' @param lookback_start Integer year to start lookback period from (e.g., 1980) (optional)
# #' @param lookback_end Integer year to end lookback period (optional, defaults to current year - 1)
# #' @return A list with two elements:
# #' \describe{
# #'   \item{historic_median}{data.frame with historic median values for each date/station}
# #'   \item{relative_to_med}{data.frame with current values as percentage of historic median}
# #' }
# #'
# #' @param parameter Character string specifying the parameter name.
# #'
# #' @details
# #' The function handles special cases for relative SWE calculation:
# #' \itemize{
# #'   \item Standard percentage when historic median > 0
# #'   \item Value of -2 when both current and historic are zero
# #'   \item Value of -1 when current > 0 but historic is zero
# #' }
# #'
# #' Data is filtered to February-May and snapped to the 1st of each month for
# #' consistency with snow bulletin reporting periods.
# #'
# #' @examples
# #' \dontrun{
# #' # Calculate using fixed lookback year
# #' result <- calculate_historic_daily_median(ts_data, lookback_start = 1980)
# #'
# #' # Calculate using rolling 30-year window
# #' result <- calculate_historic_daily_median(ts_data, lookback_length = 30)
# #' }

# calculate_historic_daily_median <- function(
#     ts,
#     lookback_length = NULL,
#     lookback_start = NULL,
#     lookback_end = NULL
# ) {
#     # Default behaviour if neither provided
#     if (is.null(lookback_start) && is.null(lookback_length)) {
#         lookback_start <- 1980
#     } else if (!is.null(lookback_start) && !is.null(lookback_length)) {
#         stop("Specify either lookback_start or lookback_length, not both.")
#     }

#     # Warning if lookback_end is not provided when using lookback_start
#     if (!is.null(lookback_start) && is.null(lookback_end)) {
#         warning(
#             "lookback_end not provided when using lookback_start. Defaulting to most recent historical value (current year - 1)."
#         )
#         lookback_end <- as.integer(format(Sys.Date(), "%Y")) - 1
#     }

#     if (!"datetime" %in% names(ts)) {
#         stop(sprintf(
#             "Input timeseries must contain 'datetime' column. Found columns: %s",
#             paste(names(ts), collapse = ", ")
#         ))
#     }

#     if (lookback_start >= lookback_end && !is.null(lookback_end)) {
#         stop("lookback_start must be less than lookback_end.")
#     }

#     if (!is.null(lookback_start) && !is.null(lookback_end)) {
#         if (
#             lookback_start < 1800 ||
#                 lookback_end < 1800 ||
#                 lookback_start > as.integer(format(Sys.Date(), "%Y")) ||
#                 lookback_end > as.integer(format(Sys.Date(), "%Y"))
#         ) {
#             stop(
#                 "lookback_start and lookback_end must be valid years (>= 1800 and <= current year)."
#             )
#         }
#     }

#     # For each unique year/month, if no value exists exactly on the 1st, grab the nearest value within 5 days
#     if ("datetime" %in% names(ts)) {
#         ts$year <- as.integer(format(ts$datetime, "%Y"))
#         ts$month <- as.integer(format(ts$datetime, "%m"))
#         ts$day <- as.integer(format(ts$datetime, "%d"))

#         # Only keep Feb-May
#         ts <- ts[ts$month %in% 2:5, , drop = FALSE]

#         # For each year/month, ensure a value exists for the 1st (or nearest within 5 days)
#         keep_rows <- logical(nrow(ts))
#         # Track which rows are snapped (not on the 1st)
#         snapped_rows <- logical(nrow(ts))
#         unique_ym <- unique(ts[, c("year", "month")])
#         for (i in seq_len(nrow(unique_ym))) {
#             y <- unique_ym$year[i]
#             m <- unique_ym$month[i]
#             idx <- which(ts$year == y & ts$month == m)
#             # Prefer day==1
#             idx1 <- idx[ts$day[idx] == 1]
#             if (length(idx1) > 0) {
#                 keep_rows[idx1] <- TRUE
#             } else {
#                 # Find nearest to 1st within 5 days
#                 days_from_first <- abs(ts$day[idx] - 1)
#                 min_diff <- min(days_from_first)
#                 if (min_diff <= 5) {
#                     nearest_idx <- idx[which.min(days_from_first)]
#                     keep_rows[nearest_idx] <- TRUE
#                     snapped_rows[nearest_idx] <- TRUE
#                 }
#             }
#         }
#         # Snap datetime to 1st of month for snapped rows
#         if (any(snapped_rows)) {
#             ts$datetime[snapped_rows] <- as.POSIXct(
#                 sprintf(
#                     "%d-%02d-01",
#                     ts$year[snapped_rows],
#                     ts$month[snapped_rows]
#                 ),
#                 tz = "UTC"
#             )
#             ts$day[snapped_rows] <- 1
#         }
#         ts <- ts[keep_rows, , drop = FALSE]
#         # Remove extra columns
#         ts$year <- NULL
#         ts$month <- NULL
#         ts$day <- NULL
#         ts$date_first_of_month <- NULL
#         ts$date_diff_days <- NULL
#     }

#     # Wide timeseries: datetime + one column per station (stations as columns)
#     if ("datetime" %in% names(ts)) {
#         ts$datetime <- as.POSIXct(ts$datetime, tz = "UTC")
#         n <- nrow(ts)
#         day <- as.integer(format(ts$datetime, "%d"))
#         month <- as.integer(format(ts$datetime, "%m"))
#         year <- as.integer(format(ts$datetime, "%Y"))

#         #doy <- snap_doy(doy)

#         # station columns are everything except datetime (and any preexisting doy/year)
#         station_cols <- setdiff(names(ts), c("datetime", "doy", "year"))
#         # prepare output data.frames
#         hist_df <- data.frame(datetime = ts$datetime, stringsAsFactors = FALSE)
#         rel_df <- data.frame(datetime = ts$datetime, stringsAsFactors = FALSE)
#         perc_df <- data.frame(datetime = ts$datetime, stringsAsFactors = FALSE)

#         # Vectorized and grouped computation for speed
#         p <- length(station_cols)
#         if (p > 0) {
#             vals_mat <- as.matrix(ts[, station_cols, drop = FALSE]) # n x p
#             hist_mat <- matrix(NA_real_, nrow = n, ncol = p)
#             rel_mat <- matrix(NA_real_, nrow = n, ncol = p)
#             perc_mat <- matrix(NA_real_, nrow = n, ncol = p)

#             # Group by month-day (usually day==1 after snapping), compute per group
#             grp <- paste(month, day, sep = "-")
#             ug <- unique(grp)
#             for (group_month_day in ug) {
#                 group_indices <- which(grp == group_month_day)
#                 # Ensure chronological order within group
#                 group_indices <- group_indices[order(year[group_indices])]

#                 # Precompute years vector for the group
#                 group_years <- year[group_indices]

#                 for (current_idx in seq_along(group_indices)) {
#                     current_row <- group_indices[current_idx]

#                     # if lookback_start is provided, use that as fixed start year
#                     # default to the current year - 1 as lookback_end
#                     if (!is.null(lookback_start)) {
#                         historical_indices <- group_indices[which(
#                             group_years < group_years[current_idx] &
#                                 group_years >= lookback_start &
#                                 group_years <= lookback_end
#                         )]

#                         # # if lookback_end is provided, apply that
#                         # if (!is.null(lookback_end)) {
#                         #     historical_indices <- historical_indices[which(
#                         #         group_years[historical_indices] <= lookback_end
#                         #     )]
#                         # }

#                         # if lookback_length is provided, apply that. Note, this is secondary to lookback_start
#                     } else {
#                         historical_indices <- group_indices[which(
#                             group_years < group_years[current_idx] &
#                                 group_years >=
#                                     (group_years[current_idx] - lookback_length)
#                         )]
#                     }

#                     if (length(historical_indices) > 0) {
#                         historical_values <- vals_mat[
#                             historical_indices,
#                             ,
#                             drop = FALSE
#                         ]
#                         hist_mat[current_row, ] <- apply(
#                             historical_values,
#                             2,
#                             stats::median,
#                             na.rm = TRUE
#                         )

#                         # Calculate percentile: what % of historical values are <= current value
#                         if (length(historical_indices) >= 4) {
#                             current_values <- vals_mat[current_row, ]
#                             if (all(is.na(current_values))) {
#                                 perc_mat[current_row, ] <- NA_real_
#                             } else {
#                                 perc_mat[current_row, ] <- (colSums(
#                                     historical_values <=
#                                         matrix(
#                                             current_values,
#                                             nrow = nrow(historical_values),
#                                             ncol = ncol(historical_values),
#                                             byrow = TRUE
#                                         ),
#                                     na.rm = TRUE
#                                 ) /
#                                     colSums(
#                                         !is.na(historical_values),
#                                         na.rm = TRUE
#                                     )) *
#                                     100
#                             }
#                         }
#                     }
#                 }
#             }

#             # Compute relative SWE in a fully vectorized way
#             cur <- vals_mat
#             hist <- hist_mat

#             # Case 1: standard percentage where historic median != 0
#             mask1 <- !is.na(hist) & !is.na(cur) & (hist != 0)
#             rel_mat[mask1] <- 100 * cur[mask1] / hist[mask1]

#             # Case 2: both zero => -2
#             mask2 <- !is.na(cur) & (cur == 0) & !is.na(hist) & (hist == 0)
#             rel_mat[mask2] <- -2

#             # Case 3: current > 0, historic == 0 => -1
#             mask3 <- !is.na(cur) & (cur > 0) & !is.na(hist) & (hist == 0)
#             rel_mat[mask3] <- -1

#             colnames(hist_mat) <- station_cols
#             colnames(rel_mat) <- station_cols

#             hist_df[station_cols] <- as.data.frame(
#                 hist_mat,
#                 stringsAsFactors = FALSE
#             )
#             rel_df[station_cols] <- as.data.frame(
#                 rel_mat,
#                 stringsAsFactors = FALSE
#             )

#             colnames(perc_mat) <- station_cols
#             perc_df[station_cols] <- as.data.frame(
#                 perc_mat,
#                 stringsAsFactors = FALSE
#             )
#         }

#         # ensure ordering by datetime
#         hist_df <- hist_df[order(hist_df$datetime), , drop = FALSE]
#         rel_df <- rel_df[order(rel_df$datetime), , drop = FALSE]
#         perc_df <- perc_df[order(perc_df$datetime), , drop = FALSE]

#         return(list(
#             historic_median = hist_df,
#             relative_to_med = rel_df,
#             percentile = perc_df
#         ))
#     }

#     stop(
#         "Input timeseries must contain a 'datetime' column (and either 'value' for single-station or station columns for wide format)."
#     )
# }

#' Split communities by type for different display scales
#'
#' @description
#' Separates communities into large and small categories for differential
#' display on maps based on community type.
#'
#' @param communities sf object containing community data with a 'description' column
#'
#' @return A list with two elements:
#' \describe{
#'   \item{large}{sf object with cities and towns}
#'   \item{small}{sf object with settlements and villages}
#' }
#'
#' @details
#' Large communities (cities and towns) are displayed at all zoom levels,
#' while small communities (settlements and villages) are only shown at
#' higher zoom levels to reduce map clutter.
#'
#' @noRd

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

    data_stats <- get_normalized_bulletin_values(
        bulletin_month = month,
        bulletin_year = year,
        ts = data$timeseries$data,
        norms = data$norms,
        param_name = data$param_name
    )

    shp <- data$metadata

    # Join data_stats entries to shp based on shp$key
    # Assume data_stats entries are named by key (station/basin name/id)
    # and shp$key matches those names
    # We'll add columns to shp: swe, relative_to_med, historic_median, percentile

    key <- as.character(shp$key)

    # Ensure data_stats vectors are named and match keys
    stats_df <- data.frame(
        key = key,
        value = as.numeric(data_stats$current[key]),
        relative_to_med = as.numeric(data_stats$relative_to_norm[key]),
        historic_median = as.numeric(data_stats$norm[key]),
        percentile = as.numeric(data_stats$percentiles[key]),
        anomalies = as.numeric(data_stats$anomalies[key])
    )

    shp <- merge(
        shp,
        stats_df,
        by.x = "key",
        by.y = "key",
        all.x = TRUE,
        sort = FALSE
    )

    return(shp)
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


#' Get distance correction factor for coordinate system conversion
#'
#' @description
#' Converts distances from kilometers to the units of the specified coordinate
#' reference system (CRS). Used for label positioning adjustments.
#'
#' @param epsg Numeric EPSG code for the coordinate reference system
#'
#' @return Numeric distance correction factor for converting km to CRS units:
#' \describe{
#'   \item{4326 (WGS84)}{111.32 km per degree}
#'   \item{3857, 3577, 3579 (Projected)}{0.0014 km per meter (with empirical adjustment)}
#' }
#'
#' @details
#' Label position adjustments were originally calibrated in EPSG:4326 (degrees).
#' When using projected coordinate systems, an empirical correction factor (1.4x)
#' is applied to maintain proper label positioning without recalculating all
#' adjustment values.
#'
#' This is a pragmatic solution to handle coordinate system differences while
#' preserving the carefully tuned label positions for optimal map readability.
#'
#' @examples
#' \dontrun{
#' # Get correction for WGS84
#' wgs84_factor <- get_km_to_crs_correction(4326)  # Returns 111.32
#'
#' # Convert 50km adjustment to degrees
#' offset_degrees <- 50 / wgs84_factor
#'
#' # Get correction for projected system
#' proj_factor <- get_km_to_crs_correction(3579)  # Returns 0.0014
#' offset_meters <- 50 / proj_factor
#' }
#'
#' @noRd
get_km_to_crs_correction <- function(epsg) {
    distance_correction <- switch(
        as.character(epsg),
        "4326" = 111.32, # degrees to km
        "3857" = 1.4 * 1 / 1000, # meters to km
        "3577" = 1.4 * 1 / 1000, # meters to km
        "3579" = 1.4 * 1 / 1000, # meters to km
        111.32 # default to degrees to km
    )
    return(distance_correction)
}


#### --------------- D: Functions to create CDDF plots -------------------- ####
# Function for calculating CDDF
getCDDF <- function(temps, year) {
    # Function for calculating cddf of dataframe (with all dates of interest)
    calcCDDF <- function(temps) {
        cddf <- 0
        temps$cddf <- NA
        temps$value[is.na(temps$value)] <- 0
        temps$cddf <- Reduce(
            function(prev, temp) pmax(0, prev - temp),
            temps$value,
            init = 0,
            accumulate = TRUE
        )[-1]
        return(temps[, c("datetime", "cddf")])
    }

    # # Keep only sept-june data
    # temps <- temps[
    #     format(temps$datetime, "%m") %in%
    #         c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07"),
    # ]

    # Find first and last year
    first_yr <- format(min(temps$datetime), "%Y")
    last_yr <- format(max(temps$datetime), "%Y")

    # if (last_yr <= year) {
    #   last_yr <- year
    # }

    stations <- setdiff(names(temps), "datetime")
    cddf_station <- list()

    cddf_timeseries <- data.frame(datetime = temps$datetime)
    # Run over every year
    for (station in stations) {
        cddf <- data.frame()

        for (y in first_yr:last_yr) {
            # Subset data
            tab <- temps[
                temps$datetime >= paste0(y, "-09-01") &
                    temps$datetime < paste0(y + 1, "-08-31"),
                c("datetime", station)
            ]
            names(tab) <- c("datetime", "value")

            # Only calculate if missing less than 10 days, but only for years that are not in the 'years' list
            if (length(tab$datetime) != 0) {
                if (
                    sum(!is.na(tab$value)) >= 276 |
                        format(min(tab$datetime), "%Y") %in% c(year - 1)
                ) {
                    cddf_y <- calcCDDF(tab)
                    names(cddf_y) <- c("datetime", station)
                    cddf <- rbind(cddf, cddf_y)
                }
            }
        }
        cddf_timeseries <- merge(
            cddf_timeseries,
            cddf,
            by = "datetime",
            all.x = TRUE,
            sort = FALSE
        )
    }

    return(cddf_timeseries)
}


#' Load all base data for the SWE mapping application
#'
#' @param con DBI database connection object
#' @param load_swe Logical indicating whether to load SWE data (default TRUE)
#' @param load_precip Logical indicating whether to load precipitation data (default FALSE)
#' @param load_temp Logical indicating whether to load temperature data (default FALSE)
#' @param load_streamflow Logical indicating whether to load streamflow data (default FALSE)
#' @param start_year_historical Integer start year for historical norms (default 1991)
#' @param end_year_historical Integer end year for historical norms (default 2020)
#' @param october_start Logical indicating if water year starts in October (default FALSE)
#' @param epsg Numeric EPSG code for coordinate reference system (default 4326)
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
    load_temp = FALSE,
    load_streamflow = FALSE,
    start_year_historical = 1991,
    end_year_historical = 2020,
    october_start = FALSE,
    epsg = 4326
) {
    # Initialize output structure
    # SWE consists of pillows (continuous), surveys (discrete), and basins (weighted average)
    # Precipitation and temperature are only continuous for now
    # Each data entry has timeseries and metadata
    # Each timeseries entry has data, historic_median, relative_to_med, percentile
    snowbull_timeseries <- list(
        swe = list(),
        sd = list(),
        precipitation = list(),
        temperature = list(),
        streamflow = list()
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

        # Ensure basin CRS is WGS84
        basins_shp <- sf::st_transform(basins_shp, epsg)

        # rename basin name column if needed
        if ("SWE_Basin" %in% names(basins_shp)) {
            names(basins_shp)[names(basins_shp) == "SWE_Basin"] <- "name"
        }

        # convert area to km2
        basins_shp$area_km2 <- sf::st_area(basins_shp) |> as.numeric() * 1e-6

        # load swe data from continuous source
        continuous_data <- download_continuous_ts(
            con,
            param_name = "snow water equivalent",
            epsg = epsg
        )

        # # process norms and relative values
        # ret <- calculate_historic_daily_median(
        #     continuous_data$timeseries$data,
        #     lookback_start = 1991,
        #     lookback_end = 2020
        # )
        # continuous_data$timeseries$historic_median <- ret$historic_median
        # continuous_data$timeseries$relative_to_med <- ret$relative_to_med
        # continuous_data$timeseries$percentile <- ret$percentile
        norms <- get_norms(
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical,
            october_start = october_start,
            ts = continuous_data$timeseries$data,
            param_name = "snow water equivalent"
        )
        # store continuous pillow data

        snowbull_timeseries$swe$pillows <- continuous_data
        snowbull_timeseries$swe$pillows$norms <- norms
        # load swe data from discrete source
        discrete_data <- download_discrete_ts(
            con = con,
            epsg = epsg,
            param_name = "snow water equivalent"
        )

        norms <- get_norms(
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical,
            ts = discrete_data$timeseries$data,
            param_name = "snow water equivalent"
        )

        # store discrete survey data
        snowbull_timeseries$swe$surveys <- discrete_data
        snowbull_timeseries$swe$surveys$norms <- norms

        discrete_data <- download_discrete_ts(
            con = con,
            epsg = epsg,
            param_name = "snow depth"
        )

        # we can leave param_name as swe here since they're calculated in the same way
        norms <- get_norms(
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical,
            ts = discrete_data$timeseries$data,
            param_name = "snow water equivalent"
        )

        # store discrete survey data
        snowbull_timeseries$sd$surveys <- discrete_data
        snowbull_timeseries$sd$surveys$norms <- norms

        # Spatial join: assign basin names to each survey station

        # Ensure both are in the same CRS
        surveys_sf <- snowbull_timeseries$swe$surveys$metadata

        # Perform spatial join: add basin name to each survey station
        # Ensure both objects have valid geometries and matching CRS
        if (sf::st_crs(surveys_sf) != sf::st_crs(basins_shp)) {
            surveys_sf <- sf::st_transform(
                surveys_sf,
                sf::st_crs(basins_shp)
            )
        }
        # Perform spatial join using st_within as a function
        basin_names <- sf::st_join(
            surveys_sf,
            basins_shp[, c("name")],
            left = TRUE,
            join = function(x, y) sf::st_within(x, y)
        )$name.y
        snowbull_timeseries$swe$surveys$metadata$basin <- basin_names

        # manually add the two alaska surveys - ideally an 'Alaska' polygon shoud be included in SWE_basins..
        snowbull_timeseries$swe$surveys$metadata[
            snowbull_timeseries$swe$surveys$metadata$location %in%
                c("08AK-SC01", "08AK-SC02"),
            "basin"
        ] <- "Alaska"

        # Load or infer weight matrix from snowcourse factors CSV using discrete metadata
        weights_df <- load_snowcourse_factors(
            metadata_discrete = discrete_data$metadata
        )

        # Add location_id column to weights_df based on metadata lookup

        weights_df <- merge(
            weights_df,
            discrete_data$metadata[, c("location", "location_id")],
            by = "location",
            all.x = TRUE,
            sort = FALSE
        )

        # Remove 'location' column and rename 'location_id' to 'location'
        weights_df$location <- NULL
        names(weights_df)[names(weights_df) == "location_id"] <- "location"

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
                c("location", basin_names),
                drop = FALSE
            ]

            swe_samples <- as.numeric(discrete_data$timeseries$data[
                i,
                as.character(weight_matrix$location),
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
        # ret <- calculate_historic_daily_median(
        #     basin_timeseries,
        #     lookback_start = 1980,
        #     lookback_end = 2020
        # )

        # store basin-averaged data
        # snowbull_timeseries$swe$basins$timeseries <- list(
        #     data = basin_timeseries,
        #     historic_median = ret$historic_median,
        #     relative_to_med = ret$relative_to_med,
        #     percentile = ret$percentile
        # )

        norms <- get_norms(
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical,
            ts = basin_timeseries,
            october_start = october_start,
            param_name = "snow water equivalent"
        )
        snowbull_timeseries$swe$basins$timeseries <- list(
            data = basin_timeseries
        )
        snowbull_timeseries$swe$basins$norms <- norms
        snowbull_timeseries$swe$basins$geom <- "poly"
        snowbull_timeseries$swe$basins$continuity <- "discrete"
        snowbull_timeseries$swe$basins$param_name <- "snow water equivalent"

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

        distance_correction <- get_km_to_crs_correction(epsg)

        snowbull_timeseries$swe$basins$metadata$x_adjusted <- snowbull_timeseries$swe$basins$metadata$x +
            vapply(
                snowbull_timeseries$swe$basins$metadata$name,
                function(n) {
                    if (n %in% names(basin_adjustments)) {
                        basin_adjustments[[n]]$x / distance_correction # convert km -> degrees (approx)
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
                        basin_adjustments[[n]]$y / distance_correction # convert km -> degrees (approx)
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
            param_name = "precipitation, total",
            start_date = "1980-01-01",
            epsg = epsg
        )

        norms <- get_norms(
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical,
            ts = precip_data$timeseries$data,
            october_start = october_start,
            param_name = "precipitation, total"
        )

        snowbull_timeseries$precipitation <- precip_data
        snowbull_timeseries$precipitation$norms <- norms
    } # end load_precip

    if (load_temp) {
        temp_data <- download_continuous_ts(
            con,
            param_name = "temperature, air",
            start_date = "1980-01-01",
            epsg = epsg
        )

        norms <- get_norms(
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical,
            ts = temp_data$timeseries$data,
            october_start = october_start,
            param_name = "temperature, air"
        )

        snowbull_timeseries$temperature <- temp_data
        snowbull_timeseries$temperature$norms <- norms

        # fdd <- calculate_fdd(
        #     temperature = temp_data$timeseries$data,
        #     percent_missing_threshold = 0.8,
        #     water_year_start_month = 10
        # )

        station_list <- names(temp_data$timeseries$data)[
            names(temp_data$timeseries$data) != "datetime"
        ]

        cddf <- getCDDF(
            temps = temp_data$timeseries$data,
            year = as.integer(format(Sys.Date(), "%Y"))
        )

        # FDD will always have october start - since 1-month FDD doesn't seem super useful
        norms <- get_norms(
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical,
            ts = cddf,
            october_start = TRUE,
            param_name = "fdd"
        )

        snowbull_timeseries$fdd <- list(
            timeseries = list(data = cddf),
            param_name = "fdd",
            continuity = "derived",
            geom = "point",
            norms = norms,
            metadata = temp_data$metadata
        )
    } # end load_temp

    if (load_streamflow) {
        water_level <- download_continuous_ts(
            con,
            param_name = "water level",
            start_date = "1990-01-01",
            epsg = epsg
        )

        paste(water_level$metadata$location_id)
        datums <- DBI::dbGetQuery(
            con,
            sprintf(
                "SELECT location_id, conversion_m FROM public.datum_conversions WHERE current = TRUE AND location_id IN (%s)",
                paste(water_level$metadata$location_id, collapse = ",")
            )
        )

        # Apply datum corrections to water_level$timeseries$data by matching columns to location_id
        if (!is.null(datums) && nrow(datums) > 0) {
            # datums: location_id, conversion_m
            ts_cols <- setdiff(names(water_level$timeseries$data), "datetime")
            for (col in ts_cols) {
                loc_id <- as.integer(col)
                datum_row <- datums[datums$location_id == loc_id, ]
                if (nrow(datum_row) == 1 && !is.na(datum_row$conversion_m)) {
                    water_level$timeseries$data[[
                        col
                    ]] <- water_level$timeseries$data[[col]] +
                        datum_row$conversion_m
                } else {
                    #if datum is not found, issue a warning and set to NA
                    warning(
                        sprintf(
                            "No datum conversion found for location_id %d. Skipping datum correction for this station.",
                            loc_id
                        )
                    )
                    water_level$timeseries$data[[col]] <- NA
                }
            }
        }

        threshold_levels <- data$flow_level_flood
        threshold_levels <- threshold_levels[
            !is.na(threshold_levels$Flood_level_asl),
        ]
        # Rename Flood_level_asl to threshold for consistency
        names(threshold_levels)[
            names(threshold_levels) == "Flood_level_asl"
        ] <- "threshold"

        water_level$metadata <-
            merge(
                water_level$metadata,
                threshold_levels[c("ID", "threshold")],
                by.x = "location",
                by.y = "ID",
                all.x = TRUE,
                sort = FALSE
            )

        norms <- get_norms(
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical,
            ts = water_level$timeseries$data,
            october_start = october_start,
            param_name = "water level"
        )

        snowbull_timeseries$water_level <- water_level
        snowbull_timeseries$water_level$norms <- norms

        water_flow <- download_continuous_ts(
            con,
            param_name = "water flow",
            start_date = "1990-01-01",
            epsg = epsg
        )

        threshold_flows <- data$flow_level_flood
        threshold_flows <- threshold_flows[
            !is.na(threshold_flows$Flood_flow),
        ]
        # Rename Flood_flow to threshold for consistency
        names(threshold_flows)[
            names(threshold_flows) == "Flood_flow"
        ] <- "threshold"

        water_flow$metadata <-
            merge(
                water_flow$metadata,
                threshold_flows[c("ID", "threshold")],
                by.x = "location",
                by.y = "ID",
                all.x = TRUE,
                sort = FALSE
            )

        norms <- get_norms(
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical,
            ts = water_flow$timeseries$data,
            october_start = october_start,
            param_name = "water flow"
        )

        snowbull_timeseries$water_flow <- water_flow
        snowbull_timeseries$water_flow$norms <- norms
    } # end load_streamflow

    return(snowbull_timeseries)
}

#' Load spatial shapefiles for snow bulletin mapping
#'
#' @description
#' Loads all required spatial data layers for snow bulletin map creation,
#' including basins, territorial boundaries, roads, and communities.
#'
#' @param con DBI database connection object
#' @param epsg Integer EPSG code for coordinate reference system (default 4326)
#'
#' @return A list containing spatial data layers:
#' \describe{
#'   \item{basins}{sf object with SWE basin polygons}
#'   \item{yukon}{sf object with Yukon territorial boundary}
#'   \item{inverted_yukon}{sf object for masking area outside Yukon}
#'   \item{roads}{sf object with primary and secondary highways}
#'   \item{communities}{sf object with community locations and adjusted label positions}
#' }
#'
#' @details
#' The function performs several spatial processing steps:
#' \enumerate{
#'   \item Loads SWE basin polygons from package data
#'   \item Downloads territorial boundaries from database
#'   \item Creates inverted boundary for masking
#'   \item Downloads and clips roads to basin area
#'   \item Downloads communities and calculates label positions
#'   \item Applies manual position adjustments for optimal display
#' }
#'
#' All spatial data is transformed to the specified CRS and label positions
#' are pre-calculated with manual adjustments to prevent overlap.
#'
#' @examples
#' \dontrun{
#' con <- AquaCache::AquaConnect(...)
#' shapefiles <- load_bulletin_shapefiles(con)
#'
#' # Check loaded data
#' print(names(shapefiles))
#' print(nrow(shapefiles$basins))  # Number of basins
#' }
#'
#' @noRd

load_bulletin_shapefiles <- function(con, epsg = 4326) {
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
        additional_query = "AND feature_name = 'Yukon'",
        epsg = epsg
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
        additional_query = "AND description IN ('Primary Highway', 'Secondary Highway')",
        epsg = epsg
    )

    # Clip roads to basin boundaries if both exist
    # Ensure both have the same CRS
    if (sf::st_crs(roads) != sf::st_crs(basins_shp)) {
        roads <- sf::st_transform(roads, sf::st_crs(basins_shp))
    }

    # Buffer basin boundaries by 10km and clip roads to buffered area
    basins_buffered <- sf::st_buffer(sf::st_union(basins_shp), dist = 100000) # 10km in meters
    roads <- sf::st_intersection(roads, basins_buffered)

    # Ensure 'roads' is an sf object (sometimes st_intersection drops class)
    if (!inherits(roads, "sf")) {
        roads <- sf::st_as_sf(roads)
    }

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
        layer_name = "Communities",
        epsg = epsg
    )
    # Ensure CRS is WGS84 for leaflet
    #if (!is.null(communities) && sf::st_crs(communities)$epsg != 4326) {
    # communities <- sf::st_transform(communities, 4326)
    #}

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

        # Customize specific communities (these correctons are in KM, and need to be converted to the correct crs)
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

        distance_correction <- get_km_to_crs_correction(epsg)

        communities$x_adjusted <- communities$x +
            vapply(
                communities$feature_name,
                function(n) {
                    if (n %in% names(community_adjustments)) {
                        community_adjustments[[n]]$x / distance_correction # convert km -> degrees (approx)
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
                        community_adjustments[[n]]$y / distance_correction # convert km -> degrees (approx)
                    } else {
                        0
                    }
                },
                numeric(1)
            )
    }

    snowbull_shapefiles$communities <- communities

    # Ensure all shapefiles are set to desired EPSG
    for (nm in names(snowbull_shapefiles)) {
        shp <- snowbull_shapefiles[[nm]]
        if (inherits(shp, "sf") && sf::st_crs(shp)$epsg != epsg) {
            snowbull_shapefiles[[nm]] <- sf::st_transform(shp, epsg)
        }
    }

    return(snowbull_shapefiles)
}

# ' Generate HTML content for SWE map popups
#' @param swe Numeric SWE value at the station
#' @param relative_to_med Numeric percent of median SWE value
#' @param historic_median Numeric historical median SWE value
#' @param name Character string for station or basin name
#' @param location Optional character string for station location description
#' @param id Optional character string for station ID (used for plot generation)
#' @param percentile Optional numeric percentile value for the station
#' @param language Character string for language ("English" or "French")
#' @param label Character string indicating data type label for translation
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
    label = ""
) {
    language <- lengthenLanguage(language)

    # match the datatype label to translation key
    label_tr_key <- switch(
        label,
        "swe_poly_discrete" = "snowbull_swe_basin",
        "swe_point_discrete" = "snowbull_snow_survey",
        "swe_point_continuous" = "snowbull_snow_pillow",
        "snowbull_snow_survey"
    )
    datatype_label <- paste0(
        "<b>Type:</b>",
        tr(label_tr_key, language),
        "<br>"
    )

    # Robust handling of location (avoid NULL/NA)
    location_str <- if (
        !is.null(location) && !is.na(location) && nzchar(location)
    ) {
        paste0(
            "<span style='font-size: 12px; color: #666;'>",
            location,
            "</span><br><br>"
        )
    } else {
        "<br>"
    }

    # Always return a character string
    paste0(
        "<div style='text-align: left; padding: 10px; width: 300px;'>",
        "<b style='font-size: 16px;'>",
        name,
        "</b><br>",
        location_str,
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
        "</div>"
    )
}


#' Filter stations by data currency
#'
#' @description
#' Removes stations that have no recent data within a specified time window.
#' Used to exclude stations with stale or discontinued measurements.
#'
#' @param df data.frame containing station metadata with a 'latest_date' column
#' @param input_date POSIXct reference date (typically the current bulletin date)
#' @param cutoff_days Integer number of days defining the acceptable data age
#'
#' @return data.frame with stations filtered to those with recent data
#'
#' @details
#' Stations are kept if:
#' \itemize{
#'   \item latest_date is NA (no data filtering applied)
#'   \item latest_date is within cutoff_days of input_date
#' }
#'
#' This prevents display of stations that appear to have current data but
#' actually have measurements from previous seasons.
#'
#' @examples
#' \dontrun{
#' # Keep only stations with data from the last year
#' current_stations <- filter_stations_by_latest_date(
#'   station_metadata,
#'   as.POSIXct("2025-03-01"),
#'   365
#' )
#' }
#'
#' @noRd

# Filter out stations with no recent data (more than 1 year old)
filter_stations_by_latest_date <- function(df, input_date, cutoff_days) {
    ok <- df[
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

    # create a label with format parameter_geom_continuity (e.g., swe_point_continuous)
    # these labels are matched to the translation dict
    label <- paste(
        dataset$param_name,
        dataset$geom,
        dataset$continuity,
        sep = "_"
    )

    # generate popup content for each station/basin
    dataset_state$popup_content <- rep(NA_character_, nrow(dataset_state))

    # Ensure all required columns exist and are character/numeric as needed
    # Use as.character/as.numeric to avoid factor/list issues
    dataset_state$popup_content <- vapply(
        seq_len(nrow(dataset_state)),
        function(i) {
            generate_popup_content(
                swe = as.numeric(dataset_state$value[i]),
                relative_to_med = as.numeric(dataset_state$relative_to_med[i]),
                historic_median = as.numeric(dataset_state$historic_median[i]),
                name = as.character(dataset_state$name[i]),
                location = if ("location" %in% names(dataset_state)) {
                    as.character(dataset_state$location[i])
                } else {
                    ""
                },
                percentile = as.numeric(dataset_state$percentile[i]),
                language = lang,
                label = label
            )
        },
        character(1)
    )

    # if point-source data, remove stations with no recent data
    if (dataset$geom == "point") {
        dataset_state <- filter_stations_by_latest_date(
            dataset_state,
            input_date,
            50
        )
    }

    dataset_state$display_value <- dataset_state[[statistic]]

    # Ensure all swe_at_* columns are numeric for each data frame
    to_numeric_cols <- function(df, cols) {
        for (col in cols) {
            if (col %in% names(df)) {
                df[[col]] <- round(as.numeric(df[[col]]), 0)
            }
        }
        df
    }

    numeric_cols <- c(
        "value",
        "relative_to_med",
        "historic_median",
        "percentile",
        "anomaly"
    )
    dataset_state <- to_numeric_cols(dataset_state, numeric_cols)

    # update the annotations to display the value for the selected type
    dataset_state$annotation <- paste0(
        dataset_state$annotation,
        "<br>(",
        round(dataset_state[[statistic]], 0),
        "%)"
    )

    # Add a 'snowbull_rmd_' tag to preposition column based on perc_hist_med
    dataset_state$preposition <- character(nrow(dataset_state))
    for (i in seq_len(nrow(dataset_state))) {
        val <- dataset_state$relative_to_med[i]
        if (is.na(val)) {
            dataset_state$preposition[i] <- "rmd_no_data"
        } else if (val < 66) {
            dataset_state$preposition[i] <- "rmd_well_below"
        } else if (val >= 66 && val < 90) {
            dataset_state$preposition[i] <- "rmd_below"
        } else if (val >= 90 && val < 98) {
            dataset_state$preposition[i] <- "rmd_close"
        } else if (val >= 98 && val < 103) {
            dataset_state$preposition[i] <- "rmd_no_preposition"
        } else if (val >= 103 && val < 110) {
            dataset_state$preposition[i] <- "rmd_close"
        } else if (val >= 110 && val < 135) {
            dataset_state$preposition[i] <- "rmd_above"
        } else if (val >= 135) {
            dataset_state$preposition[i] <- "rmd_well_above"
        } else {
            dataset_state$preposition[i] <- "rmd_no_data"
        }
    }

    # Define the snowbull translation function
    trb <- function(key) {
        tr(
            key,
            lang = language,
            translations = data$snowbull_translations
        )
    }

    # Translate the tags to the appropriate language
    dataset_state$preposition <- vapply(
        dataset_state$preposition,
        function(tag) trb(tag),
        character(1)
    )

    aggr_tag <- if (dataset$param_name %in% c("precipitation, total")) {
        "rmd_normal"
    } else {
        "rmd_average"
    }

    # get the 'relative_to_med' text description by merging the preposition, aggr_tag, and text_colour
    dataset_state$description <- vapply(
        seq_len(nrow(dataset_state)),
        function(i) {
            paste(
                dataset_state$preposition[i],
                trb(aggr_tag),
                sep = " "
            )
        },
        FUN.VALUE = character(1)
    )

    return(dataset_state)
}


#' Create a Leaflet map visualizing SWE data
#'
#' @description
#' Creates an interactive Leaflet map visualizing SWE data at basins, discrete survey stations,
#' and continuous pillow stations. The map includes styled polygons and markers,
#' labels, popups, and a dynamic legend based on the selected value type and date.
#'
#' @param point_data sf object containing point-based station data (surveys)
#' @param poly_data sf object containing polygon-based basin data
#' @param point_data_secondary sf object containing secondary point data (pillows)
#' @param snowbull_shapefiles List containing all loaded shapefiles from load_bulletin_shapefiles()
#' @param statistic Character string indicating which SWE value to visualize
#'   (e.g., "data", "relative_to_med", "percentile")
#' @param language Character string indicating the language for labels and legends (default "English")
#' @param month Integer month for map title
#' @param year Integer year for map title
#' @param filename Optional character string for HTML output file path
#'
#' @return A Leaflet map object with interactive features:
#' \describe{
#'   \item{Base layers}{Satellite imagery background}
#'   \item{Data layers}{Colored basins, survey points, pillow stations}
#'   \item{Reference layers}{Territorial boundary, roads, communities}
#'   \item{Interactive elements}{Popups with station details and plots}
#'   \item{Legend}{Dynamic legend based on selected statistic}
#'   \item{Controls}{Layer toggles and zoom controls}
#' }
#'
#' @details
#' The map includes several interactive features:
#' \itemize{
#'   \item Hover labels showing station names and locations
#'   \item Click popups with detailed information and timeseries plots
#'   \item Layer controls for toggling different data types
#'   \item Zoom-dependent visibility for communities
#'   \item Color-coded styling based on SWE statistics
#' }
#'
#' Colors and symbols are dynamically generated based on the selected statistic
#' and language settings. The map extent is automatically set to cover the
#' Yukon Territory with appropriate zoom levels.
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Create interactive map for March 2025
#' leaflet_map <- make_leaflet_map(
#'   point_data = survey_data,
#'   poly_data = basin_data,
#'   snowbull_shapefiles = shapefiles,
#'   statistic = "relative_to_med",
#'   month = 3,
#'   year = 2025
#' )
#'
#' # Display the map
#' leaflet_map
#'
#' # Save to file
#' make_leaflet_map(
#'   ...,
#'   filename = "march_2025_swe.html"
#' )
#' }

make_leaflet_map <- function(
    point_data = NULL,
    poly_data = NULL,
    point_data_secondary = NULL,
    snowbull_shapefiles = NULL,
    param_name,
    statistic = "relative_to_med",
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
        param_name = param_name,
        language = language
    )

    # legend created dynamically based on inputs
    legend_title <- paste0(
        "<b>",
        tr("snowbull_symbols", language),
        "</b><br>",
        switch(
            statistic,
            "relative_to_med" = tr("snowbull_relative_median", language),
            "data" = tr("snowbull_swe", language),
            "percentile" = tr("snowbull_percentile", language),
            "anomalies" = tr("snowbull_anomalies", language),
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
                    display_value,
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
                data = poly_data,
                lng = poly_data$x_adjusted,
                lat = poly_data$y_adjusted,
                label = lapply(poly_data$annotation, htmltools::HTML),
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

    m <- m %>%
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
                    display_value,
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

    if (!is.null(snowbull_shapefiles$roads)) {
        m <- m %>%
            leaflet::addPolylines(
                data = snowbull_shapefiles$roads,
                color = static_style_elements$roads$color,
                weight = static_style_elements$roads$weight,
                opacity = static_style_elements$roads$opacity,
                group = "Roads"
            )
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
                    display_value,
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

    if (!is.null(snowbull_shapefiles$yukon)) {
        {
            m <- m %>%
                leaflet::addPolygons(
                    .,
                    data = snowbull_shapefiles$yukon,
                    color = static_style_elements$boundary$color,
                    weight = static_style_elements$boundary$weight,
                    fill = static_style_elements$boundary$fill,
                    group = "Boundary"
                )
        }

        communities_split <- split_communities(
            snowbull_shapefiles$communities
        )

        m <- m %>%
            leaflet::addMarkers(
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
        m <- m %>%
            leaflet::addLabelOnlyMarkers(
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
        m <- m %>%
            leaflet::addMarkers(
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
        m <- m %>%
            leaflet::addLabelOnlyMarkers(
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

        m <- m %>%
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
    }

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

#' Create a static ggplot2 map for SWE data
#'
#' @description
#' Creates a high-quality static map using ggplot2 for publication or printing.
#' Optimized for professional presentation with proper typography and layout.
#'
#' @param point_data sf object containing point-based station data (surveys)
#' @param poly_data sf object containing polygon-based basin data
#' @param point_data_secondary sf object containing secondary point data (pillows)
#' @param statistic Character string indicating which SWE value to visualize
#' @param snowbull_shapefiles List containing all loaded shapefiles
#' @param language Character string for language (default "English")
#' @param month Integer month for map title
#' @param year Integer year for map title
#' @param filename Optional character string for PNG output file path
#' @param dpi Numeric resolution in dots per inch (default 300)
#' @param height Numeric height in inches (default 14)
#' @param width Numeric width in inches (default 8)
#'
#' @return A ggplot2 object with publication-ready styling:
#' \describe{
#'   \item{Background}{Terrain-colored territorial boundary}
#'   \item{Basins}{Color-coded polygons with optimal labels}
#'   \item{Stations}{Styled point markers for surveys and pillows}
#'   \item{Infrastructure}{Roads and community markers}
#'   \item{Labels}{Shadow text with anti-collision positioning}
#'   \item{Legend}{Color legend matching the selected statistic}
#'   \item{Typography}{Professional fonts and sizing}
#' }
#'
#' @details
#' The static map is optimized for:
#' \itemize{
#'   \item High-resolution printing and publication
#'   \item Professional typography with shadow text
#'   \item Optimal label positioning to prevent overlap
#'   \item Consistent symbology across different statistics
#'   \item Proper coordinate system handling
#'   \item Scalable vector elements
#' }
#'
#' The map uses a projected coordinate system (typically NAD83 Yukon) for
#' accurate distance measurements and optimal display of the territory.
#'
#' @examples
#' \dontrun{
#' # Create publication-ready map
#' static_map <- make_ggplot_map(
#'   point_data = survey_data,
#'   poly_data = basin_data,
#'   snowbull_shapefiles = shapefiles,
#'   statistic = "relative_to_med",
#'   month = 3,
#'   year = 2025,
#'   width = 12,
#'   height = 8,
#'   dpi = 300
#' )
#'
#' # Display the plot
#' print(static_map)
#'
#' # Save high-resolution PNG
#' make_ggplot_map(
#'   ...,
#'   filename = "march_2025_swe.png",
#'   dpi = 600
#' )
#' }
#'
#' @keywords internal
#' @noRd

make_ggplot_map <- function(
    point_data = NULL,
    poly_data = NULL,
    point_data_secondary = NULL,
    statistic = NULL,
    snowbull_shapefiles,
    param_name = NULL,
    units = "",
    language = "English",
    month = NULL,
    year = NULL,
    filename = NULL,
    dpi = 300,
    height = 14,
    width = 8,
    start_year_historical = NULL,
    end_year_historical = NULL
) {
    requireNamespace("ggplot2")
    requireNamespace("shadowtext")

    # processed data is values for one point in time, based on bulletin_data
    static_style_elements <- get_static_style_elements()

    # load style elements based on value_type
    # language used for legend text
    dynamic_style_elements <- get_dynamic_style_elements(
        statistic = statistic,
        param_name = param_name,
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
                alpha = 1
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
            size = static_style_elements$basins$weight * 0.6,
            fill = NA
        )
    p <- p +
        ggplot2::geom_sf(
            data = snowbull_shapefiles$basins,
            color = "black",
            size = static_style_elements$basins$weight * 0.25,
            fill = NA
        )

    # Add Yukon boundary background (underneath everything except basins)
    if (!is.null(snowbull_shapefiles$yukon)) {
        p <- p +
            ggplot2::geom_sf(
                data = snowbull_shapefiles$yukon,
                fill = NA, # No fill for background
                color = "black",
                size = 1.5,
                alpha = 0.5 # 50% transparent background
            )
    }

    if (!is.null(point_data)) {
        # point_data <<- point_data
        # point_data <- point_data[!is.na(point_data$value), ]

        point_data <- point_data[
            order(point_data$value, decreasing = FALSE, na.last = FALSE),
        ]
        p <- p +
            ggplot2::geom_point(
                data = point_data,
                ggplot2::aes(
                    x = .data$x,
                    y = .data$y
                ),
                fill = point_data$fill_colour,
                color = static_style_elements$surveys$color,
                size = static_style_elements$surveys$radius / 1.5,
                shape = 21,
                stroke = static_style_elements$surveys$weight * 0.5
            )
    }

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
                size = 3,
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
    switch(
        param_name,
        "snow water equivalent" = "mm",
        "precipitation, total" = "mm",
        "temperature, air" = "\u00B0C",
    )

    # if start_year_historical and end_year_historical are not null, add period of record to subtitle
    if (!is.null(start_year_historical) && !is.null(end_year_historical)) {
        period_of_record <- paste0(
            "\n",
            tr("snowbull_normal", language),
            " (",
            start_year_historical,
            "-",
            end_year_historical,
            ")"
        )
    } else {
        period_of_record <- ""
    }

    # make the title based on parameter, statistic, and date
    subtitle <- paste0(
        switch(
            param_name,
            "snow water equivalent" = tr("snowbull_swe", language),
            "precipitation, total" = tr("snowbull_precipitation", language),
            "temperature, air" = tr("snowbull_temperature", language),
            ""
        ),
        "\n",
        switch(
            statistic,
            "relative_to_med" = tr("snowbull_relative_median", language),
            "value" = sprintf(tr("snowbull_values", language), units),
            "percentile" = tr("snowbull_percentile", language),
            "anomalies" = sprintf(tr("snowbull_anomalies", language), units),
            ""
        ),
        "\n",
        tr(month_name_short, language),
        " 1, ",
        year,
        period_of_record
    )

    # Add a dummy legend showing fill colour bins
    legend_df <- data.frame(
        color = dynamic_style_elements$colors,
        label = dynamic_style_elements$labels,
        stringsAsFactors = FALSE
    )
    # Reverse the order of legend labels and colors
    legend_df$label <- factor(
        legend_df$label,
        levels = rev(dynamic_style_elements$labels)
    )
    legend_df$color <- rev(dynamic_style_elements$colors)

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
            legend.position = c(0.7, 0.8), # move legend to left and up
            legend.justification = c("left", "top")
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

    basin_bbox <- sf::st_bbox(snowbull_shapefiles$basins)
    yukon_bbox <- sf::st_bbox(snowbull_shapefiles$yukon)

    # Combine both bounding boxes and add a buffer (in CRS units)
    buffer_amount <- 25000 # 50 km buffer; adjust as needed for your CRS (meters for EPSG:3579)
    plot_bbox <- c(
        xmin = min(basin_bbox["xmin"], yukon_bbox["xmin"]) - buffer_amount,
        xmax = max(basin_bbox["xmax"], yukon_bbox["xmax"]) + buffer_amount,
        ymin = min(basin_bbox["ymin"], yukon_bbox["ymin"]) - buffer_amount,
        ymax = max(basin_bbox["ymax"], yukon_bbox["ymax"]) + buffer_amount
    )

    p <- p +
        ggplot2::coord_sf(
            xlim = plot_bbox[c("xmin", "xmax")],
            ylim = plot_bbox[c("ymin", "ymax")],
            expand = FALSE
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
#' @param snowbull_shapefiles shapefiles, loaded from load_bulletin_shapefiles()
#' @param snowbull_timeseries timeseries data, loaded from load_bulletin_timeseries()
#' @param width Numeric width of the plot in inches (default: 12)
#' @param height Numeric height of the plot in inches (default: 8)
#' @param filename Optional character string for PNG output file path
#' @param dpi Numeric resolution in dots per inch (default: 300)
#' @param param_name Character, parameter to plot (default: "swe")
#' @param statistic Character, "absolute", "relative", or "percentile" (default: "relative")
#' @param language Character string indicating the language for labels and legends. Default is "English".
#' @param con Optional database connection, if not provided a default connection will be used
#' @param format Character string indicating the output format: "ggplot", "leaflet", or "shiny",
#' @param start_year_historical Integer, start year for historical norms (default: 1991)
#' @param end_year_historical Integer, end year for historical norms (default: 2020)
#'
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
    param_name = NULL,
    statistic = "relative_to_med",
    start_year_historical = 1991,
    end_year_historical = 2020,
    language = "English",
    con = NULL,
    format = "ggplot"
) {
    # Load required packages
    requireNamespace("sf")
    requireNamespace("stats")

    # param_name <- standardize_param_name(param_name)

    STATISTICS <- c("data", "relative_to_med", "percentile", "anomalies")
    statistic <- match.arg(
        statistic,
        choices = STATISTICS
    )

    FORMATS <- c("ggplot", "leaflet", "shiny")
    format <- match.arg(
        format,
        choices = FORMATS
    )

    # Set file extension based on format if filename is provided
    if (!is.null(filename)) {
        ext <- tools::file_ext(filename)
        if (format == "ggplot" && tolower(ext) != "png") {
            stop("For ggplot format, filename must have .png extension")
        }
        if (format %in% c("leaflet", "shiny") && tolower(ext) != "html") {
            stop(
                "For leaflet or shiny format, filename must have .html extension"
            )
        }
    }

    # infer epsg code based on format
    epsg <- switch(
        format,
        "ggplot" = 3579, # NAD83 / Yukon (ft)
        "leaflet" = 4326, # WGS84
        "shiny" = 4326 # WGS84
    )

    dynamic_style_elements <- get_dynamic_style_elements(
        statistic = statistic,
        param_name = param_name,
        language = "English"
    )

    # static_style_elements <- get_static_style_elements()

    if (is.null(con)) {
        con <- AquaConnect(silent = TRUE)
        on.exit(DBI::dbDisconnect(con))
    }

    # Here, we load the continuous timeseries data from the database
    # it will load all params passed
    # norms are calculated using load_bulletin_timeseries
    # it needs norm period specifications, but doesn't need 'bulletin year' information yet
    if (is.null(snowbull_timeseries)) {
        snowbull_timeseries <- load_bulletin_timeseries(
            con,
            load_swe = param_name == "snow water equivalent",
            load_precip = param_name == "precipitation, total",
            load_temp = param_name == "temperature, air",
            epsg = epsg,
            start_year_historical = start_year_historical,
            end_year_historical = end_year_historical
        )
    }

    # Load snowbull_data if not provided
    if (is.null(snowbull_shapefiles)) {
        snowbull_shapefiles <- load_bulletin_shapefiles(
            con,
            epsg = epsg
        )
    }
    # Map snow bulletin data to generic map input arguments (point_data, poly_data, etc.)
    # This step organizes the loaded timeseries data into a consistent structure for mapping.
    timeseries_data <- switch(
        param_name,
        "snow water equivalent" = list(
            poly_data = snowbull_timeseries$swe$basins,
            point_data = snowbull_timeseries$swe$surveys,
            point_data_secondary = snowbull_timeseries$swe$pillows
        ),
        "precipitation, total" = list(
            poly_data = NULL,
            point_data = snowbull_timeseries$precipitation,
            point_data_secondary = NULL
        ),
        "temperature, air" = list(
            poly_data = NULL,
            point_data = snowbull_timeseries$temperature,
            point_data_secondary = NULL
        ),
        stop("Unsupported param_name: ", param_name)
    )

    # ugly units hardcoding
    units <- switch(
        param_name,
        "snow water equivalent" = "mm",
        "precipitation, total" = "mm",
        "temperature, air" = "\u00B0C",
        "fdd" = "\u00B0C days",

        ""
    )

    # Prepare map_data structure to hold processed data for each layer type
    map_data <- list(
        poly_data = NULL,
        point_data = NULL,
        point_data_secondary = NULL
    )

    # For each data type, extract display-ready data for the specified date/statistic
    for (data_type in names(map_data)) {
        dataset <- timeseries_data[[data_type]]
        if (!is.null(dataset)) {
            map_data[[data_type]] <- get_display_data(
                dataset = dataset,
                year = year,
                month = month,
                statistic = statistic,
                language = "English"
            )
            map_data[[data_type]]$fill_colour <- get_state_style_elements(
                map_data[[data_type]]$display_value,
                style_elements = dynamic_style_elements
            )
        }
    }

    # Build bulletin_data for downstream use (e.g., export, reporting)
    bulletin_data <- list(
        month = month,
        year = year,
        param_name = param_name,
        statistic = statistic,
        start_year_historical = start_year_historical,
        end_year_historical = end_year_historical
    )

    # Helper to clean up the dataframes before returning them for use in the bulletin markup
    remove_mapping_fields <- function(df) {
        remove_fields <- c(
            "x_adjusted",
            "y_adjusted",
            "annotation",
            "popup_content"
        )
        if (!is.null(df) && is.data.frame(df)) {
            df <- df[, !names(df) %in% c(remove_fields), drop = FALSE]
        }
        return(df)
    }

    # Attach processed map data to bulletin_data by parameter type
    if (param_name == "snow water equivalent") {
        bulletin_data$swe_basins <- remove_mapping_fields(map_data$poly_data)
        bulletin_data$swe_surveys <- remove_mapping_fields(map_data$point_data)
        bulletin_data$swe_pillows <- remove_mapping_fields(
            map_data$point_data_secondary
        )
    } else if (param_name == "precipitation, total") {
        bulletin_data$precip_stations <- remove_mapping_fields(
            map_data$point_data
        )
    } else if (param_name == "temperature, air") {
        bulletin_data$temp_stations <- remove_mapping_fields(
            map_data$point_data
        )
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
            map <- make_ggplot_map(
                point_data = map_data$point_data,
                poly_data = map_data$poly_data,
                point_data_secondary = map_data$point_data_secondary,
                snowbull_shapefiles = snowbull_shapefiles,
                language = language,
                statistic = statistic,
                month = month,
                year = year,
                param_name = param_name,
                filename = filename,
                height = height,
                width = width,
                dpi = dpi,
                start_year_historical = start_year_historical,
                end_year_historical = end_year_historical,
                units = units
            )

            return(list(
                map = map,
                data = bulletin_data
            ))
        },
        stop("Unknown format: ", format)
    )
}
