## Ask for something and save the result
#' @noRd
ask <- function(...) {
  choices <- c("Yes", "No")
  cli::cat_bullet(..., bullet = "arrow", col = "darkgreen")
  utils::menu(choices) == which(choices == "Yes")
}

#' Round to a specified accuracy
#'
#' @param x Numeric vector to round.
#' @param accuracy Number to round to.
#' @param f Rounding function such as floor, ceiling, or round.
#' @noRd
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' Replace infinite and NaN values with NA
#'
#' Utility function to replace `Inf`, `-Inf`, and `NaN` values with `NA`.
#'
#' @param x Numeric vector, data.frame, or data.table. If data.frame or data.table, will only work on 'numeric' class columns.
#' @return Numeric vector with infinite values converted to `NA`.
#' @export
inf_to_na <- function(x) {
  # data.table
  if (data.table::is.data.table(x)) {
    num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
    if (length(num_cols)) {
      x[,
        (num_cols) := lapply(.SD, function(col) {
          col[!is.finite(col)] <- NA
          col
        }),
        .SDcols = num_cols
      ]
    }
    return(x)
  }

  # data.frame / tibble
  if (is.data.frame(x)) {
    # TRUE for tibbles or data.frames (and data.tables, but these are dealt with differently)
    numeric_cols <- sapply(x, is.numeric)
    x[numeric_cols] <- lapply(x[numeric_cols], function(col) {
      col[!is.finite(col)] <- NA
      col
    })
    return(x)
  }

  # vector
  if (is.numeric(x)) {
    x[!is.finite(x)] <- NA
    return(x)
  }

  # If x is not numeric, return it unchanged
  warning("Input is not numeric. Returning unchanged.")
  return(x)
}

#' Convert hours to ISO 8601 duration format
#'
#' Converts a numeric value representing hours into an ISO 8601 duration string.
#'
#' @param x Numeric value representing hours.
#' @return A string in ISO 8601 duration format (e.g., "P1DT2H30M0S").
#' @noRd
#' @examples
#' iso_period(26.5)  # Returns "P1DT2H30M0S"

iso_period <- function(x) {
  if (length(x) == 0) {
    return(character())
  }

  total_seconds <- round(x * 3600)
  days <- total_seconds %/% (24 * 3600)
  remainder <- total_seconds %% (24 * 3600)
  hours <- remainder %/% 3600
  remainder <- remainder %% 3600
  minutes <- remainder %/% 60
  seconds <- remainder %% 60

  result <- sprintf("P%dDT%dH%dM%dS", days, hours, minutes, seconds)
  invalid <- is.na(total_seconds) | !is.finite(total_seconds)
  result[invalid] <- NA_character_
  result
}


# Internal helper functions for default file paths

#' @noRd
eqwin_db_path <- function() {
  "//carver/infosys/EQWin/WaterResources.mdb"
}

#' @noRd
yown_master_path <- function() {
  "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/1_YOWN_MASTER_TABLE/YOWN_MASTER.xlsx"
}

#' @noRd
yown_tracking_path <- function() {
  "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/3_OTHER/YOWN_Logger_Tracking.xlsx"
}

#' @noRd
yown_dropbox_path <- function() {
  "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_LOGGER_FILE_DROPBOX"
}

#' @noRd
yown_active_path <- function() {
  "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS"
}

#' @noRd
yown_wdc_path <- function() {
  "\\\\envgeoserver\\share\\WaterResources\\Groundwater\\YOWN_DATA\\"
}
