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

#' Replace infinite values with NA
#'
#' Utility function to replace `Inf` and `-Inf` values with `NA`.
#'
#' @param x Numeric vector.
#' @return Numeric vector with infinite values converted to `NA`.
#' @keywords internal
inf_to_na <- function(x) {
  x[!is.finite(x)] <- NA
  x
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
  days <- floor(x / 24)
  remaining_hours <- x %% 24
  minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
  seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
  paste0("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S")
}
