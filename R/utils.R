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
