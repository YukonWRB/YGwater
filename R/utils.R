## Ask for something and save the result
#' @noRd
ask <- function(...) {
  choices <- c("Yes", "No")
  cli::cat_bullet(..., bullet = "arrow", col = "darkgreen")
  utils::menu(choices) == which(choices == "Yes")
}
