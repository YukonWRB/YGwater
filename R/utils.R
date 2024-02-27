## Ask for something and save the result
#' @noRd
ask <- function(...) {
  choices <- c("Yes", "No")
  cat(crayon::green(paste0(...,"\n", collapse = "")))
  utils::menu(choices) == which(choices == "Yes")
}
