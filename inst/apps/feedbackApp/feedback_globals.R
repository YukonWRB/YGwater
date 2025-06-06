feedback_globals <- function(db_path) {
  library(shiny)
  library(RSQLite)
  config <<- list(db_path = db_path)
}
