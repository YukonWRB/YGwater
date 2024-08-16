#' The EQchecks server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  
  # Load specific modules based on input$navbar ################################
  # Store information to pass between modules
  # moduleOutputs <- reactiveValues()
  
  con <- AccessConnect("//carver/infosys/EQWin/WR/DB/Water Resources.mdb", silent = TRUE)
  print("Connected to EQWin")
  session$onSessionEnded( function() {
    DBI::dbDisconnect(con)
    print("Disconnected from EQWin")
  })
  session$onUnhandledError(function() {
    DBI::dbDisconnect(con)
    print("Disconnected from EQWin")
  })
  
  observeEvent(input$navbar, {
    if (input$navbar == "checks") {
      checks("checks", con)
    }
    if (input$navbar == "viz") {
      viz("viz", con)
    }
  })
}
