#' The YGwater app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  
  shinyjs::useShinyjs()
  
  # Load specific modules based on input$navbar ################################
  # Store information to pass between modules
  # moduleOutputs <- reactiveValues()
  
  EQWin <- AccessConnect("//carver/infosys/EQWin/WR/DB/Water Resources.mdb", silent = TRUE)
  AquaCache <- AquaConnect(silent = TRUE)
  print("Connected to EQWin")
  print("Connected to AquaCache")
  
  session$onSessionEnded( function() {
    DBI::dbDisconnect(EQWin)
    DBI::dbDisconnect(AquaCache)
    print("Disconnected from EQWin after session end")
    print("Disconnected from AquaCache after session end")
  })
  session$onUnhandledError(function() {
    DBI::dbDisconnect(EQWin)
    DBI::dbDisconnect(AquaCache)
    print("Disconnected from EQWin after unhandled error")
    print("Disconnected from AquaCache after unhandled error")
  })
  
  observeEvent(input$navbar, {
    if (input$navbar == "checks") {
      checks("checks", EQWin)
    }
    if (input$navbar == "plot") {
      plot("plot", EQWin, AquaCache)
    }
    if (input$navbar == "map") {
      map("map", EQWin, AquaCache)
    }
    if (input$navbar == "FOD") {
      FOD("FOD")
    }
  })
}
