basinsUIMod <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      
    )
  )
}

basinsServerMod <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements in the server code
    
    
  })
}


