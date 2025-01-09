basinsUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      
    )
  )
}

basinsServer <- function(id, AquaCache) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements in the server code
    
    
  })
}


