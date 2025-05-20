# UI and server code for admin/data entry module

adminUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    
  )
}

admin <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Use to create UI elements within the server function
    
  }) # End of moduleServer
}
