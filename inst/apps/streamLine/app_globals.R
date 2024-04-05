library(shiny)
library(shinyjs)
        
source(system.file("apps/streamLine/modules/mapView.R", package = "YGwater"))
source(system.file("apps/streamLine/modules/downloadData.R", package = "YGwater"))

translations <- data.table::fread(system.file("apps/streamLine/translations.csv", package = "YGwater"))
