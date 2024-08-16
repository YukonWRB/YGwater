# UI definition for EQchecks application.

# EQchecks consists of a simple sidebar and main panel layout.

# The sidebar contains menus and buttons to allow the user to select a date range in which to check samples, optional menus allowing them to limit checks to location(s) or even specific samples based on datetime. A 'Run checks' button triggers the checks, the result of which will show up in the main panel
# Optionally, we can add a collapsible set of buttons to enable/disable specific checks.

# The main panel contains a data.table with a row for each sample which failed at least one check, with metadata for location, datetime, failed checks (as string). The user can then click on individual rows which brings up a modal (pop-up) with basic medatata at the top and a table with details of the checks (failed and not) below. There is also a button to export the check result below the table.

#' The EQchecks User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/top-bar.css"), # Top bar size, position, etc
      tags$link(rel = "stylesheet", type = "text/css", href = "css/fonts.css"), # Fonts
    ),
    fluidRow(class = "top-bar-container",
             column(3,
                    div(class = "logo-container",
                        htmltools::img(src = "imgs/Yukon_logo.png", .noWS = "outside", alt = "YG logo")),
                    class = "left-aligned-logo"),
             column(9,
                    div(class = "aurora-container",
                        htmltools::img(src = "imgs/YG_Aurora_resized_flipped.png", .noWS = "outside", alt = "Aurora")),
                    class = "right-aligned-aurora")
    ),
    navbarPage("",
               id = "navbar",
               theme = shinythemes::shinytheme("flatly"), # Optional theme
               windowTitle = "EQWin Checks and Visualize App",
               collapsible = TRUE,
               fluid = TRUE,
               lang = "en",
               tabPanel(title = "Checks", value = "checks",
                        checksUI("checks")),
               tabPanel(title = "Visualize", value = "viz", 
                        vizUI("viz"))
               # Other panels could be developed
    ) # End navbarPage
  ) # End fluidPage
}
