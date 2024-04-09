
mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        HTML("
      .leaflet-left .leaflet-control{
        visibility: hidden;
      }"
        ))
    ),
    tags$script(
      HTML(
        "$(function () { 
        $('[data-toggle=tooltip]').tooltip();   
      });"
      )
    ),
    leaflet::leafletOutput(ns("map"), height = '80vh'),
    absolutePanel(id = ns("controls"), class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 150, left = "auto", width = "25%",
                  # Panel content
                  span(
                    id = ns("infoIcon"),
                    `data-toggle` = "tooltip",
                    `data-placement` = "right",
                    `data-trigger` = "click hover",
                    title = "Placeholder",
                    icon("info-circle", style = "font-size: 150%;")),
                  selectizeInput(ns("typeFlt"), "Data Type", choices = c("All")), # choices and labels are updated in the server module
                  selectizeInput(ns("paramFlt"), "Parameter", choices = c("All")),
                  selectizeInput(ns("projFlt"), "Project", choices = c("All")),
                  selectizeInput(ns("netFlt"), "Network", choices = c("All")),
                  actionButton(ns("reset"), "Reset Filters"),
                  style = "opacity: 0.9; z-index: 400;") # Adjust styling
  )
}


map <- function(id, con, language) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    data_types <- DBI::dbGetQuery(con, "SELECT p.* FROM param_types AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.param_type = p.param_type_code);")
    parameters <- DBI::dbGetQuery(con, "SELECT * FROM parameters")
    projects <- DBI::dbGetQuery(con, "SELECT * FROM projects")
    networks <- DBI::dbGetQuery(con, "SELECT * FROM networks")

    
    observe({
      lang <- language()
      abbrev <- translations[translations$id == "titleCase", ..lang][[1]]
      
      # Update the tooltip's text
      tooltipText <- translations[translations$id == "map_tooltip", ..lang][[1]]
      runjs(sprintf('$("#%s").attr("data-original-title", "%s").tooltip("dispose").tooltip();', "map-infoIcon", tooltipText))
      
      # Update selectize inputs and action button
      updateSelectizeInput(session, 
                        "typeFlt",
                        label = translations[translations$id == "data_type", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]],
                                    titleCase(data_types[[translations[translations$id == "param_type_col", ..lang][[1]]]], abbrev)))
      updateSelectizeInput(session,
                        "paramFlt",
                        label = translations[translations$id == "parameter", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]], 
                                    titleCase(parameters[[translations[translations$id == "param_name_col", ..lang][[1]]]], 
                                              abbrev)))
      updateSelectizeInput(session,
                        "projFlt",
                        label = translations[translations$id == "project", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]],
                                    titleCase(projects[[translations[translations$id == "generic_name_col", ..lang][[1]]]], 
                                              abbrev)))
      updateSelectizeInput(session,
                        "netFlt",
                        label = translations[translations$id == "network", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]],
                                     titleCase(networks[[translations[translations$id == "generic_name_col", ..lang][[1]]]], 
                                               abbrev)))
      updateActionButton(session,
                         "reset",
                         label = translations[translations$id == "reset", ..lang][[1]])
    })
    
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>% 
        leaflet::addProviderTiles("Esri.WorldTopoMap") %>%
        leaflet::setView(lng = -135.05, lat = 65.00, zoom = 5)  %>% # Center on Yukon
        htmlwidgets::onRender(
          "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }")
    })
    
    # Reset all filters
    observeEvent(input$reset, {
      lang <- language()
      abbrev <- translations[translations$id == "titleCase", ..lang][[1]]
      
      updateSelectizeInput(session, 
                        "typeFlt",
                        label = translations[translations$id == "data_type", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]],
                                    titleCase(data_types$param_type, abbrev)))
      updateSelectizeInput(session,
                        "paramFlt",
                        label = translations[translations$id == "parameter", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]], 
                                    titleCase(parameters[[translations[translations$id == "param_name_col", ..lang][[1]]]], 
                                              abbrev)))
      updateSelectizeInput(session,
                        "projFlt",
                        label = translations[translations$id == "project", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]],
                                    titleCase(projects[[translations[translations$id == "generic_name_col", ..lang][[1]]]], 
                                              abbrev)))
      updateSelectizeInput(session,
                        "netFlt",
                        label = translations[translations$id == "network", ..lang][[1]],
                        choices = c(translations[translations$id == "all", ..lang][[1]],
                                    titleCase(networks[[translations[translations$id == "generic_name_col", ..lang][[1]]]], 
                                              abbrev)))
    })
    
    # Update filters based on first selection
    observeEvent(input$typeFlt, {
      
    })
    
  })
}
