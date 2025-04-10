library(shiny)
library(leaflet)
library(DT)
library(YGwater)
library(AquaCache)

ui <- fluidPage(
    fluidRow(
        column(8,
               leafletOutput("map", height = 400)
        ),
        column(4,
               uiOutput("image_display")
        )
    ),
    fluidRow(
        column(12,
               DTOutput("table")
        )
    )
)

config <<- list(
    dbName = "aquacache",
    dbHost = Sys.getenv("aquacacheHost"),
    dbPort = Sys.getenv("aquacachePort"),
    dbUser = Sys.getenv("aquacacheAdminUser"),
    dbPass = Sys.getenv("aquacacheAdminPass")
)

server <- function(input, output, session) {

    session$userData$config <- config

    session$userData$AquaCache <- AquaConnect(name = config$dbName,
                                              host = config$dbHost,
                                              port = config$dbPort,
                                              username = config$dbUser,
                                              password = config$dbPass,
                                              silent = TRUE)

    images <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM files.images LIMIT 10000")
    locations <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM public.locations")

    colnames(locations)[colnames(locations) == "name"] <- "location_name"
    images <- merge(images, locations[, c("location_id", "location_name")], 
                    by = "location_id", all.x = TRUE)


    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(
                lng = images$longitude,
                lat = images$latitude,
                layerId = images$image_id,
                popup = paste0("<b>", images$image_id, "</b><br>",
                               "Time: ", images$datetime, "<br>",
                               "Location: ", images$location_name, "<br>"),
                radius = 6,
                color = "blue",
                fillColor = "lightblue",
                fillOpacity = 0.7,
                clusterOptions = markerClusterOptions()
            ) %>%
            setView(lng = mean(images$longitude, na.rm = TRUE),
                    lat = mean(images$latitude, na.rm = TRUE),
                    zoom = 4)
    })

    output$table <- renderDT({
        datatable(images[, c("image_id", "location_name", "datetime", "latitude", "longitude", "elevation_msl_m")], 
                  options = list(pageLength = 10),
                  rownames = FALSE,
                  selection = "single")
    })


    output$image_display <- renderUI({
        selected_image <- input$table_rows_selected
        if (length(selected_image) == 0) {
            return(NULL)
        }
        selected_image <- images[selected_image, ]
        tags$img(src = paste0("data:image/png;base64,",
                      base64enc::base64encode(do.call(c, lapply(selected_image$file, as.raw)))),
             style = "width: 100%; height: auto;")
    })
    observeEvent(input$map_marker_click, {
        clicked_marker <- input$map_marker_click
        selected_image <- images[images$image_id == clicked_marker$id, ]
        if (nrow(selected_image) > 0) {
            selected_row <- which(images$image_id == clicked_marker$id)
            DT::dataTableProxy("table") %>% selectRows(selected_row)
        }
    })

}

shinyApp(ui, server)
