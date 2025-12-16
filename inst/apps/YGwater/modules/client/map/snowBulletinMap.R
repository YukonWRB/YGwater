# =============================================================================
# Snow Water Equivalent (SWE) Basin Visualization - Shiny App
# =============================================================================
# This Shiny app creates an interactive leaflet map showing:
# - SWE basins colored by weighted SWE values from discrete stations
# - Continuous SWE monitoring stations colored by relative change
# - Discrete SWE monitoring stations colored by relative change
# - Communities, roads, and geographic boundaries
#
# The map displays snow water equivalent data as a percentage of historic normal
# for a specific date (year and day-of-year). Basin values are calculated using
# weighted averages from discrete snow course stations.
#
# Data Sources:
# - SWE basins: Local shapefile (swe_basins_ExportFeatures.shp)
# - Stations/Communities/Roads: PostgreSQL spatial database (AquaCache)
# - Timeseries data: Continuous and discrete measurements database
# - Snowcourse factors: CSV file with basin weighting factors
# =============================================================================
# DATA LOADING AND PROCESSING
# =============================================================================

# Load all base data once at startup

# =============================================================================
# SHINY APP UI
# =============================================================================

mapSnowbullUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    # Sidebar + main contentexpandLimits
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns('sidebar_page')),
        width = 2
      ),
      shiny::mainPanel(
        leaflet::leafletOutput(ns("map"), height = "100vh"),
        width = 9
      )
    ),
    # JavaScript for plot generation
    tags$script(HTML(sprintf(
      "function generatePlot(type, stationId, stationName) {
        console.log('DEBUG: generatePlot called with:', type, stationId, stationName);
        var popup = document.querySelector('.leaflet-popup-content');
        if (popup) {
          // Show loading and widen only this popup immediately on button press
          popup.innerHTML = '<div style=\"text-align: center; padding: 20px;\"><b>' + 
            stationName + '</b><br><br>Loading plot...<br>' + 
            '<div style=\"margin-top: 10px;\">⏳</div></div>';
          var wrapper = popup.closest('.leaflet-popup-content-wrapper');
          if (wrapper) wrapper.style.maxWidth = '720px';
          popup.style.width = '660px';
        }
        Shiny.setInputValue('%s', {
          type: type,
          station_id: stationId,
          station_name: stationName,
          timestamp: Date.now()
        });
      }",
      ns("generate_plot")
    ))),
    # Remove global wide popup CSS (keep popups narrow by default)
    # tags$head(tags$style(HTML(
    #   "
    #   .leaflet-popup-content-wrapper { max-width: 720px !important; }
    #   .leaflet-popup-content { width: 660px !important; }
    # "
    # ))),
    # Receive plot HTML from server; widen only the current popup
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('updatePopup', function(message) {
         console.log('DEBUG: updatePopup handler called');
         var popup = document.querySelector('.leaflet-popup-content');
         if (popup && message && message.html) {
           popup.innerHTML = message.html;
           var wrapper = popup.closest('.leaflet-popup-content-wrapper');
           if (wrapper) wrapper.style.maxWidth = '720px';
           popup.style.width = '660px';
         }
       });"
    ))
  )
}

mapSnowbull <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    months <- snowbull_months(short = TRUE)

    con <- session$userData$AquaCache
    snowbull_data <- load_bulletin_data(con)

    static_style_elements <- get_static_style_elements()

    # --- Server-side sidebar UI ---
    output$sidebar_page <- shiny::renderUI({
      div(
        id = ns("controls_panel"),
        div(
          style = "margin-bottom: 15px;",
          tags$label(
            tr("gen_snowBul_year", language$language),
            style = "display: block; margin-bottom: 5px;"
          ),
          selectInput(
            ns("year"),
            label = NULL,
            choices = as.character(2025:2000),
            selected = "2025",
            width = "100%"
          )
        ),
        div(
          style = "margin-bottom: 15px;",
          tags$label(
            tr("month", language$language),
            style = "display: block; margin-bottom: 5px;"
          ),
          selectInput(
            ns("month"),
            label = NULL,
            choices = setNames(
              as.character(c(2, 3, 4, 5)),
              c(
                tr("feb", language$language),
                tr("mar", language$language),
                tr("apr", language$language),
                tr("may", language$language)
              )
            ),
            selected = "3",
            width = "100%"
          )
        ),
        div(
          style = "margin-bottom: 10px;",
          tags$label(
            tr("snowbull_values", language$language),
            style = "display: block; margin-bottom: 5px;"
          ),
          radioButtons(
            ns("value_type"),
            label = NULL,
            choices = setNames(
              c("relative_to_med", "data", "percentile"),
              c(
                tr("snowbull_relative_median", language$language),
                tr("snowbull_swe", language$language),
                tr("snowbull_percentile", language$language)
              )
            ),
            selected = "relative_to_med",
            inline = FALSE
          )
        ),
        tags$details(
          tags$summary(tr("snowbull_details", language$language)),
          shiny::uiOutput(ns("map_details"))
        )
      )
    })

    # --- Reactive for processed data ---
    processed_data <- shiny::reactive({
      shiny::req(input$year, input$month)

      get_processed_data(
        year = input$year,
        month = input$month,
        snowbull_data = snowbull_data,
        shiny = TRUE,
        language = language$language
      )
    })

    # --- Reactive for map output ---
    map_output <- shiny::reactive({
      make_leaflet_map(
        data = processed_data(),
        value_type = input$value_type,
        language = language$language,
        snowbull_data = snowbull_data,
        month = as.integer(input$month),
        year = as.integer(input$year)
      )
    })

    # --- Render map using reactive output ---
    output$map <- leaflet::renderLeaflet({
      map_output()
    })

    # --- Observer: plot generation requests ---
    shiny::observeEvent(input$generate_plot, {
      req(
        input$generate_plot$type,
        input$generate_plot$station_id,
        input$generate_plot$station_name
      )

      # --- Popup plotting functionality  ---
      # here we access the corresponding plot function (continuous or discrete)
      # the relevant timeseries data is filtered to the selected year
      # the functions return HTML content for the popup
      plot_html <-
        if (input$generate_plot$type == "pillow") {
          create_continuous_plot_popup(
            timeseries = snowbull_data$pillows$timeseries$data[
              snowbull_data$pillows$timeseries$data$datetime <=
                as.Date(paste0(input$year, "-12-31")),
              c("datetime", as.character(input$generate_plot$station_id))
            ],
            year = as.integer(input$year),
            con = con,
            station_name = input$generate_plot$station_name,
            language = language$language
          )
        } else if (input$generate_plot$type == "survey") {
          create_discrete_plot_popup(
            timeseries = snowbull_data$surveys$timeseries$data[
              snowbull_data$surveys$timeseries$data$datetime <=
                as.Date(paste0(input$year, "-12-31")),
              c("datetime", as.character(input$generate_plot$station_id))
            ],
            station_name = input$generate_plot$station_name,
            language = language$language
          )
        } else if (input$generate_plot$type == "basin") {
          create_discrete_plot_popup(
            timeseries = snowbull_data$basins$timeseries$data[
              snowbull_data$basins$timeseries$data$datetime <=
                as.Date(paste0(input$year, "-12-31")),
              c(
                "datetime",
                input$generate_plot$station_id
              )
            ],
            station_name = input$generate_plot$station_name,
            language = language$language
          )
        }
      session$sendCustomMessage("updatePopup", list(html = plot_html))
    })

    # --- Render selected date text with details ---
    output$map_details <- shiny::renderText({
      shiny::req(input$year, input$month)
      date_obj <- get_datetime(input$year, input$month)
      shiny::HTML(paste0(
        "<b>",
        tr("snowbull_date", language$language),
        ":</b> ",
        format(date_obj, "%B %d, %Y"),
        "<br>",
        "<b>",
        tr("snowbull_historical_period", language$language),
        ":</b> 1991–2020<br>",
        "<b>",
        tr("snowbull_data_credits", language$language),
        "</b><br>",
        "<b>",
        tr("snowbull_map_credits", language$language),
        "</b><br>",
        tr("snowbull_disclaimer", language$language),
        "<br>",
        "<b>",
        tr("snowbull_gen_on", language$language),
        ":</b> ",
        format(Sys.time(), "%B %d, %Y at %H:%M %Z"),
        "<br>",
        "<b>",
        tr("app_version", language$language),
        "</b> ",
        as.character(utils::packageVersion("YGwater"))
      ))
    })
  })
}
