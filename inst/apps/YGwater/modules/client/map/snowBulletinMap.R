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
    uiOutput(ns("banner")),
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
        // Use Shiny.setInputValue with a random value to force event even if args are the same
        Shiny.setInputValue('%s', {
          type: type,
          station_id: stationId,
          station_name: stationName,
          nonce: Math.random().toString(36).substring(2)
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
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    months <- snowbull_months(short = TRUE)

    con <- session$userData$AquaCache
    snowbull_shapefiles <- load_bulletin_shapefiles(con)
    snowbull_timeseries <- load_bulletin_timeseries(con)

    static_style_elements <- get_static_style_elements()

    output$banner <- renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "mapSnowbull"
      )
    })

    # --- Server-side sidebar UI ---
    output$sidebar_page <- renderUI({
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
            ns("statistic"),
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

    # --- Reactive for processed data and map output ---
    map_output <- shiny::reactive({
      shiny::req(input$year, input$month)

      # get the 'current' data for the specified date, and create the popup data
      # returns list of sf objects with data columns
      map_data <- list(
        point_data = NULL,
        point_data_secondary = NULL,
        poly_data = NULL
      )

      timeseries_data <- list(
        poly_data = snowbull_timeseries$swe$basins,
        point_data = snowbull_timeseries$swe$surveys,
        point_data_secondary = snowbull_timeseries$swe$pillows
      )

      dynamic_style_elements <- get_dynamic_style_elements(
        statistic = input$statistic,
        language = language$language
      )

      for (data_type in names(map_data)) {
        if (!is.null(timeseries_data[[data_type]])) {
          map_data[[data_type]] <- get_display_data(
            year = as.integer(input$year),
            month = as.integer(input$month),
            dataset = timeseries_data[[data_type]],
            statistic = input$statistic,
            language = language$language
          )

          map_data[[data_type]]$fill_colour <- get_state_style_elements(
            map_data[[data_type]]$value_to_show,
            style_elements = dynamic_style_elements
          )
        }
      }

      make_leaflet_map(
        point_data = map_data$point_data,
        poly_data = map_data$poly_data,
        point_data_secondary = map_data$point_data_secondary,
        statistic = input$statistic,
        language = language$language,
        snowbull_shapefiles = snowbull_shapefiles,
        month = as.integer(input$month),
        year = as.integer(input$year)
      )
    })

    # --- Render map using reactive output ---
    output$map <- leaflet::renderLeaflet({
      map_output()
    })

    # --- Observer: plot generation requests ---
    shiny::observeEvent(
      input$generate_plot,
      {
        req(
          input$generate_plot$type,
          input$generate_plot$station_id,
          input$generate_plot$station_name
        )

        # --- DEBUGGING CODE: Just show a message, no plot ---
        debug_message <- sprintf(
          "<div style='text-align: center; padding: 40px; font-size: 18px; color: #2a5d9f;'>
          <b>generate_plot event received!</b><br>
          <br>
          <b>Type:</b> %s<br>
          <b>Station ID:</b> %s<br>
          <b>Station Name:</b> %s<br>
          <b>Year:</b> %s<br>
          <b>Month:</b> %s<br>
          <br>
          <span style='color: #888;'>If you see this, the button and observer are working.</span>
        </div>",
          htmltools::htmlEscape(input$generate_plot$type),
          htmltools::htmlEscape(input$generate_plot$station_id),
          htmltools::htmlEscape(input$generate_plot$station_name),
          htmltools::htmlEscape(isolate(input$year)),
          htmltools::htmlEscape(isolate(input$month))
        )

        session$sendCustomMessage("updatePopup", list(html = debug_message))
      },
      ignoreInit = TRUE
    )

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
