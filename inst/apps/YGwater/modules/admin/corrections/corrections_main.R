# UI and server code for data corrections module

correctionsUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    numericInput(ns("timeseries_id"), "Timeseries ID", value = NULL, width = "100%"),
    textInput(ns("start_dt"), "Start datetime (UTC)", placeholder = "YYYY-MM-DD HH:MM:SS"),
    textInput(ns("end_dt"), "End datetime (UTC)", placeholder = "YYYY-MM-DD HH:MM:SS"),
    selectizeInput(ns("correction_type"), "Correction type", choices = NULL, width = "100%"),
    numericInput(ns("value1"), "Value 1", value = NA, width = "100%"),
    numericInput(ns("value2"), "Value 2", value = NA, width = "100%"),
    numericInput(ns("window"), "Time window (seconds)", value = NA, width = "100%"),
    textInput(ns("equation"), "Equation", width = "100%"),
    actionButton(ns("preview"), "Preview"),
    actionButton(ns("apply"), "Apply"),
    br(),
    page_sidebar(
      sidebar = sidebar(
        DT::dataTableOutput(ns("preview_table")),
        bg = config$sidebar_bg,
        open = list(mobile = "always-above")
      ),
      plotly::plotlyOutput(ns("preview_plot"), height = "400px")
    )
  )
}

corrections <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(session$userData$AquaCache)
      types <- DBI::dbGetQuery(session$userData$AquaCache,
                               "SELECT correction_type_id, correction_type FROM correction_types ORDER BY priority")
      updateSelectizeInput(session, "correction_type",
                           choices = stats::setNames(types$correction_type_id, types$correction_type),
                           server = TRUE)
    })

    get_preview <- function() {
      req(input$timeseries_id, input$start_dt, input$end_dt)
      query <- paste0(
        "SELECT datetime, value, continuous.apply_corrections(",
        input$timeseries_id, ", datetime, value) AS corrected FROM measurements_continuous WHERE timeseries_id = ",
        input$timeseries_id,
        " AND datetime BETWEEN '", input$start_dt, "' AND '", input$end_dt, "' ORDER BY datetime")
      DBI::dbGetQuery(session$userData$AquaCache, query)
    }

    render_preview <- function(dat) {
      output$preview_table <- DT::renderDataTable({
        DT::datatable(dat, options = list(scrollX = TRUE))
      })
      output$preview_plot <- plotly::renderPlotly({
        req(nrow(dat) > 0)
        plotly::plot_ly(dat, x = ~datetime) %>%
          plotly::add_lines(y = ~value, name = "Original") %>%
          plotly::add_lines(y = ~corrected, name = "Corrected")
      })
    }

    observeEvent(input$preview, {
      dat <- get_preview()
      render_preview(dat)
    })

    observeEvent(input$apply, {
      req(input$timeseries_id, input$start_dt, input$end_dt, input$correction_type)
      DBI::dbExecute(
        session$userData$AquaCache,
        "INSERT INTO corrections (timeseries_id, start_dt, end_dt, correction_type, value1, value2, timestep_window, equation) VALUES ($1,$2,$3,$4,$5,$6,make_interval(secs => $7),$8)",
        params = list(
          as.integer(input$timeseries_id),
          input$start_dt,
          input$end_dt,
          as.integer(input$correction_type),
          input$value1,
          input$value2,
          if (is.na(input$window)) NA else as.integer(input$window),
          if (nzchar(input$equation)) input$equation else NA
        )
      )
      showNotification("Correction added.", type = "message")
      dat <- get_preview()
      render_preview(dat)
    })
  })
}
