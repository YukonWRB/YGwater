# UI and server code for data corrections module

continuousCorrectionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf("
     /* Add colors to the accordion. Using ns() makes it specific to this module. */
      #%s .accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ", ns("accordion")))),
    page_fluid(
      accordion(
        accordion_panel(title = "Timeseries selection",
                        DT::DTOutput(ns("ts_table")))
      ),
      accordion(
        accordion_panel(title = "Existing corrections",
                        DT::DTOutput(ns("existing_corrections")))
      ),
      accordion(
        accordion_panel(title = "New correction",
                        textInput(ns("start_dt"), "Start datetime (UTC)", placeholder = "YYYY-MM-DD HH:MM:SS"),
                        textInput(ns("end_dt"), "End datetime (UTC)", placeholder = "YYYY-MM-DD HH:MM:SS"),
                        selectizeInput(ns("correction_type"), "Correction type", choices = NULL, width = "100%"),
                        numericInput(ns("value1"), "Value 1", value = NA, width = "100%"),
                        numericInput(ns("value2"), "Value 2", value = NA, width = "100%"),
                        numericInput(ns("window"), "Time window (seconds)", value = NA, width = "100%"),
                        textInput(ns("equation"), "Equation", width = "100%"),
                        actionButton(ns("preview"), "Preview"),
                        actionButton(ns("apply"), "Apply"))
      ),
      br(),
      plotly::plotlyOutput(ns("preview_plot"), height = "400px")
    )
  )
}

continuousCorrections <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'continuous.corrections', 'insert') AS can_insert"
    )
    if (!check$can_insert) {
      showModal(modalDialog(
        title = "Insufficient Privileges",
        "You do not have write privileges to the corrections table.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
    
    moduleData <- reactiveValues(types = DBI::dbGetQuery(session$userData$AquaCache,
                                                         "SELECT correction_type_id, correction_type, description, priority, value1, value1_description, value2, value2_description, timestep_window, equation FROM correction_types ORDER BY priority"))
    observe({
      req(session$userData$AquaCache, moduleData)
      updateSelectizeInput(session, "correction_type",
                           choices = stats::setNames(moduleData$types$correction_type_id, moduleData$types$correction_type),
                           server = TRUE)
    })
    
    # Show/hide and modify labels on inputs
    observeEvent(input$correction_type, {
      row <- moduleData$types[moduleData$types$correction_type_id == input$correction_type, ]
      if (nrow(row) == 0) return()
      
      if (!row$value1) {
        shinyjs::hide("value1")
      } else {
        shinyjs::show("value1")
        updateNumericInput(session, "value1", label = row$value1_description)
      }
      if (!is.na(row$value2)) {
        if (!row$value2) {
          shinyjs::hide("value2")
        } else {
          shinyjs::show("value2")
          updateNumericInput(session, "value2", label = row$value2_description)
        }
      } else {
        shinyjs::show("value2")
        updateNumericInput(session, "value2", label = paste0(row$value2_description, " (optional)"))
      }
      
      if (!row$timestep_window) {
        shinyjs::hide("window")
      } else {
        shinyjs::show("window")
        updateNumericInput(session, "window", label = "Time window (seconds)")
      }
      if (!row$equation) {
        shinyjs::hide("equation")
      } else {
        shinyjs::show("equation")
        updateTextInput(session, "equation", label = "Equation")
      }
    }, ignoreInit = TRUE)
    
    ts_meta <- reactive({
      dbGetQueryDT(session$userData$AquaCache,
                   "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS nominal_record_rate, note FROM continuous.timeseries_metadata_en")
    })
    
    output$ts_table <- DT::renderDT({
      DT::datatable(ts_meta(), 
                    selection = 'single',
                    options = list(
                      columnDefs = list(
                        list(targets = 0, 
                             visible = FALSE) #Hides the timeseries_id column. Column index numbers start at 0 here!!!
                      ),
                      scrollX = TRUE
                    ),
                    rownames = FALSE,
                    filter = 'top',
      )
    })
    
    # Observe clicked rows on the timeseries table
    timeseries <- reactiveVal(NULL)
    observeEvent(input$ts_table_rows_selected, {
      selected_row <- input$ts_table_rows_selected
      if (length(selected_row) > 0) {
        timeseries_id <- ts_meta()[selected_row, "timeseries_id"]
        timeseries(timeseries_id)
        updateTextInput(session, "start_dt", value = "")
        updateTextInput(session, "end_dt", value = "")
      } else {
        timeseries(NULL)
      }
    })
    
    # Pull and show existing corrections
    existing_corrections <- reactive({
      req(timeseries())
      query <- paste0(
        "SELECT c.correction_id, c.start_dt, c.end_dt, ct.priority, ct.correction_type, c.value1, c.value2, c.timestep_window, c.equation FROM continuous.corrections c LEFT JOIN correction_types ct ON ct.correction_type_id = c.correction_type WHERE c.timeseries_id = ",
        timeseries(), " ORDER BY start_dt")
      DBI::dbGetQuery(session$userData$AquaCache, query)
    })
    
    
    # Render a table with the existing corrections and a plot with the raw + corrected data
    observe({
      req(existing_corrections())
      # drop cols with only NA values
      sub <- existing_corrections()[, colSums(is.na(existing_corrections())) < nrow(existing_corrections())]
      output$existing_corrections <- DT::renderDT({
        DT::datatable(sub, 
                      options = list(
                        columnDefs = list(
                          list(targets = 1, 
                               visible = FALSE) #Hides the timeseries_id column. Column index numbers start at 0 here!!!
                        ),
                        scrollX = TRUE
                      ),
                      rownames = FALSE,
                      selection = 'single')
      })
      
      # TODO: Add a plot of the existing corrections
      
      
    })
    

    
    
    # TODO: Below is problematic, not actually returning the corrected values
    get_preview <- function() {
      req(timeseries(), input$start_dt, input$end_dt)
      query <- paste0(
        "SELECT datetime, value, continuous.apply_corrections(",
        timeseries(), ", datetime, value) AS corrected FROM measurements_continuous WHERE timeseries_id = ",
        timeseries(),
        " AND datetime BETWEEN '", input$start_dt, "' AND '", input$end_dt, "' ORDER BY datetime")
      DBI::dbGetQuery(session$userData$AquaCache, query)
    }
    
    render_preview <- function(dat) {
      output$preview_plot <- plotly::renderPlotly({
        req(nrow(dat) > 0)
        plotly::plot_ly(dat, x = ~datetime) %>%
          plotly::add_lines(y = ~value, name = "Original") %>%
          plotly::add_lines(y = ~corrected, name = "Corrected")
      })
    }
    
    observeEvent(input$preview, {
      render_preview(dat)
    })
    
    observeEvent(input$apply, {
      req(timeseries(), input$start_dt, input$end_dt, input$correction_type)
      DBI::dbExecute(
        session$userData$AquaCache,
        "INSERT INTO corrections (timeseries_id, start_dt, end_dt, correction_type, value1, value2, timestep_window, equation) VALUES ($1,$2,$3,$4,$5,$6,make_interval(secs => $7),$8)",
        params = list(
          as.integer(timeseries()),
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
