syncContUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    h3("Synchronize continuous timeseries"),
    checkboxInput(ns("all_ts"), "All timeseries", FALSE),
    conditionalPanel(
      condition = "input.all_ts == false",
      ns = ns,
      DT::DTOutput(ns("ts_table"))
    ),
    dateInput(ns("start"), "Start datetime", value = Sys.Date() - 30),
    selectInput(ns("active"), "Active status behavior", choices = stats::setNames(c("default", "all"), c("Labelled 'active' = TRUE only", "All")), selected = "default"),
    checkboxInput(ns("parallel"), "Run in parallel", FALSE),
    input_task_button(ns("run"), "Synchronize"),
    # Small divider
    hr(),
    verbatimTextOutput(ns("result"))
  )
}

syncCont <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'continuous.timeseries', 'UPDATE') AS can_update"
    )
    
    if (!check$can_update) {
      showModal(modalDialog(
        title = "Insufficient Privileges",
        "You do not have the necessary privileges to synchronize this database.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      shinyjs::disable("run")
    }
    
    ts_meta <- reactive({
      dbGetQueryDT(session$userData$AquaCache,
                   "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS nominal_record_rate, note, source_fx FROM continuous.timeseries_metadata_en")
    })
    
    output$ts_table <- DT::renderDT({
      DT::datatable(ts_meta(), 
                    selection = 'multiple',
                    options = list(
                      columnDefs = list(
                        list(targets = 0, 
                             visible = FALSE) #Hides the timeseries_id column. Column index numbers start at 0 here!!!
                      ),
                      scrollX = TRUE
                    )
      )
    })
    
    task <- ExtendedTask$new(function(ts_ids, start_dt, active, parallel, config) {
      promises::future_promise({
        con <- AquaConnect(
          name = config$dbName,
          host = config$dbHost,
          port = config$dbPort,
          username = config$dbUser,
          password = config$dbPass,
          silent = TRUE
        )
        if (!is.null(con)) on.exit(DBI::dbDisconnect(con))
        AquaCache::synchronize_continuous(
          con = con,
          timeseries_id = ts_ids,
          start_datetime = start_dt,
          active = active,
          dbName = if (parallel) config$dbName else NULL,
          dbHost = if (parallel) config$dbHost else NULL,
          dbPort = if (parallel) config$dbPort else NULL,
          dbUser = if (parallel) config$dbUser else NULL,
          dbPass = if (parallel) config$dbPass else NULL
        )
      })
    }) |> bind_task_button("run")
    
    observeEvent(input$run, {
      req(input$start)
      if (isTRUE(input$all_ts)){
        ids <-  "all"
      } else {
        ids <- ts_meta()[input$ts_table_rows_selected, "timeseries_id"][[1]]
        if (length(ids) == 0) {
          showModal(modalDialog(
            title = "No Timeseries Selected",
            "Please select at least one timeseries to synchronize.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
      }
      task$invoke(ids, input$start, input$active, input$parallel, session$userData$config)
    }, ignoreInit = TRUE)
    
    output$result <- renderPrint({
      req(task$result())
      task$result()
    })
  })
}
