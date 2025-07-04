syncDiscUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    h3("Synchronize sample series"),
    tooltip(
      checkboxInput(ns("all_ss"), "All sample series", FALSE),
      "Selects all sample series in the database. If unchecked, you can select specific sample series from the table below.",
      placement = "right"
      ),
    conditionalPanel(
      condition = "input.all_ss == false",
      ns = ns,
      DT::DTOutput(ns("ss_table"))
    ),
    dateInput(ns("start"), "Start datetime", value = Sys.Date() - 30),
    selectInput(ns("active"), "Active status behavior", choices = stats::setNames(c("default", "all"), c("Labelled 'active' = TRUE only", "All")), selected = "default"),
    checkboxInput(ns("delete"), "Delete samples missing from the remote?", FALSE),
    input_task_button(ns("run"), "Synchronize"),
    verbatimTextOutput(ns("result"))
  )
}

syncDisc <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'discrete.sample_series', 'UPDATE') AS can_update"
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
    
    ss_meta <- reactive({
      dbGetQueryDT(session$userData$AquaCache,
                   "SELECT ss.sample_series_id, loc.name AS location, sl.sub_location_name AS sub_location, ss.source_fx FROM discrete.sample_series ss JOIN public.locations loc ON ss.location_id = loc.location_id LEFT JOIN public.sub_locations sl ON ss.sub_location_id = sl.sub_location_id")
    })
    
    output$ss_table <- DT::renderDT({
      DT::datatable(ss_meta(), 
                    selection = 'multiple',
                    options = list(
                      columnDefs = list(
                        list(targets = 1, 
                             visible = FALSE) #Hides the sample_series_id column. Column index numbers start at 0 here!!!
                      ),
                      scrollX = TRUE,
                      initComplete = htmlwidgets::JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({",
                        "  'background-color': '#079',",
                        "  'color': '#fff',",
                        "  'font-size': '100%',",
                        "});",
                        "$(this.api().table().body()).css({",
                        "  'font-size': '90%',",
                        "});",
                        "}"
                      )
                    )
      )
    })
    
    task <- ExtendedTask$new(function(ss_ids, start_dt, active, del, parallel, config) {
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
        AquaCache::synchronize_discrete(
          con = con,
          sample_series_id = ss_ids,
          start_datetime = start_dt,
          active = active,
          delete = del,
          snowCon = NULL, # Assumes the snow DB is on the same server as AquaCache
          EQCon = NULL
        )
      })
    }) |> bind_task_button("run")
    
    observeEvent(input$run, {
      req(input$start)
      ids <- if (isTRUE(input$all_ss)) "all" else ss_meta()[input$ss_table_rows_selected, "sample_series_id"][[1]]
      task$invoke(ids, input$start, input$active, input$delete, input$parallel, session$userData$config)
    }, ignoreInit = TRUE)
    
    output$result <- renderPrint({
      req(task$result())
      task$result()
    })
  })
}
