syncContUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    h3("Synchronize continuous timeseries"),
    tooltip(
      checkboxInput(ns("all_ts"), "All timeseries", FALSE),
      "Selects all timeseries in the database. If unchecked, you can select specific timeseries from the table below.",
      placement = "right"
    ),
    conditionalPanel(
      condition = "input.all_ts == false",
      ns = ns,
      DT::DTOutput(ns("ts_table"))
    ),
    dateInput(ns("start"), "Start datetime", value = Sys.Date() - 30),
    selectInput(
      ns("active"),
      "Active status behavior",
      choices = stats::setNames(
        c("default", "all"),
        c("Labelled 'active' = TRUE only", "All")
      ),
      selected = "default"
    ),
    checkboxInput(ns("parallel"), "Run in parallel", FALSE),
    input_task_button(ns("run"), "Synchronize"),
    # Small divider
    hr(),
    DT::DTOutput(ns("result"))
  )
}

syncCont <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "syncCont"
      )
    })

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
      res <- dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS nominal_record_rate, note FROM continuous.timeseries_metadata_en"
      )
      # Make columns factors for better filtering in DT
      res[, location := as.factor(location)]
      res[, parameter := as.factor(parameter)]
      res[, media := as.factor(media)]
      res[, aggregation := as.factor(aggregation)]
      res[, nominal_record_rate := as.factor(nominal_record_rate)]
      res
    })

    output$ts_table <- DT::renderDT({
      DT::datatable(
        ts_meta(),
        selection = 'multiple',
        options = list(
          columnDefs = list(
            list(targets = 1, visible = FALSE) #Hides the timeseries_id column. Column index numbers start at 0 here!!!
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
        ),
        filter = 'top',
      )
    })

    task <- ExtendedTask$new(function(
      ts_ids,
      start_dt,
      active,
      parallel,
      config
    ) {
      promises::future_promise({
        con <- AquaConnect(
          name = config$dbName,
          host = config$dbHost,
          port = config$dbPort,
          username = config$dbUser,
          password = config$dbPass,
          silent = TRUE
        )
        if (!is.null(con)) {
          on.exit(DBI::dbDisconnect(con))
        }
        AquaCache::synchronize_continuous(
          con = if (parallel) NULL else con,
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
    }) |>
      bind_task_button("run")

    observeEvent(
      input$run,
      {
        req(input$start)
        if (isTRUE(input$all_ts)) {
          ids <- "all"
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
        task$invoke(
          ids,
          input$start,
          input$active,
          input$parallel,
          session$userData$config
        )
      },
      ignoreInit = TRUE
    )

    output$result <- DT::renderDT({
      req(task$result())
      DT::datatable(
        task$result(),
        rownames = FALSE,
        selection = "none",
        filter = "none",
        options = list(
          scrollx = TRUE,
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
  })
}
