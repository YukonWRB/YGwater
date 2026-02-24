# UI and server code for adding new continuous measurements

addContDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ",
        ns("accordion1")
      )),
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #E5F4F6;
        /* collapsed header */
        --bs-accordion-btn-bg:      #0097A9;
        /* expanded header */
        --bs-accordion-active-bg:   #0097A9;
      }
    ",
        ns("accordion2")
      ))
    ),
    tags$head(tags$style(HTML(
      ".shiny-split-layout > div {overflow: visible;}"
    ))),
    page_fluid(
      uiOutput(ns("banner")),
      actionButton(
        ns("reload_module"),
        "Reload module data",
        icon = icon("refresh")
      ),
      accordion(
        id = ns("accordion1"),
        open = "ts_panel",
        accordion_panel(
          id = ns("ts_panel"),
          title = "Timeseries selection",
          actionButton(ns("addNewTS"), "Click here to add a new timeseries"),
          DT::DTOutput(ns("ts_table"))
        )
      ),

      accordion(
        id = ns("accordion2"),
        open = "data_entry_panel",
        accordion_panel(
          id = ns("data_entry_panel"),
          title = "Add data",
          icon = icon("table"),
          radioButtons(
            ns("entry_mode"),
            "Input method",
            choices = c("File" = "file", "Manual" = "manual"),
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.entry_mode == 'file'",
            ns = ns,
            fileInput(
              ns("file"),
              "Upload .csv or .xlsx",
              accept = c(".csv", ".xlsx")
            )
          ),
          div(
            actionButton(ns("add_row"), "Add row to end"),
            actionButton(ns("add_row_above"), "Add row above selection"),
            actionButton(ns("add_row_below"), "Add row below selection")
          ),
          tags$br(),

          DT::DTOutput(ns("data_table")),

          tags$div(
            "Hint: double click a cell to edit its contents."
          ),
          tags$br(),
          selectizeInput(
            ns("UTC_offset"),
            "UTC offset of data",
            choices = seq(-12, 12, by = 1),
            selected = 0,
            multiple = FALSE
          ),
          splitLayout(
            cellWidths = c("50%", "50%"),
            selectizeInput(
              ns("owner"),
              "Owner organization (applies to these data only)",
              choices = NULL,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select owner")
            ),
            selectizeInput(
              ns("contributor"),
              "Contributor organization (applies to these data only)",
              choices = NULL,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select contributor")
            )
          ),
          radioButtons(
            ns("no_update"),
            "Prevent updates to these data by automatic processes?",
            choices = c("Yes" = "yes", "No" = "no"),
            inline = TRUE,
            selected = "no"
          ),
          tags$div(
            "Note: data visibility is controlled by the timeseries visibility parameters."
          )
        ),
        # accordion to hold a plotly plot of the uploaded/added data for quick visual checks before upload
        accordion_panel(
          id = ns("preview_panel"),
          title = "Preview data",
          icon = icon("chart-line"),
          checkboxInput(
            ns("preview_historic_range"),
            "Show historic range",
            value = TRUE
          ),
          textInput(
            ns("preview_start_datetime"),
            "Preview start datetime (YYYY-MM-DD HH:MM:SS, UTC offset specified above)",
            placeholder = "Default uses earliest new data datetime"
          ),
          textInput(
            ns("preview_end_datetime"),
            "Preview end datetime (YYYY-MM-DD HH:MM:SS, UTC offset specified above)",
            placeholder = "Default uses latest new data datetime"
          ),
          bslib::input_task_button(
            ns("refresh_plot"),
            "Refresh plot",
            icon = icon("refresh")
          ),
          plotly::plotlyOutput(ns("data_preview"))
        ),

        # Add delete/grade/approval/qualifier functionality within accordions
        # Delete regions panel
        accordion_panel(
          id = ns("delete_panel"),
          title = "Delete data",
          icon = icon("trash"),
          actionButton(ns("delete_rows"), "Delete selected rows"),
          tags$br(),
          textInput(
            ns("delete_cutoff_datetime"),
            "Delete data before/after datetime (YYYY-MM-DD HH:MM:SS)",
            placeholder = "2024-01-01 00:00:00"
          ),
          div(
            actionButton(
              ns("delete_before_datetime"),
              "Delete rows before datetime"
            ) |>
              tooltip(
                "Only delete data that has no possible later use, such as pre/post deployment data. Data that has a non-zero chance of being useful later should be uploaded and can be suppressed using a delete region correction or graded/qualified appropriately."
              ),
            actionButton(
              ns("delete_after_datetime"),
              "Delete rows after datetime"
            ) |>
              tooltip(
                "Only delete data that has no possible later use, such as pre/post deployment data. Data that has a non-zero chance of being useful later should be uploaded and can be suppressed using a delete region correction or graded/qualified appropriately."
              )
          )
        ), # end delete accordion panel

        # Add grade panel
        accordion_panel(
          id = ns("grade_panel"),
          title = "Add grades",
          icon = icon("check"),
          div(
            actionButton(
              ns("add_grade"),
              "Add grade column with default value"
            )
          )
        ), # end grade accordion panel

        # Add qualifiers panel
        accordion_panel(
          id = ns("qualifier_panel"),
          title = "Add qualifiers",
          icon = icon("flag"),
          div(
            actionButton(
              ns("add_qc"),
              "Add QC column with default value"
            )
          )
        ), # End qualifiers accordion panel

        # Add approvals panel
        accordion_panel(
          id = ns("approval_panel"),
          title = "Add approval status",
          icon = icon("thumbs-up"),
          div(
            actionButton(
              ns("add_approval"),
              "Add approval column with default value"
            )
          )
        ) # End approval panel
      ), # end accordion for data manipulation options

      br(),

      actionButton(
        ns("upload"),
        "Upload to AquaCache (no overwrite)",
        style = "font-size: 14px;"
      ),
      actionButton(
        ns("upload_overwrite_all"),
        "Upload to AquaCache (replace all points in new data range)",
        style = "font-size: 14px;"
      ),
      actionButton(
        ns("upload_overwrite_some"),
        "Upload to AquaCache (overwrite conflicting points only)",
        style = "font-size: 14px;"
      )
    )
  )
}

addContData <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addContData"
      )
    })

    outputs <- reactiveValues() # Used to pass the user on to adding a timeseries directly

    moduleData <- reactiveValues(
      organizations = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM public.organizations ORDER BY name ASC"
      )
    )

    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'continuous.measurements_continuous', 'INSERT') AS can_insert"
    )
    if (!check$can_insert) {
      showModal(modalDialog(
        title = 'Insufficient Privileges',
        'You do not have write privileges to add measurements.',
        easyClose = TRUE,
        footer = modalButton('Close')
      ))
      shinyjs::disable('upload')
      shinyjs::disable('upload_overwrite_all')
      shinyjs::disable('upload_overwrite_some')
    }

    ts_meta <- reactiveVal({
      dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS record_rate_minutes FROM continuous.timeseries_metadata_en ORDER BY location_name, parameter_name, media_type, aggregation_type, record_rate_minutes ASC"
      )
    })

    # Reload module data when asked
    observeEvent(input$reload_module, {
      ts_meta(dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS record_rate_minutes FROM continuous.timeseries_metadata_en ORDER BY location_name, parameter_name, media_type, aggregation_type, record_rate_minutes ASC"
      ))
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM public.organizations ORDER BY name ASC"
      )
    })

    # Change to add timeseries tab when button clicked
    observeEvent(input$addNewTS, {
      outputs$change_tab <- "addTimeseries"
    })

    output$ts_table <- DT::renderDT({
      # Convert some data types to factors for better filtering in DT
      df <- ts_meta()
      df$record_rate_minutes <- as.factor(df$record_rate_minutes)
      df$location <- as.factor(df$location)
      df$media <- as.factor(df$media)
      df$aggregation <- as.factor(df$aggregation)
      df$parameter <- as.factor(df$parameter)
      DT::datatable(
        df,
        selection = 'single',
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)),
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
        rownames = FALSE
      )
    })

    # Observe timeseries selection and assign to reactiveVal
    timeseries <- reactiveVal(NULL)
    observeEvent(input$ts_table_rows_selected, {
      sel <- input$ts_table_rows_selected
      if (length(sel) > 0) {
        timeseries(ts_meta()[sel, 'timeseries_id'][[1]])
      } else {
        timeseries(NULL)
      }
    })

    # Update owner and contributor selectize inputs when organizations data is loaded
    observe({
      req(moduleData$organizations)
      updateSelectizeInput(
        session,
        "owner",
        choices = stats::setNames(
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        ),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "contributor",
        choices = stats::setNames(
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        ),
        server = TRUE
      )
    })

    # When timeseries is selected, update owner and contributor to default owner from timeseries table
    observeEvent(timeseries(), {
      req(timeseries())
      default_owner <- DBI::dbGetQuery(
        session$userData$AquaCache,
        sprintf(
          "SELECT default_owner FROM timeseries WHERE timeseries_id = %s",
          as.integer(timeseries())
        )
      )
      if (nrow(default_owner) > 0) {
        updateSelectizeInput(
          session,
          "owner",
          selected = default_owner$default_owner[[1]]
        )
        updateSelectizeInput(
          session,
          "contributor",
          selected = default_owner$default_owner[[1]]
        )
      }
    })

    ### Observe the owner selectizeInput for new owners ############
    addOrgModal <- function(name) {
      # Called when adding owner or contributor
      showModal(modalDialog(
        textInput(
          ns("org_name"),
          "Organization name",
          value = input[[name]]
        ),
        textInput(ns("org_name_fr"), "Organization name French (optional)"),
        textInput(ns("org_contact_name"), "Contact name (optional)"),
        textInput(ns("org_contact_phone"), "Contact phone (optional)"),
        textInput(ns("org_contact_email"), "Contact email (optional)"),
        textInput(
          ns("org_contact_note"),
          "Contact note (optional, for context)"
        ),
        actionButton(ns(paste0("add_", name)), "Add organization")
      ))
    }
    observeEvent(
      input$owner,
      {
        # Check for new organization (not in the list already)
        if (
          input$owner %in%
            moduleData$organizations$organization_id ||
            nchar(input$owner) == 0
        ) {
          return()
        }
        # If new, show modal dialog to add organization details
        addOrgModal(name = "owner")
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$contributor,
      {
        # Check for new organization (not in the list already)
        if (
          input$contributor %in%
            moduleData$organizations$organization_id ||
            nchar(input$contributor) == 0
        ) {
          return()
        }
        # If new, show modal dialog to add organization details
        addOrgModal(name = "contributor")
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$add_owner,
      {
        # Check that mandatory fields are filled in
        if (!isTruthy(input$org_name)) {
          shinyjs::js$backgroundCol(ns("org_name"), "#fdd")
          return()
        }
        # Add the owner to the database
        df <- data.frame(
          name = input$org_name,
          name_fr = if (isTruthy(input$org_name_fr)) {
            input$org_name_fr
          } else {
            NA
          },
          contact_name = if (isTruthy(input$org_contact_name)) {
            input$org_contact_name
          } else {
            NA
          },
          phone = if (isTruthy(input$org_contact_phone)) {
            input$org_contact_phone
          } else {
            NA
          },
          email = if (isTruthy(input$org_contact_email)) {
            input$org_contact_email
          } else {
            NA
          },
          note = if (isTruthy(input$org_contact_note)) {
            input$org_contact_note
          } else {
            NA
          }
        )
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO public.organizations (name, name_fr, contact_name, phone, email, note) VALUES ($1, $2, $3, $4, $5, $6)",
          params = list(
            df$name,
            ifelse(is.na(df$name_fr), NA, df$name_fr),
            ifelse(is.na(df$contact_name), NA, df$contact_name),
            ifelse(is.na(df$phone), NA, df$phone),
            ifelse(is.na(df$email), NA, df$email),
            ifelse(is.na(df$note), NA, df$note)
          )
        )

        # Update the moduleData reactiveValues
        moduleData$organizations <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT organization_id, name FROM organizations"
        )
        # Update the selectizeInput to the new value
        updateSelectizeInput(
          session,
          "owner",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          selected = moduleData$organizations[
            moduleData$organizations$name == df$name,
            "organization_id"
          ]
        )
        removeModal()
        showModal(modalDialog(
          "New organization added.",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_contributor,
      {
        # Check that mandatory fields are filled in
        if (!isTruthy(input$org_name)) {
          shinyjs::js$backgroundCol(ns("org_name"), "#fdd")
          return()
        }
        # Add the owner to the database
        df <- data.frame(
          name = input$org_name,
          name_fr = if (isTruthy(input$org_name_fr)) {
            input$org_name_fr
          } else {
            NA
          },
          contact_name = if (isTruthy(input$org_contact_name)) {
            input$org_contact_name
          } else {
            NA
          },
          phone = if (isTruthy(input$org_contact_phone)) {
            input$org_contact_phone
          } else {
            NA
          },
          email = if (isTruthy(input$org_contact_email)) {
            input$org_contact_email
          } else {
            NA
          },
          note = if (isTruthy(input$org_contact_note)) {
            input$org_contact_note
          } else {
            NA
          }
        )
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO public.organizations (name, name_fr, contact_name, phone, email, note) VALUES ($1, $2, $3, $4, $5, $6)",
          params = list(
            df$name,
            ifelse(is.na(df$name_fr), NA, df$name_fr),
            ifelse(is.na(df$contact_name), NA, df$contact_name),
            ifelse(is.na(df$phone), NA, df$phone),
            ifelse(is.na(df$email), NA, df$email),
            ifelse(is.na(df$note), NA, df$note)
          )
        )

        # Update the moduleData reactiveValues
        moduleData$organizations <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT organization_id, name FROM organizations"
        )
        # Update the selectizeInput to the new value
        updateSelectizeInput(
          session,
          "contributor",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          selected = moduleData$organizations[
            moduleData$organizations$name == df$name,
            "organization_id"
          ]
        )
        removeModal()
        showModal(modalDialog(
          "New organization added.",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Reactive values to hold uploaded data and parsed data
    data <- reactiveValues(
      df = data.frame(datetime = character(), value = numeric()),
      parsed_datetime = NULL,
      parsed_value = NULL
    )

    upload_raw <- reactive({
      req(input$file)
      req(input$raw_start_row)
      # Set starting row to 1 if input is null, so we don't have to catch empty inputs in validate
      starting_row <- ifelse(
        length(input$raw_start_row) < 1,
        1,
        input$raw_start_row
      )

      ext <- tools::file_ext(input$file$name)
      if (ext == 'xlsx') {
        return(openxlsx::read.xlsx(
          input$file$datapath,
          sheet = 1,
          startRow = starting_row
        ))
      } else if (ext == "csv") {
        # .csv files more complex due to ungraceful handling of non-equal
        #  number of columns and column names by read.table

        # Read data without header, skip to first row below specified header row
        out <- read.csv(
          input$file$datapath,
          header = FALSE,
          skip = starting_row
        )

        # Read in header row and convert to vector
        out_names <- read.csv(
          input$file$datapath,
          header = FALSE,
          nrows = 1,
          skip = starting_row - 1
        ) |>
          unlist() |>
          unname()
        # Apply header rows to data
        names(out) <- out_names

        return(out)
      }
    })

    class_type_choices <- reactive({
      list(
        grade = DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT grade_type_id AS id, grade_type_code AS code FROM public.grade_types ORDER BY grade_type_id"
        ),
        approval = DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT approval_type_id AS id, approval_type_code AS code FROM public.approval_types ORDER BY approval_type_id"
        ),
        qualifier = DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT qualifier_type_id AS id, qualifier_type_code AS code FROM public.qualifier_types ORDER BY qualifier_type_id"
        )
      )
    })

    map_modal_state <- reactiveValues(
      step = "columns",
      pending_df = NULL,
      class_values = list(grade = character(), approval = character(), qualifier = character())
    )

    build_df_from_column_mapping <- function() {
      req(input$upload_datetime_col, input$upload_value_col)

      df_mapped <- data.frame(
        datetime = upload_raw()[[input$upload_datetime_col]],
        value = upload_raw()[[input$upload_value_col]]
      )

      if (
        isTruthy(input$upload_grade_col) &&
          input$upload_grade_col %in% names(upload_raw())
      ) {
        df_mapped$grade <- upload_raw()[[input$upload_grade_col]]
      }
      if (
        isTruthy(input$upload_approval_col) &&
          input$upload_approval_col %in% names(upload_raw())
      ) {
        df_mapped$approval <- upload_raw()[[input$upload_approval_col]]
      }
      if (
        isTruthy(input$upload_qualifier_col) &&
          input$upload_qualifier_col %in% names(upload_raw())
      ) {
        df_mapped$qualifier <- upload_raw()[[input$upload_qualifier_col]]
      }

      df_mapped
    }

    selected_class_cols <- reactive({
      cols <- c()
      if (isTruthy(input$upload_grade_col)) {
        cols <- c(cols, "grade")
      }
      if (isTruthy(input$upload_approval_col)) {
        cols <- c(cols, "approval")
      }
      if (isTruthy(input$upload_qualifier_col)) {
        cols <- c(cols, "qualifier")
      }
      cols
    })

    # Error checking, all possible conditions of start row and upload_raw
    #  in which the confirm mapping button should be disabled
    observe({
      if (map_modal_state$step != "columns") {
        return()
      }

      target_id <- if (length(selected_class_cols()) > 0) {
        "next_mapping"
      } else {
        "confirm_mapping"
      }

      if (is.null(input$raw_start_row)) {
        shinyjs::disable(target_id)
      } else if (is.na(input$raw_start_row)) {
        shinyjs::disable(target_id)
      } else if (input$raw_start_row < 1) {
        shinyjs::disable(target_id)
      } else if (ncol(upload_raw()) < 2) {
        shinyjs::disable(target_id)
      } else {
        shinyjs::enable(target_id)
      }
    })

    observe({
      if (map_modal_state$step != "class_mapping") {
        return()
      }

      all_mapped <- TRUE
      for (class_name in names(map_modal_state$class_values)) {
        values <- map_modal_state$class_values[[class_name]]
        if (length(values) == 0) {
          next
        }
        for (i in seq_along(values)) {
          if (!isTruthy(input[[paste0("map_", class_name, "_", i)]])) {
            all_mapped <- FALSE
            break
          }
        }
      }

      if (all_mapped) {
        shinyjs::enable('confirm_mapping')
      } else {
        shinyjs::disable('confirm_mapping')
      }
    })

    output$map_col_inputs <- renderUI({
      validate(
        need(
          input$raw_start_row > 0,
          'Invalid header row'
        ),
        need(
          ncol(upload_raw()) >= 2,
          'Uploaded file must have at least two columns (one containing date time, and one containing measurement value)'
        )
      )

      uploaded_names <- names(upload_raw())

      pick_col <- function(candidates, default = "") {
        out <- uploaded_names[tolower(uploaded_names) %in% tolower(candidates)]
        if (length(out) > 0) out[[1]] else default
      }

      choices_optional <- uploaded_names

      tagList(
        selectizeInput(
          ns('upload_datetime_col'),
          'Select the column for datetime:',
          choices = uploaded_names,
          selected = pick_col(
            c('datetime', 'date_time', 'date', 'time'),
            uploaded_names[[1]]
          )
        ),
        selectizeInput(
          ns('upload_value_col'),
          'Select the column for values:',
          choices = uploaded_names,
          selected = pick_col(
            c('value', 'values', 'measurement', 'measured_value'),
            uploaded_names[[2]]
          )
        ),
        if (length(uploaded_names) > 2) {
          div(
            selectizeInput(
              ns('upload_grade_col'),
              'Optional: select the column for grades:',
              choices = choices_optional,
              selected = pick_col(c('grade', 'grades'))
            ),
            selectizeInput(
              ns('upload_approval_col'),
              'Optional: select the column for approvals:',
              choices = choices_optional,
              selected = pick_col(c('approval', 'approvals'))
            ),
            selectizeInput(
              ns('upload_qualifier_col'),
              'Optional: select the column for qualifiers:',
              choices = choices_optional,
              selected = pick_col(c('qualifier', 'qualifiers'))
            )
          )
        }
      )
    })

    parse_datetime <- function(x) {
      if (inherits(x, "POSIXct")) {
        return(x)
      }
      if (inherits(x, "Date")) {
        return(as.POSIXct(x, tz = "UTC"))
      }
      x <- as.character(x)
      # Switch T character with space for ISO like formats
      x <- x <- gsub("T", " ", x)

      lubridate::parse_date_time(
        x,
        orders = c(
          "Ymd HMS",
          "Ymd HM",
          "mdY HMS",
          "mdY HM",
          "Ymd IMS p",
          "mdY IMS p",
          "Ymd IM p",
          "mdY IM p"
        ),
        exact = FALSE,
        train = TRUE,
        tz = "UTC"
      )
    }

    prepare_table_data <- function(df) {
      df <- df[, c("datetime", "value")]
      df$datetime <- as.character(df$datetime)
      df$value <- suppressWarnings(as.numeric(df$value))
      df
    }

    output$map_modal_body <- renderUI({
      if (map_modal_state$step == "columns") {
        tagList(
          'Identify which columns represent date-time and value (and optionally grade/approval/qualifier):',
          hr(),
          numericInput(ns('raw_start_row'), label = 'Header Row', value = 1) |>
            tooltip("The row number which contains your data's column names"),
          uiOutput(ns('map_col_inputs'))
        )
      } else {
        mapping_ui <- list(
          tags$p('Map your uploaded classes to database classes.')
        )

        db_types <- class_type_choices()

        for (class_name in names(map_modal_state$class_values)) {
          class_vals <- map_modal_state$class_values[[class_name]]
          if (length(class_vals) == 0) {
            next
          }

          db_df <- db_types[[class_name]]
          db_choices <- stats::setNames(
            as.character(db_df$id),
            paste0(db_df$code, " (", db_df$id, ")")
          )

          mapping_ui[[length(mapping_ui) + 1]] <- tags$h5(
            paste0(
              toupper(substring(class_name, 1, 1)),
              substring(class_name, 2),
              " mapping"
            )
          )

          for (i in seq_along(class_vals)) {
            mapping_ui[[length(mapping_ui) + 1]] <- selectizeInput(
              ns(paste0("map_", class_name, "_", i)),
              paste0("Uploaded '", class_vals[[i]], "' maps to:"),
              choices = db_choices,
              selected = ""
            )
          }
        }

        do.call(tagList, mapping_ui)
      }
    })

    output$map_modal_footer <- renderUI({
      if (map_modal_state$step == "columns") {
        button_label <- if (length(selected_class_cols()) > 0) {
          "Next"
        } else {
          "Confirm"
        }
        button_id <- if (length(selected_class_cols()) > 0) {
          "next_mapping"
        } else {
          "confirm_mapping"
        }

        tagList(
          modalButton('Cancel'),
          actionButton(ns(button_id), button_label)
        )
      } else {
        tagList(
          modalButton('Cancel'),
          actionButton(ns('confirm_mapping'), 'Confirm')
        )
      }
    })

    # Store modal to be shown upon user uploading .csv or .xlsx
    map_col_modal <- modalDialog(
      title = 'Identify columns',
      uiOutput(ns('map_modal_body')),
      easyClose = FALSE,
      footer = uiOutput(ns('map_modal_footer'))
    )

    # Show modal when user adds file
    observeEvent(input$file, {
      req(input$file)
      map_modal_state$step <- "columns"
      map_modal_state$pending_df <- NULL
      map_modal_state$class_values <- list(
        grade = character(),
        approval = character(),
        qualifier = character()
      )
      showModal(map_col_modal)
    })

    observeEvent(input$next_mapping, {
      df_mapped <- build_df_from_column_mapping()
      map_modal_state$pending_df <- df_mapped
      sanitize_class_vals <- function(x) {
        vals <- trimws(as.character(x))
        sort(unique(vals[!is.na(vals) & nzchar(vals)]))
      }
      map_modal_state$class_values <- list(
        grade = sanitize_class_vals(df_mapped$grade),
        approval = sanitize_class_vals(df_mapped$approval),
        qualifier = sanitize_class_vals(df_mapped$qualifier)
      )
      map_modal_state$step <- "class_mapping"
    })

    observeEvent(
      input$confirm_mapping,
      {
        removeModal() # Close the modal dialog
        df_mapped <- if (map_modal_state$step == "class_mapping") {
          req(map_modal_state$pending_df)
          out <- map_modal_state$pending_df

          for (class_name in names(map_modal_state$class_values)) {
            class_vals <- map_modal_state$class_values[[class_name]]
            if (length(class_vals) == 0 || !(class_name %in% names(out))) {
              next
            }

            mapped_ids <- vapply(
              seq_along(class_vals),
              function(i) {
                as.integer(input[[paste0("map_", class_name, "_", i)]])
              },
              integer(1)
            )
            names(mapped_ids) <- class_vals

            current_vals <- trimws(as.character(out[[class_name]]))
            non_missing <- !is.na(current_vals) & nzchar(current_vals)
            out[[class_name]][non_missing] <- unname(
              mapped_ids[current_vals[non_missing]]
            )
          }
          out
        } else {
          build_df_from_column_mapping()
        }

        data$df <- prepare_table_data(df_mapped)
        if ("grade" %in% names(df_mapped)) {
          data$df$grade <- df_mapped$grade
        }
        if ("approval" %in% names(df_mapped)) {
          data$df$approval <- df_mapped$approval
        }
        if ("qualifier" %in% names(df_mapped)) {
          data$df$qualifier <- df_mapped$qualifier
        }
      },
      ignoreInit = TRUE
    )

    new_data_row <- function() {
      data.frame(
        datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        value = NA_real_
      )
    }

    add_row_relative_to_selection <- function(position = c("above", "below")) {
      position <- match.arg(position)
      if (nrow(data$df) == 0) {
        data$df <- new_data_row()
        showNotification("Table was empty; added a new row.", type = "message")
        return(invisible(NULL))
      }

      selected <- input$data_table_rows_selected
      if (length(selected) < 1) {
        showNotification(
          "Select one row first to add above or below.",
          type = "error"
        )
        return(invisible(NULL))
      }
      if (length(selected) > 1) {
        if (position == "above") {
          showNotification(
            "Multiple rows selected; using the first selected row.",
            type = "warning"
          )
          anchor <- selected[[1]]
          insert_idx <- anchor
        } else {
          showNotification(
            "Multiple rows selected; using the last selected row.",
            type = "warning"
          )
          anchor <- selected[[length(selected)]]
          insert_idx <- anchor + 1
        }
      } else {
        anchor <- selected[[1]]
        insert_idx <- if (position == "above") {
          anchor
        } else {
          anchor + 1
        }
      }

      top <- if (insert_idx > 1) {
        data$df[seq_len(insert_idx - 1), , drop = FALSE]
      } else {
        data$df[0, , drop = FALSE]
      }
      bottom <- if (insert_idx <= nrow(data$df)) {
        data$df[insert_idx:nrow(data$df), , drop = FALSE]
      } else {
        data$df[0, , drop = FALSE]
      }
      data$df <- rbind(top, new_data_row(), bottom)
    }

    observeEvent(input$add_row, {
      data$df <- rbind(data$df, new_data_row())
    })

    observeEvent(input$add_row_above, {
      add_row_relative_to_selection("above")
    })

    observeEvent(input$add_row_below, {
      add_row_relative_to_selection("below")
    })

    observeEvent(input$delete_rows, {
      req(input$data_table_rows_selected)
      data$df <- data$df[-input$data_table_rows_selected, , drop = FALSE]
    })

    apply_datetime_cutoff <- function(mode = c("before", "after")) {
      mode <- match.arg(mode)
      if (nrow(data$df) == 0) {
        showNotification("No rows to delete.", type = "message")
        return(invisible(NULL))
      }

      cutoff <- parse_datetime(input$delete_cutoff_datetime)
      if (is.na(cutoff)) {
        showNotification(
          "Invalid cutoff datetime. Use format YYYY-MM-DD HH:MM:SS.",
          type = "error"
        )
        return(invisible(NULL))
      }

      parsed_dt <- parse_datetime(data$df$datetime)
      if (any(is.na(parsed_dt))) {
        showNotification(
          "Cannot apply cutoff: one or more datetime values in the table are invalid.",
          type = "error"
        )
        return(invisible(NULL))
      }

      keep_idx <- if (mode == "before") {
        parsed_dt >= cutoff
      } else {
        parsed_dt <= cutoff
      }
      removed_n <- sum(!keep_idx)
      data$df <- data$df[keep_idx, , drop = FALSE]
      showNotification(
        sprintf("Removed %s row(s).", removed_n),
        type = "message"
      )
    }

    observeEvent(input$delete_before_datetime, {
      apply_datetime_cutoff("before")
    })

    observeEvent(input$delete_after_datetime, {
      apply_datetime_cutoff("after")
    })

    data_table_proxy <- DT::dataTableProxy(ns("data_table"))

    output$data_table <- DT::renderDT(
      {
        DT::datatable(
          data$df,
          editable = TRUE,
          selection = list(
            mode = "multiple",
            target = "row",
            selector = "td:first-child"
          ),
          options = list(scrollX = TRUE),
          callback = htmlwidgets::JS(
            "table.on('click.dt', 'tbody td', function() {",
            "  if ($(this).index() === 0) { return; }",
            "  table.cell(this).edit();",
            "});"
          ),
          rownames = FALSE
        )
      },
      server = FALSE
    )

    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      data$df <- DT::editData(
        data$df,
        info,
        proxy = data_table_proxy,
        rownames = FALSE
      )
    })

    preview_data <- reactive({
      req(timeseries())
      req(nrow(data$df) > 0)

      parsed_dt <- parse_datetime(data$df$datetime)
      parsed_val <- suppressWarnings(as.numeric(data$df$value))
      valid_idx <- !(is.na(parsed_dt) | is.na(parsed_val))
      req(any(valid_idx))

      df_new <- data$df[valid_idx, , drop = FALSE]
      df_new$datetime <- parsed_dt[valid_idx] -
        (as.numeric(input$UTC_offset) * 3600)
      df_new$value <- parsed_val[valid_idx]
      df_new$source <- "New upload"

      if ("grade" %in% names(df_new)) {
        df_new$grade <- as.character(df_new$grade)
      }
      if ("approval" %in% names(df_new)) {
        df_new$approval <- as.character(df_new$approval)
      }
      if ("qualifier" %in% names(df_new)) {
        df_new$qualifier <- as.character(df_new$qualifier)
      }

      range_start <- min(df_new$datetime, na.rm = TRUE)
      range_end <- max(df_new$datetime, na.rm = TRUE)

      custom_start <- parse_datetime(input$preview_start_datetime)
      if (!is.na(custom_start)) {
        custom_start <- custom_start - (as.numeric(input$UTC_offset) * 3600)
        range_start <- min(range_start, custom_start)
      }

      extra <- dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT datetime, value_corrected AS value FROM continuous.measurements_continuous_corrected WHERE timeseries_id = $1 AND datetime >= $2 AND datetime <= $3",
        params = list(timeseries(), range_start, range_end)
      )

      if (nrow(extra) == 0) {
        extra <- NULL
      }

      hist_out <- NULL
      if (isTRUE(input$preview_historic_range)) {
        # add a day to the end for the historic query to ensure we have enough ribbon
        hist_out <- dbGetQueryDT(
          session$userData$AquaCache,
          "SELECT date AS datetime, min, max, q25, q75 FROM continuous.measurements_calculated_daily WHERE timeseries_id = $1 AND date >= $2 AND date <= $3",
          params = list(timeseries(), range_start, range_end + 86400)
        )
        tz <- dbGetQueryDT(
          session$userData$AquaCache,
          "SELECT timezone_daily_calc FROM timeseries WHERE timeseries_id = $1",
          params = list(timeseries())
        )$timezone_daily_calc[[1]]
        hist_out$datetime <- as.POSIXct(hist_out$datetime, tz = "UTC") -
          tz * 3600
      }

      parameter <- dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT param_name, unit_default, plot_default_y_orientation FROM public.parameters JOIN continuous.timeseries ON parameters.parameter_id = timeseries.parameter_id WHERE timeseries.timeseries_id = $1",
        params = list(timeseries())
      )

      list(
        new_data = df_new,
        db = extra,
        historic = hist_out,
        parameter = parameter
      )
    })

    plot_data <- reactiveVal(NULL)
    observeEvent(input$refresh_plot, {
      req(timeseries())
      pv <- preview_data()

      # Start with the range ribbons. Like in plotTimeseries, create ranges within the historic range data so that discontinuous range data doesn't connect across gaps.
      historic_range <- FALSE
      if (!is.null(pv$historic)) {
        pv$historic[,
          has_stats := !is.na(q25) & !is.na(q75) & !is.na(min) & !is.na(max)
        ]

        # Create a run id that increments each time has_stats changes
        pv$historic[, run := data.table::rleid(has_stats)]

        # Keep only runs with data
        range_runs <- split(
          pv$historic[has_stats == TRUE],
          by = "run",
          keep.by = FALSE
        )
        if (length(range_runs) > 0) {
          historic_range <- TRUE
        }
      }

      plot <- plotly::plot_ly()

      if (historic_range) {
        for (rd in range_runs) {
          plot <- plot |>
            plotly::add_ribbons(
              data = rd,
              x = ~datetime,
              ymin = ~q25,
              ymax = ~q75,
              name = "IQR",
              color = I("#5f9da6"),
              line = list(width = 0.2),
              hoverinfo = "text",
              text = ~ paste0(
                "Q25: ",
                round(q25, 2),
                ", Q75: ",
                round(q75, 2),
                " (",
                as.Date(datetime),
                ")"
              ),
              showlegend = FALSE
            ) |>
            plotly::add_ribbons(
              data = rd,
              x = ~datetime,
              ymin = ~min,
              ymax = ~max,
              name = "Historic",
              color = I("#D4ECEF"),
              line = list(width = 0.2),
              hoverinfo = "text",
              text = ~ paste0(
                "Min: ",
                round(min, 2),
                ", Max: ",
                round(max, 2),
                " (",
                as.Date(datetime),
                ")"
              ),
              showlegend = FALSE
            )
        }

        # Add *visible* dummy legend keys (one point is enough)
        key_rd <- range_runs[[1]][1]

        plot <- plot |>
          plotly::add_ribbons(
            data = key_rd,
            x = ~datetime,
            ymin = ~q25,
            ymax = ~q75,
            name = "IQR",
            color = I("#5f9da6"),
            line = list(width = 0.2),
            hoverinfo = "none",
            showlegend = TRUE
          ) |>
          plotly::add_ribbons(
            data = key_rd,
            x = ~datetime,
            ymin = ~min,
            ymax = ~max,
            name = "Historic",
            color = I("#D4ECEF"),
            line = list(width = 0.2),
            hoverinfo = "none",
            showlegend = TRUE
          )
      }

      # Now add in the existing data
      if (!is.null(pv$db)) {
        plot <- plot |>
          plotly::add_trace(
            data = pv$db,
            x = ~datetime,
            y = ~value,
            type = "scatter",
            mode = "lines",
            line = list(width = 2.5),
            name = "Existing corrected",
            color = I("#fa9906ff"),
            hoverinfo = "text",
            text = ~ paste0(
              pv$parameter$param_name,
              ": ",
              round(.data$value, 4),
              " (",
              .data$datetime,
              ")"
            )
          )
      }

      # Finally, add the new data
      plot <- plot |>
        plotly::add_trace(
          data = pv$new_data,
          x = ~datetime,
          y = ~value,
          type = "scatter",
          mode = "lines",
          line = list(width = 2.5),
          name = "New upload",
          color = I("#00454e"),
          hoverinfo = "text",
          text = ~ paste0(
            pv$parameter$param_name,
            ": ",
            round(.data$value, 4),
            " (",
            .data$datetime,
            ")"
          )
        )

      plot <- plot |>
        plotly::layout(
          title = NULL,
          xaxis = list(
            title = list(standoff = 0),
            showgrid = FALSE,
            showline = TRUE,
            tickformat = "%b %-d '%y",
            titlefont = list(size = 14),
            tickfont = list(size = 12),
            nticks = 10,
            rangeslider = list(
              visible = TRUE
            ),
            ticks = "outside",
            ticklen = 5,
            tickwidth = 1,
            tickcolor = "black"
          ),
          # Main plot yaxis layout
          yaxis = list(
            title = list(
              text = paste0(
                pv$parameter$param_name,
                " (",
                pv$parameter$unit,
                ")"
              ),
              standoff = 10
            ),
            showgrid = FALSE,
            showline = TRUE,
            zeroline = FALSE,
            titlefont = list(size = 14),
            tickfont = list(size = 12),
            autorange = if (
              pv$parameter$plot_default_y_orientation == "inverted"
            ) {
              "reversed"
            } else {
              TRUE
            },
            ticks = "outside",
            ticklen = 5,
            tickwidth = 1,
            tickcolor = "black"
          ),
          margin = list(b = 0, t = 40, l = 50),
          hovermode = "x unified",
          legend = list(
            font = list(size = 12),
            orientation = "v"
          ),
          font = list(family = "Nunito Sans")
        ) |>
        plotly::config(locale = "en")

      return(plot)
    })

    output$data_preview <- plotly::renderPlotly({
      plot_data()
    })

    # function to check data validity before upload
    check_fx <- function() {
      if (is.null(timeseries())) {
        showNotification('Please select a timeseries first.', type = 'error')
        return(FALSE)
      }
      if (nrow(data$df) == 0) {
        showNotification('Empty data table!', type = 'error')
        return(FALSE)
      }

      if (is.null(input$owner) || is.null(input$contributor)) {
        showNotification(
          'Please select owner and contributor organizations.',
          type = 'error'
        )
        return(FALSE)
      }

      # Check for duplicated rows and drop them; warn the user
      duplicated_rows <- duplicated(data$df)
      if (any(duplicated_rows)) {
        data$df <- data$df[!duplicated_rows, ]
        showNotification(
          paste0(
            'There were ',
            sum(duplicated_rows),
            ' duplicated (completely identical) rows. Only the first occurence of each unique row was retained'
          ),
          type = 'message',
          duration = 8
        )
      }
      duplicated_datetimes <- data$df$datetime[duplicated(data$df$datetime)]
      if (length(duplicated_datetimes) > 0) {
        showNotification(
          paste0(
            'There is more than one datetime for ',
            paste(unique(duplicated_datetimes), collapse = ", ")
          ),
          type = 'error',
          duration = 10
        )
        return(FALSE)
      }

      parsed_value <- suppressWarnings(as.numeric(data$df$value))
      if (any(is.na(parsed_value))) {
        showNotification(
          'Value column must be numeric with no missing values.',
          type = 'error',
          duration = 8
        )
        return(FALSE)
      }
      # Make sure datetime is in POSIXct format or can be converted to it
      parsed_datetime <- parse_datetime(data$df$datetime)
      if (any(is.na(parsed_datetime))) {
        showNotification(
          'Datetime column is not in the correct format. Please check your data: it should be of form YYYY-MM-DD HH:MM.',
          type = 'error',
          duration = 10
        )
        return(FALSE)
      }
      data$parsed_datetime <- parsed_datetime
      data$parsed_value <- parsed_value
      return(TRUE)
    }

    observeEvent(input$upload, {
      check <- check_fx()
      if (!check) {
        return()
      }
      tryCatch(
        {
          upload_data <- data$df
          upload_data$datetime <- data$parsed_datetime -
            (as.numeric(input$UTC_offset) * 3600) # Adjust datetime to UTC 0
          upload_data$value <- data$parsed_value
          upload_data$owner <- as.integer(input$owner)
          upload_data$contributor <- as.integer(input$contributor)
          upload_data$no_update <- data.table::fifelse(
            input$no_update == "yes",
            TRUE,
            FALSE
          )
          AquaCache::addNewContinuous(
            tsid = timeseries(),
            df = upload_data,
            con = session$userData$AquaCache,
            target = "realtime",
            overwrite = "no"
          )
          showNotification('Data added.', type = 'message')
          data$df <- data.frame(
            datetime = character(),
            value = numeric()
          )
        },
        error = function(e) {
          showNotification(paste('Upload failed:', e$message), type = 'error')
        },
        warning = function(w) {
          showNotification(
            paste('Warning on upload:', w$message),
            type = 'warning'
          )
        },
        message = function(m) {
          showNotification(
            paste('Message on upload:', m$message),
            type = 'message'
          )
        }
      )
    })
    observeEvent(input$upload_overwrite_all, {
      check <- check_fx()
      if (!check) {
        return()
      }
      # Show a modal dialog to confirm overwritting all points in time range of new data
      showModal(modalDialog(
        title = 'Confirm Overwrite',
        'This will overwrite all points in the time range of the new data, including any points that do not conflict with the new data (if any). Are you sure?',
        easyClose = TRUE,
        footer = tagList(
          modalButton('Cancel'),
          actionButton(
            ns('confirm_overwrite_all'),
            'Confirm Overwrite',
            class = 'btn-danger'
          )
        )
      ))
    })
    observeEvent(input$confirm_overwrite_all, {
      removeModal() # Close the modal dialog
      tryCatch(
        {
          upload_data <- data$df
          upload_data$datetime <- data$parsed_datetime -
            (as.numeric(input$UTC_offset) * 3600) # Adjust datetime to UTC
          upload_data$value <- data$parsed_value
          upload_data$owner <- as.integer(input$owner)
          upload_data$contributor <- as.integer(input$contributor)
          upload_data$no_update <- data.table::fifelse(
            input$no_update == "yes",
            TRUE,
            FALSE
          )
          AquaCache::addNewContinuous(
            tsid = timeseries(),
            df = upload_data,
            con = session$userData$AquaCache,
            target = "realtime",
            overwrite = "all"
          )
          showNotification('Data added with overwrite.', type = 'message')
          data$df <- data.frame(
            datetime = character(),
            value = numeric()
          )
        },
        error = function(e) {
          showNotification(paste('Upload failed:', e$message), type = 'error')
        },
        warning = function(w) {
          showNotification(
            paste('Warning on upload:', w$message),
            type = 'warning'
          )
        },
        message = function(m) {
          showNotification(
            paste('Message on upload:', m$message),
            type = 'message'
          )
        }
      )
    })

    observeEvent(input$upload_overwrite_some, {
      check <- check_fx()
      if (!check) {
        return()
      }
      # Show a modal dialog to confirm overwriting conflicting points only
      showModal(modalDialog(
        title = 'Confirm Selective Overwrite',
        'This will overwrite only the points in the new data that conflict with existing points in the time range of the new data. Are you sure?',
        easyClose = TRUE,
        footer = tagList(
          modalButton('Cancel'),
          actionButton(
            ns('confirm_overwrite_some'),
            'Confirm Selective Overwrite',
            class = 'btn-warning'
          )
        )
      ))
    })
    observeEvent(input$confirm_overwrite_some, {
      removeModal() # Close the modal dialog
      tryCatch(
        {
          upload_data <- data$df
          upload_data$datetime <- data$parsed_datetime -
            (as.numeric(input$UTC_offset) * 3600) # Adjust datetime to UTC
          upload_data$value <- data$parsed_value
          upload_data$owner <- as.integer(input$owner)
          upload_data$contributor <- as.integer(input$contributor)
          upload_data$no_update <- data.table::fifelse(
            input$no_update == "yes",
            TRUE,
            FALSE
          )
          AquaCache::addNewContinuous(
            tsid = timeseries(),
            df = upload_data,
            con = session$userData$AquaCache,
            target = "realtime",
            overwrite = "conflict"
          )
          showNotification(
            'Data added with selective overwrite.',
            type = 'message'
          )
          data$df <- data.frame(
            datetime = character(),
            value = numeric()
          )
        },
        error = function(e) {
          showNotification(paste('Upload failed:', e$message), type = 'error')
        },
        warning = function(w) {
          showNotification(
            paste('Warning on upload:', w$message),
            type = 'warning'
          )
        },
        message = function(m) {
          showNotification(
            paste('Message on upload:', m$message),
            type = 'message'
          )
        }
      )
    })

    return(outputs)
  }) # End of moduleServer
}
