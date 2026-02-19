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
        open = FALSE,
        div(
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
        ),
        # Add delete/grade/approval/qualifier functionality within accordions
        accordion_panel(
          id = ns("delete_panel"),
          title = "Delete data",
          icon = icon("trash"),
          div(
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
          )
        ), # end delete accordion panel
        # Add grade/approval/qualifier functionality within an accordion
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
        ),
        div(
          tags$br(),
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
          selectizeInput(
            ns("UTC_offset"),
            "UTC offset of data",
            choices = seq(-12, 12, by = 1),
            selected = 0,
            multiple = FALSE
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
          ),
          div(
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
        "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS record_rate_minutes FROM continuous.timeseries_metadata_en"
      )
    })

    # Reload module data when asked
    observeEvent(input$reload_module, {
      ts_meta(dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS record_rate_minutes FROM continuous.timeseries_metadata_en"
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

    # Error checking, all possible conditions of start row and upload_raw
    #  in which the confirm mapping button should be disabled
    observe({
      if (is.null(input$raw_start_row)) {
        shinyjs::disable('confirm_mapping')
      } else if (is.na(input$raw_start_row)) {
        shinyjs::disable('confirm_mapping')
      } else if (input$raw_start_row < 1) {
        shinyjs::disable('confirm_mapping')
      } else if (ncol(upload_raw()) < 2) {
        shinyjs::disable('confirm_mapping')
      } else {
        shinyjs::enable('confirm_mapping')
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
      tagList(
        selectizeInput(
          ns('upload_datetime_col'),
          'Select the column for datetime:',
          choices = names(upload_raw()),
          selected = names(upload_raw())[1]
        ),
        selectizeInput(
          ns('upload_value_col'),
          'Select the column for value:',
          choices = names(upload_raw()),
          selected = names(upload_raw())[2]
        )
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

    # Store modal to be shown upon user uploading .csv or .xlsx
    map_col_modal <- modalDialog(
      title = 'Identify columns',
      'Please identify which columns represent date-time and value:',
      hr(),
      numericInput(ns('raw_start_row'), label = 'Header Row', value = 1) |>
        tooltip("The row number which contains your data's column names"),
      # UI which holds selectizeInputs for datetime and value column
      uiOutput(ns('map_col_inputs')),

      easyClose = FALSE,
      footer = tagList(
        modalButton('Cancel'),
        actionButton(ns('confirm_mapping'), 'Confirm')
      )
    )

    # Show modal when user adds file
    observeEvent(input$file, {
      req(input$file)
      showModal(map_col_modal)
    })

    observeEvent(
      input$confirm_mapping,
      {
        removeModal() # Close the modal dialog
        req(input$upload_datetime_col, input$upload_value_col)
        df_mapped <- data.frame(
          datetime = upload_raw()[[input$upload_datetime_col]],
          value = upload_raw()[[input$upload_value_col]]
        )
        data$df <- prepare_table_data(df_mapped)
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
