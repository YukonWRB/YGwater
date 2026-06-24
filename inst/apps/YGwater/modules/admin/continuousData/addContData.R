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
          p(
            "For a file containing more than one timeseries, select all target basic timeseries from the same location."
          ),
          div(
            style = "margin-bottom: 8px;",
            actionButton(
              ns("reset_timeseries_selection"),
              "Reset timeseries selection",
              icon = icon("rotate-left"),
              class = "btn-secondary"
            )
          ),
          uiOutput(ns("selected_upload_targets")),
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
          uiOutput(ns("selected_units_warning")),
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
              "Upload .csv, .xlsx, .xle, logger .html, or .hobo",
              accept = c(".csv", ".xlsx", ".xle", ".html", ".htm", ".hobo")
            )
          ),
          conditionalPanel(
            condition = "input.entry_mode == 'manual'",
            ns = ns,
            div(
              actionButton(ns("add_row"), "Add row to end"),
              actionButton(ns("add_row_above"), "Add row above selection"),
              actionButton(ns("add_row_below"), "Add row below selection"),
              actionButton(ns("delete_rows_table"), "Delete selected rows")
            ),
            tags$br()
          ),

          uiOutput(ns("data_tables_ui")),
          uiOutput(ns("data_table_note")),
          tags$br(),
          splitLayout(
            cellWidths = c("70%", "30%"),
            selectizeInput(
              ns("UTC_offset"),
              "UTC offset of data",
              choices = input_timezone_choices(),
              selected = format_utc_offset(0L),
              multiple = FALSE
            ),
            div(
              style = "padding-top: 25px;",
              actionButton(
                ns("open_unit_conversion"),
                "Convert units",
                icon = icon("calculator"),
                class = "btn-warning"
              )
            )
          ),
          splitLayout(
            cellWidths = c("50%", "50%"),
            selectizeInput(
              ns("owner"),
              "Owner organization (applies to these data only)",
              choices = NULL,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select owner"),
              width = "100%"
            ),
            selectizeInput(
              ns("contributor"),
              "Contributor organization (applies to these data only)",
              choices = NULL,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select contributor"),
              width = "100%"
            )
          ),
          radioButtons(
            ns("no_update"),
            "Prevent updates to these data by automatic processes, such as import scripts?",
            choices = c("Yes" = "yes", "No" = "no"),
            inline = TRUE,
            selected = "no"
          ),
          tags$div(
            "Note: data visibility is controlled by the timeseries visibility parameters."
          ),
          uiOutput(ns("multi_upload_note"))
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
          splitLayout(
            cellWidths = c("33%", "33%", "33%"),
            selectizeInput(
              ns("preview_utc_offset"),
              "Preview UTC offset",
              choices = input_timezone_choices(),
              selected = format_utc_offset(0L),
              multiple = FALSE,
              width = "100%"
            ),
            shinyWidgets::airDatepickerInput(
              ns("preview_start_datetime"),
              "Preview start datetime",
              value = NULL,
              range = FALSE,
              multiple = FALSE,
              timepicker = TRUE,
              update_on = "change",
              tz = air_datetime_widget_timezone(format_utc_offset(0L)),
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 15,
                timeFormat = "HH:mm"
              )
            ),
            shinyWidgets::airDatepickerInput(
              ns("preview_end_datetime"),
              "Preview end datetime",
              value = NULL,
              range = FALSE,
              multiple = FALSE,
              timepicker = TRUE,
              update_on = "change",
              tz = air_datetime_widget_timezone(format_utc_offset(0L)),
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 15,
                timeFormat = "HH:mm"
              )
            )
          ),
          bslib::input_task_button(
            ns("make_plot"),
            "Refresh plot",
            icon = icon("refresh")
          ),
          uiOutput(ns("plot_refresh_warning")),
          uiOutput(ns("preview_plot_tabs"))
        ),

        # Add delete/grade/approval/qualifier functionality within accordions
        # Delete regions panel
        accordion_panel(
          id = ns("delete_panel"),
          title = "Delete data",
          icon = icon("trash"),
          selectizeInput(
            ns("delete_utc_offset"),
            "Delete UTC offset",
            choices = input_timezone_choices(),
            selected = format_utc_offset(0L),
            multiple = FALSE,
            width = "100%"
          ),
          shinyWidgets::airDatepickerInput(
            ns("delete_cutoff_datetime"),
            "Delete data before/after datetime",
            value = NULL,
            range = FALSE,
            multiple = FALSE,
            timepicker = TRUE,
            update_on = "change",
            tz = air_datetime_widget_timezone(format_utc_offset(0L)),
            timepickerOpts = shinyWidgets::timepickerOptions(
              minutesStep = 15,
              timeFormat = "HH:mm"
            )
          ),
          uiOutput(ns("delete_target_ui")),
          div(
            conditionalPanel(
              condition = "input.entry_mode == 'manual'",
              ns = ns,
              actionButton(ns("delete_rows_accordion"), "Delete selected rows")
            ),
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

        # Add approvals panel
        accordion_panel(
          id = ns("approval_panel"),
          title = "Add/modify approval status",
          icon = icon("thumbs-up"),
          selectizeInput(
            ns("approval_utc_offset"),
            "Approval UTC offset",
            choices = input_timezone_choices(),
            selected = format_utc_offset(0L),
            multiple = FALSE,
            width = "100%"
          ),
          uiOutput(ns("approval_apply_all_ui")),
          div(
            actionButton(ns("add_approval_range"), "Add approval range"),
            actionButton(ns("edit_approval_range"), "Edit selected"),
            actionButton(ns("delete_approval_range"), "Delete selected")
          ),
          uiOutput(ns("approval_ranges_ui")),
          uiOutput(ns("approval_ranges_warning"))
        ), # End approval panel

        # Add grade panel
        accordion_panel(
          id = ns("grade_panel"),
          title = "Add/modify grades",
          icon = icon("check"),
          selectizeInput(
            ns("grade_utc_offset"),
            "Grade UTC offset",
            choices = input_timezone_choices(),
            selected = format_utc_offset(0L),
            multiple = FALSE,
            width = "100%"
          ),
          uiOutput(ns("grade_apply_all_ui")),
          div(
            actionButton(ns("add_grade_range"), "Add grade range"),
            actionButton(ns("edit_grade_range"), "Edit selected"),
            actionButton(ns("delete_grade_range"), "Delete selected")
          ),
          uiOutput(ns("grade_ranges_ui")),
          uiOutput(ns("grade_ranges_warning"))
        ), # end grade accordion panel

        # Add qualifiers panel
        accordion_panel(
          id = ns("qualifier_panel"),
          title = "Add/modify qualifiers",
          icon = icon("flag"),
          selectizeInput(
            ns("qualifier_utc_offset"),
            "Qualifier UTC offset",
            choices = input_timezone_choices(),
            selected = format_utc_offset(0L),
            multiple = FALSE,
            width = "100%"
          ),
          uiOutput(ns("qualifier_apply_all_ui")),
          div(
            actionButton(ns("add_qualifier_range"), "Add qualifier range"),
            actionButton(ns("edit_qualifier_range"), "Edit selected"),
            actionButton(ns("delete_qualifier_range"), "Delete selected")
          ),
          uiOutput(ns("qualifier_ranges_ui")),
          uiOutput(ns("qualifier_ranges_warning"))
        ) # End qualifiers accordion panel
      ), # end accordion for data manipulation options

      br(),
      uiOutput(ns("selected_units_warning_last")),
      uiOutput(ns("upload_target_checkboxes")),

      bslib::input_task_button(
        ns("upload"),
        "Upload to AquaCache (no overwrite)",
        type = "primary",
        style = "font-size: 14px;",
        label_busy = "Uploading..."
      ),
      bslib::input_task_button(
        ns("upload_overwrite_all"),
        "Upload to AquaCache (replace all points in new data range)",
        type = "primary",
        style = "font-size: 14px;",
        label_busy = "Uploading..."
      ),
      bslib::input_task_button(
        ns("upload_overwrite_some"),
        "Upload to AquaCache (overwrite conflicting points only)",
        type = "primary",
        style = "font-size: 14px;",
        label_busy = "Uploading..."
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
      ),
      unit_conversions = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT
           uc.conversion_id,
           u_from.unit_name AS from_unit,
           u_to.unit_name AS to_unit,
           uc.conversion_type,
           uc.scale_a,
           uc.scale_b
         FROM public.unit_conversions uc
         JOIN public.units u_from
           ON u_from.unit_id = uc.from_unit_id
         JOIN public.units u_to
           ON u_to.unit_id = uc.to_unit_id
        ORDER BY u_from.unit_name, u_to.unit_name"
      )
    )
    nonbasic_member_options <- reactiveVal(NULL)

    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'continuous.measurements_continuous', 'INSERT') AS can_insert"
    )
    can_insert <- isTRUE(check$can_insert[[1]])
    if (!can_insert) {
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

    load_timeseries_metadata <- function() {
      dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT
           md.timeseries_id,
           md.location_name AS location,
           md.parameter_name AS parameter,
           md.units,
           md.media_type AS media,
           md.aggregation_type AS aggregation,
           md.recording_rate AS record_rate_minutes,
           md.timeseries_type_code,
           md.timeseries_type,
           ts.active,
           ts.publicly_visible,
           ts.default_owner,
           md.sensor_priority,
           md.start_datetime,
           md.end_datetime
         FROM continuous.timeseries_metadata_en md
         INNER JOIN continuous.timeseries ts
           ON md.timeseries_id = ts.timeseries_id
         ORDER BY
           md.location_name,
           md.parameter_name,
           md.media_type,
           md.aggregation_type,
           md.recording_rate,
           md.timeseries_id"
      )
    }

    ts_meta <- reactiveVal(load_timeseries_metadata())

    load_nonbasic_member_options <- function(compound_timeseries_id) {
      DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        WITH RECURSIVE dependency_tree AS (
          SELECT
            m.timeseries_id AS requested_timeseries_id,
            m.member_alias::text AS member_path,
            m.member_alias,
            m.member_timeseries_id,
            m.member_priority,
            m.use_from,
            m.use_to,
            1 AS depth,
            ARRAY[m.timeseries_id, m.member_timeseries_id] AS path_ids
          FROM continuous.timeseries_compound_members AS m
          WHERE m.timeseries_id = $1

          UNION ALL

          SELECT
            d.requested_timeseries_id,
            d.member_path || ' -> ' || m.member_alias,
            m.member_alias,
            m.member_timeseries_id,
            m.member_priority,
            m.use_from,
            m.use_to,
            d.depth + 1,
            d.path_ids || m.member_timeseries_id
          FROM dependency_tree AS d
          INNER JOIN continuous.timeseries AS parent
            ON parent.timeseries_id = d.member_timeseries_id
          INNER JOIN continuous.timeseries_compound_members AS m
            ON m.timeseries_id = d.member_timeseries_id
          WHERE parent.timeseries_type = 'compound'
            AND NOT m.member_timeseries_id = ANY(d.path_ids)
        )
        SELECT
          d.depth,
          d.member_path,
          d.member_alias,
          d.member_priority,
          d.use_from,
          d.use_to,
          md.timeseries_id,
          md.location_name AS location,
          md.parameter_name AS parameter,
          md.units,
          md.media_type AS media,
          md.aggregation_type AS aggregation,
          md.recording_rate AS record_rate_minutes,
          md.timeseries_type_code,
          md.timeseries_type,
          ts.active,
          ts.publicly_visible,
          md.sensor_priority,
          md.start_datetime,
          md.end_datetime,
          (md.timeseries_type_code = 'basic') AS can_accept_data
        FROM dependency_tree AS d
        INNER JOIN continuous.timeseries_metadata_en AS md
          ON md.timeseries_id = d.member_timeseries_id
        INNER JOIN continuous.timeseries AS ts
          ON ts.timeseries_id = d.member_timeseries_id
        ORDER BY
          d.depth,
          d.member_path,
          d.member_priority,
          md.location_name,
          md.parameter_name,
          md.timeseries_id
        ",
        params = list(as.integer(compound_timeseries_id))
      )
    }

    # Reload module data when asked
    observeEvent(input$reload_module, {
      ts_meta(load_timeseries_metadata())
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM public.organizations ORDER BY name ASC"
      )
      moduleData$unit_conversions <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT
           uc.conversion_id,
           u_from.unit_name AS from_unit,
           u_to.unit_name AS to_unit,
           uc.conversion_type,
           uc.scale_a,
           uc.scale_b
         FROM public.unit_conversions uc
         JOIN public.units u_from
           ON u_from.unit_id = uc.from_unit_id
         JOIN public.units u_to
           ON u_to.unit_id = uc.to_unit_id
         ORDER BY u_from.unit_name, u_to.unit_name"
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
      df$timeseries_type <- as.factor(df$timeseries_type)
      df$active <- as.factor(df$active)
      df$publicly_visible <- as.factor(df$publicly_visible)
      df$sensor_priority <- as.factor(df$sensor_priority)
      df$default_owner <- as.factor(df$default_owner)

      hidden_columns <- which(
        names(df) %in% c("timeseries_id", "timeseries_type_code")
      ) -
        1L
      DT::datatable(
        df,
        selection = list(mode = "multiple", target = "row"),
        options = list(
          columnDefs = list(list(targets = hidden_columns, visible = FALSE)),
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
    upload_timeseries_ids <- reactiveVal(integer())
    suppress_ts_table_selection <- reactiveVal(FALSE)
    sync_ts_table_selection <- function(ids = integer()) {
      meta <- ts_meta()
      rows <- match(as.integer(ids), meta$timeseries_id)
      rows <- rows[!is.na(rows)]
      current_rows <- isolate(input$ts_table_rows_selected)
      if (setequal(rows, as.integer(current_rows))) {
        return(invisible(FALSE))
      }
      suppress_ts_table_selection(TRUE)
      DT::dataTableProxy("ts_table", session = session) |>
        DT::selectRows(rows)
      invisible(TRUE)
    }

    accepted_timeseries_ids_from_selection <- function(sel) {
      if (length(sel) == 0) {
        return(integer())
      }
      selected <- ts_meta()[sel, , drop = FALSE]
      basic <- selected[
        selected$timeseries_type_code == "basic",
        ,
        drop = FALSE
      ]
      if (nrow(basic) == 0) {
        return(integer())
      }
      location <- basic$location[[1]]
      basic <- basic[basic$location == location, , drop = FALSE]
      as.integer(basic$timeseries_id)
    }

    staged_upload_data_exists <- function() {
      jobs <- isolate(upload_jobs())
      validation_jobs <- isolate(upload_validation$jobs)
      nrow(isolate(data$df)) > 0 ||
        (!is.null(jobs) && length(jobs) > 0) ||
        (!is.null(validation_jobs) && length(validation_jobs) > 0)
    }

    selection_change_requires_data_reset <- function(sel) {
      if (!staged_upload_data_exists()) {
        return(FALSE)
      }
      current_ids <- as.integer(upload_timeseries_ids())
      proposed_ids <- accepted_timeseries_ids_from_selection(sel)
      !setequal(current_ids, proposed_ids)
    }

    pending_ts_table_selection <- reactiveVal(NULL)

    show_selection_reset_modal <- function() {
      showModal(modalDialog(
        title = "Reset uploaded data?",
        tags$p(
          "Changing the selected timeseries after data have been loaded will reset the uploaded data, preview plots, grades, approvals, and qualifiers."
        ),
        tags$p(
          "This prevents data mapped for one set of targets from being uploaded to a different set of targets."
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            ns("cancel_timeseries_selection_reset"),
            "Keep current selection"
          ),
          actionButton(
            ns("confirm_timeseries_selection_reset"),
            "Reset data and change selection",
            class = "btn-danger"
          )
        )
      ))
    }

    apply_ts_table_selection <- function(sel) {
      upload_jobs(NULL)
      upload_validation$jobs <- NULL
      if (length(sel) > 0) {
        selected <- ts_meta()[sel, , drop = FALSE]
        if (
          length(sel) == 1 &&
            !identical(selected$timeseries_type_code[[1]], "basic")
        ) {
          timeseries(NULL)
          upload_timeseries_ids(integer())
          sync_ts_table_selection()
          nonbasic_member_options(
            load_nonbasic_member_options(selected$timeseries_id[[1]])
          )
          showModal(modalDialog(
            title = paste(
              "Cannot add data directly to",
              selected$timeseries_type[[1]],
              "timeseries"
            ),
            tags$p(
              paste(
                "Select a basic member timeseries below, then click",
                "'Use this timeseries' to add measurements to the source",
                "series instead."
              )
            ),
            DT::DTOutput(ns("nonbasic_members_table")),
            easyClose = TRUE,
            size = "xl",
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                ns("use_member_timeseries"),
                "Use this timeseries",
                class = "btn-primary"
              )
            )
          ))
        } else {
          basic <- selected[
            selected$timeseries_type_code == "basic",
            ,
            drop = FALSE
          ]
          ignored_nonbasic <- nrow(basic) < nrow(selected)
          if (nrow(basic) == 0) {
            timeseries(NULL)
            upload_timeseries_ids(integer())
            sync_ts_table_selection()
            showNotification(
              "Select at least one Basic timeseries for direct data upload.",
              type = "error",
              duration = 8
            )
            return()
          }

          location <- basic$location[[1]]
          same_location <- basic$location == location
          if (!all(same_location)) {
            showNotification(
              "Multi-timeseries uploads must target Basic timeseries from the same location. Keeping only selections from the first selected location.",
              type = "warning",
              duration = 10
            )
            basic <- basic[same_location, , drop = FALSE]
          }
          if (ignored_nonbasic) {
            showNotification(
              "Only Basic timeseries can accept direct uploads. Non-basic selections were ignored.",
              type = "warning",
              duration = 8
            )
          }

          ids <- as.integer(basic$timeseries_id)
          timeseries(ids[[1]])
          upload_timeseries_ids(ids)
          sync_ts_table_selection(ids)
        }
      } else {
        timeseries(NULL)
        upload_timeseries_ids(integer())
      }
    }

    observeEvent(input$ts_table_rows_selected, {
      if (isTRUE(suppress_ts_table_selection())) {
        suppress_ts_table_selection(FALSE)
        return()
      }

      sel <- input$ts_table_rows_selected
      if (selection_change_requires_data_reset(sel)) {
        pending_ts_table_selection(sel)
        sync_ts_table_selection(upload_timeseries_ids())
        show_selection_reset_modal()
        return()
      }

      apply_ts_table_selection(sel)
    })

    observeEvent(input$cancel_timeseries_selection_reset, {
      pending_ts_table_selection(NULL)
      sync_ts_table_selection(upload_timeseries_ids())
      removeModal()
    })

    observeEvent(input$confirm_timeseries_selection_reset, {
      sel <- pending_ts_table_selection()
      pending_ts_table_selection(NULL)
      removeModal()
      reset_upload_state()
      apply_ts_table_selection(sel)
    })

    observeEvent(input$reset_timeseries_selection, {
      timeseries(NULL)
      upload_timeseries_ids(integer())
      reset_upload_state()
      sync_ts_table_selection()
      showNotification(
        "Timeseries selection and uploaded data reset.",
        type = "message"
      )
    })

    output$nonbasic_members_table <- DT::renderDT({
      df <- nonbasic_member_options()
      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(
          message = "No member timeseries are defined for this timeseries.",
          stringsAsFactors = FALSE
        )
        return(DT::datatable(df, selection = "none", rownames = FALSE))
      }

      df$timeseries_type <- as.factor(df$timeseries_type)
      df$can_accept_data <- as.factor(df$can_accept_data)
      df$active <- as.factor(df$active)
      df$publicly_visible <- as.factor(df$publicly_visible)
      df$sensor_priority <- as.factor(df$sensor_priority)

      DT::datatable(
        df,
        selection = "single",
        options = list(
          columnDefs = list(
            list(
              targets = which(names(df) == "timeseries_type_code") - 1L,
              visible = FALSE
            )
          ),
          pageLength = 10,
          scrollX = TRUE
        ),
        filter = "top",
        rownames = FALSE
      )
    })

    observeEvent(input$use_member_timeseries, {
      df <- nonbasic_member_options()
      sel <- input$nonbasic_members_table_rows_selected
      if (is.null(df) || nrow(df) == 0 || is.null(sel) || length(sel) != 1) {
        showNotification(
          "Select a member timeseries first.",
          type = "error"
        )
        return()
      }

      selected <- df[sel, , drop = FALSE]
      if (!isTRUE(selected$can_accept_data[[1]])) {
        showNotification(
          "Select a Basic member timeseries. Compound members cannot accept direct uploads.",
          type = "error",
          duration = 8
        )
        return()
      }

      target_id <- as.integer(selected$timeseries_id[[1]])
      main_row <- match(target_id, ts_meta()$timeseries_id)
      if (is.na(main_row)) {
        showNotification(
          "Selected member timeseries is no longer in the main table. Reload the module and try again.",
          type = "error",
          duration = 8
        )
        return()
      }

      timeseries(target_id)
      upload_timeseries_ids(as.integer(target_id))
      upload_jobs(NULL)
      upload_validation$jobs <- NULL
      sync_ts_table_selection(target_id)
      removeModal()
      showNotification(
        paste("Selected timeseries", target_id, "for data entry."),
        type = "message"
      )
    })

    selected_timeseries_meta <- reactive({
      req(timeseries())
      meta <- ts_meta()
      meta[meta$timeseries_id == timeseries(), , drop = FALSE]
    })

    selected_upload_timeseries_meta <- reactive({
      ids <- upload_timeseries_ids()
      if (length(ids) == 0 && !is.null(timeseries())) {
        ids <- as.integer(timeseries())
      }
      meta <- ts_meta()
      out <- meta[match(ids, meta$timeseries_id), , drop = FALSE]
      out[!is.na(out$timeseries_id), , drop = FALSE]
    })

    multi_upload_active <- reactive({
      nrow(selected_upload_timeseries_meta()) > 1
    })

    upload_include_input_id <- function(timeseries_id) {
      paste0("upload_include_", as.integer(timeseries_id))
    }

    class_apply_all_input_id <- function(class_name) {
      paste0(class_name, "_apply_all_timeseries")
    }

    target_output_id <- function(prefix, timeseries_id) {
      paste0(prefix, "_", as.integer(timeseries_id))
    }

    class_ranges_tabset_id <- function(class_name) {
      paste0(class_name, "_ranges_tabset")
    }

    class_range_output_id <- function(class_name, timeseries_id) {
      paste0(class_name, "_ranges_table_", as.integer(timeseries_id))
    }

    target_label <- function(target, include_id = TRUE) {
      parts <- c(
        as.character(target$location[[1]]),
        as.character(target$parameter[[1]]),
        as.character(target$units[[1]]),
        paste0(as.character(target$record_rate_minutes[[1]]), " min")
      )
      parts <- parts[!is.na(parts) & nzchar(parts)]
      label <- paste(parts, collapse = " | ")
      if (isTRUE(include_id)) {
        label <- paste0("Timeseries ", target$timeseries_id[[1]], ": ", label)
      }
      label
    }

    output$selected_upload_targets <- renderUI({
      targets <- selected_upload_timeseries_meta()
      if (nrow(targets) == 0) {
        return(NULL)
      }
      div(
        class = "alert alert-info",
        style = "padding: 8px; margin: 8px 0;",
        tags$strong(
          "Selected upload target",
          if (nrow(targets) > 1) "s" else ""
        ),
        tags$ul(lapply(seq_len(nrow(targets)), function(i) {
          tags$li(target_label(targets[i, , drop = FALSE]))
        }))
      )
    })

    output$multi_upload_note <- renderUI({
      if (!isTRUE(multi_upload_active())) {
        return(NULL)
      }
      div(
        class = "alert alert-info",
        style = "padding: 8px; margin-top: 10px;",
        "Multiple upload targets are selected. Review each mapped target below and choose which targets to upload before clicking an upload button."
      )
    })

    selected_timeseries_is_basic <- reactive({
      if (is.null(timeseries())) {
        return(FALSE)
      }
      meta <- selected_timeseries_meta()
      nrow(meta) == 1 && identical(meta$timeseries_type_code[[1]], "basic")
    })

    selected_timeseries_units <- reactive({
      meta <- selected_timeseries_meta()
      if (!nrow(meta)) {
        return(NA_character_)
      }
      unit <- meta$units[[1]]
      if (is.na(unit) || !nzchar(unit)) {
        return(NA_character_)
      }
      unit
    })

    selected_units_warning_tag <- function() {
      if (is.null(timeseries())) {
        return(div(
          class = "alert alert-warning",
          style = "margin-bottom: 10px;",
          tags$strong("Select a timeseries before entering or uploading data."),
        ))
      }
      if (isTRUE(multi_upload_active())) {
        return(NULL)
      }

      unit <- selected_timeseries_units()
      unit_text <- if (is.na(unit)) {
        "No database unit is set for this timeseries."
      } else {
        paste0("Database unit for this timeseries: ", unit)
      }
      detail <- "Enter or convert values to this unit before upload (see yellow 'Convert units' button if needed)."

      div(
        class = if (is.na(unit)) {
          "alert alert-danger"
        } else {
          "alert alert-warning"
        },
        style = "margin-bottom: 10px;",
        tags$strong(unit_text),
        tags$div(detail)
      )
    }

    output$selected_units_warning <- renderUI({
      selected_units_warning_tag()
    })
    output$selected_units_warning_last <- renderUI({
      selected_units_warning_tag()
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

    shift_datetime_inputs <- function(input_ids, tz_name) {
      for (input_id in input_ids) {
        shift_air_datetime_input_timezone(session, input, input_id, tz_name)
      }
    }

    shift_class_modal_inputs <- function(class_name, tz_name) {
      shift_datetime_inputs(
        c(
          paste0(class_name, "_modal_start"),
          paste0(class_name, "_modal_end")
        ),
        tz_name
      )
    }

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
      df = data.frame(
        datetime = character(),
        value = numeric(),
        grade = character(),
        approval = character(),
        qualifier = character(),
        stringsAsFactors = FALSE
      ),
      parsed_datetime = NULL,
      parsed_value = NULL
    )
    upload_jobs <- reactiveVal(NULL)
    upload_validation <- reactiveValues(jobs = NULL)

    unit_conversion_state <- reactiveValues(
      previous_values = NULL,
      previous_label = NULL
    )

    table_render_tick <- reactiveVal(0L)
    refresh_data_table <- function() {
      table_render_tick(isolate(table_render_tick()) + 1L)
    }

    observeEvent(timeseries(), {
      unit_conversion_state$previous_values <- NULL
      unit_conversion_state$previous_label <- NULL
    })

    unit_conversion_choices <- reactive({
      req(moduleData$unit_conversions)
      unit <- selected_timeseries_units()
      if (is.na(unit)) {
        return(moduleData$unit_conversions[0, , drop = FALSE])
      }

      moduleData$unit_conversions[
        moduleData$unit_conversions$to_unit == unit,
        ,
        drop = FALSE
      ]
    })

    unit_conversion_controls <- function() {
      req(timeseries())
      unit <- selected_timeseries_units()
      if (is.na(unit)) {
        return(div(
          class = "alert alert-danger",
          "Unit conversion is unavailable because the selected timeseries has no database unit."
        ))
      }

      choices_df <- unit_conversion_choices()
      choices <- character()
      if (nrow(choices_df) > 0) {
        choices <- stats::setNames(
          as.character(choices_df$conversion_id),
          paste0(
            choices_df$from_unit,
            " to ",
            choices_df$to_unit,
            " (value * ",
            signif(as.numeric(choices_df$scale_a), 8),
            ifelse(
              as.numeric(choices_df$scale_b) == 0,
              "",
              paste0(" + ", signif(as.numeric(choices_df$scale_b), 8))
            ),
            ")"
          )
        )
      }
      default_mode <- if (length(choices) > 0) "database" else "custom"

      tagList(
        div(
          class = "well",
          style = "padding: 10px; margin-bottom: 10px;",
          tags$strong("Convert table values to database units"),
          tags$div(
            class = "text-muted small",
            paste0(
              "Use this if the value column is not already in ",
              unit,
              ". Only the value column is changed."
            )
          ),
          radioButtons(
            ns("unit_conversion_mode"),
            "Conversion source",
            choices = c(
              "Database conversion" = "database",
              "Custom factor" = "custom"
            ),
            selected = default_mode,
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.unit_conversion_mode == 'database'",
            ns = ns,
            if (length(choices) > 0) {
              selectizeInput(
                ns("unit_conversion_id"),
                "Convert from",
                choices = choices,
                selected = choices[[1]],
                options = list(placeholder = "No database conversion listed")
              )
            } else {
              div(
                class = "text-muted small",
                "No database conversions are listed for this unit. Use a custom factor."
              )
            }
          ),
          conditionalPanel(
            condition = "input.unit_conversion_mode == 'custom'",
            ns = ns,
            numericInput(
              ns("custom_unit_factor"),
              paste0("Custom factor to ", unit),
              value = NA_real_,
              min = 0,
              step = "any"
            )
          ),
          div(
            actionButton(ns("convert_units"), "Convert value column"),
            actionButton(ns("rollback_unit_conversion"), "Roll back conversion")
          ),
          uiOutput(ns("unit_conversion_status"))
        )
      )
    }

    output$unit_conversion_modal_body <- renderUI({
      unit_conversion_controls()
    })

    observeEvent(input$open_unit_conversion, {
      if (is.null(timeseries())) {
        showNotification(
          "Select a timeseries before converting units.",
          type = "error"
        )
        return()
      }

      showModal(modalDialog(
        title = "Convert table values to database units",
        uiOutput(ns("unit_conversion_modal_body")),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "m"
      ))
    })

    output$unit_conversion_status <- renderUI({
      if (is.null(unit_conversion_state$previous_values)) {
        return(NULL)
      }

      div(
        class = "text-muted small",
        paste("Last conversion:", unit_conversion_state$previous_label)
      )
    })

    observeEvent(input$convert_units, {
      if (!is.null(unit_conversion_state$previous_values)) {
        showNotification(
          "Values have already been converted. Roll back before converting again.",
          type = "error",
          duration = 8
        )
        return()
      }

      if (nrow(data$df) == 0) {
        showNotification("No table values to convert.", type = "error")
        return()
      }

      values <- suppressWarnings(as.numeric(data$df$value))
      if (any(is.na(values))) {
        showNotification(
          "Value column must be numeric with no missing values before conversion.",
          type = "error",
          duration = 8
        )
        return()
      }

      unit <- selected_timeseries_units()
      if (is.na(unit)) {
        showNotification(
          "No database unit is set for the selected timeseries.",
          type = "error",
          duration = 8
        )
        return()
      }

      if (identical(input$unit_conversion_mode, "custom")) {
        factor <- suppressWarnings(as.numeric(input$custom_unit_factor))
        if (length(factor) != 1 || is.na(factor) || factor <= 0) {
          showNotification(
            "Enter a positive custom conversion factor.",
            type = "error"
          )
          return()
        }
        new_values <- values * factor
        label <- paste0("custom factor ", signif(factor, 8), " to ", unit)
      } else {
        if (!isTruthy(input$unit_conversion_id)) {
          showNotification(
            paste0("No database conversion to ", unit, " is selected."),
            type = "error",
            duration = 8
          )
          return()
        }
        choices_df <- unit_conversion_choices()
        idx <- match(
          as.integer(input$unit_conversion_id),
          choices_df$conversion_id
        )
        if (is.na(idx)) {
          showNotification(
            paste0("No database conversion to ", unit, " is selected."),
            type = "error",
            duration = 8
          )
          return()
        }
        new_values <- as.numeric(choices_df$scale_a[[idx]]) *
          values +
          as.numeric(choices_df$scale_b[[idx]])
        label <- paste0(
          choices_df$from_unit[[idx]],
          " to ",
          choices_df$to_unit[[idx]]
        )
      }

      unit_conversion_state$previous_values <- data$df$value
      unit_conversion_state$previous_label <- label
      data$df$value <- new_values
      refresh_data_table()
      showNotification(
        paste("Converted value column:", label),
        type = "message"
      )
    })

    observeEvent(input$rollback_unit_conversion, {
      if (is.null(unit_conversion_state$previous_values)) {
        showNotification("No unit conversion to roll back.", type = "message")
        return()
      }

      if (length(unit_conversion_state$previous_values) != nrow(data$df)) {
        showNotification(
          "Cannot roll back because the table row count has changed.",
          type = "error",
          duration = 8
        )
        return()
      }

      data$df$value <- unit_conversion_state$previous_values
      refresh_data_table()
      showNotification(
        paste("Rolled back conversion:", unit_conversion_state$previous_label),
        type = "message"
      )
      unit_conversion_state$previous_values <- NULL
      unit_conversion_state$previous_label <- NULL
    })

    uploaded_file_ext <- reactive({
      req(input$file)
      tolower(tools::file_ext(input$file$name))
    })

    uploaded_file_is_logger <- reactive({
      uploaded_file_ext() %in% c("xle", "html", "htm", "hobo")
    })

    logger_upload_message <- reactive({
      req(input$file)
      if (!uploaded_file_is_logger()) {
        return(NULL)
      }

      note <- attr(upload_raw(), "logger_timezone_note")
      if (is.character(note) && length(note) == 1 && nzchar(note)) {
        return(note)
      }

      "Logger datetimes were prepared as UTC for upload."
    })

    raw_file_preview <- reactive({
      req(input$file)
      ext <- uploaded_file_ext()
      if (ext %in% c("xle", "html", "htm", "hobo")) {
        out <- utils::head(upload_raw(), 100)
        return(data.frame(Row = seq_len(nrow(out)), out, check.names = FALSE))
      }

      if (ext == "xlsx") {
        out <- readxl::read_xlsx(
          input$file$datapath,
          sheet = 1,
          col_names = FALSE,
          n_max = 100
        ) |>
          as.data.frame()
      } else if (ext == "csv") {
        lines <- readLines(input$file$datapath, n = 100, warn = FALSE)
        if (length(lines) == 0) {
          return(data.frame())
        }
        rows <- lapply(lines, function(line) {
          if (!nzchar(line)) {
            return("")
          }
          tryCatch(
            read.csv(
              text = line,
              header = FALSE,
              stringsAsFactors = FALSE,
              check.names = FALSE
            ) |>
              unlist(use.names = FALSE),
            error = function(e) line
          )
        })
        max_cols <- max(lengths(rows), 1L)
        out <- as.data.frame(
          do.call(
            rbind,
            lapply(rows, function(row) {
              length(row) <- max_cols
              row
            })
          ),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        return(NULL)
      }

      names(out) <- paste0("Column ", seq_len(ncol(out)))
      data.frame(Line = seq_len(nrow(out)), out, check.names = FALSE)
    })

    upload_raw <- reactive({
      req(input$file)
      ext <- uploaded_file_ext()

      if (uploaded_file_is_logger()) {
        return(read_logger_file_data(input$file$datapath, file_type = ext))
      }

      req(input$raw_start_row)
      # Set starting row to 1 if input is null, so we don't have to catch empty inputs in validate
      starting_row <- ifelse(
        length(input$raw_start_row) < 1,
        1,
        input$raw_start_row
      )
      if (ext == 'xlsx') {
        out <- readxl::read_xlsx(
          input$file$datapath,
          sheet = 1,
          skip = starting_row - 1
        ) |>
          as.data.frame()

        return(out)
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

      stop("Unsupported file extension: .", ext, call. = FALSE)
    })

    class_type_choices <- reactive({
      list(
        grade = DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT grade_type_id AS id, grade_type_code AS code, grade_type_description AS description, color_code FROM public.grade_types ORDER BY grade_type_id"
        ),
        approval = DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT approval_type_id AS id, approval_type_code AS code, approval_type_description AS description, color_code FROM public.approval_types ORDER BY approval_type_id"
        ),
        qualifier = DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT qualifier_type_id AS id, qualifier_type_code AS code, qualifier_type_description AS description, color_code FROM public.qualifier_types ORDER BY qualifier_type_id"
        )
      )
    })

    map_modal_state <- reactiveValues(
      step = "columns",
      pending_df = NULL,
      pending_jobs = NULL,
      class_values = list(
        grade = character(),
        approval = character(),
        qualifier = character()
      )
    )

    upload_mapping_input_id <- function(field, timeseries_id) {
      paste0("upload_", field, "_col_", as.integer(timeseries_id))
    }

    pick_uploaded_col <- function(candidates, default = "") {
      uploaded_names <- names(upload_raw())
      out <- uploaded_names[tolower(uploaded_names) %in% tolower(candidates)]
      if (length(out) > 0) out[[1]] else default
    }

    guess_value_col <- function(target, used_cols = character()) {
      uploaded_names <- names(upload_raw())
      candidates <- uploaded_names[!uploaded_names %in% used_cols]
      candidates <- candidates[
        !grepl("date|time", candidates, ignore.case = TRUE)
      ]
      if (length(candidates) == 0) {
        return("")
      }

      parameter <- tolower(as.character(target$parameter[[1]]))
      patterns <- character()
      if (grepl("temperature|temp", parameter)) {
        patterns <- c(patterns, "temperature|temp")
      }
      if (grepl("conduct|specific conductance|spc", parameter)) {
        patterns <- c(patterns, "conduct|spc|specific")
      }
      if (grepl("level|water level", parameter)) {
        patterns <- c(patterns, "level")
      }
      if (grepl("pressure", parameter)) {
        patterns <- c(patterns, "pressure")
      }
      if (grepl("depth", parameter)) {
        patterns <- c(patterns, "depth")
      }

      for (pattern in patterns) {
        match <- candidates[grepl(pattern, candidates, ignore.case = TRUE)]
        if (length(match) > 0) {
          return(match[[1]])
        }
      }

      ""
    }

    build_upload_jobs_from_column_mapping <- function() {
      targets <- selected_upload_timeseries_meta()
      req(nrow(targets) > 0)
      multi_target <- nrow(targets) > 1
      raw <- upload_raw()

      jobs <- vector("list", nrow(targets))
      for (i in seq_len(nrow(targets))) {
        target <- targets[i, , drop = FALSE]
        tsid <- as.integer(target$timeseries_id[[1]])
        datetime_input <- if (multi_target) {
          input[[upload_mapping_input_id("datetime", tsid)]]
        } else {
          input$upload_datetime_col
        }
        value_input <- if (multi_target) {
          input[[upload_mapping_input_id("value", tsid)]]
        } else {
          input$upload_value_col
        }
        req(datetime_input, value_input)

        df_mapped <- data.frame(
          datetime = raw[[datetime_input]],
          value = raw[[value_input]]
        )

        for (class_name in c("grade", "approval", "qualifier")) {
          class_input <- if (multi_target) {
            input[[upload_mapping_input_id(class_name, tsid)]]
          } else {
            input[[paste0("upload_", class_name, "_col")]]
          }
          if (isTruthy(class_input) && class_input %in% names(raw)) {
            df_mapped[[class_name]] <- raw[[class_input]]
          }
        }

        jobs[[i]] <- list(
          timeseries_id = tsid,
          label = target_label(target),
          data = df_mapped
        )
      }

      jobs
    }

    selected_class_cols <- reactive({
      cols <- c()
      if (isTRUE(multi_upload_active())) {
        targets <- selected_upload_timeseries_meta()
        for (target_id in targets$timeseries_id) {
          for (class_name in c("grade", "approval", "qualifier")) {
            if (
              isTruthy(input[[upload_mapping_input_id(class_name, target_id)]])
            ) {
              cols <- c(cols, class_name)
            }
          }
        }
      } else {
        if (isTruthy(input$upload_grade_col)) {
          cols <- c(cols, "grade")
        }
        if (isTruthy(input$upload_approval_col)) {
          cols <- c(cols, "approval")
        }
        if (isTruthy(input$upload_qualifier_col)) {
          cols <- c(cols, "qualifier")
        }
      }
      unique(cols)
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

      if (uploaded_file_is_logger()) {
        shinyjs::enable(target_id)
      } else if (is.null(input$raw_start_row)) {
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
          uploaded_file_is_logger() || input$raw_start_row > 0,
          'Invalid header row'
        ),
        need(
          ncol(upload_raw()) >= 2,
          'Uploaded file must have at least two columns (one containing date time, and one containing measurement value)'
        )
      )

      uploaded_names <- names(upload_raw())
      choices_optional <- stats::setNames(uploaded_names, uploaded_names)

      targets <- selected_upload_timeseries_meta()
      if (nrow(targets) > 1) {
        datetime_default <- pick_uploaded_col(
          c("datetime", "date_time", "date time", "timestamp", "date"),
          uploaded_names[[1]]
        )
        used_values <- character()
        mapping_rows <- lapply(seq_len(nrow(targets)), function(i) {
          target <- targets[i, , drop = FALSE]
          tsid <- as.integer(target$timeseries_id[[1]])
          value_default <- guess_value_col(target, used_values)
          if (!nzchar(value_default) && length(uploaded_names) >= i + 1) {
            value_default <- uploaded_names[[i + 1]]
          }
          if (nzchar(value_default)) {
            used_values <<- c(used_values, value_default)
          }

          tags$tr(
            tags$td(paste0(
              tsid,
              ": ",
              target$parameter[[1]],
              " (",
              target$units[[1]],
              ")"
            )),
            tags$td(selectizeInput(
              ns(upload_mapping_input_id("datetime", tsid)),
              NULL,
              choices = uploaded_names,
              selected = datetime_default,
              width = "100%"
            )),
            tags$td(selectizeInput(
              ns(upload_mapping_input_id("value", tsid)),
              NULL,
              choices = uploaded_names,
              selected = value_default,
              width = "100%"
            )),
            tags$td(selectizeInput(
              ns(upload_mapping_input_id("grade", tsid)),
              NULL,
              choices = c("None" = "", choices_optional),
              selected = "",
              width = "100%"
            )),
            tags$td(selectizeInput(
              ns(upload_mapping_input_id("approval", tsid)),
              NULL,
              choices = c("None" = "", choices_optional),
              selected = "",
              width = "100%"
            )),
            tags$td(selectizeInput(
              ns(upload_mapping_input_id("qualifier", tsid)),
              NULL,
              choices = c("None" = "", choices_optional),
              selected = "",
              width = "100%"
            ))
          )
        })

        return(tags$div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm table-striped align-middle",
            tags$thead(tags$tr(
              tags$th("Timeseries"),
              tags$th("Datetime"),
              tags$th("Value"),
              tags$th("Grade"),
              tags$th("Approval"),
              tags$th("Qualifier")
            )),
            tags$tbody(mapping_rows)
          )
        ))
      }

      tagList(
        selectizeInput(
          ns('upload_datetime_col'),
          'Select the column for datetime:',
          choices = uploaded_names,
          selected = pick_uploaded_col(
            c('datetime', 'date_time', 'date', 'time'),
            uploaded_names[[1]]
          )
        ),
        selectizeInput(
          ns('upload_value_col'),
          'Select the column for values:',
          choices = uploaded_names,
          selected = pick_uploaded_col(
            c('value', 'values', 'measurement', 'measured_value'),
            uploaded_names[[2]]
          )
        ),
        if (length(uploaded_names) > 2) {
          div(
            selectizeInput(
              ns('upload_grade_col'),
              'Optional: select the column for grades:',
              choices = c("None" = "", choices_optional),
              selected = ""
            ),
            selectizeInput(
              ns('upload_approval_col'),
              'Optional: select the column for approvals:',
              choices = c("None" = "", choices_optional),
              selected = ""
            ),
            selectizeInput(
              ns('upload_qualifier_col'),
              'Optional: select the column for qualifiers:',
              choices = c("None" = "", choices_optional),
              selected = ""
            )
          )
        }
      )
    })

    output$raw_file_preview <- DT::renderDT({
      preview <- raw_file_preview()
      validate(
        need(
          !is.null(preview) && nrow(preview) > 0,
          "No preview is available for this file."
        )
      )

      selected_row <- if (
        uploaded_file_is_logger() ||
          is.null(input$raw_start_row) ||
          is.na(input$raw_start_row) ||
          input$raw_start_row < 1
      ) {
        NA_integer_
      } else {
        as.integer(input$raw_start_row)
      }

      preview_table <- DT::datatable(
        preview,
        rownames = FALSE,
        class = "compact stripe",
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50, 100),
          scrollX = TRUE,
          autoWidth = TRUE
        )
      )

      if (!uploaded_file_is_logger() && "Line" %in% names(preview)) {
        preview_table <- preview_table |>
          DT::formatStyle(
            "Line",
            target = "row",
            backgroundColor = DT::styleEqual(selected_row, "#fff3cd"),
            fontWeight = DT::styleEqual(selected_row, "bold")
          )
      }

      preview_table
    })

    parse_datetime <- function(x) {
      if (inherits(x, "POSIXct")) {
        return(x)
      }
      if (inherits(x, "Date")) {
        return(as.POSIXct(x, tz = "UTC"))
      }
      if (is.list(x) && !is.null(x$date)) {
        x <- unlist(x$date, use.names = FALSE)
      }
      x <- trimws(as.character(x))
      # Switch T character with space for ISO like formats
      x <- gsub("T", " ", x)
      x <- gsub("(\\d{2}:\\d{2}:\\d{2})\\.\\d+", "\\1", x)
      x <- gsub("Z$", " +0000", x)
      x <- gsub("\\s+UTC([+-]\\d{2}:?\\d{2})$", " \\1", x)
      x <- gsub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", x)

      lubridate::parse_date_time(
        x,
        orders = c(
          "Ymd HMS",
          "Ymd HM",
          "mdY HMS",
          "mdY HM",
          "Ymd HMS z",
          "Ymd HM z",
          "mdY HMS z",
          "mdY HM z",
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
      out <- df[, c("datetime", "value")]

      out$datetime <- if (inherits(out$datetime, "POSIXct")) {
        format(out$datetime, "%Y-%m-%d %H:%M:%S")
      } else {
        as.character(out$datetime)
      }

      out$value <- suppressWarnings(as.numeric(out$value))
      for (class_name in c("grade", "approval", "qualifier")) {
        out[[class_name]] <- if (class_name %in% names(df)) {
          as.character(df[[class_name]])
        } else {
          ""
        }
      }
      out
    }

    selected_offset_tz <- function(value, default = input$UTC_offset) {
      fallback <- normalize_input_timezone(
        default,
        default = format_utc_offset(0L)
      )
      normalize_input_timezone(value, default = fallback)
    }

    selected_offset_seconds <- function(value, default = input$UTC_offset) {
      parse_utc_offset_minutes(
        selected_offset_tz(value, default),
        default = "UTC+00:00"
      ) *
        60
    }

    class_offset_tz <- function(class_name) {
      selected_offset_tz(input[[paste0(class_name, "_utc_offset")]])
    }

    table_datetimes_to_utc <- function(x, tz_name = input$UTC_offset) {
      out <- parse_datetime(x)
      valid_idx <- !is.na(out)
      if (any(valid_idx)) {
        out[valid_idx] <- out[valid_idx] -
          selected_offset_seconds(tz_name, default = format_utc_offset(0L))
      }
      attr(out, "tzone") <- "UTC"
      out
    }

    parse_utc_datetime_value <- function(datetime_value) {
      if (is.null(datetime_value) || length(datetime_value) == 0) {
        return(empty_utc_datetime())
      }

      out <- tryCatch(
        suppressWarnings(coerce_utc_datetime(datetime_value)),
        error = function(e) empty_utc_datetime(length(datetime_value))
      )
      needs_parse <- is.na(out)
      if (any(needs_parse)) {
        parsed <- parse_datetime(datetime_value[needs_parse])
        out[needs_parse] <- parsed
      }
      attr(out, "tzone") <- "UTC"
      out
    }

    scalar_utc_datetime_value <- function(datetime_value) {
      out <- parse_utc_datetime_value(datetime_value)
      if (!length(out) || is.na(out[1])) {
        return(empty_utc_datetime())
      }
      out[1]
    }

    input_datetime_has_timezone <- function(datetime_value) {
      if (is.null(datetime_value) || inherits(datetime_value, "POSIXct")) {
        return(TRUE)
      }
      if (is.list(datetime_value) && !is.null(datetime_value$tz)) {
        return(TRUE)
      }
      x <- trimws(as.character(datetime_value))
      any(grepl("(Z|UTC|GMT|[+-]\\d{2}:?\\d{2})$", x))
    }

    scalar_display_datetime_to_utc <- function(datetime_value, tz_name) {
      if (is.list(datetime_value) && !is.null(datetime_value$date)) {
        input_tz <- if (!is.null(datetime_value$tz)) {
          selected_offset_tz(datetime_value$tz[[1]], default = tz_name)
        } else {
          tz_name
        }
        out <- table_datetimes_to_utc(
          unlist(datetime_value$date, use.names = FALSE),
          input_tz
        )
        if (!length(out) || is.na(out[1])) {
          return(empty_utc_datetime())
        }
        return(out[1])
      }
      if (input_datetime_has_timezone(datetime_value)) {
        return(scalar_utc_datetime_value(datetime_value))
      }
      out <- table_datetimes_to_utc(datetime_value, tz_name)
      if (!length(out) || is.na(out[1])) {
        return(empty_utc_datetime())
      }
      out[1]
    }

    format_utc_datetimes_for_display <- function(datetime_value, tz_name) {
      utc_values <- parse_utc_datetime_value(datetime_value)
      out <- rep(NA_character_, length(utc_values))
      valid_idx <- !is.na(utc_values)
      if (any(valid_idx)) {
        display_values <- utc_values[valid_idx] +
          selected_offset_seconds(tz_name, default = format_utc_offset(0L))
        out[valid_idx] <- format(
          display_values,
          "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      }
      out
    }

    shift_display_datetime_strings <- function(x, from_tz, to_tz) {
      out <- as.character(x)
      utc_values <- table_datetimes_to_utc(x, from_tz)
      valid_idx <- !is.na(utc_values)
      if (any(valid_idx)) {
        out[valid_idx] <- format_utc_datetimes_for_display(
          utc_values[valid_idx],
          to_tz
        )
      }
      out
    }

    uploaded_data_bounds <- function(tz_name = input$UTC_offset, df = data$df) {
      if (nrow(df) == 0 || !("datetime" %in% names(df))) {
        return(NULL)
      }

      utc_values <- table_datetimes_to_utc(df$datetime, input$UTC_offset)
      valid_values <- utc_values[!is.na(utc_values)]
      if (!length(valid_values)) {
        return(NULL)
      }

      tz_name <- selected_offset_tz(tz_name, default = input$UTC_offset)
      start_utc <- min(valid_values)
      end_utc <- max(valid_values)

      list(
        start_utc = start_utc,
        end_utc = end_utc,
        start_display = format_utc_datetimes_for_display(start_utc, tz_name),
        end_display = format_utc_datetimes_for_display(end_utc, tz_name),
        tz = tz_name
      )
    }

    active_preview_timeseries <- function() {
      if (!isTRUE(multi_upload_active())) {
        return(as.integer(timeseries()))
      }
      jobs <- upload_review_jobs()
      if (length(jobs) == 0) {
        return(as.integer(timeseries()))
      }
      selected <- input$preview_timeseries_tabset
      if (is.null(selected) || length(selected) == 0 || !nzchar(selected[[1]])) {
        return(as.integer(jobs[[1]]$timeseries_id))
      }
      as.integer(sub("^timeseries_", "", selected[[1]]))
    }

    active_preview_data <- function() {
      active_job_data(active_preview_timeseries())
    }

    last_preview_bounds_signature <- reactiveVal(NULL)

    update_preview_datetime_bounds <- function() {
      bounds <- uploaded_data_bounds(
        input$preview_utc_offset,
        active_preview_data()
      )
      if (is.null(bounds)) {
        last_preview_bounds_signature(NULL)
        for (input_id in c("preview_start_datetime", "preview_end_datetime")) {
          shinyWidgets::updateAirDateInput(
            session,
            inputId = input_id,
            clear = TRUE,
            options = list(minDate = FALSE, maxDate = FALSE)
          )
        }
        return(invisible(NULL))
      }

      bounds_signature <- paste(
        as.numeric(bounds$start_utc),
        as.numeric(bounds$end_utc),
        bounds$tz,
        sep = "|"
      )
      if (identical(last_preview_bounds_signature(), bounds_signature)) {
        return(invisible(NULL))
      }

      date_options <- list(
        minDate = bounds$start_utc,
        maxDate = bounds$end_utc
      )
      shinyWidgets::updateAirDateInput(
        session,
        inputId = "preview_start_datetime",
        value = bounds$start_utc,
        tz = air_datetime_widget_timezone(bounds$tz),
        options = date_options
      )
      shinyWidgets::updateAirDateInput(
        session,
        inputId = "preview_end_datetime",
        value = bounds$end_utc,
        tz = air_datetime_widget_timezone(bounds$tz),
        options = date_options
      )
      last_preview_bounds_signature(bounds_signature)
      invisible(NULL)
    }

    uploaded_data_bounds_ui <- function(class_name) {
      bounds <- class_modal_bounds(class_name)
      if (is.null(bounds)) {
        return(tags$p(
          class = "text-muted small",
          "Uploaded data range unavailable until valid datetime rows exist."
        ))
      }

      tags$div(
        class = "text-muted small mb-2",
        tags$div(tags$strong("Uploaded data range")),
        tags$div(paste("Start:", bounds$start_display)),
        tags$div(paste("End:", bounds$end_display)),
        tags$div(paste("UTC offset:", bounds$tz)),
        if (!is.null(bounds$per_target)) {
          tags$div(
            style = "margin-top: 8px;",
            tags$div(
              tags$strong("Selected timeseries data ranges")
            ),
            tags$div(
              "These ranges reflect the current rows after any deletions."
            ),
            tags$table(
              class = "table table-sm table-bordered",
              style = "font-size: 12px; margin-top: 4px;",
              tags$thead(tags$tr(
                tags$th("Timeseries"),
                tags$th("Start"),
                tags$th("End")
              )),
              tags$tbody(lapply(seq_len(nrow(bounds$per_target)), function(i) {
                tags$tr(
                  tags$td(bounds$per_target$target[[i]]),
                  tags$td(bounds$per_target$start[[i]]),
                  tags$td(bounds$per_target$end[[i]])
                )
              }))
            )
          )
        }
      )
    }

    previous_data_timezone <- reactiveVal(format_utc_offset(0L))

    observeEvent(input$UTC_offset, {
      master_tz <- selected_offset_tz(
        input$UTC_offset,
        default = format_utc_offset(0L)
      )
      previous_tz <- previous_data_timezone()

      if (!identical(previous_tz, master_tz) && nrow(data$df) > 0) {
        data$df$datetime <- shift_display_datetime_strings(
          data$df$datetime,
          previous_tz,
          master_tz
        )
        refresh_data_table()
      }

      previous_data_timezone(master_tz)
      shift_datetime_inputs("delete_cutoff_datetime", master_tz)

      for (input_id in c(
        "preview_utc_offset",
        "delete_utc_offset",
        "approval_utc_offset",
        "grade_utc_offset",
        "qualifier_utc_offset"
      )) {
        updateSelectizeInput(session, input_id, selected = master_tz)
      }
    })

    observeEvent(
      input$preview_utc_offset,
      {
        update_preview_datetime_bounds()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      data$df,
      {
        update_preview_datetime_bounds()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_utc_offset,
      {
        shift_datetime_inputs(
          "delete_cutoff_datetime",
          selected_offset_tz(
            input$delete_utc_offset,
            default = input$UTC_offset
          )
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$approval_utc_offset,
      {
        update_class_modal_datetime_limits("approval")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$grade_utc_offset,
      {
        update_class_modal_datetime_limits("grade")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$qualifier_utc_offset,
      {
        update_class_modal_datetime_limits("qualifier")
      },
      ignoreInit = TRUE
    )

    empty_class_range_df <- function() {
      data.frame(
        code = character(),
        description = character(),
        start_datetime = character(),
        end_datetime = character(),
        stringsAsFactors = FALSE
      )
    }

    class_ranges <- reactiveValues(
      grade = empty_class_range_df(),
      approval = empty_class_range_df(),
      qualifier = empty_class_range_df()
    )

    target_class_ranges <- reactiveValues(
      grade = list(),
      approval = list(),
      qualifier = list()
    )

    class_apply_all <- function(class_name) {
      !isTRUE(multi_upload_active()) ||
        checkbox_current_value(
          class_apply_all_input_id(class_name),
          class_apply_all_default(class_name)
        )
    }

    class_apply_all_default <- function(class_name) {
      ranges <- target_class_ranges[[class_name]]
      if (!length(ranges)) {
        return(TRUE)
      }
      !any(vapply(ranges, function(x) nrow(x) > 0, logical(1)))
    }

    active_class_timeseries <- function(class_name) {
      if (!isTRUE(multi_upload_active())) {
        return(as.integer(timeseries()))
      }
      selected <- input[[class_ranges_tabset_id(class_name)]]
      if (is.null(selected) || length(selected) == 0 || !nzchar(selected[[1]])) {
        targets <- selected_upload_timeseries_meta()
        if (nrow(targets) == 0) {
          return(as.integer(timeseries()))
        }
        return(as.integer(targets$timeseries_id[[1]]))
      }
      as.integer(sub("^timeseries_", "", selected[[1]]))
    }

    get_class_ranges <- function(class_name, timeseries_id = NULL) {
      if (class_apply_all(class_name) || is.null(timeseries_id)) {
        return(class_ranges[[class_name]])
      }
      ranges <- target_class_ranges[[class_name]]
      out <- ranges[[as.character(as.integer(timeseries_id))]]
      if (is.null(out)) {
        out <- empty_class_range_df()
      }
      out
    }

    set_class_ranges <- function(class_name, value, timeseries_id = NULL) {
      if (class_apply_all(class_name) || is.null(timeseries_id)) {
        class_ranges[[class_name]] <- value
        return(invisible(NULL))
      }
      ranges <- target_class_ranges[[class_name]]
      ranges[[as.character(as.integer(timeseries_id))]] <- value
      target_class_ranges[[class_name]] <- ranges
      invisible(NULL)
    }

    active_class_ranges <- function(class_name) {
      get_class_ranges(class_name, active_class_timeseries(class_name))
    }

    ensure_class_cols <- function() {
      for (nm in c("grade", "approval", "qualifier")) {
        if (!(nm %in% names(data$df))) {
          data$df[[nm]] <- ""
        }
      }
    }

    code_to_desc <- function(class_name, code) {
      types <- class_type_choices()[[class_name]]
      idx <- match(as.character(code), as.character(types$code))
      ifelse(is.na(idx), "", as.character(types$description[idx]))
    }

    ranges_from_table_classes <- function(df, class_name) {
      if (!(class_name %in% names(df)) || nrow(df) == 0) {
        return(class_ranges[[class_name]][0, , drop = FALSE])
      }
      dt <- table_datetimes_to_utc(df$datetime, input$UTC_offset)
      vals <- trimws(as.character(df[[class_name]]))
      ok <- !is.na(dt) & nzchar(vals)
      if (!any(ok)) {
        return(class_ranges[[class_name]][0, , drop = FALSE])
      }
      x <- data.table::data.table(datetime = dt[ok], code = vals[ok])
      data.table::setorder(x, datetime)
      x[, run := data.table::rleid(code)]
      out <- x[,
        .(
          code = data.table::first(code),
          start_datetime = format(min(datetime), "%Y-%m-%d %H:%M:%S"),
          end_datetime = format(max(datetime), "%Y-%m-%d %H:%M:%S")
        ),
        by = run
      ]
      out[, description := code_to_desc(class_name, code)]
      out[, run := NULL]
      as.data.frame(out[, .(code, description, start_datetime, end_datetime)])
    }

    normalize_class_ranges <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          code = character(),
          start_datetime = character(),
          end_datetime = character(),
          stringsAsFactors = FALSE
        ))
      }
      out <- data.frame(
        code = as.character(df$code),
        start_datetime = as.character(df$start_datetime),
        end_datetime = as.character(df$end_datetime),
        stringsAsFactors = FALSE
      )
      out <- out[order(out$start_datetime, out$end_datetime, out$code), ]
      row.names(out) <- NULL
      out
    }

    class_ranges_identical <- function(a, b) {
      identical(normalize_class_ranges(a), normalize_class_ranges(b))
    }

    stored_or_data_target_class_ranges <- function(class_name, timeseries_id) {
      ranges <- target_class_ranges[[class_name]][[
        as.character(as.integer(timeseries_id))
      ]]
      job <- upload_review_job(timeseries_id)
      if (
        !is.null(job) &&
          class_name %in% names(job$data) &&
          (is.null(ranges) || nrow(ranges) == 0)
      ) {
        ranges <- ranges_from_table_classes(job$data, class_name)
      }
      if (is.null(ranges)) {
        ranges <- empty_class_range_df()
      }
      ranges
    }

    target_class_range_list <- function(class_name) {
      targets <- selected_upload_timeseries_meta()
      if (nrow(targets) == 0) {
        return(list())
      }
      stats::setNames(
        lapply(
          targets$timeseries_id,
          function(target_id) {
            stored_or_data_target_class_ranges(class_name, target_id)
          }
        ),
        as.character(as.integer(targets$timeseries_id))
      )
    }

    target_class_ranges_disagree <- function(class_name) {
      ranges <- target_class_range_list(class_name)
      if (length(ranges) <= 1) {
        return(FALSE)
      }
      first <- ranges[[1]]
      any(vapply(
        ranges[-1],
        function(x) !class_ranges_identical(first, x),
        logical(1)
      ))
    }

    promote_target_ranges_to_shared <- function(class_name) {
      ranges <- target_class_range_list(class_name)
      if (length(ranges) == 0) {
        class_ranges[[class_name]] <- empty_class_range_df()
        return(invisible(NULL))
      }
      common_ranges <- ranges[[1]]
      common_ranges$description <- code_to_desc(class_name, common_ranges$code)
      class_ranges[[class_name]] <- common_ranges
      invisible(NULL)
    }

    populate_target_ranges_from_table_data <- function(class_name) {
      jobs <- upload_review_jobs()
      ranges <- target_class_ranges[[class_name]]
      for (job in jobs) {
        target_id <- as.character(as.integer(job$timeseries_id))
        ranges[[target_id]] <- ranges_from_table_classes(job$data, class_name)
      }
      target_class_ranges[[class_name]] <- ranges
      invisible(NULL)
    }

    clear_class_ranges_for_all_targets <- function(class_name) {
      targets <- selected_upload_timeseries_meta()
      ranges <- list()
      for (target_id in targets$timeseries_id) {
        ranges[[as.character(as.integer(target_id))]] <- empty_class_range_df()
      }
      target_class_ranges[[class_name]] <- ranges
      class_ranges[[class_name]] <- empty_class_range_df()
      invisible(NULL)
    }

    validate_ranges <- function(df, class_name) {
      if (nrow(df) == 0) {
        return(character())
      }
      sdt <- parse_utc_datetime_value(df$start_datetime)
      edt <- parse_utc_datetime_value(df$end_datetime)
      msgs <- character()
      bad <- which(is.na(sdt) | is.na(edt) | edt < sdt)
      if (length(bad) > 0) {
        msgs <- c(
          msgs,
          sprintf(
            "Invalid start/end datetime row(s): %s.",
            paste(bad, collapse = ", ")
          )
        )
      }
      valid <- which(!(is.na(sdt) | is.na(edt) | edt < sdt))
      if (length(valid) > 1 && class_name %in% c("grade", "approval")) {
        o <- order(sdt[valid], edt[valid])
        v <- valid[o]
        for (i in seq_along(v)[-1]) {
          if (sdt[v[i]] <= edt[v[i - 1]]) {
            msgs <- c(
              msgs,
              sprintf(
                "Overlapping or touching %s ranges in rows %s and %s.",
                class_name,
                v[i - 1],
                v[i]
              )
            )
          }
        }
      }
      unique(msgs)
    }

    class_range_validation_messages <- function(class_name) {
      if (class_apply_all(class_name) || !isTRUE(multi_upload_active())) {
        return(validate_ranges(class_ranges[[class_name]], class_name))
      }
      targets <- selected_upload_timeseries_meta()
      msgs <- character()
      for (i in seq_len(nrow(targets))) {
        target_id <- as.integer(targets$timeseries_id[[i]])
        target_msgs <- validate_ranges(
          get_class_ranges(class_name, target_id),
          class_name
        )
        if (length(target_msgs) > 0) {
          msgs <- c(
            msgs,
            paste0(target_label(targets[i, , drop = FALSE]), ": ",
                   paste(target_msgs, collapse = " "))
          )
        }
      }
      msgs
    }

    ranges_valid <- reactive({
      vapply(
        c("grade", "approval", "qualifier"),
        function(nm) {
          length(class_range_validation_messages(nm)) == 0
        },
        logical(1)
      )
    })

    observe({
      if (!can_insert) {
        shinyjs::disable("upload")
        shinyjs::disable("upload_overwrite_all")
        shinyjs::disable("upload_overwrite_some")
        return()
      }
      if (!isTRUE(selected_timeseries_is_basic())) {
        shinyjs::disable("upload")
        shinyjs::disable("upload_overwrite_all")
        shinyjs::disable("upload_overwrite_some")
        return()
      }
      if (all(ranges_valid())) {
        shinyjs::enable("upload")
        shinyjs::enable("upload_overwrite_all")
        shinyjs::enable("upload_overwrite_some")
      } else {
        shinyjs::disable("upload")
        shinyjs::disable("upload_overwrite_all")
        shinyjs::disable("upload_overwrite_some")
      }
    })

    output$grade_ranges_warning <- renderUI({
      msgs <- class_range_validation_messages("grade")
      if (length(msgs) == 0) {
        return(NULL)
      }
      div(style = "color:#b30000;", paste(msgs, collapse = " "))
    })
    output$approval_ranges_warning <- renderUI({
      msgs <- class_range_validation_messages("approval")
      if (length(msgs) == 0) {
        return(NULL)
      }
      div(style = "color:#b30000;", paste(msgs, collapse = " "))
    })
    output$qualifier_ranges_warning <- renderUI({
      msgs <- class_range_validation_messages("qualifier")
      if (length(msgs) == 0) {
        return(NULL)
      }
      div(style = "color:#b30000;", paste(msgs, collapse = " "))
    })

    apply_class_ranges_to_df <- function(df, ranges_by_class) {
      if (nrow(df) == 0) {
        return(df)
      }
      new_df <- df
      for (nm in c("grade", "approval", "qualifier")) {
        if (!(nm %in% names(new_df))) {
          new_df[[nm]] <- ""
        }
      }
      dt <- table_datetimes_to_utc(new_df$datetime, input$UTC_offset)
      if (!any(!is.na(dt))) {
        return(new_df)
      }
      for (nm in c("grade", "approval", "qualifier")) {
        new_df[[nm]] <- ""
        rr <- ranges_by_class[[nm]]
        if (nrow(rr) == 0) {
          next
        }
        sdt <- parse_utc_datetime_value(rr$start_datetime)
        edt <- parse_utc_datetime_value(rr$end_datetime)
        for (i in seq_len(nrow(rr))) {
          if (is.na(sdt[i]) || is.na(edt[i])) {
            next
          }
          idx <- which(!is.na(dt) & dt >= sdt[i] & dt <= edt[i])
          if (length(idx) == 0) {
            next
          }
          if (nm == "qualifier") {
            existing <- trimws(as.character(new_df[[nm]][idx]))
            new_df[[nm]][idx] <- ifelse(
              nzchar(existing),
              paste0(existing, ";", rr$code[i]),
              rr$code[i]
            )
          } else {
            new_df[[nm]][idx] <- rr$code[i]
          }
        }
      }
      new_df
    }

    class_ranges_for_target <- function(timeseries_id) {
      list(
        grade = get_class_ranges("grade", timeseries_id),
        approval = get_class_ranges("approval", timeseries_id),
        qualifier = get_class_ranges("qualifier", timeseries_id)
      )
    }

    sync_table_classes_from_ranges <- function() {
      jobs <- upload_review_jobs()
      if (length(jobs) == 0) {
        if (nrow(data$df) == 0) {
          return()
        }
        new_df <- apply_class_ranges_to_df(
          data$df,
          class_ranges_for_target(timeseries())
        )
        if (!isTRUE(all.equal(data$df, new_df, check.attributes = FALSE))) {
          data$df <- new_df
          refresh_data_table()
        }
        return()
      }

      for (job in jobs) {
        target_id <- as.integer(job$timeseries_id)
        new_df <- apply_class_ranges_to_df(
          job$data,
          class_ranges_for_target(target_id)
        )
        set_upload_job_data(target_id, new_df)
      }
    }

    range_table <- function(class_name, timeseries_id = NULL) {
      out <- get_class_ranges(class_name, timeseries_id)
      out$description <- code_to_desc(class_name, out$code)
      if (nrow(out) > 0) {
        tz_name <- class_offset_tz(class_name)
        out$start_datetime <- format_utc_datetimes_for_display(
          out$start_datetime,
          tz_name
        )
        out$end_datetime <- format_utc_datetimes_for_display(
          out$end_datetime,
          tz_name
        )
      }
      out
    }

    render_range_table <- function(class_name, timeseries_id = NULL) {
      DT::datatable(
        range_table(class_name, timeseries_id),
        selection = "single",
        rownames = FALSE,
        options = list(scrollX = TRUE)
      )
    }

    output$grade_ranges_table <- DT::renderDT(
      {
        render_range_table("grade")
      },
      server = FALSE
    )
    output$approval_ranges_table <- DT::renderDT(
      {
        render_range_table("approval")
      },
      server = FALSE
    )
    output$qualifier_ranges_table <- DT::renderDT(
      {
        render_range_table("qualifier")
      },
      server = FALSE
    )

    suppress_class_apply_all_observer <- reactiveValues(
      grade = FALSE,
      approval = FALSE,
      qualifier = FALSE
    )

    show_apply_all_reset_modal <- function(class_name) {
      label <- tools::toTitleCase(class_name)
      showModal(modalDialog(
        title = paste("Apply", class_name, "to all timeseries?"),
        tags$p(
          paste(
            label,
            "ranges are not currently the same for every selected timeseries."
          )
        ),
        tags$p(
          paste(
            "Switching to 'Apply to all timeseries' will erase the already",
            "applied",
            paste0(class_name, "s"),
            "for all selected timeseries."
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns(paste0("confirm_apply_all_", class_name)),
            paste("Erase", paste0(class_name, "s"), "and apply to all"),
            class = "btn-danger"
          )
        )
      ))
    }

    for (nm in c("grade", "approval", "qualifier")) {
      local({
        class_name <- nm
        output[[paste0(class_name, "_apply_all_ui")]] <- renderUI({
          if (!isTRUE(multi_upload_active())) {
            return(NULL)
          }
          checkboxInput(
            ns(class_apply_all_input_id(class_name)),
            "Apply to all timeseries",
            value = checkbox_current_value(
              class_apply_all_input_id(class_name),
              class_apply_all_default(class_name)
            )
          )
        })

        output[[paste0(class_name, "_ranges_ui")]] <- renderUI({
          targets <- selected_upload_timeseries_meta()
          if (!isTRUE(multi_upload_active()) || nrow(targets) <= 1) {
            return(DT::DTOutput(ns(paste0(class_name, "_ranges_table"))))
          }
          if (class_apply_all(class_name)) {
            return(tagList(
              tags$p(
                class = "text-muted small",
                paste(
                  tools::toTitleCase(class_name),
                  "ranges will be applied to every selected upload target."
                )
              ),
              DT::DTOutput(ns(paste0(class_name, "_ranges_table")))
            ))
          }
          tabs <- lapply(seq_len(nrow(targets)), function(i) {
            target <- targets[i, , drop = FALSE]
            target_id <- as.integer(target$timeseries_id[[1]])
            tabPanel(
              title = target_label(target),
              value = paste0("timeseries_", target_id),
              DT::DTOutput(ns(class_range_output_id(class_name, target_id)))
            )
          })
          do.call(
            tabsetPanel,
            c(list(id = ns(class_ranges_tabset_id(class_name))), tabs)
          )
        })
        observeEvent(input[[class_apply_all_input_id(class_name)]], {
          if (isTRUE(suppress_class_apply_all_observer[[class_name]])) {
            suppress_class_apply_all_observer[[class_name]] <- FALSE
            sync_table_classes_from_ranges()
            update_class_modal_datetime_limits(class_name)
            return()
          }

          apply_all <- isTRUE(input[[class_apply_all_input_id(class_name)]])
          if (isTRUE(apply_all)) {
            if (target_class_ranges_disagree(class_name)) {
              updateCheckboxInput(
                session,
                class_apply_all_input_id(class_name),
                value = FALSE
              )
              show_apply_all_reset_modal(class_name)
              return()
            }
            promote_target_ranges_to_shared(class_name)
          } else {
            populate_target_ranges_from_table_data(class_name)
          }

          sync_table_classes_from_ranges()
          update_class_modal_datetime_limits(class_name)
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0("confirm_apply_all_", class_name)]], {
          removeModal()
          clear_class_ranges_for_all_targets(class_name)
          sync_table_classes_from_ranges()
          suppress_class_apply_all_observer[[class_name]] <- TRUE
          updateCheckboxInput(
            session,
            class_apply_all_input_id(class_name),
            value = TRUE
          )
        }, ignoreInit = TRUE)
      })
    }

    observe({
      targets <- selected_upload_timeseries_meta()
      if (nrow(targets) == 0) {
        return()
      }
      for (class_name in c("grade", "approval", "qualifier")) {
        for (target_id in targets$timeseries_id) {
          local({
            class_name_local <- class_name
            target_id_local <- as.integer(target_id)
            output_id <- class_range_output_id(
              class_name_local,
              target_id_local
            )
            output[[output_id]] <- DT::renderDT(
              {
                render_range_table(class_name_local, target_id_local)
              },
              server = FALSE
            )
          })
        }
      }
    })

    class_modal_bounds <- function(class_name) {
      tz_name <- class_offset_tz(class_name)
      if (!isTRUE(multi_upload_active()) || !class_apply_all(class_name)) {
        return(uploaded_data_bounds(
          tz_name,
          active_job_data(active_class_timeseries(class_name))
        ))
      }

      jobs <- upload_review_jobs()
      if (length(jobs) == 0) {
        return(NULL)
      }

      target_rows <- lapply(jobs, function(job) {
        bounds <- uploaded_data_bounds(tz_name, job$data)
        if (is.null(bounds)) {
          return(data.frame(
            target = job$label,
            start = "No valid datetimes",
            end = "No valid datetimes",
            start_utc = NA_real_,
            end_utc = NA_real_,
            stringsAsFactors = FALSE
          ))
        }
        data.frame(
          target = job$label,
          start = bounds$start_display,
          end = bounds$end_display,
          start_utc = as.numeric(bounds$start_utc),
          end_utc = as.numeric(bounds$end_utc),
          stringsAsFactors = FALSE
        )
      })
      per_target <- do.call(rbind, target_rows)
      valid <- !is.na(per_target$start_utc) & !is.na(per_target$end_utc)
      if (!any(valid)) {
        return(NULL)
      }

      tz_name <- selected_offset_tz(tz_name, default = input$UTC_offset)
      start_utc <- as.POSIXct(
        min(per_target$start_utc[valid], na.rm = TRUE),
        origin = "1970-01-01",
        tz = "UTC"
      )
      end_utc <- as.POSIXct(
        max(per_target$end_utc[valid], na.rm = TRUE),
        origin = "1970-01-01",
        tz = "UTC"
      )

      list(
        start_utc = start_utc,
        end_utc = end_utc,
        start_display = format_utc_datetimes_for_display(start_utc, tz_name),
        end_display = format_utc_datetimes_for_display(end_utc, tz_name),
        tz = tz_name,
        per_target = per_target[, c("target", "start", "end"), drop = FALSE]
      )
    }

    class_modal_date_options <- function(bounds) {
      if (is.null(bounds)) {
        return(list())
      }
      list(
        minDate = bounds$start_utc,
        maxDate = bounds$end_utc
      )
    }

    class_modal_datetime_value <- function(class_name, input_id) {
      scalar_display_datetime_to_utc(
        input[[input_id]],
        class_offset_tz(class_name)
      )
    }

    update_class_modal_datetime_limits <- function(class_name) {
      bounds <- class_modal_bounds(class_name)
      if (is.null(bounds)) {
        return(invisible(NULL))
      }

      update_one <- function(input_id, fallback) {
        current_value <- class_modal_datetime_value(class_name, input_id)
        if (is.na(current_value)) {
          current_value <- fallback
        }
        current_value <- min(
          max(current_value, bounds$start_utc),
          bounds$end_utc
        )
        shinyWidgets::updateAirDateInput(
          session,
          inputId = input_id,
          value = current_value,
          tz = air_datetime_widget_timezone(bounds$tz),
          options = class_modal_date_options(bounds)
        )
      }

      update_one(paste0(class_name, "_modal_start"), bounds$start_utc)
      update_one(paste0(class_name, "_modal_end"), bounds$end_utc)
      invisible(NULL)
    }

    range_inside_data_bounds <- function(st, en, bounds) {
      if (is.null(bounds)) {
        return(FALSE)
      }
      !is.na(st) &&
        !is.na(en) &&
        st >= bounds$start_utc &&
        en <= bounds$end_utc
    }

    open_range_modal <- function(
      class_name,
      mode = c("add", "edit"),
      row_idx = NULL
    ) {
      mode <- match.arg(mode)
      rows <- active_class_ranges(class_name)
      edit_row <- if (
        mode == "edit" && !is.null(row_idx) && nrow(rows) >= row_idx
      ) {
        rows[row_idx, ]
      } else {
        data.frame(code = "", start_datetime = "", end_datetime = "")
      }
      bounds <- class_modal_bounds(class_name)
      start_value <- if (nzchar(edit_row$start_datetime)) {
        scalar_utc_datetime_value(edit_row$start_datetime)
      } else if (!is.null(bounds)) {
        bounds$start_utc
      } else {
        NULL
      }
      end_value <- if (nzchar(edit_row$end_datetime)) {
        scalar_utc_datetime_value(edit_row$end_datetime)
      } else if (!is.null(bounds)) {
        bounds$end_utc
      } else {
        NULL
      }
      types <- class_type_choices()[[class_name]]
      showModal(modalDialog(
        title = paste(
          ifelse(mode == "add", "Add", "Edit"),
          class_name,
          "range"
        ),
        selectizeInput(
          ns(paste0(class_name, "_modal_code")),
          "Level",
          choices = stats::setNames(
            types$code,
            paste0(types$code, ": ", types$description)
          ),
          selected = edit_row$code,
          multiple = FALSE
        ),
        shinyWidgets::airDatepickerInput(
          ns(paste0(class_name, "_modal_start")),
          "Start datetime",
          value = start_value,
          range = FALSE,
          multiple = FALSE,
          timepicker = TRUE,
          update_on = "change",
          tz = air_datetime_widget_timezone(isolate(class_offset_tz(
            class_name
          ))),
          minDate = if (!is.null(bounds)) bounds$start_utc else NULL,
          maxDate = if (!is.null(bounds)) bounds$end_utc else NULL,
          timepickerOpts = shinyWidgets::timepickerOptions(
            minutesStep = 15,
            timeFormat = "HH:mm"
          )
        ),
        shinyWidgets::airDatepickerInput(
          ns(paste0(class_name, "_modal_end")),
          "End datetime",
          value = end_value,
          range = FALSE,
          multiple = FALSE,
          timepicker = TRUE,
          update_on = "change",
          tz = air_datetime_widget_timezone(isolate(class_offset_tz(
            class_name
          ))),
          minDate = if (!is.null(bounds)) bounds$start_utc else NULL,
          maxDate = if (!is.null(bounds)) bounds$end_utc else NULL,
          timepickerOpts = shinyWidgets::timepickerOptions(
            minutesStep = 15,
            timeFormat = "HH:mm"
          )
        ),
        uploaded_data_bounds_ui(class_name),
        tags$div(
          class = "d-flex gap-2 flex-wrap mb-3",
          actionButton(
            ns(paste0(class_name, "_modal_use_data_start")),
            "Use data start"
          ),
          actionButton(
            ns(paste0(class_name, "_modal_use_data_end")),
            "Use data end"
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(paste0("save_", class_name, "_modal")), "Save")
        )
      ))
      session$userData[[paste0("edit_", class_name, "_row")]] <- row_idx
    }

    for (nm in c("grade", "approval", "qualifier")) {
      local({
        class_name <- nm
        observeEvent(input[[paste0("add_", class_name, "_range")]], {
          open_range_modal(class_name, "add")
        })
        observeEvent(input[[paste0("edit_", class_name, "_range")]], {
          target_id <- active_class_timeseries(class_name)
          table_id <- if (
            isTRUE(multi_upload_active()) && !class_apply_all(class_name)
          ) {
            class_range_output_id(class_name, target_id)
          } else {
            paste0(class_name, "_ranges_table")
          }
          idx <- input[[paste0(table_id, "_rows_selected")]]
          req(length(idx) == 1)
          open_range_modal(class_name, "edit", idx[[1]])
        })
        observeEvent(input[[paste0("delete_", class_name, "_range")]], {
          target_id <- active_class_timeseries(class_name)
          table_id <- if (
            isTRUE(multi_upload_active()) && !class_apply_all(class_name)
          ) {
            class_range_output_id(class_name, target_id)
          } else {
            paste0(class_name, "_ranges_table")
          }
          idx <- input[[paste0(table_id, "_rows_selected")]]
          req(length(idx) == 1)
          rows <- active_class_ranges(class_name)
          set_class_ranges(class_name, rows[
            -idx[[1]],
            ,
            drop = FALSE
          ], target_id)
          sync_table_classes_from_ranges()
        })
        observeEvent(input[[paste0(class_name, "_modal_use_data_start")]], {
          bounds <- class_modal_bounds(class_name)
          if (is.null(bounds)) {
            return()
          }
          shinyWidgets::updateAirDateInput(
            session,
            inputId = paste0(class_name, "_modal_start"),
            value = bounds$start_utc,
            tz = air_datetime_widget_timezone(bounds$tz),
            options = class_modal_date_options(bounds)
          )
        })
        observeEvent(input[[paste0(class_name, "_modal_use_data_end")]], {
          bounds <- class_modal_bounds(class_name)
          if (is.null(bounds)) {
            return()
          }
          shinyWidgets::updateAirDateInput(
            session,
            inputId = paste0(class_name, "_modal_end"),
            value = bounds$end_utc,
            tz = air_datetime_widget_timezone(bounds$tz),
            options = class_modal_date_options(bounds)
          )
        })
        observeEvent(input[[paste0("save_", class_name, "_modal")]], {
          code <- as.character(input[[paste0(class_name, "_modal_code")]])
          st_value <- class_modal_datetime_value(
            class_name,
            paste0(class_name, "_modal_start")
          )
          en_value <- class_modal_datetime_value(
            class_name,
            paste0(class_name, "_modal_end")
          )
          bounds <- class_modal_bounds(class_name)

          if (
            is.na(st_value) ||
              is.na(en_value) ||
              en_value < st_value
          ) {
            showNotification(
              paste("Invalid", class_name, "start/end datetime."),
              type = "error",
              duration = 8
            )
            return()
          }

          if (!range_inside_data_bounds(st_value, en_value, bounds)) {
            showNotification(
              paste(
                "The",
                class_name,
                "range must stay within the uploaded/entered data range."
              ),
              type = "error",
              duration = 8
            )
            return()
          }

          st <- if (length(st_value) && !is.na(st_value[[1]])) {
            format(st_value[[1]], "%Y-%m-%d %H:%M:%S", tz = "UTC")
          } else {
            ""
          }
          en <- if (length(en_value) && !is.na(en_value[[1]])) {
            format(en_value[[1]], "%Y-%m-%d %H:%M:%S", tz = "UTC")
          } else {
            ""
          }
          new_row <- data.frame(
            code = code,
            description = code_to_desc(class_name, code),
            start_datetime = st,
            end_datetime = en,
            stringsAsFactors = FALSE
          )
          target_id <- active_class_timeseries(class_name)
          rows <- active_class_ranges(class_name)
          edit_idx <- session$userData[[paste0("edit_", class_name, "_row")]]
          if (
            !is.null(edit_idx) && !is.na(edit_idx) && nrow(rows) >= edit_idx
          ) {
            rows[edit_idx, ] <- new_row
          } else {
            rows <- rbind(rows, new_row)
          }

          range_msgs <- validate_ranges(rows, class_name)
          if (length(range_msgs) > 0) {
            showNotification(
              paste(range_msgs, collapse = " "),
              type = "error",
              duration = 10
            )
            return()
          }

          set_class_ranges(class_name, rows, target_id)
          removeModal()
          sync_table_classes_from_ranges()
        })
      })
    }

    output$map_modal_body <- renderUI({
      if (map_modal_state$step == "columns") {
        tagList(
          'Identify which columns represent date-time and value (and optionally grade/approval/qualifier)',
          br(),
          'Hint: if this file contains more than one timeseries, go back to the timeseries selection menu and select all applicable timeseries before uploading the file.',
          hr(),
          if (uploaded_file_is_logger()) {
            div(
              class = "alert alert-info",
              style = "padding: 8px; margin-bottom: 10px;",
              logger_upload_message()
            )
          },
          if (!uploaded_file_is_logger()) {
            tagList(
              numericInput(
                ns('raw_start_row'),
                label = 'Header Row',
                value = 1
              ) |>
                tooltip(
                  "The row number which contains your data's column names"
                )
            )
          },
          tags$div(
            style = "font-size: 11px; line-height: 1.15; margin-bottom: 10px;",
            DT::DTOutput(ns("raw_file_preview"))
          ),
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
            as.character(db_df$code),
            paste0(db_df$code, ": ", db_df$description)
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
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Select class to map to"
              )
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

    # Store modal to be shown upon user uploading a tabular or logger file
    map_col_modal <- modalDialog(
      title = 'Identify columns',
      uiOutput(ns('map_modal_body')),
      easyClose = FALSE,
      size = "xl",
      footer = uiOutput(ns('map_modal_footer'))
    )

    # Show modal when user adds file
    observeEvent(input$file, {
      req(input$file)
      map_modal_state$step <- "columns"
      map_modal_state$pending_df <- NULL
      map_modal_state$pending_jobs <- NULL
      upload_jobs(NULL)
      upload_validation$jobs <- NULL
      map_modal_state$class_values <- list(
        grade = character(),
        approval = character(),
        qualifier = character()
      )
      if (
        tolower(tools::file_ext(input$file$name)) %in% c("xle", "html", "htm", "hobo")
      ) {
        updateSelectizeInput(
          session,
          "UTC_offset",
          selected = format_utc_offset(0L)
        )
        showNotification(
          logger_upload_message(),
          type = "message",
          duration = 8
        )
      }
      showModal(map_col_modal)
    })

    observeEvent(input$next_mapping, {
      jobs <- build_upload_jobs_from_column_mapping()
      map_modal_state$pending_jobs <- jobs
      map_modal_state$pending_df <- jobs[[1]]$data
      sanitize_class_vals <- function(x) {
        vals <- trimws(as.character(x))
        sort(unique(vals[!is.na(vals) & nzchar(vals)]))
      }
      job_values <- function(class_name) {
        unlist(
          lapply(jobs, function(job) {
            if (class_name %in% names(job$data)) {
              job$data[[class_name]]
            } else {
              character()
            }
          }),
          use.names = FALSE
        )
      }
      map_modal_state$class_values <- list(
        grade = sanitize_class_vals(job_values("grade")),
        approval = sanitize_class_vals(job_values("approval")),
        qualifier = sanitize_class_vals(job_values("qualifier"))
      )
      map_modal_state$step <- "class_mapping"
    })

    observeEvent(
      input$confirm_mapping,
      {
        removeModal() # Close the modal dialog
        jobs <- if (map_modal_state$step == "class_mapping") {
          req(map_modal_state$pending_jobs)
          out_jobs <- map_modal_state$pending_jobs

          for (class_name in names(map_modal_state$class_values)) {
            class_vals <- map_modal_state$class_values[[class_name]]
            if (length(class_vals) == 0) {
              next
            }

            mapped_values <- vapply(
              seq_along(class_vals),
              function(i) {
                as.character(input[[paste0("map_", class_name, "_", i)]])
              },
              character(1)
            )
            names(mapped_values) <- class_vals

            for (job_idx in seq_along(out_jobs)) {
              if (!(class_name %in% names(out_jobs[[job_idx]]$data))) {
                next
              }
              current_vals <- trimws(as.character(out_jobs[[job_idx]]$data[[
                class_name
              ]]))
              non_missing <- !is.na(current_vals) & nzchar(current_vals)
              out_jobs[[job_idx]]$data[[class_name]][non_missing] <- unname(
                mapped_values[current_vals[non_missing]]
              )
            }
          }
          out_jobs
        } else {
          build_upload_jobs_from_column_mapping()
        }

        upload_jobs(jobs)
        df_mapped <- jobs[[1]]$data
        unit_conversion_state$previous_values <- NULL
        unit_conversion_state$previous_label <- NULL
        data$df <- prepare_table_data(df_mapped)
        if ("grade" %in% names(df_mapped)) {
          data$df$grade <- as.character(df_mapped$grade)
        }
        if ("approval" %in% names(df_mapped)) {
          data$df$approval <- as.character(df_mapped$approval)
        }
        if ("qualifier" %in% names(df_mapped)) {
          data$df$qualifier <- as.character(df_mapped$qualifier)
        }
        for (class_name in c("grade", "approval", "qualifier")) {
          ranges_by_target <- list()
          for (job in jobs) {
            ranges_by_target[[as.character(as.integer(job$timeseries_id))]] <-
              ranges_from_table_classes(job$data, class_name)
          }
          target_class_ranges[[class_name]] <- ranges_by_target
        }
        class_ranges$grade <- ranges_from_table_classes(data$df, "grade")
        class_ranges$approval <- ranges_from_table_classes(data$df, "approval")
        class_ranges$qualifier <- ranges_from_table_classes(
          data$df,
          "qualifier"
        )
        sync_table_classes_from_ranges()
        refresh_data_table()
      },
      ignoreInit = TRUE
    )

    new_data_row <- function() {
      data.frame(
        datetime = format_utc_datetimes_for_display(
          coerce_utc_datetime(Sys.time()),
          selected_offset_tz(input$UTC_offset)
        )[[1]],
        value = NA_real_,
        grade = "",
        approval = "",
        qualifier = "",
        stringsAsFactors = FALSE
      )
    }

    add_row_relative_to_selection <- function(position = c("above", "below")) {
      position <- match.arg(position)
      if (nrow(data$df) == 0) {
        data$df <- new_data_row()
        refresh_data_table()
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
      refresh_data_table()
    }

    observeEvent(input$add_row, {
      if (!identical(input$entry_mode, "manual")) {
        return()
      }
      data$df <- rbind(data$df, new_data_row())
      refresh_data_table()
    })

    observeEvent(input$add_row_above, {
      add_row_relative_to_selection("above")
    })

    observeEvent(input$add_row_below, {
      add_row_relative_to_selection("below")
    })

    observeEvent(
      list(input$delete_rows_table, input$delete_rows_accordion),
      {
        click_count <- sum(
          as.numeric(c(input$delete_rows_table, input$delete_rows_accordion)),
          na.rm = TRUE
        )
        if (click_count < 1) {
          return()
        }

        if (!identical(input$entry_mode, "manual")) {
          showNotification(
            "Selected row deletion is available for manual entry only.",
            type = "message"
          )
          return()
        }
        req(input$data_table_rows_selected)
        data$df <- data$df[-input$data_table_rows_selected, , drop = FALSE]
        refresh_data_table()
      },
      ignoreInit = TRUE
    )

    output$delete_target_ui <- renderUI({
      jobs <- upload_review_jobs()
      if (!isTRUE(multi_upload_active()) || length(jobs) <= 1) {
        return(NULL)
      }
      choices <- stats::setNames(
        as.character(vapply(
          jobs,
          function(job) as.integer(job$timeseries_id),
          integer(1)
        )),
        vapply(jobs, `[[`, character(1), "label")
      )
      selectizeInput(
        ns("delete_timeseries_id"),
        "Apply deletion to",
        choices = choices,
        selected = choices[[1]],
        multiple = FALSE,
        width = "100%"
      )
    })

    selected_delete_timeseries <- function() {
      if (!isTRUE(multi_upload_active())) {
        return(as.integer(timeseries()))
      }
      selected <- input$delete_timeseries_id
      if (is.null(selected) || !nzchar(selected)) {
        jobs <- upload_review_jobs()
        if (length(jobs) == 0) {
          return(as.integer(timeseries()))
        }
        return(as.integer(jobs[[1]]$timeseries_id))
      }
      as.integer(selected)
    }

    apply_datetime_cutoff <- function(mode = c("before", "after")) {
      mode <- match.arg(mode)
      target_id <- selected_delete_timeseries()
      target_df <- active_job_data(target_id)
      if (nrow(target_df) == 0) {
        showNotification("No rows to delete.", type = "message")
        return(invisible(NULL))
      }

      cutoff <- scalar_utc_datetime(input$delete_cutoff_datetime)
      if (is.na(cutoff)) {
        showNotification(
          "Invalid cutoff datetime.",
          type = "error"
        )
        return(invisible(NULL))
      }

      parsed_dt <- table_datetimes_to_utc(target_df$datetime, input$UTC_offset)
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
      set_upload_job_data(target_id, target_df[keep_idx, , drop = FALSE])
      target_job <- upload_review_job(target_id)
      target_label_text <- if (is.null(target_job)) {
        paste("timeseries", target_id)
      } else {
        target_job$label
      }
      showNotification(
        sprintf("Removed %s row(s) from %s.", removed_n, target_label_text),
        type = "message"
      )
    }

    observeEvent(input$delete_before_datetime, {
      apply_datetime_cutoff("before")
    })

    observeEvent(input$delete_after_datetime, {
      apply_datetime_cutoff("after")
    })

    class_code_choices <- function(class_name) {
      types <- class_type_choices()[[class_name]]
      out <- as.character(types$code)
      stats::setNames(
        c("", out),
        c("", paste0(out, " - ", types$description))
      )
    }

    table_cell_input <- function(row, col, value) {
      value <- if (is.na(value)) "" else as.character(value)
      escaped_value <- htmltools::htmlEscape(value, attribute = TRUE)

      if (col %in% c("grade", "approval", "qualifier")) {
        choices <- class_code_choices(col)
        if (!(value %in% unname(choices))) {
          value <- ""
        }
        options <- paste0(
          vapply(
            seq_along(choices),
            function(i) {
              selected <- if (identical(unname(choices)[[i]], value)) {
                " selected"
              } else {
                ""
              }
              paste0(
                "<option value=\"",
                htmltools::htmlEscape(unname(choices)[[i]], attribute = TRUE),
                "\"",
                selected,
                ">",
                htmltools::htmlEscape(names(choices)[[i]]),
                "</option>"
              )
            },
            character(1)
          ),
          collapse = ""
        )
        return(paste0(
          "<select class=\"cont-data-cell cont-data-select\" style=\"width:100%;min-width:110px;box-sizing:border-box;\" data-row=\"",
          row,
          "\" data-col=\"",
          col,
          "\">",
          options,
          "</select>"
        ))
      }

      input_type <- if (identical(col, "value")) "number" else "text"
      step_attr <- if (identical(col, "value")) " step=\"any\"" else ""
      paste0(
        "<input class=\"cont-data-cell\" style=\"width:100%;min-width:110px;box-sizing:border-box;\" type=\"",
        input_type,
        "\"",
        step_attr,
        " data-row=\"",
        row,
        "\" data-col=\"",
        col,
        "\" value=\"",
        escaped_value,
        "\">"
      )
    }

    table_state_from_df <- function(df, editable) {
      if (nrow(df) == 0) {
        return(list(
          data = df,
          editable = editable,
          preview = FALSE
        ))
      }

      if (!isTRUE(editable)) {
        preview_rows_per_end <- 10L
        total_rows <- nrow(df)
        row_idx <- if (total_rows <= preview_rows_per_end * 2L) {
          seq_len(total_rows)
        } else {
          c(
            seq_len(preview_rows_per_end),
            seq.int(total_rows - preview_rows_per_end + 1L, total_rows)
          )
        }

        preview_df <- df[row_idx, , drop = FALSE]
        preview_df <- data.frame(
          row = row_idx,
          preview_df,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        return(list(
          data = preview_df,
          editable = FALSE,
          preview = total_rows > nrow(preview_df),
          total_rows = total_rows,
          displayed_rows = nrow(preview_df)
        ))
      }

      for (col in names(df)) {
        df[[col]] <- vapply(
          seq_len(nrow(df)),
          function(i) table_cell_input(i, col, df[[col]][[i]]),
          character(1)
        )
      }
      list(
        data = df,
        editable = TRUE,
        preview = FALSE
      )
    }

    display_table_data <- reactive({
      table_render_tick()
      df <- isolate(data$df)
      table_state_from_df(df, identical(input$entry_mode, "manual"))
    })

    upload_review_jobs <- reactive({
      jobs <- upload_jobs()
      if (is.null(jobs) || length(jobs) == 0) {
        if (is.null(timeseries()) || nrow(data$df) == 0) {
          return(list())
        }
        meta <- selected_timeseries_meta()
        label <- paste("Timeseries", timeseries())
        if (nrow(meta) == 1) {
          label <- target_label(meta)
        }
        return(list(list(
          timeseries_id = as.integer(timeseries()),
          label = label,
          data = data$df
        )))
      }

      active_idx <- which(vapply(
        jobs,
        function(job) {
          identical(as.integer(job$timeseries_id), as.integer(timeseries()))
        },
        logical(1)
      ))
      if (length(active_idx) > 0) {
        jobs[[active_idx[[1]]]]$data <- data$df
      }
      jobs
    })

    upload_review_job <- function(timeseries_id) {
      jobs <- upload_review_jobs()
      idx <- which(vapply(
        jobs,
        function(job) {
          identical(
            as.integer(job$timeseries_id),
            as.integer(timeseries_id)
          )
        },
        logical(1)
      ))
      if (length(idx) == 0) {
        return(NULL)
      }
      jobs[[idx[[1]]]]
    }

    set_upload_job_data <- function(timeseries_id, df) {
      tsid <- as.integer(timeseries_id)
      if (identical(tsid, as.integer(timeseries()))) {
        data$df <- df
      }
      jobs <- upload_jobs()
      if (!is.null(jobs) && length(jobs) > 0) {
        for (i in seq_along(jobs)) {
          if (identical(as.integer(jobs[[i]]$timeseries_id), tsid)) {
            jobs[[i]]$data <- df
            break
          }
        }
        upload_jobs(jobs)
      }
      refresh_data_table()
      update_preview_datetime_bounds()
    }

    active_job_data <- function(timeseries_id) {
      job <- upload_review_job(timeseries_id)
      if (!is.null(job)) {
        return(job$data)
      }
      data$df
    }

    render_data_table <- function(table_state) {
      DT::datatable(
        table_state$data,
        escape = !isTRUE(table_state$editable),
        selection = if (isTRUE(table_state$editable)) {
          list(
            mode = "multiple",
            target = "row",
            selector = "td:first-child"
          )
        } else {
          "none"
        },
        options = list(
          scrollX = TRUE,
          ordering = FALSE,
          columnDefs = list(list(targets = "_all", orderable = FALSE))
        ),
        callback = if (isTRUE(table_state$editable)) {
          htmlwidgets::JS(
            sprintf(
              "var ns = '%s';
              table.on('change', '.cont-data-cell', function() {
                Shiny.setInputValue(ns + 'data_table_cell_update', {
                  row: parseInt(this.dataset.row, 10),
                  col: this.dataset.col,
                  value: this.value,
                  nonce: Math.random()
                }, {priority: 'event'});
              });
              table.on('keydown', '.cont-data-cell', function(e) {
                if (e.key === 'Enter') {
                  e.preventDefault();
                  $(this).trigger('change');
                  this.blur();
                }
              });",
              ns("")
            ),
            "table.on('draw.dt', function() {",
            "  table.$('.cont-data-cell').css({",
            "    'width': '100%',",
            "    'min-width': '110px',",
            "    'box-sizing': 'border-box'",
            "  });",
            "});"
          )
        } else {
          htmlwidgets::JS("")
        },
        rownames = FALSE
      )
    }

    output$data_table_note <- renderUI({
      jobs <- upload_review_jobs()
      table_state <- if (isTRUE(multi_upload_active()) && length(jobs) > 1) {
        table_state_from_df(jobs[[1]]$data, FALSE)
      } else {
        display_table_data()
      }
      if (isTRUE(table_state$editable)) {
        return(tags$div(
          "Hint: type directly in the table cells. Press Enter or leave the cell to save."
        ))
      }
      if (nrow(table_state$data) == 0) {
        return(NULL)
      }

      if (isTRUE(table_state$preview)) {
        return(tags$div(
          class = "text-muted small",
          sprintf(
            "Showing the first and last %s rows of %s uploaded rows. Use the preview plot for full-data review.",
            table_state$displayed_rows / 2L,
            format(table_state$total_rows, big.mark = ",")
          )
        ))
      }

      tags$div(
        class = "text-muted small",
        "Uploaded data are shown read-only. Use the preview plot for review."
      )
    })

    output$data_tables_ui <- renderUI({
      jobs <- upload_review_jobs()
      if (!isTRUE(multi_upload_active()) || length(jobs) <= 1) {
        return(DT::DTOutput(ns("data_table")))
      }
      tabs <- lapply(jobs, function(job) {
        tabPanel(
          title = job$label,
          value = paste0("timeseries_", as.integer(job$timeseries_id)),
          DT::DTOutput(ns(target_output_id("data_table", job$timeseries_id)))
        )
      })
      do.call(
        tabsetPanel,
        c(list(id = ns("data_table_tabset")), tabs)
      )
    })

    output$data_table <- DT::renderDT(
      {
        table_state <- display_table_data()
        render_data_table(table_state)
      },
      server = FALSE
    )

    checkbox_current_value <- function(input_id, default = TRUE) {
      value <- input[[input_id]]
      if (is.null(value)) {
        return(default)
      }
      isTRUE(value)
    }

    observe({
      jobs <- upload_review_jobs()
      for (job in jobs) {
        local({
          tsid <- as.integer(job$timeseries_id)
          table_id <- target_output_id("data_table", tsid)
          output[[table_id]] <- DT::renderDT(
            {
              current_job <- upload_review_job(tsid)
              req(current_job)
              render_data_table(table_state_from_df(current_job$data, FALSE))
            },
            server = FALSE
          )
        })
      }
    })

    output$upload_target_checkboxes <- renderUI({
      jobs <- upload_review_jobs()
      if (length(jobs) == 0) {
        return(NULL)
      }
      div(
        class = "alert alert-secondary",
        style = "padding: 8px; margin: 8px 0;",
        tags$strong("Upload these targets"),
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 2px 16px;",
          lapply(jobs, function(job) {
            input_id <- upload_include_input_id(job$timeseries_id)
            checkboxInput(
              ns(input_id),
              job$label,
              value = checkbox_current_value(input_id, TRUE)
            )
          })
        )
      )
    })

    observeEvent(input$data_table_cell_update, {
      if (!identical(input$entry_mode, "manual")) {
        return()
      }

      info <- input$data_table_cell_update
      row <- as.integer(info$row)
      col <- as.character(info$col)
      if (
        is.na(row) ||
          row < 1 ||
          row > nrow(data$df) ||
          !(col %in% names(data$df))
      ) {
        return()
      }

      value <- as.character(info$value)
      if (identical(col, "value")) {
        data$df[[col]] <- as.character(data$df[[col]])
        data$df[[col]][row] <- value
      } else if (col %in% c("grade", "approval", "qualifier")) {
        allowed <- unname(class_code_choices(col))
        data$df[[col]][row] <- if (value %in% allowed) value else ""
        class_ranges[[col]] <- ranges_from_table_classes(data$df, col)
      } else {
        data$df[[col]][row] <- value
      }
    })

    plot_data <- reactiveVal(list())
    last_plot_signature <- reactiveVal(list())

    current_plot_signature <- function(timeseries_id) {
      target_id <- as.integer(timeseries_id)
      list(
        timeseries_id = target_id,
        df = active_job_data(target_id),
        class_ranges = class_ranges_for_target(target_id),
        preview_historic_range = input$preview_historic_range,
        preview_start_datetime = input$preview_start_datetime,
        preview_end_datetime = input$preview_end_datetime,
        preview_utc_offset = input$preview_utc_offset
      )
    }

    preview_request <- function(timeseries_id = active_preview_timeseries()) {
      target_id <- as.integer(timeseries_id)
      req(target_id)
      target_df <- active_job_data(target_id)
      req(nrow(target_df) > 0)

      parsed_dt <- table_datetimes_to_utc(
        target_df$datetime,
        input$UTC_offset
      )
      parsed_val <- suppressWarnings(as.numeric(target_df$value))
      valid_idx <- !(is.na(parsed_dt) | is.na(parsed_val))
      req(any(valid_idx))

      df_new <- target_df[valid_idx, , drop = FALSE]
      df_new$datetime <- parsed_dt[valid_idx]
      df_new$value <- parsed_val[valid_idx]
      df_new$source <- "New upload"
      preview_offset_seconds <- selected_offset_seconds(
        input$preview_utc_offset,
        default = input$UTC_offset
      )
      preview_tz <- selected_offset_tz(
        input$preview_utc_offset,
        default = input$UTC_offset
      )

      if ("grade" %in% names(df_new)) {
        df_new$grade <- as.character(df_new$grade)
      }
      if ("approval" %in% names(df_new)) {
        df_new$approval <- as.character(df_new$approval)
      }
      if ("qualifier" %in% names(df_new)) {
        df_new$qualifier <- as.character(df_new$qualifier)
      }

      data_start <- min(df_new$datetime, na.rm = TRUE)
      data_end <- max(df_new$datetime, na.rm = TRUE)
      range_start <- data_start
      range_end <- data_end

      custom_start <- scalar_display_datetime_to_utc(
        input$preview_start_datetime,
        input$preview_utc_offset
      )
      if (!is.na(custom_start)) {
        range_start <- min(max(custom_start, data_start), data_end)
      }
      custom_end <- scalar_display_datetime_to_utc(
        input$preview_end_datetime,
        input$preview_utc_offset
      )
      if (!is.na(custom_end)) {
        range_end <- min(max(custom_end, data_start), data_end)
      }
      if (range_end < range_start) {
        range_start <- data_start
        range_end <- data_end
      }

      in_preview_range <- df_new$datetime >= range_start &
        df_new$datetime <= range_end
      df_new <- df_new[in_preview_range, , drop = FALSE]
      req(nrow(df_new) > 0)

      list(
        timeseries_id = target_id,
        new_data = df_new,
        range_start = range_start,
        range_end = range_end,
        display_offset_seconds = preview_offset_seconds,
        display_tz = preview_tz,
        show_historic_range = isTRUE(input$preview_historic_range),
        class_ranges = class_ranges_for_target(target_id),
        class_types = class_type_choices(),
        config = session$userData$config,
        signature = current_plot_signature(target_id)
      )
    }

    output$plot_refresh_warning <- renderUI({
      target_id <- active_preview_timeseries()
      if (is.null(target_id) || is.na(target_id)) {
        return(NULL)
      }
      target_key <- as.character(target_id)
      signatures <- last_plot_signature()
      if (is.null(signatures[[target_key]])) {
        return(NULL)
      }

      changed <- !isTRUE(all.equal(
        signatures[[target_key]],
        current_plot_signature(target_id),
        check.attributes = FALSE
      ))
      if (!changed) {
        return(NULL)
      }
      div(
        style = "color:#b30000;margin-top:6px;",
        "Newly entered data or preview settings have changed since the last refresh plot action."
      )
    })

    make_preview_plot <- function(pv) {
      db_config <- pv$config
      pv$config <- NULL
      con <- AquaConnect(
        name = db_config$dbName,
        host = db_config$dbHost,
        port = db_config$dbPort,
        username = db_config$dbUser,
        password = db_config$dbPass,
        silent = TRUE
      )
      db_config <- NULL
      on.exit(
        {
          if (!is.null(con) && DBI::dbIsValid(con)) {
            DBI::dbDisconnect(con)
          }
        },
        add = TRUE
      )

      extra <- dbGetQueryDT(
        con,
        "SELECT datetime, value_corrected AS value FROM continuous.measurements_continuous_corrected($1, $2, $3)",
        params = list(pv$timeseries_id, pv$range_start, pv$range_end)
      )

      if (nrow(extra) == 0) {
        extra <- NULL
      }

      hist_out <- NULL
      if (isTRUE(pv$show_historic_range)) {
        # Add a day to the end for the historic query to ensure enough ribbon.
        hist_out <- dbGetQueryDT(
          con,
          "SELECT date AS datetime, min, max, q25, q75 FROM continuous.measurements_calculated_daily WHERE timeseries_id = $1 AND date >= $2 AND date <= $3",
          params = list(pv$timeseries_id, pv$range_start, pv$range_end + 86400)
        )
      }

      pv$new_data$datetime <- pv$new_data$datetime + pv$display_offset_seconds
      if (!is.null(extra)) {
        extra$datetime <- coerce_utc_datetime(extra$datetime) +
          pv$display_offset_seconds
      }
      if (!is.null(hist_out) && nrow(hist_out) > 0) {
        hist_out$datetime <- coerce_utc_datetime(hist_out$datetime) +
          pv$display_offset_seconds
      }

      parameter <- dbGetQueryDT(
        con,
        paste(
          "SELECT p.param_name,",
          ac_parameter_unit_select_sql(
            con,
            "p",
            "unit",
            matrix_state_alias = "ts",
            media_alias = "ts"
          ),
          ", p.plot_default_y_orientation",
          "FROM public.parameters p",
          "JOIN continuous.timeseries ts ON p.parameter_id = ts.parameter_id",
          "WHERE ts.timeseries_id = $1"
        ),
        params = list(pv$timeseries_id)
      )

      pv$db <- extra
      pv$historic <- hist_out
      pv$parameter <- parameter
      class_ranges <- pv$class_ranges

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

      # Add class bands (grade/approval/qualifier) for newly entered data
      has_class_bands <- nrow(class_ranges$grade) > 0 ||
        nrow(class_ranges$approval) > 0 ||
        nrow(class_ranges$qualifier) > 0
      if (has_class_bands && nrow(pv$new_data) > 0) {
        mindt <- min(pv$new_data$datetime, na.rm = TRUE)
        maxdt <- max(pv$new_data$datetime, na.rm = TRUE)
        type_map <- pv$class_types
        poly_list <- list()
        new_data_dt <- sort(unique(pv$new_data$datetime))
        approval_y <- NULL
        grade_y <- NULL
        qualifier_y <- NULL

        add_band <- function(class_name, yset, label_prefix) {
          rr <- class_ranges[[class_name]]
          if (nrow(rr) == 0) {
            return(invisible(NULL))
          }
          sdt <- parse_utc_datetime_value(rr$start_datetime) +
            pv$display_offset_seconds
          edt <- parse_utc_datetime_value(rr$end_datetime) +
            pv$display_offset_seconds
          rr$start_dt <- pmax(sdt, mindt)
          rr$end_dt <- pmin(edt, maxdt)

          # Extend each class band to the next data point to avoid visible gaps
          next_idx <- findInterval(rr$end_dt, new_data_dt) + 1
          has_next <- next_idx <= length(new_data_dt)
          rr$end_dt[has_next] <- new_data_dt[next_idx[has_next]]

          rr <- rr[
            !is.na(rr$start_dt) & !is.na(rr$end_dt) & rr$end_dt >= rr$start_dt,
            ,
            drop = FALSE
          ]
          if (nrow(rr) == 0) {
            return(invisible(NULL))
          }
          rr$id <- paste0(class_name, "_", seq_len(nrow(rr)))
          idx <- match(rr$code, type_map[[class_name]]$code)
          rr$color <- ifelse(
            is.na(idx),
            "#BBBBBB",
            as.character(type_map[[class_name]]$color_code[idx])
          )
          rr$description <- ifelse(
            is.na(idx),
            rr$description,
            as.character(type_map[[class_name]]$description[idx])
          )
          rr_dt <- data.table::as.data.table(rr)
          poly_list[[length(poly_list) + 1]] <<- rr_dt[,
            .(
              datetime = c(start_dt[1L], start_dt[1L], end_dt[1L], end_dt[1L]),
              y = yset,
              color = color[1L],
              text = paste0(
                label_prefix,
                ": ",
                code[1L],
                " (",
                description[1L],
                ")"
              ),
              id = id[1L]
            ),
            by = id
          ]
        }

        if (nrow(class_ranges$approval) > 0) {
          approval_y <- if (
            nrow(class_ranges$grade) > 0 && nrow(class_ranges$qualifier) > 0
          ) {
            c(2.2, 3.2, 3.2, 2.2)
          } else if (nrow(class_ranges$grade) > 0) {
            c(1.1, 2.1, 2.1, 1.1)
          } else {
            c(0, 1, 1, 0)
          }
          add_band("approval", approval_y, "Approval")
        }
        if (nrow(class_ranges$grade) > 0) {
          grade_y <- if (nrow(class_ranges$qualifier) > 0) {
            c(1.1, 2.1, 2.1, 1.1)
          } else {
            c(0, 1, 1, 0)
          }
          add_band("grade", grade_y, "Grade")
        }
        if (nrow(class_ranges$qualifier) > 0) {
          qualifier_y <- c(0, 1, 1, 0)
          add_band("qualifier", qualifier_y, "Qualifier")
        }

        if (length(poly_list) > 0) {
          polygons_df <- data.table::rbindlist(poly_list, use.names = TRUE)
          annotation_list <- list()
          if (!is.null(approval_y)) {
            annotation_list <- c(
              annotation_list,
              list(list(
                x = 0,
                y = mean(approval_y[c(1, 2)]),
                xref = "paper",
                yref = "y",
                text = "Approval",
                showarrow = FALSE,
                xanchor = "right",
                yanchor = "middle",
                font = list(size = 10)
              ))
            )
          }
          if (!is.null(grade_y)) {
            annotation_list <- c(
              annotation_list,
              list(list(
                x = 0,
                y = mean(grade_y[c(1, 2)]),
                xref = "paper",
                yref = "y",
                text = "Grade",
                showarrow = FALSE,
                xanchor = "right",
                yanchor = "middle",
                font = list(size = 10)
              ))
            )
          }
          if (!is.null(qualifier_y)) {
            annotation_list <- c(
              annotation_list,
              list(list(
                x = 0,
                y = mean(qualifier_y[c(1, 2)]),
                xref = "paper",
                yref = "y",
                text = "Qualifier",
                showarrow = FALSE,
                xanchor = "right",
                yanchor = "middle",
                font = list(size = 10)
              ))
            )
          }

          bands_plot <- plotly::plot_ly() |>
            plotly::add_polygons(
              data = polygons_df,
              x = ~datetime,
              y = ~y,
              split = ~id,
              fill = "toself",
              fillcolor = ~color,
              line = list(width = 1, color = "black"),
              hoverinfo = "text",
              hoveron = "fills",
              text = ~text,
              showlegend = FALSE
            ) |>
            plotly::layout(
              yaxis = list(
                showticklabels = FALSE,
                showgrid = FALSE,
                zeroline = FALSE
              ),
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              annotations = annotation_list,
              margin = list(t = 0, b = 20, l = 80)
            )
          plot <- plotly::subplot(
            plot,
            bands_plot,
            nrows = 2,
            shareX = TRUE,
            heights = c(0.8, 0.2),
            margin = 0.02
          )
        }
      }

      plot <- plot |>
        plotly::layout(
          title = NULL,
          xaxis = list(
            title = list(
              text = paste0("Datetime (", pv$display_tz, ")"),
              standoff = 0
            ),
            showgrid = FALSE,
            showline = TRUE,
            tickformat = "%b %-d '%y",
            titlefont = list(size = 14),
            tickfont = list(size = 12),
            nticks = 10,
            rangeslider = list(
              visible = FALSE
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

      plot
    }

    preview_plot_task <- ExtendedTask$new(function(pv) {
      promises::future_promise({
        tryCatch(
          {
            list(
              timeseries_id = pv$timeseries_id,
              plot = make_preview_plot(pv),
              signature = pv$signature
            )
          },
          error = function(e) {
            conditionMessage(e)
          }
        )
      })
    }) |>
      bslib::bind_task_button("make_plot")

    observeEvent(input$make_plot, {
      target_id <- active_preview_timeseries()
      req(target_id)
      preview_plot_task$invoke(preview_request(target_id))
    })

    observeEvent(preview_plot_task$result(), {
      result <- preview_plot_task$result()
      if (inherits(result, "character")) {
        showNotification(
          paste("Preview plot failed:", result),
          type = "error",
          duration = 10
        )
        return()
      }

      target_key <- as.character(as.integer(result$timeseries_id))
      plots <- plot_data()
      plots[[target_key]] <- result$plot
      plot_data(plots)

      signatures <- last_plot_signature()
      signatures[[target_key]] <- result$signature
      last_plot_signature(signatures)
    })

    output$preview_plot_tabs <- renderUI({
      jobs <- upload_review_jobs()
      if (!isTRUE(multi_upload_active()) || length(jobs) <= 1) {
        return(plotly::plotlyOutput(ns("data_preview")))
      }
      tabs <- lapply(jobs, function(job) {
        tabPanel(
          title = job$label,
          value = paste0("timeseries_", as.integer(job$timeseries_id)),
          plotly::plotlyOutput(
            ns(target_output_id("data_preview", job$timeseries_id))
          )
        )
      })
      do.call(
        tabsetPanel,
        c(list(id = ns("preview_timeseries_tabset")), tabs)
      )
    })

    observeEvent(input$preview_timeseries_tabset, {
      update_preview_datetime_bounds()
    }, ignoreInit = TRUE)

    observe({
      jobs <- upload_review_jobs()
      for (job in jobs) {
        local({
          tsid <- as.integer(job$timeseries_id)
          output_id <- target_output_id("data_preview", tsid)
          output[[output_id]] <- plotly::renderPlotly({
            plot_data()[[as.character(tsid)]]
          })
        })
      }
    })

    output$data_preview <- plotly::renderPlotly({
      target_id <- active_preview_timeseries()
      req(target_id)
      plot_data()[[as.character(as.integer(target_id))]]
    })

    current_upload_jobs <- function() {
      jobs <- upload_jobs()
      if (is.null(jobs) || length(jobs) == 0) {
        label <- paste("Timeseries", timeseries())
        meta <- selected_timeseries_meta()
        if (nrow(meta) == 1) {
          label <- target_label(meta)
        }
        jobs <- list(list(
          timeseries_id = as.integer(timeseries()),
          label = label,
          data = data$df
        ))
      } else {
        active_idx <- which(vapply(
          jobs,
          function(job) {
            identical(as.integer(job$timeseries_id), as.integer(timeseries()))
          },
          logical(1)
        ))
        if (length(active_idx) > 0) {
          jobs[[active_idx[[1]]]]$data <- data$df
        }
      }

      jobs <- Filter(
        function(job) {
          checkbox_current_value(
            upload_include_input_id(job$timeseries_id),
            TRUE
          )
        },
        jobs
      )
      if (length(jobs) == 0) {
        return(jobs)
      }

      for (i in seq_along(jobs)) {
        target_id <- as.integer(jobs[[i]]$timeseries_id)
        jobs[[i]]$data <- apply_class_ranges_to_df(
          jobs[[i]]$data,
          class_ranges_for_target(target_id)
        )
      }
      jobs
    }

    validate_upload_jobs <- function() {
      if (is.null(timeseries())) {
        showNotification('Please select a timeseries first.', type = 'error')
        return(NULL)
      }
      if (!isTRUE(selected_timeseries_is_basic())) {
        showNotification(
          'Data can only be added directly to Basic timeseries. Select a Basic member source timeseries first.',
          type = 'error',
          duration = 8
        )
        return(NULL)
      }
      if (is.null(input$owner) || is.null(input$contributor)) {
        showNotification(
          'Please select owner and contributor organizations.',
          type = 'error'
        )
        return(NULL)
      }

      jobs <- current_upload_jobs()
      if (length(jobs) == 0) {
        showNotification(
          'No upload targets are selected for upload.',
          type = 'error'
        )
        return(NULL)
      }

      for (i in seq_along(jobs)) {
        df <- jobs[[i]]$data
        label <- jobs[[i]]$label
        if (nrow(df) == 0) {
          showNotification(
            paste(label, "has an empty data table."),
            type = 'error'
          )
          return(NULL)
        }

        duplicated_rows <- duplicated(df)
        if (any(duplicated_rows)) {
          df <- df[!duplicated_rows, , drop = FALSE]
          showNotification(
            paste0(
              label,
              ": ",
              sum(duplicated_rows),
              ' duplicated row(s) were removed.'
            ),
            type = 'message',
            duration = 8
          )
        }

        parsed_value <- suppressWarnings(as.numeric(df$value))
        if (any(is.na(parsed_value))) {
          showNotification(
            paste(
              label,
              'value column must be numeric with no missing values.'
            ),
            type = 'error',
            duration = 8
          )
          return(NULL)
        }

        parsed_datetime <- table_datetimes_to_utc(df$datetime, input$UTC_offset)
        if (any(is.na(parsed_datetime))) {
          showNotification(
            paste(
              label,
              'datetime column is not in the correct format. Please check your data: it should be of form YYYY-MM-DD HH:MM.'
            ),
            type = 'error',
            duration = 10
          )
          return(NULL)
        }

        duplicated_datetimes <- parsed_datetime[duplicated(parsed_datetime)]
        if (length(duplicated_datetimes) > 0) {
          showNotification(
            paste0(
              label,
              ': there is more than one value for ',
              paste(
                unique(format(duplicated_datetimes, "%Y-%m-%d %H:%M:%S")),
                collapse = ', '
              )
            ),
            type = 'error',
            duration = 10
          )
          return(NULL)
        }

        df$datetime <- parsed_datetime
        df$value <- parsed_value
        df$owner <- as.integer(input$owner)
        df$contributor <- as.integer(input$contributor)
        df$no_update <- data.table::fifelse(
          input$no_update == "yes",
          TRUE,
          FALSE
        )
        jobs[[i]]$data <- df

        if (
          identical(
            as.integer(jobs[[i]]$timeseries_id),
            as.integer(timeseries())
          )
        ) {
          data$parsed_datetime <- parsed_datetime
          data$parsed_value <- parsed_value
        }
      }

      jobs
    }

    # function to check data validity before upload
    check_fx <- function() {
      jobs <- validate_upload_jobs()
      if (is.null(jobs)) {
        upload_validation$jobs <- NULL
        return(FALSE)
      }
      upload_validation$jobs <- jobs
      TRUE
    }

    empty_continuous_upload_df <- function() {
      data.frame(
        datetime = character(),
        value = numeric(),
        grade = character(),
        approval = character(),
        qualifier = character(),
        stringsAsFactors = FALSE
      )
    }

    reset_upload_state <- function() {
      data$df <- empty_continuous_upload_df()
      data$parsed_datetime <- NULL
      data$parsed_value <- NULL
      upload_jobs(NULL)
      upload_validation$jobs <- NULL
      unit_conversion_state$previous_values <- NULL
      unit_conversion_state$previous_label <- NULL
      class_ranges$grade <- class_ranges$grade[0, , drop = FALSE]
      class_ranges$approval <- class_ranges$approval[0, , drop = FALSE]
      class_ranges$qualifier <- class_ranges$qualifier[0, , drop = FALSE]
      target_class_ranges$grade <- list()
      target_class_ranges$approval <- list()
      target_class_ranges$qualifier <- list()
      plot_data(list())
      last_plot_signature(list())
      last_preview_bounds_signature(NULL)
      refresh_data_table()
    }

    build_upload_request <- function(overwrite) {
      req(upload_validation$jobs)

      list(
        jobs = upload_validation$jobs,
        overwrite = overwrite,
        config = session$userData$config
      )
    }

    upload_task <- ExtendedTask$new(function(req) {
      promises::future_promise({
        warnings <- character()
        messages <- character()

        tryCatch(
          {
            con <- AquaConnect(
              name = req$config$dbName,
              host = req$config$dbHost,
              port = req$config$dbPort,
              username = req$config$dbUser,
              password = req$config$dbPass,
              silent = TRUE
            )
            on.exit(DBI::dbDisconnect(con), add = TRUE)

            DBI::dbBegin(con)
            committed <- FALSE
            on.exit(
              {
                if (!committed) {
                  try(DBI::dbRollback(con), silent = TRUE)
                }
              },
              add = TRUE
            )

            for (job in req$jobs) {
              withCallingHandlers(
                AquaCache::addNewContinuous(
                  tsid = job$timeseries_id,
                  df = job$data,
                  con = con,
                  overwrite = req$overwrite
                ),
                warning = function(w) {
                  warnings <<- c(
                    warnings,
                    paste(job$label, conditionMessage(w), sep = ": ")
                  )
                  invokeRestart("muffleWarning")
                },
                message = function(m) {
                  messages <<- c(
                    messages,
                    paste(job$label, conditionMessage(m), sep = ": ")
                  )
                  invokeRestart("muffleMessage")
                }
              )
            }
            DBI::dbCommit(con)
            committed <- TRUE

            list(
              ok = TRUE,
              overwrite = req$overwrite,
              n_jobs = length(req$jobs),
              warnings = unique(warnings),
              messages = unique(messages)
            )
          },
          error = function(e) {
            list(
              ok = FALSE,
              message = conditionMessage(e)
            )
          }
        )
      })
    }) |>
      bslib::bind_task_button("upload") |>
      bslib::bind_task_button("upload_overwrite_all") |>
      bslib::bind_task_button("upload_overwrite_some")

    invoke_upload_task <- function(overwrite) {
      check <- check_fx()
      if (!check) {
        return()
      }

      upload_task$invoke(build_upload_request(overwrite))
    }

    observeEvent(input$upload, {
      invoke_upload_task("no")
    })

    observeEvent(upload_task$result(), {
      result <- upload_task$result()

      if (!isTRUE(result$ok)) {
        showNotification(
          paste('Upload failed:', result$message),
          type = 'error',
          duration = 10
        )
        return()
      }

      if (length(result$warnings)) {
        showNotification(
          paste('Warning on upload:', paste(result$warnings, collapse = " ")),
          type = 'warning',
          duration = 10
        )
      }
      if (length(result$messages)) {
        showNotification(
          paste('Message on upload:', paste(result$messages, collapse = " ")),
          type = 'message',
          duration = 8
        )
      }

      notification <- switch(
        result$overwrite,
        all = 'Data added with overwrite.',
        conflict = 'Data added with selective overwrite.',
        'Data added.'
      )
      if (!is.null(result$n_jobs) && result$n_jobs > 1) {
        notification <- paste0(
          notification,
          " Uploaded ",
          result$n_jobs,
          " timeseries."
        )
      }
      showNotification(notification, type = 'message')
      reset_upload_state()
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
      invoke_upload_task("all")
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
      invoke_upload_task("conflict")
    })

    return(outputs)
  }) # End of moduleServer
}
