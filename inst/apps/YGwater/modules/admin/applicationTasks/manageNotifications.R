manageNotificationsUI <- function(id, module_choices) {
  ns <- NS(id)
  page_fluid(
    tagList(
      tags$p(
        "Notifications appear in modules by server name. Use 'all' to show a",
        "notification across the entire application."
      ),
      DT::DTOutput(ns("notifications_table")),
      tags$hr(),
      checkboxInput(ns("active"), "Active", value = TRUE),
      selectizeInput(
        ns("target_module"),
        "Target modules",
        choices = module_choices,
        multiple = TRUE,
        selected = "all"
      ),
      textAreaInput(
        ns("message_en"),
        "Message (English)",
        width = "100%",
        height = "90px"
      ),
      textAreaInput(
        ns("message_fr"),
        "Message (French)",
        width = "100%",
        height = "90px"
      ),
      div(
        actionButton(ns("create_notification"), "Add notification"),
        actionButton(ns("update_notification"), "Update selected"),
        actionButton(ns("reset_form"), "Reset")
      ),
      verbatimTextOutput(ns("status"))
    )
  )
}

manageNotifications <- function(id, module_choices) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (!DBI::dbExistsTable(
      session$userData$AquaCache,
      DBI::Id(schema = "application", table = "notifications")
    )) {
      showModal(modalDialog(
        title = "Missing table",
        "The application.notifications table is not available.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "
      SELECT
        has_table_privilege(current_user, 'application.notifications', 'SELECT') AS can_select,
        has_table_privilege(current_user, 'application.notifications', 'INSERT') AS can_insert,
        has_table_privilege(current_user, 'application.notifications', 'UPDATE') AS can_update
      "
    )

    if (!check$can_select) {
      showModal(modalDialog(
        title = "Insufficient Privileges",
        "You do not have the necessary privileges to manage notifications.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    if (!check$can_insert) {
      shinyjs::disable("create_notification")
    }
    if (!check$can_update) {
      shinyjs::disable("update_notification")
    }

    parse_text_array <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(character())
      }
      if (is.list(value)) {
        value <- value[[1]]
      }
      if (length(value) > 1) {
        return(trimws(value))
      }
      value <- gsub("[{}\"]", "", value)
      if (!nzchar(value)) {
        return(character())
      }
      out <- trimws(unlist(strsplit(value, ",")))
      out[nzchar(out)]
    }

    format_text_array <- function(values) {
      if (is.null(values) || !length(values)) {
        return(NULL)
      }
      values <- values[nzchar(values)]
      if (!length(values)) {
        return(NULL)
      }
      values <- gsub('"', '\\\"', values, fixed = TRUE)
      paste0("{", paste(sprintf('"%s"', values), collapse = ","), "}")
    }

    parse_message <- function(value) {
      parsed <- value
      if (is.null(value) || all(is.na(value))) {
        return(list(English = NA_character_, `Français` = NA_character_))
      }
      if (is.character(value)) {
        parsed <- tryCatch(
          jsonlite::fromJSON(value),
          error = function(e) value
        )
      }
      if (is.list(parsed)) {
        english_msg <- parsed[["English"]]
        french_msg <- parsed[["Français"]]
        if (is.null(english_msg)) {
          english_msg <- NA_character_
        }
        if (is.null(french_msg)) {
          french_msg <- NA_character_
        }
        return(list(
          English = english_msg,
          `Français` = french_msg
        ))
      }
      list(English = parsed, `Français` = NA_character_)
    }

    build_message_json <- function(en, fr) {
      payload <- list()
      if (nzchar(en)) {
        payload[["English"]] <- en
      }
      if (nzchar(fr)) {
        payload[["Français"]] <- fr
      }
      if (!length(payload)) {
        return(NULL)
      }
      jsonlite::toJSON(payload, auto_unbox = TRUE)
    }

    load_notifications <- function() {
      raw <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT notification_id, active, target_module, message FROM application.notifications ORDER BY notification_id DESC"
      )
      if (!nrow(raw)) {
        return(data.frame())
      }
      parsed <- lapply(raw$message, parse_message)
      data.frame(
        notification_id = raw$notification_id,
        active = raw$active,
        target_module = vapply(
          raw$target_module,
          function(target) paste(parse_text_array(target), collapse = ", "),
          character(1)
        ),
        message_en = vapply(parsed, function(msg) msg$English, character(1)),
        message_fr = vapply(parsed, function(msg) msg$`Français`, character(1)),
        stringsAsFactors = FALSE
      )
    }

    notifications <- reactiveVal(load_notifications())

    refresh_notifications <- function() {
      notifications(load_notifications())
    }

    status_msg <- reactiveVal("")
    output$status <- renderText(status_msg())

    reset_form <- function() {
      updateCheckboxInput(session, "active", value = TRUE)
      updateSelectizeInput(session, "target_module", selected = "all")
      updateTextAreaInput(session, "message_en", value = "")
      updateTextAreaInput(session, "message_fr", value = "")
    }

    output$notifications_table <- DT::renderDT({
      DT::datatable(
        notifications(),
        rownames = FALSE,
        selection = "single",
        options = list(scrollX = TRUE)
      )
    })

    observeEvent(input$notifications_table_rows_selected, {
      selected <- input$notifications_table_rows_selected
      if (!length(selected)) {
        return()
      }
      row <- notifications()[selected, , drop = FALSE]
      if (!nrow(row)) {
        return()
      }
      updateCheckboxInput(session, "active", value = isTRUE(row$active))
      updateSelectizeInput(
        session,
        "target_module",
        selected = parse_text_array(row$target_module)
      )
      updateTextAreaInput(session, "message_en", value = row$message_en)
      updateTextAreaInput(session, "message_fr", value = row$message_fr)
    })

    observeEvent(input$create_notification, {
      targets <- input$target_module
      message_json <- build_message_json(input$message_en, input$message_fr)

      if (is.null(targets) || !length(targets)) {
        status_msg("Please select at least one target module.")
        return()
      }
      if (is.null(message_json)) {
        status_msg("Please provide at least one message.")
        return()
      }

      DBI::dbExecute(
        session$userData$AquaCache,
        "INSERT INTO application.notifications (active, target_module, message) VALUES ($1, ($2)::text[], $3)",
        params = list(
          isTRUE(input$active),
          format_text_array(targets),
          message_json
        )
      )

      status_msg("Notification added.")
      refresh_notifications()
      reset_form()
    })

    observeEvent(input$update_notification, {
      selected <- input$notifications_table_rows_selected
      if (!length(selected)) {
        status_msg("Select a notification to update.")
        return()
      }

      targets <- input$target_module
      message_json <- build_message_json(input$message_en, input$message_fr)

      if (is.null(targets) || !length(targets)) {
        status_msg("Please select at least one target module.")
        return()
      }
      if (is.null(message_json)) {
        status_msg("Please provide at least one message.")
        return()
      }

      notification_id <- notifications()$notification_id[selected]
      DBI::dbExecute(
        session$userData$AquaCache,
        "UPDATE application.notifications SET active = $1, target_module = ($2)::text[], message = $3 WHERE notification_id = $4",
        params = list(
          isTRUE(input$active),
          format_text_array(targets),
          message_json,
          notification_id
        )
      )

      status_msg("Notification updated.")
      refresh_notifications()
    })

    observeEvent(input$reset_form, {
      reset_form()
      status_msg("")
    })

    observeEvent(input$target_module, {
      if (!length(input$target_module)) {
        return()
      }
      if (length(input$target_module) > 1 && "all" %in% input$target_module) {
        updateSelectizeInput(session, "target_module", selected = "all")
      }
    })
  })
}
