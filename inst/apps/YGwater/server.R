#' The YGwater app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Initial setup #############################################################

  # Store the config info in the session. If the user connects with their own credentials these need to be used for plot rendering wrapped in an ExtendedTask or future/promises
  session$userData$config <- config

  # Initial database connections without edit privileges
  session$userData$AquaCache <- AquaConnect(
    name = config$dbName,
    host = config$dbHost,
    port = config$dbPort,
    username = config$dbUser,
    password = config$dbPass,
    silent = TRUE
  )

  # Reset the application_name to 'YGwater_shiny'
  DBI::dbExecute(
    session$userData$AquaCache,
    "set application_name TO 'YGwater_shiny'"
  )

  # Logging to usage tracking tables ###########################################

  safe_disconnect <- function(con) {
    if (is.null(con)) {
      return(invisible(NULL))
    }
    try(
      {
        if (DBI::dbIsValid(con)) {
          DBI::dbDisconnect(con)
        }
      },
      silent = TRUE
    )
  }

  get_client_ip <- function(session) {
    req <- session$request

    # Prefer X-Forwarded-For when behind proxies
    xff <- req$HTTP_X_FORWARDED_FOR
    if (!is.null(xff) && nzchar(xff)) {
      # XFF is a comma-separated chain; the left-most is the original client
      return(trimws(strsplit(xff, ",")[[1]][1]))
    }

    # Fallbacks
    xri <- req$HTTP_X_REAL_IP
    if (!is.null(xri) && nzchar(xri)) {
      return(trimws(xri))
    }

    req$REMOTE_ADDR
  }

  nav_menu_values <- c(
    "maps",
    "plot",
    "reports",
    "images",
    "data",
    "info",
    "continuousDataTasks",
    "discreteDataTasks",
    "dbLocsTasks",
    "fileTasks",
    "fieldTasks",
    "equipTasks",
    "wellTasks",
    "metadataTasks",
    "acquisitionTelemetryTasks",
    "adminTasks"
  )

  admin_leaf_pages <- c(
    "adminHome",
    "syncCont",
    "syncDisc",
    "addLocation",
    "addSubLocation",
    "addTimeseries",
    "deploy_recover",
    "calibrate",
    "manageInstruments",
    "addContData",
    "continuousCorrections",
    "imputeMissing",
    "editContData",
    "grades_approvals_qualifiers",
    "addDiscData",
    "addSamples",
    "editDiscData",
    "addGuidelines",
    "addSampleSeries",
    "addDocs",
    "addImgs",
    "addImgSeries",
    "manageNewsContent",
    "manageNotifications",
    "viewFeedback",
    "visit",
    "changePwd",
    "manageUsers",
    "manageOrganizations",
    "manageNetworks",
    "manageProjects",
    "manageNetworkProjectTypes",
    "manageLocationTypes",
    "manageMediaTypes",
    "manageMatrixStates",
    "manageParameterGroups",
    "manageParameterSubGroups",
    "manageParameters",
    "manageCommunicationProtocolFamilies",
    "manageCommunicationProtocols",
    "manageTransmissionMethodFamilies",
    "manageTransmissionMethods",
    "manageTransmissionComponentRoles",
    "manageInstrumentConnections",
    "manageInstrumentConnectionSignals",
    "manageTransmissionSetups",
    "manageTransmissionRoutes",
    "manageTransmissionComponents",
    "simplerIndex",
    "editBoreholesWells",
    "manageBoreholeDocuments"
  )

  map_page_ids <- c(
    "monitoringLocationsMap",
    "parameterValuesMap",
    "rasterValuesMap",
    "snowBulletinMap",
    "imgMapView",
    "WWR",
    "addLocation",
    "addSubLocation",
    "addImgs",
    "addImgSeries",
    "visit"
  )

  data_page_ids <- c(
    "discData",
    "contData",
    "docTableView",
    "imgTableView",
    "WWR",
    "viewFeedback",
    "simplerIndex",
    "editBoreholesWells",
    "manageBoreholeDocuments",
    "addDiscData",
    "editDiscData",
    "addContData",
    "editContData",
    "manageInstruments",
    "addSamples",
    "addSampleSeries",
    "syncCont",
    "syncDisc"
  )

  is_leaf_page <- function(page_id) {
    if (is.null(page_id) || length(page_id) == 0) {
      return(FALSE)
    }
    page_id <- as.character(page_id)[1]
    if (is.na(page_id) || !nzchar(page_id)) {
      return(FALSE)
    }
    !page_id %in% nav_menu_values
  }

  classify_page_side <- function(page_id) {
    if (!is_leaf_page(page_id)) {
      return("system")
    }
    if (page_id %in% admin_leaf_pages) {
      return("admin")
    }
    "public"
  }

  is_plot_trigger_input <- function(input_id) {
    if (is.null(input_id) || length(input_id) == 0) {
      return(FALSE)
    }
    input_id <- as.character(input_id)[1]
    if (is.na(input_id) || !nzchar(input_id)) {
      return(FALSE)
    }
    grepl(
      "(^|_)(go|make_plot|plot|add_new_trace|add_new_subplot|modify_trace|modify_subplot|trace[0-9]+|subplot[0-9]+|aes_apply|last_30|entire_record|stats)$",
      input_id,
      perl = TRUE
    )
  }

  is_map_filter_input <- function(input_id) {
    if (is.null(input_id) || length(input_id) == 0) {
      return(FALSE)
    }
    input_id <- as.character(input_id)[1]
    if (is.na(input_id) || !nzchar(input_id)) {
      return(FALSE)
    }
    noisy_map_ids <- c(
      "map_zoom",
      "map_center",
      "map_bounds",
      "map_marker_click",
      "map_marker_mouseover",
      "map_shape_click",
      "map_shape_mouseover",
      "map_click"
    )
    if (input_id %in% noisy_map_ids) {
      return(FALSE)
    }
    grepl(
      "filter|search|select|purpose|network|project|param|location|layer|cluster|date|year|yrs|include_|reset|well_name",
      input_id,
      ignore.case = TRUE
    )
  }

  is_data_request_input <- function(input_id, page_id = NULL) {
    if (is.null(input_id) || length(input_id) == 0) {
      return(FALSE)
    }
    input_id <- tolower(as.character(input_id)[1])
    if (is.na(input_id) || !nzchar(input_id)) {
      return(FALSE)
    }

    # Ignore read-only/noise interactions that do not request server-side data.
    if (
      grepl("(_rows_selected|_cell_clicked|_cells_selected|_state$)", input_id)
    ) {
      return(FALSE)
    }

    if (
      grepl(
        "(^|[._-])(load|reload|refresh|fetch|get|query|search|apply|submit|run|request|download|export|commit|upload|open|show|view)([._-]|$)",
        input_id
      )
    ) {
      return(TRUE)
    }

    !is.null(page_id) &&
      page_id %in% data_page_ids &&
      grepl(
        "(^|[._-])(table|dataset|records?|document|docs|image|img|well|borehole|sample|series)([._-]|$)",
        input_id
      )
  }

  summarize_usage_input_value <- function(value, max_values = 5L) {
    if (is.null(value)) {
      return(list(type = "null"))
    }
    if (is.data.frame(value)) {
      return(list(
        type = "data.frame",
        nrow = nrow(value),
        ncol = ncol(value)
      ))
    }
    if (is.list(value) && !is.atomic(value)) {
      return(list(type = "list", length = length(value)))
    }

    val <- value
    if (inherits(val, "factor")) {
      val <- as.character(val)
    }
    if (inherits(val, c("POSIXct", "POSIXt", "Date"))) {
      val <- as.character(val)
    }

    val_len <- length(val)
    val_type <- typeof(val)
    if (!is.atomic(val) || val_len == 0) {
      return(list(type = val_type, length = val_len))
    }

    if (is.character(val)) {
      shown <- utils::head(val, max_values)
      shown <- substr(shown, 1, 120)
      return(list(
        type = "character",
        length = val_len,
        values = shown
      ))
    }

    if (is.logical(val)) {
      return(list(
        type = "logical",
        length = val_len,
        values = as.logical(utils::head(val, max_values))
      ))
    }

    if (is.numeric(val)) {
      if (val_len <= max_values) {
        return(list(
          type = "numeric",
          length = val_len,
          values = as.numeric(val)
        ))
      }
      return(list(
        type = "numeric",
        length = val_len,
        min = suppressWarnings(min(val, na.rm = TRUE)),
        max = suppressWarnings(max(val, na.rm = TRUE))
      ))
    }

    list(type = val_type, length = val_len)
  }

  session$userData$usage_id <- NULL
  session$userData$usage_closed <- TRUE
  session$userData$usage_events_enabled <- TRUE
  session$userData$usage_events_warned <- FALSE

  active_usage_page <- new.env(parent = emptyenv())
  active_usage_page$page_id <- NULL
  active_usage_page$app_side <- NULL
  active_usage_page$entered_at <- NULL

  insert_usage_session <- function(user_id) {
    user_id_db <- if (is.null(user_id) || length(user_id) == 0) {
      NA_character_
    } else {
      user_id_db <- as.character(user_id)[1]
      if (is.na(user_id_db) || !nzchar(user_id_db)) {
        NA_character_
      } else {
        user_id_db
      }
    }

    tryCatch(
      {
        DBI::dbGetQuery(
          session$userData$AquaCache,
          "INSERT INTO application.shiny_app_usage (app_name, user_id, user_ip) VALUES ($1, $2, $3) RETURNING id;",
          params = list(
            if (config$public) "YGwater/public" else "YGwater/partner",
            user_id_db,
            get_client_ip(session)
          )
        )[1, 1]
      },
      error = function(e) {
        warning("Failed to log app usage to database: ", e$message)
        NULL
      }
    )
  }

  log_usage_event <- function(
    event_type,
    page_id = NULL,
    app_side = "system",
    duration_ms = NULL,
    payload = list()
  ) {
    if (!isTRUE(session$userData$usage_events_enabled)) {
      return(FALSE)
    }

    usage_id <- session$userData$usage_id
    if (is.null(usage_id) || length(usage_id) == 0) {
      return(FALSE)
    }

    event_type_db <- as.character(event_type)[1]
    if (is.na(event_type_db) || !nzchar(event_type_db)) {
      return(FALSE)
    }

    app_side <- as.character(app_side)[1]
    if (is.na(app_side) || !nzchar(app_side)) {
      app_side <- classify_page_side(page_id)
    }
    if (!app_side %in% c("public", "admin", "system")) {
      app_side <- "system"
    }

    if (!is.null(duration_ms)) {
      duration_ms <- as.numeric(duration_ms)
      if (!is.finite(duration_ms) || duration_ms < 0) {
        duration_ms <- NULL
      } else {
        duration_ms <- as.integer(round(duration_ms))
      }
    }

    payload_json <- tryCatch(
      {
        jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
      },
      error = function(e) {
        "{}"
      }
    )

    usage_id_db <- as.integer(usage_id)[1]
    if (is.na(usage_id_db)) {
      return(FALSE)
    }
    page_id_db <- if (is.null(page_id) || length(page_id) == 0) {
      NA_character_
    } else {
      page_id_db <- as.character(page_id)[1]
      if (is.na(page_id_db) || !nzchar(page_id_db)) {
        NA_character_
      } else {
        page_id_db
      }
    }
    app_side_db <- as.character(app_side)[1]
    duration_ms_db <- if (is.null(duration_ms)) {
      NA_integer_
    } else {
      as.integer(round(as.numeric(duration_ms)))[1]
    }
    payload_json_db <- as.character(payload_json)[1]

    tryCatch(
      {
        DBI::dbExecute(
          session$userData$AquaCache,
          paste(
            "INSERT INTO application.shiny_app_usage_event",
            "(usage_id, event_type, page_id, app_side, duration_ms, payload)",
            "VALUES ($1, $2, $3, $4, $5, $6::jsonb);"
          ),
          params = list(
            usage_id_db,
            event_type_db,
            page_id_db,
            app_side_db,
            duration_ms_db,
            payload_json_db
          )
        )
        TRUE
      },
      error = function(e) {
        session$userData$usage_events_enabled <- FALSE
        if (!isTRUE(session$userData$usage_events_warned)) {
          warning(
            "Failed to log usage event. Event logging disabled for this session: ",
            e$message
          )
          session$userData$usage_events_warned <- TRUE
        }
        FALSE
      }
    )
  }

  flush_active_page_event <- function(reason = "page_change") {
    page_id <- active_usage_page$page_id
    entered_at <- active_usage_page$entered_at
    app_side <- active_usage_page$app_side

    if (!is.null(page_id) && !is.null(entered_at)) {
      duration_ms <- max(
        0,
        as.numeric(difftime(Sys.time(), entered_at, units = "secs")) * 1000
      )
      log_usage_event(
        event_type = "page_leave",
        page_id = page_id,
        app_side = app_side,
        duration_ms = duration_ms,
        payload = list(reason = reason)
      )
    }

    active_usage_page$page_id <- NULL
    active_usage_page$app_side <- NULL
    active_usage_page$entered_at <- NULL
  }

  start_page_tracking_for_current_page <- function(reason = "usage_switch") {
    page_id <- isolate(input$navbar)
    if (!is_leaf_page(page_id)) {
      return(invisible(FALSE))
    }

    app_side <- classify_page_side(page_id)
    active_usage_page$page_id <- page_id
    active_usage_page$app_side <- app_side
    active_usage_page$entered_at <- Sys.time()

    log_usage_event(
      event_type = "page_enter",
      page_id = page_id,
      app_side = app_side,
      payload = list(reason = reason)
    )
    invisible(TRUE)
  }

  close_usage_session <- function(
    source,
    error_message = NULL,
    login_to = NULL,
    note = NULL
  ) {
    usage_id <- session$userData$usage_id
    if (is.null(usage_id)) {
      return(FALSE)
    }
    if (isTRUE(session$userData$usage_closed)) {
      return(TRUE)
    }

    flush_active_page_event(reason = paste0("session_close_", source))

    log_usage_event(
      event_type = "session_close_attempt",
      app_side = "system",
      payload = list(source = source)
    )

    source_db <- as.character(source)[1]
    note_db <- if (is.null(note)) NA_character_ else as.character(note)[1]
    error_message_db <- if (is.null(error_message)) {
      NA_character_
    } else {
      as.character(error_message)[1]
    }
    login_to_db <- if (is.null(login_to)) {
      NA_integer_
    } else {
      as.integer(login_to)[1]
    }
    usage_id_db <- as.integer(usage_id)[1]
    if (is.na(usage_id_db)) {
      return(FALSE)
    }

    close_sql <- paste(
      "UPDATE application.shiny_app_usage",
      "SET session_end = NOW(),",
      "session_end_source = $1,",
      "session_end_note = COALESCE($2, session_end_note),",
      "error_message = COALESCE($3, error_message),",
      "login_to = COALESCE($4, login_to)",
      "WHERE id = $5 AND session_end IS NULL;"
    )
    legacy_close_sql <- paste(
      "UPDATE application.shiny_app_usage",
      "SET session_end = NOW(),",
      "error_message = COALESCE($1, error_message),",
      "login_to = COALESCE($2, login_to)",
      "WHERE id = $3 AND session_end IS NULL;"
    )
    params <- list(
      source_db,
      note_db,
      error_message_db,
      login_to_db,
      usage_id_db
    )
    legacy_params <- list(error_message_db, login_to_db, usage_id_db)

    close_error <- NULL
    used_retry <- FALSE
    used_extended_sql <- TRUE
    rows_affected <- NA_integer_
    close_sql_variant <- "extended"

    run_close <- function(con) {
      execute_close <- function(
        sql,
        sql_params,
        used_extended_sql_flag,
        sql_variant
      ) {
        tryCatch(
          {
            rows <- DBI::dbExecute(con, sql, params = sql_params)
            list(
              error = NULL,
              rows = rows,
              used_extended_sql = used_extended_sql_flag,
              close_sql_variant = sql_variant
            )
          },
          error = function(e) {
            list(
              error = e,
              rows = NA_integer_,
              used_extended_sql = used_extended_sql_flag,
              close_sql_variant = sql_variant
            )
          }
        )
      }

      minimal_close_sql <- paste(
        "UPDATE application.shiny_app_usage",
        "SET session_end = NOW()",
        "WHERE id = $1 AND session_end IS NULL;"
      )
      minimal_params <- list(usage_id_db)

      extended_result <- execute_close(
        close_sql,
        params,
        used_extended_sql_flag = TRUE,
        sql_variant = "extended"
      )
      if (is.null(extended_result$error)) {
        return(extended_result)
      }

      legacy_result <- execute_close(
        legacy_close_sql,
        legacy_params,
        used_extended_sql_flag = FALSE,
        sql_variant = "legacy"
      )
      if (is.null(legacy_result$error)) {
        return(legacy_result)
      }

      minimal_result <- execute_close(
        minimal_close_sql,
        minimal_params,
        used_extended_sql_flag = FALSE,
        sql_variant = "minimal"
      )
      if (is.null(minimal_result$error)) {
        return(minimal_result)
      }

      list(
        error = simpleError(
          paste(
            conditionMessage(extended_result$error),
            "| legacy:",
            conditionMessage(legacy_result$error),
            "| minimal:",
            conditionMessage(minimal_result$error)
          )
        ),
        rows = NA_integer_,
        used_extended_sql = FALSE,
        close_sql_variant = "failed_all"
      )
    }

    close_result <- run_close(session$userData$AquaCache)
    close_error <- close_result$error
    rows_affected <- close_result$rows
    used_extended_sql <- close_result$used_extended_sql
    close_sql_variant <- close_result$close_sql_variant

    if (!is.null(close_error)) {
      tryCatch(
        {
          retry_con <- AquaConnect(
            name = config$dbName,
            host = config$dbHost,
            port = config$dbPort,
            username = config$dbUser,
            password = config$dbPass,
            silent = TRUE
          )
          # Reset the application_name to 'YGwater_shiny'
          DBI::dbExecute(
            retry_con,
            "set application_name TO 'YGwater_shiny'"
          )
          on.exit(safe_disconnect(retry_con), add = TRUE)
          close_result <- run_close(retry_con)
          close_error <<- close_result$error
          rows_affected <<- close_result$rows
          used_extended_sql <<- close_result$used_extended_sql
          close_sql_variant <<- close_result$close_sql_variant
          used_retry <<- TRUE
        },
        error = function(e) {
          close_error <<- e
        }
      )
    }

    if (is.null(close_error)) {
      session$userData$usage_closed <- TRUE
      log_usage_event(
        event_type = "session_closed",
        app_side = "system",
        payload = list(
          source = source,
          used_retry = used_retry,
          used_extended_sql = used_extended_sql,
          close_sql_variant = close_sql_variant,
          rows_affected = rows_affected
        )
      )
      return(TRUE)
    }

    log_usage_event(
      event_type = "session_close_failed",
      app_side = "system",
      payload = list(
        source = source,
        used_retry = used_retry,
        used_extended_sql = used_extended_sql,
        close_sql_variant = close_sql_variant,
        error = conditionMessage(close_error)
      )
    )
    warning(
      "Failed to close usage session ",
      usage_id,
      " (source: ",
      source,
      "): ",
      conditionMessage(close_error)
    )
    FALSE
  }

  session$userData$usage_id <- insert_usage_session(config$dbUser)
  session$userData$usage_closed <- is.null(session$userData$usage_id)

  session$onUnhandledError(function(e) {
    msg <- if (inherits(e, "condition")) {
      conditionMessage(e)
    } else if (!is.null(e) && !is.null(e$message)) {
      e$message
    } else {
      "Unhandled error"
    }
    close_usage_session(
      source = "unhandled_error",
      error_message = msg,
      note = "onUnhandledError callback"
    )
    safe_disconnect(session$userData$AquaCache)
  })

  session$onSessionEnded(function() {
    close_usage_session(
      source = "session_end",
      note = "onSessionEnded callback"
    )
    safe_disconnect(session$userData$AquaCache)
  })

  # Heartbeat every 5 seconds to keep app alive, prevent disconnects while doing queries and rendering plots. Note: time-consuming operations can still time out unless they use ExtendedTasks as the task otherwise blocks the server.
  output$keep_alive <- renderText({
    invalidateLater(5000, session)
    Sys.time()
  })

  # session$userData$use_webgl <- !grepl('Android', session$request$HTTP_USER_AGENT, ignore.case = TRUE) # This does not work with Shiny Server open source
  session$userData$use_webgl <- FALSE # Force webgl to FALSE for now, as it causes issues when viewing plotly plots on Android devices

  # Allow re connection to the same state the app was in if disconnected (e.g. computer put to sleep, etc.)
  session$allowReconnect(TRUE)

  # Show relevant tabs for viz mode
  showViz <- function(show = TRUE) {
    nav_fun <- if (show) nav_show else nav_hide
    tabs <- c(
      "home",
      "plot",
      "maps",
      "reports",
      "images",
      "docTableView",
      "data",
      "info",
      "WWR"
    )
    for (tab in tabs) {
      nav_fun(id = "navbar", target = tab)
    }
    if (!config$public & config$g_drive) nav_fun(id = "navbar", target = "FOD")
  }
  showAdmin <- function(show = TRUE, logout = FALSE) {
    if (show) {
      # Location tasks -------------------------------------------------------
      if (
        any(
          session$userData$admin_privs$addLocation,
          session$userData$admin_privs$addSubLocation
        )
      ) {
        nav_show(id = "navbar", target = "dbLocsTasks")
        if (!isTRUE(session$userData$admin_privs$addLocation)) {
          nav_hide(id = "navbar", target = "addLocation")
        }
        if (!isTRUE(session$userData$admin_privs$addSubLocation)) {
          nav_hide(id = "navbar", target = "addSubLocation")
        }
      } else {
        nav_hide(id = "navbar", target = "dbLocsTasks")
      }

      # Equipment tasks -----------------------------------------------------
      if (isTRUE(session$userData$admin_privs$calibrate)) {
        nav_show(id = "navbar", target = "equipTasks")
      } else {
        nav_hide(id = "navbar", target = "equipTasks")
      }

      # Continuous data tasks -----------------------------------------------
      if (
        any(
          session$userData$admin_privs$addContData,
          session$userData$admin_privs$editContData,
          session$userData$admin_privs$continuousCorrections,
          session$userData$admin_privs$imputeMissing,
          session$userData$admin_privs$grades_approvals_qualifiers,
          session$userData$admin_privs$addTimeseries,
          session$userData$admin_privs$syncCont
        )
      ) {
        nav_show(id = "navbar", target = "continuousDataTasks")
        if (!isTRUE(session$userData$admin_privs$addContData)) {
          nav_hide(id = "navbar", target = "addContData")
        }
        if (!isTRUE(session$userData$admin_privs$editContData)) {
          nav_hide(id = "navbar", target = "editContData")
        }
        if (!isTRUE(session$userData$admin_privs$continuousCorrections)) {
          nav_hide(id = "navbar", target = "continuousCorrections")
        }
        if (!isTRUE(session$userData$admin_privs$imputeMissing)) {
          nav_hide(id = "navbar", target = "imputeMissing")
        }
        if (!isTRUE(session$userData$admin_privs$grades_approvals_qualifiers)) {
          nav_hide(id = "navbar", target = "grades_approvals_qualifiers")
        }
        if (!isTRUE(session$userData$admin_privs$addTimeseries)) {
          nav_hide(id = "navbar", target = "addTimeseries")
        }
        if (!isTRUE(session$userData$admin_privs$syncCont)) {
          nav_hide(id = "navbar", target = "syncCont")
        }
      } else {
        nav_hide(id = "navbar", target = "continuousDataTasks")
      }

      # Discrete data tasks --------------------------------------------------
      if (
        any(
          session$userData$admin_privs$addDiscData,
          session$userData$admin_privs$addSamples,
          session$userData$admin_privs$editDiscData,
          session$userData$admin_privs$addSampleSeries,
          session$userData$admin_privs$syncDisc,
          session$userData$admin_privs$addGuidelines
        )
      ) {
        nav_show(id = "navbar", target = "discreteDataTasks")
        if (!isTRUE(session$userData$admin_privs$addDiscData)) {
          nav_hide(id = "navbar", target = "addDiscData")
        }
        if (!isTRUE(session$userData$admin_privs$addSamples)) {
          nav_hide(id = "navbar", target = "addSamples")
        }
        if (!isTRUE(session$userData$admin_privs$editDiscData)) {
          nav_hide(id = "navbar", target = "editDiscData")
        }
        if (!isTRUE(session$userData$admin_privs$addSampleSeries)) {
          nav_hide(id = "navbar", target = "addSampleSeries")
        }
        if (!isTRUE(session$userData$admin_privs$syncDisc)) {
          nav_hide(id = "navbar", target = "syncDisc")
        }
        if (!isTRUE(session$userData$admin_privs$addGuidelines)) {
          nav_hide(id = "navbar", target = "addGuidelines")
        }
      } else {
        nav_hide(id = "navbar", target = "discreteDataTasks")
      }

      # File tasks -----------------------------------------------------------
      if (
        any(
          session$userData$admin_privs$addDocs,
          session$userData$admin_privs$addImgs,
          session$userData$admin_privs$addImgSeries
        )
      ) {
        nav_show(id = "navbar", target = "fileTasks")
        if (!isTRUE(session$userData$admin_privs$addDocs)) {
          nav_hide(id = "navbar", target = "addDocs")
        }
        if (!isTRUE(session$userData$admin_privs$addImgs)) {
          nav_hide(id = "navbar", target = "addImgs")
        }
        if (!isTRUE(session$userData$admin_privs$addImgSeries)) {
          nav_hide(id = "navbar", target = "addImgSeries")
        }
      } else {
        nav_hide(id = "navbar", target = "fileTasks")
      }

      # Field tasks ---------------------------------------------------------
      if (
        any(
          session$userData$admin_privs$visit,
          session$userData$admin_privs$deploy_recover
        )
      ) {
        nav_show(id = "navbar", target = "fieldTasks")
        if (!isTRUE(session$userData$admin_privs$visit)) {
          nav_hide(id = "navbar", target = "visit")
        }
        if (!isTRUE(session$userData$admin_privs$deploy_recover)) {
          nav_hide(id = "navbar", target = "deploy_recover")
        }
      } else {
        nav_hide(id = "navbar", target = "fieldTasks")
      }

      # Simple Index
      if (isTRUE(session$userData$admin_privs$boreholes_wells)) {
        nav_show(id = "navbar", target = "wellTasks")
      } else {
        nav_hide(id = "navbar", target = "wellTasks")
      }

      # Reference data ------------------------------------------------------
      if (
        any(
          session$userData$admin_privs$manageOrganizations,
          session$userData$admin_privs$manageNetworks,
          session$userData$admin_privs$manageProjects,
          session$userData$admin_privs$manageNetworkProjectTypes,
          session$userData$admin_privs$manageLocationTypes,
          session$userData$admin_privs$manageMediaTypes,
          session$userData$admin_privs$manageMatrixStates,
          session$userData$admin_privs$manageParameterGroups,
          session$userData$admin_privs$manageParameterSubGroups,
          session$userData$admin_privs$manageParameters,
          session$userData$admin_privs$manageCommunicationProtocolFamilies,
          session$userData$admin_privs$manageCommunicationProtocols,
          session$userData$admin_privs$manageTransmissionMethodFamilies,
          session$userData$admin_privs$manageTransmissionMethods,
          session$userData$admin_privs$manageTransmissionComponentRoles
        )
      ) {
        nav_show(id = "navbar", target = "metadataTasks")
        if (isTRUE(session$userData$admin_privs$manageOrganizations)) {
          nav_show(id = "navbar", target = "manageOrganizations")
        }
        if (!isTRUE(session$userData$admin_privs$manageOrganizations)) {
          nav_hide(id = "navbar", target = "manageOrganizations")
        }
        if (isTRUE(session$userData$admin_privs$manageNetworks)) {
          nav_show(id = "navbar", target = "manageNetworks")
        }
        if (!isTRUE(session$userData$admin_privs$manageNetworks)) {
          nav_hide(id = "navbar", target = "manageNetworks")
        }
        if (isTRUE(session$userData$admin_privs$manageProjects)) {
          nav_show(id = "navbar", target = "manageProjects")
        }
        if (!isTRUE(session$userData$admin_privs$manageProjects)) {
          nav_hide(id = "navbar", target = "manageProjects")
        }
        if (isTRUE(session$userData$admin_privs$manageNetworkProjectTypes)) {
          nav_show(id = "navbar", target = "manageNetworkProjectTypes")
        }
        if (!isTRUE(session$userData$admin_privs$manageNetworkProjectTypes)) {
          nav_hide(id = "navbar", target = "manageNetworkProjectTypes")
        }
        if (isTRUE(session$userData$admin_privs$manageLocationTypes)) {
          nav_show(id = "navbar", target = "manageLocationTypes")
        }
        if (!isTRUE(session$userData$admin_privs$manageLocationTypes)) {
          nav_hide(id = "navbar", target = "manageLocationTypes")
        }
        if (isTRUE(session$userData$admin_privs$manageMediaTypes)) {
          nav_show(id = "navbar", target = "manageMediaTypes")
        }
        if (!isTRUE(session$userData$admin_privs$manageMediaTypes)) {
          nav_hide(id = "navbar", target = "manageMediaTypes")
        }
        if (isTRUE(session$userData$admin_privs$manageMatrixStates)) {
          nav_show(id = "navbar", target = "manageMatrixStates")
        }
        if (!isTRUE(session$userData$admin_privs$manageMatrixStates)) {
          nav_hide(id = "navbar", target = "manageMatrixStates")
        }
        if (isTRUE(session$userData$admin_privs$manageParameterGroups)) {
          nav_show(id = "navbar", target = "manageParameterGroups")
        }
        if (!isTRUE(session$userData$admin_privs$manageParameterGroups)) {
          nav_hide(id = "navbar", target = "manageParameterGroups")
        }
        if (isTRUE(session$userData$admin_privs$manageParameterSubGroups)) {
          nav_show(id = "navbar", target = "manageParameterSubGroups")
        }
        if (!isTRUE(session$userData$admin_privs$manageParameterSubGroups)) {
          nav_hide(id = "navbar", target = "manageParameterSubGroups")
        }
        if (isTRUE(session$userData$admin_privs$manageParameters)) {
          nav_show(id = "navbar", target = "manageParameters")
        }
        if (!isTRUE(session$userData$admin_privs$manageParameters)) {
          nav_hide(id = "navbar", target = "manageParameters")
        }
        if (
          isTRUE(
            session$userData$admin_privs$manageCommunicationProtocolFamilies
          )
        ) {
          nav_show(
            id = "navbar",
            target = "manageCommunicationProtocolFamilies"
          )
        }
        if (
          !isTRUE(
            session$userData$admin_privs$manageCommunicationProtocolFamilies
          )
        ) {
          nav_hide(
            id = "navbar",
            target = "manageCommunicationProtocolFamilies"
          )
        }
        if (isTRUE(session$userData$admin_privs$manageCommunicationProtocols)) {
          nav_show(id = "navbar", target = "manageCommunicationProtocols")
        }
        if (
          !isTRUE(session$userData$admin_privs$manageCommunicationProtocols)
        ) {
          nav_hide(id = "navbar", target = "manageCommunicationProtocols")
        }
        if (
          isTRUE(
            session$userData$admin_privs$manageTransmissionMethodFamilies
          )
        ) {
          nav_show(id = "navbar", target = "manageTransmissionMethodFamilies")
        }
        if (
          !isTRUE(
            session$userData$admin_privs$manageTransmissionMethodFamilies
          )
        ) {
          nav_hide(id = "navbar", target = "manageTransmissionMethodFamilies")
        }
        if (isTRUE(session$userData$admin_privs$manageTransmissionMethods)) {
          nav_show(id = "navbar", target = "manageTransmissionMethods")
        }
        if (!isTRUE(session$userData$admin_privs$manageTransmissionMethods)) {
          nav_hide(id = "navbar", target = "manageTransmissionMethods")
        }
        if (
          isTRUE(
            session$userData$admin_privs$manageTransmissionComponentRoles
          )
        ) {
          nav_show(id = "navbar", target = "manageTransmissionComponentRoles")
        }
        if (
          !isTRUE(
            session$userData$admin_privs$manageTransmissionComponentRoles
          )
        ) {
          nav_hide(id = "navbar", target = "manageTransmissionComponentRoles")
        }
      } else {
        nav_hide(id = "navbar", target = "metadataTasks")
      }

      # Acquisition / telemetry --------------------------------------------
      if (
        any(
          session$userData$admin_privs$manageInstrumentConnections,
          session$userData$admin_privs$manageInstrumentConnectionSignals,
          session$userData$admin_privs$manageTransmissionSetups,
          session$userData$admin_privs$manageTransmissionRoutes,
          session$userData$admin_privs$manageTransmissionComponents
        )
      ) {
        nav_show(id = "navbar", target = "acquisitionTelemetryTasks")
        if (isTRUE(session$userData$admin_privs$manageInstrumentConnections)) {
          nav_show(id = "navbar", target = "manageInstrumentConnections")
        }
        if (!isTRUE(session$userData$admin_privs$manageInstrumentConnections)) {
          nav_hide(id = "navbar", target = "manageInstrumentConnections")
        }
        if (
          isTRUE(
            session$userData$admin_privs$manageInstrumentConnectionSignals
          )
        ) {
          nav_show(
            id = "navbar",
            target = "manageInstrumentConnectionSignals"
          )
        }
        if (
          !isTRUE(
            session$userData$admin_privs$manageInstrumentConnectionSignals
          )
        ) {
          nav_hide(
            id = "navbar",
            target = "manageInstrumentConnectionSignals"
          )
        }
        if (isTRUE(session$userData$admin_privs$manageTransmissionSetups)) {
          nav_show(id = "navbar", target = "manageTransmissionSetups")
        }
        if (!isTRUE(session$userData$admin_privs$manageTransmissionSetups)) {
          nav_hide(id = "navbar", target = "manageTransmissionSetups")
        }
        if (isTRUE(session$userData$admin_privs$manageTransmissionRoutes)) {
          nav_show(id = "navbar", target = "manageTransmissionRoutes")
        }
        if (!isTRUE(session$userData$admin_privs$manageTransmissionRoutes)) {
          nav_hide(id = "navbar", target = "manageTransmissionRoutes")
        }
        if (isTRUE(session$userData$admin_privs$manageTransmissionComponents)) {
          nav_show(id = "navbar", target = "manageTransmissionComponents")
        }
        if (
          !isTRUE(session$userData$admin_privs$manageTransmissionComponents)
        ) {
          nav_hide(id = "navbar", target = "manageTransmissionComponents")
        }
      } else {
        nav_hide(id = "navbar", target = "acquisitionTelemetryTasks")
      }

      # Admin menu ----------------------------------------------------------
      # Admin menu is always shown because every logged in user can change their own password
      nav_show(id = "navbar", target = "adminTasks")
      nav_show(id = "navbar", target = "adminHome")
      nav_show(id = "navbar", target = "changePwd")
      if (!isTRUE(session$userData$can_create_role)) {
        nav_hide(id = "navbar", target = "manageUsers")
      }
      if (!isTRUE(session$userData$admin_privs$manageNotifications)) {
        nav_hide(id = "navbar", target = "manageNotifications")
      }
      if (!isTRUE(session$userData$admin_privs$manageNewsContent)) {
        nav_hide(id = "navbar", target = "manageNewsContent")
      }
      if (!isTRUE(session$userData$admin_privs$viewFeedback)) {
        nav_hide(id = "navbar", target = "viewFeedback")
      }
    } else {
      # We're in visualize mode
      # Hide irrelevant tabs for viz mode
      for (id in c(
        "dbLocsTasks",
        "equipTasks",
        "continuousDataTasks",
        "discreteDataTasks",
        "fileTasks",
        "fieldTasks",
        "visit",
        "deploy_recover",
        "adminTasks",
        "metadataTasks",
        "acquisitionTelemetryTasks",
        "wellTasks"
      )) {
        nav_hide(id = "navbar", target = id)
      }

      if (logout) {
        shinyjs::hide("admin")
      }
    }
  }

  # Hide all 'admin' side tabs if they were generated
  if (!config$public) {
    # Immediately run the showAdmin function to hide the admin tabs, they're only shown upon login
    showAdmin(show = FALSE)
  }

  # Bookmarking and browser history navigation -------------------------------
  bookmarkable_tabs <- c(
    "home",
    "monitoringLocationsMap",
    "parameterValuesMap",
    "rasterValuesMap",
    "snowBulletinMap",
    "discPlot",
    "contPlot",
    "FOD",
    "snowInfo",
    "waterInfo",
    "waterTemp",
    "WQReport",
    "snowBulletin",
    "imgTableView",
    "imgMapView",
    "docTableView",
    "discData",
    "contData",
    "WWR",
    "news",
    "about"
  )

  updating_from_url <- reactiveVal(FALSE)
  build_query_string <- function(page = NULL, lang = NULL) {
    params <- list()
    if (!is.null(page)) {
      params$page <- page
    }
    if (!is.null(lang)) {
      params$lang <- lang
    }
    if (!length(params)) {
      return("")
    }
    encoded <- vapply(
      names(params),
      function(name) {
        value <- params[[name]]
        paste0(name, "=", utils::URLencode(value, reserved = TRUE))
      },
      character(1)
    )
    paste0("?", paste(encoded, collapse = "&"))
  }
  get_lang_code <- function() {
    if (is.null(languageSelection$language)) {
      return(NULL)
    }
    if (languageSelection$language == "Français") "fr" else "en"
  }

  observeEvent(session$clientData$url_search, ignoreNULL = FALSE, {
    updating_from_url(TRUE)
    on.exit(updating_from_url(FALSE))
    query <- shiny::parseQueryString(isolate(session$clientData$url_search))
    page <- query[["page"]]
    lang <- query[["lang"]]
    if (!is.null(lang) && nzchar(lang)) {
      language_override(TRUE)
      set_language_selection(lang)
    }
    if (
      !is.null(page) &&
        page %in% bookmarkable_tabs &&
        !identical(page, input$navbar)
    ) {
      try({
        bslib::nav_select(id = "navbar", selected = page)
      })
    }
  })

  observeEvent(
    input$navbar,
    {
      if (updating_from_url()) {
        return()
      }
      if (is.null(input$navbar)) {
        return()
      }
      page <- if (input$navbar %in% bookmarkable_tabs) input$navbar else NULL
      updateQueryString(
        build_query_string(page = page, lang = get_lang_code()),
        mode = "push"
      )
    },
    ignoreNULL = TRUE
  )

  # Usage analytics: page-level dwell tracking for all leaf pages
  observeEvent(
    input$navbar,
    {
      page_id <- input$navbar
      if (!is_leaf_page(page_id)) {
        return()
      }

      now_utc <- Sys.time()
      prev_page <- active_usage_page$page_id
      prev_entered <- active_usage_page$entered_at
      prev_side <- active_usage_page$app_side
      reason <- if (updating_from_url()) "url_nav" else "page_change"

      if (
        !is.null(prev_page) &&
          !identical(prev_page, page_id) &&
          !is.null(prev_entered)
      ) {
        duration_ms <- max(
          0,
          as.numeric(difftime(now_utc, prev_entered, units = "secs")) * 1000
        )
        log_usage_event(
          event_type = "page_leave",
          page_id = prev_page,
          app_side = prev_side,
          duration_ms = duration_ms,
          payload = list(reason = reason)
        )
      }

      if (is.null(prev_page) || !identical(prev_page, page_id)) {
        page_side <- classify_page_side(page_id)
        active_usage_page$page_id <- page_id
        active_usage_page$app_side <- page_side
        active_usage_page$entered_at <- now_utc

        log_usage_event(
          event_type = "page_enter",
          page_id = page_id,
          app_side = page_side,
          payload = list(reason = reason)
        )
      }
    },
    ignoreNULL = TRUE
  )

  # Usage analytics: generic data download clicks
  observeEvent(
    input$usage_download_click,
    {
      event <- input$usage_download_click
      if (is.null(event)) {
        return()
      }

      page_id <- if (is_leaf_page(input$navbar)) input$navbar else NULL
      app_side <- classify_page_side(page_id)

      log_usage_event(
        event_type = "data_download",
        page_id = page_id,
        app_side = app_side,
        payload = list(
          input_id = if (!is.null(event$input_id)) {
            as.character(event$input_id)
          } else {
            NULL
          },
          filename_hint = if (!is.null(event$filename_hint)) {
            as.character(event$filename_hint)
          } else {
            NULL
          },
          href = if (!is.null(event$href)) as.character(event$href) else NULL,
          ts = if (!is.null(event$ts)) as.numeric(event$ts) else NULL
        )
      )
    },
    ignoreNULL = TRUE
  )

  # Usage analytics: classify tracked input changes
  observeEvent(
    input$usage_input_changed,
    {
      event <- input$usage_input_changed
      if (is.null(event) || is.null(event$input_id)) {
        return()
      }

      input_id <- as.character(event$input_id)
      if (!nzchar(input_id)) {
        return()
      }

      page_id <- if (is_leaf_page(input$navbar)) input$navbar else NULL
      if (is.null(page_id)) {
        return()
      }
      app_side <- classify_page_side(page_id)
      event_ts <- if (!is.null(event$ts)) as.numeric(event$ts) else NULL
      current_input_value <- tryCatch(input[[input_id]], error = function(e) {
        NULL
      })
      input_value_summary <- summarize_usage_input_value(current_input_value)

      plot_request <- is_plot_trigger_input(input_id)
      map_filter <- page_id %in% map_page_ids && is_map_filter_input(input_id)
      data_request <- is_data_request_input(input_id, page_id = page_id)

      if (plot_request) {
        log_usage_event(
          event_type = "plot_request",
          page_id = page_id,
          app_side = app_side,
          payload = list(
            input_id = input_id,
            ts = event_ts,
            input_value = input_value_summary
          )
        )
      }

      if (map_filter) {
        log_usage_event(
          event_type = "map_filter_applied",
          page_id = page_id,
          app_side = app_side,
          payload = list(
            input_id = input_id,
            ts = event_ts,
            input_value = input_value_summary
          )
        )
      }

      if (data_request && !plot_request && !map_filter) {
        log_usage_event(
          event_type = "data_request",
          page_id = page_id,
          app_side = app_side,
          payload = list(
            input_id = input_id,
            ts = event_ts,
            input_value = input_value_summary
          )
        )
      }
    },
    ignoreNULL = TRUE
  )

  # Track window dimensions (used to modify plot appearance)
  windowDims <- reactive({
    req(input$window_dimensions)
    input$window_dimensions
  })

  # Initialize reactive flags to track whether each UI has been loaded
  reset_ui_loaded <- function() {
    # visualize-side modules
    ui_loaded$viz <- FALSE
    ui_loaded$admin <- FALSE
    ui_loaded$home <- FALSE
    ui_loaded$discPlot <- FALSE
    ui_loaded$contPlot <- FALSE
    ui_loaded$paramValuesMap <- FALSE
    ui_loaded$rasterValuesMap <- FALSE
    ui_loaded$monitoringLocationsMap <- FALSE
    ui_loaded$snowBulletinMap <- FALSE
    ui_loaded$FOD <- FALSE
    ui_loaded$imgTableView <- FALSE
    ui_loaded$imgMapView <- FALSE
    ui_loaded$docTableView <- FALSE
    ui_loaded$snowInfo <- FALSE
    ui_loaded$waterInfo <- FALSE
    ui_loaded$waterTemp <- FALSE
    ui_loaded$WQReport <- FALSE
    ui_loaded$snowBulletin <- FALSE
    ui_loaded$discData <- FALSE
    ui_loaded$contData <- FALSE
    ui_loaded$WWR <- FALSE
    ui_loaded$news <- FALSE
    ui_loaded$about <- FALSE

    # Admin side modules
    ui_loaded$addLocation <- FALSE
    ui_loaded$addSubLocation <- FALSE

    ui_loaded$deploy_recover <- FALSE
    ui_loaded$calibrate <- FALSE
    ui_loaded$manageInstruments <- FALSE

    ui_loaded$addContData <- FALSE
    ui_loaded$continuousCorrections <- FALSE
    ui_loaded$imputeMissing <- FALSE
    ui_loaded$editContData <- FALSE
    ui_loaded$grades_approvals_qualifiers <- FALSE
    ui_loaded$syncCont <- FALSE
    ui_loaded$addTimeseries <- FALSE

    ui_loaded$addDiscData <- FALSE
    ui_loaded$addSamples <- FALSE
    ui_loaded$editDiscData <- FALSE
    ui_loaded$addGuidelines <- FALSE
    ui_loaded$addSampleSeries <- FALSE
    ui_loaded$syncDisc <- FALSE

    ui_loaded$addDocs <- FALSE
    ui_loaded$addImgs <- FALSE
    ui_loaded$addImgSeries <- FALSE

    ui_loaded$simplerIndex <- FALSE
    ui_loaded$editBoreholesWells <- FALSE
    ui_loaded$manageBoreholeDocuments <- FALSE

    ui_loaded$manageOrganizations <- FALSE
    ui_loaded$manageNetworks <- FALSE
    ui_loaded$manageProjects <- FALSE
    ui_loaded$manageNetworkProjectTypes <- FALSE
    ui_loaded$manageLocationTypes <- FALSE
    ui_loaded$manageMediaTypes <- FALSE
    ui_loaded$manageMatrixStates <- FALSE
    ui_loaded$manageParameterGroups <- FALSE
    ui_loaded$manageParameterSubGroups <- FALSE
    ui_loaded$manageParameters <- FALSE
    ui_loaded$manageCommunicationProtocolFamilies <- FALSE
    ui_loaded$manageCommunicationProtocols <- FALSE
    ui_loaded$manageTransmissionMethodFamilies <- FALSE
    ui_loaded$manageTransmissionMethods <- FALSE
    ui_loaded$manageTransmissionComponentRoles <- FALSE
    ui_loaded$manageInstrumentConnections <- FALSE
    ui_loaded$manageInstrumentConnectionSignals <- FALSE
    ui_loaded$manageTransmissionSetups <- FALSE
    ui_loaded$manageTransmissionRoutes <- FALSE
    ui_loaded$manageTransmissionComponents <- FALSE

    ui_loaded$changePwd <- FALSE
    ui_loaded$manageUsers <- FALSE
    ui_loaded$manageNotifications <- FALSE
    ui_loaded$manageNewsContent <- FALSE
    ui_loaded$viewFeedback <- FALSE
    ui_loaded$adminHome <- FALSE

    ui_loaded$visit <- FALSE
  }

  ui_loaded <- reactiveValues()
  reset_ui_loaded() # Initialize the ui_loaded reactive values

  notification_module_choices <- c(
    "all",
    "home",
    "discPlot",
    "contPlot",
    "mapLocs",
    "mapParams",
    "mapRaster",
    "mapSnowbull",
    "snowInfo",
    "waterInfo",
    "waterTemp",
    "WQReport",
    "snowBulletin",
    "imgTableView",
    "imgMapView",
    "docTableView",
    "discData",
    "contData",
    "wellRegistry",
    "news",
    "about",
    "FOD",
    "syncCont",
    "syncDisc",
    "addLocation",
    "addSubLocation",
    "addTimeseries",
    "deploy_recover",
    "calibrate",
    "manageInstruments",
    "addContData",
    "continuousCorrections",
    "imputeMissing",
    "editContData",
    "grades_approvals_qualifiers",
    "addDiscData",
    "addSamples",
    "addSampleSeries",
    "editDiscData",
    "addGuidelines",
    "addDocs",
    "addImgs",
    "addImgSeries",
    "simplerIndex",
    "editBoreholesWells",
    "manageBoreholeDocuments",
    "manageOrganizations",
    "manageNetworks",
    "manageProjects",
    "manageNetworkProjectTypes",
    "manageLocationTypes",
    "manageMediaTypes",
    "manageMatrixStates",
    "manageParameterGroups",
    "manageParameterSubGroups",
    "manageParameters",
    "manageCommunicationProtocolFamilies",
    "manageCommunicationProtocols",
    "manageTransmissionMethodFamilies",
    "manageTransmissionMethods",
    "manageTransmissionComponentRoles",
    "manageInstrumentConnections",
    "manageInstrumentConnectionSignals",
    "manageTransmissionSetups",
    "manageTransmissionRoutes",
    "manageTransmissionComponents",
    "adminHome",
    "changePwd",
    "manageUsers",
    "manageNotifications",
    "manageNewsContent",
    "viewFeedback",
    "visit"
  )

  # Language selection ########################################################

  # Language selection reactives and observers based on the user's selected language (which is automatically set to the browser's language on load)
  languageSelection <- reactiveValues(language = NULL, abbrev = NULL) # holds language and abbreviation
  language_override <- reactiveVal(FALSE)

  app_seo_description <- function(language = NULL) {
    if (identical(language, "Français")) {
      return(
        "Explorez les donnees yukonnaises sur l'eau de surface, les eaux souterraines, l'hydrometrie, la neige et la qualite de l'eau au moyen de cartes, graphiques, tableaux et telechargements interactifs."
      )
    }

    "Explore Yukon water, groundwater, hydrometric, snow, and water quality monitoring data through interactive maps, plots, tables, and downloads."
  }

  app_brand_title <- function(language = NULL) {
    if (identical(language, "Français")) {
      return("Explorateur des donnees sur l'eau")
    }

    "Water Data Explorer"
  }

  set_language_selection <- function(lang_code) {
    lang_code <- tolower(if (is.null(lang_code)) "en" else lang_code)
    lang_code <- if (grepl("^fr", lang_code)) "fr" else "en"

    selected_lang <- if (lang_code == "fr") "Français" else "English"

    languageSelection$language <- selected_lang
    languageSelection$abbrev <- tr("titleCase", languageSelection$language)

    updateActionButton(
      session,
      "language_button",
      label = data.table::fifelse(
        selected_lang == "English",
        "Français",
        "English"
      )
    )

    # Update the HTML <head> for language settings
    session$sendCustomMessage(
      type = 'updateLang',
      message = list(lang = lang_code)
    )
  }

  # Populate the language selection dropdown
  # Determine user's browser language. This should only run once when the app is loaded.
  observe({
    shinyjs::runjs(
      "
      var language =  window.navigator.userLanguage || window.navigator.language;
      console.log('Detected browser language: ' + language);
      Shiny.setInputValue('userLang', language, {priority: 'event'});
                     "
    )
  })

  # Set initial language based on browser language
  # Check if userLang contains en or fr in the string and set the language accordingly
  observeEvent(
    input$userLang,
    {
      if (language_override()) {
        return()
      }
      # userLang is the language of the user's browser. input$userLang is created by the runjs function above and not in the UI.
      lang_code <- substr(input$userLang, 1, 2)
      set_language_selection(lang_code)
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    once = TRUE
  ) # This observeEvent should only run once when the app is loaded.

  # Toggle language when the button is pressed
  observeEvent(input$language_button, {
    next_lang <- if (languageSelection$language == "English") "fr" else "en"
    set_language_selection(next_lang)
  })

  observeEvent(
    languageSelection$language,
    {
      if (updating_from_url()) {
        return()
      }
      page <- if (
        !is.null(input$navbar) && input$navbar %in% bookmarkable_tabs
      ) {
        input$navbar
      } else {
        NULL
      }
      updateQueryString(
        build_query_string(page = page, lang = get_lang_code()),
        mode = "replace"
      )
    },
    ignoreInit = TRUE
  )

  # Render UI text based on the selected language
  observeEvent(languageSelection$language, {
    req(languageSelection$language)

    # Render the navigation bar titles based on the language
    output$homeNavTitle <- renderUI({
      tr("home", languageSelection$language)
    })
    output$mapsNavMenuTitle <- renderUI({
      tr("maps", languageSelection$language)
    })
    output$mapsNavParamsTitle <- renderUI({
      tr("maps_params", languageSelection$language)
    })
    output$mapsNavRasterTitle <- renderUI({
      tr("maps_raster", languageSelection$language)
    })
    output$mapsNavLocsTitle <- renderUI({
      tr("maps_locs", languageSelection$language)
    })
    output$mapsNavSnowbullTitle <- renderUI({
      tr("maps_snowbull", languageSelection$language)
    })
    output$plotsNavMenuTitle <- renderUI({
      tr("plots", languageSelection$language)
    })
    output$plotsNavDiscTitle <- renderUI({
      bslib::tooltip(
        trigger = list(
          tr("discrete", languageSelection$language),
          bsicons::bs_icon("info-circle-fill")
        ),
        tr("tooltip_discrete", languageSelection$language)
      )
    })
    output$plotsNavContTitle <- renderUI({
      bslib::tooltip(
        trigger = list(
          tr("continuous", languageSelection$language),
          bsicons::bs_icon("info-circle-fill")
        ),
        tr("tooltip_continuous", languageSelection$language)
      )
    })

    output$reportsNavMenuTitle <- renderUI({
      tr("reports", languageSelection$language)
    })
    output$reportsNavSnowstatsTitle <- renderUI({
      tr("reports_snow", languageSelection$language)
    })
    output$reportsNavWaterTitle <- renderUI({
      tr("reports_water", languageSelection$language)
    })
    output$reportsNavWaterTempTitle <- renderUI({
      tr("reports_watertemp", languageSelection$language)
    })
    output$reportsNavWQTitle <- renderUI({
      tr("reports_wq", languageSelection$language)
    })
    output$reportsNavSnowbullTitle <- renderUI({
      tr("reports_snowbull", languageSelection$language)
    })
    output$reportsNavWaterTempTitle <- renderUI({
      tr("reports_waterTemp", languageSelection$language)
    })

    output$dataNavMenuTitle <- renderUI({
      tr("data", languageSelection$language)
    })
    output$dataNavDiscTitle <- renderUI({
      bslib::tooltip(
        trigger = list(
          tr("discrete", languageSelection$language),
          bsicons::bs_icon("info-circle-fill")
        ),
        tr("tooltip_discrete", languageSelection$language)
      )
    })
    output$dataNavContTitle <- renderUI({
      bslib::tooltip(
        trigger = list(
          tr("continuous", languageSelection$language),
          bsicons::bs_icon("info-circle-fill")
        ),
        tr("tooltip_continuous", languageSelection$language)
      )
    })

    output$imagesNavMenuTitle <- renderUI({
      tr("images", languageSelection$language)
    })
    output$imagesNavTableTitle <- renderUI({
      tr("images_table", languageSelection$language)
    })
    output$imagesNavMapTitle <- renderUI({
      tr("images_map", languageSelection$language)
    })

    output$documentsNavMenuTitle <- renderUI({
      tr("documents", languageSelection$language)
    })

    output$infoNavMenuTitle <- renderUI({
      tr("info", languageSelection$language)
    })
    output$infoNavNewsTitle <- renderUI({
      tr("info_news", languageSelection$language)
    })
    output$infoNavAboutTitle <- renderUI({
      tr("info_about", languageSelection$language)
    })
    output$WWRNavTitle <- renderUI({
      tr("wwr_title", languageSelection$language)
    })
    output$changePwdNavTitle <- renderUI({
      tr("changepwd_nav", languageSelection$language)
    })

    output$FODNavTitle <- renderUI({
      tr("fod_comments", languageSelection$language)
    })

    session$sendCustomMessage(
      "updateTitle",
      tr("title", languageSelection$language)
    ) # Update the browser title of the app based on the selected language
    session$sendCustomMessage(
      "updateBrandTitle",
      app_brand_title(languageSelection$language)
    )
    session$sendCustomMessage(
      "updateSeo",
      list(
        title = tr("title", languageSelection$language),
        description = app_seo_description(languageSelection$language)
      )
    )

    if (!config$public) {
      updateActionButton(
        session,
        "loginBtn",
        label = tr("login", languageSelection$language)
      )
      updateActionButton(
        session,
        "logoutBtn",
        label = tr("logout", languageSelection$language)
      )
    }

    # Render the footer based on the language
    output$footer_ui <- renderUI({
      div(
        span(
          tr("feedback_text", languageSelection$language),
          # Make 'buttons' that are bs_icons with a thumbs up and thumbs down and add a click event to them
          actionButton(
            "thumbs_up",
            label = bsicons::bs_icon(
              "hand-thumbs-up",
              size = "2em",
              fill = "#244C5A"
            ),
            class = "btn btn-link",
            width = "50px",
            style = "border: none; border-color: transparent; box-shadow: none; outline: none; background: transparent; -webkit-appearance: none; appearance: none;"
          ),
          actionButton(
            "thumbs_down",
            label = bsicons::bs_icon(
              "hand-thumbs-down",
              size = "2em",
              fill = "#244C5A"
            ),
            class = "btn btn-link",
            width = "50px",
            style = "border: none; border-color: transparent; box-shadow: none; outline: none; background: transparent; -webkit-appearance: none; appearance: none;"
          )
        ),
        # Set background color of div
        style = "background-color: white; padding: 10px; text-align: left; margin-bottom: 5px;",
        # Make a placeholder for feedback text and submit button
        uiOutput("feedback_ui")
      )
    })
  }) # No need for a bindEvent as this rendering is triggered by a language change

  # ObserveEvents for thumbs up/down buttons
  # add a textAreaInput to allow the user to write something, and a 'submit feedback' button
  feedback <- reactiveValues(type = NULL)

  observeEvent(input$thumbs_up, {
    if (!is.null(feedback$type)) {
      if (feedback$type) {
        # Means we're clicking on it again, needs to close
        shinyjs::hide("feedback_text")
        shinyjs::hide("submit_feedback")
        feedback$type <- NULL
      } else {
        # Means we're clicking on thumbs up after thumbs down, so update the placeholder text
        output$feedback_ui <- renderUI({
          div(
            textAreaInput(
              "feedback_text",
              label = NULL,
              placeholder = tr(
                "feedback_placeholder_up",
                languageSelection$language
              ),
              rows = 3,
              width = "100%"
            ),
            actionButton(
              "submit_feedback",
              tr("submit", languageSelection$language),
              class = "btn btn-primary"
            )
          )
        })
        feedback$type <- TRUE
      }
    } else {
      output$feedback_ui <- renderUI({
        div(
          textAreaInput(
            "feedback_text",
            label = NULL,
            placeholder = tr(
              "feedback_placeholder_up",
              languageSelection$language
            ),
            rows = 3,
            width = "100%"
          ),
          actionButton(
            "submit_feedback",
            tr("submit", languageSelection$language),
            class = "btn btn-primary"
          )
        )
      })
      feedback$type <- TRUE
    }
    # scroll down to the feedback text area
    session$onFlushed(
      function() {
        runjs(
          "document.getElementById('feedback_text')
              .scrollIntoView({behavior:'smooth', block:'center'});"
        )
      },
      once = TRUE
    )
  })

  observeEvent(input$thumbs_down, {
    if (!is.null(feedback$type)) {
      if (!feedback$type) {
        # Means we're clicking on it again, needs to close
        shinyjs::hide("feedback_text")
        shinyjs::hide("submit_feedback")
        feedback$type <- NULL
      } else {
        # Means we're clicking on thumbs up after thumbs down, so update the placeholder text
        output$feedback_ui <- renderUI({
          div(
            textAreaInput(
              "feedback_text",
              label = NULL,
              placeholder = tr(
                "feedback_placeholder_down",
                languageSelection$language
              ),
              rows = 3,
              width = "100%"
            ),
            actionButton(
              "submit_feedback",
              tr("submit", languageSelection$language),
              class = "btn btn-primary"
            )
          )
        })
        feedback$type <- FALSE
      }
    } else {
      output$feedback_ui <- renderUI({
        div(
          textAreaInput(
            "feedback_text",
            label = NULL,
            placeholder = tr(
              "feedback_placeholder_down",
              languageSelection$language
            ),
            rows = 3,
            width = "100%"
          ),
          actionButton(
            "submit_feedback",
            tr("submit", languageSelection$language),
            class = "btn btn-primary"
          )
        )
      })
      feedback$type <- FALSE
    }
    # scroll down to the feedback text area
    session$onFlushed(
      function() {
        runjs(
          "document.getElementById('feedback_text')
              .scrollIntoView({behavior:'smooth', block:'center'});"
        )
      },
      once = TRUE
    )
  })

  # Handle feedback submission
  observeEvent(input$submit_feedback, {
    # Save feedback to the database
    DBI::dbExecute(
      session$userData$AquaCache,
      "INSERT INTO application.feedback (sentiment, comment, page, app_state) VALUES ($1, $2, $3, $4);",
      params = list(
        feedback$type,
        input$feedback_text,
        input$navbar,
        jsonlite::toJSON(
          reactiveValuesToList(input),
          auto_unbox = TRUE
        )
      )
    )

    # Reset feedback
    shinyjs::hide("feedback_text")
    shinyjs::hide("submit_feedback")
    feedback$type <- NULL
  })

  # Log in/out ##########################################
  log_attempts <- reactiveVal(0) # counter for login attempts - prevent brute force attacks
  login_in_progress <- reactiveVal(FALSE) # prevent duplicate login submissions
  session$userData$user_logged_in <- FALSE # value to track login status
  session$userData$admin_button_inserted <- FALSE # track whether the admin toggle has been added
  session$userData$can_create_role <- FALSE # track CREATE ROLE privilege
  session$userData$table_privs <- data.frame() # track table privileges
  session$userData$admin_privs <- list() # store privilege flags for UI filtering

  # Track the user's last activity
  session$userData$last_activity <- reactiveVal(Sys.time())
  # 1 minute for testing
  inactivity_timeout_secs <- config$logout_timer_min * 60
  # Run a check every 30 seconds
  inactivity_check <- reactiveTimer(30000, session)

  observeEvent(
    input$user_last_activity,
    {
      session$userData$last_activity(as.POSIXct(
        input$user_last_activity / 1000,
        origin = "1970-01-01",
        tz = "UTC"
      ))
    },
    ignoreInit = TRUE
  )

  clear_modals <- function() {
    removeModal()
    shinyjs::runjs(
      "$('.modal-backdrop').remove();$('body').removeClass('modal-open');$('body').css('padding-right','');"
    )
  }

  perform_logout <- function(show_idle_modal = FALSE) {
    if (!isTRUE(session$userData$user_logged_in)) {
      return()
    }

    session$userData$user_logged_in <- FALSE # Set login status to FALSE
    session$userData$can_create_role <- FALSE
    session$userData$table_privs <- data.frame() # Reset table privileges
    session$userData$admin_privs <- list() # Reset privilege flags
    session$userData$last_activity(NULL)

    if (show_idle_modal) {
      clear_modals() # Remove any existing modals
      showModal(modalDialog(
        title = tr("logout_inactive_title", languageSelection$language),
        tr("logout_inactive_msg", languageSelection$language),
        easyClose = TRUE,
        footer = modalButton(tr("close", languageSelection$language))
      ))
    }

    # change the 'Logout' button back to 'Login'
    shinyjs::hide("logoutBtn")
    shinyjs::show("loginBtn")

    # Remove the 'admin' button upon logout
    if (isTRUE(session$userData$admin_button_inserted)) {
      removeUI(selector = "button#admin", multiple = TRUE, immediate = TRUE)
      session$userData$admin_button_inserted <- FALSE
    }

    new_id <- insert_usage_session(config$dbUser)
    close_usage_session(
      source = if (show_idle_modal) "logout_idle" else "logout",
      login_to = new_id,
      note = if (show_idle_modal) {
        "automatic logout due to inactivity"
      } else {
        "manual logout"
      }
    )

    session$userData$usage_id <- new_id
    session$userData$usage_closed <- is.null(new_id)
    if (!is.null(new_id)) {
      start_page_tracking_for_current_page(reason = "logout_switch")
    }

    # Drop old connection
    safe_disconnect(session$userData$AquaCache)
    # Re-create the connection with the base 'config' parameters, no edit privileges
    session$userData$AquaCache <- AquaConnect(
      name = config$dbName,
      host = config$dbHost,
      port = config$dbPort,
      username = config$dbUser,
      password = config$dbPass,
      silent = TRUE
    )
    # Reset the application_name to 'YGwater_shiny'
    DBI::dbExecute(
      session$userData$AquaCache,
      "set application_name TO 'YGwater_shiny'"
    )

    # Reset the session userData with the default credentials
    session$userData$config$dbUser <- config$dbUser
    session$userData$config$dbPass <- config$dbPass

    showAdmin(show = FALSE, logout = TRUE) # Hide admin tabs and remove logout button
    showViz(show = TRUE) # Restore public/viz tabs immediately without relying on the removed admin button

    # Clear the app_cache environment
    session$userData$app_cache <- new.env(parent = emptyenv())
    # Reset all ui_loaded flags to FALSE so that they all reload data when the user clicks on them
    reset_ui_loaded()
    # Send the user back to the 'home' tab if they were elsewhere
    updateTabsetPanel(session, "navbar", selected = "home")

    # Logout returns the shell to visualize mode, so keep the toggle state consistent for the next login.
    admin_vis_flag("admin")

    # logout button is disabled on click to prevent multiple rapid clicks; re-enable after logout performed (it's hidden at this point but needs to be enabled for later use)
    shinyjs::runjs("$('#logoutBtn').prop('disabled', false);")
  }

  observe({
    inactivity_check()
    if (!isTRUE(session$userData$user_logged_in)) {
      return()
    }
    last_activity <- session$userData$last_activity()
    if (is.null(last_activity)) {
      return()
    }
    idle_seconds <- as.numeric(difftime(
      Sys.time(),
      last_activity,
      units = "secs"
    ))
    if (!is.na(idle_seconds) && idle_seconds > inactivity_timeout_secs) {
      perform_logout(show_idle_modal = TRUE)
    }
  })

  ## Log in #########
  # Login UI elements are not created if YGwater() is launched in public mode, in which case this code would not run
  observeEvent(input$loginBtn, {
    req(languageSelection$language) # Ensure language is set before proceeding (might not be yet if the app is still loading)
    clear_modals() # Remove any existing modals
    # Check if the user has exceeded the maximum number of login attempts
    if (log_attempts() > 5) {
      showModal(modalDialog(
        title = tr("login_fail", languageSelection$language),
        tr("login_fail_attempts", languageSelection$language),
        easyClose = TRUE,
        footer = modalButton(tr("close", languageSelection$language))
      ))
      return()
      # button was disabled on click by 'onclick = "$(this).prop('disabled', true);"'; not re-enabled here since we want to prevent further login attempts after too many failed tries
    } else {
      showModal(modalDialog(
        # html below allows the user to press 'Enter' to submit the login form
        tags$script(HTML(
          "
          $(document).off('keyup.login').on('keyup.login', function(event) {
            if ($('#password').is(':focus') && (event.keyCode == 13)) {
              $('#confirmLogin').click();
            }
          });
          "
        )),
        title = tr("login", languageSelection$language),
        renderUI(HTML(
          tr("login_txt", languageSelection$language),
          "<br> <br>"
        )),
        textInput("username", tr("un", languageSelection$language)),
        passwordInput("password", tr("pwd", languageSelection$language)),
        footer = tagList(
          modalButton(tr("close", languageSelection$language)),
          actionButton(
            "confirmLogin",
            tr("login_confirm", languageSelection$language),
            class = "btn-primary",
            onclick = "$(this).prop('disabled', true);"
          )
        )
      ))
      # loginBtn button was disabled on click by 'onclick = "$(this).prop('disabled', true);"'; re-enable it so it's available after the modal is closed.
      shinyjs::runjs("$('#loginBtn').prop('disabled', false);")
    }
  })

  # Log in attempt if the button is clicked
  observeEvent(input$confirmLogin, {
    if (
      isTRUE(login_in_progress()) || isTRUE(session$userData$user_logged_in)
    ) {
      return()
    }

    login_in_progress(TRUE)
    on.exit(
      {
        login_in_progress(FALSE)
      },
      add = TRUE
    )

    if (nchar(input$username) == 0 || nchar(input$password) == 0) {
      clear_modals()
      showModal(modalDialog(
        title = tr("login_fail", languageSelection$language),
        tr("login_fail_missing", languageSelection$language),
        easyClose = TRUE,
        footer = modalButton(tr("close", languageSelection$language))
      ))
      return()
    }
    log_attempts(log_attempts() + 1)

    tryCatch(
      {
        session$userData$AquaCache_new <- AquaConnect(
          name = session$userData$config$dbName,
          host = session$userData$config$dbHost,
          port = session$userData$config$dbPort,
          username = input$username,
          password = input$password,
          silent = TRUE
        )
        # Test the connection
        test <- DBI::dbGetQuery(session$userData$AquaCache_new, "SELECT 1;")
        if (nrow(test) > 0) {
          # Means the connection was successful

          # Drop the old connection
          safe_disconnect(session$userData$AquaCache)
          session$userData$AquaCache <- session$userData$AquaCache_new
          session$userData$AquaCache_new <- NULL

          # Update the session with the new user's credentials
          session$userData$config$dbUser <- input$username
          session$userData$config$dbPass <- input$password

          # Reset the application_name to 'YGwater_shiny'
          DBI::dbExecute(
            session$userData$AquaCache,
            "set application_name TO 'YGwater_shiny'"
          )

          # Check if the user has more than SELECT privileges on relevant tables, used to determine if the 'admin' tab should be shown

          # Show list of tables that the user has more than SELECT privileges on:
          sql <- "
        WITH tbls AS (
          SELECT n.nspname AS schema, c.relname AS table_name, c.oid AS tbl_oid
          FROM pg_class c
          JOIN pg_namespace n ON n.oid = c.relnamespace
          WHERE c.relkind IN ('r','p')
            AND n.nspname IN ('public','continuous','discrete','boreholes','files','application','instruments', 'field')
        )
        SELECT t.schema,
               t.table_name,
               string_agg(p.priv, ', ' ORDER BY p.priv) AS extra_privileges
        FROM tbls t
        CROSS JOIN LATERAL unnest(ARRAY['SELECT', 'INSERT','UPDATE','DELETE','TRUNCATE','REFERENCES','TRIGGER']) AS p(priv)
        WHERE has_table_privilege($1, t.tbl_oid, p.priv)
        GROUP BY t.schema, t.table_name
        ORDER BY t.schema, t.table_name;"

          # Store the table privileges in the session for use in other modules or to show/hide certain UI elements
          session$userData$table_privs <- DBI::dbGetQuery(
            session$userData$AquaCache,
            sql,
            params = list(session$userData$config$dbUser)
          )
          # Create a qualified name column for easier filtering
          session$userData$table_privs$qual_name <- paste0(
            session$userData$table_privs$schema,
            ".",
            session$userData$table_privs$table_name
          )

          # If application.feedback is present but only has INSERT privileges, remove it
          if (
            session$userData$table_privs$extra_privileges[
              session$userData$table_privs$qual_name == "application.feedback"
            ] ==
              "INSERT"
          ) {
            session$userData$table_privs <- session$userData$table_privs[
              !session$userData$table_privs$qual_name == "application.feedback",
            ]
          }

          lookup_table_privs <- c(
            network_project_types = has_priv(
              tbl = session$userData$table_privs,
              "public.network_project_types",
              list(c("SELECT", "INSERT", "UPDATE"))
            ),
            location_types = has_priv(
              tbl = session$userData$table_privs,
              "public.location_types",
              list(c("SELECT", "INSERT", "UPDATE"))
            ),
            media_types = has_priv(
              tbl = session$userData$table_privs,
              "public.media_types",
              list(c("SELECT", "INSERT", "UPDATE"))
            ),
            matrix_states = has_priv(
              tbl = session$userData$table_privs,
              "public.matrix_states",
              list(c("SELECT", "INSERT", "UPDATE"))
            ),
            parameter_groups = has_priv(
              tbl = session$userData$table_privs,
              "public.parameter_groups",
              list(c("SELECT", "INSERT", "UPDATE"))
            ),
            parameter_sub_groups = has_priv(
              tbl = session$userData$table_privs,
              "public.parameter_sub_groups",
              list(c("SELECT", "INSERT", "UPDATE"))
            ),
            parameters = has_priv(
              tbl = session$userData$table_privs,
              c(
                "public.parameters",
                "public.parameter_relationships",
                "public.parameter_groups",
                "public.parameter_sub_groups"
              ),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT", "INSERT", "DELETE"),
                c("SELECT"),
                c("SELECT")
              )
            ),
            communication_protocol_families = has_priv(
              tbl = session$userData$table_privs,
              "instruments.communication_protocol_families",
              list(c("SELECT", "INSERT", "UPDATE"))
            ),
            communication_protocols = has_priv(
              tbl = session$userData$table_privs,
              c(
                "instruments.communication_protocols",
                "instruments.communication_protocol_families"
              ),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT")
              )
            ),
            transmission_method_families = has_priv(
              tbl = session$userData$table_privs,
              "instruments.transmission_method_families",
              list(c("SELECT", "INSERT", "UPDATE"))
            ),
            transmission_methods = has_priv(
              tbl = session$userData$table_privs,
              c(
                "instruments.transmission_methods",
                "instruments.transmission_method_families"
              ),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT")
              )
            ),
            transmission_component_roles = has_priv(
              tbl = session$userData$table_privs,
              "instruments.transmission_component_roles",
              list(c("SELECT", "INSERT", "UPDATE"))
            )
          )

          session$userData$admin_privs <- list(
            addLocation = has_priv(
              tbl = session$userData$table_privs,
              c(
                "public.locations",
                "public.locations_networks",
                "public.locations_projects",
                "public.networks",
                "public.projects"
              ),
              list(
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c("INSERT"),
                c("INSERT")
              )
            ),
            addSubLocation = has_priv(
              tbl = session$userData$table_privs,
              c("public.sub_locations", "public.locations"),
              list(
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c("SELECT")
              )
            ),
            calibrate = has_priv(
              tbl = session$userData$table_privs,
              c(
                "instruments.calibrations",
                "instruments.calibrate_ph",
                "instruments.calibrate_temperature",
                "instruments.calibrate_orp",
                "instruments.calibrate_specific_conductance",
                "instruments.calibrate_turbidity",
                "instruments.calibrate_dissolved_oxygen",
                "instruments.calibrate_depth",
                "instruments.instruments",
                "instruments.instrument_maintenance",
                "instruments.array_maintenance_changes",
                "instruments.sensors",
                "instruments.sensor_types",
                "instruments.instrument_make",
                "instruments.instrument_model",
                "instruments.instrument_type",
                "instruments.observers",
                "public.organizations"
              ),
              list(
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c("INSERT"),
                c("INSERT"),
                c("INSERT"),
                c("INSERT"),
                c("INSERT"),
                c("INSERT"),
                c("INSERT")
              )
            ),
            deploy_recover = has_priv(
              tbl = session$userData$table_privs,
              c(
                "public.locations_metadata_instruments",
                "public.locations",
                "public.sub_locations",
                "public.locations_z",
                "instruments.instruments",
                "instruments.instrument_make",
                "instruments.instrument_model",
                "instruments.instrument_type"
              ),
              list(
                c(
                  "SELECT",
                  "INSERT",
                  "UPDATE"
                ),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT")
              )
            ),
            addContData = has_priv(
              tbl = session$userData$table_privs,
              "continuous.measurements_continuous",
              list(c(
                "INSERT",
                "UPDATE"
              ))
            ),
            editContData = has_priv(
              tbl = session$userData$table_privs,
              "continuous.measurements_continuous",
              list(c(
                "UPDATE",
                "DELETE"
              ))
            ),
            continuousCorrections = has_priv(
              tbl = session$userData$table_privs,
              "continuous.corrections",
              list(c("INSERT", "UPDATE"))
            ),
            imputeMissing = has_priv(
              tbl = session$userData$table_privs,
              "continuous.measurements_continuous"
            ),
            grades_approvals_qualifiers = has_priv(
              tbl = session$userData$table_privs,
              c(
                "continuous.grades",
                "continuous.approvals",
                "continuous.qualifiers"
              )
            ),
            addTimeseries = has_priv(
              tbl = session$userData$table_privs,
              c(
                "continuous.timeseries",
                "public.locations_z",
                "public.organizations"
              ),
              list(
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT"
                ),
                c("INSERT")
              )
            ),
            syncCont = has_priv(
              tbl = session$userData$table_privs,
              c(
                "continuous.measurements_continuous",
                "continuous.measurements_calculated_daily",
                "continuous.timeseries"
              ),
              list(
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c("UPDATE")
              )
            ),
            addDiscData = has_priv(
              tbl = session$userData$table_privs,
              c(
                "application.discrete_mappings",
                "files.documents",
                "discrete.samples",
                "discrete.results"
              ),
              list(
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c("INSERT"),
                c("INSERT"),
                c("INSERT")
              )
            ),
            addSamples = has_priv(
              tbl = session$userData$table_privs,
              "discrete.samples",
              list(c(
                "INSERT",
                "UPDATE"
              ))
            ),
            editDiscData = has_priv(
              tbl = session$userData$table_privs,
              c("discrete.samples", "discrete.results"),
              list(
                c(
                  "UPDATE",
                  "DELETE"
                ),
                c(
                  "UPDATE",
                  "DELETE"
                )
              )
            ),
            addSampleSeries = has_priv(
              tbl = session$userData$table_privs,
              c("discrete.sample_series", "public.organizations"),
              list(
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c("INSERT")
              )
            ),
            syncDisc = has_priv(
              tbl = session$userData$table_privs,
              c(
                "discrete.sample_series",
                "discrete.samples",
                "discrete.results"
              ),
              list(
                c("UPDATE"),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                )
              )
            ),
            addGuidelines = has_priv(
              tbl = session$userData$table_privs,
              c(
                "discrete.guidelines",
                "discrete.samples",
                "discrete.results"
              ),
              list(
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c("INSERT"),
                c("INSERT")
              )
            ),
            addDocs = has_priv(
              tbl = session$userData$table_privs,
              "files.documents",
              list(c("INSERT"))
            ),
            addImgs = has_priv(
              tbl = session$userData$table_privs,
              "files.images",
              list(c("INSERT"))
            ),
            addImgSeries = has_priv(
              tbl = session$userData$table_privs,
              c("files.image_series", "public.organizations"),
              list(
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c("INSERT")
              )
            ),
            boreholes_wells = has_priv(
              tbl = session$userData$table_privs,
              c(
                "boreholes.boreholes",
                "boreholes.wells",
                "boreholes.drillers",
                "boreholes.borehole_well_purposes"
              )
              # Insert, update, delete by default
            ),
            manageOrganizations = has_priv(
              tbl = session$userData$table_privs,
              "public.organizations",
              list(c("SELECT", "INSERT", "UPDATE"))
            ),
            manageNetworks = has_priv(
              tbl = session$userData$table_privs,
              c("public.networks", "public.network_project_types"),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT")
              )
            ),
            manageProjects = has_priv(
              tbl = session$userData$table_privs,
              c("public.projects", "public.network_project_types"),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT")
              )
            ),
            lookup_tables = lookup_table_privs,
            manageNetworkProjectTypes = lookup_table_privs[[
              "network_project_types"
            ]],
            manageLocationTypes = lookup_table_privs[["location_types"]],
            manageMediaTypes = lookup_table_privs[["media_types"]],
            manageMatrixStates = lookup_table_privs[["matrix_states"]],
            manageParameterGroups = lookup_table_privs[["parameter_groups"]],
            manageParameterSubGroups = lookup_table_privs[[
              "parameter_sub_groups"
            ]],
            manageParameters = lookup_table_privs[["parameters"]],
            manageCommunicationProtocolFamilies = lookup_table_privs[[
              "communication_protocol_families"
            ]],
            manageCommunicationProtocols = lookup_table_privs[[
              "communication_protocols"
            ]],
            manageTransmissionMethodFamilies = lookup_table_privs[[
              "transmission_method_families"
            ]],
            manageTransmissionMethods = lookup_table_privs[[
              "transmission_methods"
            ]],
            manageTransmissionComponentRoles = lookup_table_privs[[
              "transmission_component_roles"
            ]],
            manageInstrumentConnections = has_priv(
              tbl = session$userData$table_privs,
              c(
                "public.locations_metadata_instrument_connections",
                "public.locations_metadata_instruments",
                "public.locations",
                "instruments.instruments",
                "instruments.instrument_make",
                "instruments.instrument_model",
                "instruments.instrument_type",
                "instruments.communication_protocols"
              ),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT")
              )
            ),
            manageInstrumentConnectionSignals = has_priv(
              tbl = session$userData$table_privs,
              c(
                "public.locations_metadata_instrument_connection_signals",
                "public.locations_metadata_instrument_connections",
                "public.locations_metadata_instruments",
                "instruments.instruments",
                "instruments.communication_protocols",
                "public.parameters",
                "continuous.timeseries",
                "public.locations",
                "public.media_types",
                "public.matrix_states",
                "public.units"
              ),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT")
              )
            ),
            manageTransmissionSetups = has_priv(
              tbl = session$userData$table_privs,
              c(
                "public.locations_metadata_transmission_setups",
                "public.locations_metadata_instruments",
                "instruments.instruments",
                "instruments.transmission_methods"
              ),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT")
              )
            ),
            manageTransmissionRoutes = has_priv(
              tbl = session$userData$table_privs,
              c(
                "public.locations_metadata_transmission_routes",
                "public.locations_metadata_transmission_setups",
                "public.locations_metadata_instruments",
                "instruments.instruments",
                "instruments.transmission_methods"
              ),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT")
              )
            ),
            manageTransmissionComponents = has_priv(
              tbl = session$userData$table_privs,
              c(
                "public.locations_metadata_transmission_components",
                "public.locations_metadata_transmission_setups",
                "public.locations_metadata_instruments",
                "public.locations",
                "instruments.instruments",
                "instruments.instrument_make",
                "instruments.instrument_model",
                "instruments.instrument_type",
                "instruments.transmission_methods",
                "instruments.transmission_component_roles"
              ),
              list(
                c("SELECT", "INSERT", "UPDATE"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT"),
                c("SELECT")
              )
            ),
            visit = has_priv(
              tbl = session$userData$table_privs,
              c(
                "field.field_visits",
                "field.field_visit_instruments"
              ),
              list(
                c(
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT"
                )
              )
            ),
            manageNewsContent = has_priv(
              tbl = session$userData$table_privs,
              c(
                "application.images",
                "application.text",
                "application.page_content"
              ),
              list(
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                ),
                c(
                  "DELETE",
                  "INSERT",
                  "UPDATE"
                )
              )
            ),
            manageNotifications = has_priv(
              tbl = session$userData$table_privs,
              "application.notifications",
              list(c(
                "INSERT",
                "SELECT",
                "UPDATE"
              ))
            ),
            viewFeedback = has_priv(
              tbl = session$userData$table_privs,
              "application.feedback",
              list(c("SELECT"))
            )
          )

          # IF the user has more than SELECT privileges on any tables, show the 'admin' button
          has_admin_privs <- FALSE
          if (nrow(session$userData$table_privs) > 0) {
            has_admin_privs <- any(vapply(
              session$userData$table_privs$extra_privileges,
              function(privs) {
                priv_list <- unlist(strsplit(privs, ", "))
                any(priv_list != "SELECT")
              },
              logical(1)
            ))
          }
          if (has_admin_privs) {
            if (!isTRUE(session$userData$admin_button_inserted)) {
              # Create the new element for the 'admin' mode
              # Other tabs are created if/when the user clicks on the 'admin' actionButton
              nav_insert(
                "navbar",
                nav_item(tagList(actionButton(
                  "admin",
                  "Switch to Admin mode",
                  style = "color: #F2A900;"
                ))),
                target = "home",
                position = "before"
              )
              session$userData$admin_button_inserted <- TRUE
            }

            # Check if the user has CREATE ROLE privileges, used to determine if the 'manage users' tab should be shown in admin mode
            session$userData$can_create_role <- DBI::dbGetQuery(
              session$userData$AquaCache,
              'SELECT rolcreaterole FROM pg_roles WHERE rolname = current_user;'
            )[1, 1]
          } # else the button just won't be created/shown

          # Set the login status to TRUE
          session$userData$user_logged_in <- TRUE

          # change the 'Login' button to 'Logout'
          shinyjs::hide("loginBtn")
          shinyjs::show("logoutBtn")

          # Initialize a fresh cache environment for the session
          session$userData$app_cache <- new.env(parent = emptyenv())

          # Reset all ui_loaded flags to FALSE so that they all reload data when the user clicks on them
          reset_ui_loaded()

          # Send the user back to the 'home' tab if they were elsewhere
          updateTabsetPanel(session, "navbar", selected = "home")

          # Select the last tab the user was on in viz mode. This will reload the module since the tab was previously set to 'home'.
          updateTabsetPanel(session, "navbar", selected = last_viz_tab())

          clear_modals()
          showModal(modalDialog(
            title = tr("login_success", languageSelection$language),
            paste0(
              tr("login_success_msg", languageSelection$language),
              " ",
              input$username
            ),
            easyClose = TRUE,
            footer = modalButton(tr("close", languageSelection$language))
          ))

          new_id <- insert_usage_session(input$username)
          close_usage_session(
            source = "login",
            login_to = new_id,
            note = paste0("successful login: ", input$username)
          )

          session$userData$usage_id <- new_id
          session$userData$usage_closed <- is.null(new_id)
          if (!is.null(new_id)) {
            start_page_tracking_for_current_page(reason = "login_switch")
          }

          return()
        } else {
          # Connection failed (without throwing an explicit error) or could not see any records
          clear_modals()
          showModal(modalDialog(
            title = tr("login_fail", languageSelection$language),
            tr("login_fail_msg", languageSelection$language),
            easyClose = TRUE,
            footer = modalButton(tr("close", languageSelection$language))
          ))
          # attempt a disconnect of the new connection
          try({
            DBI::dbDisconnect(session$userData$AquaCache_new)
          })
          return()
        }
      },
      error = function(e) {
        # Connection failed with error
        clear_modals()
        showModal(modalDialog(
          title = tr("login_fail", languageSelection$language),
          tr("login_fail_msg", languageSelection$language),
          easyClose = TRUE,
          footer = modalButton(tr("close", languageSelection$language))
        ))
        # attempt a disconnect of the new connection
        try({
          DBI::dbDisconnect(session$userData$AquaCache_new)
        })
        return()
      }
    )
  })

  ## Log out #####################################################
  observeEvent(input$logoutBtn, {
    perform_logout(show_idle_modal = FALSE)
  })

  # Load modules based on input$navbar ################################
  # Store information to pass between modules
  moduleOutputs <- reactiveValues()

  # Initialize reactive values to store last tabs for each mode
  last_viz_tab <- reactiveVal("home") # Default tab for viz mode
  last_admin_tab <- reactiveVal("adminHome") # Default tab for admin mode

  # Move between admin/visualize modes
  admin_vis_flag <- reactiveVal("admin")
  observeEvent(input$admin, {
    if (admin_vis_flag() == "viz") {
      # Set the flag before changing the tab programmatically

      updateActionButton(session, "admin", label = "Switch to Admin mode")

      # Show relevant tabs for viz mode
      showViz(show = TRUE)

      # Hide irrelevant tabs for viz mode
      showAdmin(show = FALSE)

      # Select the last tab the user was on in viz mode
      updateTabsetPanel(session, "navbar", selected = last_viz_tab())

      admin_vis_flag("admin")
    } else if (admin_vis_flag() == "admin") {
      updateActionButton(session, "admin", label = "Switch to Vizualize mode")

      # Show relevant tabs for admin mode
      showAdmin(show = TRUE)

      # Hide irrelevant tabs/menus
      showViz(show = FALSE)

      # Select the last tab the user was on in admin mode
      updateTabsetPanel(session, "navbar", selected = last_admin_tab())

      admin_vis_flag("viz")
    }
  })

  observeEvent(input$navbar, {
    req(languageSelection) # Ensure language is set before proceeding
    # Observe opening of the navbar (usually by a click through from another module) and close the navbar. Clicks by the user should have no effect as the navbar menu closes as soon as they click.
    session$sendCustomMessage(
      type = "toggleDropdown",
      message = list(msg = "hide dropdown")
    )

    # When user selects a tab, update the last active tab for the current mode
    if (
      input$navbar %in%
        c(
          "home",
          "discPlot",
          "contPlot",
          "mix",
          "map",
          "FOD",
          "snowInfo",
          "waterInfo",
          "waterTemp",
          "WQReport",
          "snowBulletin",
          "imgTableView",
          "imgMapView",
          "docTableView",
          "about",
          "news",
          "discData",
          "contData",
          "WWR"
        )
    ) {
      # User is in viz mode
      last_viz_tab(input$navbar)
    } else if (
      input$navbar %in%
        c(
          "adminHome",
          "syncCont",
          "syncDisc",
          "addLocation",
          "addSubLocation",
          "addTimeseries",
          "deploy_recover",
          "calibrate",
          "manageInstruments",
          "addContData",
          "continuousCorrections",
          "imputeMissing",
          "editContData",
          "grades_approvals_qualifiers",
          "addDiscData",
          "editDiscData",
          "addGuidelines",
          "addDocs",
          "addImgs",
          "addImgSeries",
          "manageNewsContent",
          "manageNotifications",
          "viewFeedback",
          "visit",
          "changePwd",
          "manageUsers",
          "simplerIndex"
        )
    ) {
      # User is in admin mode
      last_admin_tab(input$navbar)
    }

    # Load modules when the corresponding tabs are selected
    ## Visulize mode tabs ##########################
    ### Home nav_menu ##########################
    if (input$navbar == "home") {
      if (!ui_loaded$home) {
        output$home_ui <- renderUI(homeUI("home"))
        ui_loaded$home <- TRUE
        moduleOutputs$home <- home("home", language = languageSelection) # Call the server
      }
      observe({
        if (!is.null(moduleOutputs$home$change_tab)) {
          target <- moduleOutputs$home$change_tab
          nav_select(session = session, "navbar", selected = target)
          moduleOutputs$home$change_tab <- NULL
        }
      })
    }

    ### Plots nav_menu ##########################
    if (input$navbar == "discPlot") {
      # This is reached through a nav_menu
      if (!ui_loaded$discPlot) {
        output$plotDiscrete_ui <- renderUI(discPlotUI("discPlot"))
        ui_loaded$discPlot <- TRUE
        discPlot(
          "discPlot",
          config$mdb_files,
          language = languageSelection,
          windowDims,
          inputs = moduleOutputs$mapLocs
        ) # Call the server
        if (!is.null(moduleOutputs$mapLocs)) {
          moduleOutputs$mapLocs$location_id <- NULL
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "contPlot") {
      # This is reached through a nav_menu
      if (!ui_loaded$contPlot) {
        output$plotContinuous_ui <- renderUI(contPlotUI("contPlot"))
        ui_loaded$contPlot <- TRUE
        # Call the server
        contPlot(
          "contPlot",
          language = languageSelection,
          windowDims,
          # inputs temporarily disabled because of issues with narrowing the datatable using inputs.
          # inputs = moduleOutputs$mapLocs
          inputs = NULL
        )
        if (!is.null(moduleOutputs$mapLocs)) {
          moduleOutputs$mapLocs$location_id <- NULL
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      }
    }

    ### Maps nav_menu ##########################
    if (input$navbar == "monitoringLocationsMap") {
      # This is reached through a nav_menu
      if (!ui_loaded$monitoringLocationsMap) {
        output$mapLocs_ui <- renderUI(mapLocsUI("mapLocs"))
        ui_loaded$monitoringLocationsMap <- TRUE
        # Call the server
        moduleOutputs$mapLocs <- mapLocs(
          "mapLocs",
          language = languageSelection
        )
      }
      observe({
        if (!is.null(moduleOutputs$mapLocs$change_tab)) {
          target <- moduleOutputs$mapLocs$change_tab
          if (target == "discData") {
            ui_loaded$discData <- FALSE
          }
          if (target == "contData") {
            ui_loaded$contData <- FALSE
          }
          if (target == "discPlot") {
            ui_loaded$discPlot <- FALSE
          }
          if (target == "contPlot") {
            ui_loaded$contPlot <- FALSE
          }
          nav_select(session = session, "navbar", selected = target) # Change tabs
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      })
    }
    if (input$navbar == "parameterValuesMap") {
      if (!ui_loaded$paramValuesMap) {
        output$mapParams_ui <- renderUI(mapParamsUI("mapParams"))
        ui_loaded$paramValuesMap <- TRUE
        #  Call the server
        mapParams("mapParams", language = languageSelection)
      }
    }
    if (input$navbar == "rasterValuesMap") {
      if (!ui_loaded$rasterValuesMap) {
        output$mapRaster_ui <- renderUI(mapRasterUI("mapRaster"))
        ui_loaded$rasterValuesMap <- TRUE
        mapRaster("mapRaster", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "snowBulletinMap") {
      if (!ui_loaded$snowBulletinMap) {
        output$mapSnowbull_ui <- renderUI(mapSnowbullUI("mapSnowbull"))
        ui_loaded$snowBulletinMap <- TRUE
        # Call the server
        mapSnowbull("mapSnowbull", language = languageSelection)
      }
    }

    ### FOD nav_menu ##########################
    if (input$navbar == "FOD") {
      if (!ui_loaded$FOD) {
        output$fod_ui <- renderUI(FODUI("FOD"))
        ui_loaded$FOD <- TRUE
        # Call the server
        FOD("FOD", language = languageSelection)
      }
    }
    ### Image nav_menu ##########################
    if (input$navbar == "imgTableView") {
      if (!ui_loaded$imgTableView) {
        output$imgTableView_ui <- renderUI(imgTableViewUI("imgTableView"))
        ui_loaded$imgTableView <- TRUE
        # Call the server
        imgTableView("imgTableView", language = languageSelection)
      }
    }
    if (input$navbar == "imgMapView") {
      if (!ui_loaded$imgMapView) {
        output$imgMapView_ui <- renderUI(imgMapViewUI("imgMapView"))
        ui_loaded$imgMapView <- TRUE
        # Call the server
        imgMapView("imgMapView", language = languageSelection)
      }
    }

    ### Document nav_menu ##########################
    if (input$navbar == "docTableView") {
      if (!ui_loaded$docTableView) {
        output$docTableView_ui <- renderUI(docTableViewUI("docTableView"))
        ui_loaded$docTableView <- TRUE
        docTableView("docTableView", language = languageSelection)
      }
    }

    ### Reports nav_menu ##########################
    if (input$navbar == "snowInfo") {
      if (!ui_loaded$snowInfo) {
        output$snowInfo_ui <- renderUI(snowInfoUIMod("snowInfo"))
        ui_loaded$snowInfo <- TRUE
        # Call the server
        snowInfoMod("snowInfo", language = languageSelection)
      }
    }
    if (input$navbar == "waterInfo") {
      if (!ui_loaded$waterInfo) {
        output$waterInfo_ui <- renderUI(waterInfoUIMod("waterInfo"))
        ui_loaded$waterInfo <- TRUE
        # Call the server
        waterInfoMod("waterInfo", language = languageSelection)
      }
    }
    if (input$navbar == "waterTemp") {
      if (!ui_loaded$waterTemp) {
        output$waterTemp_ui <- renderUI(waterTempUIMod("waterTemp"))
        ui_loaded$waterTemp <- TRUE
        # Call the server
        waterTempMod(
          "waterTemp",
          language = languageSelection,
          inputs = NULL
        )
      }
    }
    if (input$navbar == "WQReport") {
      if (!ui_loaded$WQReport) {
        output$WQReport_ui <- renderUI(WQReportUI("WQReport"))
        ui_loaded$WQReport <- TRUE
        WQReport(
          "WQReport",
          mdb_files = config$mdb_files,
          language = languageSelection
        ) # Call the server
      }
    }
    if (input$navbar == "snowBulletin") {
      if (!ui_loaded$snowBulletin) {
        output$snowBulletin_ui <- renderUI(snowBulletinUIMod("snowBulletin"))
        ui_loaded$snowBulletin <- TRUE
        # Call the server
        snowBulletinMod("snowBulletin", language = languageSelection)
      }
    }

    ### Data download nav_menu ##########################
    if (input$navbar == "discData") {
      if (!ui_loaded$discData) {
        output$discData_ui <- renderUI(discDataUI("discData"))
        ui_loaded$discData <- TRUE
        discData(
          "discData",
          language = languageSelection,
          inputs = moduleOutputs$mapLocs
        ) # Call the server
        if (!is.null(moduleOutputs$mapLocs)) {
          moduleOutputs$mapLocs$location_id <- NULL
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "contData") {
      if (!ui_loaded$contData) {
        output$contData_ui <- renderUI(contDataUI("contData"))
        ui_loaded$contData <- TRUE
        contData(
          "contData",
          language = languageSelection,
          inputs = moduleOutputs$mapLocs
        ) # Call the server
        if (!is.null(moduleOutputs$mapLocs)) {
          moduleOutputs$mapLocs$location_id <- NULL
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      }
    }

    ### Water Well Registry
    if (input$navbar == "WWR") {
      if (!ui_loaded$WWR) {
        output$WWR_ui <- renderUI(wellRegistryUI("wellRegistry"))
        ui_loaded$WWR <- TRUE
        wellRegistry(
          "wellRegistry",
          language = languageSelection
        ) # Call the server
      }
    }

    ### Info nav_menu ##########################
    if (input$navbar == "about") {
      if (!ui_loaded$about) {
        output$about_ui <- renderUI(aboutUI("about"))
        ui_loaded$about <- TRUE
        about("about", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "news") {
      if (!ui_loaded$news) {
        output$news_ui <- renderUI(newsUI("news"))
        ui_loaded$news <- TRUE
        news("news", language = languageSelection) # Call the server
      }
    }

    ## Admin mode tabs ##########################
    if (input$navbar == "syncCont") {
      if (!ui_loaded$syncCont) {
        output$syncCont_ui <- renderUI(syncContUI("syncCont"))
        ui_loaded$syncCont <- TRUE
        syncCont("syncCont", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "syncDisc") {
      if (!ui_loaded$syncDisc) {
        output$syncDisc_ui <- renderUI(syncDiscUI("syncDisc"))
        ui_loaded$syncDisc <- TRUE
        syncDisc("syncDisc", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "addLocation") {
      if (!ui_loaded$addLocation) {
        output$addLocation_ui <- renderUI(addLocationUI("addLocation"))
        ui_loaded$addLocation <- TRUE
        addLocation(
          "addLocation",
          inputs = moduleOutputs$addDiscData,
          language = languageSelection
        ) # Call the server
        if (!is.null(moduleOutputs$addDiscData)) {
          moduleOutputs$addDiscData$location <- NULL
          moduleOutputs$addDiscData$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "addSubLocation") {
      if (!ui_loaded$addSubLocation) {
        output$addSubLocation_ui <- renderUI(addSubLocationUI("addSubLocation"))
        ui_loaded$addSubLocation <- TRUE
        addSubLocation(
          "addSubLocation",
          inputs = moduleOutputs$addDiscData,
          language = languageSelection
        ) # Call the server
        if (!is.null(moduleOutputs$addDiscData)) {
          moduleOutputs$addDiscData$sublocation <- NULL
          moduleOutputs$addDiscData$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "addTimeseries") {
      if (!ui_loaded$addTimeseries) {
        output$addTimeseries_ui <- renderUI(addTimeseriesUI("addTimeseries"))
        ui_loaded$addTimeseries <- TRUE
        addTimeseries("addTimeseries", language = languageSelection) # Call the server
        if (!is.null(moduleOutputs$addContData)) {
          moduleOutputs$addContData$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "deploy_recover") {
      if (!ui_loaded$deploy_recover) {
        output$deploy_recover_ui <- renderUI(deploy_recover_UI(
          "deploy_recover"
        )) # Render the UI
        ui_loaded$deploy_recover <- TRUE
        deploy_recover("deploy_recover", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "calibrate") {
      if (!ui_loaded$calibrate) {
        output$calibrate_ui <- renderUI(calibrateUI("calibrate")) # Render the UI
        ui_loaded$calibrate <- TRUE
        calibrate("calibrate", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "manageInstruments") {
      if (!ui_loaded$manageInstruments) {
        output$manageInstruments_ui <- renderUI(manageInstrumentsUI(
          "manageInstruments"
        ))
        ui_loaded$manageInstruments <- TRUE
        manageInstruments("manageInstruments", language = languageSelection)
      }
    }
    if (input$navbar == "addContData") {
      if (!ui_loaded$addContData) {
        output$addContData_ui <- renderUI(addContDataUI("addContData")) # Render the UI
        ui_loaded$addContData <- TRUE
        moduleOutputs$addContData <- addContData(
          "addContData",
          language = languageSelection
        ) # Call the server
      }
      # Observe the change_tab output from the addContData module
      observe({
        if (!is.null(moduleOutputs$addContData$change_tab)) {
          nav_select(
            session = session,
            "navbar",
            selected = moduleOutputs$addContData$change_tab
          )
          moduleOutputs$addContData$change_tab <- NULL
        }
      })
    }
    if (input$navbar == "continuousCorrections") {
      if (!ui_loaded$continuousCorrections) {
        output$continuousCorrections_ui <- renderUI(continuousCorrectionsUI(
          "continuousCorrections"
        ))
        ui_loaded$continuousCorrections <- TRUE
        continuousCorrections(
          "continuousCorrections",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "imputeMissing") {
      if (!ui_loaded$imputeMissing) {
        output$imputeMissing_ui <- renderUI(imputeMissingUI("imputeMissing")) # Render the UI
        ui_loaded$imputeMissing <- TRUE
        imputeMissing("imputeMissing", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "editContData") {
      if (!ui_loaded$editContData) {
        output$editContData_ui <- renderUI(editContDataUI("editContData")) # Render the UI
        ui_loaded$editContData <- TRUE
        editContData("editContData", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "grades_approvals_qualifiers") {
      if (!ui_loaded$grades_approvals_qualifiers) {
        output$grades_approvals_qualifiers_ui <- renderUI(grades_approvals_qualifiersUI(
          "grades_approvals_qualifiers"
        )) # Render the UI
        ui_loaded$grades_approvals_qualifiers <- TRUE
        grades_approvals_qualifiers(
          "grades_approvals_qualifiers",
          language = languageSelection
        ) # Call the server
      }
    }
    if (input$navbar == "addDiscData") {
      if (!ui_loaded$addDiscData) {
        output$addDiscData_ui <- renderUI(addDiscDataUI("addDiscData")) # Render the UI
        ui_loaded$addDiscData <- TRUE
        moduleOutputs$addDiscData <- addDiscData(
          "addDiscData",
          language = languageSelection
        ) # Call the server
      }
      # Observe the change_tab output from the addDiscData module
      observe({
        if (!is.null(moduleOutputs$addDiscData$change_tab)) {
          nav_select(
            session = session,
            "navbar",
            selected = moduleOutputs$addDiscData$change_tab
          )
          moduleOutputs$addDiscData$change_tab <- NULL
        }
      })
    }
    if (input$navbar == "addSamples") {
      if (!ui_loaded$addSamples) {
        output$addSamples_ui <- renderUI(addSamplesUI("addSamples"))
        ui_loaded$addSamples <- TRUE
        addSamples("addSamples", language = languageSelection)
      }
    }
    if (input$navbar == "editDiscData") {
      if (!ui_loaded$editDiscData) {
        output$editDiscData_ui <- renderUI(editDiscDataUI("editDiscData")) # Render the UI
        ui_loaded$editDiscData <- TRUE
        editDiscData("editDiscData", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "addGuidelines") {
      if (!ui_loaded$addGuidelines) {
        output$addGuidelines_ui <- renderUI(addGuidelinesUI("addGuidelines")) # Render the UI
        ui_loaded$addGuidelines <- TRUE
        addGuidelines("addGuidelines", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "addSampleSeries") {
      if (!ui_loaded$addSampleSeries) {
        output$addSampleSeries_ui <- renderUI(addSampleSeriesUI(
          "addSampleSeries"
        ))
        ui_loaded$addSampleSeries <- TRUE
        addSampleSeries("addSampleSeries", language = languageSelection)
      }
    }
    if (input$navbar == "addDocs") {
      if (!ui_loaded$addDocs) {
        output$addDocs_ui <- renderUI(addDocsUI("addDocs")) # Render the UI
        ui_loaded$addDocs <- TRUE
        addDocs("addDocs", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "addImgs") {
      if (!ui_loaded$addImgs) {
        output$addImgs_ui <- renderUI(addImgsUI("addImgs")) # Render the UI
        ui_loaded$addImgs <- TRUE
        addImgs("addImgs", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "addImgSeries") {
      if (!ui_loaded$addImgSeries) {
        output$addImgSeries_ui <- renderUI(addImgSeriesUI("addImgSeries")) # Render the UI
        ui_loaded$addImgSeries <- TRUE
        addImgSeries("addImgSeries", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "simplerIndex") {
      if (!ui_loaded$simplerIndex) {
        output$simplerIndex_ui <- renderUI(simplerIndexUI("simplerIndex")) # Render the UI
        ui_loaded$simplerIndex <- TRUE
        simplerIndex("simplerIndex", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "editBoreholesWells") {
      if (!ui_loaded$editBoreholesWells) {
        output$editBoreholesWells_ui <- renderUI(editBoreholesWellsUI(
          "editBoreholesWells"
        ))
        ui_loaded$editBoreholesWells <- TRUE
        editBoreholesWells("editBoreholesWells", language = languageSelection)
      }
    }
    if (input$navbar == "manageBoreholeDocuments") {
      if (!ui_loaded$manageBoreholeDocuments) {
        output$manageBoreholeDocuments_ui <- renderUI(manageBoreholeDocumentsUI(
          "manageBoreholeDocuments"
        ))
        ui_loaded$manageBoreholeDocuments <- TRUE
        manageBoreholeDocuments(
          "manageBoreholeDocuments",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageOrganizations") {
      if (!ui_loaded$manageOrganizations) {
        output$manageOrganizations_ui <- renderUI(manageOrganizationsUI(
          "manageOrganizations"
        ))
        ui_loaded$manageOrganizations <- TRUE
        manageOrganizations(
          "manageOrganizations",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageNetworks") {
      if (!ui_loaded$manageNetworks) {
        output$manageNetworks_ui <- renderUI(manageNetworksUI(
          "manageNetworks"
        ))
        ui_loaded$manageNetworks <- TRUE
        manageNetworks(
          "manageNetworks",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageProjects") {
      if (!ui_loaded$manageProjects) {
        output$manageProjects_ui <- renderUI(manageProjectsUI(
          "manageProjects"
        ))
        ui_loaded$manageProjects <- TRUE
        manageProjects(
          "manageProjects",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageNetworkProjectTypes") {
      if (!ui_loaded$manageNetworkProjectTypes) {
        output$manageNetworkProjectTypes_ui <- renderUI(
          manageNetworkProjectTypesUI("manageNetworkProjectTypes")
        )
        ui_loaded$manageNetworkProjectTypes <- TRUE
        manageNetworkProjectTypes(
          "manageNetworkProjectTypes",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageLocationTypes") {
      if (!ui_loaded$manageLocationTypes) {
        output$manageLocationTypes_ui <- renderUI(manageLocationTypesUI(
          "manageLocationTypes"
        ))
        ui_loaded$manageLocationTypes <- TRUE
        manageLocationTypes(
          "manageLocationTypes",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageMediaTypes") {
      if (!ui_loaded$manageMediaTypes) {
        output$manageMediaTypes_ui <- renderUI(manageMediaTypesUI(
          "manageMediaTypes"
        ))
        ui_loaded$manageMediaTypes <- TRUE
        manageMediaTypes(
          "manageMediaTypes",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageMatrixStates") {
      if (!ui_loaded$manageMatrixStates) {
        output$manageMatrixStates_ui <- renderUI(manageMatrixStatesUI(
          "manageMatrixStates"
        ))
        ui_loaded$manageMatrixStates <- TRUE
        manageMatrixStates(
          "manageMatrixStates",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageParameterGroups") {
      if (!ui_loaded$manageParameterGroups) {
        output$manageParameterGroups_ui <- renderUI(manageParameterGroupsUI(
          "manageParameterGroups"
        ))
        ui_loaded$manageParameterGroups <- TRUE
        manageParameterGroups(
          "manageParameterGroups",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageParameterSubGroups") {
      if (!ui_loaded$manageParameterSubGroups) {
        output$manageParameterSubGroups_ui <- renderUI(
          manageParameterSubGroupsUI("manageParameterSubGroups")
        )
        ui_loaded$manageParameterSubGroups <- TRUE
        manageParameterSubGroups(
          "manageParameterSubGroups",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageParameters") {
      if (!ui_loaded$manageParameters) {
        output$manageParameters_ui <- renderUI(manageParametersUI(
          "manageParameters"
        ))
        ui_loaded$manageParameters <- TRUE
        manageParameters(
          "manageParameters",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageCommunicationProtocolFamilies") {
      if (!ui_loaded$manageCommunicationProtocolFamilies) {
        output$manageCommunicationProtocolFamilies_ui <- renderUI(
          manageCommunicationProtocolFamiliesUI(
            "manageCommunicationProtocolFamilies"
          )
        )
        ui_loaded$manageCommunicationProtocolFamilies <- TRUE
        manageCommunicationProtocolFamilies(
          "manageCommunicationProtocolFamilies",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageCommunicationProtocols") {
      if (!ui_loaded$manageCommunicationProtocols) {
        output$manageCommunicationProtocols_ui <- renderUI(
          manageCommunicationProtocolsUI("manageCommunicationProtocols")
        )
        ui_loaded$manageCommunicationProtocols <- TRUE
        manageCommunicationProtocols(
          "manageCommunicationProtocols",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageTransmissionMethodFamilies") {
      if (!ui_loaded$manageTransmissionMethodFamilies) {
        output$manageTransmissionMethodFamilies_ui <- renderUI(
          manageTransmissionMethodFamiliesUI(
            "manageTransmissionMethodFamilies"
          )
        )
        ui_loaded$manageTransmissionMethodFamilies <- TRUE
        manageTransmissionMethodFamilies(
          "manageTransmissionMethodFamilies",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageTransmissionMethods") {
      if (!ui_loaded$manageTransmissionMethods) {
        output$manageTransmissionMethods_ui <- renderUI(
          manageTransmissionMethodsUI("manageTransmissionMethods")
        )
        ui_loaded$manageTransmissionMethods <- TRUE
        manageTransmissionMethods(
          "manageTransmissionMethods",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageTransmissionComponentRoles") {
      if (!ui_loaded$manageTransmissionComponentRoles) {
        output$manageTransmissionComponentRoles_ui <- renderUI(
          manageTransmissionComponentRolesUI(
            "manageTransmissionComponentRoles"
          )
        )
        ui_loaded$manageTransmissionComponentRoles <- TRUE
        manageTransmissionComponentRoles(
          "manageTransmissionComponentRoles",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageInstrumentConnections") {
      if (!ui_loaded$manageInstrumentConnections) {
        output$manageInstrumentConnections_ui <- renderUI(
          manageInstrumentConnectionsUI("manageInstrumentConnections")
        )
        ui_loaded$manageInstrumentConnections <- TRUE
        manageInstrumentConnections(
          "manageInstrumentConnections",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageInstrumentConnectionSignals") {
      if (!ui_loaded$manageInstrumentConnectionSignals) {
        output$manageInstrumentConnectionSignals_ui <- renderUI(
          manageInstrumentConnectionSignalsUI(
            "manageInstrumentConnectionSignals"
          )
        )
        ui_loaded$manageInstrumentConnectionSignals <- TRUE
        manageInstrumentConnectionSignals(
          "manageInstrumentConnectionSignals",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageTransmissionSetups") {
      if (!ui_loaded$manageTransmissionSetups) {
        output$manageTransmissionSetups_ui <- renderUI(
          manageTransmissionSetupsUI("manageTransmissionSetups")
        )
        ui_loaded$manageTransmissionSetups <- TRUE
        manageTransmissionSetups(
          "manageTransmissionSetups",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageTransmissionRoutes") {
      if (!ui_loaded$manageTransmissionRoutes) {
        output$manageTransmissionRoutes_ui <- renderUI(
          manageTransmissionRoutesUI("manageTransmissionRoutes")
        )
        ui_loaded$manageTransmissionRoutes <- TRUE
        manageTransmissionRoutes(
          "manageTransmissionRoutes",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageTransmissionComponents") {
      if (!ui_loaded$manageTransmissionComponents) {
        output$manageTransmissionComponents_ui <- renderUI(
          manageTransmissionComponentsUI("manageTransmissionComponents")
        )
        ui_loaded$manageTransmissionComponents <- TRUE
        manageTransmissionComponents(
          "manageTransmissionComponents",
          language = languageSelection
        )
      }
    }
    if (input$navbar == "manageNewsContent") {
      if (!ui_loaded$manageNewsContent) {
        output$manageNewsContent_ui <- renderUI(manageNewsContentUI(
          "manageNewsContent"
        ))
        ui_loaded$manageNewsContent <- TRUE
        manageNewsContent("manageNewsContent", language = languageSelection)
      }
    }
    if (input$navbar == "adminHome") {
      if (!ui_loaded$adminHome) {
        output$adminHome_ui <- renderUI(adminLandingUI("adminHome"))
        ui_loaded$adminHome <- TRUE
        adminLanding("adminHome", language = languageSelection)
      }
    }
    if (input$navbar == "manageNotifications") {
      if (!ui_loaded$manageNotifications) {
        output$manageNotifications_ui <- renderUI(manageNotificationsUI(
          "manageNotifications",
          notification_module_choices
        ))
        ui_loaded$manageNotifications <- TRUE
        manageNotifications(
          "manageNotifications",
          notification_module_choices,
          language = languageSelection
        )
      }
    }
    if (input$navbar == "viewFeedback") {
      if (!ui_loaded$viewFeedback) {
        output$viewFeedback_ui <- renderUI(viewFeedbackUI("viewFeedback"))
        ui_loaded$viewFeedback <- TRUE
        viewFeedback("viewFeedback", language = languageSelection)
      }
    }
    if (input$navbar == "changePwd") {
      if (!ui_loaded$changePwd) {
        output$changePwd_ui <- renderUI(changePasswordUI("changePwd"))
        ui_loaded$changePwd <- TRUE
        changePassword("changePwd", language = languageSelection)
      }
    }
    if (input$navbar == "manageUsers") {
      if (!ui_loaded$manageUsers) {
        output$manageUsers_ui <- renderUI(manageUsersUI("manageUsers"))
        ui_loaded$manageUsers <- TRUE
        manageUsers("manageUsers", language = languageSelection)
      }
    }
    if (input$navbar == "visit") {
      if (!ui_loaded$visit) {
        output$visit_ui <- renderUI(visitUI("visit")) # Render the UI
        ui_loaded$visit <- TRUE
        visit("visit", language = languageSelection) # Call the server
      }
    }
  }) # End of observeEvent for loading modules based on navbar
} # End of main server
