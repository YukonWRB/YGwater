#' Tabular output of hydrometric data
#'
#' @description
#' Creates a report of hydrometric, snow pack, precipitation, and air temperature conditions in Excel format, each table on a separate tab. List of stations/locations can be user-defined if desired. Connection is established using AquaConnect by default and MUST connect to a database created and maintained by the package AquaCache.
#'
#' Note that data can only be as recent as the last incorporation to the database. If you need the most up to date data possible, run AquaCache::getNewContinuous first.
#'
#' @param level_locations List of water level locations to include in the report, as a character vector. "default" is a pre-determined list of locations across the territory, "all" fetches all level reporting locations in the DB. NULL will not create the table.
#' @param flow_locations List of flow locations to include in the report, as a character vector. "default" is a pre-determined list of locations across the territory. "all" fetches all flow reporting locations in the DB. NULL will not create the table.
#' @param snow_locations List of snow pillow locations to include in the report, as a character vector. "default" includes all of the WRB snow pillows as of Feb 2023, "all" fetches all snow pillow locations in the DB. NULL will not create the table.
#' @param bridge_locations List of bridge freeboard radar locations to include in the report, as a character vector. "default" includes all of the radars as of Feb 2023, "all" fetches all snow pillow locations in the DB. NULL will not create the table.
#' @param precip_locations List of flow/level locations for which to report precipitation. "default" is a pre-determined list of locations, "all" is all locations for which there is a drainage polygon (which may be more or less than the number of stations reporting level or flow information). NULL will not create the table. WARNING: this portion of the script is slow. Setting this parameter to "all" could take about an hour to get all information together.
#' @param report_datetime Date-time for which to generate the report. Defaults to the current time.
#' @param past The number of days in the past for which you want data. Will be rounded to yield table columns covering at least one week, at most 4 weeks. 24, 28, and 72 hour change columns are always rendered.
#' @param save_path The path where you wish to save the Excel workbook. A folder will be created for each day's report. 'choose' will bring up a file dialog to select the folder if the session is interactive. Default is 'choose'.
#' @param archive_path The path to yesterday's file, if you wish to include yesterday's comments in this report. Full path, including extension .xlsx. Function expects a workbook exactly as produced by this function, plus of course the observer comments. Default is 'choose', set to NULL to not use a previous report.
#' @param log_level Logging threshold for file logging. One of "DEBUG", "INFO", "WARN", or "ERROR". Default is "INFO".

#' @param con A connection to the aquacache database. NULL uses [AquaConnect()] and automatically disconnects.
#'
#' @return The path to which the report was saved, and Excel workbook containing the report with one tab per timeseries type.
#' @export

# TODO: Adapt to use new DB

tabularReport <- function(
  level_locations = "all",
  flow_locations = "all",
  snow_locations = "all",
  bridge_locations = "all",
  precip_locations = "default",
  report_datetime = Sys.time(),
  past = 28,
  save_path = NULL,
  archive_path = NULL,
  log_level = "INFO",
  con = NULL
) {
  # level_locations = "all"
  # flow_locations = "all"
  # snow_locations = "all"
  # bridge_locations = "all"
  # precip_locations = "default"
  # report_datetime = as.POSIXct("2024-06-01 12:00", tz = "UTC")
  # past = 7
  # save_path = NULL
  # archive_path = NULL
  # con = NULL

  if (is.null(save_path)) {
    stop("save_path must be provided")
  }

  if (is.null(archive_path)) {
    yesterday_comments <- FALSE
    warning(
      "No archive_path provided, yesterday's comments will not be included in the report."
    )
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  report_time <- as.POSIXct(report_datetime, tz = "UTC")
  report_day <- as.Date(report_time)

  # Logging setup: one log file per function call, reset each run.
  report_dir <- save_path
  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  }
  log_path <- file.path(report_dir, "HydrometricReport.log")
  writeLines(character(0), log_path)
  .log_levels <- c(DEBUG = 10, INFO = 20, WARN = 30, ERROR = 40)
  log_level <- toupper(log_level)
  if (!(log_level %in% names(.log_levels))) {
    warning(paste0(
      "Invalid log_level '",
      log_level,
      "'. Falling back to INFO."
    ))
    log_level <- "INFO"
  }
  .log_threshold <- .log_levels[[log_level]]
  .log_event <- function(level, message_text) {
    if (.log_levels[[level]] < .log_threshold) {
      return(invisible(NULL))
    }
    line <- paste0(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " [",
      level,
      "] ",
      message_text
    )
    write(line, file = log_path, append = TRUE)
    invisible(line)
  }
  log_debug <- function(message_text) .log_event("DEBUG", message_text)
  log_info <- function(message_text) .log_event("INFO", message_text)
  log_warn <- function(message_text) .log_event("WARN", message_text)
  log_error <- function(message_text) .log_event("ERROR", message_text)
  format_log_time <- function(time_value) {
    format(as.POSIXct(time_value, tz = "UTC"), "%Y-%m-%d %H:%M:%S %Z")
  }
  summarize_precip_result <- function(result) {
    pieces <- c(
      paste0("class=", paste(class(result), collapse = ",")),
      paste0("length=", length(result))
    )
    if (is.data.frame(result)) {
      pieces <- c(
        pieces,
        paste0("rows=", nrow(result)),
        paste0("cols=", ncol(result)),
        paste0("names=", paste(names(result), collapse = ","))
      )
    } else if (!is.null(names(result))) {
      pieces <- c(
        pieces,
        paste0("names=", paste(names(result), collapse = ","))
      )
    }
    if (!is.null(result$mean_precip)) {
      pieces <- c(
        pieces,
        paste0("mean_precip=", paste(result$mean_precip, collapse = ","))
      )
    }
    paste(pieces, collapse = "; ")
  }
  run_precip_window <- function(
    location_codes,
    window_name,
    start_time,
    end_time,
    con
  ) {
    log_debug(paste0(
      "[precip] Starting ",
      window_name,
      " basinPrecip batch call for ",
      length(location_codes),
      " locations: start=",
      format_log_time(start_time),
      ", end=",
      format_log_time(end_time),
      ", duration_hours=",
      round(as.numeric(difftime(end_time, start_time, units = "hours")), 2)
    ))
    started_at <- Sys.time()
    warning_messages <- character()
    result <- tryCatch(
      withCallingHandlers(
        suppressMessages(basinPrecip(
          location = location_codes,
          start = start_time,
          end = end_time,
          silent = TRUE,
          map = FALSE,
          con = con
        )),
        warning = function(w) {
          warning_messages <<- c(warning_messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        log_error(paste0(
          "[precip] ",
          window_name,
          " basinPrecip batch failed: ",
          conditionMessage(e)
        ))
        stop(e)
      }
    )
    elapsed_seconds <- round(
      as.numeric(difftime(Sys.time(), started_at, units = "secs")),
      2
    )
    log_debug(paste0(
      "[precip] Completed ",
      window_name,
      " basinPrecip batch call in ",
      elapsed_seconds,
      " s; ",
      summarize_precip_result(result)
    ))
    if (length(warning_messages) > 0) {
      log_warn(paste0(
        "[precip] ",
        window_name,
        " batch warnings: ",
        paste(warning_messages, collapse = " | ")
      ))
    }

    if (!is.list(result)) {
      log_error(paste0(
        "[precip] ",
        window_name,
        " batch did not return a list. Result summary: ",
        summarize_precip_result(result)
      ))
      stop(paste0(
        "basinPrecip batch result for ",
        window_name,
        " is not a list"
      ))
    }

    if (length(result) != length(location_codes)) {
      log_error(paste0(
        "[precip] ",
        window_name,
        " batch length mismatch: expected ",
        length(location_codes),
        ", got ",
        length(result),
        "."
      ))
      stop(paste0(
        "basinPrecip batch result for ",
        window_name,
        " has length mismatch"
      ))
    }

    for (loc in location_codes) {
      loc_result <- if (!is.null(names(result)) && loc %in% names(result)) {
        result[[loc]]
      } else {
        result[[which(location_codes == loc)[1]]]
      }

      if (!is.list(loc_result) || !"mean_precip" %in% names(loc_result)) {
        log_error(paste0(
          "[precip] [",
          loc,
          "] ",
          window_name,
          " result is missing mean_precip."
        ))
        stop(paste0(
          "basinPrecip result for ",
          loc,
          " in ",
          window_name,
          " is missing mean_precip"
        ))
      }
      if (length(loc_result$mean_precip) < 1) {
        log_error(paste0(
          "[precip] [",
          loc,
          "] ",
          window_name,
          " returned empty mean_precip."
        ))
        stop(paste0(
          "basinPrecip result for ",
          loc,
          " in ",
          window_name,
          " has empty mean_precip"
        ))
      }
      if (is.na(loc_result$mean_precip[[1]])) {
        log_warn(paste0(
          "[precip] [",
          loc,
          "] ",
          window_name,
          " returned NA mean_precip."
        ))
      }
    }
    result
  }
  log_info(paste0("Starting tabularReport for report_day=", report_day, "."))
  log_info(paste0("Log level set to ", log_level, "."))
  log_info(paste0("Log file initialized at ", log_path, "."))

  if (!is.null(level_locations)) {
    if (level_locations[1] == "default") {
      level_locations <- c(
        "09AH001",
        "09AH004",
        "09EA003",
        "09EB001",
        "09DC006",
        "09FD003",
        "09BC001",
        "09BC002",
        "09AE002",
        "10AA001",
        "09AB001",
        "09AB004",
        "09AB010",
        "09AA004",
        "09AA017"
      )
      level_locations <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 1165 AND (l.location_code IN ('",
          paste(level_locations, collapse = "', '"),
          "') OR l.alias IN ('",
          paste(level_locations, collapse = "', '"),
          "') OR l.name IN ('",
          paste(level_locations, collapse = "', '"),
          "') OR l.name_fr IN ('",
          paste(level_locations, collapse = "', '"),
          "')) ORDER BY l.location_code;"
        )
      )
    } else if (level_locations[1] == "all") {
      level_locations <- DBI::dbGetQuery(
        con,
        "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 1165 ORDER BY l.location_code;"
      )
    } else {
      level_locations <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 1165 AND (l.location_code IN ('",
          paste(level_locations, collapse = "', '"),
          "') OR l.alias IN ('",
          paste(level_locations, collapse = "', '"),
          "') OR l.name IN ('",
          paste(level_locations, collapse = "', '"),
          "') OR l.name_fr IN ('",
          paste(level_locations, collapse = "', '"),
          "')) ORDER BY l.location_code;"
        )
      )
    }
  }

  if (!is.null(flow_locations)) {
    if (flow_locations[1] == "default") {
      flow_locations <- c(
        "09AH001",
        "09AH004",
        "09EA003",
        "09EB001",
        "09DC006",
        "09FD003",
        "09BC001",
        "09BC002",
        "09AE002",
        "10AA001",
        "09AB001",
        "09AB004",
        "09AB010",
        "09AA004",
        "09AA017"
      )
      flow_locations <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 1150 AND (l.location_code IN ('",
          paste(flow_locations, collapse = "', '"),
          "') OR l.alias IN ('",
          paste(flow_locations, collapse = "', '"),
          "') OR l.name IN ('",
          paste(flow_locations, collapse = "', '"),
          "') OR l.name_fr IN ('",
          paste(flow_locations, collapse = "', '"),
          "')) ORDER BY l.location_code;"
        )
      )
    } else if (flow_locations[1] == "all") {
      flow_locations <- DBI::dbGetQuery(
        con,
        "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 1150 ORDER BY l.location_code;"
      )
    } else {
      flow_locations <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 1150 AND (l.location_code IN ('",
          paste(flow_locations, collapse = "', '"),
          "') OR l.alias IN ('",
          paste(flow_locations, collapse = "', '"),
          "') OR l.name IN ('",
          paste(flow_locations, collapse = "', '"),
          "') OR l.name_fr IN ('",
          paste(flow_locations, collapse = "', '"),
          "')) ORDER BY l.location_code;"
        )
      )
    }
  }

  if (!is.null(snow_locations)) {
    if (snow_locations[1] == "default") {
      snow_locations <- c(
        "09AA-M1",
        "09BA-M7",
        "09DB-M1",
        "09EA-M1",
        "10AD-M2",
        "29AB-M3"
      )
      snow_locations <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 21 AND (l.location_code IN ('",
          paste(snow_locations, collapse = "', '"),
          "') OR l.alias IN ('",
          paste(snow_locations, collapse = "', '"),
          "') OR l.name IN ('",
          paste(snow_locations, collapse = "', '"),
          "') OR l.name_fr IN ('",
          paste(snow_locations, collapse = "', '"),
          "')) ORDER BY l.location_code;"
        )
      )
    } else if (snow_locations[1] == "all") {
      snow_locations <- DBI::dbGetQuery(
        con,
        "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 21 ORDER BY l.location_code;"
      )
    } else {
      snow_locations <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 21 AND (l.location_code IN ('",
          paste(snow_locations, collapse = "', '"),
          "') OR l.alias IN ('",
          paste(snow_locations, collapse = "', '"),
          "') OR l.name IN ('",
          paste(snow_locations, collapse = "', '"),
          "') OR l.name_fr IN ('",
          paste(snow_locations, collapse = "', '"),
          "')) ORDER BY l.location_code;"
        )
      )
    }
  }

  if (!is.null(bridge_locations)) {
    if (bridge_locations[1] == "default") {
      bridge_locations <- c(
        "09AH005",
        "29AB010",
        "29AB011",
        "29AE007",
        "29AH001"
      )
      bridge_locations <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 1160 AND (l.location_code IN ('",
          paste(bridge_locations, collapse = "', '"),
          "') OR l.alias IN ('",
          paste(bridge_locations, collapse = "', '"),
          "') OR l.name IN ('",
          paste(bridge_locations, collapse = "', '"),
          "') OR l.name_fr IN ('",
          paste(bridge_locations, collapse = "', '"),
          "')) ORDER BY l.location_code;"
        )
      )
    } else if (bridge_locations[1] == "all") {
      bridge_locations <- DBI::dbGetQuery(
        con,
        "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 1160 ORDER BY l.location_code;"
      )
    } else {
      bridge_locations <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT l.location_code AS location, t.timeseries_id FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id = 1160 AND (l.location_code IN ('",
          paste(bridge_locations, collapse = "', '"),
          "') OR l.alias IN ('",
          paste(bridge_locations, collapse = "', '"),
          "') OR l.name IN ('",
          paste(bridge_locations, collapse = "', '"),
          "') OR l.name_fr IN ('",
          paste(bridge_locations, collapse = "', '"),
          "')) ORDER BY l.location_code;"
        )
      )
    }
  }

  precip_location_mode <- "none"
  if (!is.null(precip_locations)) {
    log_info("[precip] Resolving precipitation locations.")
    if (precip_locations[1] == "default") {
      precip_location_mode <- "default"
      precip_locations <- c(
        "08AA003",
        "08AA010",
        "08AB001",
        "09AA001",
        "09AA004",
        "09AA013",
        "09AB001",
        "09AB010",
        "09AC001",
        "09AE002",
        "09AH001",
        "09AH004",
        "09BC001",
        "09BC002",
        "09CA002",
        "09DC005",
        "09DC006",
        "09EA003",
        "09EB001",
        "09FC001",
        "09FD002",
        "10AA001",
        "10AD002",
        "10MA002",
        "10BE001"
      )
      precip_locations <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT l.location_code AS location FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id IN (1165, 1150) AND (l.location_code IN ('",
          paste(precip_locations, collapse = "', '"),
          "') OR l.alias IN ('",
          paste(precip_locations, collapse = "', '"),
          "') OR l.name IN ('",
          paste(precip_locations, collapse = "', '"),
          "') OR l.name_fr IN ('",
          paste(precip_locations, collapse = "', '"),
          "')) ORDER BY l.location_code;"
        )
      )[, 1]
      precip_locations <- unique(precip_locations)
    } else if (precip_locations[1] == "all") {
      precip_location_mode <- "all"
      precip_locations <- DBI::dbGetQuery(
        con,
        "SELECT l.location_code AS location FROM continuous.timeseries AS t JOIN public.parameters AS p ON t.parameter_id = p.parameter_id JOIN public.locations AS l ON t.location_id = l.location_id WHERE p.parameter_id IN (1165, 1150) ORDER BY l.location_code;"
      )[, 1]
      precip_locations <- unique(precip_locations)
    } else {
      precip_location_mode <- "custom"
    }
    log_info(paste0(
      "[precip] Resolved ",
      length(precip_locations),
      " locations (mode=",
      precip_location_mode,
      ")."
    ))
    if (length(precip_locations) > 0) {
      log_debug(paste0(
        "[precip] Location codes: ",
        paste(precip_locations, collapse = ", ")
      ))
    }
  } else {
    log_info("[precip] Precipitation locations are NULL.")
  }

  #Set the days for which to generate tables
  if (past < 8) {
    past <- 7
  } else if (past >= 8 && past < 15) {
    past <- 14
  } else if (past >= 15 && past < 22) {
    past <- 21
  } else if (past >= 22) {
    past <- 28
  }
  log_debug(paste0("Normalized past window to ", past, " days."))

  #Load yesterday's workbook -----------------
  yesterday <- list(
    yesterday_general = NULL,
    yesterday_locs = NULL,
    yesterday_public_comments = NULL
  )
  if (!is.null(archive_path)) {
    log_info(paste0(
      "Attempting to load archive workbook from ",
      archive_path,
      "."
    ))
    tryCatch(
      {
        yesterday_workbook <- openxlsx::loadWorkbook(archive_path)
        for (i in names(yesterday_workbook)) {
          if (!(i %in% c("precipitation", "comments"))) {
            yesterday[["yesterday_general"]][[
              i
            ]] <- suppressWarnings(openxlsx::read.xlsx(
              yesterday_workbook,
              sheet = i,
              rows = 3,
              cols = 2,
              colNames = FALSE
            ))
            data <- suppressWarnings(openxlsx::read.xlsx(
              yesterday_workbook,
              sheet = i,
              startRow = 6
            ))
            data <- unique(data) # Gets rid of repeated rows
            yesterday[["yesterday_locs"]][[i]] <- data
          } else if (i == "precipitation") {
            yesterday[["yesterday_general"]][[
              i
            ]] <- suppressWarnings(openxlsx::read.xlsx(
              yesterday_workbook,
              sheet = i,
              rows = 3,
              cols = 2,
              colNames = FALSE
            ))
            data <- suppressWarnings(openxlsx::read.xlsx(
              yesterday_workbook,
              sheet = i,
              startRow = 8
            ))
            data <- unique(data) # Gets rid of repeated rows
            yesterday[["yesterday_locs"]][[i]] <- data
          } else if (i == "comments") {
            if ("precipitation" %in% names(yesterday_workbook)) {
              yesterday[[
                "yesterday_public_comments"
              ]] <- suppressWarnings(openxlsx::read.xlsx(
                yesterday_workbook,
                sheet = i,
                rows = c(12, 13),
                cols = 2,
                colNames = FALSE
              ))
            } else {
              yesterday[[
                "yesterday_public_comments"
              ]] <- suppressWarnings(openxlsx::read.xlsx(
                yesterday_workbook,
                sheet = i,
                rows = c(11, 12),
                cols = 2,
                colNames = FALSE
              ))
            }
          }
        }
        yesterday_comments <- TRUE
        log_info("Archive workbook loaded successfully.")
      },
      error = function(e) {
        warning(
          "Could not fetch information from yesterday's workbooks. Perhaps the file path you specified is incorrect; check the function help again."
        )
        yesterday_comments <- FALSE
        log_warn(paste0("Archive workbook load failed: ", conditionMessage(e)))
      }
    )
  } else {
    yesterday_comments <- FALSE
    log_info("No archive workbook provided; yesterday comments disabled.")
  }

  # Get the data -------------------------
  tables <- list()
  ## Precipitation -----------------------
  precip <- data.frame()

  log_info(paste0(
    "[precip] Precipitation locations: ",
    paste(precip_locations, collapse = ", ")
  ))

  log_info(paste0("Report day is ", report_day, "."))

  # ES: ensure timezone is set to GMT+7, as no tz was generating error after 17:00 (was returning next day, causing precip routine to skip)
  if (
    !is.null(precip_locations) &&
      report_day == as.Date(Sys.time(), tz = "Etc/GMT+7")
  ) {
    log_info(paste0(
      "[precip] Starting precipitation summaries for ",
      length(precip_locations),
      " locations on ",
      report_day,
      "."
    ))
    #This one is special: get the data and make the table at the same time, before other data as this is the time consuming step. This keeps the more important data more recent. Others get the data then process it later on.
    if (!yesterday_comments) {
      yesterday_comment_precip <- NA
    }
    message(
      "Fetching precipitation rasters and calculating a per-basin average. This could take a while."
    )
    precip_reference_time <- Sys.time()
    log_debug(paste0(
      "[precip] Using shared precip_reference_time=",
      format_log_time(precip_reference_time)
    ))

    lastWeek_all <- run_precip_window(
      location_codes = precip_locations,
      window_name = "lastWeek",
      start_time = precip_reference_time - 60 * 60 * 24 * 7,
      end_time = precip_reference_time,
      con = con
    )
    lastThree_all <- run_precip_window(
      location_codes = precip_locations,
      window_name = "lastThree",
      start_time = precip_reference_time - 60 * 60 * 24 * 3,
      end_time = precip_reference_time,
      con = con
    )
    lastTwo_all <- run_precip_window(
      location_codes = precip_locations,
      window_name = "lastTwo",
      start_time = precip_reference_time - 60 * 60 * 24 * 2,
      end_time = precip_reference_time,
      con = con
    )
    lastOne_all <- run_precip_window(
      location_codes = precip_locations,
      window_name = "lastOne",
      start_time = precip_reference_time - 60 * 60 * 24,
      end_time = precip_reference_time,
      con = con
    )
    next24_all <- run_precip_window(
      location_codes = precip_locations,
      window_name = "next24",
      start_time = precip_reference_time,
      end_time = precip_reference_time + 60 * 60 * 24,
      con = con
    )
    next48_all <- run_precip_window(
      location_codes = precip_locations,
      window_name = "next48",
      start_time = precip_reference_time,
      end_time = precip_reference_time + 60 * 60 * 48,
      con = con
    )

    extract_loc_result <- function(batch_result, loc_code) {
      if (!is.null(names(batch_result)) && loc_code %in% names(batch_result)) {
        return(batch_result[[loc_code]])
      }
      idx <- which(precip_locations == loc_code)[1]
      batch_result[[idx]]
    }

    for (idx in seq_along(precip_locations)) {
      i <- precip_locations[idx]
      location_started_at <- Sys.time()
      log_info(paste0(
        "[precip] (",
        idx,
        "/",
        length(precip_locations),
        ") Building precipitation row for ",
        i,
        " from batch results."
      ))
      log_debug(paste0("[precip] [", i, "] Station index=", idx, "."))
      name_query <- DBI::dbGetQuery(
        con,
        paste0("SELECT name FROM public.locations WHERE location_code = '", i, "'")
      )
      log_debug(paste0(
        "[precip] [",
        i,
        "] Location name query rows=",
        nrow(name_query),
        ", cols=",
        ncol(name_query),
        ", names=",
        paste(names(name_query), collapse = ",")
      ))
      if (nrow(name_query) == 0) {
        log_warn(paste0(
          "[precip] [",
          i,
          "] Location name query returned no rows."
        ))
      }
      name <- stringr::str_to_title(unique(name_query))
      log_debug(paste0(
        "[precip] [",
        i,
        "] Location name lookup returned: ",
        paste(name, collapse = " | ")
      ))
      yesterday_comment_precip <- if (yesterday_comments) {
        if (!is.null(yesterday$yesterday_locs$precipitation)) {
          yesterday$yesterday_locs$precipitation[
            yesterday$yesterday_locs$precipitation$Location == i,
            "Location.specific.comments"
          ]
        } else {
          log_warn(
            "[precip] Yesterday workbook loaded but precipitation sheet data is missing."
          )
          NA
        }
      } else {
        NA
      }
      log_debug(paste0(
        "[precip] [",
        i,
        "] Yesterday comment count=",
        length(yesterday_comment_precip),
        "; has_value=",
        !is.null(yesterday_comment_precip) &&
          length(yesterday_comment_precip) > 0 &&
          !all(is.na(yesterday_comment_precip))
      ))
      tryCatch(
        {
          lastWeek <- extract_loc_result(lastWeek_all, i)
          lastThree <- extract_loc_result(lastThree_all, i)
          lastTwo <- extract_loc_result(lastTwo_all, i)
          lastOne <- extract_loc_result(lastOne_all, i)
          next24 <- extract_loc_result(next24_all, i)
          next48 <- extract_loc_result(next48_all, i)
          yesterday_comment_precip <- if (yesterday_comments) {
            if (!is.null(yesterday$yesterday_locs$precipitation)) {
              yesterday$yesterday_locs$precipitation[
                yesterday$yesterday_locs$precipitation$Location == i,
                "Location.specific.comments"
              ]
            } else {
              log_warn(
                "[precip] Yesterday workbook loaded but precipitation sheet data is missing."
              )
              NA
            }
          } else {
            NA
          }

          precip <- rbind(
            precip,
            data.frame(
              "loc" = i,
              "name" = name,
              "lastWeek" = round(lastWeek$mean_precip, 1),
              "lastThree" = round(lastThree$mean_precip, 1),
              "lastTwo" = round(lastTwo$mean_precip, 1),
              "lastOne" = round(lastOne$mean_precip, 1),
              "next24" = round(next24$mean_precip, 1),
              "next48" = round(next48$mean_precip, 1),
              "location_comments" = NA,
              "yesterday_comments" = if (
                length(yesterday_comment_precip) < 1 |
                  is.null(yesterday_comment_precip)
              ) {
                NA
              } else {
                yesterday_comment_precip
              }
            )
          )
          log_debug(paste0(
            "[precip] Values for ",
            i,
            ": lastWeek=",
            round(lastWeek$mean_precip, 1),
            ", lastThree=",
            round(lastThree$mean_precip, 1),
            ", lastTwo=",
            round(lastTwo$mean_precip, 1),
            ", lastOne=",
            round(lastOne$mean_precip, 1),
            ", next24=",
            round(next24$mean_precip, 1),
            ", next48=",
            round(next48$mean_precip, 1)
          ))
          log_info(paste0(
            "[precip] Completed precipitation summary for ",
            i,
            "; elapsed_s=",
            round(
              as.numeric(difftime(
                Sys.time(),
                location_started_at,
                units = "secs"
              )),
              2
            ),
            "."
          ))
        },
        error = function(e) {
          precip <<- rbind(
            precip,
            data.frame(
              "loc" = i,
              "name" = name,
              "lastWeek" = NA,
              "lastThree" = NA,
              "lastTwo" = NA,
              "lastOne" = NA,
              "next24" = NA,
              "next48" = NA,
              "location_comments" = "Failed to fetch precipitation for this station.",
              "yesterday_comments" = if (
                length(yesterday_comment_precip) < 1 |
                  is.null(yesterday_comment_precip)
              ) {
                NA
              } else {
                yesterday_comment_precip
              }
            )
          )
          log_error(paste0(
            "[precip] Failed precipitation summary for ",
            i,
            ": ",
            conditionMessage(e)
          ))
          log_debug(paste0(
            "[precip] [",
            i,
            "] Failure context: location_name=",
            paste(name, collapse = " | "),
            "; yesterday_comment_count=",
            length(yesterday_comment_precip),
            "; elapsed_s=",
            round(
              as.numeric(difftime(
                Sys.time(),
                location_started_at,
                units = "secs"
              )),
              2
            )
          ))
          log_debug(paste0(
            "[precip] [",
            i,
            "] Call stack: ",
            paste(
              vapply(
                sys.calls(),
                function(call) paste(deparse(call), collapse = " "),
                character(1)
              ),
              collapse = " -> "
            )
          ))
        }
      )
    }
    log_info("[precip] Finished precipitation station loop.")
    colnames(precip) <- c(
      "Location",
      "Name",
      "past 7 days (mm)",
      "past 3 days (mm)",
      "past 2 days (mm)",
      "past 24 hrs (mm)",
      "next 24 hrs (mm)",
      "next 48 hrs (mm)",
      "Location specific comments",
      "Yesterday's comments"
    )
    precip <- inf_to_na(precip)
    if (nrow(precip) > 0) {
      tables$precipitation <- precip
      log_info(paste0(
        "[precip] Added precipitation table with ",
        nrow(precip),
        " rows."
      ))
      log_info(paste0(
        "[precip] Table contains ",
        sum(
          precip$`Location specific comments` ==
            "Failed to fetch precipitation for this station.",
          na.rm = TRUE
        ),
        " failed station rows."
      ))
    }
    #End of precip fetch loop
  } else {
    yesterday_comment_precip <- NA
    log_info(
      "[precip] Skipping precipitation summaries (locations missing or report date is not today)."
    )
  }

  ## Air temperature (ECCC Meteorology Network) -----------------------
  temperature <- data.frame()
  param_name <- "temperature, air"
  query_24h <- "
WITH temperature_values AS (
  SELECT
    l.location_code,
    l.name AS location_name,
    ts.timeseries_id,
    td.datetime,
    td.value
  FROM public.locations l
  INNER JOIN continuous.timeseries ts
    ON ts.location_id = l.location_id
  INNER JOIN public.parameters p
    ON p.parameter_id = ts.parameter_id
  INNER JOIN public.locations_networks ln
    ON ln.location_id = l.location_id
  INNER JOIN public.networks n
    ON n.network_id = ln.network_id
  INNER JOIN continuous.measurements_continuous td
    ON td.timeseries_id = ts.timeseries_id
  WHERE l.name ILIKE $1
    AND p.param_name ILIKE $2
    AND n.name ILIKE $3
    AND ts.record_rate = '01:00:00'
    AND td.datetime >= CURRENT_DATE - INTERVAL '7 day'
    AND td.datetime < CURRENT_DATE
),
historical_daily_aggregates AS (
  SELECT
    l.location_code,
    l.name AS location_name,
    ts.timeseries_id,
    td.datetime::date AS date,
    MIN(td.value) AS daily_min,
    AVG(td.value) AS daily_mean,
    MAX(td.value) AS daily_max
  FROM public.locations l
  INNER JOIN continuous.timeseries ts
    ON ts.location_id = l.location_id
  INNER JOIN public.parameters p
    ON p.parameter_id = ts.parameter_id
  INNER JOIN public.locations_networks ln
    ON ln.location_id = l.location_id
  INNER JOIN public.networks n
    ON n.network_id = ln.network_id
  INNER JOIN continuous.measurements_continuous td
    ON td.timeseries_id = ts.timeseries_id
  WHERE l.name ILIKE $1
    AND p.param_name ILIKE $2
    AND n.name ILIKE $3
    AND ts.record_rate = '01:00:00'
    AND EXTRACT(YEAR FROM td.datetime) BETWEEN 1990 AND 2020
    AND EXTRACT(DOY FROM td.datetime) = EXTRACT(DOY FROM CURRENT_DATE - INTERVAL '1 day')
    AND NOT (
      EXTRACT(MONTH FROM td.datetime) = 2
      AND EXTRACT(DAY FROM td.datetime) = 29
    )
  GROUP BY l.location_code, l.name, ts.timeseries_id, td.datetime::date
),
historical_daily_stats AS (
  SELECT
    hda.location_code,
    hda.location_name,
    hda.timeseries_id,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY hda.daily_min) AS historical_daily_min,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY hda.daily_mean) AS historical_daily_mean,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY hda.daily_max) AS historical_daily_max
  FROM historical_daily_aggregates hda
  GROUP BY hda.location_code, hda.location_name, hda.timeseries_id
),
week_history AS (
  SELECT
    l.location_code,
    l.name AS location_name,
    ts.timeseries_id,
    AVG(md.q50) AS week_hist_mean
  FROM public.locations l
  INNER JOIN continuous.timeseries ts
    ON ts.location_id = l.location_id
  INNER JOIN public.parameters p
    ON p.parameter_id = ts.parameter_id
  INNER JOIN public.locations_networks ln
    ON ln.location_id = l.location_id
  INNER JOIN public.networks n
    ON n.network_id = ln.network_id
  INNER JOIN continuous.measurements_calculated_daily md
    ON md.timeseries_id = ts.timeseries_id
  WHERE l.name ILIKE $1
    AND p.param_name ILIKE $2
    AND n.name ILIKE $3
    AND ts.record_rate = '01:00:00'
    AND md.date >= CURRENT_DATE - INTERVAL '7 day'
    AND md.date < CURRENT_DATE
  GROUP BY l.location_id, l.location_code, l.name, ts.timeseries_id
)
SELECT
  v.location_code,
  v.location_name,
  MIN(v.value) FILTER (
    WHERE v.datetime >= CURRENT_DATE - INTERVAL '1 day'
      AND v.datetime < CURRENT_DATE
  ) AS min_value,
  ROUND(
    AVG(v.value) FILTER (
      WHERE v.datetime >= CURRENT_DATE - INTERVAL '1 day'
        AND v.datetime < CURRENT_DATE
    )::numeric,
    1
  ) AS mean_value,
  MAX(v.value) FILTER (
    WHERE v.datetime >= CURRENT_DATE - INTERVAL '1 day'
      AND v.datetime < CURRENT_DATE
  ) AS max_value,
  ROUND(hds.historical_daily_min::numeric, 1) AS historical_daily_min,
  ROUND(hds.historical_daily_mean::numeric, 1) AS historical_daily_mean,
  ROUND(hds.historical_daily_max::numeric, 1) AS historical_daily_max,
  ROUND(MIN(v.value)::numeric, 1) AS week_min,
  ROUND(MAX(v.value)::numeric, 1) AS week_max,
  ROUND(AVG(v.value)::numeric, 1) AS week_mean,
  ROUND(
    ROUND(AVG(v.value)::numeric, 1) - wh.week_hist_mean,
    1
  ) AS week_difference_historical_mean
FROM temperature_values v
LEFT JOIN historical_daily_stats hds
  ON hds.location_code = v.location_code
  AND hds.timeseries_id = v.timeseries_id
LEFT JOIN week_history wh
  ON wh.location_code = v.location_code
  AND wh.timeseries_id = v.timeseries_id
GROUP BY
  v.location_code,
  v.location_name,
  hds.historical_daily_min,
  hds.historical_daily_mean,
  hds.historical_daily_max,
  wh.week_hist_mean,
  v.timeseries_id
ORDER BY v.location_name, v.timeseries_id;
"

  temperature <- DBI::dbGetQuery(
    con,
    query_24h,
    params = list(
      "%",
      paste0("%", param_name, "%"),
      "ECCC Meteorology Network"
    )
  )

  if (nrow(temperature) > 0) {
    names(temperature)[names(temperature) == "location_code"] <- "Location"
    names(temperature)[names(temperature) == "location_name"] <- "Name"
    names(temperature)[
      names(temperature) == "min_value"
    ] <- "y'day Tmin (\u2103)"
    names(temperature)[
      names(temperature) == "mean_value"
    ] <- "y'day Tmean (\u2103)"
    names(temperature)[
      names(temperature) == "max_value"
    ] <- "y'day Tmax (\u2103)"
    names(temperature)[
      names(temperature) == "historical_daily_min"
    ] <- "hist daily Tmin (\u2103)"
    names(temperature)[
      names(temperature) == "historical_daily_mean"
    ] <- "hist daily Tmean (\u2103)"
    names(temperature)[
      names(temperature) == "historical_daily_max"
    ] <- "hist daily Tmax (\u2103)"
    names(temperature)[names(temperature) == "week_min"] <- "1 wk Tmin (\u2103)"
    names(temperature)[names(temperature) == "week_max"] <- "1 wk Tmax (\u2103)"
    names(temperature)[
      names(temperature) == "week_mean"
    ] <- "1 wk Tmean (\u2103)"
    names(temperature)[
      names(temperature) == "week_difference_historical_mean"
    ] <- "diff. from 1 wk historical mean (\u2103)"

    temperature$`Location specific comments` <- NA
    temperature$`Yesterday's comments` <- NA

    if (yesterday_comments && !is.null(yesterday$yesterday_locs$temperature)) {
      idx <- match(
        temperature$Location,
        yesterday$yesterday_locs$temperature$Location
      )
      temperature$`Yesterday's comments` <- yesterday$yesterday_locs$temperature[
        idx,
        "Location.specific.comments"
      ]
    }

    tables$temperature <- inf_to_na(temperature)
  }

  ## Water level ------------------------
  level_daily <- list()
  level_rt <- list()
  if (!is.null(level_locations)) {
    names_level <- NULL
    for (i in 1:nrow(level_locations)) {
      daily <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT value, date, percent_historic_range, max, min, q50 FROM continuous.measurements_calculated_daily WHERE date = '",
          report_day,
          "' AND timeseries_id = ",
          level_locations[i, "timeseries_id"],
          ";"
        )
      )
      if (nrow(daily) == 0) {
        daily <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT value, date, percent_historic_range, max, min, q50 FROM continuous.measurements_calculated_daily WHERE date = '",
            report_day - 1,
            "'AND timeseries_id = ",
            level_locations[i, "timeseries_id"],
            ";"
          )
        )
      }
      if (nrow(daily) > 0) {
        level_daily[[level_locations[i, "location"]]] <- daily
      }
      rt <- DBI::dbGetQuery(
        con,
        "SELECT value_corrected AS value, datetime
         FROM continuous.measurements_continuous_corrected($1, $2, $3)",
        params = list(
          level_locations[i, "timeseries_id"],
          .POSIXct(report_time, "UTC") - (past + 2) * 60 * 60 * 24,
          .POSIXct(report_time, "UTC")
        )
      )
      if (nrow(rt) > 0) {
        level_rt[[level_locations[i, "location"]]] <- rt
      }
      if (nrow(rt) > 0 || nrow(daily) > 0) {
        names_level[level_locations[
          i,
          "location"
        ]] <- stringr::str_to_title(unique(DBI::dbGetQuery(
          con,
          paste0(
            "SELECT name FROM public.locations WHERE location_code = '",
            level_locations[i, "location"],
            "'"
          )
        )))
      }
    }
  }

  ## Water flow ---------------------------
  flow_daily <- list()
  flow_rt <- list()
  if (!is.null(flow_locations)) {
    names_flow <- NULL
    for (i in 1:nrow(flow_locations)) {
      daily <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT value, date, percent_historic_range, max, min, q50 FROM continuous.measurements_calculated_daily WHERE date = '",
          report_day,
          "' AND timeseries_id = ",
          flow_locations[i, "timeseries_id"],
          ";"
        )
      )
      if (nrow(daily) == 0) {
        daily <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT value, date, percent_historic_range, max, min, q50 FROM continuous.measurements_calculated_daily WHERE date = '",
            report_day - 1,
            "'AND timeseries_id = ",
            flow_locations[i, "timeseries_id"],
            ";"
          )
        )
      }
      if (nrow(daily) > 0) {
        flow_daily[[flow_locations[i, "location"]]] <- daily
      }
      rt <- DBI::dbGetQuery(
        con,
        "SELECT value_corrected AS value, datetime
         FROM continuous.measurements_continuous_corrected($1, $2, $3)",
        params = list(
          flow_locations[i, "timeseries_id"],
          .POSIXct(report_time, "UTC") - (past + 2) * 60 * 60 * 24,
          .POSIXct(report_time, "UTC")
        )
      )
      if (nrow(rt) > 0) {
        flow_rt[[flow_locations[i, "location"]]] <- rt
      }
      if (nrow(rt) > 0 || nrow(daily) > 0) {
        names_flow[flow_locations[
          i,
          "location"
        ]] <- stringr::str_to_title(unique(DBI::dbGetQuery(
          con,
          paste0(
            "SELECT name FROM public.locations WHERE location_code = '",
            flow_locations[i, "location"],
            "'"
          )
        )))
      }
    }
  }

  ## Snow pack --------------------------
  snow_daily <- list()
  snow_rt <- list()
  if (!is.null(snow_locations)) {
    names_snow <- NULL
    for (i in 1:nrow(snow_locations)) {
      daily <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT value, date, percent_historic_range, max, min, q50 FROM continuous.measurements_calculated_daily WHERE date = '",
          report_day,
          "' AND timeseries_id = ",
          snow_locations[i, "timeseries_id"],
          ";"
        )
      )
      if (nrow(daily) == 0) {
        daily <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT value, date, percent_historic_range, max, min, q50 FROM continuous.measurements_calculated_daily WHERE date = '",
            report_day - 1,
            "'AND timeseries_id = ",
            snow_locations[i, "timeseries_id"],
            ";"
          )
        )
      }
      if (nrow(daily) > 0) {
        snow_daily[[snow_locations[i, "location"]]] <- daily
      }
      rt <- DBI::dbGetQuery(
        con,
        "SELECT value_corrected AS value, datetime
         FROM continuous.measurements_continuous_corrected($1, $2, $3)",
        params = list(
          snow_locations[i, "timeseries_id"],
          .POSIXct(report_time, "UTC") - (past + 2) * 60 * 60 * 24,
          .POSIXct(report_time, "UTC")
        )
      )
      if (nrow(rt) > 0) {
        snow_rt[[snow_locations[i, "location"]]] <- rt
      }
      if (nrow(rt) > 0 || nrow(daily) > 0) {
        names_snow[snow_locations[
          i,
          "location"
        ]] <- stringr::str_to_title(unique(DBI::dbGetQuery(
          con,
          paste0(
            "SELECT name FROM public.locations WHERE location_code = '",
            snow_locations[i, "location"],
            "'"
          )
        )))
      }
    }
  }

  ## Bridge freeboard --------------------------
  bridges_daily <- list()
  bridges_rt <- list()
  if (!is.null(bridge_locations)) {
    names_bridges <- NULL
    for (i in 1:nrow(bridge_locations)) {
      daily <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT value, date, percent_historic_range, max, min, q50 FROM continuous.measurements_calculated_daily WHERE date = '",
          report_day,
          "' AND timeseries_id = ",
          bridge_locations[i, "timeseries_id"],
          ";"
        )
      )
      if (nrow(daily) == 0) {
        daily <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT value, date, percent_historic_range, max, min, q50 FROM continuous.measurements_calculated_daily WHERE date = '",
            report_day - 1,
            "'AND timeseries_id = ",
            bridge_locations[i, "timeseries_id"],
            ";"
          )
        )
      }
      if (nrow(daily) > 0) {
        bridges_daily[[bridge_locations[i, "location"]]] <- daily
      }
      rt <- DBI::dbGetQuery(
        con,
        "SELECT value_corrected AS value, datetime
         FROM continuous.measurements_continuous_corrected($1, $2, $3)",
        params = list(
          bridge_locations[i, "timeseries_id"],
          .POSIXct(report_time, "UTC") - (past + 2) * 60 * 60 * 24,
          .POSIXct(report_time, "UTC")
        )
      )
      if (nrow(rt) > 0) {
        bridges_rt[[bridge_locations[i, "location"]]] <- rt
      }
      if (nrow(rt) > 0 || nrow(daily) > 0) {
        names_bridges[bridge_locations[
          i,
          "location"
        ]] <- stringr::str_to_title(unique(DBI::dbGetQuery(
          con,
          paste0(
            "SELECT name FROM public.locations WHERE location_code = '",
            bridge_locations[i, "location"],
            "'"
          )
        )))
      }
    }
  } #End of data acquisition

  # Generate tables ----------------
  ## Level table -------------------
  if (length(level_rt) > 0) {
    #generate level table
    levels <- data.frame()
    for (i in names(level_rt)) {
      rt <- level_rt[[i]]
      last_time <- rt[rt$datetime == max(rt$datetime), ]$datetime
      age <- difftime(report_time, last_time, units = "hours")
      latest <- stats::median(
        rt[
          rt$datetime <= last_time & rt$datetime >= last_time - 60 * 30,
        ]$value
      ) #median of last 30 minutes of data
      percent_historic <- round(
        ((latest - level_daily[[i]]$min) /
          (level_daily[[i]]$max - level_daily[[i]]$min)) *
          100,
        0
      )
      percent_mean <- round(
        ((latest - level_daily[[i]]$min) /
          (level_daily[[i]]$q50 - level_daily[[i]]$min)) *
          100,
        0
      )
      day <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 24 &
            rt$datetime >= last_time - 60 * 60 * 24.5,
        ]$value
      ) #median of 30 minutes
      twoday <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 47.5 &
            rt$datetime >= last_time - 60 * 60 * 48.5,
        ]$value
      ) #median of 1 hour
      threeday <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 71.5 &
            rt$datetime >= last_time - 60 * 60 * 72.5,
        ]$value
      ) #median of 1 hour
      week <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 167 &
            rt$datetime >= last_time - 60 * 60 * 169,
        ]$value
      ) #median of 2 hours
      if (is.na(week)) {
        #expand the range if no data within the 2 hour timespan
        week <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 165 &
              rt$datetime >= last_time - 60 * 60 * 171,
          ]$value
        )
      }
      yesterday_comment_levels <- if (yesterday_comments) {
        yesterday$yesterday_locs$levels[
          yesterday$yesterday_locs$levels$Location == i,
          "Location.specific.comments"
        ]
      } else {
        NA
      }

      if (past <= 7) {
        levels <- rbind(
          levels,
          data.frame(
            "loc" = i,
            "name" = names_level[i],
            "level" = if (!is.na(latest)) round(latest, 3) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
            "48" = if (!is.na(twoday)) {
              round((latest - twoday) * 100, 1)
            } else {
              NA
            },
            "72" = if (!is.na(threeday)) {
              round((latest - threeday) * 100, 1)
            } else {
              NA
            },
            "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_levels) < 1 |
                is.null(yesterday_comment_levels)
            ) {
              NA
            } else {
              yesterday_comment_levels
            }
          )
        )
      }
      if (past > 7 && past <= 14) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        levels <- rbind(
          levels,
          data.frame(
            "loc" = i,
            "name" = names_level[i],
            "level" = if (!is.na(latest)) round(latest, 3) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
            "48" = if (!is.na(twoday)) {
              round((latest - twoday) * 100, 1)
            } else {
              NA
            },
            "72" = if (!is.na(threeday)) {
              round((latest - threeday) * 100, 1)
            } else {
              NA
            },
            "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek) * 100, 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = yesterday_comment_levels
          )
        )
      }
      if (past > 14 && past <= 21) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        threeweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 503 &
              rt$datetime >= last_time - 60 * 60 * 505,
          ]$value
        )
        if (is.na(threeweek)) {
          threeweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 497 &
                rt$datetime >= last_time - 60 * 60 * 511,
            ]$value
          )
        }
        levels <- rbind(
          levels,
          data.frame(
            "loc" = i,
            "name" = names_level[i],
            "level" = if (!is.na(latest)) round(latest, 3) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
            "48" = if (!is.na(twoday)) {
              round((latest - twoday) * 100, 1)
            } else {
              NA
            },
            "72" = if (!is.na(threeday)) {
              round((latest - threeday) * 100, 1)
            } else {
              NA
            },
            "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek) * 100, 1)
            } else {
              NA
            },
            "threeweek" = if (!is.na(threeweek)) {
              round((latest - threeweek) * 100, 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_levels) < 1 |
                is.null(yesterday_comment_levels)
            ) {
              NA
            } else {
              yesterday_comment_levels
            }
          )
        )
      }
      if (past > 21) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        threeweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 503 &
              rt$datetime >= last_time - 60 * 60 * 505,
          ]$value
        )
        if (is.na(threeweek)) {
          threeweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 497 &
                rt$datetime >= last_time - 60 * 60 * 511,
            ]$value
          )
        }
        fourweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 671 &
              rt$datetime >= last_time - 60 * 60 * 673,
          ]$value
        )
        if (is.na(fourweek)) {
          fourweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 667 &
                rt$datetime >= last_time - 60 * 60 * 677,
            ]$value
          )
        }
        levels <- rbind(
          levels,
          data.frame(
            "loc" = i,
            "name" = names_level[i],
            "level" = if (!is.na(latest)) round(latest, 3) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
            "48" = if (!is.na(twoday)) {
              round((latest - twoday) * 100, 1)
            } else {
              NA
            },
            "72" = if (!is.na(threeday)) {
              round((latest - threeday) * 100, 1)
            } else {
              NA
            },
            "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek) * 100, 1)
            } else {
              NA
            },
            "threeweek" = if (!is.na(threeweek)) {
              round((latest - threeweek) * 100, 1)
            } else {
              NA
            },
            "fourweek" = if (!is.na(fourweek)) {
              round((latest - fourweek) * 100, 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_levels) < 1 |
                is.null(yesterday_comment_levels)
            ) {
              NA
            } else {
              yesterday_comment_levels
            }
          )
        )
      }
    }
    if (past <= 7) {
      colnames(levels) <- c(
        "Location",
        "Name",
        " Level (m)",
        "% hist rng",
        "% hist mean",
        "24 hr chg (cm)",
        "48 hr chg (cm)",
        "72 hr chg (cm)",
        "1 week chg (cm)",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 7 && past <= 14) {
      colnames(levels) <- c(
        "Location",
        "Name",
        " Level (m)",
        "% hist rng",
        "% hist mean",
        "24 hr chg (cm)",
        "48 hr chg (cm)",
        "72 hr chg (cm)",
        "1 week chg (cm)",
        "2 week chg (cm)",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 14 && past <= 21) {
      colnames(levels) <- c(
        "Location",
        "Name",
        " Level (m)",
        "% hist rng",
        "% hist mean",
        "24 hr chg (cm)",
        "48 hr chg (cm)",
        "72 hr chg (cm)",
        "1 week chg (cm)",
        "2 week chg (cm)",
        "3 week chg (cm)",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 21) {
      colnames(levels) <- c(
        "Location",
        "Name",
        " Level (m)",
        "% hist rng",
        "% hist mean",
        "24 hr chg (cm)",
        "48 hr chg (cm)",
        "72 hr chg (cm)",
        "1 week chg (cm)",
        "2 week chg (cm)",
        "3 week chg (cm)",
        "4 week chg (cm)",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    levels <- inf_to_na(levels)
    tables$levels <- levels
  }

  ## Flow table ---------------------------
  if (length(flow_rt) > 0) {
    #generate flow table
    flows <- data.frame()
    for (i in names(flow_rt)) {
      rt <- flow_rt[[i]]
      last_time <- rt[rt$datetime == max(rt$datetime), ]$datetime
      age <- difftime(report_time, last_time, units = "hours")
      latest <- stats::median(
        rt[
          rt$datetime <= last_time & rt$datetime >= last_time - 60 * 30,
        ]$value
      ) #median of last 30 minutes of data
      percent_historic <- round(
        ((latest - flow_daily[[i]]$min) /
          (flow_daily[[i]]$max - flow_daily[[i]]$min)) *
          100,
        0
      )
      percent_mean <- round(latest / flow_daily[[i]]$q50 * 100, 0)
      day <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 24 &
            rt$datetime >= last_time - 60 * 60 * 24.5,
        ]$value
      ) #median of 30 minutes
      twoday <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 47.5 &
            rt$datetime >= last_time - 60 * 60 * 48.5,
        ]$value
      ) #median of 1 hour
      threeday <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 71.5 &
            rt$datetime >= last_time - 60 * 60 * 72.5,
        ]$value
      ) #median of 1 hour
      week <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 167 &
            rt$datetime >= last_time - 60 * 60 * 169,
        ]$value
      ) #median of 2 hours
      if (is.na(week)) {
        #expand the range if no data within the 2 hour timespan
        week <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 165 &
              rt$datetime >= last_time - 60 * 60 * 171,
          ]$value
        )
      }
      yesterday_comment_flows <- if (yesterday_comments) {
        yesterday$yesterday_locs$flows[
          yesterday$yesterday_locs$flows$Location == i,
          "Location.specific.comments"
        ]
      } else {
        NA
      }

      if (past <= 7) {
        flows <- rbind(
          flows,
          data.frame(
            "loc" = i,
            "name" = names_flow[i],
            "flow" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day), 1) else NA,
            "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
            "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
            "week" = if (!is.na(week)) round((latest - week), 1) else NA,
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_flows) < 1 |
                is.null(yesterday_comment_flows)
            ) {
              NA
            } else {
              yesterday_comment_flows
            }
          )
        )
      }
      if (past > 7 && past <= 14) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        flows <- rbind(
          flows,
          data.frame(
            "loc" = i,
            "name" = names_flow[i],
            "flow" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day), 1) else NA,
            "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
            "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
            "week" = if (!is.na(week)) round((latest - week), 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek), 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_flows) < 1 |
                is.null(yesterday_comment_flows)
            ) {
              NA
            } else {
              yesterday_comment_flows
            }
          )
        )
      }
      if (past > 14 && past <= 21) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        threeweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 503 &
              rt$datetime >= last_time - 60 * 60 * 505,
          ]$value
        )
        if (is.na(threeweek)) {
          threeweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 497 &
                rt$datetime >= last_time - 60 * 60 * 511,
            ]$value
          )
        }
        flows <- rbind(
          flows,
          data.frame(
            "loc" = i,
            "name" = names_flow[i],
            "flow" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day), 1) else NA,
            "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
            "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
            "week" = if (!is.na(week)) round((latest - week), 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek), 1)
            } else {
              NA
            },
            "threeweek" = if (!is.na(threeweek)) {
              round((latest - threeweek), 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_flows) < 1 |
                is.null(yesterday_comment_flows)
            ) {
              NA
            } else {
              yesterday_comment_flows
            }
          )
        )
      }
      if (past > 21) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        threeweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 503 &
              rt$datetime >= last_time - 60 * 60 * 505,
          ]$value
        )
        if (is.na(threeweek)) {
          threeweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 497 &
                rt$datetime >= last_time - 60 * 60 * 511,
            ]$value
          )
        }
        fourweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 671 &
              rt$datetime >= last_time - 60 * 60 * 673,
          ]$value
        )
        if (is.na(fourweek)) {
          fourweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 667 &
                rt$datetime >= last_time - 60 * 60 * 677,
            ]$value
          )
        }
        flows <- rbind(
          flows,
          data.frame(
            "loc" = i,
            "name" = names_flow[i],
            "flow" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day), 1) else NA,
            "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
            "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
            "week" = if (!is.na(week)) round((latest - week), 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek), 1)
            } else {
              NA
            },
            "threeweek" = if (!is.na(threeweek)) {
              round((latest - threeweek), 1)
            } else {
              NA
            },
            "fourweek" = if (!is.na(fourweek)) {
              round((latest - fourweek), 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_flows) < 1 |
                is.null(yesterday_comment_flows)
            ) {
              NA
            } else {
              yesterday_comment_flows
            }
          )
        )
      }
    }
    if (past <= 7) {
      colnames(flows) <- c(
        "Location",
        "Name",
        " Flow (m3/s)",
        "% hist rng",
        "% hist mean",
        "24 hr chg",
        "48 hr chg",
        "72 hr chg",
        "1 week chg",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 7 && past <= 14) {
      colnames(flows) <- c(
        "Location",
        "Name",
        " Flow (m3/s)",
        "% hist rng",
        "% hist mean",
        "24 hr chg",
        "48 hr chg",
        "72 hr chg",
        "1 week chg",
        "2 week chg",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 14 && past <= 21) {
      colnames(flows) <- c(
        "Location",
        "Name",
        " Flow (m3/s)",
        "% hist rng",
        "% hist mean",
        "24 hr chg",
        "48 hr chg",
        "72 hr chg",
        "1 week chg",
        "2 week chg",
        "3 week chg",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 21) {
      colnames(flows) <- c(
        "Location",
        "Name",
        " Flow (m3/s)",
        "% hist rng",
        "% hist mean",
        "24 hr chg",
        "48 hr chg",
        "72 hr chg",
        "1 week chg",
        "2 week chg",
        "3 week chg",
        "4 week chg",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    flows <- inf_to_na(flows)
    tables$flows <- flows
  }

  ## Snow table --------------------------
  if (length(snow_rt) > 0) {
    #generate snow table
    snow <- data.frame()
    for (i in names(snow_rt)) {
      rt <- snow_rt[[i]]
      last_time <- rt[rt$datetime == max(rt$datetime), ]$datetime
      age <- difftime(report_time, last_time, units = "hours")
      latest <- stats::median(
        rt[
          rt$datetime <= last_time & rt$datetime >= last_time - 60 * 30,
        ]$value
      ) #median of last 30 minutes of data
      percent_historic <- round(
        ((latest - snow_daily[[i]]$min) /
          (snow_daily[[i]]$max - snow_daily[[i]]$min)) *
          100,
        0
      )
      percent_mean <- round(latest / snow_daily[[i]]$q50 * 100, 0)
      day <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 24 &
            rt$datetime >= last_time - 60 * 60 * 24.5,
        ]$value
      ) #median of 30 minutes
      twoday <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 47.5 &
            rt$datetime >= last_time - 60 * 60 * 48.5,
        ]$value
      ) #median of 1 hour
      threeday <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 71.5 &
            rt$datetime >= last_time - 60 * 60 * 72.5,
        ]$value
      ) #median of 1 hour
      week <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 167 &
            rt$datetime >= last_time - 60 * 60 * 169,
        ]$value
      ) #median of 2 hours
      if (is.na(week)) {
        #expand the range if no data within the 2 hour timespan
        week <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 165 &
              rt$datetime >= last_time - 60 * 60 * 171,
          ]$value
        )
      }
      yesterday_comment_snow <- if (yesterday_comments) {
        yesterday$yesterday_locs$snow[
          yesterday$yesterday_locs$snow$Location == i,
          "Location.specific.comments"
        ]
      } else {
        NA
      }

      if (past <= 7) {
        snow <- rbind(
          snow,
          data.frame(
            "loc" = i,
            "name" = names_snow[i],
            "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day), 1) else NA,
            "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
            "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
            "week" = if (!is.na(week)) round((latest - week), 1) else NA,
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_snow) < 1 |
                is.null(yesterday_comment_snow)
            ) {
              NA
            } else {
              yesterday_comment_snow
            }
          )
        )
      }
      if (past > 7 && past <= 14) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        snow <- rbind(
          snow,
          data.frame(
            "loc" = i,
            "name" = names_snow[i],
            "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day), 1) else NA,
            "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
            "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
            "week" = if (!is.na(week)) round((latest - week), 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek), 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_snow) < 1 |
                is.null(yesterday_comment_snow)
            ) {
              NA
            } else {
              yesterday_comment_snow
            }
          )
        )
      }
      if (past > 14 && past <= 21) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        threeweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 503 &
              rt$datetime >= last_time - 60 * 60 * 505,
          ]$value
        )
        if (is.na(threeweek)) {
          threeweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 497 &
                rt$datetime >= last_time - 60 * 60 * 511,
            ]$value
          )
        }
        snow <- rbind(
          snow,
          data.frame(
            "loc" = i,
            "name" = names_snow[i],
            "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day), 1) else NA,
            "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
            "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
            "week" = if (!is.na(week)) round((latest - week), 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek), 1)
            } else {
              NA
            },
            "threeweek" = if (!is.na(threeweek)) {
              round((latest - threeweek), 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_snow) < 1 |
                is.null(yesterday_comment_snow)
            ) {
              NA
            } else {
              yesterday_comment_snow
            }
          )
        )
      }
      if (past > 21) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        threeweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 503 &
              rt$datetime >= last_time - 60 * 60 * 505,
          ]$value
        )
        if (is.na(threeweek)) {
          threeweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 497 &
                rt$datetime >= last_time - 60 * 60 * 511,
            ]$value
          )
        }
        fourweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 671 &
              rt$datetime >= last_time - 60 * 60 * 673,
          ]$value
        )
        if (is.na(fourweek)) {
          fourweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 667 &
                rt$datetime >= last_time - 60 * 60 * 677,
            ]$value
          )
        }
        snow <- rbind(
          snow,
          data.frame(
            "loc" = i,
            "name" = names_snow[i],
            "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day), 1) else NA,
            "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
            "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
            "week" = if (!is.na(week)) round((latest - week), 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek), 1)
            } else {
              NA
            },
            "threeweek" = if (!is.na(threeweek)) {
              round((latest - threeweek), 1)
            } else {
              NA
            },
            "fourweek" = if (!is.na(fourweek)) {
              round((latest - fourweek), 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_snow) < 1 |
                is.null(yesterday_comment_snow)
            ) {
              NA
            } else {
              yesterday_comment_snow
            }
          )
        )
      }
    }
    if (past <= 7) {
      colnames(snow) <- c(
        "Location",
        "Name",
        "SWE (mm)",
        "% hist rng",
        "% hist mean",
        "24 hr chg",
        "48 hr chg",
        "72 hr chg",
        "1 week chg",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 7 && past <= 14) {
      colnames(snow) <- c(
        "Location",
        "Name",
        "SWE (mm)",
        "% hist rng",
        "% hist mean",
        "24 hr chg",
        "48 hr chg",
        "72 hr chg",
        "1 week chg",
        "2 week chg",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 14 && past <= 21) {
      colnames(snow) <- c(
        "Location",
        "Name",
        "SWE (mm)",
        "% hist rng",
        "% hist mean",
        "24 hr chg",
        "48 hr chg",
        "72 hr chg",
        "1 week chg",
        "2 week chg",
        "3 week chg",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 21) {
      colnames(snow) <- c(
        "Location",
        "Name",
        "SWE (mm)",
        "% hist rng",
        "% hist mean",
        "24 hr chg",
        "48 hr chg",
        "72 hr chg",
        "1 week chg",
        "2 week chg",
        "3 week chg",
        "4 week chg",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    snow <- inf_to_na(snow)
    tables$snow <- snow
  }

  ## Bridges table ----------------------
  if (length(bridges_rt) > 0) {
    #generate bridges table
    bridges <- data.frame()
    for (i in names(bridges_rt)) {
      rt <- bridges_rt[[i]]
      last_time <- rt[rt$datetime == max(rt$datetime), ]$datetime
      age <- difftime(report_time, last_time, units = "hours")
      latest <- stats::median(
        rt[
          rt$datetime <= last_time & rt$datetime >= last_time - 60 * 30,
        ]$value
      ) #median of last 30 minutes of data
      percent_historic <- round(
        ((latest - bridges_daily[[i]]$min) /
          (bridges_daily[[i]]$max - bridges_daily[[i]]$min)) *
          100,
        0
      )
      percent_mean <- round(
        ((latest - bridges_daily[[i]]$min) /
          (bridges_daily[[i]]$q50 - bridges_daily[[i]]$min)) *
          100,
        0
      )
      day <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 24 &
            rt$datetime >= last_time - 60 * 60 * 24.5,
        ]$value
      ) #median of 30 minutes
      twoday <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 47.5 &
            rt$datetime >= last_time - 60 * 60 * 48.5,
        ]$value
      ) #median of 1 hour
      threeday <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 71.5 &
            rt$datetime >= last_time - 60 * 60 * 72.5,
        ]$value
      ) #median of 1 hour
      week <- stats::median(
        rt[
          rt$datetime <= last_time - 60 * 60 * 167 &
            rt$datetime >= last_time - 60 * 60 * 169,
        ]$value
      ) #median of 2 hours
      if (is.na(week)) {
        #expand the range if no data within the 2 hour timespan
        week <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 165 &
              rt$datetime >= last_time - 60 * 60 * 171,
          ]$value
        )
      }
      yesterday_comment_bridges <- if (yesterday_comments) {
        yesterday$yesterday_locs$bridges[
          yesterday$yesterday_locs$bridges$Location == i,
          "Location.specific.comments"
        ]
      } else {
        NA
      }

      if (past <= 7) {
        bridges <- rbind(
          bridges,
          data.frame(
            "loc" = i,
            "name" = names_bridges[i],
            "distance" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
            "48" = if (!is.na(twoday)) {
              round((latest - twoday) * 100, 1)
            } else {
              NA
            },
            "72" = if (!is.na(threeday)) {
              round((latest - threeday) * 100, 1)
            } else {
              NA
            },
            "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_bridges) < 1 |
                is.null(yesterday_comment_bridges)
            ) {
              NA
            } else {
              yesterday_comment_bridges
            }
          )
        )
      }
      if (past > 7 && past <= 14) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        bridges <- rbind(
          bridges,
          data.frame(
            "loc" = i,
            "name" = names_bridges[i],
            "distance" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
            "48" = if (!is.na(twoday)) {
              round((latest - twoday) * 100, 1)
            } else {
              NA
            },
            "72" = if (!is.na(threeday)) {
              round((latest - threeday) * 100, 1)
            } else {
              NA
            },
            "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek) * 100, 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_bridges) < 1 |
                is.null(yesterday_comment_bridges)
            ) {
              NA
            } else {
              yesterday_comment_bridges
            }
          )
        )
      }
      if (past > 14 && past <= 21) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        threeweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 503 &
              rt$datetime >= last_time - 60 * 60 * 505,
          ]$value
        )
        if (is.na(threeweek)) {
          threeweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 497 &
                rt$datetime >= last_time - 60 * 60 * 511,
            ]$value
          )
        }
        bridges <- rbind(
          bridges,
          data.frame(
            "loc" = i,
            "name" = names_bridges[i],
            "distance" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
            "48" = if (!is.na(twoday)) {
              round((latest - twoday) * 100, 1)
            } else {
              NA
            },
            "72" = if (!is.na(threeday)) {
              round((latest - threeday) * 100, 1)
            } else {
              NA
            },
            "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek) * 100, 1)
            } else {
              NA
            },
            "threeweek" = if (!is.na(threeweek)) {
              round((latest - threeweek) * 100, 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_bridges) < 1 |
                is.null(yesterday_comment_bridges)
            ) {
              NA
            } else {
              yesterday_comment_bridges
            }
          )
        )
      }
      if (past > 21) {
        twoweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 335 &
              rt$datetime >= last_time - 60 * 60 * 337,
          ]$value
        )
        if (is.na(twoweek)) {
          twoweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 331 &
                rt$datetime >= last_time - 60 * 60 * 341,
            ]$value
          )
        }
        threeweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 503 &
              rt$datetime >= last_time - 60 * 60 * 505,
          ]$value
        )
        if (is.na(threeweek)) {
          threeweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 497 &
                rt$datetime >= last_time - 60 * 60 * 511,
            ]$value
          )
        }
        fourweek <- stats::median(
          rt[
            rt$datetime <= last_time - 60 * 60 * 671 &
              rt$datetime >= last_time - 60 * 60 * 673,
          ]$value
        )
        if (is.na(fourweek)) {
          fourweek <- stats::median(
            rt[
              rt$datetime <= last_time - 60 * 60 * 667 &
                rt$datetime >= last_time - 60 * 60 * 677,
            ]$value
          )
        }
        bridges <- rbind(
          bridges,
          data.frame(
            "loc" = i,
            "name" = names_bridges[i],
            "distance" = if (!is.na(latest)) round(latest, 1) else NA,
            "percent" = if (length(percent_historic == 1)) {
              percent_historic
            } else {
              NA
            },
            "mean" = if (length(percent_mean == 1)) percent_mean else NA,
            "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
            "48" = if (!is.na(twoday)) {
              round((latest - twoday) * 100, 1)
            } else {
              NA
            },
            "72" = if (!is.na(threeday)) {
              round((latest - threeday) * 100, 1)
            } else {
              NA
            },
            "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
            "twoweek" = if (!is.na(twoweek)) {
              round((latest - twoweek) * 100, 1)
            } else {
              NA
            },
            "threeweek" = if (!is.na(threeweek)) {
              round((latest - threeweek) * 100, 1)
            } else {
              NA
            },
            "fourweek" = if (!is.na(fourweek)) {
              round((latest - fourweek) * 100, 1)
            } else {
              NA
            },
            "age" = substr(format(last_time, tz = "MST"), 1, 16),
            "Hrs" = as.numeric(paste0(round(age[1], 1))),
            "location_comment" = NA,
            "yesterday_comments" = if (
              length(yesterday_comment_bridges) < 1 |
                is.null(yesterday_comment_bridges)
            ) {
              NA
            } else {
              yesterday_comment_bridges
            }
          )
        )
      }
    }
    if (past <= 7) {
      colnames(bridges) <- c(
        "Location",
        "Name",
        " Distance (m)",
        "% hist rng",
        "% hist mean",
        "24 hr chg (cm)",
        "48 hr chg (cm)",
        "72 hr chg (cm)",
        "1 week chg",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 7 && past <= 14) {
      colnames(bridges) <- c(
        "Location",
        "Name",
        " Distance (m)",
        "% hist rng",
        "% hist mean",
        "24 hr chg (cm)",
        "48 hr chg (cm)",
        "72 hr chg (cm)",
        "1 week chg (cm)",
        "2 week chg (cm)",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 14 && past <= 21) {
      colnames(bridges) <- c(
        "Location",
        "Name",
        " Distance (m)",
        "% hist rng",
        "% hist mean",
        "24 hr chg (cm)",
        "48 hr chg (cm)",
        "72 hr chg (cm)",
        "1 week chg (cm)",
        "2 week chg (cm)",
        "3 week chg (cm)",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    if (past > 21) {
      colnames(bridges) <- c(
        "Location",
        "Name",
        " Distance (m)",
        "% hist rng",
        "% hist mean",
        "24 hr chg (cm)",
        "48 hr chg (cm)",
        "72 hr chg (cm)",
        "1 week chg (cm)",
        "2 week chg (cm)",
        "3 week chg (cm)",
        "4 week chg (cm)",
        "Last data MST",
        "Hrs",
        "Location specific comments",
        "Yesterday's comments"
      )
    }
    bridges <- inf_to_na(bridges)
    tables$bridges <- bridges
  }

  # Make the Excel workbook ---------------------------
  wb <- openxlsx::createWorkbook(
    creator = "Ghislain de Laplante (via automated process)",
    title = "Hydrometric Condition Report"
  )
  time <- report_time
  head <- data.frame(
    paste0("Issued at ", substr(format(time, tz = "MST"), 1, 16), " MST"),
    NA,
    "Forecaster name:",
    NA,
    NA,
    NA,
    paste0("Created with YGwater ", utils::packageVersion("YGwater"))
  )
  headStyle <- openxlsx::createStyle(fgFill = "turquoise2")
  fodNameStyle <- openxlsx::createStyle(
    fgFill = "darkorange",
    border = "TopBottomLeftRight",
    borderStyle = "medium"
  )
  fodCommentStyle <- openxlsx::createStyle(fgFill = "lightsteelblue")
  yesterdayFodCommentStyle <- openxlsx::createStyle(
    fgFill = "lightyellow",
    textDecoration = "italic"
  )
  colStyleYellow <- openxlsx::createStyle(bgFill = "yellow")
  colStyleRed <- openxlsx::createStyle(bgFill = "red")
  generalCommentStyle <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    fgFill = "lightsteelblue",
    wrapText = TRUE
  )
  generalCommentStyle2 <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    textDecoration = "bold",
    fgFill = "lightsteelblue",
    wrapText = TRUE
  )
  yesterdayGeneralCommentStyle <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    fgFill = "lightyellow",
    wrapText = TRUE,
    textDecoration = "italic"
  )
  yesterdayGeneralCommentStyle2 <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    textDecoration = c("bold", "italic"),
    fgFill = "lightyellow",
    wrapText = TRUE
  )
  publicCommentStyle <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    fgFill = "orange",
    wrapText = TRUE
  )
  publicCommentStyle2 <- openxlsx::createStyle(
    border = "TopBottomLeftRight",
    textDecoration = "bold",
    fgFill = "orange",
    wrapText = TRUE
  )
  increasingStyle <- openxlsx::createStyle(
    fontColour = "red3",
    textDecoration = "bold"
  )
  decreasingStyle <- openxlsx::createStyle(
    fontColour = "forestgreen",
    textDecoration = "bold"
  )
  missingDataStyle <- openxlsx::createStyle(bgFill = "grey")
  #comments
  delayComment <- openxlsx::createComment(
    "Yellow: > 2 hours. Red: > 4 hours.",
    author = "Ghislain",
    visible = FALSE
  )
  percHistComment <- openxlsx::createComment(
    "0 = historic min, 100 = historic max. Yellow = >75%, red: >100%.",
    author = "Ghislain",
    visible = FALSE
  )
  percMeanComment <- openxlsx::createComment(
    "Current level / hist. mean (excl. current yr). 100 = historic mean. Yellow: >125%, Red: >150%.",
    author = "Ghislain",
    visible = FALSE
  )
  percMeanAdjComment <- openxlsx::createComment(
    "Adjusted to historic min due to arbitrary 0 point. 100 = historic mean, 0 = historic min. Yellow: >150%, Red: >200%.",
    author = "Ghislain",
    visible = FALSE
  )

  # Create the first tab with general internal + external comments
  openxlsx::addWorksheet(wb, "comments")
  openxlsx::writeData(
    wb,
    "comments",
    head,
    startCol = 1,
    startRow = 1,
    colNames = FALSE
  )
  openxlsx::mergeCells(wb, "comments", cols = c(1:2), rows = 1)
  openxlsx::mergeCells(wb, "comments", cols = c(3:4), rows = 1)
  openxlsx::mergeCells(wb, "comments", cols = c(5:6), rows = 1)
  openxlsx::mergeCells(wb, "comments", cols = c(7:9), rows = 1)
  openxlsx::addStyle(
    wb,
    "comments",
    style = fodNameStyle,
    rows = 1,
    cols = c(5:6)
  )
  openxlsx::writeData(
    wb,
    "comments",
    NA,
    startCol = 1,
    startRow = 2,
    colNames = FALSE
  ) # Empty row

  openxlsx::writeData(
    wb,
    "comments",
    "Yesterday's Public Current Conditions",
    startCol = 1,
    startRow = 3,
    colNames = FALSE
  )
  openxlsx::writeData(
    wb,
    "comments",
    yesterday[["yesterday_public_comments"]][1, 1],
    startCol = 2,
    startRow = 3,
    colNames = FALSE
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = yesterdayGeneralCommentStyle2,
    cols = 1,
    rows = 3
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = yesterdayGeneralCommentStyle,
    cols = c(2:7),
    rows = 3,
    gridExpand = TRUE
  )
  openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 3)
  openxlsx::writeData(
    wb,
    "comments",
    "Yesterday's Public Forecast Conditions",
    startCol = 1,
    startRow = 4,
    colNames = FALSE
  )
  openxlsx::writeData(
    wb,
    "comments",
    yesterday[["yesterday_public_comments"]][2, 1],
    startCol = 2,
    startRow = 4,
    colNames = FALSE
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = yesterdayGeneralCommentStyle2,
    cols = 1,
    rows = 4
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = yesterdayGeneralCommentStyle,
    cols = c(2:7),
    rows = 4,
    gridExpand = TRUE
  )
  openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 4)
  openxlsx::writeData(
    wb,
    "comments",
    NA,
    startCol = 1,
    startRow = 5,
    colNames = FALSE
  )

  openxlsx::writeData(
    wb,
    "comments",
    "Levels comment",
    startCol = 1,
    startRow = 6,
    colNames = FALSE
  )
  openxlsx::writeFormula(
    wb,
    "comments",
    "=levels!B3",
    startCol = 2,
    startRow = 6
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = generalCommentStyle2,
    cols = 1,
    rows = 6
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = generalCommentStyle,
    cols = c(2:7),
    rows = 6,
    gridExpand = TRUE
  )
  openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 6)
  openxlsx::writeData(
    wb,
    "comments",
    "Flows comment",
    startCol = 1,
    startRow = 7,
    colNames = FALSE
  )
  openxlsx::writeFormula(
    wb,
    "comments",
    "=flows!B3",
    startCol = 2,
    startRow = 7
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = generalCommentStyle2,
    cols = 1,
    rows = 7
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = generalCommentStyle,
    cols = c(2:7),
    rows = 7,
    gridExpand = TRUE
  )
  openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 7)
  openxlsx::writeData(
    wb,
    "comments",
    "Snow comment",
    startCol = 1,
    startRow = 8,
    colNames = FALSE
  )
  openxlsx::writeFormula(wb, "comments", "=snow!B3", startCol = 2, startRow = 8)
  openxlsx::addStyle(
    wb,
    "comments",
    style = generalCommentStyle2,
    cols = 1,
    rows = 8
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = generalCommentStyle,
    cols = c(2:7),
    rows = 8,
    gridExpand = TRUE
  )
  openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 8)
  openxlsx::writeData(
    wb,
    "comments",
    "Bridges comment",
    startCol = 1,
    startRow = 9,
    colNames = FALSE
  )
  openxlsx::writeFormula(
    wb,
    "comments",
    "=bridges!B3",
    startCol = 2,
    startRow = 9
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = generalCommentStyle2,
    cols = 1,
    rows = 9
  )
  openxlsx::addStyle(
    wb,
    "comments",
    style = generalCommentStyle,
    cols = c(2:7),
    rows = 9,
    gridExpand = TRUE
  )
  openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 9)
  if ("precipitation" %in% names(tables)) {
    log_info("[precip] Writing precipitation comment rows in comments sheet.")
    openxlsx::writeData(
      wb,
      "comments",
      "Precipitation comment",
      startCol = 1,
      startRow = 10,
      colNames = FALSE
    )
    openxlsx::writeFormula(
      wb,
      "comments",
      "=precipitation!B3",
      startCol = 2,
      startRow = 10
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = generalCommentStyle2,
      cols = 1,
      rows = 10
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = generalCommentStyle,
      cols = c(2:7),
      rows = 10,
      gridExpand = TRUE
    )
    openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 10)
    openxlsx::writeData(
      wb,
      "comments",
      NA,
      startCol = 1,
      startRow = 11,
      colNames = FALSE
    )

    openxlsx::writeData(
      wb,
      "comments",
      "Public Current Conditions",
      startCol = 1,
      startRow = 12,
      colNames = FALSE
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = publicCommentStyle2,
      cols = 1,
      rows = 12
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = publicCommentStyle,
      cols = c(2:7),
      rows = 12,
      gridExpand = TRUE
    )
    openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 12)
    openxlsx::writeData(
      wb,
      "comments",
      "Public Forecast Conditions",
      startCol = 1,
      startRow = 13,
      colNames = FALSE
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = publicCommentStyle2,
      cols = 1,
      rows = 13
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = publicCommentStyle,
      cols = c(2:7),
      rows = 13,
      gridExpand = TRUE
    )
    openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 13)
    openxlsx::setRowHeights(
      wb,
      "comments",
      rows = c(1:13),
      heights = c(15, 15, 55, 55, 15, 35, 35, 35, 35, 35, 15, 55, 55)
    )
  } else {
    openxlsx::writeData(
      wb,
      "comments",
      "Public Current Conditions",
      startCol = 1,
      startRow = 11,
      colNames = FALSE
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = publicCommentStyle2,
      cols = 1,
      rows = 11
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = publicCommentStyle,
      cols = c(2:7),
      rows = 11,
      gridExpand = TRUE
    )
    openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 11)
    openxlsx::writeData(
      wb,
      "comments",
      "Public Forecast Conditions",
      startCol = 1,
      startRow = 12,
      colNames = FALSE
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = publicCommentStyle2,
      cols = 1,
      rows = 12
    )
    openxlsx::addStyle(
      wb,
      "comments",
      style = publicCommentStyle,
      cols = c(2:7),
      rows = 12,
      gridExpand = TRUE
    )
    openxlsx::mergeCells(wb, "comments", cols = c(2:7), rows = 12)
    openxlsx::setRowHeights(
      wb,
      "comments",
      rows = c(1:12),
      heights = c(15, 15, 55, 55, 15, 35, 35, 35, 35, 15, 55, 55)
    )
  }

  openxlsx::setColWidths(
    wb,
    "comments",
    cols = c(1:7),
    widths = c(15, 25, 14, 14, 14, 14, 100)
  )

  for (i in names(tables)[
    !(names(tables) %in% c("precipitation", "temperature"))
  ]) {
    openxlsx::addWorksheet(wb, i)
    #Create/format the header
    openxlsx::writeData(
      wb,
      i,
      head,
      startCol = 1,
      startRow = 1,
      colNames = FALSE
    )
    openxlsx::writeData(wb, i, NA, startCol = 1, startRow = 2, colNames = FALSE)

    openxlsx::mergeCells(wb, i, cols = c(1:2), rows = 1)
    openxlsx::mergeCells(wb, i, cols = c(3:4), rows = 1)
    openxlsx::mergeCells(wb, i, cols = c(5:6), rows = 1)
    openxlsx::mergeCells(wb, i, cols = c(7:9), rows = 1)
    openxlsx::addStyle(wb, i, style = fodNameStyle, rows = 1, cols = c(5:6))
    #add a line for general comments
    openxlsx::writeData(
      wb,
      i,
      "General comment",
      startCol = 1,
      startRow = 3,
      colNames = FALSE
    )
    openxlsx::writeData(
      wb,
      i,
      "Yesterday's comment",
      startCol = 1,
      startRow = 4,
      colNames = FALSE
    )
    #add yesterday's comments
    openxlsx::writeData(
      wb,
      i,
      yesterday[["yesterday_general"]][[i]],
      startCol = 2,
      startRow = 4,
      colNames = FALSE
    )
    openxlsx::addStyle(wb, i, style = generalCommentStyle2, cols = 1, rows = 3)
    openxlsx::addStyle(
      wb,
      i,
      style = yesterdayGeneralCommentStyle2,
      cols = 1,
      rows = 4
    )
    openxlsx::mergeCells(
      wb,
      i,
      cols = if (past == 7) {
        c(2:12)
      } else if (past == 14) {
        c(2:13)
      } else if (past == 21) {
        c(2:14)
      } else if (past == 28) {
        c(2:15)
      },
      rows = 3
    )
    openxlsx::mergeCells(
      wb,
      i,
      cols = if (past == 7) {
        c(2:12)
      } else if (past == 14) {
        c(2:13)
      } else if (past == 21) {
        c(2:14)
      } else if (past == 28) {
        c(2:15)
      },
      rows = 4
    )
    openxlsx::addStyle(
      wb,
      i,
      style = generalCommentStyle,
      cols = if (past == 7) {
        c(2:12)
      } else if (past == 14) {
        c(2:13)
      } else if (past == 21) {
        c(2:14)
      } else if (past == 28) {
        c(2:15)
      },
      rows = 3,
      gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb,
      i,
      style = yesterdayGeneralCommentStyle,
      cols = if (past == 7) {
        c(2:12)
      } else if (past == 14) {
        c(2:13)
      } else if (past == 21) {
        c(2:14)
      } else if (past == 28) {
        c(2:15)
      },
      rows = 4,
      gridExpand = TRUE
    )
    openxlsx::writeData(wb, i, NA, startCol = 1, startRow = 5, colNames = FALSE) #empty row before the data
    #add content
    openxlsx::writeData(wb, i, tables[[i]], startRow = 6)
    #format for ease of viewing
    openxlsx::freezePane(wb, sheet = i, firstActiveRow = 7, firstActiveCol = 3)
    openxlsx::setColWidths(
      wb,
      i,
      cols = if (past == 7) {
        c(1:13)
      } else if (past == 14) {
        c(1:14)
      } else if (past == 21) {
        c(1:15)
      } else if (past == 28) {
        c(1:16)
      },
      widths = if (past == 7) {
        c(10, 30, 10, 10, 10, 12, 12, 12, 12, 15, 4, 60, 60)
      } else if (past == 14) {
        c(10, 30, 10, 10, 10, 12, 12, 12, 12, 12, 15, 4, 60, 60)
      } else if (past == 21) {
        c(10, 30, 10, 10, 10, 12, 12, 12, 12, 12, 12, 15, 4, 60, 60)
      } else if (past == 28) {
        c(10, 30, 10, 10, 10, 12, 12, 12, 12, 12, 12, 12, 15, 4, 60, 60)
      }
    )
    openxlsx::addStyle(
      wb,
      i,
      headStyle,
      rows = 6,
      cols = if (past == 7) {
        c(1:13)
      } else if (past == 14) {
        c(1:14)
      } else if (past == 21) {
        c(1:15)
      } else if (past == 28) {
        c(1:16)
      }
    )
    openxlsx::addStyle(
      wb,
      i,
      fodCommentStyle,
      rows = 1:nrow(tables[[i]]) + 6,
      cols = if (past == 7) {
        12
      } else if (past == 14) {
        13
      } else if (past == 21) {
        14
      } else if (past == 28) {
        15
      }
    )
    openxlsx::addStyle(
      wb,
      i,
      yesterdayFodCommentStyle,
      rows = 1:nrow(tables[[i]]) + 6,
      cols = if (past == 7) {
        13
      } else if (past == 14) {
        14
      } else if (past == 21) {
        15
      } else if (past == 28) {
        16
      }
    )
    #Add comments
    openxlsx::writeComment(
      wb,
      sheet = i,
      col = 4,
      row = 6,
      comment = percHistComment
    )
    openxlsx::writeComment(
      wb,
      sheet = i,
      col = 5,
      row = 6,
      comment = if (i == "levels") percMeanAdjComment else percMeanComment
    )
    openxlsx::writeComment(
      wb,
      sheet = i,
      col = if (past == 7) {
        11
      } else if (past == 14) {
        12
      } else if (past == 21) {
        13
      } else if (past == 28) {
        14
      },
      row = 6,
      comment = delayComment
    )
    #Conditional format
    openxlsx::conditionalFormatting(
      wb,
      sheet = i,
      rule = ">75",
      cols = 4,
      rows = 1:nrow(tables[[i]]) + 6,
      style = colStyleYellow
    )
    openxlsx::conditionalFormatting(
      wb,
      sheet = i,
      rule = ">100",
      cols = 4,
      rows = 1:nrow(tables[[i]]) + 6,
      style = colStyleRed
    )
    openxlsx::conditionalFormatting(
      wb,
      sheet = i,
      rule = if (i == "levels") ">150" else ">125",
      cols = 5,
      rows = 1:nrow(tables[[i]]) + 6,
      style = colStyleYellow
    )
    openxlsx::conditionalFormatting(
      wb,
      sheet = i,
      rule = if (i == "levels") ">200" else ">150",
      cols = 5,
      rows = 1:nrow(tables[[i]]) + 6,
      style = colStyleRed
    )
    #conditional format for age of last data
    openxlsx::conditionalFormatting(
      wb,
      sheet = i,
      rule = ">2",
      cols = if (past == 7) {
        11
      } else if (past == 14) {
        12
      } else if (past == 21) {
        13
      } else if (past == 28) {
        14
      },
      rows = 1:nrow(tables[[i]]) + 6,
      style = colStyleYellow
    )
    openxlsx::conditionalFormatting(
      wb,
      sheet = i,
      rule = ">4",
      cols = if (past == 7) {
        11
      } else if (past == 14) {
        12
      } else if (past == 21) {
        13
      } else if (past == 28) {
        14
      },
      rows = 1:nrow(tables[[i]]) + 6,
      style = colStyleRed
    )
    #Conditional format for increasing/decreasing (!bridge radars are inverse)
    openxlsx::conditionalFormatting(
      wb,
      sheet = i,
      rule = if (i == "bridges") "<0" else ">0",
      cols = if (past == 7) {
        c(6:9)
      } else if (past == 14) {
        c(6:10)
      } else if (past == 21) {
        c(6:11)
      } else if (past == 28) {
        c(6:12)
      },
      rows = 1:nrow(tables[[i]]) + 6,
      style = increasingStyle
    )
    openxlsx::conditionalFormatting(
      wb,
      sheet = i,
      rule = if (i == "bridges") ">0" else "<0",
      cols = if (past == 7) {
        c(6:9)
      } else if (past == 14) {
        c(6:10)
      } else if (past == 21) {
        c(6:11)
      } else if (past == 28) {
        c(6:12)
      },
      rows = 1:nrow(tables[[i]]) + 6,
      style = decreasingStyle
    )
    openxlsx::conditionalFormatting(
      wb,
      sheet = i,
      rule = '=""',
      cols = if (past == 7) {
        c(3, 6:9)
      } else if (past == 14) {
        c(3, 6:10)
      } else if (past == 21) {
        c(3, 6:11)
      } else if (past == 28) {
        c(3, 6:12)
      },
      rows = 1:nrow(tables[[i]]) + 6,
      style = missingDataStyle
    )
  }

  if ("temperature" %in% names(tables)) {
    openxlsx::addWorksheet(wb, "temperature")
    #Create/format the header
    openxlsx::writeData(
      wb,
      "temperature",
      head,
      startCol = 1,
      startRow = 1,
      colNames = FALSE
    )
    openxlsx::writeData(
      wb,
      "temperature",
      NA,
      startCol = 1,
      startRow = 2,
      colNames = FALSE
    )
    openxlsx::mergeCells(wb, "temperature", cols = c(1:2), rows = 1)
    openxlsx::mergeCells(wb, "temperature", cols = c(3:4), rows = 1)
    openxlsx::mergeCells(wb, "temperature", cols = c(5:6), rows = 1)
    openxlsx::mergeCells(wb, "temperature", cols = c(7:11), rows = 1)
    openxlsx::addStyle(
      wb,
      "temperature",
      style = fodNameStyle,
      rows = 1,
      cols = c(5:6)
    )
    #add a line for general and yesterday comments
    openxlsx::writeData(
      wb,
      "temperature",
      "General comment",
      startCol = 1,
      startRow = 3,
      colNames = FALSE
    )
    openxlsx::writeData(
      wb,
      "temperature",
      "Yesterday's comment",
      startCol = 1,
      startRow = 4,
      colNames = FALSE
    )
    #add yesterday's comments
    openxlsx::writeData(
      wb,
      "temperature",
      yesterday[["yesterday_general"]][["temperature"]],
      startCol = 2,
      startRow = 4,
      colNames = FALSE
    )
    openxlsx::addStyle(
      wb,
      "temperature",
      style = generalCommentStyle2,
      cols = 1,
      rows = 3
    )
    openxlsx::addStyle(
      wb,
      "temperature",
      style = yesterdayGeneralCommentStyle2,
      cols = 1,
      rows = 4
    )
    openxlsx::mergeCells(wb, "temperature", cols = c(2:11), rows = 3)
    openxlsx::mergeCells(wb, "temperature", cols = c(2:11), rows = 4)
    openxlsx::addStyle(
      wb,
      "temperature",
      style = generalCommentStyle,
      cols = c(2:11),
      rows = 3,
      gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb,
      "temperature",
      style = yesterdayGeneralCommentStyle,
      cols = c(2:11),
      rows = 4,
      gridExpand = TRUE
    )
    openxlsx::writeData(
      wb,
      "temperature",
      NA,
      startCol = 1,
      startRow = 5,
      colNames = FALSE
    )

    openxlsx::writeData(
      wb,
      "temperature",
      tables[["temperature"]],
      startRow = 6
    )

    openxlsx::freezePane(
      wb,
      sheet = "temperature",
      firstActiveRow = 7,
      firstActiveCol = 3
    )
    openxlsx::setColWidths(
      wb,
      "temperature",
      cols = c(1:14),
      widths = c(10, 30, 14, 14, 14, 12, 12, 12, 12, 12, 12, 12, 60, 60)
    )
    openxlsx::addStyle(wb, "temperature", headStyle, rows = 6, cols = c(1:14))
    openxlsx::addStyle(
      wb,
      "temperature",
      fodCommentStyle,
      rows = 1:nrow(tables[["temperature"]]) + 6,
      cols = 13
    )
    openxlsx::addStyle(
      wb,
      "temperature",
      yesterdayFodCommentStyle,
      rows = 1:nrow(tables[["temperature"]]) + 6,
      cols = 14
    )
  }

  if ("precipitation" %in% names(tables)) {
    log_info("[precip] Creating and formatting precipitation worksheet.")
    openxlsx::addWorksheet(wb, "precipitation")
    #Create/format the header
    openxlsx::writeData(
      wb,
      "precipitation",
      head,
      startCol = 1,
      startRow = 1,
      colNames = FALSE
    )
    openxlsx::writeData(
      wb,
      "precipitation",
      NA,
      startCol = 1,
      startRow = 2,
      colNames = FALSE
    )
    openxlsx::mergeCells(wb, "precipitation", cols = c(1:2), rows = 1)
    openxlsx::mergeCells(wb, "precipitation", cols = c(3:4), rows = 1)
    openxlsx::mergeCells(wb, "precipitation", cols = c(5:6), rows = 1)
    openxlsx::mergeCells(wb, "precipitation", cols = c(7:9), rows = 1)
    openxlsx::addStyle(
      wb,
      "precipitation",
      style = fodNameStyle,
      rows = 1,
      cols = c(5:6)
    )
    #add a line for general and yesterday comments
    openxlsx::writeData(
      wb,
      "precipitation",
      "General comment",
      startCol = 1,
      startRow = 3,
      colNames = FALSE
    )
    openxlsx::writeData(
      wb,
      "precipitation",
      "Yesterday's comment",
      startCol = 1,
      startRow = 4,
      colNames = FALSE
    )
    #add yesterday's comments
    openxlsx::writeData(
      wb,
      "precipitation",
      yesterday[["yesterday_general"]][["precipitation"]],
      startCol = 2,
      startRow = 4,
      colNames = FALSE
    )
    openxlsx::addStyle(
      wb,
      "precipitation",
      style = generalCommentStyle2,
      cols = 1,
      rows = 3
    )
    openxlsx::addStyle(
      wb,
      "precipitation",
      style = yesterdayGeneralCommentStyle2,
      cols = 1,
      rows = 4
    )
    openxlsx::mergeCells(wb, "precipitation", cols = c(2:9), rows = 3)
    openxlsx::mergeCells(wb, "precipitation", cols = c(2:9), rows = 4)
    openxlsx::addStyle(
      wb,
      "precipitation",
      style = generalCommentStyle,
      cols = c(2:9),
      rows = 3,
      gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb,
      "precipitation",
      style = yesterdayGeneralCommentStyle,
      cols = c(2:9),
      rows = 4,
      gridExpand = TRUE
    )
    openxlsx::writeData(
      wb,
      "precipitation",
      NA,
      startCol = 1,
      startRow = 5,
      colNames = FALSE
    ) #empty row before comment
    openxlsx::writeData(
      wb,
      "precipitation",
      "Mean precip estimates upstream of locations are derived from HRDPA (reanalysis) and HRDPS (forecast) products. Beware: combination of liquid + solid precip.",
      startCol = 1,
      startRow = 6,
      colNames = FALSE
    )
    openxlsx::mergeCells(wb, "precipitation", cols = c(1:9), rows = 6)
    openxlsx::writeData(
      wb,
      "precipitation",
      NA,
      startCol = 1,
      startRow = 7,
      colNames = FALSE
    ) #empty row after comment)
    #add content
    openxlsx::writeData(
      wb,
      "precipitation",
      tables[["precipitation"]],
      startRow = 8
    )
    #format for ease of viewing
    openxlsx::freezePane(
      wb,
      sheet = "precipitation",
      firstActiveRow = 9,
      firstActiveCol = 3
    )
    openxlsx::setColWidths(
      wb,
      "precipitation",
      cols = c(1:10),
      widths = c(10, 30, 14, 14, 14, 14, 14, 14, 60, 60)
    )
    openxlsx::addStyle(wb, "precipitation", headStyle, rows = 8, cols = c(1:10))
    openxlsx::addStyle(
      wb,
      "precipitation",
      fodCommentStyle,
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      cols = 9
    )
    openxlsx::addStyle(
      wb,
      "precipitation",
      yesterdayFodCommentStyle,
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      cols = 10
    )
    #Conditional format
    precipYellowStyle <- openxlsx::createStyle(
      fontColour = "black",
      textDecoration = "bold",
      border = "TopBottomLeftRight",
      borderColour = "goldenrod1",
      borderStyle = "thick"
    )
    precipRedStyle <- openxlsx::createStyle(
      fontColour = "black",
      textDecoration = "bold",
      border = "TopBottomLeftRight",
      borderColour = "red2",
      borderStyle = "thick"
    )
    openxlsx::conditionalFormatting(
      wb,
      "precipitation",
      rule = ">10",
      cols = c(6, 7),
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      style = precipYellowStyle
    ) #24 hrs precip
    openxlsx::conditionalFormatting(
      wb,
      "precipitation",
      rule = ">20",
      cols = c(6, 7),
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      style = precipRedStyle
    ) #24 hrs precip
    openxlsx::conditionalFormatting(
      wb,
      "precipitation",
      rule = ">15",
      cols = c(5, 8),
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      style = precipYellowStyle
    ) #48 hrs precip
    openxlsx::conditionalFormatting(
      wb,
      "precipitation",
      rule = ">25",
      cols = c(5, 8),
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      style = precipRedStyle
    ) #48 hrs precip
    openxlsx::conditionalFormatting(
      wb,
      "precipitation",
      rule = ">20",
      cols = 4,
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      style = precipYellowStyle
    ) #past 3 day precip
    openxlsx::conditionalFormatting(
      wb,
      "precipitation",
      rule = ">30",
      cols = 4,
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      style = precipRedStyle
    ) #past 3 day precip
    openxlsx::conditionalFormatting(
      wb,
      "precipitation",
      rule = ">40",
      cols = 3,
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      style = precipYellowStyle
    ) #past week precip
    openxlsx::conditionalFormatting(
      wb,
      "precipitation",
      rule = ">60",
      cols = 3,
      rows = 1:nrow(tables[["precipitation"]]) + 8,
      style = precipRedStyle
    ) #past week precip
    #Add comments
    dayComment <- openxlsx::createComment(
      "Yellow: > 10mm, Red: > 20mm",
      author = "Ghislain",
      visible = FALSE
    )
    twoDayComment <- openxlsx::createComment(
      "Yellow: > 15mm, Red: > 25mm",
      author = "Ghislain",
      visible = FALSE
    )
    threeDayComment <- openxlsx::createComment(
      "Yellow: > 20mm, Red: > 30mm",
      author = "Ghislain",
      visible = FALSE
    )
    weekComment <- openxlsx::createComment(
      "Yellow: > 40mm, Red: > 60mm",
      author = "Ghislain",
      visible = FALSE
    )
    openxlsx::writeComment(
      wb,
      sheet = "precipitation",
      col = 6,
      row = 8,
      comment = dayComment
    )
    openxlsx::writeComment(
      wb,
      sheet = "precipitation",
      col = 7,
      row = 8,
      comment = dayComment
    )
    openxlsx::writeComment(
      wb,
      sheet = "precipitation",
      col = 5,
      row = 8,
      comment = twoDayComment
    )
    openxlsx::writeComment(
      wb,
      sheet = "precipitation",
      col = 8,
      row = 8,
      comment = twoDayComment
    )
    openxlsx::writeComment(
      wb,
      sheet = "precipitation",
      col = 4,
      row = 8,
      comment = threeDayComment
    )
    openxlsx::writeComment(
      wb,
      sheet = "precipitation",
      col = 3,
      row = 8,
      comment = weekComment
    )
    log_info("[precip] Completed precipitation worksheet formatting.")
  }

  save_path <- paste0(save_path, "/HydrometricReport_", report_day, ".xlsx")
  log_info(paste0("Saving workbook to ", save_path, "."))
  # Save the workbook ----------------------------
  openxlsx::saveWorkbook(wb, save_path, overwrite = TRUE)
  log_info("Workbook save completed successfully.")

  message("Tabular report created and saved at ", save_path, "\n")
  return(save_path)
}


#' Extract and Upload Comments
#'
#' Extracts comments from Excel workbook sheets and uploads them to the database.
#'
#' @param workbook_path Character. Path to the Excel workbook file.
#' @param report_date Date or POSIXct. The report date to associate with comments.
#' @param con DBI connection. Database connection object.
#' @param comment_category_lookup Data frame. Lookup table with columns \code{id} and \code{category_key}.
#' @param document_type_id Integer. ID of the document type to associate with comments.
#' @param author_lookup Data frame. Lookup table for resolving author information.
#' @param existing_keys Character vector. Keys of comments already in the database.
#' @param remaining_upload_slots Integer. Maximum number of comments left to upload.
#' @param chunk_size Integer. Number of records to upload per batch. Default is 500L.
#'
#' @return A list containing:
#'   \item{existing_keys}{Updated character vector of existing comment keys.}
#'   \item{remaining_upload_slots}{Updated count of remaining upload slots.}
#'
#' @details
#' This function iterates through all sheets in a workbook, extracts comment text from
#' specific cells based on sheet type, and uploads new comments to the database.
#' Comments are identified by their content hash to avoid duplicates.
#'
#' @export
extract_and_upload_comments <- function(
  workbook_path,
  report_date,
  con,
  comment_category_lookup,
  document_type_id,
  author_lookup,
  existing_keys,
  remaining_upload_slots,
  chunk_size = 500L
) {
  workbook <- openxlsx::loadWorkbook(workbook_path)

  comments <- data.frame(
    timestamp = as.POSIXct(character()),
    raw_author = character(),
    author = character(),
    comment = character(),
    category = character(),
    stringsAsFactors = FALSE
  )

  for (sheet_name in names(workbook)) {
    param_name <- standardize_param_name(sheet_name)

    author_cell_value <- openxlsx::read.xlsx(
      workbook,
      sheet = sheet_name,
      rows = 1,
      cols = 5,
      colNames = FALSE
    )
    author_details <- resolve_author_details(
      author_cell_value,
      author_lookup
    )

    if (all(is.na(author_details$author)) && "comments" %in% names(workbook)) {
      author_cell_value <- openxlsx::read.xlsx(
        workbook,
        sheet = "comments",
        rows = 1,
        cols = 5,
        colNames = FALSE
      )
      author_details <- resolve_author_details(
        author_cell_value,
        author_lookup
      )
    }

    if (length(author_details$author) == 0) {
      author_details$author <- NA_character_
    }

    if (param_name == "comments") {
      comment_text <- as.character(openxlsx::read.xlsx(
        workbook,
        sheet = sheet_name,
        rows = 12,
        cols = 2,
        colNames = FALSE
      ))
      if (length(comment_text) > 0 && !all(is.na(comment_text))) {
        comments <- rbind(
          comments,
          data.frame(
            timestamp = report_date,
            raw_author = author_details$raw_author,
            author = author_details$author,
            comment = comment_text,
            category = "current conditions",
            stringsAsFactors = FALSE
          )
        )
      }

      comment_text <- as.character(openxlsx::read.xlsx(
        workbook,
        sheet = sheet_name,
        rows = 3,
        cols = 13,
        colNames = FALSE
      ))
      if (length(comment_text) > 0 && !all(is.na(comment_text))) {
        comments <- rbind(
          comments,
          data.frame(
            timestamp = report_date,
            raw_author = author_details$raw_author,
            author = author_details$author,
            comment = comment_text,
            category = "forecast conditions",
            stringsAsFactors = FALSE
          )
        )
      }
    } else {
      comment_text <- as.character(openxlsx::read.xlsx(
        workbook,
        sheet = sheet_name,
        rows = 3,
        cols = 2,
        colNames = FALSE
      ))

      if (length(comment_text) == 0) {
        comment_text <- NA_character_
      }

      if (length(comment_text) > 0 && !all(is.na(comment_text))) {
        comments <- rbind(
          comments,
          data.frame(
            timestamp = report_date,
            raw_author = author_details$raw_author,
            author = author_details$author,
            comment = comment_text,
            category = param_name,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  if (nrow(comments) == 0) {
    return(list(
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots
    ))
  }

  comments <- unique(comments)
  comments$timestamp <- as.POSIXct(
    comments$timestamp,
    format = "%Y-%m-%d",
    tz = "UTC"
  )

  comments$category_key <- tolower(comments$category)
  comments$category_key[
    comments$category_key == "forecast conditions"
  ] <- "future conditions"
  comments$category_key[
    comments$category_key == "current conditions"
  ] <- "current conditions"
  comments$comment_category_id <- comment_category_lookup$id[match(
    comments$category_key,
    comment_category_lookup$category_key
  )]

  missing_categories <- unique(comments$category[is.na(
    comments$comment_category_id
  )])
  if (length(missing_categories) > 0) {
    stop(
      paste(
        "Unable to map comment categories in",
        basename(workbook_path),
        ":",
        paste(missing_categories, collapse = ", ")
      )
    )
  }

  author_id_matrix <- t(vapply(
    comments$author,
    map_author_ids,
    FUN.VALUE = rep(NA_integer_, 4),
    author_lookup = author_lookup,
    max_authors = 4L
  ))
  colnames(author_id_matrix) <- c(
    "author_id",
    "second_author_id",
    "third_author_id",
    "fourth_author_id"
  )

  # Prevent named vectors from being interpreted as row names in data.frame
  comments <- as.data.frame(
    lapply(comments, function(x) {
      names(x) <- NULL
      x
    }),
    stringsAsFactors = FALSE
  )

  comments_to_upload <- data.frame(
    text_en = comments$comment,
    text_fr = NA_character_,
    link = NA_character_,
    document_type_id = document_type_id,
    location_id = NA_integer_,
    comment_category_id = comments$comment_category_id,
    public = FALSE,
    raw_author = comments$raw_author,
    author_id = author_id_matrix[, "author_id"],
    second_author_id = author_id_matrix[, "second_author_id"],
    third_author_id = author_id_matrix[, "third_author_id"],
    fourth_author_id = author_id_matrix[, "fourth_author_id"],
    timestamp = comments$timestamp,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  comments_to_upload <- comments_to_upload[
    !is.na(comments_to_upload$text_en) & nzchar(comments_to_upload$text_en),
  ]
  comments_to_upload <- comments_to_upload[!duplicated(comments_to_upload), ]

  if (nrow(comments_to_upload) == 0) {
    return(list(
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots
    ))
  }

  comment_keys <- build_comment_key(comments_to_upload)
  comments_to_upload <- comments_to_upload[!comment_keys %in% existing_keys, ]

  if (nrow(comments_to_upload) == 0) {
    return(list(
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots
    ))
  }

  comments_to_upload <- utils::head(
    comments_to_upload,
    remaining_upload_slots
  )

  if (nrow(comments_to_upload) == 0) {
    return(list(
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots
    ))
  }

  upload_batches <- split(
    seq_len(nrow(comments_to_upload)),
    ceiling(seq_len(nrow(comments_to_upload)) / chunk_size)
  )

  for (batch_index in seq_along(upload_batches)) {
    DBI::dbWriteTable(
      con,
      DBI::Id(schema = "commentary", table = "comments"),
      comments_to_upload[upload_batches[[batch_index]], ],
      append = TRUE,
      row.names = FALSE
    )
  }

  list(
    existing_keys = c(existing_keys, build_comment_key(comments_to_upload)),
    remaining_upload_slots = remaining_upload_slots -
      nrow(comments_to_upload)
  )
}


#' Standardize workbook sheet category name
#'
#' @param name Character sheet name from archived workbook.
#'
#' @return Character parameter category key used by the comments workflow.
#' @noRd
standardize_param_name <- function(name) {
  # replace spaces with underscores and convert to lowercase
  if (name %in% c("bridges", "bridge")) {
    ret <- "bridges"
  } else {
    ret <- name
  }
  return(ret)
}

#' Sanitize author names extracted from workbook cells
#'
#' @param author_name Character vector or scalar author text.
#'
#' @return Character scalar normalized author string, or NA.
#' @noRd
sanitize_author_name <- function(author_name) {
  if (is.null(author_name) || length(author_name) == 0) {
    return(NA_character_)
  }

  author_name <- as.character(author_name)
  author_name <- author_name[!is.na(author_name)]
  author_name <- trimws(author_name)
  author_name <- author_name[nzchar(author_name)]

  if (!length(author_name)) {
    return(NA_character_)
  }

  parts <- split_author_names(author_name)

  if (!length(parts)) {
    return(NA_character_)
  }

  paste(parts, collapse = " and ")
}

#' Extract raw author text
#'
#' @param author_name Character vector or scalar author text.
#'
#' @return Character scalar raw author value, or NA.
#' @noRd
extract_raw_author_name <- function(author_name) {
  if (is.null(author_name) || length(author_name) == 0) {
    return(NA_character_)
  }

  raw_values <- as.character(author_name)
  raw_values <- raw_values[!is.na(raw_values)]

  if (!length(raw_values)) {
    return(NA_character_)
  }

  raw_values[[1]]
}

#' Split author text into individual names
#'
#' @param author_name Character vector or scalar author text.
#'
#' @return Character vector of unique parsed author names.
#' @noRd
split_author_names <- function(author_name) {
  if (
    is.null(author_name) ||
      length(author_name) == 0 ||
      all(is.na(author_name))
  ) {
    return(character(0))
  }

  raw_text <- paste(as.character(author_name), collapse = " ")
  raw_text <- gsub("[[:space:]]+", " ", raw_text)

  # Parse author lists using common delimiters: and, /, \\, &
  parts <- unlist(strsplit(
    raw_text,
    "(?i)\\s*(?:\\band\\b|/|\\\\|&)\\s*",
    perl = TRUE
  ))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  unique(parts)
}

#' Map author names to author IDs
#'
#' @param author_name Character scalar with one or multiple author names.
#' @param author_lookup Data frame containing columns author and author_id.
#' @param max_authors Integer maximum number of authors to map.
#'
#' @return Integer vector of length max_authors containing matched IDs or NA.
#' @noRd
map_author_ids <- function(author_name, author_lookup, max_authors = 4L) {
  ids <- rep(NA_integer_, max_authors)
  author_parts <- split_author_names(author_name)

  if (!length(author_parts)) {
    return(ids)
  }

  matched_ids <- author_lookup$author_id[match(
    author_parts,
    author_lookup$author
  )]
  matched_ids <- matched_ids[!is.na(matched_ids)]

  if (!length(matched_ids)) {
    return(ids)
  }

  keep_n <- min(length(matched_ids), max_authors)
  ids[seq_len(keep_n)] <- as.integer(matched_ids[seq_len(keep_n)])
  ids
}

#' Resolve parsed author details
#'
#' @param author_cell_value Character value read from workbook author cell.
#' @param author_lookup Data frame containing columns author and author_id.
#'
#' @return List with raw_author, author, and author_ids.
#' @noRd
resolve_author_details <- function(author_cell_value, author_lookup) {
  raw_author <- extract_raw_author_name(author_cell_value)
  sanitized_author <- sanitize_author_name(author_cell_value)
  author_ids <- map_author_ids(sanitized_author, author_lookup)

  list(
    raw_author = raw_author,
    author = sanitized_author,
    author_ids = author_ids
  )
}

key_columns <- c(
  "text_en",
  "document_type_id",
  "comment_category_id",
  "public",
  "author_id",
  "second_author_id",
  "third_author_id",
  "fourth_author_id",
  "timestamp"
)

#' Build deduplication key for comment rows
#'
#' @param data Data frame of comments upload rows.
#'
#' @return Character vector of row-level deduplication keys.
#' @noRd
build_comment_key <- function(data) {
  apply(data[, key_columns, drop = FALSE], 1, function(row) {
    paste(
      ifelse(is.na(row), "<NA>", as.character(row)),
      collapse = "||"
    )
  })
}

#' Extract and upload comments from one workbook
#'
#' @param workbook_path Character path to workbook.
#' @param report_date Character, Date, or POSIXct report date.
#' @param con DBI connection.
#' @param comment_category_lookup Data frame with id and category_key.
#' @param document_type_id Integer document type ID.
#' @param author_lookup Data frame with author lookup values.
#' @param existing_keys Character vector of existing dedupe keys.
#' @param remaining_upload_slots Integer or Inf upload budget.
#' @param chunk_size Integer batch size for dbWriteTable.
#'
#' @return List with updated existing_keys and remaining_upload_slots.
#' @noRd
extract_and_upload_comments <- function(
  workbook_path,
  report_date,
  con,
  comment_category_lookup,
  document_type_id,
  author_lookup,
  existing_keys,
  remaining_upload_slots,
  chunk_size = 500L
) {
  workbook <- openxlsx::loadWorkbook(workbook_path)

  comments <- data.frame(
    timestamp = as.POSIXct(character()),
    raw_author = character(),
    author = character(),
    comment = character(),
    category = character(),
    stringsAsFactors = FALSE
  )

  for (sheet_name in names(workbook)) {
    param_name <- standardize_param_name(sheet_name)

    author_cell_value <- openxlsx::read.xlsx(
      workbook,
      sheet = sheet_name,
      rows = 1,
      cols = 5,
      colNames = FALSE
    )
    author_details <- resolve_author_details(
      author_cell_value,
      author_lookup
    )

    if (all(is.na(author_details$author)) && "comments" %in% names(workbook)) {
      author_cell_value <- openxlsx::read.xlsx(
        workbook,
        sheet = "comments",
        rows = 1,
        cols = 5,
        colNames = FALSE
      )
      author_details <- resolve_author_details(
        author_cell_value,
        author_lookup
      )
    }

    if (length(author_details$author) == 0) {
      author_details$author <- NA_character_
    }

    if (param_name == "comments") {
      comment_text <- as.character(openxlsx::read.xlsx(
        workbook,
        sheet = sheet_name,
        rows = 12,
        cols = 2,
        colNames = FALSE
      ))
      if (length(comment_text) > 0 && !all(is.na(comment_text))) {
        comments <- rbind(
          comments,
          data.frame(
            timestamp = report_date,
            raw_author = author_details$raw_author,
            author = author_details$author,
            comment = comment_text,
            category = "current conditions",
            stringsAsFactors = FALSE
          )
        )
      }

      comment_text <- as.character(openxlsx::read.xlsx(
        workbook,
        sheet = sheet_name,
        rows = 3,
        cols = 13,
        colNames = FALSE
      ))
      if (length(comment_text) > 0 && !all(is.na(comment_text))) {
        comments <- rbind(
          comments,
          data.frame(
            timestamp = report_date,
            raw_author = author_details$raw_author,
            author = author_details$author,
            comment = comment_text,
            category = "forecast conditions",
            stringsAsFactors = FALSE
          )
        )
      }
    } else {
      comment_text <- as.character(openxlsx::read.xlsx(
        workbook,
        sheet = sheet_name,
        rows = 3,
        cols = 2,
        colNames = FALSE
      ))

      if (length(comment_text) == 0) {
        comment_text <- NA_character_
      }

      if (length(comment_text) > 0 && !all(is.na(comment_text))) {
        comments <- rbind(
          comments,
          data.frame(
            timestamp = report_date,
            raw_author = author_details$raw_author,
            author = author_details$author,
            comment = comment_text,
            category = param_name,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  if (nrow(comments) == 0) {
    return(list(
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots
    ))
  }

  comments <- unique(comments)
  comments$timestamp <- as.POSIXct(
    comments$timestamp,
    format = "%Y-%m-%d",
    tz = "UTC"
  )

  comments$category_key <- tolower(comments$category)
  comments$category_key[
    comments$category_key == "forecast conditions"
  ] <- "future conditions"
  comments$category_key[
    comments$category_key == "current conditions"
  ] <- "current conditions"
  comments$comment_category_id <- comment_category_lookup$id[match(
    comments$category_key,
    comment_category_lookup$category_key
  )]

  missing_categories <- unique(comments$category[is.na(
    comments$comment_category_id
  )])
  if (length(missing_categories) > 0) {
    stop(
      paste(
        "Unable to map comment categories in",
        basename(workbook_path),
        ":",
        paste(missing_categories, collapse = ", ")
      )
    )
  }

  author_id_matrix <- t(vapply(
    comments$author,
    map_author_ids,
    FUN.VALUE = rep(NA_integer_, 4),
    author_lookup = author_lookup,
    max_authors = 4L
  ))
  colnames(author_id_matrix) <- c(
    "author_id",
    "second_author_id",
    "third_author_id",
    "fourth_author_id"
  )

  comments <- as.data.frame(
    lapply(comments, function(x) {
      names(x) <- NULL
      x
    }),
    stringsAsFactors = FALSE
  )

  comments_to_upload <- data.frame(
    text_en = comments$comment,
    text_fr = NA_character_,
    link = NA_character_,
    document_type_id = document_type_id,
    location_id = NA_integer_,
    comment_category_id = comments$comment_category_id,
    public = FALSE,
    raw_author = comments$raw_author,
    author_id = author_id_matrix[, "author_id"],
    second_author_id = author_id_matrix[, "second_author_id"],
    third_author_id = author_id_matrix[, "third_author_id"],
    fourth_author_id = author_id_matrix[, "fourth_author_id"],
    timestamp = comments$timestamp,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  comments_to_upload <- comments_to_upload[
    !is.na(comments_to_upload$text_en) & nzchar(comments_to_upload$text_en),
  ]
  comments_to_upload <- comments_to_upload[!duplicated(comments_to_upload), ]

  if (nrow(comments_to_upload) == 0) {
    return(list(
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots
    ))
  }

  comment_keys <- build_comment_key(comments_to_upload)
  comments_to_upload <- comments_to_upload[!comment_keys %in% existing_keys, ]

  if (nrow(comments_to_upload) == 0) {
    return(list(
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots
    ))
  }

  if (is.finite(remaining_upload_slots)) {
    comments_to_upload <- utils::head(
      comments_to_upload,
      remaining_upload_slots
    )
  }

  if (nrow(comments_to_upload) == 0) {
    return(list(
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots
    ))
  }

  upload_batches <- split(
    seq_len(nrow(comments_to_upload)),
    ceiling(seq_len(nrow(comments_to_upload)) / chunk_size)
  )

  for (batch_index in seq_along(upload_batches)) {
    DBI::dbWriteTable(
      con,
      DBI::Id(schema = "commentary", table = "comments"),
      comments_to_upload[upload_batches[[batch_index]], ],
      append = TRUE,
      row.names = FALSE
    )
  }

  list(
    existing_keys = c(existing_keys, build_comment_key(comments_to_upload)),
    remaining_upload_slots = if (is.finite(remaining_upload_slots)) {
      remaining_upload_slots - nrow(comments_to_upload)
    } else {
      remaining_upload_slots
    }
  )
}


#' Scrape Comments From Archived Conditions
#'
#' Iterates through archived tabular hydrometric condition workbooks and uploads
#' comments using \\code{extract_and_upload_comments}.
#'
#' @param archive_dir Character. Root archive directory that contains one
#'   subfolder per report date.
#' @param con DBI connection. Database connection object.
#' @param comment_category_lookup Data frame. Lookup table with columns
#'   \\code{id} and \\code{category_key}.
#' @param document_type_id Integer. ID of the document type to associate with
#'   uploaded comments.
#' @param author_lookup Data frame. Lookup table for resolving author
#'   information.
#' @param existing_keys Character vector. Keys of comments already in the
#'   database.
#' @param remaining_upload_slots Integer. Maximum number of comments left to
#'   upload.
#' @param show_progress Logical. If \\code{TRUE}, displays a text progress bar.
#'
#' @return A list containing:
#'   \\item{existing_keys}{Updated character vector of existing comment keys.}
#'   \\item{remaining_upload_slots}{Updated count of remaining upload slots.}
#'   \\item{processed_workbooks}{Integer count of archive workbooks processed.}
#' @noRd
scrape_comments_from_archived_conditions <- function(
  archive_dir,
  con,
  comment_category_lookup,
  document_type_id,
  author_lookup,
  existing_keys,
  remaining_upload_slots,
  show_progress = TRUE
) {
  folders <- list.dirs(archive_dir, full.names = FALSE, recursive = FALSE)
  workbook_paths <- file.path(
    archive_dir,
    folders,
    paste0("HydrometricReport_", folders, ".xlsx")
  )
  valid_workbook_idx <- which(file.exists(workbook_paths))
  workbook_paths <- workbook_paths[valid_workbook_idx]
  folders <- folders[valid_workbook_idx]

  if (length(workbook_paths) == 0) {
    return(list(
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots,
      processed_workbooks = 0L
    ))
  }

  processed_workbooks <- 0L
  folder_progress <- NULL
  if (isTRUE(show_progress)) {
    folder_progress <- utils::txtProgressBar(
      min = 0,
      max = length(workbook_paths),
      style = 3,
      initial = 0
    )
  }

  on.exit(
    {
      if (!is.null(folder_progress)) {
        close(folder_progress)
      }
    },
    add = TRUE
  )

  for (workbook_index in seq_along(workbook_paths)) {
    if (remaining_upload_slots <= 0) {
      break
    }

    upload_result <- extract_and_upload_comments(
      workbook_path = workbook_paths[[workbook_index]],
      report_date = folders[[workbook_index]],
      con = con,
      comment_category_lookup = comment_category_lookup,
      document_type_id = document_type_id,
      author_lookup = author_lookup,
      existing_keys = existing_keys,
      remaining_upload_slots = remaining_upload_slots
    )

    existing_keys <- upload_result$existing_keys
    remaining_upload_slots <- upload_result$remaining_upload_slots
    processed_workbooks <- processed_workbooks + 1L

    if (!is.null(folder_progress)) {
      utils::setTxtProgressBar(folder_progress, workbook_index)
    }
  }

  list(
    existing_keys = existing_keys,
    remaining_upload_slots = remaining_upload_slots,
    processed_workbooks = processed_workbooks
  )
}
