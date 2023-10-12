#' Get timeseries data from the hydromet database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Extracts timeseries information from the WRB's hydrometric database. Refer to [DB_browse_ts()] if unsure of how to formulate your query.
#'
#' @param path The path to the database, passed to [hydrometConnect()]. Default uses hydrometConnect default path.
#' @param locations The location code/id(s) for the requested timeseries as a character vector of 1 or more. Refer to [DB_get_ts()] if unsure of location code.
#' @param parameter The parameter requested for the timeseries.  Refer to [DB_browse_ts()] if unsure of parameter spelling.
#' @param frequency One of "daily", "realtime", or "discrete".
#' @param start The start date or datetime of records requested (inclusive). Specify a Date or POSIXct object, or a character vector of form "2022-01-01" or "2022-01-01 10:10:10". Set before timeseries start to get all records up to the end date/time. Times are passed to the database with 0 UTC offset: apply the offset yourself if supplying a character vector, or remember that the timezone attribute of POSIXct or Date objects will be changed to UTC 0.
#' @param end The end date or datetime of records requested (inclusive). Format as per 'start'. Set after timeseries end to get all records after the start date/time
#' @param save_path Specify a path here if you want an Excel workbook saved to disk. "choose" lets you interactively choose your folder.
#'
#' @seealso [DB_browse_ts()] for browsing timeseries data from the database, or, for spatial data, [DB_browse_spatial()] and [DB_get_spatial()] to browse and extract spatial data. For location metadata use [DB_get_meta()].
#'
#' @return A list of data frames (one per location) containing the information requested and, optionally, an Excel workbook saved to disk with one tab per location.
#' @export
#'

DB_get_ts <- function(path = "default", locations, parameter, frequency, start = "1900-01-01", end = Sys.time(), save_path = NULL) {

  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the folder where you want this information saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
    if (!dir.exists(save_path)){
      stop("The save path you pointed me to does not exist.")
    }
  }

  if (length(parameter) != 1){
    stop("You can only request data for one parameter at a time.")
  }
  if (length(frequency) != 1){
    stop("You can only request data for one frequency type at a time.")
  }

  if (TRUE %in% (class(start) %in% c("POSIXct", "Date"))){
    start <- as.POSIXct(start)
    attr(start, "tzone") <- "UTC"
    start <- as.character(start)
  }

  if (TRUE %in% (class(end) %in% c("POSIXct", "Date"))){
    end <- as.POSIXct(end)
    attr(end, "tzone") <- "UTC"
    end <- as.character(end)
  }

  DB <- hydrometConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(DB), add=TRUE)

  ls <- list()
  for (i in locations){
    data <- DBI::dbGetQuery(DB, paste0("SELECT * FROM '", frequency, "' WHERE location = '", i, "' AND parameter = '", parameter, "' AND ", if(frequency == "daily") "date" else if (frequency == "realtime") "datetime_UTC" else if (frequency == "discrete") "sample_date", " BETWEEN '", start, "' AND '", end, "'"))
    if (nrow(data) > 0){
      ls[[i]] <- data
    }
  }

  if (length(ls) > 0){
    if (!is.null(save_path)){
      openxlsx::write.xlsx(ls, paste0(save_path, "/db_extract_", Sys.Date(), ".xlsx"))
    }
    return(ls)
  } else {
    print("No records matched your inputs.")
  }
}
