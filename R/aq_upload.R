#' Upload data to Aquarius
#'
#'@description
#' Bypasses the web GUI and allows you to append data to Aquarius directly. By default (overwrite = FALSE), will NOT overwrite or modify points in Aquarius - even if there is no visible data point but an NA or NULL stored in the Aquarius database. If you get a situation where attempting to append points results in no newly appended points, there may be an 'invisible' point; try an overwrite append once sure that you've got the right 'loc_id' and 'timeseries_name'.
#'
#'@details
#' The parameter `data` should consist of a data.frame with two named columns: Value and Time (case sensitive). Units for the column `Value` are set according the units already in use for the timeseries on the Aquarius server, so it should only contain numbers compatible with this. The `Time` column should be formatted as.POSIXct in timezone UTC, keeping in mind that Aquarius will apply the station UTC offset. Failure to ensure the correct timezone of input data will result in offset points.
#'
#' Deleting or overwriting: this function can be used to simply delete data without appending anything. Simply specify a data.frame with both columns ('Time' and 'Value')and NA values, set overwrite = TRUE, and specify a start and end time (inclusive).
#'
#' To store login credentials in your .renviron profile, call [usethis::edit_r_environ()] and enter your username and password as value pairs, as AQUSER="your username" and AQPASS="your password". The server should be entered at server="your_server_url".
#'
#' @param loc_id The location ID, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `29EA001` or `YOWN-0804`.
#' @param ts_name The timeseries name exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `Wlevel_bgs.Calculated`.
#' @param data The data you wish to append to an existing timeseries. Must contain columns named `Value` and `Time`, case sensitive. Time must be in UTC as Aquarius applies the station offset only when displaying or exporting data interactively. See details for more information.
#' @param overwrite Set to TRUE to modify existing data. See details for more information, including if you only want to delete and not append anything.
#' @param start Not required; specify only if you with to overwrite existing data, as a character string of format "2022-01-01 00:00:00" or as a POSIXct object. Inclusive, must be >= to the first data point in data. Timezones should match that of data$Time.
#' @param end Not required; specify only if you with to overwrite existing data, as a character string of format "2022-01-01 00:00:00" or as a POSIXct object. Inclusive, must be <= to the final data point in data. Timezones should match that of data$Time.
#' @param login Your Aquarius login credentials as a character vector of two. Default pulls information from your .renviron profile; see details.
#' @param server The URL for your organization's Aquarius web server. Default pulls from your .renviron file; see details.
#'
#' @return Appends points to the Aquarius server.
#' @export

aq_upload <- function(
  loc_id,
  ts_name,
  data,
  overwrite = FALSE,
  start = NULL,
  end = NULL,
  login = Sys.getenv(c("AQUSER", "AQPASS")),
  server = Sys.getenv("AQSERVER")
) {
  if (overwrite) {
    if (is.null(start) | is.null(end)) {
      stop(
        "You input overwrite = TRUE but have not specified an end and/or start time. Both of these parameters must be set for overwrite to work."
      )
    }
  }

  # Check that data has correct column names
  if (!(all(c("Value", "Time") %in% names(data)))) {
    stop(
      "Your data.frame must contain columns labelled Value and Time. Case sensitive."
    )
  }

  # check if httr and jsonlite are installed as it's needed by timeseries_client.R
  rlang::check_installed("httr", reason = "to use the aq_upload() function")
  rlang::check_installed("jsonlite", reason = "to use the aq_upload() function")

  data$Value[data$Value == ""] <- NA #Set blank spaces to NA
  data$Value[data$Value == " "] <- NA
  data$Value[data$Value == "NA"] <- NA
  data$Value[data$Value == "<NA>"] <- NA

  data <- stats::na.omit(data) # Very important! Any NA data actually gets appended to AQ as a point and is then a PITA to overwrite.

  # Start with server connection
  source(system.file("scripts", "timeseries_client.R", package = "YGwater"))
  # Make the Aquarius configuration and connect
  config = list(
    server = server,
    username = login[1],
    password = login[2],
    timeSeriesName = paste0(ts_name, "@", loc_id),
    eventPeriodStartDay = start,
    eventPeriodEndDay = end
  )

  timeseries$connect(config$server, config$username, config$password)
  on.exit(timeseries$disconnect())

  # Then append Points.
  # Notes about how AQ handles timestamps: it doesn't. The server will take the data fed to it as if it was UTC, without considering the tz attribute, and applies the station offset to that value. Therefore times must be converted to UTC prior to being uploaded, even if the TZ attribute does not matter. Time data can be fed in as.POSIXct or as dateTtime.

  result <- timeseries$waitForCompletedAppendRequest(
    timeseries$appendPoints(config$timeSeriesName, data, start, end),
    120
  ) # This makes it wait up to 120 seconds to show the append result - long enough for even giant datasets.
  points_in_file <- nrow(data)

  output <- list(
    appended = result$NumberOfPointsAppended,
    input = points_in_file
  )

  if (result$AppendStatus == "Completed") {
    cli::cli_alert_success(
      "{.strong Your request was completed:} {result$NumberOfPointsAppended} points were appended out of the {points_in_file} that were in the provided dataset.\nThe points were appended to the timeseries {.strong {ts_name}} at location {.strong {loc_id}}."
    )
  } else {
    cli::cli_alert_danger(
      "{.strong Your request was not completed or had an irregular status:} {result$AppendStatus}.\n{result$NumberOfPointsAppended} points were appended out of the {points_in_file} that were in the provided dataset.\nThe target timeseries was {.strong {ts_name}} at location {.strong {loc_id}}."
    )
  }
  return(output)
}
