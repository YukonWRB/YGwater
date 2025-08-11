#' Combine continuous ECCC climate station data
#'
#' @description
#' Retrieves climate station data from two stations, ideally in close proximity (but that's up to you) and with different temporal coverage and combines them to make a continuous dataset. You can use this to fill gaps in a timeseries or to combine two timeseries. Since the two locations likely have offset measurements at the exact same moment even if they are quite close (for example, station 1 might be slightly warmer than station 2), an offset is calculated if possible and applied to the *second* or subsequent station to make the datasets comparable. Put another way, data from the first listed station takes precedence where an overlap exists; see details for more information. Statistics about this offset are printed to the console for each variable. If the stations do not have temporal overlap no offset is calculated and the two are joined as is.
#'
#' @seealso [getWeather()], [chooseWeather()].
#'
#' @details
#' ## Choosing which station to list first
#' Since the two stations likely have offset measurements at the same moment in time, an offset value is calculated and applied to correct the values of the **second** station. In addition, all values from the *first* station are kept, while overlapping values in the *second* station are discarded after the offset is calculated. Choose the station you list first carefully! If the stations do not have temporal overlap, no offset is calculated and the two are joined as is.
#' 
#' ## Calculation of offset and offset statistics.
#' The offset is calculated using the following formula:
#' 
#' `offset = mean(overlap_station1) - mean(overlap_station2)`
#' 
#' The root mean square error of the offset is also calculated:
#' 
#' `RMSE = sqrt(mean((overlap_station1 - overlap_station2)^2))`
#' 
#' NA values are ignored in the calculation of the offset and RMSE.
#'
#' @param stations The two or more stations you want to combine, given as a list. Can either be a data.frame, data.table, or tibble as output by [getWeather()] or a vector passed to the `station` argument of [getWeather()]. The station listed first is filled in with the 2nd dataset when possible and so on.
#' @param interval Select from 'hour', 'day', or 'month'. Passed to [getWeather()] if the list elements of parameter `stations` are character vectors, ignored if passing data.frames instead.
#' @param start The start date of the dataset to fetch if `stations` is specified as a character vector (otherwise leave as NULL). Specified as a character vector of form "yyyy-mm-dd".
#' @param end The end date of the dataset to fetch if `stations` is specified as a character vector (otherwise leave as NULL). Specified as a character vector of form "yyyy-mm-dd".
#' @param datetime_col The name of the column in the data.frames that contains the datetime information. If relying on getWeather() to get the data this is automatically set, otherwise please specify a column name common to the data.frames passed in `stations`.
#' @param variables The variable or variables to combine in the station datasets, given as the column names common in the data.frames. Ex: c("mean_temp", "total_precip").
#' @param months The months to plot, given as a vector. Must be given as a character, where the January is "01" and October is "10". Leave as NULL for all 12 months.
#'
#' @return A list of data.frames, one for each variable. The data.frame has a date column and a variable column. All dates between start and end are included, whether or not data is available for those dates.
#' @export
#'

##TODO
# Subset to months of interest currently doesn't work because all dates are filled in after subset. Need to think of a way to get around this.
# Add station name to table.

combineWeather <- function(stations, interval = NULL, start = NULL, end = NULL, datetime_col = NULL, variables, months=NULL) {

  if (is.null(months)) {
    months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  }
  
  if (!inherits(stations, "list")) {
    stop("The 'stations' parameter must be a list.")
  }

  if (length(data) < 2) {
    stop("The 'stations' parameter must be a list with two or more elements.")
  
  }
  
#---------------------------------- Get data ----------------------------------##########

  ## For each station
  for (s in 1:length(stations)) {

    #NOTE: s will always NOT be a data.frame as it's an integer from 1 to the length of stations
    # Get data
    if (is.data.frame(stations[[s]])) {
      station <- stations[[s]]
      if (is.null(datetime_col)) {
        stop("If the list elements in the 'stations' parameter are data.frames, the 'datetime_col' parameter must be specified.")
      }
    } else if (is.character(stations[[s]])) {
      if (is.null(interval)) {
       stop("If the list elements in the 'stations' parameter are character strings, the 'interval' parameter must be specified.") 
      }
      if (interval == "hour") {
        datetime_col <- "time"
      } else {
        datetime_col <- "date"
      }
      station <- getWeather(
        station = stations[[s]],
        start = start,
        end = end,
        interval = interval,
        save_path = NULL
      )
    } else {warning("The list elements in the 'stations' parameter must be either station ids (character string) or data.frames")}

    # Check if data exists for this time frame
    if (nrow(station) == 0) {
      warning(paste0("Station ", station, " does not have data for this time frame"))
    } else {
      # Subset to variables of interest and months of interest
      station <- station[, c(datetime_col, intersect(variables, names(station))), drop = FALSE]
      station <- station %>%
        dplyr::filter(lubridate::month(.data[[datetime_col]]) %in% months)

      # Transform to long format
      station <- station %>%
        tidyr::pivot_longer(cols = variables,
                            names_to = "variable",
                            values_to = "value")

      # Add column with station name or list element number
      if (is.character(stations[[s]])) {
        station$station <- stations[[s]]
      } else {
        station$station <- s
      }
    }

    # Replace in stations list
    stations[[s]] <- station
  }

#--------------------- Run for loop over every station ------------------------###########
  # Create dataframe of final product, starting with the first listed station.
  combined_stations <- stations[[1]][!is.na(stations[[1]][[datetime_col]]),]

  for (s in 2:length(stations)) {

    # Select station from list to be added
    station2 <- stations[[s]][!is.na(stations[[s]][[datetime_col]]),]

    if (nrow(station2) == 0) {next}

    # Run for loop over every variable of interest
    combined_variables <- stats::setNames(data.frame(matrix(ncol = 4, nrow = 0)), c(datetime_col, "value", "variable", "station")) # Create empty dataframe
    for (v in variables) {

      message(paste0("------------- Combining variable '", v, "'--------------"))

      # Subset station dataframe to variable of interest
      stn1 <- combined_stations[combined_stations$variable == v,]
      stn2 <- station2[station2$variable == v,]

      #----------------------------- Find overlap -----------------------------#
      # Find overlap
      overlap <- dplyr::inner_join(stn1, stn2, by = datetime_col)
      # Only keep overlap where both are not NA values
      overlap <- overlap[!(is.na(overlap$value.x) | is.na(overlap$value.y)),]
      # # find earliest date that has the month and day +1 of max_d
      # overlap <- overlap %>%
      #   dplyr::filter(date >=
      #                   min(
      #                     date[grepl(
      #                       paste0(
      #                         substr(
      #                           as.character(max(overlap$date) + 1),
      #                           nchar(as.character(max(overlap$date) + 1)) - 5,
      #                           nchar(as.character(max(overlap$date) + 1))
      #                         ), "$"
      #                       ), format(date, "-%m-%d"))])
      #   )

      #--------------------------- Calculate stats ---------------------------#
      message(paste0("Mean at station 1:", mean(overlap$value.x, na.rm = TRUE)))
      message(paste0("Mean at station 2:", mean(overlap$value.y, na.rm = TRUE)))
      bias <- mean(overlap$value.x, na.rm = TRUE) - mean(overlap$value.y, na.rm = TRUE)
      message(paste0("Bias: ", bias))
      message(paste0("RMSE: ", sqrt(mean((overlap$value.x - overlap$value.y)^2, na.rm = TRUE))))
      message("Applying corrections....")

      #--------------------------- Apply correction ---------------------------#
      # If there is no overlap, and as such, no bias, set bias to 0 so that the original data is kept
      if (is.na(bias)) {
        bias <- 0
      }

      if (is.numeric(stn2$value)) {
        # Apply correction to all values of station 2
        stn2$value <- stn2$value + bias
        message("Correction applied")
      }

      #------------------------ Fill gaps in station 1 ------------------------#
      # Where station 1 is missing data in range, fill in with corrected station 2
      # Fill in all missing days in range for station 1 with NAs
      if (inherits(stn2[[datetime_col]], "Date")) {
        all_dates <- seq.Date(as.Date(min(stn1[[datetime_col]], stn2[[datetime_col]])), as.Date(max(stn1[[datetime_col]], stn2[[datetime_col]])), by = "days")
        missing_dates <- as.Date(setdiff(all_dates, stn1[[datetime_col]]))
      } else {
        interval <- calculate_period(stn1, datetime_col = datetime_col) %>%
          dplyr::count(.data$period, name = "count") %>%
          dplyr::filter("count" == max("count")) %>%
          dplyr::pull("period")
        interval <- as.numeric(lubridate::period(interval))
        all_dates <- seq.POSIXt(from = as.POSIXct(min(stn1[[datetime_col]], stn2[[datetime_col]])), to = as.POSIXct(max(stn1[[datetime_col]], stn2[[datetime_col]])), by = interval)
        missing_dates <- as.POSIXct(setdiff(all_dates, stn1[[datetime_col]]))
      }

      if (length(missing_dates) >= 1) {
        names <- c(datetime_col, "value", "variable", "station")
        missing_data <- data.frame("datetime" = missing_dates, 
                                   "value" = NA, 
                                   "variable" = v, 
                                   "station" = NA)
        names(missing_data) <- names
        
        stn1 <- dplyr::bind_rows(stn1[, c(datetime_col, "value", "variable", "station")], missing_data)
      }

      # Replace NAs with corrected station 2
      combined_data <- stn1 %>%
        dplyr::left_join(stn2, by = datetime_col, suffix = c("_stn1", "_stn2")) %>%
        dplyr::mutate(value = dplyr::coalesce(.data$value_stn1, .data$value_stn2),
                      variable = .data$variable_stn1,
                      station = ifelse(!is.na(.data$value_stn1), .data$station_stn1, .data$station_stn2)
                      ) %>%
        dplyr::select(datetime_col, "value", "variable", "station") %>%
        dplyr::arrange(datetime_col)

      message("Gaps filled")

      combined_variables <- rbind(combined_variables, combined_data)

    }
    combined_stations <- combined_variables
  }
  combined_stations <- combined_stations[order(combined_stations[[datetime_col]]),] # Order by datetime
return(combined_stations)
}


