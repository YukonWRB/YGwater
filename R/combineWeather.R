#' Combine continuous ECCC climate station data
#'
#' stations, start, end, variable, months=NULL
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Retrieves climate station data from two stations in proximity and combines them to make continuous dataset.
#'
#'
#' @details
#'
#' @param stations The two stations you want to combine, given as a list with two elements. Can either be a dataframe as outputted by getWeather or a station id. The station listed first is filled in with the 2nd dataset when possible.
#' @param start The start date of the dataset to create. Is specified as a character string of form "yyyy-mm-dd".
#' @param end The end date of the dataset to create.
#' @param variables The variable or variables to combine in the station datasets, given as a character string. Ex: c("mean_temp", "total_precip").
#' @param months The months to plot, given as a vector. Must be given as a character, where the January is "01" and October is "10".
#'
#'
#' @return A list of dataframes, one for each variable. The dataframe has a date column and a variable column. All dates between start and end are included, whether or not data is available for those dates.
#' @export
#'

##TODO
# Subset to months of interest currently doesn't work because all dates are filled in after subset. Need to think of a way to get around this.
# Add station name to table.

# Fill in remaining gaps with simple interpolation?
# Fill out roxygen
# stations = list("2101310", "2101300", "2101400")
# start='2010-01-01'
# end='2013-12-03'
# variables=c("mean_temp", "total_precip", "total_rain", "total_snow", "min_temp", "max_temp")
# months=NULL

# test <- combineWeather(stations = list("2101310", "2101300", "2101400"), start='2010-01-01', end='2013-11-01', variables=c("mean_temp", "total_precip", "min_temp", "max_temp"), months=NULL)

# # # Check for hourly data on missing days?
# test <- whitehorse[whitehorse$variable=="mean_temp", ]
# for (d in test[is.na(test$value), ]$date) {
#   d <- as.Date(d)
#   # Subset to row of interest
#   rw <- test[test$date == d,]
#   # Get station before this empty one
#   stn <- test %>%
#     dplyr::filter(date <= d, !is.na(value)) %>%
#     dplyr::slice_max(order_by = date) %>%
#     dplyr::pull(station)
#
#   # Get hourly data for that day
#   test2 <- getWeather(station = stn, start = d, end = d,
#                      tzone = "UTC",
#                      interval = "hour",
#                      save_path = NULL)
#   # Calculate mean from min and max
#   avg <- (max(test2$temp) - min(test2$temp)) / 2
#   # Replace NA with avg
#   test[test$date==d,]$value <- avg
#   test[test$date==d,]$station <- stn
# }



combineWeather <- function(stations, start, end, variables, months=NULL) {

  if (is.null(months)) {
    months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  }

#---------------------------------- Get data ----------------------------------#

  ## For each station
  for (s in 1:length(stations)) {

    # Get data
    if (is.data.frame(s)) {
      station <- stations[[s]]
    } else if (is.character(stations[[s]])) {
      station <- getWeather(
        station = stations[[s]],
        start = start,
        end = end,
        interval = "day",
        save_path = NULL
      )
    } else {warning("The objects in the stations parameter must be either a station id (character string) or a dataframe")}

    test2 <<- station
    # Check if data exists for this time frame
    if (nrow(station)==0) {
      station <- station
      warning(paste0("Station ", station, " does not have data for this time frame"))
    } else {
      # Subset to variables of interest and months of interest
      station <- station %>% dplyr::select(c("date", tidyselect::all_of(variables))) %>%
        dplyr::filter(lubridate::month(date) %in% months)

      # Transform to long format
      station <- station %>%
        tidyr::pivot_longer(cols = variables,
                            names_to = "variable",
                            values_to = "value")

      # Add column with station name
      station$station <- stations[[s]]
    }

    # Replace in stations list
    stations[[s]] <- station

    test1 <<- stations

  }

#--------------------- Run for loop over every station ------------------------#
  # Create dataframe of final product which is
  combined_stations <- stations[[1]]

  for (s in 2:length(stations)) {

    # Select station from list to be added
    station2 <- stations[[s]]

    if (nrow(station2)==0) {next}

    # Run for loop over every variable of interest
    combined_variables <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("date", "value", "variable", "station"))
    for (v in variables) {

      print(paste0("Combining variable '", v, "'"))

      # Subset station dataframe to variable of interest
      stn1 <- combined_stations[combined_stations$variable==v,]
      stn2 <- station2[station2$variable==v,]

      #----------------------------- Find overlap -----------------------------#
      # Find overlap
      overlap <- dplyr::inner_join(stn1, stn2, by='date')
      # find earliest date that has the month and day +1 of max_d
      overlap <- overlap %>%
        dplyr::filter(date >=
                        min(
                          date[grepl(
                            paste0(
                              substr(
                                as.character(max(overlap$date) + 1),
                                nchar(as.character(max(overlap$date) + 1)) - 5,
                                nchar(as.character(max(overlap$date) + 1))
                              ), "$"
                            ), format(date, "-%m-%d"))])
        )

      #--------------------------- Calculate stats ---------------------------#
      print(paste0("Mean at station 1:", mean(overlap$value.x, na.rm=TRUE)))
      print(paste0("Mean at station 2:", mean(overlap$value.y, na.rm=TRUE)))
      bias <- mean(overlap$value.x, na.rm=TRUE) - mean(overlap$value.y, na.rm=TRUE)
      print(paste0("Bias: ", bias))
      print(paste0("RMSE: ", sqrt(mean((overlap$value.x-overlap$value.y)^2, na.rm=TRUE))))

      #--------------------------- Apply correction ---------------------------#
      # Apply correction to all values of station 2
      stn2$value <- stn2$value + bias

      print("Correction applied")

      #------------------------ Fill gaps in station 1 ------------------------#
      # Where station 1 is missing data in range, fill in with corrected station 2
      # Fill in all missing days in range for station 1 with NAs
      all_dates <- seq(as.Date(start), as.Date(end), by = "days")
      missing_dates <- as.Date(setdiff(all_dates, stn1$date))

      if (length(missing_dates)>=1) {
        missing_data <- data.frame(date = missing_dates, value = NA, variable = v, station = NA)
        stn1 <- dplyr::bind_rows(stn1[, c("date", "value", "variable", "station")], missing_data)
      }

      # Replace NAs with corrected station 2
      combined_data <- stn1 %>%
        dplyr::left_join(stn2, by = "date", suffix = c("_stn1", "_stn2")) %>%
        dplyr::mutate(value = dplyr::coalesce(.data$value_stn1, .data$value_stn2),
                      variable = .data$variable_stn1,
                      station = ifelse(!is.na(value_stn1), station_stn1, station_stn2)
                      ) %>%
        dplyr::select(date, value, variable, station) %>%
        dplyr::arrange(date)

      print("Gaps filled")

      combined_variables <- rbind(combined_variables, combined_data)

    }
    combined_stations <- combined_variables
  }
return(combined_stations)
}


