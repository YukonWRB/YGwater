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
#' @param variable The variable to combine in the station datasets.
#' @param months The months to plot, given as a vector. Must be given as a character, where the January is "01" and October is "10".
#'
#'
#' @return A datraframe with a date column and a variable column.
#' @export
#'

##TODO
# Subset to months of interest currently doesn't work because all dates are filled in after subset. Need to think of a way to get around this.
# Fill in remaining gaps with simple interpolation?
# Fill out roxygen

combineWeather <- function(stations, start, end, variable, months=NULL) {

  if (is.null(months)) {
    months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  }

  #---------------------------------- Get data ----------------------------------#
  if (is.data.frame(stations[[1]])) {
    station1 <- stations[[1]]
  } else if (is.character(stations[[1]])) {
    station1 <- getWeather(
      station = stations[[1]],
      start = start,
      end = end,
      interval = "day",
      save_path = NULL
    )
  } else {warning("The objects in the stations parameter must be either a station id (character string) or a dataframe")}

  if (is.data.frame(stations[[2]])) {
    station2 <- stations[[2]]
  } else if (is.character(stations[[2]])) {
    station2 <- getWeather(
      station = stations[[2]],
      start = start,
      end = end,
      interval = "day",
      save_path = NULL
    )
  } else {warning("The objects in the stations parameter must be either a station id (character string) or a dataframe")}

  # Subset to variables of interest and months of interest
  station1 <- station1 %>% dplyr::select(c("date", variable)) %>%
    dplyr::filter(lubridate::month(date) %in% months)

  station2 <- station2 %>% dplyr::select(c("date", variable)) %>%
    dplyr::filter(lubridate::month(date) %in% months)
  # Set generic name of variable column in both dataframes
  colnames(station1) <- c("date", "variable")
  colnames(station2) <- c("date", "variable")

  #-------------------------------- Find overlap --------------------------------#
  # Find overlap
  overlap <- dplyr::inner_join(station1, station2, by='date')
  # find earliest date that has the month and day +1 of max_d
  overlap <- overlap %>%
    dplyr::filter(date >
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

  #---------------------------- Calculate stats ----------------------------#
  print(paste0("Mean at station 1:", mean(overlap$variable.x, na.rm=TRUE)))
  print(paste0("Mean at station 2:", mean(overlap$variable.y, na.rm=TRUE)))
  bias <- mean(overlap$variable.x, na.rm=TRUE) - mean(overlap$variable.y, na.rm=TRUE)
  print(paste0("Bias: ", bias))
  print(paste0("RMSE: ", sqrt(mean((overlap$variable.x-overlap$variable.y)^2, na.rm=TRUE))))

  #------------------------------ Apply correction ------------------------------#
  # Apply correction to all values of station 2
  station2$variable <- station2$variable + bias

  #--------------------------- Fill gaps in station 1 ---------------------------#
  # Where station 1 is missing data in range, fill in with corrected station 2
  # Fill in all missing days in range for station 1 with NAs
  all_dates <- seq(as.Date(start), as.Date(end), by = "days")
  missing_dates <- as.Date(setdiff(all_dates, station1$date))

  if (length(missing_dates)>=1) {
    missing_data <- data.frame(date = missing_dates, variable = NA)
    station1 <- dplyr::bind_rows(station1[, c("date", "variable")], missing_data)
  }

  # Replace NAs with corrected station 2
  combined_data <- station1 %>%
    dplyr::left_join(station2, by = "date", suffix = c("_station1", "_station2")) %>%
    dplyr::mutate(variable = dplyr::coalesce(.data$variable_station1, .data$variable_station2)) %>%
    dplyr::select(date, variable) %>%
    dplyr::arrange(date)

  # Rename variable column with original name
  colnames(combined_data) <- c("date", variable)

  return(combined_data)
}

