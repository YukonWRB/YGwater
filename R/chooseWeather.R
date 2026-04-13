#' Help user choose ECCC climate station data to use
#'
#' @description
#' Provides information on the coverage of chosen climate variables for chosen climate stations in proximity. Outputs station descriptions and plots to help you choose the most appropriate climate station(s) to use.
#'
#' chooses station based on given name, coordinates, and period of interest, and displays in a plot the availability of the months of interest for the variables of interest.
#'
#' @seealso [getWeather()] to download climate data from ECCC (called by this function); [combineWeather()] to combine data from multiple stations.
#'
#'
#' @param location A key word to search for station names. A location does not need to be given if coords are supplied.
#' @param coords 	Numeric. A vector of length 2 with latitude and longitude of a place to match against.
#' @param dist Numeric. Match all stations within this many kilometres of the `coords` argument. If `coords` is not given, this argument is ignored.
#' @param interval The interval at which the climate data is aggregated by ECCC. Currently, only the 'day' and 'month' options are available, 'hour' could eventually be added.
#' @param start The earliest year of data to be searched.
#' @param end The latest year of data to be searched.
#' @param variable The variables of interest given in a character vector. The names must correspond to the variable names found in the downloaded csv files from ECCC, retrieved using [getWeather()]. Common variables are 'mean_temp', 'snow_grnd', 'total_precip', 'total_rain', 'total_snow', 'spd_max_gust'...
#' @param months The months of data to be searched, given as a numeric vector from 1 to 12.
#' @param return_data Whether to return the data pulled from ECCC (TRUE) or not (FALSE). Data is returned as a list of dataframes, with one for each station.
#'
#' @return A table printed to the console describing the stations and a plot displayed in RStudio. Assign the function to a variable to also get a plot in your global environment as a ggplot object which can be further modified.
#'
#' @export
#'

##TODO
# Add option to give the data
# Add hour interval
# Output stats on amount of missing data
# Deal with time. Downloaded data is in UTC.
# Need to subset data for start and end dates of interest, not just years
# Map of station locations

chooseWeather <-
  function(
    location = NULL,
    coords = NULL,
    dist,
    interval,
    start,
    end,
    variable,
    months = NULL,
    return_data = FALSE
  ) {
    # For testing
    # location=NULL
    # coords=c(64.0639, -139.4333) #c(60.1660, -132.7247) # Dawson c(64.0639, -139.4333) Whitehorse: c(60.7197, -135.0523)
    # dist=10
    # interval="month"
    # start=1978
    # end=2023
    # variable=c("mean_temp")
    # months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12)
    # return_data = TRUE

    #initial checks
    rlang::check_installed(
      "remotes",
      reason = "to update dependencies for this function."
    )
    if (!rlang::is_installed("weathercan")) {
      #This is here because getWeather is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
      message("Installing dependency 'weathercan'...")
      remotes::install_github("ropensci/weathercan")
      if (rlang::is_installed("weathercan")) {
        message("Package weathercan successfully installed.")
      } else {
        stop(
          "Failed to install package weathercan. You could troubleshoot by running 'remotes::install_github('ropensci/weathercan')' by itself."
        )
      }
    }

    if (is.null(months)) {
      months <- c(
        '01',
        '02',
        '03',
        '04',
        '05',
        '06',
        '07',
        '08',
        '09',
        10,
        11,
        12
      )
    }

    # If end is current year, get sys.date
    if (end == format(Sys.Date(), "%Y")) {
      end_date <- Sys.Date()
    } else {
      end_date <- paste0(end, "-01-01")
    }

    # Get station list and metadata
    stns <- weathercan::stations_search(
      name = location,
      coords = coords,
      dist = dist,
      interval = interval,
      starts_latest = end,
      ends_earliest = start
    )

    # Get each dataset
    # Create list of data

    stn.data <- list()
    for (s in unique(stns$station_id)) {
      suppressWarnings({
        table <- getWeather(
          station = s,
          start = paste0(start, "-01-01"),
          end = end_date,
          interval = interval,
          save_path = NULL
        )
      })
      stn.data <- append(stn.data, list(table))
    }

    # Select only columns and months of interest
    if (interval == "month") {
      cols <- c(
        'station_name',
        'station_id',
        'lat',
        'lon',
        'elev',
        'climate_id',
        'date',
        'year',
        'month',
        paste0(variable),
        paste0(variable, "_flag")
      )
    } else if (interval == "day") {
      cols <- c(
        'station_name',
        'station_id',
        'lat',
        'lon',
        'elev',
        'climate_id',
        'date',
        'year',
        'month',
        'day',
        paste0(variable),
        paste0(variable, "_flag")
      )
    } else {
      stop("Chosen interval not available. Must be 'day', 'month' or 'hour'")
    }

    # Run for loop over each station table
    for (s in 1:length(stn.data)) {
      tab <- stn.data[[s]]
      # Select columns of interest
      tab <- tab[, cols[cols %in% names(tab)], drop = FALSE]
      # Select months of interest
      tab <- tab %>%
        dplyr::filter(.data$month %in% months) %>%
        # Select years of interest
        dplyr::filter(.data$year >= start & .data$year <= end)
      # Set climate_id to character
      tab$climate_id <- as.character(tab$climate_id)
      # Check for logical flags and change to character
      logical_cols <- sapply(tab, is.logical)
      tab[logical_cols] <- lapply(tab[logical_cols], function(x) {
        as.character(x)
      })
      # Set variables to double
      for (v in variable) {
        # If variable exists
        if (v %in% colnames(tab)) {
          tab[, v] <- tab[, v]
        } else {
          # IF VARIABLE DOES NOT EXIST!
          warning(paste0(
            "Variable ",
            v,
            " does not exist for station '",
            unique(tab$station_name),
            "' at the '",
            interval,
            "' interval"
          ))
          tab[v] <- rep(NA, length(tab$station_name))
          tab[paste0(v, "_flag")] <- rep("M", length(tab$station_name))
        }
      }
      # Add table for station to stn.data
      stn.data[[s]] <- tab
    }

    # Print out table about stations
    stn_cols <- c(
      'station_name',
      'station_id',
      'climate_id',
      'lat',
      'lon',
      'elev',
      'tz',
      'start',
      'end'
    )
    stns <- stns[, stn_cols[stn_cols %in% names(stns)], drop = FALSE]
    print("------------ Station descriptions ----------")
    print(as.data.frame(stns))
    print("--------------------------------------------")

    # Plot climate data overlap
    # Merge list of dataframes into single dataframe
    sdata <- dplyr::bind_rows(stn.data)
    # Combine station name and Id
    sdata$station <- paste0(sdata$station_name, " - ", sdata$climate_id)
    # Reshape to long format
    sdata <-
      tidyr::pivot_longer(
        sdata,
        cols = variable,
        names_to = 'variable',
        values_to = 'value'
      )

    # For each variable, take the flag that corresponds to it.
    sdata$flag <- NA
    for (v in variable) {
      sdata[sdata$variable == v, "flag"] <-
        as.vector(sdata[sdata$variable == v, paste0(v, "_flag")])
    }

    # Set missing to true or false
    sdata$missing <- NA
    sdata[is.na(sdata$value), "missing"] <- TRUE
    sdata[sdata$flag %in% c("C", "M", "TRUE"), "missing"] <- TRUE
    sdata[is.na(sdata$missing), "missing"] <- FALSE

    # Remove redundant columns
    sdata <- sdata[, !names(sdata) %in% paste0(variable, "_flag"), drop = FALSE]
    sdata$date <- as.Date(sdata$date)

    plot <- ggplot2::ggplot(
      sdata,
      ggplot2::aes(x = date, y = as.factor(.data$station))
    ) +
      ggplot2::geom_point(ggplot2::aes(colour = missing)) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = "#DC4405", "FALSE" = "#7A9A01")
      ) +
      ggplot2::facet_grid(rows = ggplot2::vars(variable)) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::ylab("Station")

    if (interval == "month") {
      plot <- plot +
        ggplot2::scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")
    }
    if (interval == "day") {
      plot <- plot +
        ggplot2::scale_x_date(date_breaks = "2 year", date_labels = "%Y")
    }

    print(plot)

    return(stn.data)
  }
