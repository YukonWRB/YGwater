#' Calculates snow survey stats for stations
#'
#' @description
#' `r lifecycle::badge('stable')`
#' The purpose of this script is to summarise the SWE data of each station for a particular year and month and compare to previous years. This information is used for the snow bulletin, specifically the SWE map and the 'Drainage basin and snow course" summary table. It is meant to replace Ellen Ward's code from 2020-04-16, script called swe_compiled.R.

#' @param year The year of interest. The stats will be calculated based on all years prior to 'year'.
#' @param month The month of interest. Options are 3, 4 and 5 for March, April and May, respectively. Historical stats are given for the first day of this month.
#' @param csv TRUE or FALSE. If TRUE, a csv will be created.
#' @param return_missing TRUE or FALSE. If TRUE, stations with missing data in the year and month of interest are shown in output table with empty 'depth' and 'swe' columns.
#' @param source Database from which to fetch this data. Options are: hydromet or snow.
#' @return A table and a csv file (if csv = TRUE) with the current snow depth and swe, the swe of the previous year, historical median swe, the swe relative to the median (swe / swe_median), and the number of years with data at that station.
#' @export

#TODO: (1) Add units and parameter to table (2) Create tests (2) query more than 1 month

# # For testing
# test <- SWE_station(year=2023,
#            month=c(3),
#            csv = TRUE,
#            return_missing = TRUE, source = "snow")

SWE_station <-
  function(year,
           month,
           csv = FALSE,
           return_missing = FALSE,
           source = "hyromet") {

    ## From hydromet ##
    if (source == "hydromet") {
      # Retrieve data from db
      con <- hydrometConnect()

      # Get measurements
      Meas <- DBI::dbGetQuery(con,
                              "SELECT locations.name, timeseries.location, measurements_discrete.value, measurements_discrete.target_datetime, measurements_discrete.datetime, timeseries.parameter
                      FROM measurements_discrete
                      INNER JOIN timeseries ON measurements_discrete.timeseries_id=timeseries.timeseries_id
                      INNER JOIN locations ON timeseries.location=locations.location
                      WHERE timeseries.parameter = 'SWE' OR timeseries.parameter = 'snow depth'"
      )

      DBI::dbDisconnect(con)
      # Rename columns:
      colnames(Meas) <- c("location_name", "location_id", "value", "target_date", "sample_date", "parameter")
    } else if (source == "snow") {
      # Retrieve data from db
      con <- snowConnect()

      # Get measurements
      Meas <- DBI::dbGetQuery(con, "SELECT locations.name, means.location, means.swe, means.depth, surveys.target_date, means.sample_datetime
                         FROM means
                         INNER JOIN locations ON means.location=locations.location
                         INNER JOIN surveys on means.survey_id=surveys.survey_id")
      DBI::dbDisconnect(con)

      # Reformat table
      Meas <- reshape2::melt(Meas, id.vars=c("name", "location", "target_date", "sample_datetime"), variable.name = "parameter", value.name="value")

      # Set swe upper case
      Meas$parameter <- as.character(Meas$parameter)
      Meas$parameter[Meas$parameter == "swe"] <- "SWE"
      Meas$parameter[Meas$parameter == "depth"] <- "snow depth"

      # Change column names
      colnames(Meas) <- c("location_name", "location_id", "target_date", "sample_date", "parameter", "value")

      # Extract date from sample_datetime
      Meas$sample_date <- as.Date(Meas$sample_date)

    } else {stop("Parameter 'source' must be either 'hydromet' or 'snow'")}


    # Add Day, Month and Year columns to the Meas dataframe:
    Meas$mon <- lubridate::month(Meas$target_date)
    Meas$yr <- lubridate::year(Meas$target_date)
    Meas$day <- lubridate::day(Meas$target_date)

    # Get elevation (elevation is not in locations table in db...)

    # Only take target_date ending in 0?-01 (to get month of interest and remove mid-month targets
    Meas <- Meas %>% dplyr::filter(grepl(paste0("0", month, "-01"), .data$target_date))

    # For each station: calculate historical median, years of record, and retrieve last years SWE and this years SWE and depth.
    # Create empty table
    swe_station_summary <-
      stats::setNames(data.frame(matrix(ncol = 9, nrow = 0)),
                      c("location_name", "location_id", "sample_date",
                        "depth", "swe", "swe_prevyear", "swe_med", "swe_rat", "years"))
    # swe_station_summary <-
    #   data.frame(location_name=character(),
    #              location_id=character(),
    #              sample_date=character(),
    #              depth=numeric(),
    #              swe=numeric(),
    #              swe_prevyear=numeric(),
    #              swe_med=numeric(),
    #              swe_rat=numeric(),
    #              years=numeric(),
    #              stringsAsFactors=FALSE)

    for (l in unique(Meas$location_id)) {
      # Subset to location of interest
      tab <- Meas %>% dplyr::filter(.data$location_id==l)
      if (return_missing==FALSE) {
        if (length(tab[tab$yr==year,]$value)==0) next
      }

      # get sample date
       sample_date <- tab[tab$yr==year & tab$parameter=="SWE",]$sample_date
       sample_date <- as.Date(sample_date)
       if (length(sample_date)==0) {sample_date <- NA}
      # Get current years depth
       depth <- tab[tab$yr==year & tab$parameter=="snow depth",]$value
       if (length(depth)==0) {depth <- NA}
      # Get current years swe
       swe <- tab[tab$yr==year & tab$parameter=="SWE",]$value
       if (length(swe)==0) {swe <- NA}
      # get previous years swe
       swe_prevyear <- tab[tab$yr==year-1 & tab$parameter=="SWE",]$value
       if (length(swe_prevyear)==0) {swe_prevyear <- NA}
      # Get median swe not including year of interest. Is that right?
       swe_med <- stats::median(tab[tab$yr!=year & tab$parameter=="SWE",]$value)
       if (length(swe_med)==0) {swe_med <- NA}
       # Get ratio between current year and median
       swe_rat <- swe/swe_med
       if (length(swe_rat)==0 | is.infinite(swe_rat)) {swe_rat <- NA}

      # create vector will all row values
      swe_summary_loc <- c(unique(tab$location_name),                # get location name
               l,                                                    # location id
               sample_date,
               depth,
               swe,
               swe_prevyear,
               swe_med,
               swe_rat,
               length(unique(tab$target_date))                # get years of record
               )
      if (length(swe_summary_loc)!=9) {
        warning(paste0("Location ", l, " does not have complete data"))
      }
     # append to table
      swe_station_summary[nrow(swe_station_summary) + 1, ] = swe_summary_loc
    }

    # Set column classes
    swe_station_summary$depth <- as.numeric(swe_station_summary$depth)
    swe_station_summary$swe <- as.numeric(swe_station_summary$swe)
    swe_station_summary$swe_prevyear <- as.numeric(swe_station_summary$swe_prevyear)
    swe_station_summary$swe_med <- as.numeric(swe_station_summary$swe_med)
    swe_station_summary$swe_rat <- as.numeric(swe_station_summary$swe_rat)
    swe_station_summary$years <- as.numeric(swe_station_summary$years)
    swe_station_summary$sample_date <- as.Date(as.numeric(swe_station_summary$sample_date), origin = "1970-01-01")

    # Round swe median and ratio
    swe_station_summary$swe_med <- round(swe_station_summary$swe_med, 0)
    swe_station_summary$swe_rat <- round(swe_station_summary$swe_rat, 2)

    # Set swe_rat NaN to NULL
    swe_station_summary$swe_rat[swe_station_summary$swe_rat == "NaN"] <- NA
    #
    # swe_station_summary <- swe_station_summary %>%
    #   dplyr::mutate(dplyr::across(
    #     c("swe_med", "swe_rat"),
    #     \(x) round (x, 1)
    #   ))

    # Write csv if csv = TRUE
    if (csv == TRUE) {
      utils::write.csv(swe_station_summary, file = paste0("SweSationSummary_", year, "-0", month, ".csv"), row.names = FALSE)
    }

    return(swe_station_summary)

  }


