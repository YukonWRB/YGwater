#' Calculates snow survey stats for stations
#'
#' @description
#' `r lifecycle::badge('stable')`
#' The purpose of this script is to summarise the SWE data of each station for a particular year and month and compare to previous years. This information is used for the snow bulletin, specifically the SWE map and the 'Drainage basin and snow course" summary table. It is meant to replace Ellen Ward's code from 2020-04-16, script called swe_compiled.R.

#' @param stations A vector of snow course stations, identified with their location ID. Default is "all", where all stations in the snow db are taken.
#' @param year The year of interest. The stats will be calculated based on all years prior to 'year'.
#' @param month The month of interest. Options are 3, 4 and 5 for March, April and May, respectively. Historical stats are given for the first day of this month.
#' @param csv TRUE or FALSE. If TRUE, a csv will be created.
#' @param return_missing TRUE or FALSE. If TRUE, stations with missing data in the year and month of interest are shown in output table with empty 'depth' and 'swe' columns.
#' @param active TRUE or FALSE. If TRUE, only active stations are retrieved. If FALSE, all stations, whether active or not, are retrieved.
#' @param source Database from which to fetch this data. Options are: hydromet or snow.
#' @param summarise TRUE or FALSE. If TRUE, the output table is summarised by sub-basin. If FALSE, the output table is not summarised.
#' @return A table and a csv file (if csv = TRUE) with the current snow depth and swe, the swe of the previous year, historical median swe, the swe relative to the median (swe / swe_median), and the number of years with data at that station.
#' @export

#TODO: (1) Add units and parameter to table (2) Create tests (2) query more than 1 month

# # For testing
# test1 <- SWE_station(stations = "08AA-SC01",
#                      year=2024,
#            month=c(3),
#            csv = FALSE,
#            return_missing = TRUE,
#            active = TRUE,
#            source = "snow",
#            summarise = TRUE)
# # 
# stations = "08AA-SC01"
# year <- 2024
# month <- 3
# csv = FALSE
# return_missing = TRUE
# active = TRUE
# source <- "snow"
# summarise = TRUE


SWE_station <-
  function(stations = "all",
           year,
           month,
           csv = FALSE,
           return_missing = FALSE,
           active = TRUE,
           source = "hydromet",
           summarise = TRUE) {
    
    # First retrieve location-basin info from snow db
    con <- snowConnect(silent = TRUE)
    loc_basin <- DBI::dbGetQuery(con, "SELECT location, sub_basin, active FROM locations")
    DBI::dbDisconnect(con)
    colnames(loc_basin) <- c("location_id", "sub_basin", "active")
    
    # Create list of stations
    if (all(stations == "all")) {
      stations <- unique(loc_basin$location_id)
    }

#### --------------------------- Retrieve data ---------------------------- ####
    ## From hydromet db ##
    if (source == "hydromet") {
      # Retrieve data from db
      con <- hydrometConnect(silent = TRUE)
      
      # Get measurements
      Meas <- DBI::dbGetQuery(con,
                              paste0("SELECT locations.name, locations.location, measurements_discrete.value, measurements_discrete.target_datetime, measurements_discrete.datetime, parameters.param_name, datum_conversions.conversion_m, measurements_discrete.note
                      FROM measurements_discrete
                      INNER JOIN timeseries ON measurements_discrete.timeseries_id=timeseries.timeseries_id
                      INNER JOIN locations ON timeseries.location=locations.location
                      INNER JOIN parameters ON timeseries.parameter = parameters.param_code
                      INNER JOIN datum_conversions ON locations.location_id = datum_conversions.location_id
                      WHERE (parameters.param_name = 'snow water equivalent' OR parameters.param_name = 'snow depth') AND
                              locations.location IN ('", paste0(stations, collapse="', '"), "')")
      )
      
      DBI::dbDisconnect(con)
      # Rename columns:
      colnames(Meas) <- c("location_name", "location_id", "value", "target_date", "sample_date", "parameter", "elevation", "note")
      # Change 'snow water equivalent' to SWE
      Meas[Meas$parameter=='snow water equivalent',]$parameter <- "SWE"
      # Where note = estimated, make estimate_flag = TRUE
      Meas$estimate_flag <- NA
      Meas[grep("estimated", Meas$note), ]$estimate_flag <- TRUE
      # Remove note
      Meas <- subset(Meas, select = -c(note))
      
      # Calculate density
      # Spread the data into separate columns for swe and snow_depth
      wider_data <- Meas %>%
        tidyr::pivot_wider(names_from = "parameter", values_from = "value")
      # calculate
      wider_data$density <- round(wider_data$SWE / wider_data$`snow depth` * 10, 2)
      # Reformat into long format
      Meas <- wider_data %>%
        tidyr::pivot_longer(cols = c("SWE", "snow depth", "density"), names_to = "parameter", values_to = "value")
      
      
      ## From snow db ##
    } else if (source == "snow") {
      # Retrieve data from db
      con <- snowConnect(silent = TRUE)
      
      # Get measurements
      Meas <- DBI::dbGetQuery(con, paste0("SELECT means.name, means.location, means.swe, means.depth, means.target_date,
                         means.survey_date, locations.elevation, means.estimate_flag
                         FROM means
                         INNER JOIN locations ON means.location = locations.location
                         WHERE means.location IN ('", paste0(stations, collapse="', '"), "')"))
      DBI::dbDisconnect(con)
      
      # Calculate density
      Meas$density <- round((Meas$swe / Meas$depth) *10, 2)
      
      # Reformat table
      Meas <- reshape2::melt(Meas, id.vars = c("name", "location", "target_date", "survey_date", 
                                               "elevation", "estimate_flag"), variable.name = "parameter", value.name = "value")
      
      # Set swe upper case
      Meas$parameter <- as.character(Meas$parameter)
      Meas$parameter[Meas$parameter == "swe"] <- "SWE"
      Meas$parameter[Meas$parameter == "depth"] <- "snow depth"
      
      # Change column names
      colnames(Meas) <- c("location_name", "location_id", "target_date", "sample_date", 
                          "elevation", "estimate_flag", "parameter", "value")
      
      # Extract date from sample_datetime
      Meas$sample_date <- as.Date(Meas$sample_date)
      
    } else {stop("Parameter 'source' must be either 'hydromet' or 'snow'")}
    
    
    # Add Day, Month and Year columns to the Meas dataframe:
    Meas$mon <- lubridate::month(Meas$target_date)
    Meas$yr <- lubridate::year(Meas$target_date)
    Meas$day <- lubridate::day(Meas$target_date)
    
    # Only take target_date ending in 0?-01 (to get month of interest and remove mid-month targets
    Meas <- Meas %>% dplyr::filter(.data$mon %in% month & .data$day == 1)
    # Denote record years, estimate years and years outside of valid sampling range
    tabl <- Meas
    
#### ------------------------- Summarise results -------------------------- ####
    if (summarise == TRUE) {
      # For each station: calculate historical median, years of record, and retrieve last years SWE and this years SWE and depth.
      # Create empty table
      swe_station_summary <-
        stats::setNames(data.frame(matrix(ncol = 19, nrow = 0)),
                        c("location_name", "location_id", "elevation", "sample_date",
                          "swe", "swe_prevyear", "swe_med", "swe_norm_1991_2020", "swe_rat", "swe_min", 
                          "swe_max", "depth", "depth_med", "density", "density_med", 
                          "years", "record_flag", "date_flag", "estimate_flag"))
      
      for (l in unique(Meas$location_id)) {
        # Subset to location of interest
        tab <- Meas %>% dplyr::filter(.data$location_id == l)
        if (return_missing == FALSE) {
          if (length(tab[tab$yr == year,]$value) == 0) next
        }
        # Remove duplicate rows to deal with error....hopefully can eventually remove when error is fixed
        # tab <- tab %>% 
        #   dplyr::distinct(location_id, estimate_flag, parameter, mon, yr, day, .keep_all = TRUE)
        # get sample date
        sample_date <- tab[tab$yr == year & tab$parameter == "SWE",]$sample_date
        sample_date <- as.Date(sample_date)
        if (length(sample_date) == 0) {sample_date <- NA}
        # Get current years swe
        swe <- tab[tab$yr == year & tab$parameter == "SWE",]$value
        if (length(swe) == 0) {swe <- NA}
        # get previous years swe
        swe_prevyear <- tab[tab$yr == year - 1 & tab$parameter == "SWE",]$value
        if (length(swe_prevyear) == 0) {swe_prevyear <- NA}
        # Get median swe not including year of interest.
        swe_med <- round(stats::median(tab[tab$yr != year & tab$parameter == "SWE",]$value), 0)
        if (length(swe_med) == 0) {swe_med <- NA}
        # Get normal swe (1991-2020)
        swe_norm_1991_2020 <- round(mean(tab[tab$parameter == "SWE" &
                                          tab$yr >= 1991 & tab$yr <= 2020,]$value), 0)
        if (length(swe_norm_1991_2020) == 0 | swe_norm_1991_2020 == "NaN") {swe_norm_1991_2020 <- NA}
        # Get ratio between current year and median
        swe_rat <- round(swe/swe_med, 2)
        if (length(swe_rat) == 0 | is.infinite(swe_rat)) {swe_rat <- NA}
        # Get min SWE not including year of interest
        swe_min <- round(min(tab[tab$yr != year & tab$parameter == "SWE",]$value), 0)
        if (length(swe_min) == 0 | is.infinite(swe_min)) {swe_min <- NA}
        # Get max SWE not including year of interest
        swe_max <- round(max(tab[tab$yr != year & tab$parameter == "SWE",]$value), 0)
        if (length(swe_max) == 0 | is.infinite(swe_max)) {swe_max <- NA}
        # Get current years depth
        depth <- tab[tab$yr == year & tab$parameter == "snow depth",]$value
        if (length(depth) == 0) {depth <- NA}
        # Get median depth not including year of interest
        depth_med <- round(stats::median(tab[tab$yr != year & tab$parameter == "snow depth",]$value), 0)
        if (length(depth_med) == 0) {depth_med <- NA}
        # Get current year % density 
        density <- round(tab[tab$yr == year & tab$parameter == "density",]$value, 0)
        if (length(density) == 0) {density <- NA}
        # Get median % density not including year of interest
        density_med <- round(stats::median(tab[tab$yr != year & tab$parameter == "density",]$value, na.rm=TRUE), 0)
        if (length(density_med) == 0) {density_med <- NA}
        # Record flag
        if (is.na(swe) | is.na(swe_max)) {
          record_flag <- FALSE
        } else if (swe > swe_max | swe < swe_min) {
          record_flag <- TRUE
        } else if (swe < swe_max) {record_flag <- FALSE}
        # date flag (sample_date outside of valid sampling range)
        target_date <- tab[tab$yr == year & tab$parameter == "SWE",]$target_date
        if (length(target_date) == 0) {
          date_flag <- FALSE
        } else if (sample_date > target_date + lubridate::days(7) |
            sample_date < target_date - lubridate::days(7)) {
          date_flag <- TRUE
        } else {date_flag <- FALSE}
        # estimate_flag
        estimate_flag <- tab[tab$yr==year & tab$parameter=="SWE",]$estimate_flag
        if (length(estimate_flag) == 0) {
          estimate_flag <- FALSE
        } else if (is.na(estimate_flag)) {
          estimate_flag <- FALSE
        } 
        # if (length(tab[tab$yr==year & tab$parameter=="SWE",]$note) == 0) {
        #   estimate_flag <- FALSE
        # } else if (is.na(tab[tab$yr==year & tab$parameter=="SWE",]$note)) {
        #   estimate_flag <- FALSE
        # } else if (tab[tab$yr==year & tab$parameter=="SWE",]$note == 'estimated') {
        #   estimate_flag <- TRUE
        # } else {estimate_flag <- FALSE}
        
        # create vector with all row values
        swe_summary_loc <- c(unique(tab$location_name),                # get location name
                             l, # location id
                             unique(tab$elevation),
                             sample_date,
                             swe,
                             swe_prevyear,
                             swe_med,
                             swe_norm_1991_2020,
                             swe_rat,
                             swe_min,
                             swe_max,
                             depth,
                             depth_med,
                             density,
                             density_med,
                             length(unique(tab$target_date)),  # get years of record
                             record_flag,
                             date_flag,
                             estimate_flag
        )
        if (length(swe_summary_loc) != 10) {
          warning(paste0("Location ", l, " does not have complete data"))
        }
        # append to table
        swe_station_summary[nrow(swe_station_summary) + 1, ] = swe_summary_loc
      }
      
      # Set column classes
      swe_station_summary$sample_date <- as.Date(as.numeric(swe_station_summary$sample_date), origin = "1970-01-01")
      swe_station_summary$swe <- as.numeric(swe_station_summary$swe)
      swe_station_summary$swe_prevyear <- as.numeric(swe_station_summary$swe_prevyear)
      swe_station_summary$swe_med <- as.numeric(swe_station_summary$swe_med)
      swe_station_summary$swe_norm_1991_2020 <- as.numeric(swe_station_summary$swe_norm_1991_2020)
      swe_station_summary$swe_rat <- as.numeric(swe_station_summary$swe_rat)
      swe_station_summary$swe_min <- as.numeric(swe_station_summary$swe_min)
      swe_station_summary$swe_max <- as.numeric(swe_station_summary$swe_max)
      swe_station_summary$depth <- as.numeric(swe_station_summary$depth)
      swe_station_summary$depth_med <- as.numeric(swe_station_summary$depth_med)
      swe_station_summary$density <- as.numeric(swe_station_summary$density)
      swe_station_summary$density_med <- as.numeric(swe_station_summary$density_med)
      swe_station_summary$years <- as.numeric(swe_station_summary$years)
      
      # Set swe_rat NaN to NA
      swe_station_summary$swe_rat[swe_station_summary$swe_rat == "NaN"] <- NA
      
      tabl <- swe_station_summary
    }
    
    # Add column for sub_basin
    tabl <- merge(tabl, loc_basin, by = "location_id")
   
#### ----------------------------- Last bits ------------------------------ ####
    
    # Remove those that are inactive or not
    if (active == TRUE) {
      tabl <- tabl[tabl$active == TRUE, ]
    } else {tabl <- tabl}
    
    # remove active column
    tabl <- subset(tabl, select = -active)
    
    # Write csv if csv = TRUE
    if (csv == TRUE) {
      utils::write.csv(tabl, file = paste0("SweSationSummary_", year, "-0", month, ".csv"), row.names = FALSE)
    }
    
    return(tabl)
    
  }
