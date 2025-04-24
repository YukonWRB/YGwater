#' Calculates snow survey stats for basins
#'
#' @description
#' `r lifecycle::badge('stable')`
#' The purpose of this script is to summarise the SWE data of each basin for a particular year and month and compare to previous years. It is used for the snow bulletin, specifically the SWE map and the plot B. It is meant to replace Ellen Ward's code from 2020-04-16, r script called swe_compiled_basin.R.

#' @param year The year of interest. If summarise = TRUE, the stats will be calculated based on all years prior to 'year'. If summarise = FALSE, only data from the current year and before are taken.
#' @param month The month of interest. Options are 3, 4 and 5 for March, April and May, respectively. Can also give multiple months as a vector. Historical stats are given for the first day of this month.
#' @param lookback The number of years to look back for historical stats. Default is 30. NOT YET IMPLEMENTED.
#' @param threshold A number between 1 and 10 giving the threshold below which the SWE for that basin and year are ignored. These numbers represent the sum of the factors of the stations for a basin which are not missing data for that year. 10 means that the swe values calculated from less than all the stations of that basin are ignored. 1 means that only the swe calculated from less than 1 out of 10 are ignored.
#' @param summarise TRUE to summarise the data into a data.frame with the current SWE, historical median, the swe relative to the median (swe/swe_median), historical maximum, historical minimum, and year of maximum and minimum for each basin.
#' @param csv TRUE or FALSE. If TRUE, a csv will be created.
#' @param source Database from which to fetch this data. Options are: aquacache or snow.
#' @param con A connection to the database specified in `source`. If NULL, a new connection will be created with [AquaConnect()] or [snowConnect()] (depending on 'source' parameter) and automatically closed.
#' 
#' @return A table and a csv file (if csv = TRUE) with either (summarise = FALSE) the swe for all basins, years and months of interest or (summarise = TRUE) the current SWE, historical median, the swe relative to the median (swe / swe_median), historical maximum, historical minimum, and year of maximum and minimum for each basin and month of interest.
#' @export

SWE_basin <- function(year,
                      month,
                      lookback = 100,
                      threshold = 7,
                      csv = FALSE,
                      summarise = FALSE,
                      source = "aquacache",
                      con = NULL) {
  
  # parameter checks
  if (any(!month %in% c(3, 4, 5))) {
    stop("Parameter 'month' must be either 3, 4 or 5")
  }
  if (threshold < 1 | threshold > 10) {
    stop("Parameter 'threshold' must be between 1 and 10")
  }
  source <- tolower(source)
  if (!source %in% c("aquacache", "snow")) {
    stop("Parameter 'source' must be either 'aquacache' or 'snow'")
  }
  
  ### Retrieve data from aquacache db
  if (source == "aquacache") {
    if (is.null(con)) {
      con <- AquaConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
    }
    
    samples <- DBI::dbGetQuery(con, paste0(
                               "SELECT s.sample_id, l.location, s.target_datetime 
                               FROM samples s 
                               INNER JOIN locations l ON l.location_id = s.location_id 
                               WHERE media_id = 7 
                               AND collection_method = 1
                               AND target_datetime >= '", year - lookback, "-01-01';")) # media = 'atmospheric', collection_method = 'observation'
    
    Meas <- DBI::dbGetQuery(con, 
                            paste0("SELECT r.sample_id, r.result AS value
                                   FROM results AS r 
                                   JOIN parameters AS p ON p.parameter_id = r.parameter_id 
                                   WHERE r.sample_id IN ('", paste(samples$sample_id, collapse = "', '"), "') 
                                   AND p.parameter_id = 21")) # 21 = SWE
    Meas <- merge(Meas, samples, by = "sample_id")
    Meas <- Meas[, c("location", "value", "target_datetime")]
    # Rename columns:
    colnames(Meas) <- c("location_id", "SWE", "target_date")
  } else if (source == "snow") { ### Retrieve data from snow db
    if (is.null(con)) {
      con <- snowConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
    }
    
    Meas <- DBI::dbGetQuery(con, paste0("SELECT location, swe, target_date FROM means WHERE target_date >= '", year - lookback, "-01-01'"))
    # Rename columns:
    colnames(Meas) <- c("location_id", "SWE", "target_date")
  }
  
  ###### PART 1. Aggregate SWE by basin and year ######
  # 1. Import the Factors table: To use location_numS and Weights for basin-scale SWE estimates:
  # Factors <-
  #   openxlsx::read.xlsx(paste0(file_loc, "/Course_Factors.xlsx"))
  Factors <- data$snowcourse_factors
  
  # 2. Add Day, Month and Year columns to the Meas dataframe:
  Meas$mon <- lubridate::month(Meas$target_date)
  Meas$yr <- lubridate::year(Meas$target_date)
  Meas$day <- lubridate::day(Meas$target_date)
  
  # 3. Subset to month or months of interest
  Meas <- Meas %>% dplyr::filter(.data$mon %in% month & .data$day == 1)
  
  # Create vector of basins
  basins <- c(
    "Pelly",
    "Liard",
    "Stewart",
    "Peel",
    "Porcupine",
    "White",
    "Central_Yukon",
    "Lower_Yukon",
    "Upper_Yukon",
    "Teslin_Big_Salmon",
    "Alsek"
  )
  # 4. go through each year one by one
  swe_basin_year <-
    stats::setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                    c("basin", "yr", "mon", "swe", "perc"))
  # Create year, month dataframe to run through
  yr_mon <- expand.grid(years = 1980:year, months = month)
  
  for (ym in 1:nrow(yr_mon)) {
    tab <- Meas %>% dplyr::filter(.data$yr == yr_mon[ym,1] & .data$mon == yr_mon[ym,2])
    # Go through each basin one by one
    for (b in basins) {
      # subset factors to only what we need
      fact <- Factors[, c(b, "location_id")]
      names(fact) <- c("val", "location_id")
      fact <- fact[fact$val != 0,]
      
      # Go through each location one by one
      # Initialize empty dataframe
      sweb <- data.frame("location_id" = numeric(),
                         "swe" = numeric(),
                         "perc" = numeric())
      for (l in fact$location_id) {
        # Check if location has measurement in tab
        if (length(tab[tab$location_id == l,]$SWE) == 0 |
            is.null(tab[tab$location_id == l,]$SWE)) {
          # Create vector with location, swe, percentage of basin that it represents. 0 if value is missing
          swe <- c(l, NA, 0)
        } else if (!is.null(tab[tab$location_id == l,]$SWE &
                            length(tab[tab$location_id == l,]$SWE) == 1)) {
          # Create vector with location, swe, percentage of basin that it represents
          swe <-
            c(l,
              fact[fact$location_id == l,]$val * tab[tab$location_id == l,]$SWE / 10,
              fact[fact$location_id == l,]$val)
        }
        # Calculate sum for basin with total percentage not missing
        sweb[nrow(sweb) + 1, ] = swe
      }
      # calculate total percent of available numbers
      perc <- sum(as.numeric(sweb$perc), na.rm = TRUE)
      if (perc >= 10) {
        swe <- hablar::rationalize(sum(as.numeric(sweb$swe), na.rm = TRUE))
      } else {
        swe <- hablar::rationalize(sum(as.numeric(sweb$swe), na.rm = TRUE) * 10 / perc)
      }
      swe_basin_year[nrow(swe_basin_year) + 1, ]  <- c(b, yr_mon[ym,1], yr_mon[ym,2], swe, perc)
    }
    # swe_basin_year[nrow(swe_basin_year) + 1, ]  <- swe_basin_year
  }
  
  # Set column classes
  swe_basin_year$yr <- as.numeric(swe_basin_year$yr)
  swe_basin_year$swe <- as.numeric(swe_basin_year$swe)
  swe_basin_year$perc <- as.numeric(swe_basin_year$perc)
  
  # Add units and parameter
  swe_basin_year$parameter <- rep("SWE", length(swe_basin_year$basin))
  swe_basin_year$units <- rep("mm", length(swe_basin_year$basin))
  
  # Get current year values
  swe_basin_current <- swe_basin_year %>%
    dplyr::filter(.data$yr == year)
  
  # Remove years based on percentage of basin stations with measurements
  swe_basin_year <- swe_basin_year %>% dplyr::filter(perc >= threshold)
  swe_basin <- swe_basin_year
  
  # rename columns
  colnames(swe_basin) <- c("location", "year", "month", "value", "perc", "parameter", "units")
  
  ## Calculate max, min and median historical SWE for each basin if summarise = TRUE
  if (summarise) {
    # calculate stats excluding current year
    swe_basin_summary <- swe_basin_year %>%
      dplyr::filter(.data$yr != year) %>%
      dplyr::group_by(.data$basin, .data$mon) %>%
      dplyr::summarize(
        swe_max = max(swe, na.rm = TRUE),
        year_max = .data$yr[which.max(swe)],
        swe_min = min(swe, na.rm = TRUE),
        year_min = .data$yr[which.min(swe)],
        swe_median = stats::median(swe, na.rm = TRUE)
      )
    # combine tables
    swe_basin_summary <- merge(swe_basin_summary, swe_basin_current[, c("basin", "mon", "swe")])
    # calculate relative swe
    swe_basin_summary$swe_relative <- swe_basin_summary$swe / swe_basin_summary$swe_median
    # round all values
    swe_basin_summary <- swe_basin_summary %>%
      dplyr::mutate(dplyr::across(
        c("swe_max", "swe_min", "swe_median", "swe", "swe_relative"),
        \(x) round(x, 2)
      ))
    swe_basin <- swe_basin_summary
  }
  
  # Write csv if csv = TRUE
  if (csv) {
    utils::write.csv(swe_basin, file = paste0("SweBasin_", year, "-0", month, ".csv"), row.names = FALSE)
  }
  return(swe_basin)
}



