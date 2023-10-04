#' Calculates snow survey stats for basins
#'
#' @description
#' `r lifecycle::badge('stable')`
#' The purpose of this script is to summarise the swe data of each basin for a particular year and month and compare to previous years. It is used for the snow bulletin, specifically the SWE map and the plot B. It is meant to replace Ellen Ward's code from 2020-04-16, r script called swe_compiled_basin.R.

#' @param year The year of interest. If summarise = TRUE, the stats will be calculated based on all years prior to 'year'. If summarise = FALSE, only data from the current year and before are taken.
#' @param month The month of interest. Options are 3, 4 and 5 for March, April and May, respectively. Can also give multiple months as a vector. Historical stats are given for the first day of this month.
#' @param threshold A number between 1 and 10 giving the threshold below which the SWE for that basin and year are ignored. These numbers represent the sum of the factors of the stations for a basin which are not missing data for that year. 10 means that the swe values calculated from less than all the stations of that basin are ignored. 1 means that only the swe calculated from less than 1 out of 10 are ignored.
#' @param summarise Summarises the data into a dataframe with the current SWE, historical median, the swe relative to the median (swe / swe_median), historical maximum, historical minimum, and year of maximum and minimum for each basin.
#' @param csv TRUE or FALSE. If TRUE, a csv will be created.
#'
#' @return A table and a csv file (if csv = TRUE) with either (summarise = FALSE) the swe for all basins, years and months of interest or (summarise = TRUE) the current SWE, historical median, the swe relative to the median (swe / swe_median), historical maximum, historical minimum, and year of maximum and minimum for each basin and month of interest.
#' @export

#TODO: 

# For testing
# year <- 2022
# month <- c(3, 4, 5) # c(3) 
# threshold <- 6


SWE_basin <-
  function(year,
           month,
           threshold = 7,
           csv = FALSE,
           summarise = FALSE) {
    ### Retrieve data from db
    con <- hydroConnect()
    Meas <-
      DBI::dbGetQuery(con,
                      "SELECT location, value, target_date FROM discrete WHERE parameter = 'SWE'")
    DBI::dbDisconnect(con)
    # Rename columns:
    colnames(Meas) <- c("location_id", "SWE", "target_date")
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
    Meas <- Meas %>% dplyr::filter(mon %in% month & day == 1)
    
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
      tab <- Meas %>% dplyr::filter(yr == yr_mon[ym,1] & mon == yr_mon[ym,2])
      # Go through each basin one by one
      for (b in basins) {
        # subset factors to only what we need
        fact <- Factors %>% dplyr::select(tidyselect::all_of(c(b, "location_id"))) %>%
          dplyr::rename(val = b) %>% dplyr::filter(val != 0)
        # Go through each location one by one
        sweb <- stats::setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                         c("location_id", "swe", "perc"))
        for (l in fact$location_id) {
          # Check if location has measurement in tab
          if (length(tab[tab$location_id == l,]$SWE) == 0 |
              is.null(tab[tab$location_id == l,]$SWE)) {
            # Create vector with location, swe, percentage of basin that it represents. 0 if value is missing
            swe <- c(l,
                     NA,
                     0)
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
        perc <- sum(as.numeric(sweb$perc))
        if (perc > 10) {
          swe <- sum(sweb$swe)
          warning(paste0(
            "Factors for location ",
            l,
            " add up to ",
            perc,
            " for the year ",
            ym
          ))
        } else if (perc == 10) {
          swe <- sum(as.numeric(sweb$swe), na.rm = TRUE)
        } else {
          swe <- sum(as.numeric(sweb$swe), na.rm = TRUE) * 10 / perc
          warning(paste0(
            "Factors for location ",
            l,
            " add up to ",
            perc,
            " for the year ",
            ym
          ))
        }
        swe_basin_year[nrow(swe_basin_year) + 1, ] = c(b, yr_mon[ym,1], yr_mon[ym,2], swe, perc)
      }
      swe_basin_year[nrow(swe_basin_year) + 1, ] = swe_basin_year
    }
    
    # Set column classes
    swe_basin_year$yr <- as.numeric(swe_basin_year$yr)
    swe_basin_year$swe <- as.numeric(swe_basin_year$swe)
    swe_basin_year$perc <- as.numeric(swe_basin_year$perc)
    
    # Remove years based on percentage of basin stations with measurements
    swe_basin_year <- swe_basin_year %>% dplyr::filter(perc >= threshold)
    swe_basin <- swe_basin_year
    
    # Add units and parameter
    swe_basin$parameter <- rep("SWE", length(swe_basin$basin))
    swe_basin$units <- rep("mm", length(swe_basin$basin))
    # rename columns
    colnames(swe_basin) <- c("location", "year", "month", "value", "perc", "parameter", "units")
    
    ## Calculate max, min and median historical SWE for each basin if summarise = TRUE
    if (summarise == TRUE) {
      # Get current year values
      swe_basin_current <- swe_basin_year %>%
        dplyr::filter(yr == year)
      # calculate stats excluding current year
      swe_basin_summary <- swe_basin_year %>%
        dplyr::filter(yr != year) %>%
        dplyr::group_by(basin, mon) %>%
        dplyr::summarize(
          swe_max = max(swe),
          year_max = yr[which.max(swe)],
          swe_min = min(swe),
          year_min = yr[which.min(swe)],
          swe_median = stats::median(swe)
        )
      # combine tables
      swe_basin_summary <-
        merge(swe_basin_summary, swe_basin_current[, c("basin", "mon", "swe")])
      # calculate relative swe
      swe_basin_summary$swe_relative <-
        swe_basin_summary$swe / swe_basin_summary$swe_median
      # round all values
      swe_basin_summary <- swe_basin_summary %>%
        dplyr::mutate(dplyr::across(
          c("swe_max", "swe_min", "swe_median", "swe", "swe_relative"),
          \(x) round (x, 2)
        ))
      swe_basin <- swe_basin_summary
    }
    
    # Write csv if csv = TRUE
    if (csv == TRUE) {
      utils::write.csv(swe_basin, file = paste0("SweBasin_", year, "-0", month, ".csv"), row.names = FALSE)
    }
    return(swe_basin)
  }



