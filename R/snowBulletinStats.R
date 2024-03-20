#' Function for creating snow bulletin statistics - internal
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function generates the statistics associated with the snow bulletin for a specified date. The output is list of tables (in R), or a number of excel workbooks.
#'
#' This function fetches data from the local postgresql hydrometric database created/maintained by the HydroMetDB package.
#'
#' @details
#' To download data, you MUST have your hydromet credentials loaded
#' into your .Renviron profile as values pairs of hydrometHost="10.250.12.154", hydrometPort="5433", hydrometUser="hydromet_read", hydrometPass="hydromet".
#'
#' @param year Year for which the snow bulletin stats are calculated.
#' @param month Month for which the snow bulletin stats are calculated. Options are 3, 4 or 5.
#' @param basins The name of the sub_basin you wish to generate stats for. One or many of "Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek". North Slope will be added when hydromet is updated with the new snow database. Default is NULL, where all basins are shown in bulletin.
#' @param excel_output If TRUE, calculated stats will be outputted in multiple excel tables. 
#' @param save_path The path to the directory (folder) where the excel files should be saved. Enter the path as a character string.
#' @param synchronize Should the timeseries be synchronized with source data? If TRUE, all timeseries used in the snow bulletin will be synchronized. If FALSE (default), none will be synchronized. 
#'
#' @return A snow bulletin in Microsoft Word format.
#'
#' @export
#'

snowBulletinStats <-
  function(year,
           month,
           basins = NULL,
           excel_output,
           save_path,
           synchronize=FALSE) {
    
    con <- hydrometConnect()
    year <- 2024
    month <- 3
    
    #### ----------------- Pillows with historical record ----------------- ####
      pillow_stats <- DBI::dbGetQuery(con, paste0("SELECT locations.name, locations.location, date,  
                    parameters.param_name, value, q50, max, min FROM calculated_daily 
                    INNER JOIN timeseries ON calculated_daily.timeseries_id = timeseries.timeseries_id
                    INNER JOIN locations ON timeseries.location = locations.location
                    INNER JOIN parameters ON timeseries.parameter = parameters.param_code
                    WHERE calculated_daily.timeseries_id IN (20, 145, 51, 75, 122, 85, 649)
                    AND date = '", year, "-0", month, "-01'"))
      pillow_stats$perc_hist_med <- round(pillow_stats$value / pillow_stats$q50 * 100)
    
    #### -----------------------  SWE stations ---------------------------- ####
      station_stats <- SWE_station(stations="all", year=year, month=month, return_missing = TRUE, 
                  active = TRUE, source="hydromet", summarise=TRUE)
    
    #### ---------------- Pillows with snow survey record ----------------- ####
      # Log Cabin
      pillow_stats[pillow_stats$location == "09AA-M2", ]$q50 <- station_stats[station_stats$location_id == "09AA-SC03",]$swe_med
      pillow_stats[pillow_stats$location == "09AA-M2", ]$max <- station_stats[station_stats$location_id == "09AA-SC03",]$swe_max
      pillow_stats[pillow_stats$location == "09AA-M2", ]$min <- station_stats[station_stats$location_id == "09AA-SC03",]$swe_min
      pillow_stats[pillow_stats$location == "09AA-M2", ]$perc_hist_med <- (pillow_stats[pillow_stats$location == "09AA-M2", ]$value /
                                                                          pillow_stats[pillow_stats$location == "09AA-M2", ]$q50) * 100
      # King Solomon Dome
      pillow_stats[pillow_stats$location == "09EA-M1", ]$q50 <- station_stats[station_stats$location_id == "09EA-SC01",]$swe_med
      pillow_stats[pillow_stats$location == "09EA-M1", ]$max <- station_stats[station_stats$location_id == "09EA-SC01",]$swe_max
      pillow_stats[pillow_stats$location == "09EA-M1", ]$min <- station_stats[station_stats$location_id == "09EA-SC01",]$swe_min
      pillow_stats[pillow_stats$location == "09EA-M1", ]$perc_hist_med <- (pillow_stats[pillow_stats$location == "09EA-M1", ]$value /
                                                                             pillow_stats[pillow_stats$location == "09EA-M1", ]$q50) * 100
    #### --------------------------- Basins ------------------------------- ####
        basin_stats <- SWE_basin(year = year,
                                 month = month,
                                 threshold = 6,
                                 csv = FALSE,
                                 summarise = TRUE,
                                 source = "hydromet")
        basin_stats$perc_hist_med <- basin_stats$swe_relative *100
        
    #### ------------------------- Monthly precip ------------------------- ####
        precipStats <- function() {
          tsid <- c(663, 665, 666, 668, 664, 671, 667)
          # Should also bring in location ID and name
          tab <- DBI::dbGetQuery(con, paste0("SELECT locations.name, locations.location, datetime, value
                                           FROM measurements_continuous 
                                           INNER JOIN timeseries ON measurements_continuous.timeseries_id = timeseries.timeseries_id
                                           INNER JOIN locations ON timeseries.location = locations.location
                                           WHERE measurements_continuous.timeseries_id IN ('", paste0(tsid, collapse="', '"),
                                             "')"))
          #AND datetime >= '", year_param-40, "-10-01'"))
          attr(tab$datetime, "tzone") <- "MST"
          tab$date <- as.Date(tab$datetime - lubridate::days(1))
          tab$month <- format(tab$datetime, "%m")
          tab$year <- format(tab$datetime, "%Y")
          
          # Only keep Oct to month of interest
          if (month == 3) {
            tab <- tab[tab$month %in% c(10, 11, 12, '01', '02'),]
          } else if (month_param == 4) {
            tab <- tab[tab$month %in% c(10, 11, 12, '01', '02', '03'),]
          } else if (month_param == 5) {
            tab <- tab[tab$month %in% c(10, 11, 12, '01', '02', '03', '04'),]
          }
          
          # Calculate Oct-month of interest cumulative precip for each loc and year
          # Add fake_year. Where month is 1,2,3,4, change year to year - 1
          tab$fake_year <- NA
          tab[tab$month %in% c("01", "02", "03", "04"),]$fake_year <- as.numeric(tab[tab$month %in% c("01", "02", "03", "04"),]$year)
          tab[tab$month %in% c("10", "11", "12"),]$fake_year <- as.numeric(tab[tab$month %in% c("10", "11", "12"),]$year) + 1 
          
          precip_years <- tab %>%
            dplyr::group_by(.data$fake_year, .data$location, .data$name) %>%
            dplyr::summarise(value = sum(.data$value), 
                             count = dplyr::n(),
                             type = "sum")
          # Only keep those with all 5, 6 or 7 months (depending on month_param)
          precip_years <- precip_years[precip_years$count == 2+month,]
          
          # Get value from year of interest
          tab_yr <- precip_years[precip_years$fake_year == year, ]
          # Remove year of interest from precip_years
          precip_years <- precip_years[precip_years$fake_year != year, ]
          
          ### Calculate stats
          # Calculate stats. For entire period of record
          precip_allyrs <- precip_years %>%
            dplyr::group_by(.data$location, .data$name) %>%
            dplyr::summarise(value = min(.data$value), type = "min") %>%
            dplyr::bind_rows(precip_years %>%
                               dplyr::group_by(.data$location, .data$name) %>%
                               dplyr::summarise(value = max(.data$value), type = "max")) %>%
            dplyr::bind_rows(precip_years %>%
                               dplyr::group_by(.data$location, .data$name) %>%
                               dplyr::summarise(value = stats::median(.data$value), type = "median"))
          # Add period column
          precip_allyrs$period <- paste0("All years")
          
          # Calculate stats. For last 40 years
          precip_40yrs <- precip_years[precip_years$fake_year > year-40,]
          precip_40yrs <- precip_40yrs %>%
            dplyr::group_by(.data$location, .data$name) %>%
            dplyr::summarise(value = min(.data$value), type = "min") %>%
            dplyr::bind_rows(precip_40yrs %>%
                               dplyr::group_by(.data$location, .data$name) %>%
                               dplyr::summarise(value = max(.data$value), type = "max")) %>%
            dplyr::bind_rows(precip_40yrs %>%
                               dplyr::group_by(.data$location, .data$name) %>%
                               dplyr::summarise(value = stats::median(.data$value), type = "median"))
          # Add period column
          precip_40yrs$period <- paste0("Last 40 years")
          
          # Calculate stats. For last 1991-2020 period
          precip_9120 <- precip_years[precip_years$fake_year >= 1991 & 
                                        precip_years$fake_year <= 2020,]
          precip_9120 <- precip_9120 %>%
            dplyr::group_by(.data$location, .data$name) %>%
            dplyr::summarise(value = min(.data$value), type = "min") %>%
            dplyr::bind_rows(precip_9120 %>%
                               dplyr::group_by(.data$location, .data$name) %>%
                               dplyr::summarise(value = max(.data$value), type = "max")) %>%
            dplyr::bind_rows(precip_9120 %>%
                               dplyr::group_by(.data$location, .data$name) %>%
                               dplyr::summarise(value = stats::median(.data$value), type = "median"))
          # Add period column
          precip_9120$period <- paste0("1991-2020")
          
          # Calculate stats. For last 1981-2010 period
          precip_8110 <- precip_years[precip_years$fake_year >= 1981 & 
                                        precip_years$fake_year <= 2010,]
          precip_8110 <- precip_8110 %>%
            dplyr::group_by(.data$location, .data$name) %>%
            dplyr::summarise(value = min(.data$value), type = "min") %>%
            dplyr::bind_rows(precip_8110 %>%
                               dplyr::group_by(.data$location, .data$name) %>%
                               dplyr::summarise(value = max(.data$value), type = "max")) %>%
            dplyr::bind_rows(precip_8110 %>%
                               dplyr::group_by(.data$location, .data$name) %>%
                               dplyr::summarise(value = stats::median(.data$value), type = "median"))
          # Add period column
          precip_8110$period <- paste0("1981-2010")
          
          ## Bind all together
          precip_stats <- rbind(precip_40yrs, precip_9120, precip_8110, precip_allyrs)
          
          # Reshape to wide format
          precip_stats <- tidyr::pivot_wider(precip_stats, names_from = c(type), values_from = value)
          # Bind current year data
          precip_stats <- merge(precip_stats, tab_yr[, c("location", "name", "value")])
          # Calculate percent historical
          precip_stats$perc_hist_med <- round(precip_stats$value / precip_stats$median *100, 0)
          # Add variable coloumn
          precip_stats$variable <- paste0("Oct - ", month.abb[month-1], " cumulative precipitation")
          
          return(precip_stats)
        }
        
        precip_stats <- precipStats()
        
        
        
    
    #### ------------------------------ CDDF ------------------------------ ####
        getCDDF <- function(temps, year) {
          
          # Function for calculating cddf of dataframe (with all dates of interest)
          calcCDDF <- function(temps) {
            cddf <- 0
            temps$cddf <- NA
            for (d in 1:length(temps$value)) {
              t <- temps$value[d]
              # If temperature is NA
              if (is.na(t)) {
                t <- 0
              }
              # If yesterday's cddf is 0 and todays temp is >= 0, keep cddf at 0
              if (cddf==0 & t>=0) {
                cddf <- 0
              } else { 
                cddf <- cddf - t
                if (cddf<0){cddf <- 0}}
              # Set cddf for that day
              temps$cddf[d] <- cddf
            }
            return(temps)
          } 
          
          
          # Keep only sept-june data
          temps <- temps[format(temps$datetime, "%m") %in% c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06"),]
          
          # Find first and last year 
          first_yr <- format(min(temps$datetime), "%Y")
          last_yr <- format(max(temps$datetime), "%Y")
          
          # if (last_yr <= year) {
          #   last_yr <- year 
          # }
          
          cddf <- data.frame()
          # Run over every year
          for (y in first_yr:last_yr) {
            # Subset data
            tab <- temps[temps$datetime >= paste0(y, '-09-01') 
                         & temps$datetime < paste0(y+1, '-06-14'),]
            # Only calculate if missing less than 10 days, but only for years that are not in the 'years' list
            if (length(tab$datetime) != 0) {
              if (sum(!is.na(tab$value)) >= 276 | format(min(tab$datetime), "%Y") %in% c(year-1)) {
                cddf_y <- calcCDDF(tab)
                cddf <- rbind(cddf, cddf_y)
              }
            }
          }
          
          # Rename columns and remove temp
          cddf <- cddf[,c("datetime", "cddf")]
          colnames(cddf) <- c("datetime", "value")
          
          return(cddf)
        }
        
        tsid <- c(484, 532, 540, 500, 548, 492, 556, 508)
        tabl <- DBI::dbGetQuery(con, paste0("SELECT locations.name, locations.location, datetime, value 
                                                FROM measurements_continuous 
                                                INNER JOIN timeseries ON measurements_continuous.timeseries_id = timeseries.timeseries_id
                                                INNER JOIN locations ON timeseries.location = locations.location
                                                WHERE measurements_continuous.timeseries_id IN ('", paste0(tsid, collapse="', '"),
                                                "')"))# ", timeseries_id))
        
        attr(tabl$datetime, "tzone") <- "MST"
        
        cddf <- data.frame()
        for (l in unique(tabl$location)) {
          tab <- tabl[tabl$location == l, ]
          cddf_l <- getCDDF(tab, year_param)
          cddf_l$location <- l
          cddf_l$name <- unique(tab$name)
          cddf <- rbind(cddf, cddf_l)
        }
        
        cddf$value <- round(cddf$value, 0)
        
        
        
  }




