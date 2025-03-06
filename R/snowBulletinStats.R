#' Function for creating snow bulletin statistics - internal
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function generates the statistics associated with the snow bulletin for a specified date. The output is a list of tables (in R), or a number of excel workbooks. Data is fetched from the local AquaCache database created/maintained by the AquaCache package OR from the snow database.
#'
#' @details
#' To download data, you MUST have your aquacache credentials added to your .Renviron file. See function [AquaConnect()] or talk to the database administrator/data scientist for help.
#'
#' @param year Year for which the snow bulletin stats are calculated.
#' @param month Month for which the snow bulletin stats are calculated. Options are 3, 4 or 5 as a vector of length 1.
#' @param basins The name of the sub_basin you wish to generate stats for. One or more of "Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek" as a character vector, or NULL for all basins. North Slope will be added when AquaCache is updated with the new snow database.
#' @param excel_output If TRUE, calculated stats will be output in multiple excel tables. 
#' @param save_path The path to the directory (folder) where the excel files should be saved. Enter the path as a character string or leave as 'choose' to select interactively. Not used if excel_output = FALSE.
#' @param synchronize Should the timeseries be synchronized with source data? If TRUE, all timeseries used in this function will be synchronized. If FALSE (default), none will be synchronized. Currently is a placeholder and does not work.
#' @param source Should the SWE statistics for stations (station_stats) be calculated from the 'aquacache' database or the 'snow' database? Default is 'aquacache'.
#' @param con A connection to the database. Leave as NULL to use function [AquaConnect()] with default settings and close the connection automatically afterwards.
#'
#' @return A list of data.frames OR, if requested, three Excel workbooks.
#'
#' @export
#'

snowBulletinStats <- function(year,
                              month,
                              basins = NULL,
                              excel_output = FALSE,
                              save_path = "choose",
                              synchronize = FALSE,
                              source = "aquacache",
                              con = NULL) {
  
  # Check parameters are valid
  if (!month %in% c(3, 4, 5)) {
    stop("Month must be 3, 4 or 5.")
  }
  source <- tolower(source)
  if (!source %in% c("aquacache", "snow")) {
    stop("Source must be 'aquacache' or 'snow'.")
  }
  if (!is.null(basins)) {
    if (!all(basins %in% c("Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek"))) {
      stop("Basins must be one or more of 'Upper Yukon', 'Teslin', 'Central Yukon', 'Pelly', 'Stewart', 'White', 'Lower Yukon', 'Porcupine', 'Peel', 'Liard', 'Alsek'.")
    }
  }
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  # Set up save_path
  if (excel_output) {
    if (save_path == "choose") {
      if (!interactive()) {
        stop("You must specify a save path when running in non-interactive mode.")
      }
      message("Select the path to the folder where you want the workbook(s) saved.")
      save_path <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"),"Desktop"))
    } else {
      if (!dir.exists(save_path)) {
        stop("The save path you specified does not exist.")
      }
    }
  }
  
  
  #### ---------------------------- Functions --------------------------- ####
  precipStats <- function() {
    tsid <- c(663, 665, 666, 668, 664, 671, 667)
    # Should also bring in location ID and name
    tab <- DBI::dbGetQuery(con, paste0("SELECT locations.name AS location_name, 
                                           locations.location AS location_id, 
                                           datetime, value_corrected AS value
                                           FROM measurements_continuous_corrected 
                                           INNER JOIN timeseries ON measurements_continuous_corrected.timeseries_id = timeseries.timeseries_id
                                           INNER JOIN locations ON timeseries.location = locations.location
                                           WHERE measurements_continuous_corrected.timeseries_id IN ('", paste0(tsid, collapse = "', '"),
                                       "')"))
    #AND datetime >= '", year_param-40, "-10-01'"))
    attr(tab$datetime, "tzone") <- "MST"
    tab$date <- as.Date(tab$datetime - lubridate::days(1))
    tab$month <- format(tab$datetime, "%m")
    tab$year <- format(tab$datetime, "%Y")
    
    # Only keep Oct to month of interest
    if (month == 3) {
      tab <- tab[tab$month %in% c(10, 11, 12, '01', '02'),]
    } else if (month == 4) {
      tab <- tab[tab$month %in% c(10, 11, 12, '01', '02', '03'),]
    } else if (month == 5) {
      tab <- tab[tab$month %in% c(10, 11, 12, '01', '02', '03', '04'),]
    }
    
    # Calculate Oct-month of interest cumulative precip for each loc and year
    # Add fake_year. Where month is 1,2,3,4, change year to year - 1
    tab$fake_year <- NA
    tab[tab$month %in% c("01", "02", "03", "04"),]$fake_year <- as.numeric(tab[tab$month %in% c("01", "02", "03", "04"),]$year)
    tab[tab$month %in% c("10", "11", "12"),]$fake_year <- as.numeric(tab[tab$month %in% c("10", "11", "12"),]$year) + 1 
    
    precip_years <- tab %>%
      dplyr::group_by(.data$fake_year, .data$location_id, .data$location_name) %>%
      dplyr::summarise(value = sum(.data$value), 
                       count = dplyr::n(),
                       type = "sum")
    
    # Only keep those with all 5, 6 or 7 months (depending on month_param)
    precip_years <- precip_years[precip_years$count == 2 + month,]
    
    # Get value from year of interest
    tab_yr <- precip_years[precip_years$fake_year == year, ]
    # Remove year of interest from precip_years
    precip_years <- precip_years[precip_years$fake_year != year, ]
    
    ### Calculate stats
    # Calculate stats. For entire period of record
    precip_allyrs <- precip_years %>%
      dplyr::group_by(.data$location_id, .data$location_name) %>%
      dplyr::summarise(value = min(.data$value), type = "min", years = dplyr::n()) %>%
      dplyr::bind_rows(precip_years %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = max(.data$value), type = "max",
                                          years = dplyr::n())) %>%
      dplyr::bind_rows(precip_years %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = stats::median(.data$value), 
                                          type = "median", years = dplyr::n()))
    # Add period column
    precip_allyrs$period <- paste0("all years")
    
    # Calculate stats. For last 40 years
    precip_40yrs <- precip_years[precip_years$fake_year > year - 40,]
    precip_40yrs <- precip_40yrs %>%
      dplyr::group_by(.data$location_id, .data$location_name) %>%
      dplyr::summarise(value = min(.data$value), type = "min", years = dplyr::n()) %>%
      dplyr::bind_rows(precip_40yrs %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = max(.data$value), type = "max", 
                                          years = dplyr::n())) %>%
      dplyr::bind_rows(precip_40yrs %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = stats::median(.data$value), 
                                          type = "median", years = dplyr::n()))
    # Add period column
    precip_40yrs$period <- paste0("last 40 years")
    
    # Calculate stats. For last 1991-2020 period
    precip_9120 <- precip_years[precip_years$fake_year >= 1991 & 
                                  precip_years$fake_year <= 2020,]
    precip_9120 <- precip_9120 %>%
      dplyr::group_by(.data$location_id, .data$location_name) %>%
      dplyr::summarise(value = min(.data$value), type = "min", years = dplyr::n()) %>%
      dplyr::bind_rows(precip_9120 %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = max(.data$value), type = "max", 
                                          years = dplyr::n())) %>%
      dplyr::bind_rows(precip_9120 %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = stats::median(.data$value), 
                                          type = "median", years = dplyr::n()))
    # Add period column
    precip_9120$period <- paste0("1991-2020")
    
    # Calculate stats. For last 1981-2010 period
    precip_8110 <- precip_years[precip_years$fake_year >= 1981 & 
                                  precip_years$fake_year <= 2010,]
    precip_8110 <- precip_8110 %>%
      dplyr::group_by(.data$location_id, .data$location_name) %>%
      dplyr::summarise(value = min(.data$value), type = "min", years = dplyr::n()) %>%
      dplyr::bind_rows(precip_8110 %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = max(.data$value), type = "max", 
                                          years = dplyr::n())) %>%
      dplyr::bind_rows(precip_8110 %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = stats::median(.data$value), 
                                          type = "median", years = dplyr::n()))
    # Add period column
    precip_8110$period <- paste0("1981-2010")
    
    ## Bind all together
    precip_stats <- rbind(precip_40yrs, precip_9120, precip_8110, precip_allyrs)
    
    # Reshape to wide format
    precip_stats <- tidyr::pivot_wider(precip_stats, names_from = c("type"), values_from = "value")
    # Bind current year data
    if (nrow(tab_yr) == 0) {
      precip_stats$value <- NA
    } else {
      precip_stats <- merge(precip_stats, tab_yr[, c("location_id", "location_name", "value")])
    }
    
    # Calculate percent historical
    precip_stats$perc_hist_med <- round(precip_stats$value / precip_stats$median * 100, 0)
    # Add variable coloumn
    precip_stats$variable <- paste0("Oct - ", month.abb[month - 1], " cumulative precipitation")
    # Add description of % median
    precip_stats <- precip_stats %>%
      dplyr::mutate(description = dplyr::case_when(
        perc_hist_med < 66 ~ "well below",
        perc_hist_med >= 66 & perc_hist_med < 90 ~ "below",
        perc_hist_med >= 98 & perc_hist_med < 103 ~ "",
        perc_hist_med >= 90 & perc_hist_med < 110 ~ "close to",
        perc_hist_med >= 110 & perc_hist_med < 135 ~ "above",
        perc_hist_med >= 135 ~ "well above",
        # Add more conditions as needed
        TRUE ~ NA_character_  # This acts as an 'else' statement to catch all other cases
      ))
    # Reorder columns
    precip_stats <- precip_stats[, c("location_name", "location_id", "variable", "period", "value",
                                     "median", "min", "max", "perc_hist_med", "description", "years")]
    
    return(precip_stats)
  }
  getCDDF <- function(temps, year) {
    
    # Function for calculating cddf of dataframe (with all dates of interest)
    calcCDDF <- function(temps) {
      cddf <- 0
      temps$cddf <- NA
      # Order temps by date
      temps <- temps[order(temps$datetime),]
      for (d in 1:length(temps$value)) {
        t <- temps$value[d]
        # If temperature is NA
        if (is.na(t)) {
          t <- 0
        }
        # If yesterday's cddf is 0 and todays temp is >= 0, keep cddf at 0
        if (cddf == 0 & t >= 0) {
          cddf <- 0
        } else { 
          cddf <- cddf - t
          if (cddf < 0) {cddf <- 0}}
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
                   & temps$datetime < paste0(y + 1, '-06-14'),]
      # Only calculate if missing less than 10 days, but only for years that are not in the 'years' list
      if (length(tab$datetime) != 0) {
        if (sum(!is.na(tab$value)) >= 276 | format(min(tab$datetime), "%Y") %in% c(year - 1)) {
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
  cddfStats <- function(month, year) {
    tsid <- c(484, 532, 540, 500, 548, 492, 556, 508)
    tabl <- DBI::dbGetQuery(con, paste0("SELECT locations.name AS location_name, 
                                                locations.location AS location_id,
                                                datetime, value_corrected AS value
                                                FROM measurements_continuous_corrected 
                                                INNER JOIN timeseries ON measurements_continuous_corrected.timeseries_id = timeseries.timeseries_id
                                                INNER JOIN locations ON timeseries.location = locations.location
                                                WHERE measurements_continuous_corrected.timeseries_id IN ('", paste0(tsid, collapse = "', '"),
                                        "')"))
    
    attr(tabl$datetime, "tzone") <- "MST"
    
    cddf <- data.frame()
    for (l in unique(tabl$location_id)) {
      tab <- tabl[tabl$location_id == l, ]
      cddf_l <- getCDDF(tab, year)
      cddf_l$location_id <- l
      cddf_l$location_name <- unique(tab$location_name)
      cddf <- rbind(cddf, cddf_l)
    }
    
    cddf$value <- round(cddf$value, 0)
    
    # Add month, year columns
    cddf$date <- as.Date(cddf$datetime - lubridate::days(1))
    cddf$day <- format(cddf$datetime, "%d")
    cddf$month <- format(cddf$datetime, "%m")
    cddf$year <- format(cddf$datetime, "%Y")
    
    # Only Keep CDDF for first day of month of interest
    cddf <- cddf[cddf$month == paste0("0", month) & cddf$day == "01", ]
    
    # Get value from year of interest
    cddf_yr <- cddf[cddf$year == year, ]
    # Remove year of interest from precip_years
    cddf <- cddf[cddf$year != year, ]
    
    ### Calculate stats
    # Calculate stats. For entire period of record
    cddf_allyrs <- cddf %>%
      dplyr::group_by(.data$location_id, .data$location_name) %>%
      dplyr::summarise(value = min(.data$value), type = "min", years = dplyr::n()) %>%
      dplyr::bind_rows(cddf %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = max(.data$value), type = "max", 
                                          years = dplyr::n())) %>%
      dplyr::bind_rows(cddf %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = stats::median(.data$value), 
                                          type = "median", years = dplyr::n()))
    # Add period column
    cddf_allyrs$period <- paste0("all years")
    
    # Calculate stats. For last 40 years
    cddf_40yrs <- cddf[cddf$year > year - 40,]
    cddf_40yrs <- cddf_40yrs %>%
      dplyr::group_by(.data$location_id, .data$location_name) %>%
      dplyr::summarise(value = min(.data$value), type = "min", years = dplyr::n()) %>%
      dplyr::bind_rows(cddf_40yrs %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = max(.data$value), type = "max", 
                                          years = dplyr::n())) %>%
      dplyr::bind_rows(cddf_40yrs %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = stats::median(.data$value), 
                                          type = "median", years = dplyr::n()))
    # Add period column
    cddf_40yrs$period <- paste0("last 40 years")
    
    # Calculate stats. For last 1991-2020 period
    cddf_9120 <- cddf[cddf$year >= 1991 & 
                        cddf$year <= 2020,]
    cddf_9120 <- cddf_9120 %>%
      dplyr::group_by(.data$location_id, .data$location_name) %>%
      dplyr::summarise(value = min(.data$value), type = "min", years = dplyr::n()) %>%
      dplyr::bind_rows(cddf_9120 %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = max(.data$value), type = "max", 
                                          years = dplyr::n())) %>%
      dplyr::bind_rows(cddf_9120 %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = stats::median(.data$value), 
                                          type = "median", years = dplyr::n()))
    # Add period column
    cddf_9120$period <- paste0("1991-2020")
    
    # Calculate stats. For last 1981-2010 period
    cddf_8110 <- cddf[cddf$year >= 1981 & 
                        cddf$year <= 2010,]
    cddf_8110 <- cddf_8110 %>%
      dplyr::group_by(.data$location_id, .data$location_name) %>%
      dplyr::summarise(value = min(.data$value), type = "min", years = dplyr::n()) %>%
      dplyr::bind_rows(cddf_8110 %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = max(.data$value), type = "max", 
                                          years = dplyr::n())) %>%
      dplyr::bind_rows(cddf_8110 %>%
                         dplyr::group_by(.data$location_id, .data$location_name) %>%
                         dplyr::summarise(value = stats::median(.data$value), 
                                          type = "median", years = dplyr::n()))
    # Add period column
    cddf_8110$period <- paste0("1981-2010")
    
    ## Bind all together
    cddf_stats <- rbind(cddf_40yrs, cddf_9120, cddf_8110, cddf_allyrs)
    
    # Reshape to wide format
    cddf_stats <- tidyr::pivot_wider(cddf_stats, names_from = c("type"), values_from = "value")
    # Bind current year data
    if (nrow(cddf_yr) == 0) {
      cddf_stats$value <- NA
    } else {
      cddf_stats <- merge(cddf_stats, cddf_yr[, c("location_id", "location_name", "value")])
    }
    # Calculate percent historical
    cddf_stats$perc_hist_med <- round(cddf_stats$value / cddf_stats$median * 100, 0)
    # Add variable column
    cddf_stats$variable <- paste0(month.name[month], " 1st CDDF")
    # Reorder columns
    cddf_stats <- cddf_stats[, c("location_name", "location_id", "variable", "period", "value",
                                 "median", "min", "max", "perc_hist_med", "years")]
    
    return(cddf_stats)
  }
  
  #### ----------------- Pillows with historical record ----------------- ####
  pillow_stats <- DBI::dbGetQuery(con, paste0(
    "WITH latest_measurements AS (
  SELECT 
    l.name AS location_name,
    l.location AS location_id,
    m.date,
    p.param_name AS variable,
    m.value,
    m.q50 AS median,
    m.min,
    m.max,
    m.doy_count AS years,
    m.timeseries_id,
    ROW_NUMBER() OVER (PARTITION BY m.timeseries_id ORDER BY m.date DESC) AS rn
  FROM measurements_calculated_daily_corrected m
    INNER JOIN timeseries t ON m.timeseries_id = t.timeseries_id
    INNER JOIN locations l ON t.location = l.location
    INNER JOIN parameters p ON t.parameter_id = p.parameter_id
  WHERE m.timeseries_id IN (20, 145, 51, 75, 122, 85, 649)
    AND m.date BETWEEN ('", year, "-0", month, "-01'::date - INTERVAL '7 day') AND '", year, "-0", month, "-01'
)
SELECT location_name, location_id, date, variable, value, median, min, max, years
FROM latest_measurements
WHERE rn = 1;"
  ))
  pillow_stats$perc_hist_med <- round(pillow_stats$value / pillow_stats$median * 100)
  
  #### -----------------------  SWE stations ---------------------------- ####
  station_stats <- SWE_station(stations = "all", 
                               year = year, 
                               month = month, 
                               return_missing = TRUE, 
                               active = TRUE, 
                               source = source, 
                               summarise = TRUE, 
                               aquaCon = con)  # If source == 'aquacache', the (mandatory) snow DB connection will by default use the same ip and host as the aquacache connection
  # Remove 'Snow Course' from name
  
  #### ---------------- Pillows with snow survey record ----------------- ####
  # Log Cabin
  if (nrow(pillow_stats[pillow_stats$location_id == "09AA-M2", ]) > 0) {
    pillow_stats[pillow_stats$location_id == "09AA-M2", ]$median <- station_stats[station_stats$location_id == "09AA-SC03",]$swe_med
    pillow_stats[pillow_stats$location_id == "09AA-M2", ]$max <- station_stats[station_stats$location_id == "09AA-SC03",]$swe_max
    pillow_stats[pillow_stats$location_id == "09AA-M2", ]$min <- station_stats[station_stats$location_id == "09AA-SC03",]$swe_min
    pillow_stats[pillow_stats$location_id == "09AA-M2", ]$perc_hist_med <- round((pillow_stats[pillow_stats$location_id == "09AA-M2", ]$value /
                                                                                    pillow_stats[pillow_stats$location_id == "09AA-M2", ]$median) * 100)
  } else {
    message("Log Cabin does not have pillow stats for this year and month combo")
  }
  
  # King Solomon Dome
  if (nrow(pillow_stats[pillow_stats$location_id == "09EA-M1", ]) > 0) {
    pillow_stats[pillow_stats$location_id == "09EA-M1", ]$median <- station_stats[station_stats$location_id == "09EA-SC01",]$swe_med
    pillow_stats[pillow_stats$location_id == "09EA-M1", ]$max <- station_stats[station_stats$location_id == "09EA-SC01",]$swe_max
    pillow_stats[pillow_stats$location_id == "09EA-M1", ]$min <- station_stats[station_stats$location_id == "09EA-SC01",]$swe_min
    pillow_stats[pillow_stats$location_id == "09EA-M1", ]$perc_hist_med <- round((pillow_stats[pillow_stats$location_id == "09EA-M1", ]$value /
                                                                                    pillow_stats[pillow_stats$location_id == "09EA-M1", ]$median) * 100)
  } else {
    message("King Solomon Dome does not have pillow stats for this year and month combo")
  }
  
  #### --------------------------- Basins ------------------------------- ####
  basin_stats <- SWE_basin(year = year,
                           month = month,
                           threshold = 6,
                           csv = FALSE,
                           summarise = TRUE,
                           source = "aquacache",
                           con = con)
  basin_stats$perc_hist_med <- basin_stats$swe_relative * 100
  basin_stats$swe <- round(basin_stats$swe)
  # Add description of % median
  basin_stats <- basin_stats %>%
    dplyr::mutate(description = dplyr::case_when(
      perc_hist_med < 66 ~ "well below",
      perc_hist_med >= 66 & perc_hist_med < 90 ~ "below",
      perc_hist_med >= 90 & perc_hist_med < 98 ~ "close to",
      perc_hist_med >= 98 & perc_hist_med < 103 ~ "",
      perc_hist_med >= 103 & perc_hist_med < 110 ~ "close to",
      perc_hist_med >= 110 & perc_hist_med < 135 ~ "above",
      perc_hist_med >= 135 ~ "well above",
      # Add more conditions as needed
      TRUE ~ NA_character_  # This acts as an 'else' statement to catch all other cases
    ))
  
  #### ------------------------- Monthly precip ------------------------- ####
  # if (Sys.Date() < paste0(year, "-0", month, "-01")) {
  #   precip_stats <- NULL
  # } else {
  precip_stats <- suppressMessages(precipStats())
  # }
  
  
  #### ------------------------------ CDDF ------------------------------ ####
  cddf_stats <- suppressMessages(cddfStats(month, year))
  
  #### ------------------------- Flow/level stats ----------------------- ####
  
  flow_stats <- DBI::dbGetQuery(con, paste0(
    "WITH latest_measurements AS (
  SELECT 
    l.name AS location_name,
    l.location AS location_id,
    m.date,
    p.param_name AS variable,
    m.value,
    m.q50 AS median,
    m.min,
    m.max,
    m.doy_count AS years,
    m.timeseries_id,
    ROW_NUMBER() OVER (PARTITION BY m.timeseries_id ORDER BY m.date DESC) AS rn
  FROM measurements_calculated_daily_corrected m
    INNER JOIN timeseries t ON m.timeseries_id = t.timeseries_id
    INNER JOIN locations l ON t.location = l.location
    INNER JOIN parameters p ON t.parameter_id = p.parameter_id
  WHERE m.timeseries_id IN (30, 31, 38, 48, 57, 81, 69, 71, 107, 132, 110, 14)
    AND m.date BETWEEN ('", year, "-0", month, "-01'::date - INTERVAL '7 day') AND '", year, "-0", month, "-01'
)
SELECT location_name, location_id, date, variable, value, median, min, max, years
FROM latest_measurements
WHERE rn = 1;"
  ))
  flow_stats$perc_hist_med <- round(flow_stats$value / flow_stats$median * 100)
  # Add description of % median
  flow_stats <- flow_stats %>%
    dplyr::mutate(description = dplyr::case_when(
      perc_hist_med < 66 ~ "well below",
      perc_hist_med >= 66 & perc_hist_med < 90 ~ "below",
      perc_hist_med >= 90 & perc_hist_med < 110 ~ "close to",
      perc_hist_med >= 98 & perc_hist_med < 103 ~ "",
      perc_hist_med >= 110 & perc_hist_med < 135 ~ "above",
      perc_hist_med >= 135 ~ "well above",
      # Add more conditions as needed
      TRUE ~ NA_character_  # This acts as an 'else' statement to catch all other cases
    ))
  
  
  #### ---------------------------- Map stats --------------------------- ####
  
  swe_basin_summary <- basin_stats[, c("basin", "swe_relative")]
  if (nrow(basin_stats) == 0) {
    swe_basin_summary <- swe_basin_summary
  } else {
    swe_basin_summary$Bulletin_Edition <- paste0(year, "-", month.name[month])
    colnames(swe_basin_summary) <- c("SWE_Basin", "RELATIVE_SWE", "Bulletin_Edition")
  }
  
  swe_compiled <- station_stats
  # If swe = 0, swe_med = 0 ---> no snow where median zero
    if (length( swe_compiled[swe_compiled$swe == 0 & swe_compiled$swe_med == 0,]$swe_rat) < 0) {
      swe_compiled[swe_compiled$swe == 0 & swe_compiled$swe_med == 0,]$swe_rat <- "no snow present where historical median is zero"
    }
  # If swe !=0, swe_med = 0  ---> snow where median zero
    if (length(swe_compiled[swe_compiled$swe != 0 & swe_compiled$swe_med == 0,]$swe_rat) > 0) {
      swe_compiled[swe_compiled$swe != 0 & swe_compiled$swe_med == 0,]$swe_rat <- "snow present where historical median is zero"
    }
  # If swe = 0, swe_med != 0 ---> no snow
  if (length(swe_compiled[swe_compiled$swe == 0 & swe_compiled$swe_med != 0,]$swe_rat) > 0) {
    swe_compiled$swe_rat[swe_compiled$swe == 0 & swe_compiled$swe_med != 0] <- "no snow present"
  }
  
  swe_compiled <- swe_compiled[, c("location_name", "swe_rat")]
  swe_compiled$Bulletin_Edition <- paste0(year, "-", month.name[month])
  colnames(swe_compiled) <- c("Snow Course Name", "RATIO", "Bulletin_Edition")
  
  #### -------------------- Bringing it all together -------------------- ####
  
  tables <- list("pillow_stats" = pillow_stats, "station_stats" = station_stats, 
                 "basin_stats" = basin_stats, "precip_stats" = precip_stats, 
                 "cddf_stats" = cddf_stats, "flow_stats" = flow_stats,
                 "swe_basin_summary" = swe_basin_summary, "swe_compiled" = swe_compiled)
  
  if (excel_output) {
    
    ## Main workbook
    wb <- openxlsx::createWorkbook()
    # Loop through the list of tables and add each one as a new sheet
    for (i in 1:5) {
      # Add a new sheet with a name based on the index
      openxlsx::addWorksheet(wb, paste(names(tables)[i]))
      
      # Write the table to the newly added sheet
      openxlsx::writeDataTable(wb, sheet = i, x = tables[[i]])
      
      # Optionally, you can customize the table style here by specifying the `tableStyle` argument in `writeDataTable`
    }
    
    # Save the workbook
    openxlsx::saveWorkbook(wb, paste0(save_path, "/SnowbulletinStats_", year, "-0", month, ".xlsx"), overwrite = TRUE)
    
    ## swe_compiled workbook
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "data")
    openxlsx::writeDataTable(wb, sheet = 1, x = swe_compiled)
    openxlsx::saveWorkbook(wb, paste0(save_path, "/swe_compiled.xlsx"), overwrite = TRUE)
    
    ## swe_basin_summary workbook
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "data")
    openxlsx::writeDataTable(wb, sheet = 1, x = swe_basin_summary)
    openxlsx::saveWorkbook(wb, paste0(save_path, "/swe_basin_summary.xlsx"), overwrite = TRUE)
    
    message("Excel workbooks saved to ", save_path)
    
  } else {
    return(tables)
  }
}




