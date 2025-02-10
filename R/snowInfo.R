#' Plots and tabular data for snow survey locations
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function is intended to facilitate the reporting of snow survey data by compiling basic statistics (years of record, missing years, mean, max, etc.), trend information (Mann-Kendall direction and p-value, Sen's slope), and creating simple plots of SWE, depth, and density for all requested stations. At its most basic (parameters to FALSE or NULL where applicable), the result is a list of two data.frames to the R environment with location metadata and field measurements.
#'
#' @param locations The list of locations requested, as a character vector of length n. Default "all" fetches all stations.
#' @param inactive Boolean specifying whether to include inactive stations. In this case, a station with no measurements for 5 or more years is considered inactive. Default is FALSE, i.e. no inactive stations.
#' @param save_path The path where the .csv(s) and plots should be saved. Set to NULL for data only as an R object. Plots are not created if there is no save path.
#' @param stats set TRUE if you want basic statistics (mean, min, max) and calculated trends.
#' @param complete_yrs Should only years with complete data be used? If TRUE, will not use the current year's data until after May.
#' @param plots Set TRUE if you want plots of SWE, depth, and density generated (but see next parameter).
#' @param plot_type Set to "separate" for 3 plots per location, or "combined" for a single compound plot per location.
#' @param quiet Suppresses most messages and warnings.
#' @param con A connection to the aquacache. Leave as NULL to use [AquaConnect()] to establish a connection, which will be closed when finished. If you pass your own connection remember to close it when done.
#'
#' @return A list with four data.frames: location metadata, basic statistics, trend information, and snow course measurements is returned to the R environment. In addition, an Excel workbook is saved to the save_path with the four data.frames, and a new folder created to hold SWE and depth plots for each station requested.
#'
#' @seealso [waterInfo()] for a similar function dealing with water flow/level.
#' @export
#'
#'
snowInfo <- function(locations = "all", inactive = FALSE, save_path = "choose", stats = TRUE, complete_yrs = TRUE, plots = TRUE, plot_type = "combined", quiet = FALSE, con = NULL) {
  
  # parameters for testing (remember to comment out when done)
  # locations <- "all"
  # inactive <- FALSE
  # save_path <- "C:/Users/gtdelapl/Desktop"
  # stats <- TRUE
  # complete_yrs <- TRUE
  # plots <- FALSE
  # plot_type <- "combined"
  # quiet <- FALSE
  # con <- NULL
  
  rlang::check_installed("trend", reason = "necessary to calculate trends.")
  if (plots) {
    rlang::check_installed("gridExtra", reason = "necessary to create plots.")
  }
  
  if (!is.null(save_path)) {
    if (save_path == "choose") {
      if (!interactive()) {
        stop("You must specify a save path when running in non-interactive mode.")
      }
      message("Select the path to the folder where you want this report saved.")
      save_path <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"),"Desktop"))
    }
    dir.create(paste0(save_path, "/SnowInfo_", Sys.Date()))
    if (plots) {
      dir.create(paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots"))
    }
  }
  
  
  if (!(plot_type %in% c("separate", "combined"))) {
    stop("The parameter 'plot_type' must be set to either 'separate' or 'combined'.")
  }
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  if (locations[1] == "all") {
    locations <- DBI::dbGetQuery(con, "
    SELECT DISTINCT l.location, l.name, l.location_id, l.latitude, l.longitude, d.conversion_m
    FROM locations AS l
    JOIN locations_networks AS ln ON l.location_id = ln.location_id
    JOIN networks AS n ON ln.network_id = n.network_id
    JOIN samples AS s ON l.location_id = s.location_id
    JOIN datum_conversions AS d ON l.location_id = d.location_id
    WHERE n.name = 'Snow Survey Network';
")
    all <- TRUE
  } else {
    loc_tbl <- DBI::dbGetQuery(con, paste0("
    SELECT DISTINCT l.location, l.name, l.location_id, l.latitude, l.longitude, d.conversion_m
    FROM locations AS l
    JOIN locations_networks AS ln ON l.location_id = ln.location_id
    JOIN networks AS n ON ln.network_id = n.network_id
    JOIN samples AS s ON l.location_id = s.location_id
    JOIN datum_conversions AS d ON l.location_id = d.location_id
    WHERE n.name = 'Snow Survey Network' AND l.location IN ('", paste(locations, collapse = "', '"), "');
"))
    # Find the missing locations if any were not found. locations is a vector of requested locations
    check_locs <- locations[!locations %in% loc_tbl$location]
    if (length(check_locs) > 0) {
      message("Could not find a record for location ", check_locs, ". All other locations will be returned.")
    }
    all <- FALSE
  }
  
  #Get the measurements
  
  samples <- DBI::dbGetQuery(con, paste0("SELECT sample_id, location_id, target_datetime FROM samples WHERE location_id IN ('", paste(locations$location_id, collapse = "', '"), "') AND media_id = 7 AND collection_method = 1")) #media = 'atmospheric', collection_method = 'observation'
  
  if (!inactive) { # Filter out any location with no measurements for 5 or more years
    rm.inactive <- samples %>%
      dplyr::group_by(location_id) %>%
      dplyr::summarise(min_year = min(lubridate::year(target_datetime)),
                       max_year = max(lubridate::year(target_datetime))) %>%
      dplyr::mutate(rm.inactive = max_year - min_year < 5) %>%
      dplyr::filter(rm.inactive) %>%
      dplyr::pull(location_id)
    
    locations <- locations[!locations$location_id %in% rm.inactive,]
    samples <- samples[!samples$location_id %in% rm.inactive,]
  }
  
  results <- DBI::dbGetQuery(con, paste0("SELECT r.sample_id, r.result, p.param_name FROM results AS r JOIN parameters AS p ON p.parameter_id = r.parameter_id WHERE r.sample_id IN ('", paste(samples$sample_id, collapse = "', '"), "') AND p.parameter_id IN (21, 1220)"))
  
  #Manipulate/preprocess things a bit
  samples$year <- lubridate::year(samples$target_datetime)
  samples$month <- lubridate::month(samples$target_datetime)
  
  results <- merge(results, samples, by = "sample_id")
  
  results <- merge(results, locations[, c("location_id", "location", "name")], by.x = "location_id", by.y = "location_id")
  
  if (stats) {
    #Calculate station basic stats: min, max, mean, median, total yrs, gaps
    stats_df <- data.frame()
    for (i in 1:nrow(locations)) {
      yrs <- unique(results[results$location_id == locations$location_id[i] , "year"])
      if (lubridate::month(Sys.Date()) %in% c(1:5) & complete_yrs) {
        yrs <- yrs[!yrs == lubridate::year(Sys.Date())]
      }
      total_yrs <- max(yrs) - min(yrs)
      gaps <- seq(min(yrs), max(yrs))[!(seq(min(yrs), max(yrs)) %in% yrs)]
      sample_months <- sort(unique(lubridate::month(results[results$location_id == locations$location_id[i] , ]$target_datetime, label = TRUE, abbr = TRUE)))
      allMaxSWE <- max(results[results$location_id == locations$location_id[i] & results$param_name == "snow water equivalent" , "result"], na.rm = TRUE)
      allMaxDepth <- max(results[results$location_id == locations$location_id[i] & results$param_name == "snow depth" , "result"], na.rm = TRUE)
      
      depthMaxes <- NULL
      SWEMaxes <- NULL
      for (j in unique(yrs)) {
        subset <- results[results$year == j & results$location_id == locations$location_id[i], ]
        months <- unique(subset$month)
        if (3 %in% months & 4 %in% months) {
          subsetDepth <- max(subset[subset$param_name == "snow depth", "result"], na.rm = TRUE)
          subsetSWE <- max(subset[subset$param_name == "snow water equivalent", "result"], na.rm = TRUE)
          depthMaxes <- c(depthMaxes, subsetDepth)
          SWEMaxes <- c(SWEMaxes, subsetSWE)
        }
      }
      
      medianMaxDepth <- stats::median(depthMaxes, na.rm = TRUE)
      meanMaxDepth <- mean(depthMaxes, na.rm = TRUE)
      medianMaxSWE <- stats::median(SWEMaxes, na.rm = TRUE)
      meanMaxSWE <- mean(SWEMaxes, na.rm = TRUE)
      
      stats_df <- rbind(stats_df,
                        data.frame("location_code" = locations$location[i],
                                   "total_record_yrs" = total_yrs,
                                   "start" = min(yrs),
                                   "end" = max(yrs),
                                   "missing_yrs" = paste(gaps, collapse = ", ", sep = ", "),
                                   "sample_months" = paste(sample_months, collapse = ", "),
                                   "max_SWE_mm" = round(allMaxSWE, 0),
                                   "mean_max_SWE_mm" = round(meanMaxSWE, 1),
                                   "median_max_SWE_mm" = round(medianMaxSWE, 1),
                                   "max_DEPTH_cm" = round(allMaxDepth, 0),
                                   "mean_max_DEPTH_cm" = round(meanMaxDepth, 1),
                                   "median_max_DEPTH_cm" = round(medianMaxDepth,1)
                        )
      )
    }
    
    trends <- data.frame()
    #Calculate trends for all locations
    for (i in 1:nrow(locations)) {
      yrs <- unique(results[results$location_id == locations$location_id[i] , "year"])
      yrs <- yrs[order(yrs)]
      if (lubridate::month(Sys.Date()) %in% c(1:5) & complete_yrs) {
        yrs <- yrs[!yrs == lubridate::year(Sys.Date())]
      }
      AllSWEMax <- numeric(0)
      for (j in unique(yrs)) {
        AllSWEMax <- c(AllSWEMax, max(results[results$location_id == locations$location_id[i] & results$year == j & results$param_name == "snow water equivalent", "result"]), na.rm = TRUE)
      }
      AllSWEMax <- stats::na.omit(hablar::rationalize(AllSWEMax))
      if (length(AllSWEMax) > 6) {
        AllSWESensMax <- trend::sens.slope(AllSWEMax)
      } else {
        AllSWESensMax$estimates <- NA
        AllSWESensMax$p.value <- NA
      }
      
      AllDepthMax <- numeric(0)
      for (j in unique(yrs)) {
        AllDepthMax <- c(AllDepthMax, max(results[results$location_id == locations$location_id[i] & results$year == j & results$param_name == "snow depth", "result"]), na.rm = TRUE)
      }
      AllDepthMax <- stats::na.omit(hablar::rationalize(AllDepthMax))
      if (length(AllDepthMax) > 6) {
        AllDepthSensMax <- trend::sens.slope(AllDepthMax)
      } else {
        AllDepthSensMax$estimates <- NA
        AllDepthSensMax$p.value <- NA
      }
      
      trends <- rbind(trends,
                      data.frame("location_id" = locations$location_id[i],
                                 "location_code" = locations$location[i],
                                 "p.value_SWE_max" = round(unname(AllSWESensMax$p.value), 3),
                                 "sens.slope_SWE_max" = round(unname(AllSWESensMax$estimates), 3),
                                 "n_years_SWE" = AllSWESensMax$parameter,
                                 "p.value_DEPTH_max" = round(unname(AllDepthSensMax$p.value), 3),
                                 "sens.slope_DEPTH_max" = round(unname(AllDepthSensMax$estimates), 3),
                                 "n_years_DEPTH" = AllDepthSensMax$parameter
                      ))
    }
    
    for (i in 1:nrow(trends)) {
      subset <- results[results$location_id == trends$location_id[i] & results$result > 0,]
      intercept_yr <- min(subset$year)
      intercept_value_SWE <- unname(stats::lm(formula = subset[subset$param_name == "snow water equivalent", "result"] ~ subset[subset$param_name == "snow water equivalent", "target_datetime"])$coefficients[1])
      intercept_value_depth <- unname(stats::lm(formula = subset[subset$param_name == "snow depth", "result"] ~ subset[subset$param_name == "snow depth", "target_datetime"])$coefficients[1])
      trends$annual_prct_chg_SWE[i] <- round(trends[trends$location_code == trends$location_code[i] , "sens.slope_SWE_max"] / intercept_value_SWE, 4)
      trends$annual_prct_chg_DEPTH[i] <- round(trends[trends$location_code == trends$location_code[i] , "sens.slope_DEPTH_max"] / intercept_value_depth, 4)
      trends$note[i] <- paste0("Prct chg based on linear model intercepts of ", round(intercept_value_SWE, 1), " and ", round(intercept_value_depth,1), " for SWE and depth at the start year.")
    }
    # Drop the location_id key column
    trends$location_id <- NULL
    
    if (all) {
      # Below is commented out as it's a bit misleading. The mean of the % annual change is not meaningful for the territory as a whole.
      #Calculate the territory trend and add it to trends
      # terr_prct_chg_SWE <- mean(trends$annual_prct_chg_SWE, na.rm = TRUE)
      # terr_prct_chg_depth <- mean(trends$annual_prct_chg_DEPTH, na.rm = TRUE)
      # trends <- plyr::rbind.fill(trends, data.frame("location_code" = "territory",
      #                                               "annual_prct_chg_SWE" = terr_prct_chg_SWE,
      #                                               "annual_prct_chg_DEPTH" = terr_prct_chg_depth,
      #                                               "note" = "Mean of the annual percent changes."))
      
      yrs <- seq(1980, lubridate::year(Sys.Date())) #Start in 1980 because the network is essentially unchanged since then
      meanMaxSWE <- NULL
      meanMaxDepth <- NULL
      meanApr1SWE <- NULL
      meanApr1Depth <- NULL
      for (i in yrs) {
        yearMaxSWE <- NULL
        yearMaxDepth <- NULL
        yearApr1SWE <- NULL
        yearApr1Depth <- NULL
        for (j in 1:nrow(locations)) {
          subset <- results[results$year == i & results$location_id == locations$location_id[j] , ]
          months <- unique(subset$month)
          add <- FALSE
          if (3 %in% months & 4 %in% months) {
            locationSWE <- max(subset[subset$param_name == "snow water equivalent", "result"], na.rm = TRUE)
            locationDepth <- max(subset[subset$param_name == "snow depth", "result"], na.rm = TRUE)
            add <- TRUE
          }
          if (add) {
            yearMaxSWE <- c(yearMaxSWE, locationSWE)
            yearMaxDepth <- c(yearMaxDepth, locationDepth)
          }
          if (4 %in% months) {
            locApr1SWE <- subset[as.Date(subset$target_date) == paste0(i, "-04-01") & subset$param_name == "snow water equivalent", "result"]
            locApr1Depth <- subset[as.Date(subset$target_date) == paste0(i, "-04-01") & subset$param_name == "snow depth", "result"]
            yearApr1SWE <- c(yearApr1SWE, locApr1SWE)
            yearApr1Depth <- c(yearApr1Depth, locApr1Depth)
          }
        }
        if (!is.null(yearMaxSWE) & !is.null(yearMaxDepth) & length(yearMaxSWE) > nrow(locations)/2) {
          meanMaxSWE <- c(meanMaxSWE, mean(yearMaxSWE, na.rm = TRUE))
          meanMaxDepth <- c(meanMaxDepth, mean(yearMaxDepth, na.rm = TRUE))
        }
        if (!is.null(yearApr1SWE) & !is.null(yearApr1Depth)) {
          meanApr1SWE <- c(meanApr1SWE, mean(yearApr1SWE, na.rm = TRUE))
          meanApr1Depth <- c(meanApr1Depth, mean(yearApr1Depth, na.rm = TRUE))
        }
      }
      
      meanMaxSWESens <- trend::sens.slope(meanMaxSWE)
      meanMaxDepthSens <- trend::sens.slope(meanMaxDepth)
      meanApr1SWESens <- trend::sens.slope(meanApr1SWE)
      meanApr1DepthSens <- trend::sens.slope(meanApr1Depth)
      
      plot_all_SWE <- data.frame("location" = "all_locs_max",
                                 target_datetime = as.POSIXct(paste0(seq(min(yrs), min(yrs) + length(meanMaxSWE) - 1), "-01-01 00:00")),
                                 param_name = "snow water equivalent",
                                 result = meanMaxSWE)
      plot_all_depth <- data.frame("location" = "all_locs_max",
                                   target_datetime = as.POSIXct(paste0(seq(min(yrs), min(yrs) + length(meanMaxSWE) - 1), "-01-01 00:00")),
                                   param_name = "snow depth",
                                   result = meanMaxDepth)
      plot_apr1_SWE <- data.frame("location" = "all_locs_Apr1",
                                  target_datetime = as.POSIXct(paste0(seq(min(yrs), min(yrs) + length(meanApr1SWE) - 1), "-04-01 00:00")),
                                  param_name = "snow water equivalent",
                                  result = meanApr1SWE)
      plot_apr1_depth <- data.frame("location" = "all_locs_Apr1",
                                    target_datetime = as.POSIXct(paste0(seq(min(yrs), min(yrs) + length(meanApr1SWE) - 1), "-04-01 00:00")),
                                    param_name = "snow depth",
                                    result = meanApr1Depth)
      new_all <- data.frame("location" = "all_locs_max",
                            "name" = "Territory-averaged maximum")
      new_apr1 <- data.frame("location" = "all_locs_Apr1",
                             "name" = "Territory-averaged April 1")
      
      results <- plyr::rbind.fill(results, plot_all_SWE)
      results <- plyr::rbind.fill(results, plot_all_depth)
      results <- plyr::rbind.fill(results, plot_apr1_SWE)
      results <- plyr::rbind.fill(results, plot_apr1_depth)
      
      locations <- plyr::rbind.fill(locations, new_all)
      locations <- plyr::rbind.fill(locations, new_apr1)
      
      territory <- data.frame("subset" = c("mean max", "mean Apr 1"),
                              "inactive_locs" = inactive,
                              "n_locs" = nrow(locations) - 2,
                              "yr_start" = min(yrs),
                              "yr_end" = max(yrs),
                              "mean_SWE_mm" = c(round(mean(meanMaxSWE, na.rm = TRUE), 1), round(mean(meanApr1SWE, na.rm = TRUE), 1)),
                              "median_SWE_mm" = c(round(stats::median(meanMaxSWE, na.rm = TRUE), 1), round(stats::median(meanApr1SWE, na.rm = TRUE), 1)),
                              "mean_DEPTH_cm" = c(round(mean(meanMaxDepth, na.rm = TRUE), 1), round(mean(meanApr1Depth, na.rm = TRUE), 1)),
                              "median_DEPTH_cm" = c(round(stats::median(meanMaxDepth, na.rm = TRUE), 1), round(stats::median(meanApr1Depth, na.rm = TRUE), 1)),
                              "p.val_SWE_mean" = c(round(unname(meanMaxSWESens$p.value), 3), round(unname(meanApr1SWESens$p.value), 3)),
                              "sens.slope_SWE_mean" = c(round(unname(meanMaxSWESens$estimates), 3), round(unname(meanApr1SWESens$estimates), 3)),
                              "p.val_DEPTH_mean" = c(round(unname(meanMaxDepthSens$p.value), 3), round(unname(meanApr1DepthSens$p.value), 3)),
                              "sens.slope_DEPTH_mean" = c(round(unname(meanMaxDepthSens$estimates), 3), round(unname(meanApr1DepthSens$estimates), 3)),
                              "annual_prct_chg_SWE" = c(
                                unname(meanMaxSWESens$estimates) / unname(stats::lm(plot_all_SWE$result ~ plot_all_SWE$target_datetime)$coefficients[1]),
                                unname(meanApr1SWESens$estimates) / unname(stats::lm(plot_apr1_SWE$result ~ plot_apr1_SWE$target_datetime)$coefficients[1])
                              ),
                              "annual_prct_chg_DEPTH" = c(
                                unname(meanMaxDepthSens$estimates) / unname(stats::lm(plot_all_depth$result ~ plot_all_depth$target_datetime)$coefficients[1]),
                                unname(meanApr1DepthSens$estimates) / unname(stats::lm(plot_apr1_depth$result ~ plot_apr1_depth$target_datetime)$coefficients[1])
                              ),
                              "description" = c(paste0("Computed on one data point per year, consisting of the mean of maximum values reported for each location. Percent annual change calculated based on linear model intercepts of ", round(unname(stats::lm(plot_all_SWE$result ~ plot_all_SWE$target_datetime)$coefficients[1]), 0), " mm SWE and ", round(unname(stats::lm(plot_all_depth$result ~ plot_all_depth$target_datetime)$coefficients[1]), 0), " cm depth at start year."),
                                                paste0("Computed on one data point per year, consisting of April 1 values reported for each location. Percent annual change calculated based on linear model intercepts of ", round(unname(stats::lm(plot_apr1_SWE$result ~ plot_apr1_SWE$target_datetime)$coefficients[1]), 0), " mm SWE and ", round(unname(stats::lm(plot_apr1_depth$result ~ plot_apr1_depth$target_datetime)$coefficients[1]), 0), " cm depth at start year."))
      )
    }
  } #End of stats loop
  
  if (plots) {
    plotsSWE <- list()
    plotsDepth <- list()
    plotsDensity <- list()
    plotsCombined <- list()
    
    for (i in 1:nrow(locations)) {
      name <- locations$location[i]
      if (name == "all_locs_max") {
        display_name <- "Territory mean maximum"
      } else if (name == "all_locs_Apr1") {
        display_name <- "Territory mean April 1"
      } else {
        display_name <- locations$name[i]
      }
      plot_results <- results[results$location == name , ]
      plot_results <- plot_results[order(plot_results$target_datetime), ]
      plot_results$location_id <- NULL
      plot_results$sample_id <- NULL
      
      SWE <- plot_results[plot_results$param_name == "snow water equivalent", "result"]
      depth <- plot_results[plot_results$param_name == "snow depth", "result"]
      target_datetimes <- plot_results[plot_results$param_name == "snow water equivalent", "target_datetime"]
      years <- plot_results[plot_results$param_name == "snow water equivalent", "year"]
      months <- plot_results[plot_results$param_name == "snow water equivalent", "month"]
      
      density <- data.frame(target_datetime = target_datetimes,
                            result = (SWE/10)/depth,
                            year = years,
                            month = months,
                            location = unique(plot_results$location),
                            name = unique(plot_results$name),
                            param_name = "density")
      density <- hablar::rationalize(density)
      
      plot_results <- rbind(plot_results, density[!is.na(density$result), ])
      
      
      plotSWE <- ggplot2::ggplot(data = plot_results[plot_results$param_name == "snow water equivalent" & plot_results$result > 0, ], ggplot2::aes(x = .data$target_datetime, y = .data$result, group = .data$year)) +
        ggplot2::scale_x_datetime() +
        ggplot2::geom_point() +
        ggplot2::geom_line(linewidth = 0.1) +
        ggplot2::theme_classic()
      if (plot_type == "separate") {
        plotSWE <- plotSWE +
          ggplot2::labs(x = "Sample date", y = "SWE (mm)", title = paste0(locations$location[i], ": " , display_name))
      } else {
        plotSWE <- plotSWE +
          ggplot2::labs(y = "SWE (mm)", title = paste0(locations$location[i], ": " , display_name)) +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())
      }
      
      plotDepth <- ggplot2::ggplot(data = plot_results[plot_results$param_name == "snow water equivalent" & plot_results$result > 0 , ], ggplot2::aes(x = .data$target_datetime, y = .data$result, group = .data$year)) +
        ggplot2::scale_x_datetime() +
        ggplot2::geom_point(size = 1.75) +
        ggplot2::geom_line(linewidth = 0.1) +
        ggplot2::theme_classic()
      if (plot_type == "separate") {
        plotDepth <- plotDepth +
          ggplot2::labs(x = "Sample date", y = "Snow depth (cm)", title = paste0(locations$location[i], ": " , display_name))
      } else {
        plotDepth <- plotDepth +
          ggplot2::labs(y = "Snow depth (cm)") +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())
      }
      
      plotDensity <- ggplot2::ggplot(data = plot_results[plot_results$param_name == "density" & plot_results$result > 0 , ], ggplot2::aes(x = .data$target_datetime, y = .data$result, group = .data$year)) +
        ggplot2::scale_x_datetime() +
        ggplot2::geom_point(size = 1.75) +
        ggplot2::geom_line(linewidth = 0.1) +
        ggplot2::theme_classic()
      if (plot_type == "separate") {
        plotDensity <- plotDensity +
          ggplot2::labs(x = "Sample date", y = bquote('Density (g/' ~cm^{"3"} *')'), title = paste0(locations$location[i], ": " , display_name))
      } else {
        plotDensity <- plotDensity +
          ggplot2::labs(x = "Sample date", y = bquote('Density (g/' ~cm^{"3"} *')'))
      }
      
      if (plot_type == "combined") {
        plots_combined <- gridExtra::arrangeGrob(plotSWE, plotDepth, plotDensity)
        plotsCombined[[display_name]] <- plots_combined
      } else {
        plotsSWE[[display_name]] <- plotSWE
        plotsDepth[[display_name]] <- plotDepth
        plotsDensity[[display_name]] <- plotDensity
      }
      if (!is.null(save_path)) {
        grDevices::pdf(NULL)
        if (plot_type == "combined") {
          ggplot2::ggsave(filename = paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_combined.png"), plot = plots_combined, height = 8, width = 8, units = "in", device = "png", dpi = 300)
        } else {
          ggplot2::ggsave(filename = paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_SWE.png"), plot = plotSWE, height = 6, width = 10, units = "in", device = "png", dpi = 300)
          ggplot2::ggsave(filename = paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_DEPTH.png"), plot = plotDepth, height = 6, width = 10, units = "in", device = "png", dpi = 300)
          ggplot2::ggsave(filename = paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_DENSITY.png"), plot = plotDensity, height = 6, width = 10, units = "in", device = "png", dpi = 300)
        }
        dev.off()
      }
    } #End of for locations loop
    
    if (plot_type == "combined") {
      message("Combined SWE, depth, density plots were returned in a list element. You can view each combined plot by calling grid::grid.draw on the desired object, or dig a bit deeper and find each individual ggplot object.")
    }
  } #End of plots loop
  
  
  if (all) {
    locations <- locations[!locations$location %in% c("all_locs_max", "all_locs_Apr1") , ]
    results <- results[!results$location %in% c("all_locs_max", "all_locs_Apr1") , ]
  }
  
  locations$last_survey <- NA
  for (i in 1:nrow(locations)) {
    locations$last_survey[i] <- as.character(as.Date(max(samples[samples$location == locations$location_id[i] , "target_datetime"])))
  }
  
  #Fix up the location metadata table
  locations <- locations[, -which(names(locations) == "location_id")]
  names(locations) <- c("location_code", "location_name", "latitude", "longitude", "elevation_m", "last_survey")
  
  # Fix up the results table
  results <- results[ , -which(names(results) == "location_id")]
  results <- results[, c("location", "name", "param_name", "sample_id", "target_datetime", "year", "month", "result")]
  names(results) <- c("location_code", "location_name", "parameter", "sample_id", "target_date", "year", "month", "result")
  results$target_date <- as.Date(results$target_date)
  
  #Concatenate the various products into a list to return.
  if (stats) {
    if (all) {
      return <- list("locations" = locations, "stats" = stats_df, "trends" = trends, "territory_stats_trends" = territory, "measurements" = results)
    } else {
      return <- list("locations" = locations, "stats" = stats_df, "trends" = trends, "measurements" = results)
    }
  } else {
    return <- list("locations" = locations, "measurements" = results)
  }
  if (!is.null(save_path)) {
    openxlsx::write.xlsx(return, paste0(save_path, "/SnowInfo_", Sys.Date(), "/measurements+stats.xlsx"))
  }
  
  if (plots) {
    if (plot_type == "combined") {
      return[["plots"]] <- plotsCombined
      
    } else if (plot_type == "separate") {
      return[["plots"]] <- list("SWE" = plotsSWE,
                                "Depth" = plotsDepth,
                                "Density" = plotsDensity)
    }
  }
  return(return)
} #End of function
