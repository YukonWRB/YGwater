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
#' @param con A connection to the AquaCache database. Leave as NULL to use [AquaConnect()] to establish a connection, which will be closed when finished. If you pass your own connection remember to close it when done.
#'
#' @return A list with four data.frames: location metadata, basic statistics, trend information, and snow course measurements is returned to the R environment. In addition, an Excel workbook is saved to the save_path with the four data.frames, and a new folder created to hold SWE and depth plots for each station requested.
#'
#' @seealso [waterInfo()] for a similar function dealing with water flow/level.
#' @export
#'
#TODO: This function should really be getting data from the hydro database.

snowInfo <- function(locations = "all", inactive = FALSE, save_path = "choose", stats = TRUE, complete_yrs = TRUE, plots = TRUE, plot_type = "combined", quiet = FALSE, con = NULL) {

  # parameters for testing (remember to comment out when done)
  # locations <- "all"
  # inactive <- FALSE
  # save_path <- "choose"
  # stats <- TRUE
  # complete_yrs <- TRUE
  # plots <- TRUE
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
    locations <- DBI::dbGetQuery(con, "SELECT DISTINCT l.location, l.name, l.location_id, l.latitude, l.longitude, d.conversion_m
FROM locations AS l
JOIN locations_networks AS ln ON l.location_id = ln.location_id
JOIN networks AS n ON ln.network_id = n.network_id
JOIN timeseries AS t ON l.location_id = t.location_id
JOIN datum_conversions AS d ON l.location_id = d.location_id
WHERE n.name = 'Snow Survey Network'
AND t.category = 'discrete';
")
    all <- TRUE
  } else {
    loc_tbl <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT l.location, l.name, l.location_id
FROM locations AS l
JOIN locations_networks AS ln ON l.location_id = ln.location_id
JOIN networks AS n ON ln.network_id = n.network_id
JOIN timeseries AS t ON l.location_id = t.location_id
WHERE n.name = 'Snow Survey Network'
AND t.category = 'discrete' AND l.location IN ('", paste(locations, collapse = "', '"), "');
"))
    check_locs <- loc_tbl$location[!(loc_tbl$location %in% locations)]
    if (length(check_locs) > 0) {
      message("Could not find a record for location ", check_locs, ". Other locations will be returned.")
    }
    locations <- loc_tbl[loc_tbl$location %in% locations , ]
  }

  #Get the measurements
  tsids <- DBI::dbGetQuery(con, paste0("SELECT t.timeseries_id, t.location_id, t.end_datetime, p.param_name, l.location FROM timeseries as T JOIN parameters as p ON t.parameter_id = p.parameter_id JOIN locations AS l ON t.location = l.location WHERE t.location_id IN ('", paste(locations$location_id, collapse = "', '"), "')"))
  
  meas <- DBI::dbGetQuery(con, paste0("SELECT target_datetime, value, timeseries_id FROM measurements_discrete WHERE timeseries_id IN (", paste(tsids$timeseries_id, collapse = ", "), ")"))

  if (!inactive) { #Filter out the inactive stations if inactive is FALSE
    remove <- tsids[tsids$end_datetime < Sys.time() - 365*60*60*24*5, ]
    locations <- locations[!locations$location_id %in% remove$location_id,]
    meas <- meas[!meas$timeseries_id %in% remove$timeseries_id, ]
  }

  #Manipulate/preprocess things a bit
  meas$year <- lubridate::year(meas$target_date)
  meas$month <- lubridate::month(meas$target_date)
  
  meas <- merge(meas, tsids, by = "timeseries_id")
  
  if (stats) {
    #Calculate station basic stats: min, max, mean, median, total yrs, gaps
    stats_df <- data.frame()
    for (i in 1:nrow(locations)) {
      yrs <- unique(meas[meas$location == locations$location[i] , "year"])
      if (lubridate::month(Sys.Date()) %in% c(1:5) & complete_yrs) {
        yrs <- yrs[!yrs == lubridate::year(Sys.Date())]
      }
      total_yrs <- max(yrs) - min(yrs)
      gaps <- seq(min(yrs), max(yrs))[!(seq(min(yrs), max(yrs)) %in% yrs)]
      sample_months <- sort(unique(lubridate::month(meas[meas$location == locations$location[i] , ]$target_datetime, label = TRUE, abbr = TRUE)))
      allMaxSWE <- max(meas[meas$location == locations$location[i] & meas$param_name == "snow water equivalent" , "value"], na.rm = TRUE)
      allMaxDepth <- max(meas[meas$location == locations$location[i] & meas$param_name == "snow depth" , "value"], na.rm = TRUE)

      depthMaxes <- NULL
      SWEMaxes <- NULL
      for (j in unique(yrs)) {
        subset <- meas[meas$year == j & meas$location == locations$location[i], ]
        months <- unique(subset$month)
        if (3 %in% months & 4 %in% months) {
          subsetDepth <- max(subset[subset$param_name == "snow depth", "value"], na.rm = TRUE)
          subsetSWE <- max(subset[subset$param_name == "snow water equivalent", "value"], na.rm = TRUE)
          depthMaxes <- c(depthMaxes, subsetDepth)
          SWEMaxes <- c(SWEMaxes, subsetSWE)
        }
      }

      medianMaxDepth <- stats::median(depthMaxes)
      meanMaxDepth <- mean(depthMaxes)
      medianMaxSWE <- stats::median(SWEMaxes)
      meanMaxSWE <- mean(SWEMaxes)

      stats_df <- rbind(stats_df,
                     data.frame("location_ID" = locations$location[i],
                                "total_record_yrs" = total_yrs,
                                "start" = min(yrs),
                                "end" = max(yrs),
                                "missing_yrs" = paste(gaps, collapse = ", ", sep = ", "),
                                "sample_months" = paste(sample_months, collapse = ", "),
                                "max_SWE" = allMaxSWE,
                                "mean_max_SWE" = round(meanMaxSWE, 1),
                                "median_max_SWE" = round(medianMaxSWE, 1),
                                "max_DEPTH" = allMaxDepth,
                                "mean_max_DEPTH" = round(meanMaxDepth, 1),
                                "median_max_DEPTH" = round(medianMaxDepth,1)
                     )
      )
    }

    trends <- data.frame()
    #Calculate trends for all locations
    for (i in 1:nrow(locations)) {
      yrs <- unique(meas[meas$location == locations$location[i] , "year"])
      if (lubridate::month(Sys.Date()) %in% c(1:5) & complete_yrs) {
        yrs <- yrs[!yrs == lubridate::year(Sys.Date())]
      }
      AllSWEMax <- numeric(0)
      for (j in unique(yrs)) {
        AllSWEMax <- c(AllSWEMax, max(meas[meas$location == locations$location[i] & meas$year == j & meas$param_name == "snow water equivalent", "value"]))
      }
      AllSWEMax <- stats::na.omit(hablar::rationalize(AllSWEMax))
      if (length(AllSWEMax) > 6) {
        AllSWESensMax <- trend::sens.slope(AllSWEMax)
      } else {
        AllSWESensMax$estimates <- NA
        AllSWESensMax$p.value <- NA
      }

      AllDepthMax <- numeric(0)
      for (j in yrs) {
        AllDepthMax <- c(AllDepthMax, max(meas[meas$location == locations$location[i] & meas$year == j & meas$param_name == "snow depth", "value"]))
      }
      AllDepthMax <- stats::na.omit(hablar::rationalize(AllDepthMax))
      if (length(AllDepthMax) > 6) {
        AllDepthSensMax <- trend::sens.slope(AllDepthMax)
      } else {
        AllDepthSensMax$estimates <- NA
        AllDepthSensMax$p.value <- NA
      }

      trends <- rbind(trends,
                      data.frame("location_ID" = locations$location[i],
                                 "p.value_SWE_max" = round(unname(AllSWESensMax$p.value), 3),
                                 "sens.slope_SWE_max" = round(unname(AllSWESensMax$estimates), 3),
                                 "n_years_SWE" = AllSWESensMax$parameter,
                                 "p.value_DEPTH_max" = round(unname(AllDepthSensMax$p.value), 3),
                                 "sens.slope_DEPTH_max" = round(unname(AllDepthSensMax$estimates), 3),
                                 "n_years_DEPTH" = AllDepthSensMax$parameter
                      ))
    }

    for (i in 1:nrow(trends)) {
      subset <- meas[meas$location == trends$location_ID[i] & meas$value > 0,]
      intercept_yr <- min(subset$year)
      intercept_value_SWE <- unname(stats::lm(formula = subset[subset$param_name == "snow water equivalent", "value"] ~ subset[subset$param_name == "snow water equivalent", "target_datetime"])$coefficients[1])
      intercept_value_depth <- unname(stats::lm(formula = subset[subset$param_name == "snow depth", "value"] ~ subset[subset$param_name == "snow depth", "target_datetime"])$coefficients[1])
      trends$annual_prct_chg_SWE[i] <- round(trends[trends$location_ID == trends$location_ID[i] , "sens.slope_SWE_max"] / intercept_value_SWE, 4)
      trends$annual_prct_chg_DEPTH[i] <- round(trends[trends$location_ID == trends$location_ID[i] , "sens.slope_DEPTH_max"] / intercept_value_depth, 4)
      trends$note[i] <- paste0("Prct chg based on linear model intercepts of ", round(intercept_value_SWE, 1), " and ", round(intercept_value_depth,1), " for SWE and depth at the start year.")
    }

    if (all) {
      #Calculate the territory trend and add it to trends
      terr_prct_chg_SWE <- mean(trends$annual_prct_chg_SWE)
      terr_prct_chg_depth <- mean(trends$annual_prct_chg_DEPTH)
      trends <- plyr::rbind.fill(trends, data.frame("location_ID" = "territory",
                                                    "annual_prct_chg_SWE" = terr_prct_chg_SWE,
                                                    "annual_prct_chg_DEPTH" = terr_prct_chg_depth,
                                                    "note" = "Mean of the annual percent changes."))

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
          subset <- meas[meas$year == i & meas$location == locations$location[j] , ]
          months <- unique(subset$month)
          add <- FALSE
          if (3 %in% months & 4 %in% months) {
            locationSWE <- max(subset[subset$param_name == "snow water equivalent", "value"], na.rm = TRUE)
            locationDepth <- max(subset[subset$param_name == "snow depth", "value"], na.rm = TRUE)
            add <- TRUE
          }
          if (add) {
            yearMaxSWE <- c(yearMaxSWE, locationSWE)
            yearMaxDepth <- c(yearMaxDepth, locationDepth)
          }
          if (4 %in% months) {
            locApr1SWE <- subset[as.Date(subset$target_date) == paste0(i, "-04-01") & subset$param_name == "snow water equivalent", "value"]
            locApr1Depth <- subset[as.Date(subset$target_date) == paste0(i, "-04-01") & subset$param_name == "snow depth", "value"]
            yearApr1SWE <- c(yearApr1SWE, locApr1SWE)
            yearApr1Depth <- c(yearApr1Depth, locApr1Depth)
          }
        }
        if (!is.null(yearMaxSWE) & !is.null(yearMaxDepth) & length(yearMaxSWE) > nrow(locations)/2) {
          meanMaxSWE <- c(meanMaxSWE, mean(yearMaxSWE))
          meanMaxDepth <- c(meanMaxDepth, mean(yearMaxDepth))
        }
        if (!is.null(yearApr1SWE) & !is.null(yearApr1Depth)) {
          meanApr1SWE <- c(meanApr1SWE, mean(yearApr1SWE))
          meanApr1Depth <- c(meanApr1Depth, mean(yearApr1Depth))
        }
      }

      meanMaxSWESens <- trend::sens.slope(meanMaxSWE)
      meanMaxDepthSens <- trend::sens.slope(meanMaxDepth)
      meanApr1SWESens <- trend::sens.slope(meanApr1SWE)
      meanApr1DepthSens <- trend::sens.slope(meanApr1Depth)

      plot_all_SWE <- data.frame("location" = "all_locs_max",
                                 target_datetime = as.POSIXct(paste0(seq(min(yrs), min(yrs) + length(meanMaxSWE) - 1), "-01-01 00:00")),
                                 param_name = "snow water equivalent",
                                 value = meanMaxSWE)
      plot_all_depth <- data.frame("location" = "all_locs_max",
                                   target_datetime = as.POSIXct(paste0(seq(min(yrs), min(yrs) + length(meanMaxSWE) - 1), "-01-01 00:00")),
                                   param_name = "snow depth",
                                   value = meanMaxDepth)
      plot_apr1_SWE <- data.frame("location" = "all_locs_Apr1",
                                 target_datetime = as.POSIXct(paste0(seq(min(yrs), min(yrs) + length(meanApr1SWE) - 1), "-04-01 00:00")),
                                 param_name = "snow water equivalent",
                                 value = meanApr1SWE)
      plot_apr1_depth <- data.frame("location" = "all_locs_Apr1",
                                   target_datetime = as.POSIXct(paste0(seq(min(yrs), min(yrs) + length(meanApr1SWE) - 1), "-04-01 00:00")),
                                   param_name = "snow depth",
                                   value = meanApr1Depth)
      new_all <- data.frame("location" = "all_locs_max",
                            "name" = "Territory-averaged maximum")
      new_apr1 <- data.frame("location" = "all_locs_Apr1",
                            "name" = "Territory-averaged April 1")
      
      meas <- plyr::rbind.fill(meas, plot_all_SWE)
      meas <- plyr::rbind.fill(meas, plot_all_depth)
      meas <- plyr::rbind.fill(meas, plot_apr1_SWE)
      meas <- plyr::rbind.fill(meas, plot_apr1_depth)
      
      locations <- plyr::rbind.fill(locations, new_all)
      locations <- plyr::rbind.fill(locations, new_apr1)

      territory <- data.frame("subset" = c("mean max", "mean Apr 1"),
                              "inactive_locs" = inactive,
                              "n_locs" = nrow(locations) - 2,
                              "yr_start" = min(yrs),
                              "yr_end" = max(yrs),
                              "mean_SWE" = c(round(mean(meanMaxSWE), 1), round(mean(meanApr1SWE), 1)),
                              "median_SWE" = c(round(stats::median(meanMaxSWE), 1), round(stats::median(meanApr1SWE), 1)),
                              "mean_DEPTH" = c(round(mean(meanMaxDepth), 1), round(mean(meanApr1Depth), 1)),
                              "median_DEPTH" = c(round(stats::median(meanMaxDepth), 1), round(stats::median(meanApr1Depth), 1)),
                              "p.val_SWE_mean" = c(round(unname(meanMaxSWESens$p.value), 3), round(unname(meanApr1SWESens$p.value), 3)),
                              "sens.slope_SWE_mean" = c(round(unname(meanMaxSWESens$estimates), 3), round(unname(meanApr1SWESens$estimates), 3)),
                              "p.val_DEPTH_mean" = c(round(unname(meanMaxDepthSens$p.value), 3), round(unname(meanApr1DepthSens$p.value), 3)),
                              "sens.slope_DEPTH_mean" = c(round(unname(meanMaxDepthSens$estimates), 3), round(unname(meanApr1DepthSens$estimates), 3)),
                              "annual_prct_chg_SWE" = c(
                                unname(meanMaxSWESens$estimates) / unname(stats::lm(plot_all$SNOW_WATER_EQUIV ~ plot_all$SAMPLE_DATE)$coefficients[1]),
                                unname(meanApr1SWESens$estimates) / unname(stats::lm(plot_apr1$SNOW_WATER_EQUIV ~ plot_apr1$SAMPLE_DATE)$coefficients[1])
                              ),
                              "annual_prct_chg_DEPTH" = c(
                                unname(meanMaxDepthSens$estimates) / unname(stats::lm(plot_all$DEPTH ~ plot_all$SAMPLE_DATE)$coefficients[1]),
                                unname(meanApr1DepthSens$estimates) / unname(stats::lm(plot_apr1$DEPTH ~ plot_apr1$SAMPLE_DATE)$coefficients[1])
                                ),
                              "description" = c(paste0("Computed on one data point per year, consisting of the mean of maximum values reported for each location. Percent annual change calculated based on linear model intercepts of ", round(unname(stats::lm(plot_all$SNOW_WATER_EQUIV ~ plot_all$SAMPLE_DATE)$coefficients[1]), 0), " mm SWE and ", round(unname(stats::lm(plot_all$DEPTH ~ plot_all$SAMPLE_DATE)$coefficients[1]), 0), " cm depth at start year."),
                                                paste0("Computed on one data point per year, consisting of April 1 values reported for each location. Percent annual change calculated based on linear model intercepts of ", round(unname(stats::lm(plot_apr1$SNOW_WATER_EQUIV ~ plot_apr1$SAMPLE_DATE)$coefficients[1]), 0), " mm SWE and ", round(unname(stats::lm(plot_apr1$DEPTH ~ plot_apr1$SAMPLE_DATE)$coefficients[1]), 0), " cm depth at start year."))
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
        name <- "Territory mean maximum"
      }
      if (name == "all_locs_Apr1") {
        name <- "Territory mean April 1"
      }
      plot_meas <- meas[meas$location == locations$location[i] , -which(names(meas) == "end_datetime") ]
      plot_meas <- plot_meas[order(plot_meas$target_datetime), ]

      SWE <- plot_meas[plot_meas$param_name == "snow water equivalent", "value"]
      depth <- plot_meas[plot_meas$param_name == "snow depth", "value"]
      target_datetimes <- plot_meas[plot_meas$param_name == "snow water equivalent", "target_datetime"]
      years <- plot_meas[plot_meas$param_name == "snow water equivalent", "year"]
      months <- plot_meas[plot_meas$param_name == "snow water equivalent", "month"]
      
      density <- data.frame(timeseries_id = paste(unique(plot_meas$timeseries_id), collapse = ", "),
                            target_datetime = target_datetimes,
                            value = (SWE/10)/depth,
                            year = years,
                            month = months,
                            location_id = unique(plot_meas$location_id),
                            location = unique(plot_meas$location),
                            param_name = "density")
      
      plot_meas <- rbind(plot_meas, density)
                            

      plotSWE <- ggplot2::ggplot(data = plot_meas[plot_meas$param_name == "snow water equivalent" & plot_meas$value > 0, ], ggplot2::aes(x = .data$target_datetime, y = .data$value, group = .data$year)) +
        ggplot2::scale_x_datetime() +
        ggplot2::geom_point() +
        ggplot2::geom_line(linewidth = 0.1) +
        ggplot2::theme_classic()
      if (plot_type == "separate") {
        plotSWE <- plotSWE +
          ggplot2::labs(x = "Sample date", y = "SWE (mm)", title = paste0(locations$location[i], ": " , locations$name[i]))
      } else {
        plotSWE <- plotSWE +
          ggplot2::labs(y = "SWE (mm)", title = paste0(locations$location[i], ": " , locations$name[i])) +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())
      }

      plotDepth <- ggplot2::ggplot(data = plot_meas[plot_meas$param_name == "snow water equivalent" & plot_meas$value > 0 , ], ggplot2::aes(x = .data$target_datetime, y = .data$value, group = .data$year)) +
        ggplot2::scale_x_datetime() +
        ggplot2::geom_point(size = 1.75) +
        ggplot2::geom_line(linewidth = 0.1) +
        ggplot2::theme_classic()
      if (plot_type == "separate") {
        plotDepth <- plotDepth +
          ggplot2::labs(x = "Sample date", y = "Snow depth (cm)", title = paste0(locations$location[i], ": " , locations$name[i]))
      } else {
        plotDepth <- plotDepth +
          ggplot2::labs(y = "Snow depth (cm)") +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())
      }

      plotDensity <- ggplot2::ggplot(data = plot_meas[plot_meas$param_name == "density" & plot_meas$value > 0 , ], ggplot2::aes(x = .data$target_datetime, y = .data$value, group = .data$year)) +
        ggplot2::scale_x_datetime() +
        ggplot2::geom_point(size = 1.75) +
        ggplot2::geom_line(linewidth = 0.1) +
        ggplot2::theme_classic()
      if (plot_type == "separate") {
        plotDensity <- plotDensity +
          ggplot2::labs(x = "Sample date", y = bquote('Density (g/' ~cm^{"3"} *')'), title = paste0(locations$location[i], ": " , locations$name[i]))
      } else {
        plotDensity <- plotDensity +
          ggplot2::labs(x = "Sample date", y = bquote('Density (g/' ~cm^{"3"} *')'))
      }

      if (plot_type == "combined") {
        plots_combined <- gridExtra::arrangeGrob(plotSWE, plotDepth, plotDensity)
        plotsCombined[[name]] <- plots_combined
      } else {
        plotsSWE[[name]] <- plotSWE
        plotsDepth[[name]] <- plotDepth
        plotsDensity[[name]] <- plotDensity
      }
      if (!is.null(save_path)) {
        if (plot_type == "combined") {
          ggplot2::ggsave(filename = paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_combined.png"), plot = plots_combined, height = 10, width = 10, units = "in", device = "png", dpi = 500)
        } else {
          ggplot2::ggsave(filename = paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_SWE.png"), plot = plotSWE, height = 8, width = 12, units = "in", device = "png", dpi = 500)
          ggplot2::ggsave(filename = paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_DEPTH.png"), plot = plotDepth, height = 8, width = 12, units = "in", device = "png", dpi = 500)
          ggplot2::ggsave(filename = paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_DENSITY.png"), plot = plotDensity, height = 8, width = 12, units = "in", device = "png", dpi = 500)
        }
      }
    }

    if (plot_type == "combined") {
      message("Combined SWE, depth, density plots were returned in a list element. You can view each combined plot by calling grid::grid.draw on the desired object, or dig a bit deeper and find each individual ggplot object.")
    }
  } #End of plots loop

  
  if (all) {
    locations <- locations[!locations$location %in% c("all_locs_max", "all_locs_Apr1") , ]
    meas <- meas[!meas$location %in% c("all_locs_max", "all_locs_Apr1") , ]
  }

  #Fix up the location metadata table
  locations <- locations[, -which(names(locations) == "location_id")]
  names(locations) <- c("location_ID", "location_name", "latitude", "longitude", "elevation")
  
  locations$last_survey <- NA
  for (i in 1:nrow(locations)) {
    locations$last_survey[i] <- as.character(as.Date(max(tsids[tsids$location == locations$location_ID[i] , "end_datetime"])))
  }

  #Concatenate the various products into a list to return.
  if (stats) {
    if (all) {
      results <- list("locations" = locations, "stats" = stats_df, "trends" = trends, "territory_stats_trends" = territory, "measurements" = meas)
    } else {
      results <- list("locations" = locations, "stats" = stats_df, "trends" = trends, "measurements" = meas)
    }
  } else {
    results <- list("locations" = locations, "measurements" = meas)
  }
  if (!is.null(save_path)) {
    openxlsx::write.xlsx(results, paste0(save_path, "/SnowInfo_", Sys.Date(), "/measurements+stats.xlsx"))
  }

  if (plots) {
    if (plot_type == "combined") {
      results[[plots]] <- plots_combined

    } else if (plot_type == "separate") {
      results[[plots]] <- list("SWE" = plotsSWE,
                               "Depth" = plotsDepth,
                               "Density" = plotsDensity)
    }
  }
  return(results)
} #End of function
