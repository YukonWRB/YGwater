#' Plots and tabular data for snow survey locations
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function is intended to facilitate the reporting of snow survey data by compiling basic statistics (years of record, missing years, mean, max, etc.), trend information (Mann-Kendall direction and p-value, Sen's slope), and creating simple plots of SWE, depth, and density for all requested stations. At its most basic (parameters to FALSE or NULL where applicable), the result is a list of two data.frames to the R environment with location metadata and field measurements.
#'
#' @param db_path The path to the local Snow Survey database including extension, passed to [snowConnect()].
#' @param locations The list of locations requested, as a character vector of length n. Default "all" fetches all stations.
#' @param inactive Boolean specifying whether to include inactive stations. For 10AD-SC01 and 09BA-SC02 which require conversion factors due to moved measurement locations, this filter is applied after conversion. Therefore, if set to TRUE while 10AD-SC01B or 09BA-SC02B are active then the returned data will include measurements taken at 10AD-SC01 and 09BA-SC02A under their respective current "sister" locations, with conversion factors applied.
#' @param save_path The path where the .csv(s) and plots should be saved. Set to NULL for data only as an R object. Plots are not created if there is no save path.
#' @param stats set TRUE if you want basic statistics (mean, min, max) and calculated trends.
#' @param complete_yrs Should only years with complete data be used? If TRUE, will not use the current year's data until after May.
#' @param plots Set TRUE if you want plots of SWE, depth, and density generated (but see next parameter).
#' @param plot_type Set to "separate" for 3 plots per location, or "combined" for a single compound plot per location.
#' @param quiet Suppresses most messages and warnings.
#'
#' @return A list with four data.frames: location metadata, basic statistics, trend information, and snow course measurements is returned to the R environment. In addition, an Excel workbook is saved to the save_path with the four data.frames, and a new folder created to hold SWE and depth plots for each station requested.
#'
#' @seealso [waterInfo()] for a similar function dealing with water flow/level.
#' @export
#'
#TODO: This function should really be getting data from the hydro database.

snowInfo <- function(db_path ="default", locations = "all", inactive = FALSE, save_path = "choose", stats = TRUE, complete_yrs = TRUE, plots = TRUE, plot_type = "combined", quiet = FALSE) {

  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the path to the folder where you want this report saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
    dir.create(paste0(save_path, "/SnowInfo_", Sys.Date()))
    if (plots){
      dir.create(paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots"))
    }
  }


  if (!(plot_type %in% c("separate", "combined"))){
    stop("The parameter 'plot_type' must be set to either 'separate' or 'combined'.")
  }

  snowCon <- snowConnect_access(path = db_path, silent=TRUE)
  on.exit(DBI::dbDisconnect(snowCon))

  location_table <- DBI::dbReadTable(snowCon, "SNOW_COURSE")
  if (locations[1] == "all"){
    locations <- location_table
    all <- TRUE
  } else {
    check_locs <- locations[!(locations %in% location_table$SNOW_COURSE_ID)]
    if (length(check_locs) > 0){
      print(paste0("Could not find a record for location ", check_locs, ". Other locations will be returned."))
    }
    locations <- location_table[location_table$SNOW_COURSE_ID %in% locations , ]
  }


  #Get the measurements
  meas <- DBI::dbGetQuery(snowCon, paste0("SELECT * FROM SNOW_SAMPLE WHERE SNOW_COURSE_ID IN ('", paste(locations$SNOW_COURSE_ID, collapse = "', '"), "')"))

  #Manipulate/preprocess things a bit
  meas <- meas[which(meas$EXCLUDE_FLG==0),] # OMIT VALUES OF EXCLUDEFLG=1, aka TRUE
  meas$SAMPLE_DATE <- as.Date(meas$SAMPLE_DATE)
  meas$year <- lubridate::year(meas$SAMPLE_DATE)
  meas$month <- lubridate::month(meas$SAMPLE_DATE)

  #Deal with special cases
  if ("09BA-SC02A" %in% locations$SNOW_COURSE_ID & "09BA-SC02B" %in% locations$SNOW_COURSE_ID){
    # Special case (i) Twin Creeks: 09BA-SC02B will take precedence over A from 2016 onwards.
    # Calculate correction factors:
    subset <- meas[meas$SNOW_COURSE_ID %in% c("09BA-SC02A", "09BA-SC02B"),]
    duplicated <- data.frame(table(subset$SAMPLE_DATE))
    duplicated <- duplicated[duplicated$Freq > 1 , ]
    duplicated <- as.Date(as.vector(duplicated$Var1))
    swe_factor <- NULL
    for (i in 1:length(duplicated)){
      a <- subset[subset$SNOW_COURSE_ID == "09BA-SC02A" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
      b <- subset[subset$SNOW_COURSE_ID == "09BA-SC02B" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
      swe_factor[i] <- 1 + (b-a)/a
    }
    depth_factor <- NULL
    for (i in 1:length(duplicated)){
      a <- subset[subset$SNOW_COURSE_ID == "09BA-SC02A" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
      b <- subset[subset$SNOW_COURSE_ID == "09BA-SC02B" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
      depth_factor[i] <- 1 + (b-a)/a
    }
    swe_correction <- mean(swe_factor)
    depth_correction <- mean(depth_factor)

    # Remove 09BA-SC02A values in 2016 and later.
    meas <- meas[!(meas$SNOW_COURSE_ID=="09BA-SC02A" & meas$year >= 2016),]
    # Multiply all 09BA-SC02A values by correction factors:
    meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- swe_correction*(meas[meas$SNOW_COURSE_ID=="09BA-SC02A", "SNOW_WATER_EQUIV"])
    meas$DEPTH[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- depth_correction*(meas[meas$SNOW_COURSE_ID=="09BA-SC02A", "DEPTH"])
    # Rename as 09BA-SC02B (A will no longer exist here)
    meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- "09BA-SC02B"
    locations <- locations[!(locations$SNOW_COURSE_ID == "09BA-SC02A") , ]
    corrected <- TRUE
  } else if (("09BA-SC02A" %in% locations$SNOW_COURSE_ID | "09BA-SC02B" %in% locations$SNOW_COURSE_ID ) & !quiet) {
    print("Be careful with stations 09BA-SC02A and B. A is no longer active. When requesting data from both, a correction factor determined by operating the stations in parallel over several years is applied to A, and the result reported as 09BA-SC02B. Since you requested only data from A or B, no correction was applied.")
  }

  if ("10AD-SC01" %in% locations$SNOW_COURSE_ID & "10AD-SC01B" %in% locations$SNOW_COURSE_ID){
    # Special case (ii) Hyland 10AD-SC01 and 10AD-SC01B. B will take precedence over (no letter) from 2018 onwards.
    # Calculate correction factors:
    subset <- meas[meas$SNOW_COURSE_ID %in% c("10AD-SC01", "10AD-SC01B"),]
    duplicated <- data.frame(table(subset$SAMPLE_DATE))
    duplicated <- duplicated[duplicated$Freq > 1 , ]
    duplicated <- as.Date(as.vector(duplicated$Var1))
    swe_factor <- NULL
    for (i in 1:length(duplicated)){
      a <- subset[subset$SNOW_COURSE_ID == "10AD-SC01" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
      b <- subset[subset$SNOW_COURSE_ID == "10AD-SC01B" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
      swe_factor[i] <- 1 + (b-a)/a
    }
    depth_factor <- NULL
    for (i in 1:length(duplicated)){
      a <- subset[subset$SNOW_COURSE_ID == "10AD-SC01" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
      b <- subset[subset$SNOW_COURSE_ID == "10AD-SC01B" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
      depth_factor[i] <- 1 + (b-a)/a
    }
    swe_correction <- mean(swe_factor)
    depth_correction <- mean(depth_factor)

    #Step 1: Remove SC01 blank values in 2018 and later.
    meas <- meas[!(meas$SNOW_COURSE_ID == "10AD-SC01" & meas$year >= 2018),]
    #Step 2: Multiply all remaining SC01 values by correction factors:
    meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID == "10AD-SC01"] <- swe_correction*(meas[meas$SNOW_COURSE_ID=="10AD-SC01", "SNOW_WATER_EQUIV"])
    meas$DEPTH[meas$SNOW_COURSE_ID=="10AD-SC01"] <- depth_correction*(meas[meas$SNOW_COURSE_ID=="10AD-SC01", "DEPTH"])
    # Step 3: Rename as 010AD-SC01B (blank will no longer exist)
    meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="10AD-SC01"] <- "10AD-SC01B"
    corrected <- TRUE
    locations <- locations[!(locations$SNOW_COURSE_ID == "10AD-SC01") , ]
  } else if (("10AD-SC01" %in% locations$SNOW_COURSE_ID | "10AD-SC01B" %in% locations$SNOW_COURSE_ID) & !quiet) {
    print("Be careful with stations 10AD-SC01 (no letter) and 10AD-SC01B. The first is no longer active. When requesting data from both, a correction factor determined by operating the stations in parallel over several years is applied to the first, and the result reported as 10AD-SC01B. Since you requested only data from (no letter) or B, no correction was applied.")
  }

  if (!inactive){ #Filter out the inactive stations if inactive is FALSE
    remove <- locations[locations$ACTIVE_FLG==TRUE,]$SNOW_COURSE_ID
    meas <- meas[meas$SNOW_COURSE_ID %in% remove , ]
    locations <- locations[locations$ACTIVE_FLG == TRUE ,]
  }
  if (corrected & !quiet){
    print("Warning: locations 09BA-SC02B and/or 10AD-SC01B are in fact composites of defunct locations 09BA-SC02A and/or 10AD-SC01. A correction factor (determined by operating locations in parallel over several years) was applied to defunct location data to make it comparable to the new locations.")
  }


  if (stats){
    #Calculate station basic stats: min, max, mean, median, total yrs, gaps
    stats_df <- data.frame()
    for (i in 1:nrow(locations)){
      yrs <- lubridate::year(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$SAMPLE_DATE)
      if (lubridate::month(Sys.Date()) %in% c(1:5) & complete_yrs){
        yrs <- yrs[!yrs == lubridate::year(Sys.Date())]
      }
      total_yrs <- max(yrs) - min(yrs)
      gaps <- seq(min(yrs), max(yrs))[!(seq(min(yrs), max(yrs)) %in% yrs)]
      sample_months <- sort(unique(lubridate::month(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$SAMPLE_DATE, label = TRUE, abbr = TRUE)))
      allMaxSWE <- max(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$SNOW_WATER_EQUIV, na.rm=TRUE)
      allMaxDepth <- max(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$DEPTH, na.rm=TRUE)

      depthMaxes <- NULL
      SWEMaxes <- NULL
      for (j in unique(yrs)){
        subset <- meas[meas$year == j & meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i], ]
        months <- unique(subset$month)
        if (3 %in% months & 4 %in% months){
          subsetDepth <- max(subset$DEPTH, na.rm=TRUE)
          subsetSWE <- max(subset$SNOW_WATER_EQUIV, na.rm=TRUE)
          depthMaxes <- c(depthMaxes, subsetDepth)
          SWEMaxes <- c(SWEMaxes, subsetSWE)
        }
      }

      medianMaxDepth <- stats::median(depthMaxes)
      meanMaxDepth <- mean(depthMaxes)
      medianMaxSWE <- stats::median(SWEMaxes)
      meanMaxSWE <- mean(SWEMaxes)

      stats_df <- rbind(stats_df,
                     data.frame("location_ID" = locations$SNOW_COURSE_ID[i],
                                "total_record_yrs" = total_yrs,
                                "start" = min(yrs),
                                "end" = max(yrs),
                                "missing_yrs" = paste(gaps, collapse=", ", sep = ", "),
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
    for (i in 1:nrow(locations)){
      yrs <- unique(lubridate::year(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]$SAMPLE_DATE))
      if (lubridate::month(Sys.Date()) %in% c(1:5) & complete_yrs){
        yrs <- yrs[!yrs == lubridate::year(Sys.Date())]
      }
      AllSWEMax <- numeric(0)
      for (j in yrs){
        AllSWEMax <- c(AllSWEMax, max(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] & lubridate::year(meas$SAMPLE_DATE) == j, ]$SNOW_WATER_EQUIV))
      }
      AllSWEMax <- stats::na.omit(hablar::rationalize(AllSWEMax))
      if (length(AllSWEMax) > 6){
        AllSWESensMax <- trend::sens.slope(AllSWEMax)
      } else {
        AllSWESensMax$estimates <- NA
        AllSWESensMax$p.value <- NA
      }

      AllDepthMax <- numeric(0)
      for (j in yrs){
        AllDepthMax <- c(AllDepthMax, max(meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] & lubridate::year(meas$SAMPLE_DATE) == j, ]$DEPTH))
      }
      AllDepthMax <- stats::na.omit(hablar::rationalize(AllDepthMax))
      if(length(AllDepthMax) > 6) {
        AllDepthSensMax <- trend::sens.slope(AllDepthMax)
      } else {
        AllDepthSensMax$estimates <- NA
        AllDepthSensMax$p.value <- NA
      }

      trends <- rbind(trends,
                      data.frame("location_ID" = locations$SNOW_COURSE_ID[i],
                                 "p.value_SWE_max" = round(unname(AllSWESensMax$p.value), 3),
                                 "sens.slope_SWE_max" = round(unname(AllSWESensMax$estimates), 3),
                                 "n_years_SWE" = AllSWESensMax$parameter,
                                 "p.value_DEPTH_max" = round(unname(AllDepthSensMax$p.value), 3),
                                 "sens.slope_DEPTH_max" = round(unname(AllDepthSensMax$estimates), 3),
                                 "n_years_DEPTH" = AllDepthSensMax$parameter
                      ))
    }

    for (i in 1:nrow(trends)){
      subset <- meas[meas$SNOW_COURSE_ID == trends$location_ID[i] & meas$SNOW_WATER_EQUIV > 0,]
      intercept_yr <- min(subset$year)
      intercept_value_SWE <- unname(stats::lm(formula = subset$SNOW_WATER_EQUIV ~ subset$SAMPLE_DATE)$coefficients[1])
      intercept_value_depth <- unname(stats::lm(formula = subset$DEPTH ~ subset$SAMPLE_DATE)$coefficients[1])
      trends$annual_prct_chg_SWE[i] <- trends[trends$location_ID == trends$location_ID[i] , "sens.slope_SWE_max"] / intercept_value_SWE
      trends$annual_prct_chg_DEPTH[i] <- trends[trends$location_ID == trends$location_ID[i] , "sens.slope_DEPTH_max"] / intercept_value_depth
      trends$note[i] <- paste0("Prct chg based on linear model intercepts of ", round(intercept_value_SWE, 1), " and ", round(intercept_value_depth,1), " for SWE and depth at the start year.")
    }

    if (all){
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
      for (i in yrs){
        yearMaxSWE <- NULL
        yearMaxDepth <- NULL
        yearApr1SWE <- NULL
        yearApr1Depth <- NULL
        for (j in 1:nrow(locations)){
          subset <- meas[meas$year == i & meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[j] , ]
          months <- unique(subset$month)
          add <- FALSE
          if (3 %in% months & 4 %in% months){
            locationSWE <- max(subset$SNOW_WATER_EQUIV, na.rm=TRUE)
            locationDepth <- max(subset$DEPTH, na.rm=TRUE)
            add <- TRUE
          }
          if (add){
            yearMaxSWE <- c(yearMaxSWE, locationSWE)
            yearMaxDepth <- c(yearMaxDepth, locationDepth)
          }
          if (4 %in% months){
            locApr1SWE <- subset[subset$SAMPLE_DATE == paste0(i, "-04-01") ,"SNOW_WATER_EQUIV"]
            locApr1Depth <- subset[subset$SAMPLE_DATE == paste0(i, "-04-01") ,"DEPTH"]
            yearApr1SWE <- c(yearApr1SWE, locApr1SWE)
            yearApr1Depth <- c(yearApr1Depth, locApr1Depth)
          }
        }
        if (!is.null(yearMaxSWE) & !is.null(yearMaxDepth) & length(yearMaxSWE) > nrow(locations)/2){
          meanMaxSWE <- c(meanMaxSWE, mean(yearMaxSWE))
          meanMaxDepth <- c(meanMaxDepth, mean(yearMaxDepth))
        }
        if (!is.null(yearApr1SWE) & !is.null(yearApr1Depth)){
          meanApr1SWE <- c(meanApr1SWE, mean(yearApr1SWE))
          meanApr1Depth <- c(meanApr1Depth, mean(yearApr1Depth))
        }
      }

      meanMaxSWESens <- trend::sens.slope(meanMaxSWE)
      meanMaxDepthSens <- trend::sens.slope(meanMaxDepth)
      meanApr1SWESens <- trend::sens.slope(meanApr1SWE)
      meanApr1DepthSens <- trend::sens.slope(meanApr1Depth)

      plot_all <- data.frame("SNOW_COURSE_ID" = "all_locs_max",
                             "SAMPLE_DATE" = as.Date(paste0(seq(min(yrs), min(yrs) + length(meanMaxSWE) - 1), "-01-01")),
                             "SNOW_WATER_EQUIV" = meanMaxSWE,
                             "DEPTH" = meanMaxDepth)
      plot_apr1 <- data.frame("SNOW_COURSE_ID" = "all_locs_Apr1",
                              "SAMPLE_DATE" = as.Date(paste0(seq(min(yrs), min(yrs) + length(meanApr1SWE) - 1), "-04-01")),
                              "SNOW_WATER_EQUIV" = meanApr1SWE,
                              "DEPTH" = meanApr1Depth)
      new_all <- data.frame("SNOW_COURSE_ID" = "all_locs_max",
                            "SNOW_COURSE_NAME" = "Territory-averaged maximum")
      new_apr1 <- data.frame("SNOW_COURSE_ID" = "all_locs_Apr1",
                            "SNOW_COURSE_NAME" = "Territory-averaged April 1")
      meas <- plyr::rbind.fill(meas, plot_all)
      meas <- plyr::rbind.fill(meas, plot_apr1)
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

  if (plots){
    plotsSWE <- list()
    plotsDepth <- list()
    plotsDensity <- list()
    plotsCombined <- list()

    for (i in 1:nrow(locations)){
      name <- locations$SNOW_COURSE_ID[i]
      if (name == "all_locs_max") {
        name <- "Territory mean maximum"
      }
      if (name == "all_locs_Apr1") {
        name <- "Territory mean April 1"
      }
      plot_meas <- meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]
      plot_meas$density <- (plot_meas$SNOW_WATER_EQUIV / 10) / plot_meas$DEPTH

      plotSWE <- ggplot2::ggplot(data=plot_meas[plot_meas$SNOW_WATER_EQUIV > 0 , ], ggplot2::aes(x = .data$SAMPLE_DATE, y = .data$SNOW_WATER_EQUIV, group = .data$year)) +
        ggplot2::scale_x_date() +
        ggplot2::geom_point() +
        ggplot2::geom_line(linewidth = 0.1) +
        ggplot2::theme_classic()
      if (plot_type == "separate"){
        plotSWE <- plotSWE +
          ggplot2::labs(x = "Sample date", y = "SWE (mm)", title = paste0(locations$SNOW_COURSE_ID[i], ": " , locations$SNOW_COURSE_NAME[i]))
      } else {
        plotSWE <- plotSWE +
          ggplot2::labs(y = "SWE (mm)", title = paste0(locations$SNOW_COURSE_ID[i], ": " , locations$SNOW_COURSE_NAME[i])) +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())
      }

      plotDepth <- ggplot2::ggplot(data=plot_meas[plot_meas$DEPTH > 0 , ], ggplot2::aes(x = .data$SAMPLE_DATE, y = .data$DEPTH, group = .data$year)) +
        ggplot2::scale_x_date() +
        ggplot2::geom_point(size = 1.75) +
        ggplot2::geom_line(linewidth = 0.1)+
        ggplot2::theme_classic()
      if (plot_type == "separate"){
        plotDepth <- plotDepth +
          ggplot2::labs(x = "Sample date", y = "Snow depth (cm)", title = paste0(locations$SNOW_COURSE_ID[i], ": " , locations$SNOW_COURSE_NAME[i]))
      } else {
        plotDepth <- plotDepth +
          ggplot2::labs(y = "Snow depth (cm)") +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())
      }

      plotDensity <- ggplot2::ggplot(data=plot_meas[plot_meas$DEPTH > 0 , ], ggplot2::aes(x = .data$SAMPLE_DATE, y = .data$density, group = .data$year)) +
        ggplot2::scale_x_date() +
        ggplot2::geom_point(size = 1.75) +
        ggplot2::geom_line(linewidth = 0.1)+
        ggplot2::theme_classic()
      if (plot_type == "separate"){
        plotDensity <- plotDensity +
          ggplot2::labs(x = "Sample date", y = bquote('Density (g/' ~cm^{"3"} *')'), title = paste0(locations$SNOW_COURSE_ID[i], ": " , locations$SNOW_COURSE_NAME[i]))
      } else {
        plotDensity <- plotDensity +
          ggplot2::labs(x = "Sample date", y = bquote('Density (g/' ~cm^{"3"} *')'))
      }

      if (plot_type == "combined"){
        plots_combined <- gridExtra::arrangeGrob(plotSWE, plotDepth, plotDensity)
        plotsCombined[[name]] <- plots_combined
      } else {
        plotsSWE[[name]] <- plotSWE
        plotsDepth[[name]] <- plotDepth
        plotsDensity[[name]] <- plotDensity
      }
      if (!is.null(save_path)){
        if (plot_type == "combined"){
          ggplot2::ggsave(filename = paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_combined.png"), plot=plots_combined, height=10, width=10, units="in", device="png", dpi=500)
        } else {
          ggplot2::ggsave(filename=paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_SWE.png"), plot=plotSWE, height=8, width=12, units="in", device="png", dpi=500)
          ggplot2::ggsave(filename=paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_DEPTH.png"), plot=plotDepth, height=8, width=12, units="in", device="png", dpi=500)
          ggplot2::ggsave(filename=paste0(save_path, "/SnowInfo_", Sys.Date(), "/plots/", name, "_DENSITY.png"), plot=plotDensity, height=8, width=12, units="in", device="png", dpi=500)
        }
      }
    }

    if (plot_type == "combined"){
      print("Combined SWE, depth, density plots were returned in a list element. You can view each combined plot by calling grid::grid.draw on the desired object, or dig a bit deeper and find each individual ggplot object.")
    }
  } #End of plots loop

  if (all) {
    locations <- locations[!locations$SNOW_COURSE_ID %in% c("all_locs_max", "all_locs_Apr1") , ]
    meas <- meas[!meas$SNOW_COURSE_ID %in% c("all_locs_max", "all_locs_Apr1") , ]
  }

  #Fix up the location metadata table
  locations$LATITUDE_SEC[is.na(locations$LATITUDE_SEC)] <- as.numeric(0)
  latitude <- locations$LATITUDE_DEG + locations$LATITUDE_MIN/60 + locations$LATITUDE_SEC/3600
  locations$LONGITUDE_SEC[is.na(locations$LONGITUDE_SEC)] <- as.numeric(0)
  longitude <- -1 * abs(locations$LONGITUDE_DEG + locations$LONGITUDE_MIN/60 + locations$LONGITUDE_SEC/3600) #currently all longitudes are not negative, but if it changes then -1*abs will ensure all are rendered negative regardless.
  locations <- locations[ , c("SNOW_COURSE_ID", "SNOW_COURSE_NAME", "ACTIVE_FLG", "ELEVATION")]
  locations <- cbind(locations, latitude, longitude)
  names(locations) <- c("location_ID", "location_name", "active", "elevation_m", "latitude", "longitude")

  #Concatenate the various products into a list to return.
  if (stats){
    if (all){
      results <- list("locations" = locations, "stats" = stats_df, "trends" = trends, "territory_stats_trends" = territory, "measurements" = meas)
    } else {
      results <- list("locations" = locations, "stats" = stats_df, "trends" = trends, "measurements" = meas)
    }
  } else {
    results <- list("locations" = locations, "measurements" = meas)
  }
  if (!is.null(save_path)){
    openxlsx::write.xlsx(results, paste0(save_path, "/SnowInfo_", Sys.Date(), "/measurements+stats.xlsx"))
  }

  if (plots){
    if (plot_type == "combined"){
      results[[plots]] <- plots_combined

    } else if (plot_type == "separate"){
      results[[plots]] <- list("SWE" = plotsSWE,
                               "Depth" = plotsDepth,
                               "Density" = plotsDensity)
    }
  }
  return(results)
} #End of function
