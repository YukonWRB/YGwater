#' Plots and tabular data for hydrometric locations
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function is intended to facilitate the reporting of hydrology data by compiling basic statistics (years of record, months of operation, min, max, etc.), trend information (Mann-Kendall direction and p-value, Sen's slope), and creating simple plots of level (for lakes) or flow for all requested locations.
#'
#'@details
#' Sites that routinely see 0 values for flow or level in mid-winter may report a negative annual percent change *even if* the sens's slope is positive. This is due to the intercept of a linear model calculated using minimum flows and years being below 0 at the first year of record. Unfortunately there is no fix, but the Sen's value is still valid.
#'
#' @param con A connection to the database. Default uses function [AquaConnect()] with default settings.
#' @param locations The list of locations requested, as either a vector of location IDs/codes from table 'locations'. Default "all" fetches all stations.
#' @param level_flow Default 'both' will get and calculate level and flow information wherever possible. 'one' will pick flow where it exists, otherwise level. Exception to this is if there is no flow on the end_date requested AND most recent flow is > 1 month older than level ; in this case flow is assumed to be discontinued and level is used.
#' @param end_date The most recent day to include in calculations. Defaults to today.
#' @param months_min The month range in which to look for minimums. Does *not* currently support a month range overlapping two years.
#' @param months_max The month range in which to look for maximums.
#' @param allowed_missing The percent of data allowed to be missing and still return a minimum and/or maximum value. Applied separately to minimums and maximums for the months specified in 'months'.
#' @param plots Set TRUE if you want plots generated for each location (level or flow).
#' @param plot_type "combined" or "separate". Combined returns a single plot with yearly minimum and maximum on the same log-scale y axis, separate returns two stacked plots.
#' @param save_path The path where the .csv(s) and plots should be saved.
#' @param quiet Suppresses most messages and warnings.
#'
#' @return A list with four data.frames: location metadata, basic statistics, trend information, and daily measurements is returned to the R environment. In addition, an Excel workbook is saved to the save_path with the four data.frames as tabs, and a new folder created to hold level/flow plots for each station requested.
#'
#' @seealso [snowInfo()] for a similar function dealing with snowpack data.
#'
#' @export

waterInfo <- function(con = AquaConnect(), locations = "all", level_flow = "both", end_date = Sys.Date(), months_min = c(1:4), months_max = c(5:9), allowed_missing = 10, save_path = "choose", plots = TRUE, plot_type = "combined", quiet = FALSE)
  {
  
  # locations <- "all"
  # level_flow <- "one"
  # end_date <- "2025-01-08"
  # months_min <- c(1:4)
  # months_max <- c(5:9)
  # allowed_missing = 10
  # plots = TRUE
  # plot_type = "combined"
  # save_path = "C:/Users/gtdelapl/Desktop"
  # quiet = TRUE
  
  
  
  rlang::check_installed("trend", reason = "necessary to calculate trends.")

  if (!is.null(save_path)) {
    if (save_path %in% c("Choose", "choose")) {
      message("Select the path to the folder where you want this report saved.")
      save_path <- as.character(utils::choose.dir(caption = "Select Save Folder"))
    }
    dir.create(paste0(save_path, "/WaterInfo_", Sys.Date()))
    if (plots) {
      dir.create(paste0(save_path, "/WaterInfo_", Sys.Date(), "/plots"))
    }
  }

  if (!(level_flow %in% c("both", "one"))) {
    stop("Parameter level_flow must be either 'both' or 'one'")
  }

  end_date <- as.POSIXct(end_date)
  attr(end_date, "tzone") <- "UTC"
  
  params <- DBI::dbGetQuery(con, "SELECT parameter_id, param_name FROM parameters WHERE param_name IN ('water level', 'flow')")

  #select the locations
  if (locations[1] == "all") {
    locs <- DBI::dbGetQuery(con, paste0("SELECT * FROM timeseries WHERE parameter_id IN (", paste(params$parameter_id, collapse = ", "), ");"))
  } else {
    locs <- DBI::dbGetQuery(con, paste0("SELECT * FROM timeseries WHERE parameter_id IN (", paste(params$parameter_id, collapse = ", "), ") AND location IN ('", paste(locations, collapse = "', '"), "');"))
  }

  #now retain only level or flow if level_flow == 'one'.
  if (level_flow == "one") {
    for (i in unique(locs$location)) {
      sub <- locs[locs$location == i ,]
      if (nrow(sub) > 1) {
        if (params[params$param_name == "flow", "parameter_id"] %in% sub$parameter_id) {
          level_end <- sub[sub$parameter_id == params[params$param_name == "water level", "parameter_id"], "end_datetime"]
          if (level_end > end_date) {
            level_end <- end_date
          }
          flow_end <- sub[sub$parameter_id == params[params$param_name == "flow", "parameter_id"], "end_datetime"]
          if (flow_end > end_date) {
            flow_end <- end_date
          }
          if (flow_end > level_end - 6*30*24*60*60) { #check if last flow is recent enough
            locs <- locs[!(locs$location == i & locs$parameter_id == params[params$param_name == "water level", "parameter_id"]) , ] #drop level
          } else {
            locs <- locs[!(locs$location == i & locs$parameter_id == params[params$param_name == "flow", "parameter_id"]) , ] #drop flow
          }
        }
      }
    }
  }

  data <- list()
  extremes <- list()
  for (i in 1:nrow(locs)) {
    daily <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM measurements_calculated_daily_corrected WHERE timeseries_id = '", locs[i, "timeseries_id"], "' AND date < '", as.character(end_date), "'"))
    daily$year <- lubridate::year(daily$date)
    daily$month <- lubridate::month(daily$date)
    tryCatch({
      extremes[[paste0(locs$location[i], "_", locs$parameter_id[i])]] <- fasstr::calc_annual_extremes(data = daily, dates = date, values = value, months_min = months_min, months_max = months_max, allowed_missing = allowed_missing, water_year_start = 1)
      data[[paste0(locs$location[i], "_", locs$parameter_id[i])]] <- daily
    }, error = function(e) {
      message("Failed on location ", locs$location[i], " and parameter_id ", locs$parameter_id[i])
    })
  }

  #Several objects are initialized below as extremes is run once per element in extremes for all objects/outputs
  metadata <- data.frame()
  info <- data.frame()
  trends <- data.frame()
  if (plots) {
    plot_list <- list()
  }
  for (i in names(extremes)) {
    tbl <- extremes[[i]]
    name <- DBI::dbGetQuery(con, paste0("SELECT name FROM locations where location = '", sub("_.*", "", i), "'"))[1,1]
    #metadata
    metadata <- rbind(metadata,
                      data.frame("location" = sub("_.*", "", i),
                                 "name" = name,
                                 "parameter" = sub(".*_", "", i),
                                 "latitude" = DBI::dbGetQuery(con, paste0("SELECT latitude FROM locations where location = '", sub("_.*", "", i), "'"))[1,1],
                                 "longitude" = DBI::dbGetQuery(con, paste0("SELECT longitude FROM locations where location = '", sub("_.*", "", i), "'"))[1,1],
                                 "active" = max(data[[i]]$date) > as.Date(end_date) - 365,
                                 "note" = paste0("Last data available on ", max(data[[i]]$date), "."))
    )
    #info
    yrs_min <- lubridate::year(tbl$Min_1_Day_Date)
    yrs_min <- yrs_min[!is.na(yrs_min)]
    yrs_max <- lubridate::year(tbl$Max_1_Day_Date)
    yrs_max <- yrs_max[!is.na(yrs_max)]
    tryCatch({
      gaps_min <- seq(min(yrs_min), max(yrs_min))[!(seq(min(yrs_min), max(yrs_min)) %in% yrs_min)]
    }, error = function(e) {
      gaps_min <<- NA
    })
    tryCatch({
      gaps_max <- seq(min(yrs_max), max(yrs_max))[!(seq(min(yrs_max), max(yrs_max)) %in% yrs_max)]
    }, error = function(e) {
      gaps_max <<- NA
    })
    info <- rbind(info, data.frame("location" = sub("_.*", "", i),
                                   "name" = name,
                                   "parameter" = params[params$parameter_id == sub(".*_", "", i), "param_name"],
                                   "start" = min(min(yrs_min), min(yrs_max)),
                                   "end" = max(max(yrs_min), max(yrs_max)),
                                   "missing_yrs_min" = paste(gaps_min, collapse = ", "),
                                   "missing_yrs_max" = paste(gaps_max, collapse = ", "),
                                   "min_min" = round(min(tbl$Min_1_Day, na.rm = TRUE), 1),
                                   "mean_min" = round(mean(tbl$Min_1_Day, na.rm = TRUE), 1),
                                   "median_min" = round(stats::median(tbl$Min_1_Day, na.rm = TRUE), 1),
                                   "max_min" = round(max(tbl$Min_1_Day, na.rm = TRUE), 1),
                                   "min_max" = round(min(tbl$Max_1_Day, na.rm = TRUE), 1),
                                   "mean_max" = round(mean(tbl$Max_1_Day, na.rm = TRUE), 1),
                                   "median_max" = round(stats::median(tbl$Max_1_Day, na.rm = TRUE), 1),
                                   "max_max" = round(max(tbl$Max_1_Day, na.rm = TRUE), 1)
    ))

    #Trends
    tryCatch({
      minimum <- trend::sens.slope(tbl$Min_1_Day[!is.na(tbl$Min_1_Day)])
      sub <- tbl[!is.na(tbl$Min_1_Day) , ]
      lm <- stats::lm(formula = sub$Min_1_Day ~ sub$Year)
      min_intercept <- lm$coefficients[1] + lm$coefficients[2] * min(sub$Year)
    }, error = function(e) {
      minimum <<- data.frame("parameter" = NA,
                         "p.value" = NA,
                         "estimates" = NA)
      min_intercept <<- NA
    })
    tryCatch({
      maximum <- trend::sens.slope(tbl$Max_1_Day[!is.na(tbl$Max_1_Day)])
      sub <- tbl[!is.na(tbl$Max_1_Day) , ]
      lm <- stats::lm(formula = sub$Max_1_Day ~ sub$Year)
      max_intercept <- lm$coefficients[1] + lm$coefficients[2] * min(sub$Year)
    }, error = function(e) {
      maximum <<- data.frame("parameter" = NA,
                         "p.value" = NA,
                         "estimates" = NA)
      max_intercept <<- NA
    })
    trends <- rbind(trends, data.frame("location" = sub("_.*", "", i),
                                       "name" = name,
                                       "parameter" = params[params$parameter_id == sub(".*_", "", i), "param_name"],
                                       "n_min" = unname(minimum$parameter),
                                       "p.value_min" = round(unname(minimum$p.value), 3),
                                       "sens.slope_min" = round(unname(minimum$estimates), 3),
                                       "prct_annual_chg_min" = round(unname(minimum$estimates) / unname(min_intercept), 3),
                                       "n_max" = unname(maximum$parameter),
                                       "p.value_max" = round(unname(maximum$p.value), 3),
                                       "sens.slope_max" = round(unname(maximum$estimates), 3),
                                       "prct_annual_chg_max" = round(unname(maximum$estimates) / unname(max_intercept), 3),
                                       "note" = paste0("Percent annual change based on intercepts of ", round(min_intercept, 1), " at the start year for miniumum values and ", round(max_intercept, 1), " at the start year for maximum values"))
    )

    if (plots) {
      grDevices::pdf(NULL)
      
      x_lim <- c(min(min(yrs_min), min(yrs_max)), max(max(yrs_min), max(yrs_max)))
      plot <- ggplot2::ggplot(data = tbl, ggplot2::aes(x = .data$Year, y = .data$Min_1_Day)) +
        ggplot2::geom_point(color = "royalblue4") +
        ggplot2::geom_line(linewidth = 0.1, color = "royalblue4") +
        ggplot2::theme_classic() +
        ggplot2::xlim(x_lim)
      if (plot_type == "combined") {
        plot <- plot +
          ggplot2::geom_point(ggplot2::aes(y = .data$Max_1_Day), color = "red3") +
          ggplot2::geom_line(ggplot2::aes(y = .data$Max_1_Day), linewidth = 0.1, color = "red3")
        if (params[params$parameter_id == sub(".*_", "", i), "param_name"] == "flow") {
          plot <- plot +
            ggplot2::labs(y = "Annual extreme value (log scale)", title = paste0(sub("_.*", "", i), ": " , name), subtitle = paste0("Parameter: ", params[params$parameter_id == sub(".*_", "", i), "param_name"], ", m3/s")) +
            ggplot2::scale_y_log10()
        } else {
          plot <- plot +
            ggplot2::labs(y = "Annual extreme value", title = paste0(sub("_.*", "", i), ": " , name), subtitle = paste0("Parameter: ",params[params$parameter_id == sub(".*_", "", i), "param_name"], ", m relative to benchmark"))
        }

        plot_list[[i]] <- plot

      } else if (plot_type == "separate") {
        plot <- plot +
          ggplot2::labs(y = "Annual Minimum")

        plot2 <- ggplot2::ggplot(data = tbl, ggplot2::aes(x = .data$Year, y = .data$Max_1_Day)) +
          ggplot2::labs(y = "Annual Maximum", title = paste0(sub("_.*", "", i), ": " , name), subtitle = paste0("Parameter: ", params[params$parameter_id == sub(".*_", "", i), "param_name"])) +
          ggplot2::xlim(x_lim) +
          ggplot2::geom_point() +
          ggplot2::geom_line(linewidth = 0.1) +
          ggplot2::theme_classic() +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())
        plots_separate <- cowplot::plot_grid(plot2, plot, ncol = 1, align = "v", rel_heights = c(1, 1))
        plot_list[[i]] <- plots_separate
      }
      if (!is.null(save_path)) {
        # Make a directory for each location (level + flow plots will go in this folder if both are made)
        if (!dir.exists(paste0(save_path, "/WaterInfo_", Sys.Date(), "/plots/", sub("/", "_", name)))) {
          dir.create(paste0(save_path, "/WaterInfo_", Sys.Date(), "/plots/", sub("/", "_", name)), recursive = TRUE)
        }
        if (plot_type == "combined") {
          ggplot2::ggsave(filename = paste0(save_path, "/WaterInfo_", Sys.Date(), "/plots/", sub("/", "_", name), "/", sub("/", "_", params[params$parameter_id == sub(".*_", "", i), "param_name"]), "_combined.png"), plot = plot, height = 6, width = 8, units = "in", device = "png", dpi = 300)
        } else {
          ggplot2::ggsave(filename = paste0(save_path, "/WaterInfo_", Sys.Date(), "/plots/", sub("/", "_", name), "/", sub("/", "_", params[params$parameter_id == sub(".*_", "", i), "param_name"]),  "_separate.png"), plot = plots_separate, height = 6, width = 8, units = "in", device = "png", dpi = 300)
        }
      }
      grDevices::dev.off()
    } #End of plots loop
  }#End of loop working on extremes list of tables.
  if (plot_type == "combined" & plots & !quiet) {
    message("Minimum and maximum flow plots were combined and returned in a list element. You can view each combined plot by calling grid::grid.draw on the desired object, or dig a bit deeper and find each individual ggplot object.")
  }


  result <- list("locations" = metadata, "stats" = info, "trends" = trends)

  if (!is.null(save_path)) {
    openxlsx::write.xlsx(result, paste0(save_path, "/WaterInfo_", Sys.Date(), "/measurements+stats.xlsx"))
  }

  if (plots) {
    result <- list("locations" = metadata, "stats" = info, "trends" = trends, "plots" = plot_list)
  }
  return(result)

} #End of function
