#' Level and Flow condition reporting utility - internal
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function generates condition reports for preset or user-specified Water Survey of Canada stations. In addition to water level and flow, precipitation data, still images, and weather forecasts are incorporated. The output is a Microsoft Word document on a Yukon Government template.
#'
#' This function fetches data directly from the Water Survey of Canada and does not use the local hydrometric database created/maintained by the HydroMetDB package.
#'
#' @details
#' To download real-time WSC data, you MUST have your hydat credentials loaded
#' into your .Renviron profile as values pairs of WS_USRNM=”your_username” and
#' WS_PWD=”your_password”.
#'
#' To download WSC images, you MUST have your ECCC credentials loaded into your .Renviron profile as value pairs of ECCCUSER="your_username" and ECCCPASS="your_password".  Refer to the R and GitHub for the WRB word document for more information.
#'
#' @param report_name The name of the report you wish to generate. One of "Dawson", "Southern Lakes", Carmacks", "Ross/Pelly", "Mayo/Stewart", "Liard/Watson Lake", "Teslin", "Old Crow", "Aishihik", "Alsek", or "Territory". Most minor spelling variations should work. Leave as NULL (default) if specifying stations under custom_report_stations.
#' @param custom_report_stations A user-specified list of stations for which to generate a report. Defaults to NULL to operate on the report_name parameter instead. Input must be a character vector of station IDs, as in c("station1", "station2"). Reminder: you can create a character vector from a column of a data.frame, and you can reference an environment object instead of typing in the vector!
#' @param extra_years Specify extra years of data to plot for one or multiple stations. Use the form "09AB001:1990,2020", "09EB003:1985". Concatenate if more than one station. Can be used together with preset_extra_years.
#' @param preset_extra_years TRUE or FALSE, defaults to FALSE. Can be used together with extra_years.
#' @param report_type What do you want your report to contain? Choose from "Level", "Flow", or "Both." Defaults to Both.
#' @param plot_titles Do you want the plots to have a title?
#' @param level_zoom Do you want a zoomed-in plot for level? Choose from TRUE or FALSE. Defaults to TRUE.
#' @param flow_zoom Do you want a zoomed-in plot for flow? TRUE/FALSE/NULL, defaults to NULL which copies the setting for level_zoom.
#' @param zoom_days The number of days to plot for zoomed in level plots. Defaults to 30, but not used unless level_zoom is set to TRUE.
#' @param MESH Should MESH forecasts be incorporated into the graphs?
#' @param CLEVER Should CLEVER forecasts be incorporated into the graphs?
#' @param flow_returns Should flow returns be calculated, plotted, and added to the flows table? You have the option of using pre-determined flow returns only (option "table"), auto-calculated values with no human verification (option "auto", calculated on-the-fly using all data available from March to September, up to the current date), "both" (with priority to pre-determined flows), or none (option "none"). Defaults to "both".
#' @param level_returns Should level returns be calculated, plotted, and added to the level table? You have the option of using pre-determined level returns only (option "table"), auto-calculated values with no human verification (option "calculated", calculated on-the-fly using all data available from March to September, up to the current date), or "auto" (priority to pre-determined levels, fallback to auto-calculated levels), or none (option "none"). Defaults to "auto".
#' @param rate Should rates of change for flow and level be included? The 24-hour rate of change will be plotted on zoomed-in graphs on the right y-axis, and a rate table will be included. TRUE/FALSE, defaults to TRUE.
#' @param meteogram Should meteograms relevant to the stations in the report be generated? TRUE or FALSE.
#' @param image_path The path to the directory (folder) containing the images you wish to include. Default to NULL to not include any extra images. Set to "choose" to navigate to the folder, or enter the folder path directly as a character string. Some reports automatically include web-hosted images, do not include them here.
#' @param save_path The path to the directory (folder) where the report should be saved. Default "choose" lets you select your folder, otherwise enter the path as a character string.
#'
#' @return A flood report containing flow and water level information in Microsoft Word format.
#'
#' @export
#'

#TODO: add some error catching if the inputs do not match what is expected. ELSE statement? tryCatch?

floodReport <-
  function(report_name = NULL,
           custom_report_stations = NULL,
           extra_years = NULL,
           preset_extra_years = FALSE,
           report_type = "Both",
           plot_titles = FALSE,
           level_zoom = TRUE,
           flow_zoom = NULL,
           zoom_days = 30,
           MESH = TRUE,
           CLEVER = TRUE,
           flow_returns = "auto",
           level_returns = "auto",
           rate = TRUE,
           meteogram = TRUE,
           image_path = NULL,
           save_path = "choose") {

    #initial checks
    rlang::check_installed("R.utils", reason = "necessary for this function to download WSC images.")
    if (!rlang::is_installed("R.utils")) { #This is here because R.utils is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
      message("Installing dependency 'R.utils'...")
      utils::install.packages("R.utils")
      if (rlang::is_installed("R.utils")){
        message("Package R.utils successfully installed.")
      } else {
        stop("Failed to install package R.utils You could troubleshoot by running utils::install.packages('R.utils') by itself.")
      }
    }

    rlang::check_installed("imager", reason = "necessary for this function to download precip rasters.")
    if (!rlang::is_installed("imager")) { #This is here because imager is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
      message("Installing dependency 'imager'...")
      utils::install.packages("imager")
      if (rlang::is_installed("imager")){
        message("Package imager successfully installed.")
      } else {
        stop("Failed to install package imager You could troubleshoot by running utils::install.packages('imager') by itself.")
      }
    }

    rlang::check_installed("purrr", reason = "necessary for the report to run.")
    if (!rlang::is_installed("purrr")) { #This is here because purrr is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
      message("Installing dependency 'purrr'...")
      utils::install.packages("purrr")
      if (rlang::is_installed("purrr")){
        message("Package purrr successfully installed.")
      } else {
        stop("Failed to install package purrr You could troubleshoot by running utils::install.packages('purrr') by itself.")
      }
    }

    rlang::check_installed("knitr", reason = "necessary to create a report using Rmarkdown.")
    if (!rlang::is_installed("knitr")) { #This is here because knitr is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
      message("Installing dependency 'knitr'...")
      utils::install.packages("knitr")
      if (rlang::is_installed("knitr")){
        message("Package knitr successfully installed.")
      } else {
        stop("Failed to install package knitr You could troubleshoot by running utils::install.packages('knitr') by itself.")
      }
    }

    #####Selection of image path and save path#####
    if (is.null(image_path) == FALSE) {
      if (image_path == "choose") {
        message("Select the path to the folder containing your images.")
        image_path <- utils::choose.dir( caption="Select Image Folder")
      }
    }
    if (save_path == "choose") {
      message("Select the path to the folder where you want this report saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }

    #####Set flow_zoom#####
    if (is.null(flow_zoom)){
      flow_zoom=level_zoom
    }

    #####Generate reports#####
    if (is.null(report_name) == FALSE & is.null(custom_report_stations) == FALSE) {
      #deals with mistakes
      message("You specified custom report stations while the preset report was also set. I've set the preset to Custom Hydrometric Report so you get a custom report instead.")
      report_name <- "Custom Hydrometric Report"
    }

    if (is.null(report_name) == FALSE & is.null(custom_report_stations) == TRUE) {

      ### Generate a report for the whole territory###
      if (report_name %in% c("Territory", "territory", "Communities", "communities", "Yukon", "Yukon Wide", "Yukon wide", "yukon wide")) {
        stations <- c("09AH001", "09AH004", "09EA003", "09EB001", "09DC006", "09FD003", "09BC001", "09BC002", "09AE002", "10AA001", "09AB001", "09AB004", "09AB010", "09AA004", "09AA017")
        preset_extras <- c("09EA003:2013,1972","09EB001:2013,1964", "09AH001:2021,1992","09AH004:2021","09AE002:1962,1992,2022", "09BC002:2013,1992,1972", "09FD003:2007,2015", "10AA001:2007,2012,2013", "09DC006:1992,1983,2013", "09AB004:2007,2021", "09AB010:2007,2021")

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Yukon Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Yukon Conditions Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate = rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
        )
      } #End of territory report

      ### Generate a report for Dawson###
      if (report_name %in% c("Dawson", "dawson", "Dawson City", "Dawson city")) {
        stations <- c("09EA003", "09EA006", "09EA004", "09EA005", "09EB001", "09EB003", "09EB004", "09DD003", "09CD001", "09CB001")
        preset_extras <- c("09EA003:2013,1972","09EB001:2013,1964")

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

          rmarkdown::render(
            input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
            output_file = paste0("Dawson Condition Report ", Sys.Date()),
            output_dir = save_path,
            params = list(
              stations = stations,
              report_name = "Dawson Condition Report",
              extra_years = extra_years,
              image_path = image_path,
              report_type = report_type,
              level_zoom = level_zoom,
              flow_zoom = flow_zoom,
              zoom_days = zoom_days,
              MESH = MESH,
              CLEVER = CLEVER,
              flow_returns = flow_returns,
              level_returns = level_returns,
              rate = rate,
              meteogram = meteogram,
              plot_titles = plot_titles)
          )
      } #End of Dawson report

      ### Generate a report for Carmacks###
      if (report_name %in% c("Carmacks", "carmacks")) {
        stations <-c ("09AH001", "09AH004", "09AG001", "09AH005", "09AB010", "09AC001", "09AE002")
        preset_extras <- c("09AH001:2021,1992","09AH004:2021")

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Carmacks Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Carmacks Condition Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate = rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
        )
      } #End of Carmacks report

      ### Generate a report for Teslin###
      if (report_name %in% c("Teslin", "teslin")) {
        stations <-c ("09AE002", "09AE006", "09AE003", "09AD002", "10AC005")
        preset_extras <- "09EA002:1962,1992,2021"

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Teslin Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Teslin Condition Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate = rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
        )
      } #End of Carmacks report

      ### Generate a report for Pelly/Ross###
      if (report_name %in% c("Pelly", "pelly", "Pelly Crossing", "Pelly crossing", "Ross", "ross", "Ross River", "ross river", "Ross river", "Ross/Pelly", "Pelly/Ross", "Pelly River/Ross River", "Pelly/Ross River", "Ross/Pelly River", "Pelly/Ross Rivers", "Ross/Pelly Rivers")) {
        stations <- c("09BB001", "09BA001", "09BC002", "09BC004", "09BC001")
        preset_extras <- "09BC002:2013,1992,1972"

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Pelly.Ross Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Pelly/Ross River Condition Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate = rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
        )
      } #End of Pelly report

      ### Generate a report for Old Crow###
      if (report_name %in% c("Old Crow", "Old crow", "old crow")) {
        stations <-c ("09FD002", "09FD003", "09FC001", "09FA001", "09FB003", "09FB002")
        preset_extras <- "09FD003:2007,2015"

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Old Crow Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Old Crow Condition Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate = rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
        )
      } #End of Old Crow report

    ### Generate a report for Liard/Watson###
    if (report_name %in% c("Liard", "Watson", "Watson Lake", "Watson lake", "watson lake", "Liard River", "Liard river", "liard river", "Liard/Watson", "Watson/Liard", "Watson Lake/Liard River", "Liard River/Watson Lake")) {
      stations <-c ("10AA001", "10AA006", "10AA004", "10AA005", "10AB001", "10AD002")
      preset_extras <- "10AA001:2007,2012,2013"

      if (preset_extra_years==TRUE){
        extra_years <- c(preset_extras, extra_years)
      } else {
        extra_years <- extra_years
      }

      rmarkdown::render(
        input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
        output_file = paste0("Liard.Watson Condition Report ", Sys.Date()),
        output_dir = save_path,
        params = list(
          stations = stations,
          report_name = "Liard River and Watson Lake Area Condition Report",
          extra_years = extra_years,
          image_path = image_path,
          report_type = report_type,
          level_zoom = level_zoom,
          flow_zoom = flow_zoom,
          zoom_days = zoom_days,
          MESH = MESH,
          CLEVER = CLEVER,
          flow_returns = flow_returns,
          level_returns = level_returns,
          rate = rate,
          meteogram = meteogram,
          plot_titles = plot_titles)
      )
    } #End of Liard/Watson report

      ### Generate a report for Mayo/Stewart###
      if (report_name %in% c("Mayo", "mayo", "Stewart", "stewart", "Stewart River", "Stewart river", "stewart river", "Stewart Crossing", "Stewart crossing", "stewart crossing", "Mayo/Stewart", "stewart/Mayo")) {
        stations <-c ("09DC006", "09DC005", "09DA001", "09DB001", "09DD004")
        preset_extras <- "09DC006:1992,1983,2013"

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Mayo.Stewart Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Mayo and Stewart River Condition Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate = rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
        )
      } #End of Mayo/Stewart report

      ### Generate a report for Southern Lakes###
      if (report_name %in% c("Southern Lakes", "Southern lakes", "southern lakes")) {
        stations <-c ("09AA001", "09AA004", "09AA017", "09AB004", "09AB001", "09AB010", "09AA012", "09AA013", "09AC001", "09AC007")
        preset_extras <- c("09AA004:2007,2021", "09AA001:2007,2021", "09AA017:2007,2021", "09AB004:2007,2021", "09AB001:2007,2021", "09AB010:2007,2021")

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Southern Lakes Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Southern Lakes Condition Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate=rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
        )
      } #End of Southern Lakes report

      ### Generate a report for Aishihik###
      if (report_name %in% c("Champagne", "Aishihik", "aishihik", "champagne")) {
        stations <-c ("08AA007", "08AA008", "08AA009", "08AA012", "08AA005", "08AA010", "08AA011")
        preset_extras <- c("08AA007:2020", "08AA008:2020", "08AA005:2020", "08AA010:2020")

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Aishihik Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Aishihik Condition Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate = rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
        )
      } #End of Aishihik report

      ### Generate a report for Alsek###
      if (report_name %in% c("Alsek", "alsek")) {
        stations <- c("08AC002", "08AC001", "08AB001", "08AA003")
        preset_extras <- NULL

        if (preset_extra_years==TRUE){
          extra_years <- c(preset_extras, extra_years)
        } else {
          extra_years <- extra_years
        }

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Alsek Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = "Alsek Condition Report",
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate = rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
        )
      } #End of Alsek report

    }

    ### Generate a custom report ###
    if (is.null(custom_report_stations)==FALSE){
      if (methods::is(custom_report_stations, "character")==TRUE) {
        #Check for existence of all stations in hydat
        stop_go <- "go"
        for (i in custom_report_stations){
          if (!(i %in% tidyhydat::allstations$STATION_NUMBER)){
            stop_go <- "stop"
            warning(paste0("Station ", i, " is not a station listed in the tidyhydat database. Please correct this discrepancy or omit the station before proceeding."))
          }}

        if (stop_go == "go"){
          stations <- custom_report_stations

          if (is.null(extra_years)==FALSE) {
            extra_years <- extra_years
          } else { extra_years <- NULL}

        rmarkdown::render(
          input = system.file("rmd", "Condition_report.Rmd", package="YGwater"),
          output_file = paste0("Custom Condition Report ", Sys.Date()),
          output_dir = save_path,
          params = list(
            stations = stations,
            report_name = report_name,
            extra_years = extra_years,
            image_path = image_path,
            report_type = report_type,
            level_zoom = level_zoom,
            flow_zoom = flow_zoom,
            zoom_days = zoom_days,
            MESH = MESH,
            CLEVER = CLEVER,
            flow_returns = flow_returns,
            level_returns = level_returns,
            rate = rate,
            meteogram = meteogram,
            plot_titles = plot_titles)
          )
        } else {stop("Please correct your station inputs by referencing the message above.")}
      } #End of custom report
    }

    if (is.null(report_name) == TRUE &
        is.null(custom_report_stations) == TRUE) {
      stop("You must specify either a report_name or provide a character vector for custom_report_station.")   #to catch an error where no parameter was entered in both of these
    }

  } #End of function.
