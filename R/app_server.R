#' The hydroApp server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  #Initial tasks ----------------
  con <- hydrometConnect(silent = TRUE)
  onStop(function() {DBI::dbDisconnect(con)}
    )
  
  #Render some text
  output$plot_years_note <- renderText("For ranges covering December-January, select the December year(s)")
  output$standby <- renderText("<b>Standby...<b>")

  #Show/hide some things right off the bat
  shinyjs::hide("standby")
  shinyjs::hide("export_fod_comments")
  shinyjs::hide("export_hydro_plot")
  shinyjs::hide("export_plot_data")
  shinyjs::hide("export_precip_map")

  #Create containers
  FOD_comments <- reactiveValues(comments = list(),
                                 dates = vector(),
                                 tables = list("general" = data.frame(),
                                               "specific" = data.frame()))
  precip <- reactiveValues()
  plotContainer <- reactiveValues()
  runCheck <- reactiveValues(precip = FALSE,
                             plots = FALSE)
  output$time_adj_note <- renderText("Note: times in the past will be adjusted to the nearest 6 hours, times in future to the nearest hour.")

  #Tasks related to first selection/selection of which page to view ------------------
  #Load some necessary information if the user is trying the get precipitation maps/data
  observeEvent(input$first_selection, {
    if (input$first_selection == "View precipitation maps + data") {
      if (!runCheck$precip) {
        temp <- DBI::dbGetQuery(con, "SELECT feature_name, description FROM vectors WHERE layer_name = 'Drainage basins';")
        names(temp) <- c("code", "name")
        precip$poly_names_codes <- temp
        updateSelectizeInput(session, "precip_loc_code", choices = c("", precip$poly_names_codes$code))
        updateSelectizeInput(session, "precip_loc_name", choices = c("", precip$poly_names_codes$name))
        runCheck$precip <- TRUE
      }
    } else if (input$first_selection == "View hydromet plots + data") {
      if (!runCheck$plots) {
        plotContainer$all_ts <- DBI::dbGetQuery(con, "SELECT ts.timeseries_id, ts.location_id, ts.location, ts.parameter, ts.param_type, ts.unit, ts.category, ts.start_datetime, ts.end_datetime, loc.name FROM timeseries AS ts INNER JOIN locations AS loc ON ts.location_id = loc.location_id AND ts.location = loc.location;")
        plotContainer$parameters_discrete <- DBI::dbGetQuery(con, "SELECT DISTINCT parameters.param_code, parameters.param_name FROM timeseries INNER JOIN parameters ON timeseries.parameter = parameters.param_code WHERE timeseries.category = 'discrete';")
        plotContainer$parameters_continuous <- DBI::dbGetQuery(con, "SELECT DISTINCT parameters.param_code, parameters.param_name FROM timeseries INNER JOIN parameters ON timeseries.parameter = parameters.param_code WHERE timeseries.category = 'continuous';")
        datums <- DBI::dbGetQuery(con, "SELECT dc.location_id, dc.datum_id_to, dc.conversion_m, dc.current, dl.datum_name_en FROM datum_conversions dc INNER JOIN locations l ON dc.location_id = l.location_id INNER JOIN datum_list dl ON dc.datum_id_to = dl.datum_id;")
        
        datums$datum_name_en <- gsub("GEODETIC SURVEY OF CANADA DATUM", "CGVD28 (assumed)", datums$datum_name_en)
        datums$datum_name_en <- gsub("CANADIAN GEODETIC VERTICAL DATUM 2013:EPOCH2010", "CGVD2013:2010", datums$datum_name_en)
        datums$datum_name_en <- gsub("APPROXIMATE", "approx.", datums$datum_name_en)
        plotContainer$datums <- datums
        runCheck$plots <- TRUE
      }
    }
  })

  #observeEvents related to precipitation data/maps -----------------------
  #Change the actionButton for rendering a map/data depending on which option the user chooses
  observeEvent(input$show_map, {
    if (input$show_map) {
      updateActionButton(session, "precip_go", "Render map and calculate precip")
    } else {
      updateActionButton(session, "precip_go", "Calculate precip")
    }
  }, ignoreInit = TRUE)

  #Cross-updating of precipitation location name or code
  observeEvent(input$precip_loc_code, {
    updateSelectizeInput(session, "precip_loc_name", selected = precip$poly_names_codes[precip$poly_names_codes$code == input$precip_loc_code, "name"])
  }, ignoreInit = TRUE)
  observeEvent(input$precip_loc_name, {
    updateSelectizeInput(session, "precip_loc_code", selected = precip$poly_names_codes[precip$poly_names_codes$name == input$precip_loc_name, "code"])
  }, ignoreInit = TRUE)

  # Render the precipitation map and/or precip data. Make a file available for download via the download button.
  observeEvent(input$precip_go, {
    #Check if inputs are filled in
    if (input$precip_loc_code %in% precip$poly_names_codes$code) {
      precip$location_code <- input$precip_loc_code
      updateActionButton(session, "precip_go", label = "Standby...")
      shinyjs::show("standby")
      start <- input$precip_start
      start <- start + 7*60*60
      attr(start, "tzone") <- "UTC"
      end <- input$precip_end
      end <- end + 7*60*60
      attr(end, "tzone") <- "UTC"
      precip_res <- basinPrecip(input$precip_loc_code, start = start, end = end, map = if (input$show_map) TRUE else FALSE)
      if (input$show_map) {
        output$precip_map <- renderPlot(precip_res$plot)
        shinyjs::show("export_precip_map")
      }
      shinyjs::hide("standby")
      updateActionButton(session, "precip_go", "Go!")
      actual_start <- as.POSIXct(precip_res$total_time_range_UTC[1], tz = "UTC")
      attr(actual_start, "tzone") <- "MST"
      actual_end <- as.POSIXct(precip_res$total_time_range_UTC[2], tz = "UTC")
      attr(actual_end, "tzone") <- "MST"
      output$results_head <- renderText(paste0("<br><b>Results<b>"))
      output$start_time <- renderText(paste0("Actual start time: ", actual_start, " MST"))
      output$end_time <- renderText(paste0("Actual end time: ", actual_end, " MST"))
      output$mean <- renderText(paste0("Basin mean: ", round(precip_res$mean_precip, 3), " mm"))
      output$min <- renderText(paste0("Basin min: ", round(precip_res$min, 3), " mm"))
      output$max <- renderText(paste0("Basin max: ", round(precip_res$max, 3), " mm"))
      output$watershed_area <- renderText(paste0("Basin area: ", precip$poly_names_codes[precip$poly_names_codes$description == input$precip_loc_code, "areas"], " km2"))

      output$export_precip_map <- downloadHandler(
        filename = function() {paste0("precip abv ", precip$location_code, " from ", precip_res$total_time_range_UTC[1], " to ", precip_res$total_time_range_UTC[2] , ".png")},
        content = function(file) {
          grDevices::png(file, width = 1000, height = 700, units = "px")
          print(precip_res$plot)  #WARNING do not remove this print call, it is not here for debugging purposes
          grDevices::dev.off()})
    } else {
      shinyalert::shinyalert("Location code is not valid", type = "error", timer = 4000)
    }
  }, ignoreInit = TRUE)

  # observeEvents related to displaying FOD comments -------------------------
  # Display FOD comments and make .csv available for download
  observeEvent(input$FOD_go, {
    #Load workbooks where required
    FOD_seq <- seq.Date(from = input$comment_start_date, to = input$comment_end_date, by = "day")
    for (j in as.character(FOD_seq)) {
      tryCatch({ # to handle if the file doesn't exist or otherwise can't be read
        if (!(j %in% FOD_comments$dates)) { #don't look if it's already loaded
          if (j != Sys.Date()) {
            workbook <- openxlsx::loadWorkbook(paste0("//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/Archive/", j, "/HydrometricReport_", j, ".xlsx"))
          } else {
            workbook <- openxlsx::loadWorkbook(paste0("//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/", j, "/HydrometricReport_", j, ".xlsx"))
          }
          for (k in names(workbook)) {
            if (k %in% c("bridges", "bridge")) {
              sheet_name <- "bridges"
            } else {
              sheet_name <- k
            }
            if (k != "precipitation") {
              FOD_comments$comments[["FOD"]][[sheet_name]][[j]] <- as.character(openxlsx::read.xlsx(workbook, sheet = k, rows = 1, cols = 5, colNames = FALSE))
              FOD_comments$comments[["general"]][[sheet_name]][[j]] <- as.character(openxlsx::read.xlsx(workbook, sheet = k, rows = 3, cols = 2, colNames = FALSE))
              FOD_comments$comments[["specific"]][[sheet_name]][[j]] <- openxlsx::read.xlsx(workbook, sheet = k, startRow = 6)

            } else {
              FOD_comments$comments[["FOD"]][[sheet_name]][[j]] <- as.character(openxlsx::read.xlsx(workbook, sheet = k, rows = 1, cols = 5, colNames = FALSE))
              FOD_comments$comments[["general"]][[sheet_name]][[j]] <- as.character(openxlsx::read.xlsx(workbook, sheet = k, rows = 3, cols = 2, colNames = FALSE))
              FOD_comments$comments[["specific"]][[sheet_name]][[j]] <- openxlsx::read.xlsx(workbook, sheet = k, startRow = 8)
            }
          }
          FOD_comments$dates <- c(FOD_comments$dates, j)
        }
      }, error = function(e) {})
    }

    #Make and render the appropriate table
    types <- if(input$comment_data_type == "All") c("levels", "flows", "snow", "bridges", "precipitation") else if (input$comment_data_type == "Water levels") "levels" else if (input$comment_data_type == "Water flows") "flows" else if (input$comment_data_type == "Snow pillows") "snow" else if (input$comment_data_type == "Bridge freeboard") "bridges" else if (input$comment_data_type == "Precipitation") "precipitation"

    if (input$comment_type == "General comments") {
      FOD_comments$tables[["general"]] <- data.frame()
      for (i in as.character(FOD_seq)) {
        for (j in types) {
          if (length(FOD_comments$comments$general[[j]][[i]]) > 0) {
            fod_name <- if (length(FOD_comments$comments$FOD[[j]][[i]]) > 0) FOD_comments$comments$FOD[[j]][[i]] else FOD_comments$comments$FOD[["levels"]][[i]]
            fod_cmt <- FOD_comments$comments$general[[j]][[i]]
            if (length(fod_name) == 0) {
              fod_name <- NA_character_
            }
            if (length(fod_cmt) == 0) {
              fod_cmt <- NA_character_
            }
            row <- data.frame("Date" = i,
                              "Forecaster" = fod_name,
                              "Data sheet source" = j,
                              "Comment" = fod_cmt,
                              check.names = FALSE
            )
            FOD_comments$tables[["general"]] <- rbind(FOD_comments$tables[["general"]], row)
          }
        }
      }
      output$FOD_table <- DT::renderDataTable(FOD_comments$tables[["general"]], rownames = FALSE)
      output$export_fod_comments <- downloadHandler(
        filename = function() {paste0("general comments ", input$comment_start_date, " to ", input$comment_end_date , ".csv")},
        content = function(file) {utils::write.csv(FOD_comments$tables[["general"]], file, row.names = FALSE)})
    } else { #location-specific comments are requested
      FOD_comments$tables[["specific"]] <- data.frame()
      for (i in as.character(FOD_seq)) {
        for (j in types) {
          tryCatch({
            for (k in 1:nrow(FOD_comments$comments$specific[[j]][[i]])) {
              row <- FOD_comments$comments$specific[[j]][[i]][k , ]
              if (!is.na(row$Location.specific.comments)[1]) {
                append_row <- data.frame("Date" = i,
                                         "Forecaster" = if (length(FOD_comments$comments$FOD[[j]][[i]]) > 0) FOD_comments$comments$FOD[[j]][[i]] else FOD_comments$comments$FOD[["levels"]][[i]],
                                         "Location" = FOD_comments$comments$specific[[j]][[i]][k,"Location"],
                                         "Data sheet source" = j,
                                         "Location name" = FOD_comments$comments$specific[[j]][[i]][k,"Name"],
                                         "Comment" = FOD_comments$comments$specific[[j]][[i]][k,"Location.specific.comments"],
                                         check.names = FALSE)
                FOD_comments$tables[["specific"]] <- rbind(FOD_comments$tables[["specific"]], append_row)
              }
            }
          }, error = function(e) {})

        }
      }
      output$FOD_table <- DT::renderDataTable(FOD_comments$tables[["specific"]], rownames = FALSE)
      output$export_fod_comments <- downloadHandler(
        filename = function() {paste0("station specific comments ", input$comment_start_date, " to ", input$comment_end_date , ".csv")},
        content = function(file) {utils::write.csv(FOD_comments$tables[["specific"]], file, row.names = FALSE)})
    }
    shinyjs::show("export_fod_comments")
  }, ignoreInit = TRUE)


  # observe and observeEvents related to plotting data --------------------
  observeEvent(input$plot_data_type, {
    if (input$plot_data_type == "Discrete") {
      updateSelectizeInput(session, "plot_param", choices = titleCase(plotContainer$parameters_discrete$param_name))
      shinyjs::hide("return_periods")
      shinyjs::hide("return_type")
      shinyjs::hide("return_months")
      shinyjs::hide("start_date")
      shinyjs::hide("end_date")
      shinyjs::hide("plot_years_note")
      shinyjs::hide("plot_filter")
      shinyjs::hide("historic_range")
      shinyjs::hide("historic_range_overlap")
      shinyjs::hide("return_yrs")
      shinyjs::show("plot_sub_type")
      updateSelectizeInput(session, "plot_type", choices = c("Binned", "Scatter"), selected = "Binned")
    } else if (input$plot_data_type == "Continuous") {
      updateSelectizeInput(session, "plot_param", choices = titleCase(plotContainer$parameters_continuous$param_name))
      shinyjs::show("return_periods")
      shinyjs::show("return_type")
      shinyjs::show("return_months")
      shinyjs::show("plot_years_note")
      shinyjs::show("plot_filter")
      shinyjs::hide("plot_sub_type")
      updateSelectizeInput(session, "plot_type", choices = c("Overlapping years", "Long timeseries", "Multi timeseries"), selected = "Overlapping years")
    }
  })
  
  observeEvent(input$plot_param, {
    if (input$plot_data_type == "Discrete") { 
      updateDateInput(session, "end_doy", value = paste0(lubridate::year(Sys.Date()), "-12-31"))
      updateDateInput(session, "start_doy", value = paste0(lubridate::year(Sys.Date()), "-01-01"))
      
        plotContainer$param_code <- plotContainer$parameters_discrete[plotContainer$parameters_discrete$param_name == if (input$plot_param == "SWE") input$plot_param else tolower(input$plot_param), "param_code"]
        updateSelectizeInput(session, "plot_loc_name", choices = unique(plotContainer$all_ts[plotContainer$all_ts$parameter == plotContainer$param_code & plotContainer$all_ts$category == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = unique(plotContainer$all_ts[plotContainer$all_ts$parameter == plotContainer$param_code & plotContainer$all_ts$category == "discrete", "location"]))
      
      } else if (input$plot_data_type == "Continuous") {
        plotContainer$param_code <- plotContainer$parameters_continuous[plotContainer$parameters_continuous$param_name == if (input$plot_param == "SWE") input$plot_param else tolower(input$plot_param), "param_code"]
        updateSelectizeInput(session, "plot_loc_name", choices = unique(plotContainer$all_ts[plotContainer$all_ts$parameter == plotContainer$param_code & plotContainer$all_ts$category == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = unique(plotContainer$all_ts[plotContainer$all_ts$parameter == plotContainer$param_code & plotContainer$all_ts$category == "continuous", "location"]))
        if (input$plot_param %in% c("SWE", "Depth")) {
          updateDateInput(session, "start_doy", value = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"))
          updateDateInput(session, "end_doy", value = paste0(lubridate::year(Sys.Date()), "-06-01"))
          updateTextInput(session, "return_months", value = "3,4,5")
        }  else {
          updateTextInput(session, "return_months", value = "5,6,7,8,9")
        }
      }
    if (input$plot_param == "Water Level") {
      shinyjs::show("apply_datum")
    } else {
      shinyjs::hide("apply_datum")
      updateCheckboxInput(session, "apply_datum", value = FALSE)
    }
  }) #Do not ignoreInit = TRUE
  
  observeEvent(input$plot_type, {
    if (input$plot_type == "Overlapping years") {
      shinyjs::show("return_periods")
      shinyjs::show("return_type")
      shinyjs::show("return_months")
      shinyjs::show("end_doy")
      shinyjs::show("start_doy")
      shinyjs::show("plot_years_note")
      shinyjs::show("plot_years")
      shinyjs::show("historic_range_overlap")
      shinyjs::hide("historic_range")
      shinyjs::hide("return_yrs")
      shinyjs::hide("start_date")
      shinyjs::hide("end_date")
    } else if (input$plot_type == "Long timeseries") {
      shinyjs::hide("return_periods")
      shinyjs::hide("return_type")
      shinyjs::hide("return_months")
      shinyjs::hide("end_doy")
      shinyjs::hide("start_doy")
      shinyjs::hide("plot_years_note")
      shinyjs::hide("plot_years")
      shinyjs::hide("return_yrs")
      shinyjs::hide("historic_range_overlap")
      shinyjs::show("historic_range")
      shinyjs::show("start_date")
      shinyjs::show("end_date")
    } else if (input$plot_type == "Multi timeseries") {
      shinyjs::hide("return_periods")
      shinyjs::hide("return_type")
      shinyjs::hide("return_months")
      shinyjs::hide("end_doy")
      shinyjs::hide("start_doy")
      shinyjs::hide("plot_years_note")
      shinyjs::hide("plot_years")
      shinyjs::hide("return_yrs")
      shinyjs::hide("historic_range")
      shinyjs::hide("historic_range_overlap")
      shinyjs::show("start_date")
      shinyjs::show("end_date")
    } else if (input$plot_type == "Binned") {
      shinyjs::show("plot_sub_type")
    } else if (input$plot_type == "Scatter") {
      shinyjs::hide("plot_sub_type")
    }
  })
  
  #Cross-updating of plot selection location name or code
  observeEvent(input$plot_loc_code, {
    if (input$plot_loc_code %in% plotContainer$all_ts$location) { #otherwise it runs without actually getting any information, which results in an error
      updateSelectizeInput(session, "plot_loc_name", selected = unique(plotContainer$all_ts[plotContainer$all_ts$location == input$plot_loc_code, "name"]))
      try({
        possible_years <- seq(as.numeric(substr(plotContainer$all_ts[plotContainer$all_ts$location == input$plot_loc_code & plotContainer$all_ts$parameter == plotContainer$param_code &  plotContainer$all_ts$category == tolower(input$plot_data_type), "start_datetime"], 1, 4)), as.numeric(substr(plotContainer$all_ts[plotContainer$all_ts$location == input$plot_loc_code & plotContainer$all_ts$parameter == plotContainer$param_code &  plotContainer$all_ts$category == tolower(input$plot_data_type), "end_datetime"], 1, 4)))
      shinyWidgets::updatePickerInput(session, "plot_years", choices = possible_years)
      })
      
      plotContainer$possible_datums <- plotContainer$datums[plotContainer$datums$location == input$plot_loc_code & plotContainer$datums$conversion_m != 0, ]
      if (nrow(plotContainer$possible_datums) < 1) {
        shinyjs::hide("apply_datum")
        updateCheckboxInput(session, "apply_datum", value = FALSE)
      } else {
        shinyjs::show("apply_datum")
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_loc_name, {
    updateSelectizeInput(session, "plot_loc_code", selected = unique(plotContainer$all_ts[plotContainer$all_ts$name == input$plot_loc_name, "location"]))
  }, ignoreInit = TRUE)
  
  observeEvent(input$return_periods, {
    if (input$return_periods == "none") {
      plotContainer$returns <- "none"
    } else if (input$return_periods == "auto select") {
      plotContainer$returns <- "auto"
    } else if (input$return_periods == "calculate") {
      plotContainer$returns <- "calculate"
    } else if (input$return_periods == "from table") {
      plotContainer$returns <- "table"
    }
  }) #Do not ignoreInit = TRUE otherwise will not be populated
  
  observeEvent(input$return_periods, {
    if (input$return_periods == "none") {
      shinyjs::hide("return_type")
      shinyjs::hide("return_months")
      shinyjs::hide("return_yrs")
    } else {
      shinyjs::show("return_type")
      shinyjs::show("return_months")
      shinyjs::show("return_yrs")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$return_months, {
    plotContainer$return_months <- as.numeric(unlist(strsplit(input$return_months,",")))
  }) #Do not ignoreInit = TRUE otherwise will not be populated
  
  observeEvent(input$plot_type, {
    if (input$plot_type == "Overlapping years") {
      plotContainer$plot_type <- "plotOverlap"
    } else if (input$plot_type == "Long timeseries") {
      plotContainer$plot_type <- "plotTimeseries"
    } else if (input$plot_type == "Multi timeseries") {
      plotContainer$plot_type <- "plotTimeseriesMulti"
    } else if (input$plot_type == "Binned") {
      plotContainer$plot_type <- "hydrometDiscrete"
    } else if (input$plot_type == "Scatter") {
      plotContainer$plot_type <- "plotScatter"
    }
  }) #Do not ignoreInit = TRUE otherwise will not be populated initially
  
  
  observeEvent(input$plot_filter, {
    if (input$plot_filter) {
      plotContainer$plot_filter <- 20
    } else {
      plotContainer$plot_filter <- NULL
    }
  }) #Do not ignoreInit = TRUE otherwise will not be populated initially
  
  observeEvent(input$plot_go, {
    tryCatch({
      if (plotContainer$plot_type == "plotOverlap") {
        plotContainer$plot <- plotOverlap(location = input$plot_loc_code, parameter = tolower(input$plot_param), startDay = input$start_doy, endDay = input$end_doy, years = input$plot_years, historic_range = input$historic_range_overlap, datum = input$apply_datum, filter = plotContainer$plot_filter, returns = plotContainer$returns, return_type = input$return_type, return_months = plotContainer$return_months, return_max_year = input$return_yrs, plot_scale = 1.4, con = con)
      } else if (plotContainer$plot_type == "plotTimeseries") {
        plotContainer$plot <- plotTimeseries(location = input$plot_loc_code, parameter = tolower(input$plot_param), start_date = input$start_date, end_date = input$end_date, historic_range = input$historic_range, datum = input$apply_datum, filter = plotContainer$plot_filter, con = con)
      } else if (plotContainer$plot_type == "plotTimeseriesMulti") {
        #TODO: add multi timeseries plot
        # This one doesn't exist yet so show a warning the user.
        shinyjs::alert("This plot type is not yet implemented.")
        return()
      } else if (plotContainer$plot_type == "hydrometDiscrete") {
        if (!input$plot_param %in% c("SWE", "Snow Depth")) {
          shinyjs::alert("This plot type is only available for SWE and Snow Depth at this time. Please select one of these parameters.")
          return()
        }
        if (input$plot_sub_type == "Violin plot") {
          plotContainer$plot <- hydrometDiscrete(location = input$plot_loc_code, parameter = input$plot_param, years = input$plot_years, startDay = input$start_doy, endDay = input$end_doy, plot_type = "violin", plot_scale = 1.4, con = con)
        } else if (input$plot_sub_type == "Box plot") {
          plotContainer$plot <- hydrometDiscrete(location = input$plot_loc_code, parameter = input$plot_param, years = input$plot_years, startDay = input$start_doy, endDay = input$end_doy, plot_type = "boxplot", plot_scale = 1.4, con = con)
        } else if (input$plot_sub_type == "Line-box plot") {
          plotContainer$plot <- hydrometDiscrete(location = input$plot_loc_code, parameter = input$plot_param, years = input$plot_years, startDay = input$start_doy, endDay = input$end_doy, plot_type = "linedbox", plot_scale = 1.4, con = con)
        }
      } else if (plotContainer$plot_type == "plotScatter") {
        #TODO: add scatter plot
        # This one doesn't exist yet so show a warning the user.
        shinyjs::alert("This plot type is not yet implemented.")
        return()
      }
        
      if (inherits(plotContainer$plot, "plotly")) {
        shinyjs::show("hydro_plotly")
        shinyjs::hide("hydro_plot")
        output$hydro_plotly <- plotly::renderPlotly(plotContainer$plot)
        shinyjs::hide("export_hydro_plot")
        shinyjs::hide("export_plot_data")
      } else if (inherits(plotContainer$plot, "gg")) {
        shinyjs::show("hydro_plot")
        shinyjs::hide("hydro_plotly")
        output$hydro_plot <- renderPlot(plotContainer$plot)
        shinyjs::show("export_hydro_plot")
        output$export_hydro_plot <- downloadHandler(
          filename = function() {paste0(input$plot_loc_code, "_", tolower(input$plot_param), "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz = "MST")), lubridate::minute(as.POSIXct(format(Sys.time()), tz = "MST")), ".png")},
          content = function(file) {
            grDevices::png(file, width = 900, height = 700, units = "px")
            print(plotContainer$plot)  #WARNING do not remove this print call, it is not here for debugging purposes
            grDevices::dev.off()})
        shinyjs::show("export_plot_data")
        output$export_plot_data <- downloadHandler(
          filename = function() {paste0(input$plot_loc_code, "_", tolower(input$plot_param), "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz = "MST")), lubridate::minute(as.POSIXct(format(Sys.time()), tz = "MST")), ".csv")},
          content = function(file) {
            write.csv(plotContainer$plot$data, file, row.names = FALSE)
          }
        )
      }
    }, error = function(e) {
      shinyalert::shinyalert("Error in rendering plot", "Try again with a different set of input parameters.", type = "error")
    })

  }, ignoreInit = TRUE)
}
