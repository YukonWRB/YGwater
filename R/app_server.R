#' The hydroApp server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  #Initial tasks ----------------
  con <- hydrometConnect(silent = TRUE)
  onStop(function(){DBI::dbDisconnect(con)}
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
    if (input$first_selection == "View precipitation maps + data"){
      if (!runCheck$precip){
        temp <- DBI::dbGetQuery(con, "SELECT feature_name, description FROM vectors WHERE layer_name = 'Drainage basins';")
        names(temp) <- c("code", "name")
        precip$poly_names_codes <- temp
        updateSelectizeInput(session, "precip_loc_code", choices = c("", precip$poly_names_codes$code))
        updateSelectizeInput(session, "precip_loc_name", choices = c("", precip$poly_names_codes$name))
        runCheck$precip <- TRUE
      }
    } else if (input$first_selection == "View hydromet plots + data"){
      if (!runCheck$plots){
        timeseries <- DBI::dbGetQuery(con, "SELECT timeseries_id, location_id, location, parameter, param_type, unit, category, start_datetime, end_datetime FROM timeseries")
        locations <- DBI::dbGetQuery(con, "SELECT location, location_id, name FROM locations")
        result <- merge(timeseries, locations)
        plotContainer$all_ts <- result
        datum_conversions <- DBI::dbGetQuery(con, "SELECT locations.location, datum_conversions.location_id, datum_id_to, conversion_m, current FROM datum_conversions INNER JOIN locations ON locations.location_id = datum_conversions.location_id")
        datum_list <- DBI::dbGetQuery(con, "SELECT datum_id, datum_name_en FROM datum_list")
        datums <- merge(datum_conversions, datum_list, by.x = "datum_id_to", by.y = "datum_id")
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
    if (input$show_map){
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
    if (input$precip_loc_code %in% precip$poly_names_codes$code){
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
      if (input$show_map){
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
    for (j in as.character(FOD_seq)){
      tryCatch({ # to handle if the file doesn't exist or otherwise can't be read
        if (!(j %in% FOD_comments$dates)){ #don't look if it's already loaded
          if (j != Sys.Date()){
            workbook <- openxlsx::loadWorkbook(paste0("//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/Archive/", j, "/HydrometricReport_", j, ".xlsx"))
          } else {
            workbook <- openxlsx::loadWorkbook(paste0("//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/", j, "/HydrometricReport_", j, ".xlsx"))
          }
          for (k in names(workbook)){
            if (k %in% c("bridges", "bridge")){
              sheet_name <- "bridges"
            } else {
              sheet_name <- k
            }
            if (k != "precipitation"){
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

    if (input$comment_type == "General comments"){
      FOD_comments$tables[["general"]] <- data.frame()
      for (i in as.character(FOD_seq)) {
        for (j in types){
          if (length(FOD_comments$comments$general[[j]][[i]]) > 0){
            fod_name <- if(length(FOD_comments$comments$FOD[[j]][[i]]) > 0) FOD_comments$comments$FOD[[j]][[i]] else FOD_comments$comments$FOD[["levels"]][[i]]
            fod_cmt <- FOD_comments$comments$general[[j]][[i]]
            if (length(fod_name) == 0){
              fod_name <- NA_character_
            }
            if (length(fod_cmt) == 0){
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
        for (j in types){
          tryCatch({
            for (k in 1:nrow(FOD_comments$comments$specific[[j]][[i]])){
              row <- FOD_comments$comments$specific[[j]][[i]][k , ]
              if (!is.na(row$Location.specific.comments)[1]){
                append_row <- data.frame("Date" = i,
                                         "Forecaster" = if(length(FOD_comments$comments$FOD[[j]][[i]]) > 0) FOD_comments$comments$FOD[[j]][[i]] else FOD_comments$comments$FOD[["levels"]][[i]],
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


  # observe and observeEvents related to plotting level/flow/snow/bridge freeboard --------------------
  observeEvent(input$plot_data_type, {
    if (input$plot_data_type == "Discrete"){
      updateSelectizeInput(session, "plot_param", choices = c("SWE", "Snow depth"))
      shinyjs::hide("return_periods")
      shinyjs::hide("return_type")
      shinyjs::hide("return_months")
      shinyjs::hide("end_doy")
      shinyjs::hide("start_doy")
      shinyjs::hide("plot_years_note")
      shinyjs::hide("plot_filter")
      shinyjs::hide("historic_range")
      shinyjs::hide("return_yrs")
      shinyjs::show("discrete_plot_type")
    } else if (input$plot_data_type == "Continuous"){
      updateSelectizeInput(session, "plot_param", choices = c("Level", "Flow", "Bridge freeboard", "SWE", "Snow depth"))
      shinyjs::show("return_periods")
      shinyjs::show("return_type")
      shinyjs::show("return_months")
      shinyjs::show("end_doy")
      shinyjs::show("start_doy")
      shinyjs::show("plot_years_note")
      shinyjs::show("plot_filter")
      shinyjs::show("historic_range")
      shinyjs::show("return_yrs")
      shinyjs::hide("discrete_plot_type")
    }
  })

  observeEvent(input$plot_param, {
    if (input$plot_param %in% c("SWE", "Snow depth")){
      if (input$plot_data_type == "Discrete"){  #NOTE: This is only set to -12-31 and -01-01 because of limitations in how the plotting function handles start/end dates for discrete data. Can be modified once the plotting utility is adapted, if desired.
        updateDateInput(session, "end_doy", value = paste0(lubridate::year(Sys.Date()), "-12-31"))
        updateDateInput(session, "start_doy", value = paste0(lubridate::year(Sys.Date()), "-01-01"))
      } else if (input$plot_data_type == "Continuous") {
        updateDateInput(session, "start_doy", value = paste0(lubridate::year(Sys.Date())-1, "-09-01"))
        updateDateInput(session, "end_doy", value = paste0(lubridate::year(Sys.Date()), "-06-01"))
        updateTextInput(session, "return_months", value = "3,4,5")
      }
    } else {
      updateTextInput(session, "return_months", value = "5,6,7,8,9")
    }
  })

  # Update user's choices for plots based on selected plot type
  observe(
    if (input$plot_param == "Level"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "level"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "level" & plotContainer$all_ts$category == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "level" & plotContainer$all_ts$category == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "level"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "level" & plotContainer$all_ts$category == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "level" & plotContainer$all_ts$category == "discrete", "location"]))
      }
    } else if (input$plot_param == "Flow"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "flow"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "flow" & plotContainer$all_ts$category == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "flow" & plotContainer$all_ts$category == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "flow"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "flow" & plotContainer$all_ts$category == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "flow" & plotContainer$all_ts$category == "discrete", "location"]))
      }
    } else if (input$plot_param == "Bridge freeboard"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "distance"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "distance" & plotContainer$all_ts$category == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "distance" & plotContainer$all_ts$category == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "distance"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "distance" & plotContainer$all_ts$category == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "distance" & plotContainer$all_ts$category == "discrete", "location"]))
      }
    } else if (input$plot_param == "SWE"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "SWE"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "SWE" & plotContainer$all_ts$category == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "SWE" & plotContainer$all_ts$category == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "SWE"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "SWE" & plotContainer$all_ts$category == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "SWE" & plotContainer$all_ts$category == "discrete", "location"]))
      }
    } else if (input$plot_param == "Snow depth"){
      if (input$plot_data_type == "Continuous"){
        plotContainer$plot_data_type <- "continuous"
        plotContainer$plot_param <- "snow depth"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "snow depth" & plotContainer$all_ts$category == "continuous", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "snow depth" & plotContainer$all_ts$category == "continuous", "location"]))
      } else if (input$plot_data_type == "Discrete"){
        plotContainer$plot_data_type <- "discrete"
        plotContainer$plot_param <- "snow depth"
        updateSelectizeInput(session, "plot_loc_name", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "snow depth" & plotContainer$all_ts$category == "discrete", "name"]))
        updateSelectizeInput(session, "plot_loc_code", choices = c("", plotContainer$all_ts[plotContainer$all_ts$parameter == "snow depth" & plotContainer$all_ts$category == "discrete", "location"]))
      }
    }
  )

  observeEvent(input$plot_param, {
    if (input$plot_param == "Level"){
      shinyjs::show("apply_datum")
    } else {
      shinyjs::hide("apply_datum")
      updateCheckboxInput(session, "apply_datum", value = FALSE)
    }
  }) #Do not ignoreInit = TRUE otherwise will not added

  #Cross-updating of plot selection location name or code
  observeEvent(input$plot_loc_code, {
    if (input$plot_loc_code %in% plotContainer$all_ts$location){ #otherwise it runs without actually getting any information, which results in an error
      updateSelectizeInput(session, "plot_loc_name", selected = plotContainer$all_ts[plotContainer$all_ts$location == input$plot_loc_code, "name"])
      possible_years <- seq(as.numeric(substr(plotContainer$all_ts[plotContainer$all_ts$location == input$plot_loc_code & plotContainer$all_ts$parameter == plotContainer$plot_param &  plotContainer$all_ts$category == plotContainer$plot_data_type, "start_datetime"], 1, 4)), as.numeric(substr(plotContainer$all_ts[plotContainer$all_ts$location == input$plot_loc_code & plotContainer$all_ts$parameter == plotContainer$plot_param &  plotContainer$all_ts$category == plotContainer$plot_data_type, "end_datetime"], 1, 4)))
      shinyWidgets::updatePickerInput(session, "plot_years", choices = possible_years)
      plotContainer$possible_datums <- plotContainer$datums[plotContainer$datums$location == input$plot_loc_code & plotContainer$datums$conversion_m != 0, ]
      if (nrow(plotContainer$possible_datums) < 1){
        shinyjs::hide("apply_datum")
        updateCheckboxInput(session, "apply_datum", value = FALSE)
      }
    }
  }, ignoreInit = TRUE)
  observeEvent(input$plot_loc_name, {
    updateSelectizeInput(session, "plot_loc_code", selected = plotContainer$all_ts[plotContainer$all_ts$name == input$plot_loc_name, "location"])
  }, ignoreInit = TRUE)

  observeEvent(input$return_periods, {
    if (input$return_periods == "none"){
      plotContainer$returns <- "none"
    } else if (input$return_periods == "auto select"){
      plotContainer$returns <- "auto"
    } else if (input$return_periods == "calculate"){
      plotContainer$returns <- "calculate"
    } else if (input$return_periods == "from table"){
      plotContainer$returns <- "table"
    }
  }) #Do not ignoreInit = TRUE otherwise will not be populated
  observeEvent(input$return_periods, {
    if (input$return_periods == "none"){
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

  observeEvent(input$discrete_plot_type, {
    if (input$discrete_plot_type == "Box plot"){
      plotContainer$discrete_plot_type <- "boxplot"
    } else if (input$discrete_plot_type == "Violin plot")
      plotContainer$discrete_plot_type <- "violin"
  }) #Do not ignoreInit = TRUE otherwise will not be populated

  observeEvent(input$plot_go, {
    tryCatch({
      if (plotContainer$plot_data_type == "continuous"){
        if (input$plot_filter){
          plotContainer$plot_filter <- 20
        } else {
          plotContainer$plot_filter <- NULL
        }
        plotContainer$plot <- hydrometContinuous(location = input$plot_loc_code, parameter = plotContainer$plot_param, startDay = input$start_doy, endDay = input$end_doy, years = input$plot_years, historic_range = input$historic_range, datum = input$apply_datum, filter = plotContainer$plot_filter, returns = plotContainer$returns, return_type = input$return_type, return_months = plotContainer$return_months, return_max_year = input$return_yrs, plot_scale = 1.4)
      } else if (plotContainer$plot_data_type == "discrete"){
        plotContainer$plot <- hydrometDiscrete(location = input$plot_loc_code, parameter = plotContainer$plot_param, years = input$plot_years, plot_type = plotContainer$discrete_plot_type, plot_scale = 1.4)
      }
    output$hydro_plot <- renderPlot(plotContainer$plot)
    shinyjs::show("export_hydro_plot")
    output$export_hydro_plot <- downloadHandler(
      filename = function() {paste0(input$plot_loc_code, "_", plotContainer$plot_param, "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz="MST")), lubridate::minute(as.POSIXct(format(Sys.time()), tz="MST")), ".png")},
      content = function(file) {
        grDevices::png(file, width = 900, height = 700, units = "px")
        print(plotContainer$plot)  #WARNING do not remove this print call, it is not here for debugging purposes
        grDevices::dev.off()})
    shinyjs::show("export_plot_data")
    output$export_plot_data <- downloadHandler(
      filename = function() {paste0(input$plot_loc_code, "_", plotContainer$plot_param, "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz="MST")), lubridate::minute(as.POSIXct(format(Sys.time()), tz="MST")), ".csv")},
      content = function(file) {
        write.csv(plotContainer$plot$data, file, row.names = FALSE)
      }
    )
    }, error = function(e){
      shinyalert::shinyalert("Error in rendering plot", "Try again with a different set of input parameters. For plots overlaping the new year, use the *December* year.", type = "error")
    })

  }, ignoreInit = TRUE)
}
