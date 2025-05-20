# UI and server code for plotting tab. Modules are called depending on the plot type selected.

FODUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    sidebarPanel(
      selectInput(ns("comment_type"), "Select comment type", choices = c("General comments", "Location-specific comments"), selected = "General comments"),
      selectInput(ns("comment_data_type"), "Select a data type", choices = c("All", "Water levels", "Water flows", "Bridge freeboard", "Snow pillows", "Precipitation")),
      dateInput(ns("comment_start_date"), "Start date (inclusive)", value = Sys.Date() - 7, max = Sys.Date() - 1),
      dateInput(ns("comment_end_date"), "End date (inclusive)",value = Sys.Date(), max = Sys.Date()),
      actionButton(ns("FOD_go"), "Load data")
    ),
    mainPanel(
      DT::dataTableOutput(ns("FOD_table")),
      downloadButton(ns("export_fod_comments"), "Export as .csv")
    )
  )
}

FOD <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c("FOD_go"))
    
    ns <- session$ns  # Used for generating UI elements from server
    
    #Create containers
    FOD_comments <- reactiveValues(comments = list(),
                                   dates = vector(),
                                   tables = list("general" = data.frame(),
                                                 "specific" = data.frame()))
    
    shinyjs::hide("export_fod_comments")
    
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
      types <- if (input$comment_data_type == "All") c("levels", "flows", "snow", "bridges", "precipitation") else if (input$comment_data_type == "Water levels") "levels" else if (input$comment_data_type == "Water flows") "flows" else if (input$comment_data_type == "Snow pillows") "snow" else if (input$comment_data_type == "Bridge freeboard") "bridges" else if (input$comment_data_type == "Precipitation") "precipitation"
      
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
    
  }) # End of moduleServer
}
