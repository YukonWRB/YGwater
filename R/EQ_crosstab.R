#' Concatenate 'crosstab' type lab reports for entry to EQWin
#' 
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' This function is designed to concatenate lab reports that are in a 'crosstab' format. The function reads in the lab reports, concatenates them, and saves the output as an Excel file. The function is designed to work with lab reports that are in a specific format, and may need to be modified to work with other formats.
#'
#' @param folder The folder containing the lab reports. Default "choose" opens a dialog box to select the folder.
#' @param save_path The path to save the concatenated report (folder path only). Default NULL saves the output in the folder provided in the first argument, 'choose' lets you choose interactively, or specify your own.
#'
#' @return An Excel file with the concatenated lab reports.
#' @export
#'

EQ_crosstab <- function(folder = "choose", save_path = NULL) {
  
  
  if (folder == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select folder where your lab reports are located.")
    folder <- rstudioapi::selectDirectory(caption = "Select Lab Report Folder", path = file.path(Sys.getenv("USERPROFILE"),"Desktop"))
  } else {
    if (!dir.exists(folder)) {
      stop("The folder you specified does not exist.")
    }
  }
  
  if (is.null(save_path)) {
    save_path <- folder
  } else if (save_path == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select the folder folder where you want the workbook saved.")
    save_path <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"),"Desktop"))
  } else {
    if (!dir.exists(save_path)) {
      stop("The save path you specified does not exist.")
    }
  }
  
  # Get the files into two lists: one for the headers and one for the data
  files <- list.files(folder, pattern = ".xlsx", full.names = TRUE)
  
  header <- list()
  data <- list()
  tryCatch({
    for (i in 1:length(files)) {
      header[[i]] <- data.table::as.data.table(openxlsx::read.xlsx(files[i], sheet = 1, startRow = 1, rows = c(1:14), colNames = FALSE))
      data[[i]] <-  data.table::as.data.table(openxlsx::read.xlsx(files[i], sheet = 1, startRow = 15))
    }
  }, error = function(e) {
    stop("There was an error reading the files. Make sure that none are open in Excel!")
  })
  
  # 2. Combine into two data.frames
  for (i in 1:length(data)) {
    if (i == 1) {
      # Deal with data
      results <- data[[i]]
      results <- unique(results)
      if (nrow(results[results$Parameter.Code == "P-T", ]) > 1) {
        results[results$Parameter.Code == "P-T", ][1,"Parameter.Code"] <- "P-PO4T"
      }
      tmp.head <- header[[i]]
      data.table::setnames(results, c(names(results)[1:3], as.character(tmp.head[2, 2:ncol(tmp.head)])))
      # Deal with headers
      head <- header[[i]]
      data.table::setnames(head, c("Variable", as.character(header[[i]][2, 2:ncol(header[[i]])])))
    } else {
      # Deal with data
      tmp <- data[[i]]
      tmp <- unique(tmp)
      if (nrow(tmp[tmp$Parameter.Code == "P-T", ]) > 1) {
        tmp[tmp$Parameter.Code == "P-T", ][1,"Parameter.Code"] <- "P-PO4T"
      }
      tmp.head <- header[[i]]
      data.table::setnames(tmp, c(names(tmp)[1:3], as.character(tmp.head[2, 2:ncol(tmp.head)])))
      results <- merge(results, tmp, all.x = TRUE, all.y = TRUE)
      
      # Deal with headers
      tmp.head[, names(tmp.head)[1] := NULL]
      data.table::setnames(tmp.head, as.character(c(tmp.head[2, 1:ncol(tmp.head)])))
      
      head <- cbind(head, tmp.head)
    }
  }
  
  # Tack on the head to the result data.frame, remembering to add two empty cols in front of the header
  blank <- data.table::data.table(delete = rep(NA, nrow(head)), delete = rep(NA, nrow(head)))
  head <- cbind(blank, head)  # Now should have a df with same # of cols as results
  
  # Create the workbook
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "Sheet1")
  
  openxlsx::writeData(wb, 1, head, startCol = 1, startRow = 1, colNames = FALSE) # Write the header with no column names
  openxlsx::writeData(wb, sheet = 1, x = "", startCol = 1, startRow = nrow(head) + 1) # Write a blank row
  openxlsx::writeData(wb, sheet = 1, results, startCol = 1, startRow = nrow(head) + 2, colNames = TRUE)
  
  # Save the workbook
  openxlsx::saveWorkbook(wb, paste0(save_path, "/EQ_crosstab_concat_", Sys.Date(), ".xlsx"), overwrite = TRUE)
  
  return("Done")
}
