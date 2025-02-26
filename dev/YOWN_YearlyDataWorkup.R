#### YOWN Yearly Data Workup ####

#### Initialize database connection ####
con <- AquaConnect()

#### Read in excel sheets ####
master_sheet <- "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\2_SPREADSHEETS\\1_YOWN_MASTER_TABLE\\YOWN_MASTER.xlsx"
master_sheet <- openxlsx::read.xlsx(master_sheet, sheet = "YOWN_MASTER")
analysis_sheet <- "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\2_SPREADSHEETS\\1_YOWN_MASTER_TABLE\\YOWN_MASTER_Data_Tracking_Analysis.xlsx"

# Create text file to log progress
log_file <- file(paste0("G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\", format(Sys.Date(), "%Y"), " YOWN_YearlyDataWorkup_Log.txt"), open = "wt")
write("#### YOWN ANALYSIS LOG FILE ####\n", file = log_file)

# Extract vector of all YOWN IDs, write filtered sites by publish column
YOWNIDs <- na.omit(master_sheet$YOWN.Code)
YOWNnames <- master_sheet %>%
  dplyr::select("YOWN.Code", "Name")
YOWNIDs_pY <- master_sheet$YOWN.Code[master_sheet$Publish == "YES"]
write(paste0("#### YOWN SITES FILTERED BY PUBLISH COLUMN ####\n", paste(setdiff(YOWNIDs, YOWNIDs_pY), collapse = ", ")), file = log_file)

# Set manual screen wells
manual_scrn <- c("YOWN-1923", "YOWN-1908")
YOWNIDs_pY <- na.omit(setdiff(YOWNIDs_pY, manual_scrn))

# Read current year analysis sheet
analysis_sheet_cy <- openxlsx::read.xlsx(analysis_sheet, sheet = as.character(as.numeric(format(Sys.Date(), "%Y")) - 1))

# Read previous year analysis sheet
analysis_sheet_py <- openxlsx::read.xlsx(analysis_sheet, sheet = as.character(as.numeric(format(Sys.Date(), "%Y")) - 2))

#### Create vector of YOWN.Codes to be analyzed, based on current year and previous year peak and trough captures ####
# Create vector of YOWN.Codes for which Peak.Captured is Y for both current analysis_sheet_cy and analysis_sheet_py
cy_peak_f <- analysis_sheet_cy %>%
  dplyr::filter(Peak.Captured == "Y") %>%
  dplyr::filter(YOWN.Code %in% YOWNIDs_pY) %>%
  dplyr::pull("YOWN.Code")
  

py_peak_f <- analysis_sheet_py %>%
  dplyr::filter(Peak.Captured == "Y") %>%
  dplyr::filter(YOWN.Code %in% YOWNIDs_pY) %>%
  dplyr::pull("YOWN.Code")

peak_f <- intersect(cy_peak_f, py_peak_f)

write(paste0("#### YOWN SITES FILTERED BY UNCAPTURED PEAK IN CURRENT YEAR OR PREVIOUS YEAR ####\n", paste(setdiff(YOWNIDs, peak_f), collapse = ", ")), file = log_file)

# Create vector of YOWN.Codes for which Trough.Captured is Y for both current analysis_sheet_cy and analysis_sheet_py
cy_trough_f <- analysis_sheet_cy %>%
  dplyr::filter(Trough.Captured == "Y") %>%
  dplyr::filter(YOWN.Code %in% YOWNIDs_pY) %>%
  dplyr::pull("YOWN.Code")

py_trough_f <- analysis_sheet_py %>%
  dplyr::filter(Trough.Captured == "Y") %>%
  dplyr::filter(YOWN.Code %in% YOWNIDs_pY) %>%
  dplyr::pull("YOWN.Code")

trough_f <- intersect(cy_trough_f, py_trough_f)

write(paste0("#### YOWN SITES FILTERED BY UNCAPTURED TROUGH IN CURRENT YEAR OR PREVIOUS YEAR ####\n", paste(setdiff(YOWNIDs, trough_f), collapse = ", ")), file = log_file)

write(paste0("#### TOTAL NUMBER OF YOWN SITES SCREENED DUE TO UNCAPTURED PEAK OR TROUGH ####\n", length(union(peak_f, trough_f)), " out of ", length(YOWNIDs)), file = log_file)

# Combine peak_f and trough_f
peak_trough_f <- dplyr::union(peak_f, trough_f)

#### Define DB fetch function ####
YOWN_getDBdata <- function(location,
                           parameter = 1166, # m bgs
                           record_rate = NULL,
                           period_type = NULL,
                           z = NULL,
                           tzone = "MST",
                           start_date = "1990-01-01",
                           end_date = Sys.Date(),
                           master_sheet = master_sheet,
                           analysis_sheet = analysis_sheet) {
  
  # location = "YOWN-1908"
  # parameter = 1166 # m bgs
  # record_rate = NULL
  # period_type = NULL
  # z = NULL
  # tzone = "MST"
  # start_date = "1990-01-01"
  # end_date = Sys.Date()
  # master_sheet = master_sheet
  # analysis_sheet = analysis_sheet
  # 
  # Function arg checks
  if (inherits(parameter, "character")) {
    parameter <- tolower(parameter)
    escaped_parameter <- gsub("'", "''", parameter)
    parameter_tbl <- DBI::dbGetQuery(con, 
                                     paste0("SELECT parameter_id, param_name, param_name_fr, unit_default FROM parameters WHERE param_name = '", escaped_parameter, "' OR param_name_fr = '", escaped_parameter, "';")
    )
    parameter_code <- parameter_tbl$parameter_id[1]
    if (is.na(parameter_code)) {
      stop("The parameter you entered does not exist in the database.")
    }
  } else if (inherits(parameter, "numeric")) {
    parameter_tbl <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, param_name, param_name_fr, unit_default FROM parameters WHERE parameter_id = ", parameter, ";"))
    if (nrow(parameter_tbl) == 0) {
      stop("The parameter you entered does not exist in the database.")
    }
    parameter_code <- parameter
  }
  units <- parameter_tbl$unit_default[1]
  
  #### Retrieve location and timeseries ID ####
  location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location, "';"))[1,1]
  
  if (length(location_id) == 0) {
    stop("This location ID doesnt exist")
  }
  
  tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter, " AND media_id = 2"))
  
  if (nrow(tsid) == 0) {
    stop("There is no data for this combination of parameter ID and location")
  } else if (nrow(tsid) > 1) {
    stop("There are multiple timeseries IDs for this parameter and location combo; Adjust script to handle z elevations. Thanks a lot Ghislain.")
  }
  tsid <- tsid[1,1]
  
  #### Retrieve data and metadata, format and prepare ####
  
  # Retrieve timestamp and data, set up blank grade and qualifier columns
  data <- DBI::dbGetQuery(con, paste0("SELECT datetime, value_corrected AS value FROM measurements_continuous_corrected WHERE timeseries_id = ", tsid, ";")) %>%
    dplyr::mutate(grade = NA, qualifier = NA)
  
  # Retrieve grades, add to data
  grades <- DBI::dbGetQuery(con, paste0("SELECT gt.grade_type_code, gt.grade_type_description, g.start_dt, g.end_dt FROM grades AS g LEFT JOIN grade_types AS gt ON gt.grade_type_id = g.grade_type_id WHERE g.timeseries_id = ", tsid, ";"))
  
  # Create index of rows with grades that are not A, B, C, or E
  gradechange_index <- which(!grades$grade_type_code %in% c("A", "B", "C", "E"))
  
  # Expand bad grades by 1 hour on each side
  for (i in gradechange_index) {
    grades[i, "start_dt"] <- (grades[i, "start_dt"] - 3600)
    grades[i, "end_dt"] <- (grades[i, "end_dt"] + 3600)
  }
  
  # Reduce good grades by 1 hour on each side
  for (i in setdiff(rownames(grades), gradechange_index)) {
    grades[i, "start_dt"] <- (grades[i, "start_dt"] + 3600)
    grades[i, "end_dt"] <- (grades[i, "end_dt"] - 3600)
  }
  
  for (i in 1:nrow(grades)) {
    start <- grades[i, "start_dt"]
    end <- grades[i, "end_dt"]
    data[data$datetime == start, "grade"] <- grades[i, "grade_type_code"]
  }
  
  if (is.na(data$grade[1])) {
    data$grade[1] <- "UNK"
    warning("Grades for timeseries ", tsid, " do not begin at start of time series. UNK added until first grade observed.")
  }

  # Fill grades
  data$grade <- zoo::na.locf(data$grade)
  
  # Retrieve qualifier, add to data
  qualifiers <- DBI::dbGetQuery(con, paste0("SELECT qt.qualifier_type_code, qt.qualifier_type_description, q.start_dt, q.end_dt FROM qualifiers AS q LEFT JOIN qualifier_types AS qt ON qt.qualifier_type_id = q.qualifier_type_id WHERE q.timeseries_id = ", tsid, ";"))
  
  if (length(qualifiers$qualifier_type_code) != 0) {
    # Create index of rows with qualifiers that are not A, B, C, or E
    qualchange_index <- which(!grades$grade_type_code %in% c("A", "B", "C", "E"))
    
    # Expand bad grades by 1 hour on each side
    for (i in gradechange_index) {
      grades[i, "start_dt"] <- (grades[i, "start_dt"] - 3600)
      grades[i, "end_dt"] <- (grades[i, "end_dt"] + 3600)
    }
    
    # Reduce good grades by 1 hour on each side
    for (i in setdiff(rownames(grades), gradechange_index)) {
      grades[i, "start_dt"] <- (grades[i, "start_dt"] + 3600)
      grades[i, "end_dt"] <- (grades[i, "end_dt"] - 3600)
    }
  }
  
  if (length(qualifiers$qualifier_type_code) != 0) {
    for (i in 1:nrow(qualifiers)) {
      start <- qualifiers[i, "start_dt"]
      end <- qualifiers[i, "end_dt"]
      data[data$datetime == start, "qualifier"] <- qualifiers[i, "qualifier_type_code"]
    }
  }
  
  if (is.na(data$qualifier[1])) {
    data$qualifier[1] <- "UNK"
    warning("Qualifier for timeseries ", tsid, " do not begin at start of time series. UNK added until first qualifier observed, if any.")
  }
  
  # Fill qualifiers
  data$qualifier <- zoo::na.locf(data$qualifier)
  
  # Convert timestamp to MST
  data$datetime <- as.POSIXct(data$datetime, tz = "MST")
  
  return(data)
}


#### Data Processing ####

# Create blank data frame with columns YOWN.Code, cy_peak, py_peak, and peak_change
peak_df <- data.frame(YOWN.Code = peak_f,
                      cy_peak = NA,
                      py_peak = NA,
                      peak_change = NA)
trough_df <- data.frame(YOWN.Code = trough_f,
                        cy_trough = NA,
                        py_trough = NA,
                        trough_change = NA)
# Initiate df to store peak and trough values for every year from 2001 to present, with separate columns for peak and trough for each year
historical_maxmin <- data.frame(YOWN.Code = YOWNIDs_pY,
                                historical_max = NA,
                                historical_min = NA)

for (i in YOWNIDs_pY) {
  print(i)
  # Get full data for station
  full_data <- tryCatch({
    YOWN_getDBdata(location = i, start_date = "1990-01-01", end_date = Sys.Date()) %>%
      # replace values where grade is UNS or N with NA
      dplyr::mutate(value = ifelse(.data$grade %in% c("UNS", "N"), NA, .data$value)) %>%
      # replace values where qualifier is DD with NA
      dplyr::mutate(value = ifelse(.data$qualifier == "DD", NA, .data$value))
  }, error = function(e) {
    print(paste0(e))
    return(NULL)
  })
  
  if (!is.null(full_data)) {
    # Filter data for current year and previous year
    cy_data <- full_data %>%
      dplyr::filter(datetime >= as.POSIXct(paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-01-01")))
    py_data <- full_data %>%
      dplyr::filter(datetime >= as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 2, "-01-01"))) %>%
      dplyr::filter(datetime < as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-01-01")))
    
    if (i %in% peak_f) {
      cy_max <- min(na.omit(cy_data$value))
      cy_max_date <- cy_data$datetime[which(cy_data$value == cy_max)]
      hist_max <- min(na.omit(full_data$value))
      hist_max_date <- full_data$datetime[which(full_data$value == hist_max)]
      peak_df[peak_df$YOWN.Code == i, "cy_peak"] <- cy_max # Add to peak comparison table
      py_max <- min(na.omit(py_data$value))
      py_max_date <- py_data$datetime[which(py_data$value == py_max)]
      peak_df[peak_df$YOWN.Code == i, "py_peak"] <- py_max
      if (cy_max == hist_max) { # Populate historical_maxmin table
        historical_maxmin[historical_maxmin$YOWN.Code == i, "historical_max"] <- "Y"
      } else {
        historical_maxmin[historical_maxmin$YOWN.Code == i, "historical_max"] <- "N"
      }
    }
    if (i %in% trough_f) {
      cy_min <- max(na.omit(cy_data$value))
      cy_min_date <- cy_data$datetime[which(cy_data$value == cy_min)]
      hist_min <- max(na.omit(full_data$value))
      hist_min_date <- full_data$datetime[which(full_data$value == hist_min)]
      trough_df[trough_df$YOWN.Code == i, "cy_trough"] <- cy_min # Add to trough comparison table
      py_min <- max(na.omit(py_data$value))
      py_min_date <- py_data$datetime[which(py_data$value == py_min)]
      trough_df[trough_df$YOWN.Code == i, "py_trough"] <- py_min
      if (cy_min == hist_min) { # Populate historical_maxmin table
        historical_maxmin[historical_maxmin$YOWN.Code == i, "historical_min"] <- "Y"
      } else {
        historical_maxmin[historical_maxmin$YOWN.Code == i, "historical_min"] <- "N"
      }
    }
  }
}

# Create data frames showing what percentage of wells had a historical maximum or minimum in 2024
historical_maxmin_perc <- data.frame(historical_max = NA,
                                     historical_min = NA)

# Find percentage of wells that reached their maximum value in 2024
historical_maxmin_perc$historical_max <- sum(na.omit(historical_maxmin$historical_max) == "Y") / length(na.omit(historical_maxmin$historical_max))*100
maxtext <- paste0(sum(na.omit(historical_maxmin$historical_max) == "Y"), " out of ", length(na.omit(historical_maxmin$historical_max)))

# Find percentage of wells that reached their minimum value in 2024
historical_maxmin_perc$historical_min <- sum(na.omit(historical_maxmin$historical_min) == "Y") / length(na.omit(historical_maxmin$historical_min))*100
mintext <- paste0(sum(na.omit(historical_maxmin$historical_min) == "Y"), " out of ", length(na.omit(historical_maxmin$historical_min)))

# Populate peak_change and trough_change columns
peak_df$peak_change <-  peak_df$py_peak - peak_df$cy_peak
trough_df$trough_change <-  trough_df$py_trough - trough_df$cy_trough
# Add YOWN names to peak_df and trough_df
peak_df <- dplyr::left_join(peak_df, YOWNnames, by = "YOWN.Code")
trough_df <- dplyr::left_join(trough_df, YOWNnames, by = "YOWN.Code")

#### Create max comparison plot ####
plot <- ggplot2::ggplot(data = peak_df, ggplot2::aes(x = reorder(Name, -peak_change), y = peak_change, fill = peak_change < 0)) +
  ggplot2::scale_fill_manual(name = "", values = c("FALSE" = "#7A9A01", "TRUE" = "#DC4405")) + 
  ggplot2::geom_bar(position = "dodge", stat = "identity") +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  cowplot::theme_cowplot() +
  ggplot2::theme(text = ggplot2::element_text(family = "Montserrat SemiBold"),
                 legend.position = "none",
                 plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
                 panel.border = ggplot2::element_rect(color = "grey",
                                                      fill = NULL,
                                                      linewidth = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 55,
                                                     hjust  = 1,
                                                     vjust = 1,
                                                     size = 9),
                 axis.line.x.bottom = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(hjust = 1,
                                                     size = 10),
                 axis.title.y = ggplot2::element_text(vjust = 2,
                                                      size = 10,
                                                      colour = "#464646"),
                 axis.line.y.left = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(colour = "lightgrey", linewidth = 0.5, linetype = 1)) +
  ggplot2::scale_y_continuous(name = "Change in Max GW Level from 2023 to 2024",
                              limits = c(plyr::round_any(min(na.omit(peak_df$peak_change)), 0.5, f = floor), plyr::round_any(max(na.omit(peak_df$peak_change)), 0.5, f = ceiling)),
                              breaks = seq(floor(min(na.omit(peak_df$peak_change))), ceiling(max(na.omit(peak_df$peak_change))), by = 0.5),
                              expand = c(0, 0))

title <- ggplot2::ggplot() +
  ggplot2::geom_blank() +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = paste0("Change in Maximum Groundwater Levels (2023-2024)")) +
  ggplot2::theme(text = ggplot2::element_text(family = "Montserrat SemiBold"),
                 plot.title = ggplot2::element_text(hjust = 0,
                                                    vjust = 0,
                                                    size = 14,
                                                    colour = "#244C5A",
                                                    face = "bold"),
                 plot.margin = ggplot2::unit(c(6.3, 0, 0, 0.51), "cm"))

caption <- ggplot2::ggplot() +
  ggplot2::geom_blank() +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = paste0("Plot generated: ", Sys.Date(), "\nYukon Observation Well Network")) +
  ggplot2::theme(text = ggplot2::element_text(family = "Montserrat SemiBold"),
                 plot.title = ggplot2::element_text(hjust = 0,
                                                    vjust = 0,
                                                    size = 9,
                                                    colour = "#464646"),
                 plot.margin = ggplot2::unit(c(-2.39, 0, 0, 0.6), "cm"))

subtitle <- ggplot2::ggplot() +
  ggplot2::geom_blank() +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = paste0("All YOWN wells with sufficient data quality and quantity for analysis")) +
  ggplot2::theme(text = ggplot2::element_text(family = "Montserrat SemiBold"),
                 plot.title = ggplot2::element_text(hjust = 0,
                                                    vjust = 0,
                                                    size = 10,
                                                    color = "#464646"),
                 plot.margin = ggplot2::unit(c(6.85, 0, 0, 0.6), "cm"))

# Use plot_grid method to combine titles, captions, and main plot in proper orientation
final <- cowplot::plot_grid(title, subtitle, plot, caption, ncol = 1, nrow = 4, rel_heights = c(0.1, 0.1, 2, 0.1))

# Add final aesthetic tweaks, print plot onto template
final_plot <- cowplot::ggdraw() +
  cowplot::draw_image("G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\01_MARKUP_IMAGES\\Template_nogrades.jpg") +
  cowplot::draw_plot(final)

ggplot2::ggsave(plot = final_plot, filename = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\2024_YOWN_MAXcompare.jpg",  height = 8.5, width = 11, units = "in")

print("Max Plot Generated")

#### Create historical max comparison plot ####
# Create data frame showing percentage of wells that reached the historical max in 2024

historical_max_pie <- data.frame(
  Category = c("Highest Level on Record", "Not Highest Level on Record"),
  Percentage = c(historical_maxmin_perc$historical_max, 100 - historical_maxmin_perc$historical_max)
)
# Round numbers to nearest percent
historical_max_pie$Percentage <- round(historical_max_pie$Percentage, 0)
historical_max_pie <- historical_max_pie %>%
  dplyr::mutate(labels = scales::percent(.data$Percentage/100))

maxpie <- ggplot2::ggplot(historical_max_pie, ggplot2::aes(x = "", y = Percentage, fill = Category)) +
  ggplot2::geom_col(colour = "black", linewidth = 1) +
  ggplot2::geom_bar(stat = "identity", width = 1) +
  ggplot2::coord_polar("y", start = 0) +
  ggplot2::scale_fill_manual(values = c("Highest Level on Record" = "#DC4405", "Not Highest Level on Record" = "#244C5A")) +
  ggplot2::theme_void() +
  ggplot2::geom_label(ggplot2::aes(x = 1.1, label = labels),
                      size = 4,
                      colour = "white",
                      position = ggplot2::position_stack(vjust = 0.5),
                      show.legend = FALSE) +
  ggplot2::ggtitle("Percentage of Wells that Reached a\nMaximum Level in 2024") +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 14),
                 plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")) +
  ggplot2::theme(text = ggplot2::element_text(),
                 legend.position = "bottom",
                 legend.margin = ggplot2::margin(-20,0,7,0),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(hjust = 0.5, vjust = -5, size = 9))
ggplot2::ggsave(plot = maxpie, filename = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\2024_MaxHistorical_PieChart.jpg",  height = 3, width = 3.5, units = "in")
  
#### Create min comparison plot ####
plot <- ggplot2::ggplot(data = trough_df, ggplot2::aes(x = reorder(Name, -trough_change), y = trough_change, fill = trough_change < 0)) +
  ggplot2::scale_fill_manual(name = "", values = c("FALSE" = "#7A9A01", "TRUE" = "#DC4405")) + 
  ggplot2::geom_bar(position = "dodge", stat = "identity") +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  cowplot::theme_cowplot() +
  ggplot2::theme(text = ggplot2::element_text(family = "Montserrat SemiBold"),
                 legend.position = "none",
                 plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
                 panel.border = ggplot2::element_rect(color = "grey",
                                                      fill = NULL,
                                                      linewidth = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 55,
                                                     hjust  = 1,
                                                     vjust = 1,
                                                     size = 9),
                 axis.line.x.bottom = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(hjust = 1,
                                                     size = 10),
                 axis.title.y = ggplot2::element_text(vjust = 2,
                                                      size = 10,
                                                      colour = "#464646"),
                 axis.line.y.left = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(colour = "lightgrey", linewidth = 0.5, linetype = 1)) +
  ggplot2::scale_y_continuous(name = "Change in Min GW Level from 2023 to 2024",
                              limits = c(plyr::round_any(min(na.omit(trough_df$trough_change)), 0.5, f = floor), plyr::round_any(max(na.omit(trough_df$trough_change)), 0.5, f = ceiling)),
                              breaks = seq(floor(min(na.omit(trough_df$trough_change))), ceiling(max(na.omit(trough_df$trough_change))), by = 0.5),
                              expand = c(0, 0))

title <- ggplot2::ggplot() +
  ggplot2::geom_blank() +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = paste0("Change in Minimum Groundwater Levels (2023-2024)")) +
  ggplot2::theme(text = ggplot2::element_text(family = "Montserrat SemiBold"),
                 plot.title = ggplot2::element_text(hjust = 0,
                                                    vjust = 0,
                                                    size = 14,
                                                    colour = "#244C5A",
                                                    face = "bold"),
                 plot.margin = ggplot2::unit(c(6.3, 0, 0, 0.51), "cm"))

# Use plot_grid method to combine titles, captions, and main plot in proper orientation
final <- cowplot::plot_grid(title, subtitle, plot, caption, ncol = 1, nrow = 4, rel_heights = c(0.1, 0.1, 2, 0.1))

# Add final aesthetic tweaks, print plot onto template
final_plot <- cowplot::ggdraw() +
  cowplot::draw_image("G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\01_MARKUP_IMAGES\\Template_nogrades.jpg") +
  cowplot::draw_plot(final)

ggplot2::ggsave(plot = final_plot, filename = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\2024_YOWN_MINcompare.jpg",  height = 8.5, width = 11, units = "in")

print("Min Plot Generated")

#### Create historical max comparison plot ####
# Create data frame showing percentage of wells that reached the historical max in 2024

historical_min_pie <- data.frame(
  Category = c("Lowest Level on Record", "Not Lowest Level on Record"),
  Percentage = c(historical_maxmin_perc$historical_min, 100 - historical_maxmin_perc$historical_min)
)
# Round numbers to nearest percent
historical_min_pie$Percentage <- round(historical_min_pie$Percentage, 0)
historical_min_pie <- historical_min_pie %>%
  dplyr::mutate(labels = scales::percent(.data$Percentage/100))

minpie <- ggplot2::ggplot(historical_min_pie, ggplot2::aes(x = "", y = Percentage, fill = Category)) +
  ggplot2::geom_col(colour = "black", linewidth = 1) +
  ggplot2::geom_bar(stat = "identity", width = 1) +
  ggplot2::coord_polar("y", start = 0) +
  ggplot2::scale_fill_manual(values = c("Lowest Level on Record" = "#DC4405", "Not Lowest Level on Record" = "#244C5A")) +
  ggplot2::theme_void() +
  ggplot2::geom_label(ggplot2::aes(x = 1.1, label = labels),
                      size = 4,
                      colour = "white",
                      position = ggplot2::position_stack(vjust = 0.5),
                      show.legend = FALSE) +
  ggplot2::ggtitle("Percentage of Wells that Reached a\n Historical Minimum Level in 2024") +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 14),
                 plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")) +
  ggplot2::theme(text = ggplot2::element_text(),
                 legend.position = "bottom",
                 legend.margin = ggplot2::margin(-20,0,7,0),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(hjust = 0.5, vjust = -5, size = 9))
ggplot2::ggsave(plot = minpie, filename = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\2024_MinHistorical_PieChart.jpg",  height = 3, width = 3.5, units = "in")

