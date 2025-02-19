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
  dplyr::select(YOWN.Code, Name)
YOWNIDs_pY <- master_sheet$YOWN.Code[master_sheet$Publish == "YES"]
write(paste0("#### YOWN SITES FILTERED BY PUBLISH COLUMN ####\n", paste(setdiff(YOWNIDs, YOWNIDs_pY), collapse = ", ")), file = log_file)

# Read current year analysis sheet
analysis_sheet_cy <- openxlsx::read.xlsx(analysis_sheet, sheet = as.character(as.numeric(format(Sys.Date(), "%Y")) - 1))

# Read previous year analysis sheet
analysis_sheet_py <- openxlsx::read.xlsx(analysis_sheet, sheet = as.character(as.numeric(format(Sys.Date(), "%Y")) - 2))

#### Create vector of YOWN.Codes to be analyzed, based on current year and previous year peak and trough captures ####
# Create vector of YOWN.Codes for which Peak.Captured is Y for both current analysis_sheet_cy and analysis_sheet_py
cy_peak_f <- analysis_sheet_cy %>%
  dplyr::filter(Peak.Captured == "Y") %>%
  dplyr::pull("YOWN.Code")

py_peak_f <- analysis_sheet_py %>%
  dplyr::filter(Peak.Captured == "Y") %>%
  dplyr::pull("YOWN.Code")

peak_f <- intersect(cy_peak_f, py_peak_f)

write(paste0("#### YOWN SITES FILTERED BY UNCAPTURED PEAK IN CURRENT YEAR OR PREVIOUS YEAR ####\n", paste(setdiff(YOWNIDs, peak_f), collapse = ", ")), file = log_file)

# Create vector of YOWN.Codes for which Trough.Captured is Y for both current analysis_sheet_cy and analysis_sheet_py
cy_trough_f <- analysis_sheet_cy %>%
  dplyr::filter(Trough.Captured == "Y") %>%
  dplyr::pull("YOWN.Code")

py_trough_f <- analysis_sheet_py %>%
  dplyr::filter(Trough.Captured == "Y") %>%
  dplyr::pull("YOWN.Code")

trough_f <- intersect(cy_trough_f, py_trough_f)

write(paste0("#### YOWN SITES FILTERED BY UNCAPTURED TROUGH IN CURRENT YEAR OR PREVIOUS YEAR ####\n", paste(setdiff(YOWNIDs, trough_f), collapse = ", ")), file = log_file)

write(paste0("#### TOTAL NUMBER OF YOWN SITES SCREENED DUE TO UNCAPTURED PEAK OR TROUGH ####\n", length(union(peak_f, trough_f)), " out of ", length(YOWNIDs)), file = log_file)

manual_scrn <- c()

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
  
  # location
  # parameter = 1166 # m bgs
  # record_rate = NULL
  # period_type = NULL
  # z = NULL
  # tzone = "MST"
  # start_date = "1990-01-01"
  # end_date = Sys.Date()
  # master_sheet = master_sheet
  # analysis_sheet = analysis_sheet
  
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
    stop("There are multiple timeseries IDs for this parameter and location combol;. Adjust script to handle z elevations. Thanks a lot Ghislain.")
  }
  tsid <- tsid[1,1]
  
  #### Retrieve data and metadata, format and prepare ####
  
  # Retrieve timestamp and data, set up blank grade and qualifier columns
  data <- DBI::dbGetQuery(con, paste0("SELECT datetime, value_corrected AS value FROM measurements_continuous_corrected WHERE timeseries_id = ", tsid, ";")) %>%
    dplyr::mutate(grade = NA, qualifier = NA)
  
  # Retrieve grades, add to data
  grades <- DBI::dbGetQuery(con, paste0("SELECT gt.grade_type_code, gt.grade_type_description, g.start_dt, g.end_dt FROM grades AS g LEFT JOIN grade_types AS gt ON gt.grade_type_id = g.grade_type_id WHERE g.timeseries_id = ", tsid, ";"))
  
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


#### Define sin regression function ####
sinreg <- function(x, y, fixed_period = NA, plot = FALSE) {
  if (is.na(fixed_period)) {
    Ots <- ts(y)
    ssp <- spectrum(Ots, plot = FALSE)
    Nper <- 1/ssp$freq[ssp$spec == max(ssp$spec)]
    Dper <- Nper * diff(range(x))/length(x)
  }
  else if (is.numeric(fixed_period) & length(fixed_period) == 
           1) {
    Dper <- fixed_period
  }
  else {
    stop("ERROR: Supplied value for fixed period is not recognized")
  }
  sinlm <- lm(y ~ sin(2 * pi/Dper * x) + cos(2 * pi/Dper * 
                                               x))
  sinm <- sinlm$fitted
  if (plot == TRUE) {
    dev.new()
    plot(x, y)
    lines(x, sinm, col = "red")
  }
  coeff <- summary(sinlm)[["coefficients"]]
  I <- coeff[1, 1]
  A <- sqrt(coeff[2, 1]^2 + coeff[3, 1]^2)
  phase <- -acos(coeff[2, 1]/A)
  peak <- (0.25 + (phase/(2 * pi))) * Dper
  R2adj <- summary(sinlm)$adj.r.squared
  p <- as.numeric(pf(summary(sinlm)$fstatistic[1], summary(sinlm)$fstatistic[2], 
                     summary(sinlm)$fstatistic[3], lower.tail = F))
  metadata <- data.frame("Parameter" = c("Intercept", "Amplitude", 
                                         "Period", "Phase", "R2adj", "p-value"), "Value" = c(I, A, Dper, 
                                                                                             phase, R2adj, p))
  return(list(metadata, sinm))
}

#### Data Processing ####

# Apply YOWN_GetDBdata to every i in in YOWNIDs, create a df containing every instance of grade UNK
for (i in YOWNIDs) {
  print(i)
  full_data <- tryCatch({
    YOWN_getDBdata(location = i, start_date = "1990-01-01", end_date = Sys.Date())
  }, error = function(e) {
    print(paste0(e))
    return(NULL)
  })
  # If the grades column contains UNK, print thw YOWN ID with UNK
  if ("UNK" %in% full_data$grade) {
    print(paste0(i, " UNK"))
  }
}


# Create blank data frame with columns YOWN.Code, cy_peak, py_peak, and peak_change
peak_df <- data.frame(YOWN.Code = peak_f,
                      cy_peak = NA,
                      py_peak = NA,
                      peak_change = NA)
trough_df <- data.frame(YOWN.Code = trough_f,
                        cy_trough = NA,
                        py_trough = NA,
                        trough_change = NA)
for (i in YOWNIDs) {
  print(i)
  # Get full data for station
  full_data <- tryCatch({
    YOWN_getDBdata(location = i, start_date = "1990-01-01", end_date = Sys.Date())
  }, error = function(e) {
    print(paste0(e))
    return(NULL)
  })
  
  if (!is.null(full_data)) {
    # Filter data for current year-1, pull max and min, add to peak_df and trough_df
    cy_data <- full_data %>%
      dplyr::filter(datetime >= as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-01-01")))
    cy_max <- min(cy_data$value)
    cy_max_date <- cy_data$datetime[which(cy_data$value == cy_max)]
    cy_min <- max(cy_data$value)
    cy_min_date <- cy_data$datetime[which(cy_data$value == cy_min)]
    peak_df[peak_df$YOWN.Code == i, "cy_peak"] <- cy_max
    trough_df[trough_df$YOWN.Code == i, "cy_trough"] <- cy_min
    
    # Filter data for current year-2, pull max and min, add to peak_df and trough_df
    py_data <- full_data %>%
      dplyr::filter(datetime >= as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 2, "-01-01"))) %>%
      dplyr::filter(datetime < as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-01-01")))
    py_max <- min(py_data$value)
    py_max_date <- py_data$datetime[which(py_data$value == py_max)]
    py_min <- max(py_data$value)
    py_min_date <- py_data$datetime[which(py_data$value == py_min)]
    peak_df[peak_df$YOWN.Code == i, "py_peak"] <- py_max
    trough_df[trough_df$YOWN.Code == i, "py_trough"] <- py_min
    
    # Filter peak_df by YOWN codes in peak_f
    peak_df <- peak_df[peak_df$YOWN.Code %in% peak_f,]
    
    # Filter trough_df by YOWN codes in trough_f
    trough_df <- trough_df[trough_df$YOWN.Code %in% trough_f,]
  }
}

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

ggplot2::ggsave(plot = final_plot, filename = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\YOWN_MAXcompare.jpg",  height = 8.5, width = 11, units = "in")

print("Max Plot Generated")

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

ggplot2::ggsave(plot = final_plot, filename = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\YOWN_MINcompare.jpg",  height = 8.5, width = 11, units = "in")

print("Min Plot Generated")






# Create smoothed time series data
cy_loess <- loess(value ~ as.numeric(datetime), data = cy_data, span = 0.8)
cy_loess <- data.frame(datetime = cy_data$datetime, value = predict(cy_loess))
# identify regions in cy_loess where slope  = 0
cy_loess_d <- diff(cy_loess$value)
index_loess <- which(diff(sign(cy_loess_d)) != 0 ) + 1
points_loess <- which(cy_loess$datetime %in% cy_loess$datetime[index_loess])
points_loess <- data.frame(datetime = cy_loess$datetime[points_loess], value = cy_loess$value[points_loess])
# Create a data frame to store max, min, and corresponding dates
limits <- data.frame(cy_max_date = cy_max_date, cy_max = cy_max, cy_min_date = cy_min_date, cy_min = cy_min)

peak_df[peak_df$YOWN.Code == i, "cy_peak"] <- cy_max
trough_df[trough_df$YOWN.Code == i, "cy_trough"] <- cy_min

#### Check for suitability of sine regression ####
sinregression <- sinreg(cy_data$datetime, cy_data$value) 
sinmetadata <- sinregression[[1]]

# Check for suitable R2 value
if (sinmetadata[which(sinmetadata$Parameter == "R2adj"), 2] > 0.9) {
  print(i)
  sinregression <- data.frame(datetime = cy_data$datetime, value = sinregression[[2]])
  # Calculate peak/trough and inflection points from sin regression
  sinregression_d <- diff(sinregression$value)
  index_pt <- which(diff(sign(sinregression_d)) != 0 ) + 1
  points_pt <- which(sinregression$datetime %in% sinregression$datetime[index_pt])
  points_pt <- data.frame(datetime = sinregression$datetime[points_pt], value = sinregression$value[points_pt])
  sinregression_d2 <- diff(sinregression_d)
  index_infl <- which(diff(sign(sinregression_d2)) != 0 ) + 1
  points_infl <- which(sinregression$datetime %in% sinregression$datetime[index_infl])
  points_infl <- data.frame(datetime = sinregression$datetime[points_infl], value = sinregression$value[points_infl])
  
} else {
  print("R2 value is less than 0.9, indicating fluctuations may not follow a sinusoidal pattern")
}
}




plot <- ggplot2::ggplot() +
  ggplot2::geom_line(data = cy_data, ggplot2::aes(x = datetime, y = value), color = "blue") +
  # ggplot2::geom_line(data = cy_loess, ggplot2::aes(x = datetime, y = value), color = "black") +
  # ggplot2::geom_line(data = test2, ggplot2::aes(x = datetime, y = value), color = "red") +
  ggplot2::geom_line(data = cy_loess, ggplot2::aes(x = datetime, y = value), color = "black") +
  ggplot2::geom_line(data = sinregression, ggplot2::aes(x = datetime, y = value), color = "purple") +
  ggplot2::geom_point(data = points_infl, ggplot2::aes(x = datetime, y = value), color = "brown",size = 3) +
  ggplot2::geom_point(data = points_pt, ggplot2::aes(x = datetime, y = value), color = "green",size = 3) +
  ggplot2::geom_point(data = points_loess, ggplot2::aes(x = datetime, y = value), color = "red",size = 3) +
  ggplot2::scale_y_reverse(n.breaks = 10)
plot



# Filter data for current year-2, pull max and min


# Smooth data by weekly averaging




py_data <- full_data %>%
  dplyr::filter(datetime >= as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 2, "-01-01"))) %>%
  dplyr::filter(datetime < as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-01-01")))
py_max <- min(py_data$value)
py_max_date <- py_data$datetime[which(py_data$value == py_max)]
py_min <- max(py_data$value)
py_min_date <- py_data$datetime[which(py_data$value == py_min)]
peak_df[peak_df$YOWN.Code == i, "py_peak"] <- py_max
trough_df[trough_df$YOWN.Code == i, "py_trough"] <- py_min
}

# Get last date of previous year
py_start_date <- paste0(as.numeric(format(Sys.Date(), "%Y")) - 2, "-01-01")


# Fill in cy_peak and py_peak columns with max yearly values



