#### YOWN Yearly Data Workup ####

#### Initialize database connection ####
con <- AquaConnect()

#### Read in excel sheets ####
master_sheet <- "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\2_SPREADSHEETS\\1_YOWN_MASTER_TABLE\\YOWN_MASTER.xlsx"
master_sheet <- openxlsx::read.xlsx(master_sheet, sheet = "YOWN_MASTER")

analysis_sheet <- "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\2_SPREADSHEETS\\1_YOWN_MASTER_TABLE\\YOWN_MASTER_Data_Tracking_Analysis.xlsx"

# Extract vector of all YOWN IDs, filter by publication status
YOWNIDs <- master_sheet$YOWN.Code[master_sheet$Publish == "YES"]

# Read current year analysis sheet
analysis_sheet_cy <- openxlsx::read.xlsx(analysis_sheet, sheet = as.character(as.numeric(format(Sys.Date(), "%Y")) - 1))

# Read previous year analysis sheet
analysis_sheet_py <- openxlsx::read.xlsx(analysis_sheet, sheet = as.character(as.numeric(format(Sys.Date(), "%Y")) - 2))

#### Create vector of YOWN.Codes to be analyzed, based on current year and previous year peak and trough captures ####
cy_peak_f <- analysis_sheet_cy %>%
  dplyr::filter(Peak.Captured == "Y") %>%
  dplyr::pull("YOWN.Code")

py_peak_f <- analysis_sheet_py %>%
  dplyr::filter(Peak.Captured == "Y") %>%
  dplyr::pull("YOWN.Code")

peak_f <- intersect(cy_peak_f, py_peak_f)

# Create vector of YOWN.Codes for which Trough.Captured is Y for both current analysis_sheet_cy and analysis_sheet_py
cy_trough_f <- analysis_sheet_cy %>%
  dplyr::filter(Trough.Captured == "Y") %>%
  dplyr::pull("YOWN.Code")

py_trough_f <- analysis_sheet_py %>%
  dplyr::filter(Trough.Captured == "Y") %>%
  dplyr::pull("YOWN.Code")

trough_f <- intersect(cy_trough_f, py_trough_f)

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

#### Data Processing ####

# Create blank data frame with columns YOWN.Code, cy_peak, py_peak, and peak_change
data <- data.frame(YOWN.Code = cy_peak_f,
                   cy_peak = NA,
                   py_peak = NA,
                   peak_change = NA)

# For each 


