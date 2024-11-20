
master <- openxlsx::read.xlsx("G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/1_YOWN_MASTER_TABLE/YOWN_MASTER.xlsx", sheet = "YOWN_MASTER")
YOWNIDs <- master$YOWN.Code

logger_tracking <- "G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/3_OTHER/YOWN_Logger_Tracking.xlsx"

YOWN_logger_folder <- "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_LOGGER_FILE_DROPBOX"

#### Define pressure conversion function ####
convert_pressure <- function(column, pressure_unit) {
  
  # Define conversion factors for pressure units to meters of water column
  pressure_conversion_factors <- data.frame(
    unit = c("psi", "kPa", "bar", "mbar", "mm Hg", "in Hg", "cm H2O", "in H2O"),
    to_m_H2O = c(
      0.7030700000, # psi
      0.1019716213, # kPa
      10.1971621298, # bar
      0.0101971621, # mbar
      0.0135950982, # mm Hg
      0.3453187748, # in Hg
      0.0101971621, # cm H2O
      0.0254000000  # in H2O
    )
  )
  
  # Check if the unit is "m H2O"
  if (pressure_unit == "m") {
    return(column)
  } else {
    # Find the conversion factor for the specified unit
    conversion_factor <- pressure_conversion_factors$to_m_H2O[pressure_conversion_factors$unit == pressure_unit]
    # If the unit is found, apply the conversion; if not, throw an error and stop code
    if (length(conversion_factor) == 1) {
      column <- column * conversion_factor
      return(column)
    } else {
      stop(paste("Unrecognized unit:", pressure_unit, "- No conversion applied."))
    }
  }
}

#### Define depth conversion function ####
convert_depth <- function(column, depth_unit) {
  
  # Define conversion factors for pressure units to meters of water column
  depth_conversion_factors <- data.frame(
    unit = c("mm", "cm", "in", "ft"),
    to_m = c(1000, # mm
             100, # cm
             39.3701, # in
             3.28084 # ft
    )
  )
  
  # Check if the unit is "m H2O" already
  if (depth_unit != "m") {
    # Find the conversion factor for the specified unit
    conversion_factor <- depth_conversion_factors$to_m[depth_conversion_factors$unit == depth_unit]
    # If the unit is found, apply the conversion; if not, throw an error and stop code
    if (length(conversion_factor) == 1) {
      column <- column * conversion_factor
    } else {
      stop(paste("Unrecognized unit:", depth_unit, "- No conversion applied."))
    }
    return(column)
  } else {
    return(column)
  }
}

#### Define temperature conversion function ####
convert_temp <- function(column, temperature_unit) {
  if (temperature_unit == "°C") {
    # No conversion needed
    return(column)
  } else if (temperature_unit == "°F") {
    # Convert Fahrenheit to Celsius
    column <- (column - 32) * 5/9
    return(column)
  } else {
    stop("Unsupported temperature unit. Please use '°C' or '°F'.")
  }
}
