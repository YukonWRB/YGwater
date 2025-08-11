#' Creates the files for download on Yukon Open Data
#'
#' @description
#' The purpose of this script is create files for the Yukon Open Data portal.
#' @return Two excel documents, one with location metadata and another with the data. The snow data includes all surveys in the database. it does not include individual samples, but simply a mean of all samples.
#' @export

createSnowOpenData <- function() {
    
    # Pull data from snow bulletin
    con <- snowConnect(silent = TRUE)
    locations <- DBI::dbGetQuery(con, "SELECT location, name, basin, sub_basin, active, elevation, latitude, longitude FROM locations")
    snow_data <- DBI::dbGetQuery(con, "SELECT location, name, sub_basin, target_date, survey_date, swe, depth, estimate_flag FROM means")
    
    field_descriptions <- DBI::dbGetQuery(con, "SELECT LOCATION_COMMENT FROM information_Schema.columns WHERE TABLE_SCHEMA = ''")
    DBI::dbDisconnect(con)
    
    # Field descriptions
    field_descriptions <- data.frame(Field = c("location", "name", "sub_basin", "target_date", "survey_date", "swe", "depth", "estimate_flag"),
                                     Description = c("The snow course the measurement is associated to.",
                                                     "The commonly used name of the snow course the measurement is associated to.",
                                                     "The sub-basin is which the measurement was taken. Refers to the basin names used in the snow bulletin.",
                                                     "The target date for the snow survey. Usually the first of the month.",
                                                     "The date on which the snow survey was conducted.",
                                                     "The mean SWE of all the samples taken for the snow survey.",
                                                     "The mean snow depth of all the samples taken for the snow survey.",
                                                     "TRUE if one of the samples of the survey have estimate_flag = TRUE. Currently, only samples which are themselves means of multiple samples should be flagged as an estimate."
                                                     ))
    
  }
