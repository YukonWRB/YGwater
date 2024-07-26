#' Calculate standards using formulas in EQWin
#' 
#' The EQWin database stores formulas for calculating standards. This function retrieves the formula, parses it into R code, and calculates the standard for a specified parameter. As these calculations typically require additional parameter measurements, the SampleId is also required. You can then use the 
#'
#' @param CalcId The ID identifying the calculation to perform, from column CalcId of table eqcalcs.
#' @param ParamId The parameter ID for the calculation, from column ParamId of table eqcalcs.
#' @param SampleId The sample ID for the calculation, from column SampleId of table eqsampls. This is required to retrieve the additional parameter values needed for the calculation.
#' @param dbPath The path to the EQWin database. Default is "X:/EQWin/WR/DB/Water Resources.mdb".
#'
#' @return A value for the calculated standard.
#' @export
#'

EQWinStd <- function(CalcId, ParamId, SampleId, dbPath = "X:/EQWin/WR/DB/Water Resources.mdb") {
  
  # initial checks, connection, and validations #######################################################################################

  if (!inherits(CalcId, "integer") | !inherits(ParamId, "integer") | !inherits(SampleId, "integer")) {
    stop("CalcId, ParamId, and SampleId must be integers.")
  }
  
  # Connect to EQWin
  EQWin <- AccessConnect(dbPath, silent = TRUE)
  on.exit(DBI::dbDisconnect(EQWin), add = TRUE)

  # Check that the CalcId, ParamId, SampleId exist while retrieving necessary data
  calc <- DBI::dbGetQuery(EQWin, paste0("SELECT * FROM eqcalcs WHERE CalcId = ", CalcId))
  
  
  # Return the calculated standard value
  return(value)
}
