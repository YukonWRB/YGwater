#' Calculate standards using formulas in EQWin
#' 
#' The EQWin database stores formulas for calculating standards. This function retrieves the formula, parses it into R code, and calculates the standard for a specified parameter. As these calculations typically require additional parameter measurements, the SampleId is also required.
#'
#' @param CalcId The ID identifying the calculation to perform, from column CalcId of table eqcalcs.
#' @param ParamId The parameter ID for the calculation, from column ParamId of table eqcalcs.
#' @param SampleId The sample ID for the calculation, from column SampleId of table eqsampls. This is required to retrieve the additional parameter values and the sample date needed for the calculation.
#' @param dbPath The path to the EQWin database. Default is "X:/EQWin/WR/DB/Water Resources.mdb".
#'
#' @return A value for the calculated standard.
#' @export
#'

EQWinStd <- function(CalcId, ParamId, SampleId, dbPath = "X:/EQWin/WR/DB/Water Resources.mdb") {

  # initial checks, connection, and validations #######################################################################################
  
  # Connect to EQWin
  EQWin <- AccessConnect(dbPath, silent = TRUE)
  on.exit(DBI::dbDisconnect(EQWin), add = TRUE)
  
  # Check that the CalcId, ParamId, SampleId exist while retrieving necessary data
  script <- DBI::dbGetQuery(EQWin, paste0("SELECT CalcScript FROM eqcalcs WHERE CalcId = ", CalcId))$CalcScript
  if (length(script) == 0) {
    stop("CalcId does not exist in the database.")
  }
  param <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamCode FROM eqparams WHERE ParamId = ", ParamId))$ParamCode
  if (length(param) == 0) {
    stop("ParamId does not exist in the database.")
  }
  sample <- DBI::dbGetQuery(EQWin, paste0("SELECT SampleId, CollectDateTime FROM eqsampls WHERE SampleId = ", SampleId))
  if (nrow(sample) == 0) {
    stop("SampleId does not exist in the database.")
  }
  
  results <- DBI::dbGetQuery(EQWin, paste0("SELECT eqdetail.ParamId, eqdetail.Result, eqparams.ParamCode FROM eqdetail INNER JOIN eqparams ON eqdetail.ParamId = eqparams.ParamId WHERE eqdetail.SampleId = ", SampleId))
  
  # Convert results to numeric, handle "<" values as 0
  results$Result <- sapply(results$Result, function(x) {
    if (grepl("^<", x)) {
      return(0)
    } else {
      return(as.numeric(x))
    }
  })
  
  # Prepare the script and variables for parsing #######################################################################################
  # Extract comments and actual script lines
  script <- gsub("\r", "", script)
  script_lines <- unlist(strsplit(script, "\n"))
  actual_script <- script_lines[!grepl("^\\*", script_lines)]
  actual_script <- actual_script[actual_script != ""]
  
  
  # Extract lines beginning with "#". Not sure what to do with these yet!
  comments <- actual_script[grepl("^#", actual_script)]
  actual_script <- actual_script[!grepl("^#", actual_script)]
  
  # Create a named vector for parameters
  params <- setNames(results$Result, results$ParamCode)
  
  # Create functions for later use ####################################################################################################
  # Function to replace parameter names with their values, defaulting to 0 if missing
  replace_params <- function(expression, params) {
    for (param in names(params)) {
      expression <- gsub(paste0("\\[", param, "\\]"), params[param], expression)
    }
    # Replace any remaining parameter placeholders with 0
    expression <- gsub("\\[[^\\]]+\\]", "0", expression)
    return(expression)
  }
  
  replace_vars <- function(expression, vars) {
    # Replace letters in 'expression' with their values in 'vars'
    for (var in names(vars)) {
      expression <- gsub(var, vars[var], expression)
    }
    return(expression)
  }
  
  # Evealuate the script ##############################################################################################################
  # Initialize variables
  variables <- list("X" = NA)
  first_x <- FALSE
  # Evaluate each line of the script
  for (line in actual_script) {
    line <- gsub(" ", "", line) # Remove spaces as they are sometimes present but not always
    
    # Replace parameter names with their actual values
    line <- replace_params(line, params)
    
    # Split the line at the "=" sign to get the variable and the expression. The expression can in turn contain "=" signs.
    parts <- unlist(strsplit(line, "=", fixed = TRUE))
    var <- parts[1]
    expr <- paste(parts[-1], collapse = "=")
    
    if (var == "X") {
      first_x <- TRUE
    }
    
    # Evaluate the expression
    if (grepl("\\?", expr)) {
      # Handle conditional expressions
      condition <- strsplit(expr, "\\?")[[1]][1]
      true_value <- strsplit(strsplit(expr, "\\?")[[1]][2], ":")[[1]][1]
      false_value <- ifelse(length(strsplit(strsplit(expr, "\\?")[[1]][2], ":")[[1]]) > 1, 
                            strsplit(strsplit(expr, "\\?")[[1]][2], ":")[[1]][2], 
                            NA)

      # Replace [2.CollectDateTime] with the sample collect date, and fix dates stuck between # signs to simple character strings
      condition <- gsub("\\[2\\.CollectDateTime\\]", paste0("'", as.Date(sample$CollectDateTime), "'"), condition)
      condition <- gsub("#(\\d{4}-\\d{2}-\\d{2})#", "'\\1'", condition)
      # If a letter is present in the condition, replace it with its value (if possible)
      condition <- replace_vars(condition, variables)
      # Replace "=" (but not "<=", ">=", "=<", "=>") with "==" for evaluation in R
      condition <- gsub("([^<>=])=([^<>=])", "\\1==\\2", condition)      
      # If single capital letters are still present surround them in single quotes so the equation parses properly
      condition <- gsub("([A-Z])", "'\\1'", condition)
      # Replace @exp, @lna, @log with their R equivalents
      condition <- gsub("@exp", "exp", condition)
      condition <- gsub("@lna", "log", condition)
      condition <- gsub("@log", "log10", condition)
      
      # Evaluate the condition and, if needed, the TRUE/FALSE value
      condition_result <- eval(parse(text = condition))
      
     # Same treatment as above on true_value and false_value
      
      true_value <- gsub("\\[2\\.CollectDateTime\\]", paste0("'", as.Date(sample$CollectDateTime), "'"), true_value)
      false_value <- gsub("#(\\d{4}-\\d{2}-\\d{2})#", "'\\1'", false_value)
      true_value <- gsub("\\[2\\.CollectDateTime\\]", paste0("'", as.Date(sample$CollectDateTime), "'"), true_value)
      false_value <- gsub("#(\\d{4}-\\d{2}-\\d{2})#", "'\\1'", false_value)
      true_value <- replace_vars(true_value, variables)
      false_value <- replace_vars(false_value, variables)
      true_value <- replace_vars(true_value, variables)
      false_value <- replace_vars(false_value, variables)
      true_value <- gsub("([^<>=])=([^<>=])", "\\1==\\2", true_value)
      false_value <- gsub("([^<>=])=([^<>=])", "\\1==\\2", false_value)
      true_value <- gsub("([A-Z])", "'\\1'", true_value)
      false_value <- gsub("([A-Z])", "'\\1'", false_value)
      true_value <- gsub("@exp", "exp", true_value)
      true_value <- gsub("@lna", "log", true_value)
      true_value <- gsub("@log", "log10", true_value)
      false_value <- gsub("@exp", "exp", false_value)
      false_value <- gsub("@lna", "log", false_value)
      false_value <- gsub("@log", "log10", false_value)
      
      # Assign the result based on the condition
      if (condition_result) {
        variables[[var]] <- eval(parse(text = true_value))
        if (first_x) {
          break
        }
      } else {
        variables[[var]] <- ifelse(!is.na(false_value), eval(parse(text = false_value)), variables[[var]])
      }
    } else { # ? is not present in the expression so we're dealing with direct assignment
      if (grepl("[A-Z]", expr) && nchar(expr) == 1) { # If the expression is a simple variable
        variables[[expr]] <- expr
      } else { # The expression must be a simple value OR a calculation that may or may not involve other variables
        # Deal with dates
        expr <- gsub("\\[2\\.CollectDateTime\\]", paste0("'", as.Date(sample$CollectDateTime), "'"), expr)
        expr <- gsub("#(\\d{4}-\\d{2}-\\d{2})#", "'\\1'", expr)
        # Replace any letters in the expression with their values
        expr <- replace_vars(expr, variables)
        expr <- gsub("([^<>=])=([^<>=])", "\\1==\\2", expr)      
        # If letters are still present surround them in single quotes so the equation parses
        expr <- gsub("([a-zA-Z])", "'\\1'", expr)
        # Replace @exp, @lna, @log with their R equivalents
        expr <- gsub("@exp", "exp", expr)
        expr <- gsub("@lna", "log", expr)
        expr <- gsub("@log", "log10", expr)
        # Evaluate the expression
        expr <- eval(parse(text = expr))
      }
      variables[[var]] <- expr
      if (first_x) {
        break
      }
    }
  }
  
  # Return the calculated standard value
  return(variables$X)
}
