#' Calculate standards using formulas in EQWin
#' 
#' The EQWin database stores formulas for calculating standards. This function retrieves the formula, parses it into R code, and calculates the standard for a specified parameter. As these calculations typically require additional measured values, the SampleId is also required. Note that the target parameter is NOT required; this is identified by the CalcId.
#'
#' @param CalcId The ID identifying the calculation to perform, from column CalcId of table eqcalcs.
#' @param SampleId The sample ID for the calculation, from column SampleId of table eqsampls. This is required to retrieve the additional parameter values and the sample date needed for the calculation.
#' @param con A connection to the EQWin database. Default NULL creates a connection to the default database location and closes the connection when done.
#'
#' @return A value for the calculated standard.
#' @export
#'

EQWinStd <- function(CalcId, SampleId, con = NULL) {

  # initial checks, connection, and validations #######################################################################################
  
  # Connect to EQWin
  if (is.null(con)) {
    EQWin <- AccessConnect("X:/EQWin/WR/DB/Water Resources.mdb", silent = TRUE)
    on.exit(DBI::dbDisconnect(EQWin), add = TRUE)
  } else {
    EQWin <- con
  }

  
  # Check that the CalcId, ParamId, SampleId exist while retrieving necessary data
  script <- DBI::dbGetQuery(EQWin, paste0("SELECT CalcScript FROM eqcalcs WHERE CalcId = ", CalcId))$CalcScript
  
  # Check encoding and if necessary
  locale_info <- Sys.getlocale("LC_CTYPE")
  encoding <- sub(".*\\.([^@]+).*", "\\1", locale_info)
  if (encoding != "utf8") {
    script <- iconv(script, from = encoding, to = "UTF-8")
  }
  
  if (length(script) == 0) {
    stop("CalcId does not exist in the database.")
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
  params <- stats::setNames(results$Result, results$ParamCode)
  
  # Create functions for later use ####################################################################################################
  # Function to replace parameter names with their values, defaulting to 0 if missing
  replace_params <- function(expression, params) {
    for (param in names(params)) {
      expression <- gsub(paste0("\\[", param, "\\]"), params[param], expression)
    }
    expression <- gsub("\\[2\\.CollectDateTime\\]", paste0("'", as.Date(sample$CollectDateTime), "'"), expression)
    
    expression <- gsub("#(y{4})", paste0("#", lubridate::year(sample$CollectDateTime)), expression)
    expression <- gsub("-m{2}-)", paste0("-", lubridate::month(sample$CollectDateTime), "-"), expression)
    expression <- gsub("-d{2}#)", paste0("-", lubridate::day(sample$CollectDateTime), "#"), expression)
    
    expression <- gsub("#(\\d{4}-\\d{2}-\\d{2})#", "'\\1'", expression)
    
    # Replace any remaining parameter placeholders with 0
    expression <- gsub("\\[[^]]*\\]", "0", expression)
    
    return(expression)
  }

  # Replace letters in 'expression' with their values in 'vars'
  replace_vars <- function(expression, vars) {
    for (var in names(vars)) {
      expression <- gsub(var, vars[var], expression)
    }
    return(expression)
  }
  
  # Function to format the expression for evaluation in R
  format <- function(expression) {
    # Replace "=" (but not "<=", ">=", "=<", "=>") with "==" for evaluation in R
    expression <- gsub("([^<>=])=([^<>=])", "\\1==\\2", expression)      
    # If single capital letters are still present surround them in single quotes so the equation parses properly
    expression <- gsub("([A-Z])", "'\\1'", expression)
    # Replace @exp, @lna, @log with their R equivalents
    expression <- gsub("@exp", "exp", expression)
    expression <- gsub("@lna", "log", expression)
    expression <- gsub("@log", "log10", expression)
    expression <- gsub("@ln", "log", expression)
    return(expression)
  }
  
  # Evaluate the script ##############################################################################################################
  # Initialize variables
  variables <- list("X" = NA)
  first_x <- FALSE
  # Evaluate each line of the script
  for (line in actual_script) {
    # Remove spaces from everything except for character strings intended as calculation outputs
    quoted_portion <- regmatches(line, gregexpr('"[^"]*"', line))[[1]]
    cleaned_expression <- gsub(' ".*?"', '', line)  # Remove the quoted part temporarily
    cleaned_expression <- gsub(" ", "", cleaned_expression)  # Remove all spaces
    line <- paste0(cleaned_expression, quoted_portion)  # Add the quoted part back
    
    # Replace parameter names with their actual values
    line <- replace_params(line, params)
    
    # Split the line at the "=" sign to get the variable and the expression. The expression can in turn contain "=" signs.
    parts <- unlist(strsplit(line, "=", fixed = TRUE))
    var <- parts[1]
    expr <- paste(parts[-1], collapse = "=")
    
    if (var == "X") {
      first_x <- TRUE
    } else {
      # Sometimes X is not evaluated as TRUE and then other variables come before other X assignments.
      first_x <- FALSE
    }
    
    # Evaluate the expression
    if (grepl("\\?", expr)) {
      # Handle conditional expressions
      condition <- strsplit(expr, "\\?")[[1]][1]
      true_value <- strsplit(strsplit(expr, "\\?")[[1]][2], ":")[[1]][1]
      false_value <- ifelse(length(strsplit(strsplit(expr, "\\?")[[1]][2], ":")[[1]]) > 1, 
                            strsplit(strsplit(expr, "\\?")[[1]][2], ":")[[1]][2], 
                            NA)

      # If a letter is present in the condition, replace it with its value (if possible)
      condition <- replace_vars(condition, variables)
      condition <- format(condition)
      
      # Evaluate the condition and, if needed, the TRUE/FALSE value
      condition_result <- eval(parse(text = condition))
      
     # Same treatment as above on true_value and false_value

      # All or a portion of true_value might be in " " quotes, and this portion should not be replaced nor does it need to go through the next steps.
      if (condition_result) {
        true_value_quoted <- regmatches(true_value, gregexpr('"[^"]*"', true_value))[[1]]
        true_value <- gsub('".*?"', '', true_value)  # Remove the quoted part temporarily
        if (nchar(true_value) > 0) {
          true_value <- replace_vars(true_value, variables)
          true_value <- format(true_value)
        }
        true_value <- paste0(true_value, true_value_quoted)
        
        variables[[var]] <- eval(parse(text = true_value))
        if (first_x) {
          break
        }
        
      } else if (!condition_result) {
        if (!is.na(false_value)) {
          false_value_quoted <- regmatches(false_value, gregexpr('"[^"]*"', false_value))[[1]]
          false_value <- gsub('".*?"', '', false_value)  # Remove the quoted part temporarily
          if (nchar(false_value) > 0) {
            false_value <- replace_vars(false_value, variables)
            false_value <- format(false_value)
          }
          false_value <- paste0(false_value, false_value_quoted)
          variables[[var]] <- ifelse(!is.na(false_value), eval(parse(text = false_value)), variables[[var]])
        } else {
          # The condition evaluated as FALSE and there is no FALSE value, so do nothing and move on to the next line even if first_x is TRUE
          next
        }
      }
    } else { # ? is not present in the expression so we're dealing with direct assignment
      if (grepl("[A-Z]", expr) && nchar(expr) == 1) { # If the expression is a simple variable
        expr <- replace_vars(expr, variables)
        variables[[expr]] <- expr
      } else { # The expression must be a simple value OR a calculation that may or may not involve other variables
        # Deal with dates
        expr <- gsub("\\[2\\.CollectDateTime\\]", paste0("'", as.Date(sample$CollectDateTime), "'"), expr)
        expr <- gsub("#(\\d{4}-\\d{2}-\\d{2})#", "'\\1'", expr)
        # Replace any letters in the expression with their values
        expr <- replace_vars(expr, variables)
        expr <- format(expr)
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
