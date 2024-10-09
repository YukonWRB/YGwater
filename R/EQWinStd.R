#' Calculate standards using formulas in EQWin (vectorized)
#' 
#' The EQWin database stores formulas for calculating standards. This function retrieves the formula, parses it into R code, and calculates the standard for a specified parameter. As these calculations typically require additional measured values, the SampleId is also required. Note that the target parameter is NOT required; this is identified by the CalcId.
#'
#' @param CalcIds A vector of IDs identifying the calculation to perform, from column CalcId of table eqcalcs.
#' @param SampleIds A vector of SampleIds for the calculation, from column SampleId of table eqsampls. This is required to retrieve the additional parameter values and the sample date needed for the calculation.
#' @param con A connection to the EQWin database. Default NULL creates a connection to the default database location ("//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb") and closes the connection when done.
#'
#' @return A value for the calculated standard.
#' @export
#'

EQWinStd <- function(CalcIds, SampleIds, con = NULL) {
  # Ensure CalcIds and SampleIds are vectors
  CalcIds <- unique(CalcIds)
  SampleIds <- unique(SampleIds)
  
  # Connect to EQWin
  if (is.null(con)) {
    EQWin <- AccessConnect("//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb", silent = TRUE)
    on.exit(DBI::dbDisconnect(EQWin), add = TRUE)
  } else {
    EQWin <- con
  }
  
  # Fetch CalcScripts for all unique CalcIds
  calc_query <- paste0("SELECT CalcId, CalcScript FROM eqcalcs WHERE CalcId IN (", paste(CalcIds, collapse = ","), ");")
  calc_scripts_df <- DBI::dbGetQuery(EQWin, calc_query)
  
  if (nrow(calc_scripts_df) == 0) {
    stop("No valid CalcIds found in the database.")
  }
  
  # Fetch sample data for all SampleIds
  sample_query <- paste0("SELECT SampleId, CollectDateTime FROM eqsampls WHERE SampleId IN (", paste(SampleIds, collapse = ","), ");")
  samples_df <- DBI::dbGetQuery(EQWin, sample_query)
  
  if (nrow(samples_df) == 0) {
    stop("No valid SampleIds found in the database.")
  }
  
  # Fetch results for all SampleIds
  results_query <- paste0(
    "SELECT eqdetail.SampleId, eqdetail.ParamId, eqdetail.Result, eqparams.ParamCode ",
    "FROM eqdetail INNER JOIN eqparams ON eqdetail.ParamId = eqparams.ParamId ",
    "WHERE eqdetail.SampleId IN (", paste(SampleIds, collapse = ","), ");"
  )
  results_df <- DBI::dbGetQuery(EQWin, results_query)
  
  # Convert results to numeric, handle "<" values as 0
  results_df$Result <- sapply(results_df$Result, function(x) {
    if (grepl("^<", x)) {
      return(0)
    } else {
      return(as.numeric(x))
    }
  })
  
  # Prepare helper functions
  replace_params <- function(expression, params, sample_date) {
    for (param in names(params)) {
      expression <- gsub(paste0("\\[", param, "\\]"), params[[param]], expression)
    }
    expression <- gsub("\\[2\\.CollectDateTime\\]", paste0("'", as.Date(sample_date), "'"), expression)
    
    # Replace date components
    expression <- gsub("#(y{4})", paste0("#", format(sample_date, "%Y")), expression)
    expression <- gsub("-m{2}-", paste0("-", format(sample_date, "%m"), "-"), expression)
    expression <- gsub("-d{2}#", paste0("-", format(sample_date, "%d"), "#"), expression)
    
    expression <- gsub("#(\\d{4}-\\d{2}-\\d{2})#", "'\\1'", expression)
    
    # Replace any remaining parameter placeholders with 0
    expression <- gsub("\\[[^]]*\\]", "0", expression)
    
    return(expression)
  }
  
  replace_vars <- function(expression, vars) {
    for (var in names(vars)) {
      expression <- gsub(var, vars[[var]], expression, fixed = TRUE)
    }
    return(expression)
  }
  
  format_expression <- function(expression) {
    # Replace "=" (but not "<=", ">=", "=<", "=>") with "==" for evaluation in R
    expression <- gsub("([^<>=])=([^<>=])", "\\1==\\2", expression)
    # Surround single capital letters with single quotes
    expression <- gsub("([A-Z])", "'\\1'", expression)
    # Replace function names
    expression <- gsub("@exp", "exp", expression)
    expression <- gsub("@lna", "log", expression)
    expression <- gsub("@log", "log10", expression)
    expression <- gsub("@ln", "log", expression)
    return(expression)
  }
  
  # Function to process and evaluate a script for a single sample
  process_script <- function(script_lines, params, sample_date) {
    variables <- list("X" = NA)
    first_x <- FALSE
    
    for (line in script_lines) {
      # Remove spaces except within quotes
      quoted_portion <- regmatches(line, gregexpr('"[^"]*"', line))[[1]]
      cleaned_expression <- gsub(' ".*?"', '', line)  # Remove the quoted part temporarily
      cleaned_expression <- gsub(" ", "", cleaned_expression)  # Remove all spaces
      line <- paste0(cleaned_expression, quoted_portion)  # Add the quoted part back
      
      # Replace parameter names with actual values
      line <- replace_params(line, params, sample_date)
      
      # Split the line at the "=" sign
      parts <- unlist(strsplit(line, "=", fixed = TRUE))
      var <- parts[1]
      expr <- paste(parts[-1], collapse = "=")
      
      if (var == "X") {
        first_x <- TRUE
      } else {
        first_x <- FALSE
      }
      
      if (grepl("\\?", expr)) {
        # Handle conditional expressions
        condition <- strsplit(expr, "\\?")[[1]][1]
        true_value <- strsplit(strsplit(expr, "\\?")[[1]][2], ":")[[1]][1]
        false_value <- ifelse(length(strsplit(strsplit(expr, "\\?")[[1]][2], ":")[[1]]) > 1,
                              strsplit(strsplit(expr, "\\?")[[1]][2], ":")[[1]][2],
                              NA)
        
        condition <- replace_vars(condition, variables)
        condition <- format_expression(condition)
        condition_result <- eval(parse(text = condition))
        
        if (condition_result) {
          true_value_quoted <- regmatches(true_value, gregexpr('"[^"]*"', true_value))[[1]]
          true_value <- gsub('".*?"', '', true_value)  # Remove the quoted part temporarily
          if (nchar(true_value) > 0) {
            true_value <- replace_vars(true_value, variables)
            true_value <- format_expression(true_value)
          }
          true_value <- paste0(true_value, true_value_quoted)
          
          variables[[var]] <- eval(parse(text = true_value))
          if (first_x) {
            break
          }
        } else {
          if (!is.na(false_value)) {
            false_value_quoted <- regmatches(false_value, gregexpr('"[^"]*"', false_value))[[1]]
            false_value <- gsub('".*?"', '', false_value)  # Remove the quoted part temporarily
            if (nchar(false_value) > 0) {
              false_value <- replace_vars(false_value, variables)
              false_value <- format_expression(false_value)
            }
            false_value <- paste0(false_value, false_value_quoted)
            variables[[var]] <- eval(parse(text = false_value))
          } else {
            next
          }
        }
      } else {
        # Direct assignment
        expr <- replace_vars(expr, variables)
        expr <- format_expression(expr)
        variables[[var]] <- eval(parse(text = expr))
        if (first_x) {
          break
        }
      }
    }
    
    return(variables$X)
  }
  
  # Process each CalcScript and create functions
  calc_functions <- list()
  for (i in seq_len(nrow(calc_scripts_df))) {
    calc_id <- calc_scripts_df$CalcId[i]
    script <- calc_scripts_df$CalcScript[i]
    
    # Encoding check and conversion
    script <- gsub("\r", "", script)
    script_lines <- unlist(strsplit(script, "\n"))
    # Remove comments and empty lines
    actual_script <- script_lines[!grepl("^\\*|^#", script_lines)]
    actual_script <- actual_script[actual_script != ""]
    
    # Create a function that applies the script to a sample's data
    calc_functions[[as.character(calc_id)]] <- function(params, sample_date) {
      process_script(actual_script, params, sample_date)
    }
  }
  
  # Prepare the results list
  results_list <- list()
  
  # Apply each function to all samples
  for (calc_id in CalcIds) {
    func <- calc_functions[[as.character(calc_id)]]
    # Initialize a vector to store results
    calc_results <- numeric(length(SampleIds))
    
    for (j in seq_along(SampleIds)) {
      sample_id <- SampleIds[j]
      # Get sample date
      sample_date <- samples_df$CollectDateTime[samples_df$SampleId == sample_id]
      # Get parameters for this sample
      sample_results <- results_df[results_df$SampleId == sample_id, ]
      params <- stats::setNames(sample_results$Result, sample_results$ParamCode)
      
      # Apply the function
      calc_results[j] <- func(params, sample_date)
    }
    
    # Store results in a data frame
    results_df_temp <- data.frame(
      SampleId = SampleIds,
      Value = calc_results
    )
    results_list[[as.character(calc_id)]] <- results_df_temp
  }
  
  return(results_list)
}
