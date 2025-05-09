# This script contains a collection of functions which are used by the 'learnR' function.

#' learnR: learn R with a Water Resources Branch twist
#' 
#' @description 
#' This function takes no arguments and opens an R script where you can learn R in a Water Resources Branch context.
#'
#' @param lesson The lesson number to open. Leave NULL (default) and the function will attempt to find the last lesson you had open, or, if you completed the lesson and ran the last line, the next lesson.
#' 
#' @returns A file with exercises to learn R.
#' @export
#'
#' @examples
#' 
#' # Open the last lesson you worked on:
#' learnR()
#' 
#' # Open the first lesson:
#' learnR(1)


learnR <- function(lesson = NULL) {
  
  # Get the last lesson if needed
  if (is.null(lesson)) {
    lesson <- .get_last_lesson()
    # If no last lesson, set to 1 (user hasn't opened any lessons yet)
    if (is.null(lesson)) {
      lesson <- 1
    }
  }
  
  # Check if within RStudio
  if (interactive()) {
    # Copy the file to open it while preserving the original copy
    original <- system.file(paste0("learnR/lesson", lesson, ".R"), package = "YGwater")
    new <- paste0(.cache_dir(), "/lesson", lesson, ".R")
    file.copy(original, new, overwrite = TRUE)
    
    # Check if the user is in RStudio
    if (Sys.getenv("RSTUDIO") == "1") {
      rstudioapi::navigateToFile(new, line = 1, column = 1)
    } else {
      # Open the file in a text editor
      utils::file.edit(new)
    }
  } else {
    # Open the file in a text editor
    stop("This function can only be run in an interactive developement environment (IDE), ideally RStudio.")
  }
  
  # Set the last lesson
  .set_last_lesson(lesson)
}

#' Hints for the learnR exercises
#'
#' @param exercise The exercise number for which you want a hint.
#'
#' @returns A hint for the specified exercise.
#' @export
#'
#' @examples
#' 
#' learnR_hint(1)

learnR_hint <- function(exercise) {
  # Ask the user if they're sure they want a hint
  cat("Are you sure you want a hint? (y/n): ")
  response <- readline()
  if (tolower(response) != "y") {
    cat("Allright!.\n")
    return()
  }
  
  if (exercise == 1) {
    cat("Hint: the console is usually the bottom-left pane and is where you would have just type in a 'y', and a library call looks like this:  library(packageName).\n")
  } else if (exercise == 2) {
    # Ask the user
    # Give a hint to a novice user for creating a data.frame with a column for date ('Date' class) and 'value' (Numeric, sine function)
    cat("Hint: you can generate a sequence of dates using the seq.Date() function. For example, seq.Date(from = as.Date('2023-01-01'), to = as.Date('2023-01-10'), by = 'day') will give you a sequence of dates from January 1, 2023 to January 10, 2023.\n")
    
    # ASk the user if they need another hint
    cat("Do you need another hint? (y/n): ")
    response <- readline()
    if (tolower(response) == "y") {
      cat("Hint: You can create a sequence of numbers from 1 to 10, increasing by 1 using seq(from = 1, to = 1, by = 1).\n")
    } else {
      return()
    }
    
    # Ask the user if they need another hint
    cat("Do you need another hint? (y/n): ")
    response <- readline()
    if (tolower(response) == "y") {
      cat("Hint: You can use the sin() function to generate sine values. For example, sin(seq(0, 2*pi, length.out = 10)) will give you 10 sine values between 0 and 2*pi.\n")
    } else {
      return()
    }
    
    # Ask the user if they need another hint
    cat("Do you need another hint? (y/n): ")
    response <- readline()
    if (tolower(response) == "y") {
      cat("Hint: With 365 days in 2023, you'll need 365 corresponding entries in the 'values' column. Look at the length.out argument of the seq() function.\n")
    } else {
      return()
    }
  } else if (exercise == 2) {
    
  }
}

learnR_answer <- function(exercise) {
  # Ask the user if they're sure they want the answer
  cat("Are you sure you want the answer? (y/n): ")
  response <- readline()
  if (tolower(response) != "y") {
    cat("Allright!.\n")
    return()
  }
  
  if (exercise == 1) {
    cat("Answer: In the R console window, usually in the bottom left of the screen in RSTudio, type in  library('YGwater'). I've added this to your console to help you out, you just need to press 'Enter'.\n")
    rstudioapi::sendToConsole("library('YGwater')", execute = FALSE)
  } else if (exercise == 2) {
    cat("Answer: You can generate a sequence of dates using the seq.Date() function. For example, seq.Date(from = as.Date('2023-01-01'), to = as.Date('2023-01-10'), by = 'day') will give you a sequence of dates from January 1, 2023 to January 10, 2023.\n")
}
  
}


# Helper functions
# helper to find the per‐user config dir for your package
#' @noRd
#' 
.config_dir <- function() {
  dir <- tools::R_user_dir("YGwater", "config")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

# path to the “last lesson” file
#' @noRd
#' 
.last_lesson_file <- function() {
  file.path(.config_dir(), "last_lesson.rds")
}

# write the last‐opened lesson name/id
#' @noRd
#' 
.set_last_lesson <- function(lesson_id) {
  saveRDS(lesson_id, .last_lesson_file())
}

# read it back (NULL if none yet)
#' @noRd
#' 
.get_last_lesson <- function() {
  path <- .last_lesson_file()
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

# path to the user's copy directory
#' @noRd
#' 
.cache_dir <- function() {
  dir <- tools::R_user_dir("YGwater", "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}
