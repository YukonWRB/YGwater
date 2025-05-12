# This script contains a collection of functions which are used by the 'learnR' function.

#' learnR: learn R with a Water Resources Branch twist
#' 
#' @description 
#' This function takes no arguments and opens an R script where you can learn R in a Water Resources Branch context.
#'
#' @param lesson The lesson number to open. Leave NULL (default) and the function will attempt to find the last lesson you had open, or, if you completed the lesson and ran the last line, the next lesson.
#' 
#' @returns A file with lessons to learn R.
#' @export
#'
#' @examples
#' 
#' # Open the last lesson you worked on, or lesson 1 is you haven't opened a lesson yet:
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
      stop("This training course is designed to work with RStudio. Please open the file in RStudio.")
    }
  } else {
    # Open the file in a text editor
    stop("This training course must be used in an interactive session, in RStudio. Please open the file in RStudio.")
  }
  
  # Set the last lesson
  .set_last_lesson(lesson)
}

#' Hints for the learnR lessons
#'
#' @param lesson The lesson number for which you want a hint.
#' @param part The part of the lesson for which you want a hint.
#'
#' @returns A hint for the specified lesson.
#' @export
#'
#' @examples
#' 
#' learnR_hint(1)

learnR_hint <- function(lesson, part = 1) {
  # # Ask the user if they're sure they want a hint
  # cat("Are you sure you want a hint? (y/n): ")
  # response <- readline()
  # if (tolower(response) != "y") {
  #   cat("Allright!.\n")
  #   return()
  # }
  
  if (lesson == 1) {
    cat("Hint: the console is usually the bottom-left pane and is where you would have just type in a 'y', and a library call looks like this:  library(packageName).\n")
  } else if (lesson == 2) {
    if (part == 1) {
      cat("You call a function by typing its name (with pacakgeName:: first if you haven't done a library call) in the console, followed by parantheses. If there are arguments they go in the parentheses, but for swirl there are no arguments. Just swirl::swirl() will do!")
    } else {
      cat("There are no hints for this part of lesson 2!")
    }
  } else if (lesson == 3) {
    if (part == 1) {
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
    } else if (part == 2) {
      cat("There are no hints for this part.")
    } else if (part == 3) {
      cat("There are no hints for this part.")
    } else if (part == 4) {
      cat("There are no hints for this part.")
    } else if (part == 5) {
      cat("The assignment operator is '<-' in R, and it is used to assign values to variables. For example, x <- 5 assigns the value 5 to the variable x. Once x becomes a variable, you can assign it to a new variable, as in y <- y\n")
    }
    
  } else if (lesson == 4) {
    cat("There are no hints for this lesson!")
  } else {
    cat("There are no hints for this lesson!")
  }
}

#' Answers for the learnR lessons
#'
#' @param lesson The lesson number for which you want the answer.
#' @param part The part of the lesson for which you want the answer.
#'
#' @returns The answer for the specified lesson, both as text and as a command to be executed in the console.
#' @export
#'
#' @examples
#' 
#' learnR_answer(1)

learnR_answer <- function(lesson, part = 1) {
  # # Ask the user if they're sure they want the answer
  # cat("Are you sure you want the answer? (y/n): ")
  # response <- readline()
  # if (tolower(response) != "y") {
  #   cat("Allright!.\n")
  #   return()
  # }
  
  if (lesson == 1) {
    cat("Answer: In the R console window (where you're reading this message), type in  library('YGwater'). I've added this to your console to help you out, you just need to press 'Enter'.\n")
    rstudioapi::sendToConsole("library('YGwater')", execute = FALSE)
  } else if (lesson == 2) {
    cat("No answer available for this lesson.\n")
  } else if (lesson == 3) {
    if (part == 1) {
      cat("Answer: Use df <- data.frame() to create a data.frame, and assign columns as such: data.frame(date = ???, value = ???). The date sequence can be made as such: seq.Date(from = as.Date('2023-01-01'), to = as.Date('2023-12-31'), by = 'day'), and the value sequence can be made as such: sin(seq(0, 2*pi, length.out = 365))\n")
      rstudioapi::sendToConsole("df  <- data.frame(date = seq.Date(from = as.Date('2023-01-01'), to = as.Date('2023-12-31'), by = 'day'),\nvalue = sin(seq(0, 2*pi, length.out = 365)))", execute = FALSE)
    } else if (part == 2) {
      cat("Answer: df$value[1:30] means 'take df_value and select rows 1 to 30 (inclusively)'. We need the mean of that, so...\n")
      rstudioapi::sendToConsole("mean(df$value[1:30])", execute = FALSE)
    } else if (part == 3) {
      cat("Answer: Look below!\n")
      rstudioapi::sendToConsole('plot(df$date, df$value, type = "l", xlab = "Date", ylab = "Value", main = "Sine wave pattern")', execute = FALSE)
    } else if (part == 4) {
      cat("Answer: Look below!\n")
      rstudioapi::sendToConsole('df$value <- df$value * 10', execute = FALSE)
    } else if (part == 5) {
      cat("Answer: Look below!\n")
      rstudioapi::sendToConsole('df2 <- df', execute = FALSE)
    } else if (part == 6) {
      cat("Answer: Look below!\n")
      rstudioapi::sendToConsole('df2$date <- df2$date + 365', execute = FALSE)
    } else if (part == 7) {
      cat("Answer: Look below!\n")
      rstudioapi::sendToConsole('df3 <- rbind(df, df2)', execute = FALSE)
    }
  } else if (lesson == 4) {
    
    
    
  } else {
    cat("No answer available for this lesson.\n")
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
