#' Connect to the aquacache
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the aquacache database. You can pass your own database connection parameters, for example to connect to a dev instance instead of the default production instance.
#' Database superusers or admins will have access to all database records (unless your instance of aquacache was set up differently), but other users will be asked to provide their row level security username and password to access records other than the 'public' ones. Note that this is *not necessarily* the same username and password as the one used to log into the database itself.
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form aquacacheHost="hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter=value pair of form aquacachePort="1234".
#' @param username Username. By default searches the .Renviron file for parameter=value pair of form aquacacheUser="username". Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password. By default searches the .Renviron file for parameter=value pair of form aquacachePass="password".
#' @param RLS_user Username for row-level security. Default searches the .Renviron file for parameter=value pair of form RLS_user="username" and if not found either prompts the user for input or logs in as 'public' if not interactive.
#' @param RLS_pass Password for row-level security. Default searches the .Renviron file for parameter=value pair of form RLS_pass="password" and if not found either prompts the user for input or logs in as 'public' if not interactive.
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

AquaConnect <- function(name = "aquacache", host = Sys.getenv("aquacacheHost"), port = Sys.getenv("aquacachePort"), username = Sys.getenv("aquacacheUser"), password = Sys.getenv("aquacachePass"), RLS_user = Sys.getenv("RLS_user"), RLS_pass = Sys.getenv("RLS_pass"), silent = FALSE){

  tryCatch({
    con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)
    
    # Explicitly set the timezone to UTC as all functions in this package work with UTC timezones
    DBI::dbExecute(con, "SET timezone = 'UTC'")
    
    user <- DBI::dbGetQuery(con, "SELECT current_user;")
    if (!user[1,1] %in% c("postgres", "admin")) {
      if (nchar(RLS_user) > 0 && nchar(RLS_pass) > 0) { # If the credentials exist, try them
        res <- validateACUser(RLS_user, RLS_pass, con)
        if (res) {
          DBI::dbExecute(con, paste0("SET logged_in_user.username = '", RLS_user, "';"))
        } else { # If the credentials fail, log in as public and warn the user
          DBI::dbExecute(con, "SET logged_in_user.username = 'public';")
          message("Row-level security username or password failed when pulling from you .Renviron file. You are now logged in as 'public'.")
        }
      } else { # no credentials in the .renviron file
        if (interactive()) {
          # Prompt the user to enter their username and password
          message("You are not connecting to the database as a supersuer. Please enter your username and password (no quotes) to view records other than the 'public' ones, or enter nothing to log in as public.")
          username <- readline("Username: ")
          if (nchar(username) > 0) {
            password <- readline("Password: ")
            res <- validateACUser(username, password, con)
            if (res) {
              DBI::dbExecute(con, paste0("SET logged_in_user.username = '", username, "';"))
              message("You are now logged in as '", username, "'.")
            } else {
              DBI::dbExecute(con, "SET logged_in_user.username = 'public';")
              message("Username or password failed. You are now logged in as 'public'.")
            }
          } else {
            DBI::dbExecute(con, "SET logged_in_user.username = 'public';")
            message("You are now logged in as 'public'.")
          }
        } else {
          DBI::dbExecute(con, "SET logged_in_user.username = 'public';")
          message("You are now logged in as 'public'. If you need to change this either connect using an interactive session or use superuser credentials.")
        }
      }
    }
    
    if (!silent) {
      message("Connected to the aquacache database with the timezone set to UTC.")
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(con)
  }, error = function(e){
    stop("Connection failed.")
  })
}
