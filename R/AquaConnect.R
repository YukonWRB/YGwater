#' Connect to the AquaCache database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the AquaCache database. You can pass your own database connection parameters, for example to connect to a dev instance instead of the default production instance.
#' Database superusers will have access to all database records, but other users will be asked to provide their username and password to access records other than the 'public' ones. Note that this is *not necessarily* the same username and password as the one used to log into the database itself.
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form AquaCacheHost="hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter=value pair of form AquaCachePort="1234".
#' @param username Username. By default searches the .Renviron file for parameter=value pair of form AquaCacheUser="username". Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password. By default searches the .Renviron file for parameter=value pair of form AquaCachePass="password".
#' @param RLS_user Username for row-level security. Default searches the .Renviron file for parameter=value pair of form RLS_user="username" and if not found either prompts the user for input or logs in as 'public' if not interactive.
#' @param RLS_pass Password for row-level security. Default searches the .Renviron file for parameter=value pair of form RLS_pass="password" and if not found either prompts the user for input or logs in as 'public' if not interactive.
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

AquaConnect <- function(name = "AquaCache", host = Sys.getenv("AquaCacheHost"), port = Sys.getenv("AquaCachePort"), username = Sys.getenv("AquaCacheUser"), password = Sys.getenv("AquaCachePass"), RLS_user = Sys.getenv("RLS_user"), RLS_pass = Sys.getenv("RLS_pass"), silent = FALSE){

  tryCatch({
    hydro <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)
    
    if (!DBI::dbGetQuery(hydro, "SELECT rolsuper FROM pg_roles WHERE rolname = current_user;")[1,1]) { # If the user is not a superuser, check for row-level security credentials
      if (nchar(RLS_user) > 0 && nchar(RLS_pass) > 0) { # If the credentials exist, try them
        res <- validateACUser(RLS_user, RLS_pass, hydro)
        if (res) {
          DBI::dbExecute(hydro, paste0("SET logged_in_user.username = '", RLS_user, "';"))
        } else { # If the credentials fail, log in as public and warn the user
          DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
          message("Row-level security username or password failed when pulling from you .Renviron file. You are now logged in as 'public'.")
        }
      } else { # no credentials in the .renviron file
        if (interactive()) {
          # Prompt the user to enter their username and password
          message("You are not connecting to the database as a supersuer. Please enter your username and password (no quotes) to view records other than the 'public' ones, or enter nothing to log in as public.")
          username <- readline("Username: ")
          if (nchar(username) > 0) {
            password <- readline("Password: ")
            res <- validateACUser(username, password, hydro)
            if (res) {
              DBI::dbExecute(hydro, paste0("SET logged_in_user.username = '", username, "';"))
              message("You are now logged in as '", username, "'.")
            } else {
              DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
              message("Username or password failed. You are now logged in as 'public'.")
            }
          } else {
            DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
            message("You are now logged in as 'public'.")
          }
        } else {
          DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
          message("You are now logged in as 'public'. If you need to change this either connect using an interactive session or use superuser credentials.")
        }
      }
    }
    
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(hydro)
  }, error = function(e){
    stop("Connection failed.")
  })
}
