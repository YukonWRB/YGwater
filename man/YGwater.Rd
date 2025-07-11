% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/YGwater.R
\name{YGwater}
\alias{YGwater}
\title{Launch the YGwater Shiny application}
\usage{
YGwater(
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port"),
  dbName = "aquacache",
  dbHost = Sys.getenv("aquacacheHost"),
  dbPort = Sys.getenv("aquacachePort"),
  dbUser = Sys.getenv("aquacacheUser"),
  dbPass = Sys.getenv("aquacachePass"),
  accessPath1 =
    "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB",
  accessPath2 = "//carver/infosys/EQWin",
  server = FALSE,
  public = TRUE
)
}
\arguments{
\item{host}{Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in \code{port} to be open on the host machine. Ignored if \code{server} is set to TRUE (Shiny Server takes care of that).}

\item{port}{Port number (numeric) on which to serve the app. Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network. Ignored if \code{server} is set to TRUE (Shiny Server takes care of that).}

\item{dbName}{Name of the aquacache database. Default is "aquacache".}

\item{dbHost}{Host address of the aquacache database. Default is pulled from the .Renviron file.}

\item{dbPort}{Port number of the aquacache database. Default is pulled from the .Renviron file.}

\item{dbUser}{Username for the aquacache database. Default is pulled from the .Renviron file.}

\item{dbPass}{Password for the aquacache database. Default is pulled from the .Renviron file.}

\item{accessPath1}{to the folder where EQWin databases are stored. Default is "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB". The function will search for all *.mdb files in this folder (but not its sub-folders) and list them as options.}

\item{accessPath2}{Path to the folder where EQWin databases are stored. Default is "//carver/infosys/EQWin". The function will search for all *.mdb files in this folder (but not its sub-folders) and list them as options, combined with the files in accessPath1.}

\item{server}{Set to TRUE to run on Shiny Server, otherwise FALSE to run locally.}

\item{public}{TRUE restricts or doesn't create some UI elements that are not intended for public use, such as a login button and some crude report generation modules. Default is TRUE.}
}
\value{
Opens a Shiny application.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Launches a Shiny application to visualize the WRB's water-related data in maps and plots, and to download data from the WRB's hydrometric database. The application is designed to work with the WRB's aquacache database, which contains hydrometric data, and optionally with EQWin databases for water quality data.
}
