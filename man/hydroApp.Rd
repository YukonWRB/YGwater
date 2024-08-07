% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydroApp.R
\name{hydroApp}
\alias{hydroApp}
\title{Original Shiny application (hydroApp) for WRB data viewing}
\usage{
hydroApp(
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port"),
  dev = FALSE
)
}
\arguments{
\item{host}{Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in \code{port} to be open on the host machine.}

\item{port}{Port number (numeric). Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network.}

\item{dev}{Set to TRUE to run with the dev database. Default is FALSE.}
}
\value{
Opens a Shiny application.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

The original WRB Shiny application. Not intended for public viewing.
}
