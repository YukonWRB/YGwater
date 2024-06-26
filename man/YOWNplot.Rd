% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/YOWNplot.R
\name{YOWNplot}
\alias{YOWNplot}
\title{YOWNplot}
\usage{
YOWNplot(
  AQID,
  timeSeriesID = "Wlevel_bgs.Calculated",
  chartXinterval = "auto",
  dateRange = "all",
  stats = FALSE,
  smooth = FALSE,
  saveTo = "desktop",
  format = "png",
  login = Sys.getenv(c("AQUSER", "AQPASS")),
  server = "https://yukon.aquaticinformatics.net/AQUARIUS"
)
}
\arguments{
\item{AQID}{YOWN location for which a plot will be generated.}

\item{timeSeriesID}{Aquarius time series ID exactly as in Aquarius (ie. "Wlevel_bgs.Calculated", "Wlevel_masl.Calculated"). Defaults to m bgs.}

\item{chartXinterval}{X axis interval, can be specified "auto" for best fit calculation, or as desired (ie. "1 day", "1 month", "1 year", etc.). Defaults to "auto"}

\item{dateRange}{X axis limits, can be "all" for all data, "1yr" for most recent year of data, or vector of 2 in format c("2020/01/01", "2023/01/01"). Defaults to "all". Does not apply to stats = "line", which always plots the most current year of data.}

\item{stats}{Can be "line", "ribbon", or FALSE. Line shows years plotted in separate lines, ribbon shows max/min ribbon geom, and FALSE excludes stats. If set to "line", dateRange ignored as most current year of data will be plotted alongside historical data.}

\item{smooth}{Can be FALSE or a numeric day value (ie. 14) for plotting rolling average.}

\item{saveTo}{Optional directory in which the plot will be saved. Can specify "desktop" to automatically create YOWN ID folder on your desktop as save directory. Default NULL only outputs the plot to the console.}

\item{format}{If \code{saveTo} is not NULL, this parameter specifies the format of the saved plot. Default is "png", other option is "png".}

\item{login}{Your Aquarius login credentials as a character vector of two (eg. c("cmfische", "password") Default pulls information from your .renviron profile; see details. Passed to \code{\link[=aq_download]{aq_download()}}.}

\item{server}{The URL for your organization's Aquarius web server. Default is for the Yukon Water Resources Branch. Passed to \code{\link[=aq_download]{aq_download()}}.}
}
\value{
Writes a .pdf containing YOWN data in the specified directory.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Generalized YOWN plotting function
}
\details{
To store login credentials in your .renviron profile, call \code{\link[usethis:edit]{usethis::edit_r_environ()}} and enter your username and password as value pairs, as AQUSER="your username" and AQPASS = "your password".
}
