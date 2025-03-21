% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotDiscrete.R
\name{plotDiscrete}
\alias{plotDiscrete}
\title{Discrete (lab or field point data) plotting function}
\usage{
plotDiscrete(
  start,
  end = Sys.Date() + 1,
  locations = NULL,
  locGrp = NULL,
  sub_locations = NULL,
  parameters = NULL,
  paramGrp = NULL,
  standard = NULL,
  log = FALSE,
  facet_on = "params",
  loc_code = "name",
  shareX = TRUE,
  shareY = FALSE,
  rows = "auto",
  target_datetime = TRUE,
  colorblind = FALSE,
  lang = "en",
  point_scale = 1,
  guideline_scale = 1,
  axis_scale = 1,
  legend_scale = 1,
  legend_position = "v",
  gridx = FALSE,
  gridy = FALSE,
  dbSource = "EQ",
  dbCon = NULL,
  dbPath =
    "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb"
)
}
\arguments{
\item{start}{The date to fetch data from, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'. Uses the actual sample datetime, not the target datetime.}

\item{end}{The end date to fetch data up to, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'. Uses the actual sample datetime, not the target datetime. Default is the current date.}

\item{locations}{A vector of station names or codes. If dbSource == 'AC': from aquacache 'locations' table use column 'location', 'name', or 'name_fr' (character vector) or 'location_id' (numeric vector). If dbSource == 'EQ' use EQWiN 'eqstns' table, column 'StnCode' or leave NULL to use \code{locGrp} instead.}

\item{locGrp}{Only used if \code{dbSource} is 'EQ'. A station group as listed in the EWQin 'eqgroups' table, column 'groupname.' Leave NULL to use \code{locations} instead.}

\item{sub_locations}{A vector of sub-location names or codes, only used if dbSource == 'AC' and table 'sub_locations'. Default is NULL; if there are sub-locations applicable, these will all be fetched and displayed as distinct traces. Must match the length of 'locations', use NA for locations without sub-locations.}

\item{parameters}{A vector of parameter names or codes. If dbSource == 'AC': from aquacache 'parameters' table use column 'param_name' or 'param_name_fr' (character vector) or 'parameter_id' (numeric vector). If dbSource == 'EQ' use EQWin 'eqparams' table, column 'ParamCode' or leave NULL to use \code{paramGrp} instead.}

\item{paramGrp}{Only used if \code{dbSource} is 'EQ'. A parameter group as listed in the EQWin 'eqgroups' table, column 'groupname.' Leave NULL to use \code{parameters} instead.}

\item{standard}{A standard or guideline name as listed in the EQWin eqstds table, column StdCode. Leave NULL to exclude standards. Only valid if \code{dbSource} is 'EQ'.}

\item{log}{Should the y-axis be log-transformed?}

\item{facet_on}{Should the plot be faceted by locations or by parameters? Specify one of 'locs' or 'params'. Default is 'locs'.}

\item{loc_code}{Should the location code be used instead of the full location name? Options are 'code', 'name', 'codeName', 'nameCode'. Default is 'name'.}

\item{shareX}{Should the x-axis be shared across facets? Default is TRUE (dates are shared).}

\item{shareY}{Should the y-axis be shared across facets? Default is FALSE (values are not shared).}

\item{rows}{The number of rows to use in the facet grid. Default is 'auto' to automatically determine the number of rows based on the number of facets.}

\item{target_datetime}{Should the plot datetime use the 'target' datetime instead of the 'actual' datetime? Default is TRUE. This is only applicable is dbSource == 'AC'.}

\item{colorblind}{Should the plot be colorblind-friendly? Default is FALSE.}

\item{lang}{The language to use for the plot. Currently only "en" and "fr" are supported. Default is "en", and this is only supported for dbSource == 'AC'.}

\item{point_scale}{A scale factor to apply to the size of points. Default is 1.}

\item{guideline_scale}{A scale factor to apply to the size of standard/guideline values Default is 1.}

\item{axis_scale}{A scale factor to apply to the size of axis labels. Default is 1.}

\item{legend_scale}{A scale factor to apply to the size of text in the legend. Default is 1.}

\item{legend_position}{The position of the legend, 'v' for vertical on the right side or 'h' for horizontal on the bottom. Default is 'v'. If 'h', slider will be set to FALSE due to interference.}

\item{gridx}{Should gridlines be drawn on the x-axis? Default is FALSE}

\item{gridy}{Should gridlines be drawn on the y-axis? Default is FALSE}

\item{dbSource}{The database source to use, 'AC' for aquacache or 'EQ' for EQWin. Default is 'EQ'. Connections to aquacache are made using function \code{\link[=AquaConnect]{AquaConnect()}} while EQWin connections use \code{\link[=AccessConnect]{AccessConnect()}}.}

\item{dbCon}{A database connection object, optional. Leave NULL to create a new connection and have it closed automatically.}

\item{dbPath}{The path to the EQWin database, if called for in parameter \code{dbSource}. Default is "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/
databases/EQWinDB/WaterResources.mdb".}
}
\value{
An interactive HTML plot of the data from EQWin.
}
\description{
Plots data directly from EQWin or from the aquacache for one or more location (station) and one or more parameter. Depending on the setting for argument 'facet_on', the function can either make a facet plot where each station is a facet (with parameters as distinct traces) or where each parameter is a facet (with locations as distinct traces). Values above or below the detection limit are shown as the detection limit but symbolized differently (open circles).
}
