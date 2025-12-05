#' Plot a continous timeseries from the aquacache
#'
#' @description
#' This function plots continuous timeseries from the aquacache. The plot is zoomable and hovering over the historical ranges or the measured values brings up additional information. If corrections are applied to the data within AquaCache, the corrected values will be used.
#'
#' @param timeseries_id The timeseries ID to plot, if known (else leave NULL).
#' @param location The location for which you want a plot.
#' @param sub_location Your desired sub-location, if applicable. Default is NULL as most locations do not have sub-locations. Specify as the exact name of the sub-location (character) or the sub-location ID (numeric).
#' @param parameter The parameter name (text) or code (numeric) that you wish to plot. The location:sublocation:parameter combo must be in the local database.
#' @param record_rate The recording rate for the parameter and location to plot, from column 'record_rate' of table 'timeseries'. In most cases there are not multiple recording rates for a location and parameter combo and you can leave this NULL. Otherwise NULL will default to the most frequent record rate. Can be passed in a character string or number of seconds coercible to a period by [lubridate::period()].
#' @param aggregation_type The period type for the parameter and location to plot. Options other than the default NULL are 'sum', 'min', 'max', or '(min+max)/2', which is how the daily 'mean' temperature is often calculated for meteorological purposes. NULL will search for what's available and get the first timeseries found in this order: 'instantaneous', followed by the 'mean', '(min+max)/2', 'min', and 'max'.
#' @param z Depth/height in meters further identifying the timeseries of interest. Default is NULL, and where multiple elevations exist for the same location/parameter/record_rate/aggregation_type combo the function will default to the absolute elevation value closest to ground. Otherwise set to a numeric value.
#' @param z_approx Number of meters by which to approximate the elevation. Default is NULL, which will use the exact elevation. Otherwise set to a numeric value.
#' @param start_date The day or datetime on which to start the plot as character, Date, or POSIXct. Default is one year ago.
#' @param end_date The day or datetime on which to end the plot as character, Date, or POSIXct. Default is today.
#' @param invert Should the y-axis be inverted? TRUE/FALSE, or leave as default NULL to use the database default value.
#' @param slider Should a slider be included to show where you are zoomed in to? If TRUE the slider will be included but this prevents horizontal zooming or zooming in using the box tool. If legend_position is set to 'h', slider will be set to FALSE due to interference. Default is TRUE.
#' @param datum Should a vertical offset be applied to the data? Looks for it in the database and applies it if it exists. Default is TRUE.
#' @param title Should a title be included?
#' @param custom_title Custom title to be given to the plot. Default is NULL, which will set the title as the location name as entered in the database.
#' @param filter Should an attempt be made to filter out spurious data? Will calculate the rolling IQR and filter out clearly spurious values. Set this parameter to an integer, which specifies the rolling IQR 'window'. The greater the window, the more effective the filter but at the risk of filtering out real data. Negative values are always filtered from parameters "water level" ("niveau d'eau"), "flow" ("débit"), "snow depth" ("profondeur de la neige"), "snow water equivalent" ("équivalent en eau de la neige"), "distance", and any "precip" related parameter. Otherwise all values below -100 are removed.
#' @param unusable Should unusable data be displayed? Default is FALSE. Note that unusable data is not used in the calculation of historic ranges.
#' @param grades Should grades be included on the y-axis? Default is FALSE.
#' @param approvals Should approvals be included on the y-axis? Default is FALSE.
#' @param qualifiers Should qualifiers be included on the y-axis? Default is FALSE.
#' @param historic_range Should the historic range be plotted? Default is TRUE.
#' @param lang The language to use for the plot. Currently only "en" and "fr" are supported. Default is "en".
#' @param line_scale A scale factor to apply to the size (width) of the line. Default is 1.
#' @param axis_scale A scale factor to apply to the size of axis labels. Default is 1.
#' @param legend_scale A scale factor to apply to the size of text in the legend. Default is 1.
#' @param legend_position The position of the legend, 'v' for vertical on the right side or 'h' for horizontal on the bottom. Default is 'v'. If 'h', slider will be set to FALSE due to interference.
#' @param hover Should hover text be included? Default is TRUE.
#' @param gridx Should gridlines be drawn on the x-axis? Default is FALSE
#' @param gridy Should gridlines be drawn on the y-axis? Default is FALSE
#' @param webgl Use WebGL ("scattergl") for faster rendering when possible. Set to FALSE to force standard scatter traces.
#' @param resolution The resolution at which to plot the data. Default is NULL, which will adjust for reasonable plot performance depending on the date range. Otherwise set to one of "max", "hour", "day".
#' @param tzone The timezone to use for the plot. Default is "auto", which will use the system default timezone. Otherwise set to a valid timezone string.
#' @param raw Should raw data be used instead of corrected data (if corrections exist)? Default is FALSE.
#' @param imputed Should imputed data be displayed differently? Default is FALSE.
#' @param data Should the data used to create the plot be returned? Default is FALSE.
#' @param con A connection to the target database. NULL uses [AquaConnect()] and automatically disconnects.
#'
#' @return A plotly object and a data frame with the data used to create the plot (if `data` is TRUE).
#'
#' @export

plotTimeseries <- function(
  timeseries_id = NULL,
  location = NULL,
  sub_location = NULL,
  parameter = NULL,
  record_rate = NULL,
  aggregation_type = NULL,
  z = NULL,
  z_approx = NULL,
  start_date = Sys.Date() - 365,
  end_date = Sys.Date(),
  invert = NULL,
  slider = TRUE,
  datum = TRUE,
  title = TRUE,
  custom_title = NULL,
  filter = NULL,
  unusable = FALSE,
  grades = FALSE,
  approvals = FALSE,
  qualifiers = FALSE,
  historic_range = TRUE,
  lang = "en",
  line_scale = 1,
  axis_scale = 1,
  legend_scale = 1,
  legend_position = "v",
  hover = TRUE,
  gridx = FALSE,
  gridy = FALSE,
  webgl = TRUE,
  resolution = NULL,
  tzone = "auto",
  raw = FALSE,
  imputed = FALSE,
  data = FALSE,
  con = NULL
) {
  # location <- "29AB009"
  # sub_location <- NULL
  # parameter = 1165
  # start_date <- "2024-08-12"
  # end_date <- "2025-08-13"
  # record_rate = NULL
  # aggregation_type = NULL
  # z = NULL
  # z_approx = NULL
  # invert = NULL
  # slider = TRUE
  # datum = FALSE
  # title = TRUE
  # unusable = FALSE
  # grades = TRUE
  # approvals = FALSE
  # qualifiers = FALSE
  # custom_title = NULL
  # filter = NULL
  # historic_range = TRUE
  # lang = "en"
  # line_scale = 1
  # axis_scale = 1
  # legend_scale = 1
  # legend_position = "v"
  # resolution = "max"
  # tzone = "auto"
  # raw = FALSE
  # imputed = FALSE
  # con = NULL
  # gridx = FALSE
  # gridy = FALSE
  # hover = TRUE
  # webgl = FALSE

  # Checks and initial work ##########################################

  # Deal with non-standard evaluations from data.table to silence check() notes
  period_secs <- period <- expected <- datetime <- gap_exists <- start_dt <- end_dt <- NULL

  if (!is.null(resolution)) {
    if (resolution == "day" && raw) {
      warning(
        "You have requested raw data at a daily rate. Daily data is only available as corrected data, so raw was reset to FALSE."
      )
      raw <- FALSE
    }
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  if (tzone == "auto") {
    tzone <- Sys.timezone()
  }

  if (!is.null(parameter)) {
    if (inherits(parameter, "character")) {
      parameter <- tolower(parameter)
    }
  } else if (is.null(timeseries_id)) {
    stop("Parameter is required when timeseries_id is NULL.")
  }

  if (!is.null(invert)) {
    if (!inherits(invert, "logical")) {
      stop(
        "Your entry for the parameter 'invert' is invalid. Leave it as NULL or TRUE/FALSE."
      )
    }
  }

  if (!is.null(resolution)) {
    resolution <- tolower(resolution)
    if (!(resolution %in% c("max", "hour", "day"))) {
      stop(
        "Your entry for the parameter 'resolution' is invalid. Please review the function documentation and try again."
      )
    }
  }

  if (!is.null(z)) {
    if (!is.numeric(z)) {
      stop(
        "Your entry for the parameter 'z' is invalid. Please review the function documentation and try again."
      )
    }
    if (!is.null(z_approx)) {
      if (!is.numeric(z_approx)) {
        stop(
          "Your entry for the parameter 'z_approx' is invalid. Please review the function documentation and try again."
        )
      }
    }
  }

  if (inherits(start_date, "character")) {
    start_date <- as.Date(start_date)
  }
  if (inherits(start_date, "Date")) {
    start_date <- as.POSIXct(start_date, tz = tzone)
    start_date <- start_date + 24 * 60 * 60
  }
  if (inherits(end_date, "character")) {
    end_date <- as.Date(end_date)
  }
  if (inherits(end_date, "Date")) {
    end_date <- as.POSIXct(end_date, tz = tzone)
    end_date <- end_date + 24 * 60 * 60
  }

  #back to UTC because DB queries are in UTC
  attr(start_date, "tzone") <- "UTC"
  attr(end_date, "tzone") <- "UTC"

  if (!(lang %in% c("en", "fr"))) {
    stop(
      "Your entry for the parameter 'lang' is invalid. Please review the function documentation and try again."
    )
  }

  if (!inherits(historic_range, "logical")) {
    warning(
      "Parameter `historic_range` must be TRUE or FALSE. Resetting it to FALSE."
    )
    historic_range <- FALSE
  }

  # Determine the timeseries and adjust the date range #################
  if (is.null(timeseries_id)) {
    if (!is.null(record_rate)) {
      record_rate <- lubridate::period(record_rate)
      if (!lubridate::is.period(record_rate)) {
        warning(
          "Your entry for parameter record_rate is invalid. It's been reset to the default NULL."
        )
        record_rate <- NULL
      }
    }

    if (!is.null(aggregation_type)) {
      if (inherits(aggregation_type, "character")) {
        aggregation_type <- tolower(aggregation_type)
        if (
          !(aggregation_type %in%
            c('instantaneous', 'sum', 'min', 'max', '(min+max)/2'))
        ) {
          warning(
            "Your entry for parameter aggregation_type is invalid. It's been reset to the default NULL."
          )
          aggregation_type <- NULL
        } else {
          aggregation_type <- DBI::dbGetQuery(
            con,
            glue::glue_sql(
              "SELECT aggregation_type_id FROM aggregation_types WHERE aggregation_types.aggregation_type = {aggregation_type};",
              .con = con
            )
          )[1, 1]
        }
      } else {
        if (inherits(aggregation_type, "numeric")) {
          aggregation_type <- DBI::dbGetQuery(
            con,
            glue::glue_sql(
              "SELECT aggregation_type_id FROM aggregation_types WHERE aggregation_type_id = {aggregation_type};",
              .con = con
            )
          )[1, 1]
          if (is.na(aggregation_type)) {
            warning(
              "Your entry for parameter aggregation_type is invalid. It's been reset to the default NULL."
            )
            aggregation_type <- NULL
          }
        }
      }
    }

    aggregation_type_id <- aggregation_type

    location_txt <- as.character(location)
    location_id <- DBI::dbGetQuery(
      con,
      glue::glue_sql(
        "SELECT location_id FROM locations WHERE location = {location_txt} OR name = {location_txt} OR name_fr = {location_txt} OR location_id::text = {location_txt} LIMIT 1;",
        .con = con
      )
    )[1, 1]
    if (is.na(location_id)) {
      stop("The location you entered does not exist in the database.")
    }

    # Confirm parameter and location exist in the database and that there is only one entry
    parameter_txt <- tolower(as.character(parameter))
    parameter_tbl <- DBI::dbGetQuery(
      con,
      glue::glue_sql(
        "SELECT parameter_id, param_name, param_name_fr, plot_default_y_orientation, unit_default FROM parameters WHERE LOWER(param_name) = {parameter_txt} OR LOWER(param_name_fr) = {parameter_txt} OR parameter_id::text = {parameter_txt} LIMIT 1;",
        .con = con
      )
    )
    parameter_code <- parameter_tbl$parameter_id[1]
    if (is.na(parameter_code)) {
      stop("The parameter you entered does not exist in the database.")
    }

    if (is.null(sub_location)) {
      # Check if there are multiple sub_locations for this parameter_code, location regardless of sub_location. If so, throw a stop
      sub_loc_check <- DBI::dbGetQuery(
        con,
        "SELECT sub_location_id FROM timeseries WHERE location_id = $1 AND parameter_id = $2 AND sub_location_id IS NOT NULL;",
        params = list(location_id, parameter_code)
      )
      if (nrow(sub_loc_check) > 1) {
        stop(
          "There are multiple sub-locations for this location and parameter. Please specify a sub-location."
        )
      }

      if (is.null(record_rate)) {
        # aggregation_type_id may or may not be NULL
        if (is.null(aggregation_type_id)) {
          #both record_rate and aggregation_type_id are NULL
          exist_check <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT ts.timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, ts.aggregation_type_id, lz.z_meters AS z, ts.start_datetime, ts.end_datetime FROM timeseries ts LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id WHERE ts.location_id = ",
              location_id,
              " AND ts.parameter_id = ",
              parameter_code,
              ";"
            )
          )
        } else {
          #aggregation_type_id is not NULL but record_rate is
          exist_check <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT ts.timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, ts.aggregation_type_id, lz.z_meters AS z, ts.start_datetime, ts.end_datetime FROM timeseries ts LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id WHERE ts.location_id = ",
              location_id,
              " AND ts.parameter_id = ",
              parameter_code,
              " AND ts.aggregation_type_id = ",
              aggregation_type_id,
              ";"
            )
          )
        }
      } else if (is.null(aggregation_type_id)) {
        #record_rate is not NULL but aggregation_type_id is
        exist_check <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT ts.timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, ts.aggregation_type_id, lz.z_meters AS z, ts.start_datetime, ts.end_datetime 
          FROM timeseries ts 
          LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id WHERE ts.location_id = ",
            location_id,
            " AND ts.parameter_id = ",
            parameter_code,
            " AND ts.record_rate = '",
            record_rate,
            "';"
          )
        )
      } else {
        # both record_rate and aggregation_type_id are not NULL
        exist_check <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT ts.timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, ts.aggregation_type_id, lz.z_meters AS z, ts.start_datetime, ts.end_datetime 
          FROM timeseries ts 
          LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id WHERE ts.location_id = ",
            location_id,
            " AND ts.parameter_id = ",
            parameter_code,
            " AND ts.record_rate = '",
            record_rate,
            "' AND ts.aggregation_type_id = ",
            aggregation_type_id,
            ";"
          )
        )
      }
    } else {
      #sub location was specified
      # Find the sub location_id
      sub_loc_txt <- as.character(sub_location)
      sub_location_id <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT sub_location_id FROM sub_locations WHERE sub_location_name = {sub_loc_txt} OR sub_location_name_fr = {sub_loc_txt} OR sub_location_id::text = {sub_loc_txt} LIMIT 1;",
          .con = con
        )
      )[1, 1]
      if (is.na(sub_location_id)) {
        stop("The sub-location you entered does not exist in the database.")
      }
      if (is.null(record_rate)) {
        # aggregation_type_id may or may not be NULL
        if (is.null(aggregation_type_id)) {
          #both record_rate and aggregation_type_id are NULL
          exist_check <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT ts.timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, ts.aggregation_type_id, lz.z_meters AS z, ts.start_datetime, ts.end_datetime FROM timeseries ts LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id WHERE ts.location_id = ",
              location_id,
              " AND ts.sub_location_id = ",
              sub_location_id,
              " AND ts.parameter_id = ",
              parameter_code,
              ";"
            )
          )
        } else {
          #aggregation_type_id is not NULL but record_rate is
          exist_check <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT ts.timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, ts.aggregation_type_id, lz.z_meters AS z, ts.start_datetime, ts.end_datetime FROM timeseries ts LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id WHERE ts.location_id = ",
              location_id,
              " AND ts.sub_location_id = ",
              sub_location_id,
              " AND ts.parameter_id = ",
              parameter_code,
              " AND ts.aggregation_type_id = ",
              aggregation_type_id,
              ";"
            )
          )
        }
      } else if (is.null(aggregation_type_id)) {
        #record_rate is not NULL but aggregation_type_id is
        exist_check <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT ts.timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, ts.aggregation_type_id, lz.z_meters AS z, ts.start_datetime, ts.end_datetime FROM timeseries ts LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id WHERE ts.location_id = ",
            location_id,
            " AND ts.sub_location_id = ",
            sub_location_id,
            " AND ts.parameter_id = ",
            parameter_code,
            " AND ts.record_rate = '",
            record_rate,
            "';"
          )
        )
      } else {
        #both record_rate and aggregation_type_id are not NULL
        exist_check <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT ts.timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, ts.aggregation_type_id, lz.z_meters AS z, ts.start_datetime, ts.end_datetime FROM timeseries ts LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id WHERE ts.location_id = ",
            location_id,
            " AND ts.sub_location_id = ",
            sub_location_id,
            " AND ts.parameter_id = ",
            parameter_code,
            " AND ts.record_rate = '",
            record_rate,
            "' AND ts.aggregation_type_id = ",
            aggregation_type_id,
            ";"
          )
        )
      }
    }

    # Narrow down by z if necessary
    if (!is.null(z)) {
      if (is.null(z_approx)) {
        exist_check <- exist_check[exist_check$z == z, ]
      } else {
        exist_check <- exist_check[abs(exist_check$z - z) < z_approx, ]
      }
    }

    if (nrow(exist_check) == 0) {
      if (is.null(record_rate) & is.null(aggregation_type_id) & is.null(z)) {
        stop(
          "There doesn't appear to be a match in the database for location ",
          location,
          ", parameter ",
          parameter,
          ", and continuous category data."
        )
      } else {
        stop(
          "There doesn't appear to be a match in the database for location ",
          location,
          ", parameter ",
          parameter,
          ", record rate ",
          if (is.null(record_rate)) "(not specified)" else record_rate,
          ", aggregation type ",
          if (is.null(aggregation_type_id)) {
            "(not specified)"
          } else {
            aggregation_type_id
          },
          ", z of ",
          if (is.null(z)) "(not specified)" else z,
          " and continuous category data. You could try leaving the record rate and/or aggregation_type to the default 'NULL', or explore different z or z_approx values."
        )
      }
    } else if (nrow(exist_check) > 1) {
      if (is.null(record_rate)) {
        warning(
          "There is more than one entry in the database for location ",
          location,
          ", parameter ",
          parameter,
          ", and continuous category data. Since you left the record_rate as NULL, selecting the one(s) with the most frequent recording rate."
        )
        exist_check <- exist_check[order(exist_check$record_rate), ]
        temp <- exist_check[1, ]
      }
      if (nrow(temp) > 1) {
        exist_check <- temp
        if (is.null(aggregation_type_id)) {
          warning(
            "There is more than one entry in the database for location ",
            location,
            ", parameter ",
            parameter,
            ", and continuous category data. Since you left the aggregation_type as NULL, selecting the one(s) with the most frequent aggregation type."
          )
          agg_types <- DBI::dbGetQuery(
            con,
            "SELECT aggregation_type_id, aggregation_type FROM aggregation_types;"
          )

          exist_check <- exist_check[
            exist_check$aggregation_type_id ==
              agg_types[
                agg_types$aggregation_type == "instantaneous",
                "aggregation_type_id"
              ],
          ]
          if (nrow(exist_check) == 0) {
            exist_check <- exist_check[
              exist_check$aggregation_type_id ==
                agg_types[
                  agg_types$aggregation_type == "mean",
                  "aggregation_type_id"
                ],
            ]
          }
          if (nrow(exist_check) == 0) {
            exist_check <- exist_check[
              exist_check$aggregation_type_id ==
                agg_types[
                  agg_types$aggregation_type == "(min+max)/2",
                  "aggregation_type_id"
                ],
            ]
          }
          if (nrow(exist_check) == 0) {
            exist_check <- exist_check[
              exist_check$aggregation_type_id ==
                agg_types[
                  agg_types$aggregation_type == "minimum",
                  "aggregation_type_id"
                ],
            ]
          }
          if (nrow(exist_check) == 0) {
            exist_check <- exist_check[
              exist_check$aggregation_type_id ==
                agg_types[
                  agg_types$aggregation_type == "maximum",
                  "aggregation_type_id"
                ],
            ]
          }
        }
      } else if (nrow(temp) == 1) {
        exist_check <- temp
      }
    }

    # If there are multiple z values after all that, select the one closest to ground
    if (nrow(exist_check) > 1) {
      exist_check <- exist_check[which.min(abs(exist_check$z)), ]
    }

    tsid <- exist_check$timeseries_id
  } else {
    # timeseries_id was provided
    tsid <- timeseries_id
    # need to fetch pieces to get location_id and parameter_id
    exist_check <- DBI::dbGetQuery(
      con,
      "SELECT ts.timeseries_id, ts.location_id, ts.parameter_id, ts.start_datetime, ts.end_datetime, lz.z_meters AS z FROM timeseries ts LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id WHERE ts.timeseries_id = $1;",
      params = list(tsid)
    )
    location_id <- exist_check$location_id[1]
    parameter_tbl <- DBI::dbGetQuery(
      con,
      "SELECT param_name, param_name_fr, plot_default_y_orientation, unit_default FROM parameters WHERE parameter_id = $1;",
      params = list(exist_check$parameter_id[1])
    )
  }

  # adjust start and end datetimes
  if (start_date < exist_check$start_datetime) {
    start_date <- exist_check$start_datetime
  }
  if (end_date > exist_check$end_datetime) {
    end_date <- exist_check$end_datetime
  }

  if (end_date < start_date) {
    stop(
      "It looks like data for this location begins before your requested start date."
    )
  }

  # Where column param_name_fr is not filled in, use the English name
  parameter_tbl$param_name_fr[is.na(
    parameter_tbl$param_name_fr
  )] <- parameter_tbl$param_name[is.na(parameter_tbl$param_name_fr)]

  if (lang == "fr") {
    parameter_name <- titleCase(parameter_tbl$param_name_fr[1], "fr")
  } else if (lang == "en" || is.na(parameter_name)) {
    parameter_name <- titleCase(parameter_tbl$param_name[1], "en")
  }

  # Find the ts units
  units <- parameter_tbl$unit_default[1]

  # Find the necessary datum (latest datum)
  if (datum) {
    if (units != "m") {
      warning(
        "The parameter you are plotting is not in meters. Datum will not be applied."
      )
      datum_m <- 0
      datum <- FALSE
    } else {
      datum_m <- DBI::dbGetQuery(
        con,
        "SELECT conversion_m FROM datum_conversions WHERE location_id = $1 AND current = TRUE;",
        params = list(location_id)
      )[1, 1]
      if (is.na(datum_m)) {
        warning(
          "No datum conversion found for this location. Datum will not be applied."
        )
        datum <- FALSE
        datum_m <- 0
      }
    }
  }

  range <- seq.POSIXt(start_date, end_date, by = "day")
  if (is.null(resolution)) {
    if (length(range) > 3000) {
      if (!raw) {
        resolution <- "day"
      } else {
        resolution <- "hour"
      }
    } else if (length(range) > 1000) {
      resolution <- "hour"
    } else {
      resolution <- "max"
    }
  }

  if (title) {
    if (is.null(custom_title)) {
      if (lang == "fr") {
        stn_name <- DBI::dbGetQuery(
          con,
          "SELECT name_fr FROM locations where location_id = $1;",
          params = list(location_id)
        )[1, 1]
      }
      if (lang == "en" || is.na(stn_name)) {
        stn_name <- DBI::dbGetQuery(
          con,
          "SELECT name FROM locations where location_id = $1;",
          params = list(location_id)
        )[1, 1]
      }
      stn_name <- titleCase(stn_name, lang)
    } else {
      stn_name <- custom_title
    }
  }

  ## Grades, approvals, qualifiers ############
  if (grades | !unusable) {
    # if unusable, the grades must be pulled so that we can filter them out
    grades_dt <- dbGetQueryDT(
      con,
      paste0(
        "SELECT g.start_dt, g.end_dt, gt.grade_type_description, gt.grade_type_description_fr, gt.color_code FROM grades g LEFT JOIN grade_types gt ON g.grade_type_id = gt.grade_type_id WHERE g.timeseries_id = ",
        tsid,
        " AND g.end_dt >= '",
        start_date,
        "' AND g.start_dt <= '",
        end_date,
        "' ORDER BY start_dt;"
      )
    )

    # Many rows could be adjacent repeats of grade_type_description with different start_dt and end_dt, in which case these rows should be amalgamated
    grades_dt[, "run" := data.table::rleid(grades_dt$grade_type_description)]

    # now collapse each run into one interval
    grades_dt <- grades_dt[,
      .(
        start_dt = data.table::first(.SD$start_dt),
        end_dt = data.table::last(.SD$end_dt),
        grade_type_description = data.table::first(.SD$grade_type_description),
        grade_type_description_fr = data.table::first(
          .SD$grade_type_description_fr
        ),
        color_code = data.table::first(.SD$color_code)
      ),
      by = "run"
    ]
    # drop the helper
    grades_dt[, "run" := NULL]
  }
  if (approvals) {
    approvals_dt <- dbGetQueryDT(
      con,
      paste0(
        "SELECT a.start_dt, a.end_dt, at.approval_type_description, at.approval_type_description_fr, at.color_code FROM approvals a LEFT JOIN approval_types at ON a.approval_type_id = at.approval_type_id WHERE a.timeseries_id = ",
        tsid,
        " AND a.end_dt >= '",
        start_date,
        "' AND a.start_dt <= '",
        end_date,
        "' ORDER BY start_dt;"
      )
    )
    # amalgamate
    approvals_dt[,
      "run" := data.table::rleid(approvals_dt$approval_type_description)
    ]
    approvals_dt <- approvals_dt[,
      .(
        start_dt = data.table::first(.SD$start_dt),
        end_dt = data.table::last(.SD$end_dt),
        approval_type_description = data.table::first(
          .SD$approval_type_description
        ),
        approval_type_description_fr = data.table::first(
          .SD$approval_type_description_fr
        ),
        color_code = data.table::first(.SD$color_code)
      ),
      by = "run"
    ]
    approvals_dt[, "run" := NULL]
  }
  if (qualifiers) {
    qualifiers_dt <- dbGetQueryDT(
      con,
      paste0(
        "SELECT q.start_dt, q.end_dt, qt.qualifier_type_description, qt.qualifier_type_description_fr, qt.color_code FROM qualifiers q LEFT JOIN qualifier_types qt ON q.qualifier_type_id = qt.qualifier_type_id WHERE q.timeseries_id = ",
        tsid,
        " AND q.end_dt >= '",
        start_date,
        "' AND q.start_dt <= '",
        end_date,
        "' ORDER BY start_dt;"
      )
    )
    # amalgamate
    qualifiers_dt[,
      "run" := data.table::rleid(qualifiers_dt$qualifier_type_description)
    ]
    qualifiers_dt <- qualifiers_dt[,
      .(
        start_dt = data.table::first(.SD$start_dt),
        end_dt = data.table::last(.SD$end_dt),
        qualifier_type_description = data.table::first(
          .SD$qualifier_type_description
        ),
        qualifier_type_description_fr = data.table::first(
          .SD$qualifier_type_description_fr
        ),
        color_code = data.table::first(.SD$color_code)
      ),
      by = "run"
    ]
    qualifiers_dt[, "run" := NULL]
  }

  ## fetch trace and range data ###################
  # Trace data first
  if (resolution == "day") {
    trace_data <- dbGetQueryDT(
      con,
      "SELECT date AS datetime, value, imputed FROM measurements_calculated_daily_corrected WHERE timeseries_id = $1 AND date BETWEEN $2 AND $3 ORDER BY date DESC;",
      params = list(tsid, start_date, end_date)
    )
    trace_data$datetime <- as.POSIXct(trace_data$datetime, tz = "UTC")
  } else if (resolution == "hour") {
    trace_data <- dbGetQueryDT(
      con,
      paste0(
        "SELECT datetime, value_corrected AS value, imputed",
        if (raw) ", value_raw",
        " FROM measurements_hourly_corrected WHERE timeseries_id = $1 AND datetime BETWEEN $2 AND $3 ORDER BY datetime DESC;"
      ),
      params = list(tsid, start_date, end_date)
    )
  } else if (resolution == "max") {
    trace_data <- dbGetQueryDT(
      con,
      paste0(
        "SELECT datetime, value_corrected AS value, imputed",
        if (raw) ", value_raw",
        " FROM measurements_continuous_corrected WHERE timeseries_id = $1 AND datetime BETWEEN $2 AND $3 ORDER BY datetime DESC LIMIT 200000;"
      ),
      params = list(tsid, start_date, end_date)
    )

    if (nrow(trace_data) > 0) {
      if (min(trace_data$datetime) > start_date) {
        infill <- dbGetQueryDT(
          con,
          paste0(
            "SELECT datetime, value_corrected AS value, imputed",
            if (raw) ", value_raw",
            " FROM measurements_hourly_corrected WHERE timeseries_id = $1 AND datetime BETWEEN $2 AND $3 ORDER BY datetime DESC;"
          ),
          params = list(
            tsid,
            start_date,
            min(trace_data$datetime) - 1
          )
        )
        trace_data <- rbind(infill, trace_data)
      }
    }
  }
  attr(trace_data$datetime, "tzone") <- tzone

  # Range data
  if (historic_range) {
    # get data from the measurements_calculated_daily_corrected table for historic ranges plus values from measurements_continuous_corrected view. Where there isn't any data in the table fill in with the value from the daily table.
    range_end <- end_date + 1 * 24 * 60 * 60
    range_start <- start_date - 1 * 24 * 60 * 60
    range_data <- dbGetQueryDT(
      con,
      "SELECT date AS datetime, min, max, q75, q25 FROM measurements_calculated_daily_corrected WHERE timeseries_id = $1 AND date BETWEEN $2 AND $3 ORDER BY date ASC;",
      params = list(tsid, range_start, range_end)
    )
    range_data$datetime <- as.POSIXct(range_data$datetime, tz = "UTC")
    attr(range_data$datetime, "tzone") <- tzone

    # Check if the range data extends to the end of trace data. Because range is taken from calculated daily means, it's possible that there's no data yet for the current day. In this case we'll just use the values from the last available range_data$datetime, incrementing it by a day
    if (max(range_data$datetime) < max(trace_data$datetime)) {
      last <- range_data[nrow(range_data), ]
      range_data <- rbind(
        range_data,
        data.table(
          datetime = last$datetime + 1 * 24 * 60 * 60,
          min = last$min,
          max = last$max,
          q25 = last$q25,
          q75 = last$q75
        )
      )
    }
  }

  if (!unusable) {
    # Trow out unusable data (replace with NAs)
    unus <- grades_dt[grades_dt$grade_type_description == "Unusable"]
    if (nrow(unus) > 0) {
      # Using a non-equi join to update trace_data: it finds all rows where datetime falls between start_dt and end_dt and updates value to NA in one go.
      trace_data[
        unus,
        on = .(datetime >= start_dt, datetime <= end_dt),
        "value" := NA
      ]
    }
  }

  # fill gaps with NA values
  # Since recording rate can change within a timeseries, use calculate_period and some data.table magic to fill in gaps
  min_trace <- suppressWarnings(min(trace_data$datetime, na.rm = TRUE))
  if (!is.infinite(min_trace)) {
    trace_data <- suppressWarnings(calculate_period(
      trace_data,
      timeseries_id = tsid,
      con = con
    ))
    # if calculate_period didn't return a column for trace_data, it couldn't be done. No need to continue
    if ("period" %in% colnames(trace_data)) {
      trace_data[, "period_secs" := as.numeric(lubridate::period(period))]
      # Shift datetime and add period_secs to compute the 'expected' next datetime
      trace_data[,
        "expected" := data.table::shift(datetime, type = "lead") - period_secs
      ]
      # Create 'gap_exists' column to identify where gaps are
      trace_data[, "gap_exists" := datetime < expected & !is.na(expected)]
      # Find indices where gaps exist
      gap_indices <- which(trace_data$gap_exists)
      # Create a data.table of NA rows to be inserted
      na_rows <- data.table::data.table(
        datetime = trace_data[gap_indices, "datetime"][[1]] + 1, # Add 1 second to place it at the start of the gap
        value = NA,
        imputed = FALSE
      )
      if (raw) {
        na_rows[, "value_raw" := NA]
        # Combine with NA rows
        trace_data <- data.table::rbindlist(
          list(
            trace_data[, c("datetime", "value", "value_raw", "imputed")],
            na_rows
          ),
          use.names = TRUE
        )
      } else {
        # Combine with NA rows
        trace_data <- data.table::rbindlist(
          list(trace_data[, c("datetime", "value", "imputed")], na_rows),
          use.names = TRUE
        )
      }
      # order by datetime
      data.table::setorder(trace_data, datetime)
    }

    # Find out where trace_data values need to be filled in with daily means (this usually only deals with HYDAT daily mean data)
    if (min_trace > start_date) {
      extra <- dbGetQueryDT(
        con,
        "SELECT date AS datetime, value, imputed FROM measurements_calculated_daily_corrected WHERE timeseries_id = $1 AND date < $2 AND date >= $3;",
        params = list(
          tsid,
          min(trace_data$datetime),
          start_date
        )
      )
      extra$datetime <- as.POSIXct(extra$datetime, tz = "UTC")
      attr(extra$datetime, "tzone") <- tzone
      if (raw) {
        extra[, "value_raw" := NA]
      }
      trace_data <- rbind(trace_data, extra)
    }
  } else {
    # this means that no trace data could be had because there are no measurements in the continuous or the hourly views table
    trace_data <- dbGetQueryDT(
      con,
      "SELECT date AS datetime, value, imputed FROM measurements_calculated_daily_corrected WHERE timeseries_id = $1 AND date BETWEEN $2 AND $3 ORDER BY date DESC;",
      params = list(tsid, start_date, end_date)
    )
    trace_data$datetime <- as.POSIXct(trace_data$datetime, tz = "UTC")
    attr(trace_data$datetime, "tzone") <- tzone
    if (raw) {
      trace_data[, "value_raw" := NA]
    }
  }

  if (nrow(trace_data) == 0) {
    stop("No data found for the specified location, parameter, and time range.")
  }

  if (datum) {
    trace_data$value <- trace_data$value + datum_m
    if (raw) {
      trace_data$value_raw <- trace_data$value_raw + datum_m
    }
    if (historic_range) {
      range_data$min <- range_data$min + datum_m
      range_data$max <- range_data$max + datum_m
      range_data$q25 <- range_data$q25 + datum_m
      range_data$q75 <- range_data$q75 + datum_m
    }
  }
  trace_data <- trace_data[order(trace_data$datetime), ]

  if (!is.null(filter)) {
    # Use the same approach as in ggplotOverlap to filter the value column
    if (!inherits(filter, "numeric")) {
      message(
        "Parameter 'filter' was modified from the default NULL but not properly specified as a class 'numeric'. Filtering will not be done."
      )
    } else {
      if (
        parameter_name %in%
          c(
            "water level",
            "niveau d'eau",
            "flow",
            "d\u00E9bit d'eau",
            "snow depth",
            "profondeur de la neige",
            "snow water equivalent",
            "\u00E9quivalent en eau de la neige",
            "distance"
          ) |
          grepl("precipitation", parameter_name, ignore.case = TRUE)
      ) {
        # remove all values less than 0
        trace_data[
          trace_data$value < 0 & !is.na(trace_data$value),
          "value"
        ] <- NA
      } else {
        # remove all values less than -100 (in case of negative temperatures or -DL values in lab results)
        trace_data[
          trace_data$value < -100 & !is.na(trace_data$value),
          "value"
        ] <- NA
      }

      rollmedian <- zoo::rollapply(
        trace_data$value,
        width = filter,
        FUN = "median",
        align = "center",
        fill = "extend",
        na.rm = TRUE
      )
      rollmad <- zoo::rollapply(
        trace_data$value,
        width = filter,
        FUN = "mad",
        align = "center",
        fill = "extend",
        na.rm = TRUE
      )
      outlier <- abs(trace_data$value - rollmedian) > 5 * rollmad
      trace_data$value[outlier] <- NA
    }
  }

  # Make the plot ###################################
  if (is.null(invert)) {
    if (parameter_tbl$plot_default_y_orientation[1] == "inverted") {
      invert <- TRUE
    } else {
      invert <- FALSE
    }
  }

  y_title <- paste0(
    parameter_name,
    if (!is.na(exist_check$z)) paste0(" ", exist_check$z, " meters") else "",
    " (",
    units,
    ")"
  )

  plot <- plotly::plot_ly()
  if (historic_range) {
    plot <- plot %>%
      plotly::add_ribbons(
        data = range_data[!is.na(range_data$q25) & !is.na(range_data$q75), ],
        x = ~datetime,
        ymin = ~q25,
        ymax = ~q75,
        # name = if (lang == "en") "IQR" else "EIQ",
        name = if (lang == "en") "Typical" else "Typique",
        color = I("#5f9da6"),
        line = list(width = 0.2),
        hoverinfo = if (hover) "text" else "none",
        text = ~ paste0(
          "Q25: ",
          round(q25, 2),
          ", Q75: ",
          round(q75, 2),
          " (",
          as.Date(datetime),
          ")"
        )
      ) %>%
      plotly::add_ribbons(
        data = range_data[!is.na(range_data$min) & !is.na(range_data$max), ],
        x = ~datetime,
        ymin = ~min,
        ymax = ~max,
        # name = "Min-Max",
        name = if (lang == "en") "Historic" else "Historique",
        color = I("#D4ECEF"),
        line = list(width = 0.2),
        hoverinfo = if (hover) "text" else "none",
        text = ~ paste0(
          "Min: ",
          round(.data$min, 2),
          ", Max: ",
          round(.data$max, 2),
          " (",
          as.Date(.data$datetime),
          ")"
        )
      )
  }

  corrected_name <- parameter_name
  if (raw) {
    corrected_name <- if (lang == "fr") {
      paste(parameter_name, "(corrig\u00E9)")
    } else {
      paste(parameter_name, "(corrected)")
    }
  }

  if (
    raw &&
      "value_raw" %in% names(trace_data) &&
      any(!is.na(trace_data$value_raw))
  ) {
    raw_label <- if (lang == "fr") {
      paste(parameter_name, "(brut)")
    } else {
      paste(parameter_name, "(raw)")
    }

    # Add the raw data first so it's below the corrected data
    plot <- plot %>%
      plotly::add_trace(
        data = trace_data,
        x = ~datetime,
        y = ~value_raw,
        type = if (webgl) "scattergl" else "scatter",
        mode = "lines",
        line = list(width = 2.5 * line_scale),
        name = raw_label,
        color = I("#d95f02"),
        hoverinfo = if (hover) "text" else "none",
        text = ~ paste0(
          raw_label,
          ": ",
          round(.data$value_raw, 4),
          " (",
          .data$datetime,
          ")"
        )
      )
  }

  # Add the corrected data trace
  if (imputed) {
    plot <- plot %>%
      plotly::add_trace(
        data = trace_data[trace_data$imputed == FALSE, ],
        x = ~datetime,
        y = ~value,
        type = if (webgl) "scattergl" else "scatter",
        mode = "lines",
        line = list(width = 2.5 * line_scale),
        name = corrected_name,
        color = I("#00454e"),
        hoverinfo = if (hover) "text" else "none",
        text = ~ paste0(
          parameter_name,
          ": ",
          round(.data$value, 4),
          " (",
          .data$datetime,
          ")"
        )
      )
    plot <- plot %>%
      plotly::add_trace(
        data = trace_data[trace_data$imputed == TRUE, ],
        x = ~datetime,
        y = ~value,
        type = if (webgl) "scattergl" else "scatter",
        mode = "markers",
        marker = list(
          symbol = "circle",
          size = 6 * line_scale,
          color = "#e600b4ff",
          line = list(width = 1 * line_scale, color = "#e600b4ff")
        ),
        name = if (lang == "en") {
          paste0(corrected_name, " (imputed)")
        } else {
          paste0(corrected_name, " (imput\u00E9)")
        },
        hoverinfo = if (hover) "text" else "none",
        text = ~ paste0(
          parameter_name,
          ": ",
          round(.data$value, 4),
          " (",
          .data$datetime,
          ")"
        )
      )
  } else {
    plot <- plot %>%
      plotly::add_trace(
        data = trace_data,
        x = ~datetime,
        y = ~value,
        type = if (webgl) "scattergl" else "scatter",
        mode = "lines",
        line = list(width = 2.5 * line_scale),
        name = corrected_name,
        color = I("#00454e"),
        hoverinfo = if (hover) "text" else "none",
        text = ~ paste0(
          parameter_name,
          ": ",
          round(.data$value, 4),
          " (",
          .data$datetime,
          ")"
        )
      )
  }

  # Add the grades, approvals, qualifiers as ribbons below the plotting area
  if (any(grades, approvals, qualifiers)) {
    slider <- FALSE
    bands_subplot <- plotly::plot_ly()

    mindt <- trace_data[, min(datetime)]
    maxdt <- trace_data[, max(datetime)]

    poly_list <- list()

    if (approvals) {
      approvals_y_set <- if (grades & qualifiers) {
        c(2.2, 3.2, 3.2, 2.2)
      } else if (grades) {
        c(1.1, 2.1, 2.1, 1.1)
      } else {
        c(0, 1, 1, 0)
      }
      approvals_dt[approvals_dt$start_dt < mindt, "start_dt" := mindt]
      approvals_dt[approvals_dt$end_dt > maxdt, "end_dt" := maxdt]
      if (nrow(approvals_dt) > 0) {
        approvals_dt[, "id" := paste0("approval_", .I)]
        poly_list[[length(poly_list) + 1]] <- approvals_dt[,
          .(
            datetime = c(
              .SD$start_dt[1L],
              .SD$start_dt[1L],
              .SD$end_dt[1L],
              .SD$end_dt[1L]
            ),
            y = approvals_y_set,
            color = .SD$color_code[1L],
            4L,
            text = if (lang == "en") {
              paste0("Approval: ", .SD$approval_type_description[1L])
            } else {
              paste0("Approbation:", .SD$approval_type_description_fr[1L])
            },
            id = "id"
          ),
          by = "id"
        ]
      }
    }

    if (grades) {
      grades_y_set <- if (qualifiers) c(1.1, 2.1, 2.1, 1.1) else c(0, 1, 1, 0)
      grades_dt[grades_dt$start_dt < mindt, "start_dt" := mindt]
      grades_dt[grades_dt$end_dt > maxdt, "end_dt" := maxdt]
      if (nrow(grades_dt) > 0) {
        grades_dt[, "id" := paste0("grade_", .I)]
        poly_list[[length(poly_list) + 1]] <- grades_dt[,
          .(
            datetime = c(
              .SD$start_dt[1L],
              .SD$start_dt[1L],
              .SD$end_dt[1L],
              .SD$end_dt[1L]
            ),
            y = grades_y_set,
            color = .SD$color_code[1L],
            4L,
            text = if (lang == "en") {
              paste0("Grade: ", .SD$grade_type_description[1L])
            } else {
              paste0("Cote:", .SD$grade_type_description_fr[1L])
            },
            id = "id"
          ),
          by = "id"
        ]
      }
    }

    if (qualifiers) {
      qualifiers_y_set <- c(0, 1, 1, 0)
      qualifiers_dt[qualifiers_dt$start_dt < mindt, "start_dt" := mindt]
      qualifiers_dt[qualifiers_dt$end_dt > maxdt, "end_dt" := maxdt]
      if (nrow(qualifiers_dt) > 0) {
        qualifiers_dt[, "id" := paste0("qualifier_", .I)]
        poly_list[[length(poly_list) + 1]] <- qualifiers_dt[,
          .(
            datetime = c(
              .SD$start_dt[1L],
              .SD$start_dt[1L],
              .SD$end_dt[1L],
              .SD$end_dt[1L]
            ),
            y = qualifiers_y_set,
            color = .SD$color_code[1L],
            4L,
            text = if (lang == "en") {
              paste0("Qualifier: ", .SD$qualifier_type_description[1L])
            } else {
              paste0("Qualificatif:", .SD$qualifier_type_description_fr[1L])
            },
            id = "id"
          ),
          by = "id"
        ]
      }
    }

    if (length(poly_list) > 0) {
      polygons_df <- data.table::rbindlist(poly_list, use.names = TRUE)
      bands_subplot <- bands_subplot %>%
        plotly::add_polygons(
          data = polygons_df,
          x = ~datetime,
          y = ~y,
          split = ~id,
          fill = "toself",
          fillcolor = ~color,
          line = list(width = 1, color = "black"),
          hoverinfo = "text",
          hoveron = "fills",
          text = ~text,
          showlegend = FALSE
        )
    }

    # Hide the y axis labels and replace with annotations
    annotation_list <- list()

    if (approvals) {
      annotation_list <- c(
        annotation_list,
        list(list(
          x = 0.0,
          y = (approvals_y_set[1] + approvals_y_set[2]) / 2,
          xref = "paper",
          yref = "y",
          text = if (lang == "en") "Approval" else "Approbation",
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "middle",
          textangle = 0,
          font = list(size = axis_scale * 10)
        ))
      )
    }
    if (grades) {
      annotation_list <- c(
        annotation_list,
        list(list(
          x = 0.0,
          y = (grades_y_set[1] + grades_y_set[2]) / 2,
          xref = "paper",
          yref = "y",
          text = if (lang == "en") "Grade" else "Cote",
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "middle",
          textangle = 0,
          font = list(size = axis_scale * 10)
        ))
      )
    }
    if (qualifiers) {
      annotation_list <- c(
        annotation_list,
        list(list(
          x = 0.0,
          y = (qualifiers_y_set[1] + qualifiers_y_set[2]) / 2,
          xref = "paper",
          yref = "y",
          text = if (lang == "en") "Qualifier" else "Qualificatif",
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "middle",
          textangle = 0,
          font = list(size = axis_scale * 10)
        ))
      )
    }

    bands_subplot <- bands_subplot %>%
      plotly::layout(
        yaxis = list(
          showticklabels = FALSE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        xaxis = list(showgrid = FALSE),
        annotations = annotation_list,
        font = list(family = "Nunito Sans")
      )

    plot <- plotly::subplot(
      plot,
      bands_subplot,
      nrows = 2,
      shareX = TRUE,
      margin = 0.0, # slight vertical gap *between* the main plot & the bands
      heights = if ((grades + qualifiers + approvals) == 3) {
        c(0.94, 0.06)
      } else if ((grades + qualifiers + approvals) == 2) {
        c(0.96, 0.04)
      } else {
        c(0.98, 0.02)
      }
    )
  }

  plot <- plot %>%
    plotly::layout(
      title = if (title) {
        list(
          text = stn_name,
          x = 0.05,
          xref = "container",
          font = list(
            size = axis_scale * 18,
            family = "Nunito Sans",
            color = "#000000"
          )
        )
      } else {
        NULL
      },
      xaxis = list(
        title = list(standoff = 0),
        showgrid = gridx,
        showline = TRUE,
        tickformat = if (lang == "en") "%b %-d '%y" else "%d %-b '%y",
        titlefont = list(size = axis_scale * 14),
        tickfont = list(size = axis_scale * 12),
        nticks = 10,
        rangeslider = list(
          visible = if (slider & legend_position == "v") TRUE else FALSE
        ),
        ticks = "outside",
        ticklen = 5,
        tickwidth = 1,
        tickcolor = "black"
      ),
      # Main plot yaxis layout
      yaxis = list(
        title = list(text = y_title, standoff = 10),
        showgrid = gridy,
        showline = TRUE,
        zeroline = FALSE,
        titlefont = list(size = axis_scale * 14),
        tickfont = list(size = axis_scale * 12),
        autorange = if (invert) "reversed" else TRUE,
        ticks = "outside",
        ticklen = 5,
        tickwidth = 1,
        tickcolor = "black"
      ),
      margin = list(b = 0, t = 40 * axis_scale, l = 50 * axis_scale),
      hovermode = if (hover) "x unified" else ("none"),
      legend = list(
        font = list(size = legend_scale * 12),
        orientation = legend_position
      ),
      font = list(family = "Nunito Sans")
    ) %>%
    plotly::config(locale = lang)

  # Return the plot and data if requested ##########################
  if (data) {
    datalist <- list(
      trace_data = trace_data,
      range_data = if (historic_range) range_data else data.frame()
    )
    return(list(plot = plot, data = datalist))
  } else {
    return(plot)
  }
} # end of function
