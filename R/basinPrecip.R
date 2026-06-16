#' Accumulated precipitation
#'
#' @description
#' Calculates accumulated precipitation above defined basins or immediately surrounding a specified point. If possible, make sure to specify a location in parameters hrdpa_loc and hrdps_loc to speed things up! Uses functions [getHRDPA()] and [getHRDPS()] to bring in precipi rasters if any need to be downloaded.
#'
#' If specifying `location` as coordinates rather than a WSC station (or non WSC station, see note below), the numeric result will be of precipitation in the 2.5 km2 cell on which the point falls. Map output, if requested, will be for the smallest available drainage polygon containing that point.
#'
#' Raster images of precipitation are only kept by ECCC for 30 days. If specifying a `start` or `end` prior to that without having precipitation rasters available locally and specified with `hrdpa_loc`, the start date/time will be adjusted and a warning given.
#'
#' Rasters must be downloaded for each 6-hour period between `start` and `end`. It is therefore strongly suggested that you designate and use a folder for this purpose. `getHRDPA` is called for downloading and clipping rasters.
#'
#' Additional spatial data is added to the maps when possible, Keeping in mind that large vector files can be lengthy for R to graphically represent. To ease this limitation, the watercourses file is left out when the drainage extent is greater than 30 000 km2; by that size, major water courses are expected to be represented by polygons in the waterbodies layer anyways.
#'
#' @param location The location above which you wish to calculate precipitation. Specify either a WSC or WSC-like station ID (e.g. `"09AB004"`) for which there is a database entry, or coordinates in signed decimal degrees in form latitude, longitude (`"60.1234 -139.1234"`; note the space, negative sign, and lack of comma). See details for more info if specifying coordinates.
#' @param start The start of the time period over which to accumulate precipitation. Use format `"yyyy-mm-dd hh:mm"` (character vector) in UTC time, or a POSIXct object (e.g. `Sys.time()-60*60*24` for one day in the past). In the case of a POSIXct object, the timezone is converted to UTC without modifying the time. See details if requesting earlier than 30 days prior to now.
#' @param end The end of the time period over which to accumulate precipitation. Other details as per `start`
#' @param map Should a map be output to the console? See details for more info.
#' @param maptype Choose from 'dynamic' or 'static'. Leaflet maps are interactive and return the entire Yukon territory with the requested basin highlighted. Static maps are printed to the console and show only the requested basin. Only used if map = TRUE.
#' @param title Should a title be added to the map? Only used if map = TRUE.
#' @param raster_col The color palette to use for the raster plot. Default is terra::map.pal("elevation", 10), **not** the terra::plot default. In all cases a very light blue-grey is the lowest precip amount color. Refer to [terra::map.pal()] for other color options. Only used if map = TRUE.
#' @param silent If TRUE, no output is printed to the console.
#' @param con A connection to the aquacache database. NULL uses [AquaConnect()] and automatically disconnects.
#' @param hrdpa_loc The directory (folder) where past precipitation rasters are to be downloaded. Suggested use is to specify a repository where all such rasters are saved to speed processing time and reduce data usage. If using the default NULL, rasters will not persist beyond your current R session.
#' @param hrdps_loc The directory (folder) where forecast precipitation rasters are to be downloaded. A folder will be created for the specific parameter (in this case, precipitation) or selected if already existing.
#'
#' @return The accumulated precipitation in mm of water within the drainage specified or immediately surrounding the point specified in `location` printed to the console and (optionally) a map of precipitation printed to the console. A list is also generated containing statistics and the optional plot; to save this plot to disc use either grDevices::png() or grDevices::dev.print(), or use the graphical device export functionality.
#' @export

#TODO Update function to work directly with DB, with terra object, or with shapefiles, or with gpkg files.
#TODO: problem with extents not matching. reproduce by first calling a map for somewhere in YT, then in Ontario. will get Error:[sds] extents do not match, which means that the DL and/or file selection process didn't work properly
#IDEA: Allow multiple plots to be fetched, or a time-lapse of plots (better). Allow setting increments and number of plots.
#IDEA: use leaflet to display maps
#TODO: Get precip further in future using RDPS beyond HRDPS range.

basinPrecip <- function(
  location,
  start = Sys.time() - 60 * 60 * 24,
  end = Sys.time(),
  map = FALSE,
  maptype = "dynamic",
  title = TRUE,
  raster_col = terra::map.pal("elevation", 20),
  silent = FALSE,
  con = NULL,
  hrdpa_loc = NULL,
  hrdps_loc = NULL
) {
  # location <- "09EB001"
  # start = Sys.time() - 60*60*24
  # end = Sys.time()
  # map = TRUE
  # maptype = "dynamic"
  # title = TRUE
  # raster_col = terra::map.pal("elevation", 20)
  # silent = FALSE
  # con = NULL
  # hrdpa_loc = NULL
  # hrdps_loc = NULL

  # Check that terra is 1.7.81 at minimum.
  if (maptype == "dynamic") {
    if (utils::packageVersion("terra") < "1.7.8") {
      stop(
        "This function requires terra version 1.7.81 or higher for dynamic maps. You can switch to static maps, or update your version of terra. The CRAN version might not be up to date enough yet, in which case you need to use remotes::install_github('rspatial/terra') to get the latest version."
      )
    }
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  # #Set defaults
  # if (is.null(hrdpa_loc)) {
  #   hrdpa_loc = "//env-fs/env-data/corp/water/Common_GW_SW/Data/precip_HRDPA"
  # }
  # if (is.null(hrdps_loc)) {
  #   hrdps_loc <- "//env-fs/env-data/corp/water/Common_GW_SW/Data/HRDPS"
  # }

  #Basic checks on location
  if (!inherits(location, "character")) {
    stop("Parameter `location` should be specified as a character vector.")
  }
  maptype <- tolower(maptype)
  if (!maptype %in% c("dynamic", "static")) {
    stop("Parameter `maptype` should be either 'dynamic' or 'static'.")
  }

  #Basic checks on start and end
  if (!(inherits(start[1], "POSIXct"))) {
    if (inherits(start, "character")) {
      if (nchar(start) < 15) {
        stop(
          "Check your parameter for start: it should either be a POSIXct object or a character vector of form yyy-mm-dd HH:mm."
        )
      }
    }
  }
  if (!(inherits(end[1], "POSIXct"))) {
    if (inherits(end, "character")) {
      if (nchar(end) < 15) {
        stop(
          "Check your parameter for end: it should either be a POSIXct object or a character vector of form yyy-mm-dd HH:mm."
        )
      }
    }
  }

  location_id <- NA_integer_
  type <- NA_character_
  # Coerce location to either decimal degrees or a location in the database
  if (
    grepl("^[0-9]{2}[\\.]{1}[0-9]*[ ]{1}[-]{1}[0-9]*[\\.]{1}[0-9]*", location)
  ) {
    tryCatch(
      {
        location <- as.numeric(unlist(strsplit(location, " ")))
        requested_point <- location #so that it can be spit back out on the map later
        location <- data.frame(lat = location[1], long = location[2])
        location <- terra::vect(
          location,
          geom = c("long", "lat"),
          crs = "+proj=longlat +EPSG:3347"
        )
        type <- "longlat"
      },
      error = function(e) {
        stop(
          "Please check your input for `location`. It looks like you're trying to input decimal degrees, so please ensure that the latitude and longitude are only numbers separated by a comma."
        )
      }
    )
  } else {
    location_id <- lookup_location_id(con, location)
    if (!is.na(location_id)) {
      type <- "DB"
    }
  }

  if (type == "longlat") {
    # already handled
  } else if (!is.na(location_id)) {
    location_metadata <- DBI::dbGetQuery(
      con,
      "SELECT location_code, longitude, latitude FROM locations WHERE location_id = $1 LIMIT 1;",
      params = list(location_id)
    )

    # Convert to terra spatial vector based on latitude and longitude
    if (nrow(location_metadata) > 0) {
      location <- terra::vect(
        location_metadata,
        geom = c("longitude", "latitude"),
        crs = "EPSG:4326"
      )
    }

    requested_point <- location_metadata$location_code[1]

    # requested_point <- if (nrow(location_metadata) > 0) {
    #   location_metadata$location_code[1]
    # } else {
    #   location
    # }
  } else {
    cli::cli_abort(c(
      "x" = "Your input for `location` could not be coerced to decimal degrees or to a location in our database.",
      "i" = "Please review the help file for proper format and try again."
    ))
  }

  #Load the drainage basin
  tryCatch(
    {
      if (type == "longlat") {
        # Find the smallest basins possible
        basins <- getVector(
          layer_name = "Drainage basins",
          silent = TRUE,
          con = con
        )
        basins <- terra::project(basins, "+proj=longlat +EPSG:3347")
        ids <- terra::extract(basins, location)
        basins <- basins[basins$geom_id %in% ids[, "geom_id"], ]
        basins$area <- terra::expanse(basins)
        basin <- basins[basins$area == min(basins$area), ]
      } else {
        basin <- getVector(
          layer_name = "Drainage basins",
          feature_name = requested_point,
          con = con,
          silent = TRUE
        )
        basin <- terra::project(basin, "+proj=longlat +EPSG:3347")
      }
    },
    error = function(e) {
      basin <<- data.frame()
    }
  )

  if (type == "DB" & nrow(basin) == 0) {
    cli::cli_abort(c(
      "x" = "No drainage basin found for the location {requested_point}.",
      "i" = "Please check that the location is correct and try again."
    ))
  }

  #Determine the appropriate clip polygon for the files to minimize space requirements.
  polygons <- prov_buff # Package data
  polygons <- terra::project(polygons, "+proj=longlat +EPSG:3347")

  if (type == "DB") {
    location <- terra::project(location, "+proj=longlat +EPSG:3347")
    within <- terra::relate(location, polygons, relation = "within")
    within <- as.data.frame(polygons[which(within)])$PREABBR
  } else if (type == "longlat") {
    terra::crs(location) <- terra::crs(polygons)
    location <- terra::project(location, "+proj=longlat +EPSG:3347")
    terra::intersect(location, polygons)
    within <- terra::relate(location, polygons, relation = "within")
    within <- as.data.frame(polygons[which(within)])$PREABBR
  }

  #Determine the sequence of rasters from start to end. Load from database instead of downloading.
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  attr(start, "tzone") <- "UTC"
  attr(end, "tzone") <- "UTC"
  #Check that start < end
  if (start > end) {
    stop(
      "The start time you specified is *after* the end time. R is not a time travel machine, please try again."
    )
  }

  if (start > Sys.time() - 60 * 60 * 1.2) {
    #if TRUE, the start is prior to any issued HRDPA and so is the end. Only a sequence for forecast will be generated.
    hrdpa <- FALSE
    actual_times_hrdpa <- NULL
    if (end > (Sys.time() + 60 * 60 * 44)) {
      end <- Sys.time() + 60 * 60 * 44
      attr(end, "tzone") <- "UTC"
    }
  } else {
    #start is not in future, try to load hrdpa from database
    hrdpa <- TRUE
  }

  if (hrdpa) {
    start_hrdpa <- start + 60 * 60 * 4.8 #Assuming that rasters are issued at most 1.2 hours post valid time, this sets the start time so that the 6 hours before the requested start time is not included.
    start_hrdpa <- lubridate::floor_date(start_hrdpa, "6 hours")
    end_hrdpa <- lubridate::floor_date(end, "6 hours")
    if (end_hrdpa < start_hrdpa) {
      end_hrdpa <- start_hrdpa
    }
    sequence_hrdpa <- seq.POSIXt(start_hrdpa, end_hrdpa, by = "6 hour")

    # Query rasters_reference to get valid times for HRDPA
    ref_query_hrdpa <- paste0(
      "SELECT reference_id, valid_from, valid_to FROM rasters_reference ",
      "WHERE model = 'HRDPA' ",
      "AND valid_from <= '",
      format(end_hrdpa, "%Y-%m-%d %H:%M:%S"),
      "'::timestamp ",
      "AND valid_to >= '",
      format(start_hrdpa, "%Y-%m-%d %H:%M:%S"),
      "'::timestamp"
    )

    hrdpa_refs <- DBI::dbGetQuery(con, ref_query_hrdpa)

    # Try to retrieve HRDPA rasters from database
    tryCatch(
      {
        if (nrow(hrdpa_refs) > 0) {
          # Build clause using reference IDs and their valid times
          ref_ids <- paste0("'", hrdpa_refs$reference_id, "'", collapse = ", ")
          clauses_hrdpa <- paste0(
            "WHERE reference_id IN (",
            ref_ids,
            ")"
          )

          hrdpa_rasters_db <- getRaster(
            clauses = clauses_hrdpa,
            tbl_name = c("spatial", "rasters"),
            col_name = "rast",
            con = con
          )
          if (!is.null(hrdpa_rasters_db)) {
            hrdpa_files <- list(hrdpa_rasters_db)
            actual_times_hrdpa <- c(
              min(hrdpa_refs$valid_from),
              max(hrdpa_refs$valid_to)
            )
          } else {
            hrdpa <- FALSE
            actual_times_hrdpa <- NULL
          }
        } else {
          hrdpa <- FALSE
          actual_times_hrdpa <- NULL
        }
      },
      error = function(e) {
        if (!silent) {
          message(
            "HRDPA data not found in database. Proceeding with forecast data only."
          )
        }
        hrdpa <<- FALSE
        actual_times_hrdpa <<- NULL
      }
    )
  } else {
    hrdpa_files <- NULL
    actual_times_hrdpa <- NULL
  }

  end_hrdps <- lubridate::floor_date(end, "1 hours")
  start_hrdps <- lubridate::floor_date(start, "1 hours")
  hrdps <- FALSE #overwritten if hrdps are usable
  actual_times_hrdps <- NULL
  # Query database for HRDPS rasters
  if (!hrdpa) {
    #only hrdps needed, times are in the future
    hrdps <- TRUE
    start_hrdps <- lubridate::floor_date(start, "1 hours")
    end_hrdps <- lubridate::floor_date(end, "1 hours")

    # Query rasters_reference to get valid times for HRDPS
    ref_query_hrdps <- paste0(
      "SELECT reference_id, valid_from, valid_to FROM rasters_reference ",
      "WHERE model = 'HRDPS' ",
      "AND valid_from <= '",
      format(end_hrdps, "%Y-%m-%d %H:%M:%S"),
      "'::timestamp ",
      "AND valid_to >= '",
      format(start_hrdps, "%Y-%m-%d %H:%M:%S"),
      "'::timestamp"
    )

    hrdps_refs <- DBI::dbGetQuery(con, ref_query_hrdps)

    if (nrow(hrdps_refs) == 0) {
      warning(
        "No HRDPS rasters found in database for the requested time range."
      )
    }

    tryCatch(
      {
        if (nrow(hrdps_refs) > 0) {
          # Build clause using reference IDs and their valid times
          ref_ids <- paste0("'", hrdps_refs$reference_id, "'", collapse = ", ")
          clauses_hrdps <- paste0(
            "WHERE reference_id IN (",
            ref_ids,
            ")"
          )

          forecast_precip <- getRaster(
            clauses = clauses_hrdps,
            tbl_name = c("spatial", "rasters"),
            col_name = "rast",
            con = con
          )
          if (!is.null(forecast_precip)) {
            forecast_precip <- terra::project(
              forecast_precip,
              "+proj=longlat +EPSG:3347"
            )
            names(forecast_precip) <- "precip"
            actual_times_hrdps <- c(
              min(hrdps_refs$valid_from),
              max(hrdps_refs$valid_to)
            )
          } else {
            hrdps <- FALSE
          }
        } else {
          hrdps <- FALSE
          if (!silent) {
            message("No HRDPS data found in database for requested time range.")
          }
        }
      },
      error = function(e) {
        if (!silent) {
          message("Error retrieving HRDPS data from database.")
        }
        hrdps <<- FALSE
      }
    )
  } else if (hrdpa & end > (Sys.time() - 60 * 60 * 6)) {
    #There might be a need for some hrdps to fill in to the requested end time.
    hrdps <- TRUE
    start_hrdps <- lubridate::floor_date(start, "1 hours")
    end_hrdps <- lubridate::floor_date(end, "1 hours")

    clauses_hrdps <- paste0(
      "WHERE valid_time >= '",
      format(start_hrdps, "%Y-%m-%d %H:%M:%S"),
      "'::timestamp AND valid_time <= '",
      format(end_hrdps, "%Y-%m-%d %H:%M:%S"),
      "'::timestamp AND model = 'HRDPS'"
    )

    tryCatch(
      {
        forecast_precip <- getRaster(
          clauses = clauses_hrdps,
          tbl_name = c("rasters", "precipitations"),
          col_name = "rast",
          con = con
        )
        if (!is.null(forecast_precip)) {
          forecast_precip <- terra::project(
            forecast_precip,
            "+proj=longlat +EPSG:3347"
          )
          names(forecast_precip) <- "precip"
          actual_times_hrdps <- c(start_hrdps, end_hrdps)
        } else {
          hrdps <- FALSE
        }
      },
      error = function(e) {
        hrdps <<- FALSE
      }
    )
  }

  ###now the rasters are present for the extent and time required, finally! Proceed to accumulating them into a single raster.
  if (hrdpa & !hrdps) {
    if (length(hrdpa_files) > 1) {
      # hrdpa_files is now a list of raster objects from database
      all_hrdpa <- hrdpa_files
      xmin <- numeric(0)
      xmax <- numeric(0)
      ymin <- numeric(0)
      ymax <- numeric(0)
      for (i in 1:length(all_hrdpa)) {
        all_hrdpa[[i]] <- terra::project(all_hrdpa[[i]], all_hrdpa[[1]])
        xmin <- c(xmin, terra::ext(all_hrdpa[[i]])[1])
        xmax <- c(xmax, terra::ext(all_hrdpa[[i]])[2])
        ymin <- c(ymin, terra::ext(all_hrdpa[[i]])[3])
        ymax <- c(ymax, terra::ext(all_hrdpa[[i]])[4])
      }
      # Check if at least one of the extents are not uniform across all rasters, trim if TRUE
      if (
        (stats::var(xmin) != 0) |
          (stats::var(xmax) != 0) |
          (stats::var(ymin) != 0) |
          (stats::var(ymax) != 0)
      ) {
        min_extent <- terra::rast()
        min_extent <- terra::project(min_extent, all_hrdpa[[1]]) #Project must happend before setting the boundaries, otherwise the boundaries of the project from layer apply
        terra::ext(min_extent) <- c(max(xmin), min(xmax), max(ymin), min(ymax))
        min_extent <- terra::as.polygons(min_extent)
        for (i in 1:length(all_hrdpa)) {
          all_hrdpa[[i]] <- terra::mask(all_hrdpa[[i]], min_extent)
          all_hrdpa[[i]] <- terra::trim(all_hrdpa[[i]])
        }
      }
      hrdpa_rasters <- terra::sds(all_hrdpa)
      total <- hrdpa_rasters[1] #prepare to accumulate/add raster values
      for (i in 2:length(hrdpa_rasters)) {
        total <- total + hrdpa_rasters[i]
      }
    } else if (length(hrdpa_files) == 1) {
      total <- hrdpa_files[[1]]
    } else {
      cli::cli_abort("No HRDPA rasters available for the requested time range.")
    }
    names(total) <- "precip"
    total <- terra::project(total, "+proj=longlat +EPSG:3347")
    actual_times <- actual_times_hrdpa
  }

  if (!hrdpa & hrdps) {
    total <- forecast_precip
    actual_times <- actual_times_hrdps
  }

  if (hrdpa & hrdps) {
    #start with hrdpa - files are now raster objects from database
    if (length(hrdpa_files) > 1) {
      all_hrdpa <- hrdpa_files
      xmin <- numeric(0)
      xmax <- numeric(0)
      ymin <- numeric(0)
      ymax <- numeric(0)
      for (i in 1:length(all_hrdpa)) {
        all_hrdpa[[i]] <- terra::project(all_hrdpa[[i]], all_hrdpa[[1]])
        xmin <- c(xmin, terra::ext(all_hrdpa[[i]])[1])
        xmax <- c(xmax, terra::ext(all_hrdpa[[i]])[2])
        ymin <- c(ymin, terra::ext(all_hrdpa[[i]])[3])
        ymax <- c(ymax, terra::ext(all_hrdpa[[i]])[4])
      }
      # Check if at least one of the extents are not uniform across all rasters, trim if TRUE
      if (
        (stats::var(xmin) != 0) |
          (stats::var(xmax) != 0) |
          (stats::var(ymin) != 0) |
          (stats::var(ymax) != 0)
      ) {
        min_extent <- terra::rast()
        min_extent <- terra::project(min_extent, all_hrdpa[[1]])
        terra::ext(min_extent) <- c(max(xmin), min(xmax), max(ymin), min(ymax))
        min_extent <- terra::as.polygons(min_extent)
        for (i in 1:length(all_hrdpa)) {
          all_hrdpa[[i]] <- terra::mask(all_hrdpa[[i]], min_extent)
          all_hrdpa[[i]] <- terra::trim(all_hrdpa[[i]])
        }
      }
      hrdpa_rasters <- terra::sds(all_hrdpa)
      total_hrdpa <- hrdpa_rasters[1] #prepare to accumulate/add raster values
      for (i in 2:length(hrdpa_rasters)) {
        total_hrdpa <- total_hrdpa + hrdpa_rasters[i]
      }
    } else if (length(hrdpa_files) == 1) {
      total_hrdpa <- hrdpa_files[[1]]
    } else {
      cli::cli_abort("No HRDPA rasters available for the requested time range.")
    }
    names(total_hrdpa) <- "precip"
    total_hrdpa <- terra::project(total_hrdpa, "+proj=longlat +EPSG:3347")
    actual_times <- c(min(actual_times_hrdpa), max(actual_times_hrdps))

    #trim one by the other (and the reverse) to get extents the same
    forecast_precip <- terra::resample(forecast_precip, total_hrdpa) #aligns the rasters
    forecast_precip <- terra::extend(forecast_precip, total_hrdpa)
    total_hrdpa <- terra::extend(total_hrdpa, forecast_precip)

    total <- total_hrdpa + forecast_precip
  }

  #Add zeros for the actual_times so that they are not ambiguous at midnight
  actual_times <- as.character(actual_times)
  for (i in 1:2) {
    if (nchar(actual_times[i]) < 13) {
      actual_times[i] <- paste0(actual_times[i], " 00:00")
    }
    if (nchar(actual_times[i]) > 16) {
      actual_times[i] <- substr(actual_times[i], 1, 16)
    }
  }
  if (hrdpa) {
    actual_times_hrdpa <- as.character(actual_times_hrdpa)
    for (i in 1:2) {
      if (nchar(actual_times_hrdpa[i]) < 13) {
        actual_times_hrdpa[i] <- paste0(actual_times_hrdpa[i], " 00:00")
      }
      if (nchar(actual_times_hrdpa[i]) > 16) {
        actual_times_hrdpa[i] <- substr(actual_times_hrdpa[i], 1, 16)
      }
    }
  }
  if (hrdps) {
    actual_times_hrdps <- as.character(actual_times_hrdps)
    for (i in 1:2) {
      if (nchar(actual_times_hrdps[i]) < 13) {
        actual_times_hrdps[i] <- paste0(actual_times_hrdps[i], " 00:00")
      }
      if (nchar(actual_times_hrdps[i]) > 16) {
        actual_times_hrdps[i] <- substr(actual_times_hrdps[i], 1, 16)
      }
    }
  }

  #crop to the watershed or to the point to get the precip within the basin or at the point, if no basin exists
  if (type == "DB") {
    cropped_precip <- terra::mask(total, basin)
  } else {
    if (nrow(basin) == 1) {
      cropped_precip <- terra::mask(total, basin)
    } else {
      cropped_precip <- terra::mask(total, terra::buffer(location, 5000)) # Rain within 5 km of the point
    }
  }
  cropped_precip <- terra::trim(cropped_precip)
  mean_precip <- as.data.frame(cropped_precip)
  mean_precip <- mean(mean_precip$precip)
  if (type != "longlat") {
    minmax_precip <- terra::minmax(cropped_precip)
    min <- minmax_precip[1]
    max <- minmax_precip[2]
  }

  ###Map the output if requested
  if (map) {
    tryCatch(
      {
        if (type == "longlat") {
          #Special treatment so we can try to return a watershed with the point for context
          if (nrow(basin) == 1) {
            buff_size <- terra::expanse(basin) * 0.0000006
            if (buff_size < 2500) {
              buff_size <- 2500
            }
            watershed_buff <- terra::buffer(basin, buff_size)
          } else {
            #The watershed cannot be found, so make a big buffer
            watershed_buff <- terra::buffer(basin, 50000) #50km radius buffer
            #TODO: make this a rectangular/square extent instead to better use the display
            basin <- watershed_buff
            basin$feature_name <- "No watershed number"
            basin$description <- "Not a watershed"
          }
        } else {
          #"type" == DB
          buff_size <- terra::expanse(basin) * 0.0000006
          if (buff_size < 2500) {
            buff_size <- 2500
          }
          watershed_buff <- terra::buffer(basin, buff_size)
        }

        #Load supporting layers
        roads <- getVector(layer_name = "Roads", con = con, silent = TRUE)
        roads <- terra::project(roads, "+proj=longlat +EPSG:3347")

        communities <- getVector(
          layer_name = "Communities",
          con = con,
          silent = TRUE
        )
        communities <- terra::project(communities, "+proj=longlat +EPSG:3347")

        borders <- getVector(
          layer_name = "Provincial/Territorial Boundaries",
          feature_name = "Yukon",
          con = con,
          silent = TRUE
        )
        borders <- terra::project(borders, "+proj=longlat +EPSG:3347")

        #And finally plot it all!
        if (maptype == "dynamic") {
          leafmap <- terra::plet(
            total,
            col = c("#F0F7FF", raster_col),
            tiles = NULL,
            legend = NULL
          ) |>
            terra::lines(basin, col = "darkred")
          if (type == "longlat") {
            leafmap <- leafmap |>
              terra::points(
                location,
                pch = 17,
                col = "darkorchid1",
                cex = 2,
                group = "Communities"
              )
          }
          leafmap <- leafmap |>
            terra::lines(
              roads,
              lwd = 1.5,
              alpha = 0.7,
              dashArray = "4 1 2 3",
              group = "Roads"
            )

          # Left out for now because it's a lot of lines to process and clutters the display
          streams <- getVector(
            layer_name = "Watercourses",
            feature_name = "Yukon watercourses",
            con = con,
            silent = TRUE
          )
          streams <- terra::project(streams, "+proj=longlat +EPSG:3347")
          leafmap <- leafmap |>
            terra::lines(
              streams,
              lwd = 0.08,
              col = "blue",
              alpha = 1,
              group = "Streams"
            )

          waterbodies <- getVector(
            layer_name = "Waterbodies",
            feature_name = "Yukon waterbodies",
            con = con,
            silent = TRUE
          )
          waterbodies <- terra::project(waterbodies, "+proj=longlat +EPSG:3347")

          leafmap <- leafmap |>
            terra::polys(
              waterbodies,
              col = "blue",
              border = "blue",
              alpha = 0.5,
              lwd = 0.001,
              group = "Waterbodies"
            ) |>
            terra::points(communities, cex = 1.5, group = "Communities") |>
            terra::lines(borders, lwd = 2, dashArray = "4", group = "Borders")

          if (title) {
            text <- paste0(
              "Precipitation as mm of water equivalent from ",
              actual_times[1],
              " to ",
              actual_times[2],
              " UTC  <br> Watershed: ",
              basin$feature_name,
              ", ",
              stringr::str_to_title(basin$description)
            )
            leafmap <- leafmap |>
              leaflet::addControl(
                text,
                position = "topright",
                className = "map-title"
              )
          }

          minmax_total <- terra::minmax(total)
          leafmap <- leafmap |>
            leaflet::addLegend(
              pal = leaflet::colorNumeric(
                palette = c("#F0F7FF", raster_col),
                domain = minmax_total
              ),
              values = seq(minmax_total[1], minmax_total[2], length.out = 100),
              title = "Precipitation (mm)",
              position = "bottomright"
            ) |>
            leaflet::addLayersControl(
              overlayGroups = c(
                "Streams",
                "Waterbodies",
                "Communities",
                "Roads",
                "Borders"
              ),
              options = leaflet::layersControlOptions(collapsed = FALSE)
            ) |>
            leaflet::hideGroup(c("Streams"))

          plot <- leafmap
        } else {
          cropped_precip_rast <- terra::mask(total, watershed_buff)
          cropped_precip_rast <- terra::trim(cropped_precip_rast)

          if (terra::expanse(basin) < 30000000000) {
            streams <- getVector(
              layer_name = "Watercourses",
              feature_name = "Yukon watercourses",
              con = con,
              silent = TRUE
            )
            streams <- terra::project(streams, "+proj=longlat +EPSG:3347")
            streams <- terra::crop(streams, basin)
          }

          waterbodies <- getVector(
            layer_name = "Waterbodies",
            feature_name = "Yukon waterbodies",
            con = con,
            silent = TRUE
          )
          waterbodies <- terra::project(waterbodies, "+proj=longlat +EPSG:3347")
          waterbodies <- terra::crop(waterbodies, basin) #even though masked, water body features that are even a little within the watershed polygon will be retained. This is nice as it brings out the major rivers for context.

          terra::plot(
            cropped_precip_rast,
            col = c("#F0F7FF", raster_col),
            buffer = TRUE,
            alpha = 0.5
          )
          terra::polys(basin, border = "darkred")
          if (type == "longlat") {
            terra::points(location, pch = 17, col = "darkorchid1", cex = 2)
          }
          terra::plot(roads, lwd = 1.5, alpha = 0.7, add = T)
          if (terra::expanse(basin) < 30000000000) {
            terra::plot(
              streams,
              lwd = 0.08,
              col = "blue",
              border = NULL,
              alpha = 0.5,
              add = T
            )
          }
          terra::plot(
            waterbodies,
            col = "blue",
            border = "blue",
            alpha = 0.1,
            lwd = 0.001,
            add = T
          )
          terra::points(communities, cex = 1.5)
          terra::lines(borders, lwd = 2, lty = "twodash")
          terra::text(
            communities,
            labels = communities$feature_name,
            pos = 4,
            offset = 1,
            font = 2,
            cex = 0.9
          )
          if (type == "longlat") {
            terra::text(
              location,
              labels = paste0(requested_point[1], ", ", requested_point[2]),
              col = "black",
              pos = 4,
              offset = 1,
              font = 2
            )
          }
          if (title) {
            text <- paste0(
              "Precipitation as mm of water equivalent from ",
              actual_times[1],
              " to ",
              actual_times[2],
              " UTC  \nWatershed: ",
              basin$feature_name,
              ", ",
              stringr::str_to_title(basin$description)
            )
            graphics::mtext(text, side = 3, line = 4, outer = FALSE, adj = 0.5)
          }

          plot <- grDevices::recordPlot()
        }
      },
      error = function(e) {
        cli::cli_warn(
          "The map could not be generated. Please check your internet connection and try again."
        )
      }
    )
  }

  if (type == "longlat") {
    list <- list(
      mean_precip = mean_precip,
      total_time_range_UTC = actual_times,
      reanalysis_time_range_UTC = actual_times_hrdpa,
      forecast_time_range_UTC = actual_times_hrdps,
      point = requested_point,
      plot = if (map) plot else NULL
    )
    if (!silent) {
      if (map) {
        # decide verb and note based on flags
        verb <- if (!hrdps) {
          "fell"
        } else if (hrdps && hrdpa) {
          "will have fallen"
        } else {
          "will fall"
        }
        note_txt <- if (!hrdps) {
          "Precipitation is based solely on retrospective-looking data produced by the HRDPA."
        } else if (hrdps && hrdpa) {
          "Precipitation is based on a mixture of retrospective-looking data from the HRDPA re-analysis and modelled precipitation using the HRDPS."
        } else {
          "Precipitation is based on the HRDPS climate model outputs."
        }
        cli::cli_text(
          "\n\n",
          "{.fg_blue {.strong {.underline {round(mean_precip, 2)}}}} mm of rain or water equivalent ",
          verb,
          " at your requested point (",
          requested_point[1],
          ", ",
          requested_point[2],
          ") between {.fg_blue {.strong {as.character(actual_times[1])}}} and ",
          "{.fg_blue {.strong {as.character(actual_times[2])}}} UTC.\n",
          "The smallest watershed for which I could find a polygon is ",
          basin$feature_name,
          ", ",
          stringr::str_to_title(basin$description),
          "\n\n",
          "NOTES:\n",
          "  Your requested times have been adjusted to align with available data.\n",
          "  ",
          note_txt,
          "\n\n"
        )
      } else {
        # decide verb and note based on flags
        verb <- if (!hrdps) {
          "fell"
        } else if (hrdps && hrdpa) {
          "will have fallen"
        } else {
          "will fall"
        }

        note_txt <- if (!hrdps) {
          "Precipitation is based solely on retrospective-looking data produced by the HRDPA."
        } else if (hrdps && hrdpa) {
          "Precipitation is based on a mixture of retrospective-looking data from the HRDPA re-analysis and modelled precipitation using the HRDPS."
        } else {
          "Precipitation is based on the HRDPS climate model outputs."
        }

        # emit styled text
        cli::cli_text(
          "\n\n",
          "{.fg_blue {.strong {.underline {round(mean_precip, 2)}}}} mm of rain or water equivalent ",
          verb,
          " at your requested point (",
          requested_point[1],
          ", ",
          requested_point[2],
          ") between {.fg_blue {.strong {as.character(actual_times[1])}}} and ",
          "{.fg_blue {.strong {as.character(actual_times[2])}}} UTC.\n\n",
          "NOTES:\n",
          "  Your requested times have been adjusted to align with available data.\n",
          "  ",
          note_txt,
          "\n\n"
        )
      }
    }
  } else {
    list <- list(
      mean_precip = mean_precip,
      min = min,
      max = max,
      total_time_range_UTC = actual_times,
      reanalysis_time_range_UTC = actual_times_hrdpa,
      forecast_time_range_UTC = actual_times_hrdps,
      watershed = basin$feature_name,
      plot = if (map) plot else NULL
    )
    if (!silent) {
      # Determine verb
      verb <- if (!hrdps) {
        "fell"
      } else if (hrdps && hrdpa) {
        "will have fallen"
      } else {
        "will fall"
      }

      # Determine notes text
      note_txt <- if (!hrdps) {
        "Precipitation is based solely on retrospective-looking data produced by the HRDPA."
      } else if (hrdps && hrdpa) {
        "Precipitation is based on a mixture of retrospective-looking data from the HRDPA re-analysis and modelled precipitation using the HRDPS."
      } else {
        "Precipitation is based on the HRDPS climate model outputs."
      }

      # Emit styled output
      cli::cli_text(
        "\n\n",
        "On average, {.fg_blue {.strong {.underline {round(mean_precip, 2)}}}} mm of rain or water equivalent ",
        "(range: {round(min, 2)} to {round(max, 2)} mm) ",
        verb,
        " across the watershed requested (",
        basin$feature_name,
        ", ",
        stringr::str_to_title(basin$description),
        ") between ",
        "{.fg_blue {.strong {as.character(actual_times[1])}}} and ",
        "{.fg_blue {.strong {as.character(actual_times[2])}}} UTC.\n\n",
        "NOTES:\n",
        "  Your requested times have been adjusted to align with available data.\n",
        "  ",
        note_txt,
        "\n"
      )
    }
  }
  if (maptype == "dynamic") {
    list <- c(list, total_raster = total)
  }
  return(list)
}
