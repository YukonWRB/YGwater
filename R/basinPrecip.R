#' Accumulated precipitation
#'
#' Calculates accumulated precipitation above defined drainages or immediately surrounding a specified point. If possible, make sure to specify a location in parameters hrdpa_loc and hrdps_loc to speed things up!
#'
#' If specifying `location` as coordinates rather than a WSC station (or non WSC station, see note below), the numeric result will be of precipitation in the 2.5 km2 cell on which the point falls. Map output, if requested, will be for the smallest available drainage polygon containing that point.
#'
#' Raster images of precipitation are only kept by ECCC for 30 days. If specifying a `start` or `end` prior to that without having precipitation rasters available locally and specified with `hrdpa_loc`, the start date/time will be adjusted and a warning given.
#'
#' Rasters must be downloaded for each 6-hour period between `start` and `end`. It is therefore strongly suggested that you designate and use a folder for this purpose. `getHRDPA` is called for downloading and clipping rasters.
#'
#' Drainage polygons pointed to by `drainage_loc` are best created with [WSC_drainages()] and must be named drainage_polygons.xxx. The drainage polygon IDs (in WSC format, i.e. 10EA001) must be listed in the attribute table under column StationNum and the name under column NameNom. To deal with non-WSC data sources it is possible to have non-WSC polygons here as well. In order for this function to recognize these as non-WSC drainages please respect the following: a) strings should be made of two digits, two letters, and three digits (same as WSC); b) The starting digits must NOT be in the sequence from 01 to 12 as these are taken by the WSC; c) duplicate entries are not allowed.
#'
#' Additional spatial data pointed to by `spatial_loc` must be in shapefiles with the following names recognized: waterbodies, watercourses, roads, communities, borders, coastlines. Keep in mind that large shapefiles can be lengthy for R to graphically represent. To ease this limitation, the watercourses file is left out when the drainage extent is greater than 30 000 km2; by that size, major water courses are expected to be represented by polygons in the waterbodies layer.
#'
#' @param location The location above which you wish to calculate precipitation. Specify either a WSC or WSC-like station ID (e.g. `"09AB004"`) for which there is a corresponding entry in the shapefile pointed to by drainage_loc, or coordinates in signed decimal degrees in form latitude, longitude (`"60.1234 -139.1234"`; note the space, negative sign, and lack of comma). See details for more info if specifying coordinates.
#' @param start The start of the time period over which to accumulate precipitation. Use format `"yyyy-mm-dd hh:mm"` (character vector) in UTC time, or a POSIXct object (e.g. `Sys.time()-60*60*24` for one day in the past). In the case of a POSIXct object, the timezone is converted to UTC without modifying the time. See details if requesting earlier than 30 days prior to now.
#' @param end The end of the time period over which to accumulate precipitation. Other details as per `start`
#' @param map Should a map be output to the console? See details for more info.
#' @param org_defaults This parameter can override the default file locations for rasters, drainages, and spatial files. As of now, only available for "YWRB" (Yukon Water Resources Branch) or NULL. Specifying any of hrdpa_loc, hrdps_loc, drainage_loc, and/or spatial_loc directories will override the organization default for that/those parameters.
#' @param hrdpa_loc The directory (folder) where past precipitation rasters are to be downloaded. Suggested use is to specify a repository where all such rasters are saved to speed processing time and reduce data usage. If using the default NULL, rasters will not persist beyond your current R session.
#' @param hrdps_loc The directory (folder) where forecast precipitation rasters are to be downloaded. A folder will be created for the specific parameter (in this case, precipitation) or selected if already existing.
#' @param drainage_loc The shapefile drainage polygons. See Notes for more info.
#' @param spatial_loc The directory in which spatial data (as shapefiles) is kept for making maps nicer. Will recognize the following shapefile names (case sensitive): waterbodies, watercourses, roads, communities, borders (provincial/territorial/international), coastlines. See additional notes.
#' @param silent If TRUE, no output is printed to the console.
#'
#' @return The accumulated precipitation in mm of water within the drainage specified or immediately surrounding the point specified in `location` printed to the console and (optionally) a map of precipitation printed to the console. A list is also generated containing statistics and the optional plot; to save this plot to disc use either png() or dev.print(), or use the graphical device export functionality.
#' @export

#TODO Update function to work directly with DB, with terra object, or with shapefiles, or with gpkg files.
#TODO: problem with extents not matching. reproduce by first calling a map for somewhere in YT, then in Ontario. will get Error:[sds] extents do not match, which means that the DL and/or file selection process didn't work properly
#IDEA: Allow multiple plots to be fetched, or a time-lapse of plots (better). Allow setting increments and number of plots.
#IDEA: use leaflet to display maps
#TODO: Get precip further in future using RDPS beyond HRDPS range.

basinPrecip <- function(location,
                        start = Sys.time()-60*60*24,
                        end = Sys.time(),
                        map = FALSE,
                        org_defaults = "YWRB",
                        hrdpa_loc = NULL,
                        hrdps_loc = NULL,
                        drainage_loc = NULL,
                        spatial_loc = NULL,
                        silent = FALSE
                        )
{

  #Set organization defaults
  if (org_defaults == "YWRB"){
    if (is.null(hrdpa_loc)){
      hrdpa_loc = "//env-fs/env-data/corp/water/Common_GW_SW/Data/precip_HRDPA"
    }
    if (is.null(hrdps_loc)){
      hrdps_loc <- "//env-fs/env-data/corp/water/Common_GW_SW/Data/HRDPS"
    }
    if (is.null(drainage_loc)){
      drainage_loc <- "//env-fs/env-data/corp/water/Common_GW_SW/Data/basins/drainage_polygons.shp"
    }
    if (is.null(spatial_loc)){
      spatial_loc <- "//env-fs/env-data/corp/water/Common_GW_SW/Data/r_map_layers"
    }
  }

  #Basic checks on location
  if(!inherits(location, "character")){
    stop("Parameter `location` should be specified as a character vector.")
  }
  #Basic checks on start and end
  if (!(inherits(start[1], "POSIXct"))) {
    if (inherits(start, "character")) {
      if (nchar(start) < 15){
        stop("Check your parameter for start: it should either be a POSIXct object or a character vector of form yyy-mm-dd HH:mm.")
      }
    }
  }
  if (!(inherits(end[1], "POSIXct"))) {
    if (inherits(end, "character")) {
      if (nchar(end) < 15){
        stop("Check your parameter for end: it should either be a POSIXct object or a character vector of form yyy-mm-dd HH:mm.")
      }
    }
  }

  if (grepl("^[0-9]{2}[\\.]{1}[0-9]*[ ]{1}[-]{1}[0-9]*[\\.]{1}[0-9]*", location)){
    tryCatch({
      location <- as.numeric(unlist(strsplit(location, " ")))
      requested_point <- location #so that it can be spit back out on the map later
      location <- data.frame(lat = location[1], long = location[2])
      location <- terra::vect(location, geom = c("long", "lat"))
      type <- "longlat"
    }, error = function(e) {
      stop("Please check your input for `location`. It looks like you're trying to input decimal degrees, so please ensure that the latitude and longitude are only numbers separated by a comma. If you're trying to input a WSC station please use a standard WSC or WSC-like ID.")
    })
  } else if (grepl("[0-9]{2}[A-Za-z]{2}[0-9]{3}", location)){
    type <- "WSC"
  } else {crayon::red(stop("Your input for `location` could not be coerced to decimal degrees or to a standard WSC or WSC-like station ID. Please review the help file for proper format and try again."))}

  #Load the drainages
  drainages <- terra::vect(drainage_loc)
  if (type == "WSC"){
      station_check_polygon <- location %in% drainages$StationNum
      if (!station_check_polygon){
        stop(crayon::red(paste0("You've stumbled upon a station for which there is no corresponding polygon in the shapefile specified in drainage_loc. Try a different station, or use the latitude/longitude of a point just upstream of your station of interest. If this is a WSC station and you know it should have a defined watershed you could use the function WSC_drainages to create the polygons and update the shapefile in drainage_loc. If this is not a WSC station you could use GIS software or function drainageBasin() to define the watershed and add the polygon to the shapefile.")))
      }
    }

  #Determine the appropriate clip polygon for the files to minimize space requirements.
  polygons <- terra::vect(prov_buff)
  polygons <- terra::project(polygons, "+proj=longlat +EPSG:3347")

  if (type == "WSC"){
    location <- terra::subset(drainages, drainages$StationNum == location)
    location <- terra::project(location, "+proj=longlat +EPSG:3347")
    within <- terra::relate(location, polygons, relation = "within")
    within <- as.data.frame(polygons[which(within)])$PREABBR
  } else if (type == "longlat"){
    terra::crs(location) <- terra::crs(polygons)
    location <- terra::project(location, "+proj=longlat +EPSG:3347")
    terra::intersect(location, polygons)
    within <- terra::relate(location, polygons, relation = "within")
    within <- as.data.frame(polygons[which(within)])$PREABBR
  }

  #Determine the sequence of rasters from start to end and if the clip is adequate. If not, call getHRDPA or getHRDPS to fill the gap.
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  attr(start, "tzone") <- "UTC"
  attr(end, "tzone") <- "UTC"
  #Check that start < end
  if (start > end) {
    stop("The start time you specified is *after* the end time. R is not a time travel machine, please try again.")
  }

  if (start > Sys.time()-60*60*1.2) { #if TRUE, the start is prior to any issued HRDPA and so is the end. Only a sequence for forecast will be generated.
    hrdpa <- FALSE
    actual_times_hrdpa <- NULL
    if (end > (Sys.time() + 60*60*44)){
      end <- Sys.time() + 60*60*44
      attr(end, "tzone") <- "UTC"
    }
  } else { #start is not in future, there might be hrdpa available
    hrdpa <- TRUE
  }

  if (hrdpa == TRUE){
    start_hrdpa <- start + 60*60*4.8 #Assuming that rasters are issued at most 1.2 hours post valid time, this sets the start time so that the 6 hours before the requested start time is not included.
    start_hrdpa <- lubridate::floor_date(start_hrdpa, "6 hours")
    end_hrdpa <- lubridate::floor_date(end, "6 hours")
    if (end_hrdpa < start_hrdpa){
      end_hrdpa <- start_hrdpa
    }
    sequence_hrdpa <- seq.POSIXt(start_hrdpa, end_hrdpa, by = "6 hour")

    #Check if any files exist yet to match sequence_hrdpa
    available <- xml2::read_html("https://dd.weather.gc.ca/analysis/precip/hrdpa/grib2/polar_stereographic/06/") #6 hour products for first day
    available <- rvest::html_elements(available, xpath='//*[contains(@href, ".grib2")]') %>%
      rvest::html_attr("href")
    available <- as.data.frame(as.character(available))
    names(available) <- "link"
    available <- available %>% dplyr::mutate(cutoff = substr(.data$link, 20,23),
                                             valid = as.POSIXct(substr(.data$link, 45,54), format = "%Y%m%d%H", tz="UTC")
    )

    last_available_01 <- max(dplyr::filter(available, .data$cutoff == "0100")$valid)
    if (min(sequence_hrdpa) <= last_available_01){ #TRUE means that there is at least one HRDPA available online or that there is one locally, so fetch it. Otherwise, no HRDPA is available yet and only HRDPS forecast will be used.

      if (!is.null(hrdpa_loc)){ #Get the list of available files locally, if hrdpa_loc != NULL
        if(!dir.exists(hrdpa_loc)) { #Does the directory exist? If no, do you want to make it?
          create <- ask(paste0("The directory you pointed to (", hrdpa_loc, ") does not exist. Do you wish to create it?"))
          if (create) {
            dir.create(hrdpa_loc)
          } else stop("Directory will not be created. Try specifying the directory again.")
        }

        # check if the local files are labelled properly and if so, sufficient for the time span requested and of the correct clip.
        available_local <- data.frame(files = list.files(hrdpa_loc))
        available_local <- available_local %>%
          dplyr::mutate(timedate = stringr::str_extract(.data$files, "[0-9]{10}"),
                        clipped = grepl("clipped", .data$files),
                        extent = gsub("_", " ", stringr::str_extract_all(.data$files, "(?<=clipped_)(.*)(?=_HRDPA.)", simplify = TRUE))
          )

        available_local$timedate <- as.POSIXct(available_local$timedate, format = "%Y%m%d%H", tz="UTC")
        available_local <- available_local[!is.na(available_local$timedate),]

        #Match the time sequence_hrdpa to what's available, making sure the extent is ok. If not, call getHRDPA.
        missing_time <- sequence_hrdpa[which(!(sequence_hrdpa %in% available_local$timedate))] #if this is missing, then it doesn't matter if the extent is not matched!
        have_time <- available_local[which(available_local$timedate %in% sequence_hrdpa),]

        have_extent <- data.frame()
        for (i in within){ #Since within could actually be many polygons
          have_extent <- rbind(have_extent, have_time[which((stringr::str_detect(have_time$extent, i))),])
        }
        have_extent <- rbind(have_extent, have_time[have_time$clipped==FALSE,]) #add in the unclipped ones too

        missing_extent <- have_time[!(have_time$files %in% have_extent$files), ]

        #Check if have_time and/or have_extent are length 0, if they are, getHRDPA(clip) should get assigned the smallest clip polygon possible.
        if(length(missing_time) > 0){
          if(is.na(have_time$extent[1])){
            smallest <- as.data.frame(prov_buff[prov_buff$PREABBR %in% within,])
            smallest <- smallest[order(smallest$Shape_Area),][1,2]

            for (i in 1:length(missing_time)){
              getHRDPA(start = missing_time[i], end = missing_time[i], clip = smallest, save_path = hrdpa_loc)
            }
          } else {
            for (i in 1:length(missing_time)){
              getHRDPA(start = missing_time[i], end = missing_time[i], clip = unique(have_time$extent)[1], save_path = hrdpa_loc)
            }
          }
        }

        if (nrow(missing_extent) > 0){
          if (is.na(have_extent$extent[1])){
            smallest <- as.data.frame(prov_buff[prov_buff$PREABBR %in% within,])
            smallest <- smallest[order(smallest$Shape_Area),][1,2]

            for (i in 1:length(missing_extent)){
              getHRDPA(start = missing_extent[i,2], end = missing_extent[i,2], clip = smallest, save_path = hrdpa_loc)
            }
          } else {
            for (i in 1:length(missing_extent)){
              getHRDPA(start = missing_extent[i,2], end = missing_extent[i,2], clip = unique(have_extent$extent)[1], save_path = hrdpa_loc)
            }
          }
        }

        #finally, make the list of files to use: when a clipped and full file can both work, use either.
        available_local <- data.frame(files = list.files(hrdpa_loc))
        available_local <- available_local %>%
          dplyr::mutate(timedate = stringr::str_extract(.data$files, "[0-9]{10}")
          )
        available_local$timedate <- as.POSIXct(available_local$timedate, format = "%Y%m%d%H", tz="UTC")
        available_local <- available_local[!is.na(available_local$timedate),]

        hrdpa_files <- available_local[which(available_local$timedate %in% sequence_hrdpa),]
        hrdpa_files <- hrdpa_files[!duplicated(hrdpa_files$timedate), ]
        actual_times_hrdpa <- c(min(hrdpa_files$timedate)-60*60*6, max(hrdpa_files$time))
        hrdpa_files <- hrdpa_files$files

      } else { #hrdpa_loc is NULL, so pull from the web into a tempdir
        suppressWarnings(dir.create(paste0(tempdir(), "/HRDPA")))
        hrdpa_loc <- paste0(tempdir(), "/HRDPA")
        on.exit(unlink(hrdpa_loc))

        #compare the sequence_hrdpa against what exists in save_path, make list of files to dl.
        #check if the local files are labelled properly and if so, sufficient for the time span requested and of the correct clip.
        available_local <- data.frame(files = list.files(hrdpa_loc))
        available_local <- available_local %>%
          dplyr::mutate(timedate = stringr::str_extract(.data$files, "[0-9]{10}"),
                        clipped = grepl("clipped", .data$files),
                        extent = gsub("_", " ", stringr::str_extract_all(.data$files, "(?<=clipped_)(.*)(?=_HRDPA.)", simplify = TRUE))
          )

        available_local$timedate <- as.POSIXct(available_local$timedate, format = "%Y%m%d%H", tz="UTC")
        available_local <- available_local[!is.na(available_local$timedate),]

        #Match the sequence_hrdpa to what's available. Extent doesn't matter as anything here is temporary and will be full size anyways.
        missing_time <- sequence_hrdpa[which(!(sequence_hrdpa %in% available_local$timedate))] #if this is missing, then it doesn't matter if the extent is not matched!
        have_time <- available_local[which(available_local$timedate %in% sequence_hrdpa),]


        if(length(missing_time) > 0){
          for (i in 1:length(missing_time)){
            getHRDPA(start = missing_time[i], end = missing_time[i], clip = NULL, save_path = hrdpa_loc)
          }
        }

        #finally, make the list of files to use: when a clipped and full file can both work, use either.
        available_local <- data.frame(files = list.files(hrdpa_loc))
        available_local <- available_local %>%
          dplyr::mutate(timedate = stringr::str_extract(.data$files, "[0-9]{10}")
          )
        available_local$timedate <- as.POSIXct(available_local$timedate, format = "%Y%m%d%H", tz="UTC")
        available_local <- available_local[!is.na(available_local$timedate),]

        hrdpa_files <- available_local[which(available_local$timedate %in% sequence_hrdpa),]
        hrdpa_files <- hrdpa_files[!duplicated(hrdpa_files$timedate), ]
        actual_times_hrdpa <- c(min(hrdpa_files$timedate)-60*60*6, max(hrdpa_files$time))
        hrdpa_files <- hrdpa_files$files
      }
    } else {
      hrdpa <- FALSE
      actual_times_hrdpa <- NULL
    }
  }

  end_hrdps <- lubridate::floor_date(end, "1 hours")
  start_hrdps <- lubridate::floor_date(start, "1 hours")
  hrdps <- FALSE #overwritten if hrdps are usable
  actual_times_hrdps <- NULL
  if (hrdpa == FALSE){ #only hrdps need to be downloaded, times are in the future
    hrdps <- TRUE
    getHRDPS(clip = NULL, save_path = hrdps_loc, param = "APCP_Sfc") #This will not run through if the files are already present
    available_hrdps <- data.frame(files = list.files(paste0(hrdps_loc, "/APCP_Sfc"), pattern = ".*.tiff$", full.names=TRUE))
    available_hrdps <- available_hrdps %>%
      dplyr::mutate(from = paste0(stringr::str_extract(.data$files, "[0-9]{8}"), stringr::str_extract(.data$files, "T[0-9]{2}Z")),
                    time = as.numeric(substr(sub(".*Z_", "", .data$files), 1,2))
      )
    available_hrdps$from <- as.POSIXct(available_hrdps$from, format = "%Y%m%dT%HZ", tz="UTC")
    available_hrdps <- available_hrdps[!is.na(available_hrdps$from),]
    available_hrdps$to <- available_hrdps$from + available_hrdps$time*60*60
    past_precip <- available_hrdps[available_hrdps$to == start_hrdps,] #this is subtracted from the total hrdps to match with the requested start time
    end_precip <- available_hrdps[available_hrdps$to == end_hrdps,]
    if (nrow(end_precip) < 1) {
      end_precip <- available_hrdps[available_hrdps$to == max(available_hrdps$to), ]
    }
    actual_times_hrdps <- c(past_precip$to, end_precip$to)
    forecast_precip <- terra::rast(end_precip$files) - terra::rast(past_precip$files)
    forecast_precip <- terra::project(forecast_precip, "+proj=longlat +EPSG:3347")
    names(forecast_precip) <- "precip"
  } else if (hrdpa == TRUE & end > (Sys.time()-60*60*6)) { #There might be a need for some hrdps to fill in to the requested end time. Determine the difference between the actual end time and requested end time, fill in with hrdps if necessary
    getHRDPS(clip = NULL, save_path = hrdps_loc, param = "APCP_Sfc") #This will not run through if the files are already present
    available_hrdps <- data.frame(files = list.files(paste0(hrdps_loc, "/APCP_Sfc"), pattern = ".*.tiff$", full.names=TRUE))
    available_hrdps <- available_hrdps %>%
      dplyr::mutate(from = paste0(stringr::str_extract(.data$files, "[0-9]{8}"), stringr::str_extract(.data$files, "T[0-9]{2}Z")),
                    time = as.numeric(substr(sub(".*Z_", "", .data$files), 1,2))
      )
    available_hrdps$from <- as.POSIXct(available_hrdps$from, format = "%Y%m%dT%HZ", tz="UTC")
    available_hrdps <- available_hrdps[!is.na(available_hrdps$from),]
    available_hrdps$to <- available_hrdps$from + available_hrdps$time*60*60
    if (actual_times_hrdpa[2] %in% available_hrdps$from) { #if TRUE, the hrdps is suitable to use without special considerations
      past_precip <- available_hrdps[available_hrdps$to == actual_times_hrdpa[2], ]
      end_precip <- available_hrdps[available_hrdps$to == end_hrdps, ]
      if (nrow(end_precip) < 1) {
        end_precip <- available_hrdps[available_hrdps$to == max(available_hrdps$to), ]
      }
      if (!identical(past_precip, end_precip)){
        hrdps <- TRUE
        if (nrow(past_precip) == 0){
          actual_times_hrdps <- c(end_precip$from, end_precip$to)
          forecast_precip <- terra::rast(end_precip$files)
          forecast_precip <- terra::project(forecast_precip, "+proj=longlat +EPSG:3347")
          names(forecast_precip) <- "precip"
        } else {
          actual_times_hrdps <- c(end_precip$from, end_precip$to)
          forecast_precip <- terra::rast(end_precip$files) - terra::rast(past_precip$files)
          forecast_precip <- terra::project(forecast_precip, "+proj=longlat +EPSG:3347")
          names(forecast_precip) <- "precip"
        }
      }
    } else if ((actual_times_hrdpa[2] - 60*60*6) %in% available_hrdps$from) { #If this is true, it likely means that the requested time is *just* falling in the gap before the next available hrdps. Resolve the problem by using the latest actually available hrdps.
      past_precip <- available_hrdps[available_hrdps$to == actual_times_hrdpa[2], ]
      end_precip <- available_hrdps[available_hrdps$to == end_hrdps, ]
      if (nrow(end_precip) < 1) {
        end_precip <- available_hrdps[available_hrdps$to == max(available_hrdps$to), ]
      }
      if (!identical(past_precip, end_precip)){
        hrdps <- TRUE
        if (nrow(past_precip) == 0){
          actual_times_hrdps <- c(end_precip$from, end_precip$to)
          forecast_precip <- terra::rast(end_precip$files)
          forecast_precip <- terra::project(forecast_precip, "+proj=longlat +EPSG:3347")
          names(forecast_precip) <- "precip"
        } else {
          actual_times_hrdps <- c(end_precip$from, end_precip$to)
          forecast_precip <- terra::rast(end_precip$files) - terra::rast(past_precip$files)
          forecast_precip <- terra::project(forecast_precip, "+proj=longlat +EPSG:3347")
          names(forecast_precip) <- "precip"
        }
      }
    }
  }

  ###now the rasters are present for the extent and time required, finally! Proceed to accumulating them into a single raster.
  if (hrdpa == TRUE & hrdps == FALSE){
    if (length(hrdpa_files) > 1){
      hrdpa_rasters <- terra::sds(paste0(hrdpa_loc, "/", hrdpa_files))
      total <- hrdpa_rasters[1] #prepare to accumulate/add raster values
      for (i in 2:length(hrdpa_rasters)){
        total <- total + hrdpa_rasters[i]
      }
    } else {
      total <- terra::rast(paste0(hrdpa_loc, "/", hrdpa_files))
    }
    names(total) <- "precip"
    total <- terra::project(total, "+proj=longlat +EPSG:3347")
    actual_times <- actual_times_hrdpa
  }

  if (hrdpa == FALSE & hrdps == TRUE){
    total <- forecast_precip
    actual_times <- actual_times_hrdps
  }

  if (hrdpa == TRUE & hrdps == TRUE){
    #start with hrdpa
    if (length(hrdpa_files) > 1){
      hrdpa_rasters <- terra::sds(paste0(hrdpa_loc, "/", hrdpa_files))
      total_hrdpa <- hrdpa_rasters[1] #prepare to accumulate/add raster values
      for (i in 2:length(hrdpa_rasters)){
        total_hrdpa <- total_hrdpa + hrdpa_rasters[i]
      }
    } else {
      total_hrdpa <- terra::rast(paste0(hrdpa_loc, "/", hrdpa_files))
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
  for (i in 1:2){
    if (nchar(actual_times[i]) < 13){
      actual_times[i] <- paste0(actual_times[i], " 00:00")
    }
    if (nchar(actual_times[i]) > 16){
      actual_times[i] <- substr(actual_times[i], 1, 16)
    }
  }
  if (hrdpa){
    actual_times_hrdpa <- as.character(actual_times_hrdpa)
    for (i in 1:2){
      if (nchar(actual_times_hrdpa[i]) < 13){
        actual_times_hrdpa[i] <- paste0(actual_times_hrdpa[i], " 00:00")
      }
      if (nchar(actual_times_hrdpa[i]) > 16){
        actual_times_hrdpa[i] <- substr(actual_times_hrdpa[i], 1, 16)
      }
    }
  }
  if (hrdps){
    actual_times_hrdps <- as.character(actual_times_hrdps)
    for (i in 1:2){
      if (nchar(actual_times_hrdps[i]) < 13){
        actual_times_hrdps[i] <- paste0(actual_times_hrdps[i], " 00:00")
      }
      if (nchar(actual_times_hrdps[i]) > 16){
        actual_times_hrdps[i] <- substr(actual_times_hrdps[i], 1, 16)
      }
    }
  }

  #crop to the watershed or to the point
  cropped_precip <- terra::mask(total, location)
  cropped_precip <- terra::trim(cropped_precip)
  mean_precip <- as.data.frame(cropped_precip)
  mean_precip <- mean(mean_precip$precip)
  if (type != "longlat"){
    minmax_precip <- terra::minmax(cropped_precip)
    min <- minmax_precip[1]
    max <- minmax_precip[2]
  }

  ###Map the output if requested
  if (map == TRUE){
    if (type == "longlat"){ #Special treatment so we can return a watershed with the point for context
      WSC_polygons <- drainages
      WSC_polygons <- terra::project(WSC_polygons, "+proj=longlat +EPSG:3347")
      watershed <- terra::relate(location, WSC_polygons, relation = "within")
      watershed <- WSC_polygons[which(watershed)]
      if (length(watershed) > 0){
        smallest <- sort(watershed$Area_km2)[1]
        watershed <- terra::subset(watershed, watershed$Area_km2 == smallest) #This will be used to show the point with some context
        buff_size <- terra::expanse(watershed)*0.0000006
        if (buff_size < 2500) {
          buff_size <- 2500
        }
        watershed_buff <- terra::buffer(watershed, buff_size)
      } else { #The watershed cannot be found, so make a big buffer
        watershed_buff <- terra::buffer(location, 50000) #50km radius buffer
        #TODO: make this a rectangular/square extent instead to better use the display
        watershed <- watershed_buff
        watershed$StationNum <- "No watershed number"
        watershed$NameNom <- "Not a watershed"
      }
    } else { #"type" == WSC
      buff_size <- terra::expanse(location)*0.0000006
      if (buff_size < 2500) {
        buff_size <- 2500
      }
      watershed_buff <- terra::buffer(location, buff_size)
      watershed <- location
    }


    cropped_precip_rast <- terra::mask(total, watershed_buff)
    cropped_precip_rast <- terra::trim(cropped_precip_rast)

    #Load supporting layers and crop
    roads <- terra::vect(paste0(spatial_loc, "/roads.shp"))
    roads <- terra::project(roads, "+proj=longlat +EPSG:3347")

    if (terra::expanse(watershed) < 30000000000){
      streams <- terra::vect(paste0(spatial_loc, "/watercourses.shp"))
      streams <- terra::project(streams, "+proj=longlat +EPSG:3347")
      streams <- terra::mask(streams, watershed)
    }

    waterbodies <- terra::vect(paste0(spatial_loc, "/waterbodies.shp"))
    waterbodies <- terra::project(waterbodies, "+proj=longlat +EPSG:3347")
    waterbodies <- terra::mask(waterbodies, watershed) #even though masked, waterbody features that are even a little within the watershed polygon will be retained. This is nice as it brings out the major rivers for context.

    communities <- terra::vect(paste0(spatial_loc, "/communities.shp"))
    communities <- terra::project(communities, "+proj=longlat +EPSG:3347")

    borders <- terra::vect(paste0(spatial_loc, "/borders.shp"))
    borders <- terra::project(borders, "+proj=longlat +EPSG:3347")

    #And finally plot it all!
    terra::plot(cropped_precip_rast)
    terra::polys(watershed, border = "darkred")
    if (type == "longlat"){
      terra::points(location, pch=17, col="darkorchid1", cex=2)
    }
    terra::plot(roads, lwd = 1.5, alpha = 0.7, add=T)
    if (terra::expanse(watershed) < 30000000000){
      terra::plot(streams, lwd=0.08, col = "blue", border = NULL, alpha = 0.5, add=T)
    }
    terra::plot(waterbodies, col = "blue", border = "blue", alpha = 0.1, lwd=0.001, add=T)
    terra::points(communities, cex = 1.5)
    terra::lines(borders, lwd = 2, lty = "twodash")
    terra::text(communities, labels = communities$PLACE_NAME, pos=4, offset = 1, font=2, cex=0.9)
    if (type == "longlat"){
      terra::text(location, labels = paste0(requested_point[1], ", ", requested_point[2]), col = "black", pos=4, offset = 1, font=2)
    }
    graphics::mtext(paste0("Precipitation as mm of water equivalent from ", actual_times[1], " to ", actual_times[2], " UTC  \nWatershed: ", watershed$StationNum, ", ", stringr::str_to_title(watershed$NameNom), " "), side = 3, adj = 0.5)

    plot <- grDevices::recordPlot()

    # Code save to turn this into a leaflet app (much more rapid, zoomable, etc)
    # terra::plet(cropped_precip_rast, tiles = "Streets") %>%
    #   lines(waterbodies, alpha = 0.5, fill = "blue", col = "blue") %>%
    #   lines(streams, col = "blue", alpha = 0.5) %>%
    #   lines(watershed, col = "darkred")
    #   ...and what about a ggplotly object instead? Would require switching vectors to sf objects and the raster to a stars object.

  } else {
    watershed <- location
  }

  if (type == "longlat"){
    list <- list(mean_precip = mean_precip, total_time_range_UTC = actual_times, reanalysis_time_range_UTC = actual_times_hrdpa, forecast_time_range_UTC = actual_times_hrdps, point = requested_point, plot = if(map) plot else NULL)
    if (silent == FALSE){
      if (map == TRUE){
        cat("  \n  \n", crayon::blue(crayon::bold(crayon::underline(round(mean_precip, 2)))), " mm of rain or water equivalent ", if(hrdps == FALSE) "fell" else if (hrdps == TRUE & hrdpa == TRUE) "will have falllen" else if (hrdps == TRUE & hrdpa == FALSE) "will fall", " at your requested point (", requested_point[1], ", ", requested_point[2], ") between ", crayon::blue(crayon::bold(as.character(actual_times[1]), "and ", as.character(actual_times[2]), "UTC.")), "The smallest watershed for which I could find a polygon is ", watershed$StationNum, ", ", stringr::str_to_title(watershed$NameNom), "  \n  \nNOTES:  \n Your requested times have been adjusted to align with available data.\n", if(hrdps == FALSE) "Precipitation is based solely on retrospective-looking data produced by the HRDPA" else if (hrdps == TRUE & hrdpa == TRUE) "Precipitation is based on a mixture of retrospective-looking data from the HRDPA re-analysis and modelled precipitation using the HRDPS." else if (hrdps == TRUE & hrdpa == FALSE) "Precipitation is based on the HRDPS climate model outputs.", "  \n\n")
      } else {
        cat("  \n  \n", crayon::blue(crayon::bold(crayon::underline(round(mean_precip, 2)))), " mm of rain or water equivalent ", if(hrdps == FALSE) "fell" else if (hrdps == TRUE & hrdpa == TRUE) "will have falllen" else if (hrdps == TRUE & hrdpa == FALSE) "will fall", " at your requested point (", requested_point[1], ", ", requested_point[2], ") between ", crayon::blue(crayon::bold(as.character(actual_times[1]), "and ", as.character(actual_times[2]), "UTC.")), "  \n  \nNOTES:  \n Your requested times have been adjusted to align with available data.\n", if(hrdps == FALSE) "Precipitation is based solely on retrospective-looking data produced by the HRDPA" else if (hrdps == TRUE & hrdpa == TRUE) "Precipitation is based on a mixture of retrospective-looking data from the HRDPA re-analysis and modelled precipitation using the HRDPS." else if (hrdps == TRUE & hrdpa == FALSE) "Precipitation is based on the HRDPS climate model outputs.", "  \n\n")
      }
    }
  } else {
    list <- list(mean_precip = mean_precip, min = min, max = max, total_time_range_UTC = actual_times, reanalysis_time_range_UTC = actual_times_hrdpa, forecast_time_range_UTC = actual_times_hrdps, watershed = watershed$StationNum, plot = if(map) plot else NULL)
    if (silent == FALSE){
      cat("  \n  \nOn average,", crayon::blue(crayon::bold(crayon::underline(round(mean_precip, 2)))), " mm of rain or water equivalent (range:", round(min,2), "to", round(max,2), "mm)", if(hrdps == FALSE) "fell" else if (hrdps == TRUE & hrdpa == TRUE) "will have fallen" else if (hrdps == TRUE & hrdpa == FALSE) "will fall", "across the watershed requested (", watershed$StationNum, ", ", stringr::str_to_title(watershed$NameNom), ") between", crayon::blue(crayon::bold(as.character(actual_times[1]), "and", as.character(actual_times[2]), "UTC.")), "  \n  \nNOTES:  \n Your requested times have been adjusted to align with available data.\n", if(hrdps == FALSE) "Precipitation is based solely on retrospective-looking data produced by the HRDPA" else if (hrdps == TRUE & hrdpa == TRUE) "Precipitation is based on a mixture of retrospective-looking data from the HRDPA re-analysis and modelled precipitation using the HRDPS." else if (hrdps == TRUE & hrdpa == FALSE) "Precipitation is based on the HRDPS climate model outputs.", "  \n")
    }
  }
  return(list)
}
