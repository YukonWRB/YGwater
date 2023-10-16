#' Watershed/basin delineation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Hydro-processes a DEM, creating flow accumulation, direction, and streams rasters, and (optionally) delineates watersheds above one or more points using [Whitebox Tools](www.whiteboxgeo.com/). To facilitate this task in areas with poor quality/low resolution DEMs, can "burn-in" a stream network to the DEM to ensure proper stream placement (see details). Many time-consuming raster operations are performed, so the function will attempt to use existing rasters if they are present in the same path as the base DEM and named according to the function's naming conventions. In practice, this means that only the first run of the function needs to be very time consuming. See details for more information.
#'
#' NOTE 1: This tool can be slow to execute, and will use a lot of memory. Be patient, it might take several hours with a large DEM.
#'
#' NOTE 2: ESRI shapefiles, on which the Whitebox Tools functions depend, truncate column names to 10 characters. You may want to save and re-assign column names to the output {terra} object after this function has run.
#'
#' NOTE 3: If you are have already run this tool and are using a DEM in the same directory as last time, you only need to specify the DEM and the points (and, optionally, a projection for the points output). Operations using the optional streams shapefile and generating flow accumulation direction, and the artificial streams raster do not need to be repeated unless you want to use a different DEM or streams shapefile.
#'
#' NOTE 4: This function is very memory (RAM) intensive. You'll want at least 16GB of RAM, and to ensure that most of it is free. If you get an error such as 'cannot allocate xxxxx bytes', you probably don't have the resources to run the tool. The WhiteboxTool functions are memory hungry: all rasters are un-compressed and converted to 64-bit float type before starting work, and there needs to be room to store more than twice that uncompressed raster size in memory. Example: for the Yukon at a resolution of 16.9 meters (the highest resolution CDEM) the tool attempts to allocate 36GB of memory.
#'
#' @details
#' This function uses software from the Whitebox geospatial analysis package, built by Prof. John Lindsay. Refer to [this link](https://www.whiteboxgeo.com/manual/wbt_book/intro.html) for more information.
#'
#' ## Creating derived raster layers without defining watersheds
#' This function can be run without having any specific point above which to define a watershed. This can come in handy if you need to know where the synthetic streams raster will end up to ensure that your defined watershed pour points do not end up on the wrong stream branch, or if you simply want to front-load work while you work on defining the watershed pour points. To do this, leave the parameter 'points' and associated parameters as `NULL`.
#'
#' ## Explanation of process:
#' Starting from a supplied DEM, the function will fill single-cell pits, burn-in a stream network depression if requested (ensuring that flow accumulations happen in the correct location), breach depressions in the digital elevation model using a least-cost algorithm (i.e. using the pathway resulting in minimal changes to the DEM considering distance and elevation) then calculate flow accumulation and direction rasters. Then, a raster of streams is created where flow accumulation is greatest. The points provided by the user are then snapped to the derived streams raster and watersheds are computed using the flow direction rasters. Finally, the watershed/drainage basin polygons are saved to the specified save path along with the provided points and the snapped pour points.
#'
#' ## Using a streams shapefile to burn-in depressions to the DEM:
#' Be aware that this part of the function should ideally be used with a "simplified" streams shapefile. In particular, avoid or pre-process stream shapefiles that represent side-channels, as these will burn-in several parallel tracks to the DEM. ESRI has a tool called "simplify hydrology lines" which is great if you can ever get it to work, and WhiteboxTools has functions [whitebox::wbt_remove_short_streams()] to trim the streams raster, and [whitebox::wbt_repair_stream_vector_topology()] to help in converting a corrected streams vector to raster in the first place.
#'
#' @param DEM The path to a DEM including extension from which to delineate watersheds/catchments. Must be in .tif format. Derived layers such as flow accumulation, flow direction, and streams will inherit the DEM coordinate reference system.
#' @param streams Optionally, the path to the polylines shapefile containing streams, which can be used to improve accuracy when using poor quality DEMs. If this shapefile is the only input parameter being modified from previous runs (i.e. you've found a new/better streams shapefile but the DEM is unchanged) then speficy a shapefile here and overwrite = TRUE.
#' @param breach_dist The max radius (in raster cells) for which to search for a path to breach depressions, passed to [whitebox::wbt_breach_depressions_least_cost()]. This value should be high to ensure all depressions are breached. Note that the DEM is *not* breached in order of lowest elevation to greatest, nor is it breached sequentially (order is unknown, but the raster is presumably searched in some grid pattern for depressions). This means that flow paths may need to cross multiple depressions, especially in low relief areas.
#' @param threshold The accumulation threshold in DEM cells necessary to start defining a stream. This streams raster is necessary to snap pout points to, so make sure not to make this number too great!
#' @param overwrite If applicable, should rasters present in the same directory as the DEM be overwritten? This will also force the recalculation of derived layers.
#' @param points The path to the points shapefile (extension .shp) containing the points from which to build watersheds. The attribute of each point will be attached to the newly-created drainage polygons. Leave NULL (along with related parameters) to only process the DEM without defining watersheds.
#' @param points_name_col The name of the column in the points shapefile containing names to assign to the watersheds. Duplicates *are* allowed, and are labelled with the suffix _duplicate and a number for duplicates 2+.
#' @param projection Optionally, a projection string in the form "epsg:3579" (find them [here](https://epsg.io/)). The derived watersheds and point output layers will use this projection. If NULL the projection of the points will be used.
#' @param snap Snap to the "nearest" derived (calculated) stream, or to the "greatest" flow accumulation cell within the snap distance? Beware that "greatest" will move the point downstream by up to the 'snap_dist' specified, while nearest might snap to the wrong stream.
#' @param snap_dist The search radius within which to snap points to streams. Snapping method depends on 'snap' parameter. Note that distance units will match the projection, so probably best to work on a meter grid.
#' @param save_path The path where you want the output shapefiles saved. Default "choose" lets you choose interactively.
#' @param force_update_wbt Whitebox Tools is by default only downloaded if it cannot be found on the computer, and no check are performed to ensure the local version is current. Set to TRUE if you know that there is a new version and you would like to use it.
#'
#' @return A list of {terra} objects. If points are specified: delineated drainages, pour points as provided, snapped pour points, and the derived streams network. If no points: flow accumulation and direction rasters, and the derived streams network. If points specified, also saved to disk: an ESRI shapefile for each drainage basin, plus the associated snapped pour point and the point as provided and a shapefiles for all basins/points together. In all cases the created or discovered rasters will be in the same folder as the DEM.
#'
#' @seealso [WSC_drainages()] if looking for drainages associated with a WSC monitoring location.
#' @export

drainageBasins <- function(DEM, streams = NULL, breach_dist = 10000, threshold = 500, overwrite = FALSE, points = NULL, points_name_col = NULL, projection = NULL, snap = "nearest", snap_dist = 200, save_path = "choose", force_update_wbt = FALSE) {

  #initial checks
  rlang::check_installed("whitebox", reason = "Package whitebox is required to use function drainageBasins") #This is here because whitebox is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"

  if (!(snap %in% c("nearest", "greatest"))){
    stop("The parameter 'snap' must be one of 'nearest' or 'greatest'.")
  }
  if (save_path == "choose") {
    print("Select the output folder for shapefiles...")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  if (!file.exists(DEM)){
    stop("The DEM you pointed to does not exist. Perhaps your file path is wrong?")
  }
  directory <- dirname(DEM)
  input_DEM <- DEM #at this point DEM is a path, not an object
  if (!is.null(streams)){
    if (!file.exists(streams)){
      stop("The streams shapefile you pointed to does not exist. Perhaps your file path is wrong? Reminder, I'm looking for the .shp file with extension.")
    }
  }

  #Check whitebox existence and version, install if necessary or if force_update_wbt = TRUE.
  wbt_check <- whitebox::check_whitebox_binary()
  if (wbt_check){
    version <- invisible(utils::capture.output(whitebox::wbt_version()))
    print(paste0("Using WhiteboxTools version ", substr(version[1], 16, 20), ". If this is out of date, run function with force_update_wbt = TRUE."))
  } else {
    print("Installing WhiteboxTools binaries...")
    whitebox::wbt_install()
    version <- invisible(utils::capture.output(whitebox::wbt_version()))
    print(paste0("Installed WhiteboxTools version ", substr(version[1], 16, 20)))
  }
  if (force_update_wbt) {
    whitebox::wbt_install()
    version <- whitebox::wbt_version()
    print(paste0("Installed WhiteboxTools version ", substr(version[1], 16, 20), " (force update)."))
  }

  #change terra options
  old <- terra::terraOptions(print = FALSE)
  terra::terraOptions(memfrac = 0.9)
  on.exit(terra::terraOptions(memfrac = old$memfrac))

  if (!is.null(points)){
    points <- terra::vect(points) #load the points
    if (!(points_name_col %in% names(points))){
      stop("The column name you passed to parameter points_name_col does not appear to be in the points shapefile. If the column name had spaces, slashes, or other problematic characters, it might have been modified upon reading it in. To see what R thinks the column names are you could load the layer using names(terra::vect('path_to_your_shp')).")
    }
    if (is.na(terra::crs(points))){
      stop("The points shapefile does not have a coordinate reference system specified. Please fix this issue and try again.")
    }

    original_projection <- paste0("epsg:", terra::crs(points, describe=TRUE)$code)
    DEM <- terra::rast(DEM) #load the DEM to R environment
    points <- terra::project(points, DEM)
    suppressWarnings(dir.create(paste0(tempdir(), "/temp_inputs")))
    terra::writeVector(points, paste0(tempdir(), "/temp_inputs/points.shp"), overwrite=TRUE)
  }

  #Check for existence of final layers and that their extents match the provided DEM. Create them if not exist or different extent.
  d8pntr_exists <- FALSE
  streams_derived_exists <- FALSE
  d8fac_exists <- FALSE
  if (!is.null(streams) | !overwrite){ #no point in checking the derived rasters if a new streams layer is specified or if we're overwriting anyways
    print("Checking if the right layers already exist...")
    if (file.exists(paste0(directory, "/D8pointer.tif")) & !overwrite){
      d8pntr <- terra::rast(paste0(directory, "/D8pointer.tif"))
      if (terra::compareGeom(d8pntr, DEM)){
        d8pntr_exists <- TRUE
      }
    }
    if (file.exists(paste0(directory, "/streams_derived.tif")) & d8pntr_exists & !overwrite){
      streams_derived <- terra::rast(paste0(directory, "/streams_derived.tif"))
      if (terra::compareGeom(streams_derived, DEM)){
        streams_derived_exists <- TRUE
      }
    }
    if (snap == "greatest"){
      if (file.exists(paste0(directory, "/D8fac.tif"))){
        d8fac <- terra::rast(paste0(directory, "/D8fac.tif"))
        if (terra::compareGeom(d8fac, DEM)){
          d8fac_exists <- TRUE
        }
      }
    } else {
      d8fac_exists <- TRUE
    }
  }

  if (!streams_derived_exists | !d8pntr_exists | !d8fac_exists | overwrite){
    print("Caculating layers derived from the DEM as they are either missing, have different extents as the provided DEM, you've requested an overwrite of calculated layers, or you specified a streams shapefile.")

    print("Filling single-cell pits in the DEM...")
    whitebox::wbt_fill_single_cell_pits(dem = input_DEM,
                                        output = paste0(directory, "/filled_single_cells.tif"))

    if (!is.null(streams)){ #load streams, process to raster, and burn-in the DEM
      print("Creating a stream raster from the provided stream shapefile...")
      streams_input <- terra::vect(streams)
      streams_input <- terra::project(streams_input, DEM)
      streams_input <- terra::rasterize(streams_input, DEM, touches = TRUE, filename = paste0(tempdir(), "/temp_inputs/streams_input_rasterized.tif"), overwrite=TRUE) #Make raster stream network. Background has values NA. Write to disk to avoid memory restrictions.
      streams_input <- (streams_input/streams_input) * 20 #Make each cell value = 20 to later burn in a 20 meter depression
      streams_input <- terra::subst(streams_input, NA, 0) #replace background NAs with 0 so that it subtracts (nothing) from the DEM later; subtracting NA results in NA cells.
      print("Creating depressions in the DEM where streams should be...")
      filled_single_cells <- terra::rast(paste0(directory, "/filled_single_cells.tif"))
      DEM_burned <- filled_single_cells - streams_input #burn-in the DEM
      terra::writeRaster(DEM_burned, paste0(directory, "/DEM_burned.tif"), overwrite = TRUE)
    }

    print("Breaching depressions in the DEM to ensure continuous flow paths...")
    whitebox::wbt_breach_depressions_least_cost(
      dem = if (!is.null(streams)) paste0(directory, "/DEM_burned.tif") else paste0 (directory, "/filled_single_cells.tif"),
      output = paste0(directory, "/FilledDEM.tif"),
      dist = breach_dist,
      fill = TRUE,
      flat_increment = 0.0001)

    print("Calculating a flow accumulation raster...")
    whitebox::wbt_d8_flow_accumulation(input = paste0(directory, "/FilledDEM.tif"),
                                       output = paste0(directory, "/D8fac.tif"))
    d8fac <- terra::rast(paste0(directory, "/D8fac.tif"))

    print("Calculating a flow directions raster...")
    whitebox::wbt_d8_pointer(dem = paste0(directory, "/FilledDEM.tif"),
                             output = paste0(directory, "/D8pointer.tif"))
    d8pntr <- terra::rast(paste0(directory, "/D8pointer.tif"))

    # Make a raster of streams only from the DEM, with a threshold (in cells) for flow accumulation
    print("Creating a raster of streams based on the flow accumulation raster...")
    whitebox::wbt_extract_streams(flow_accum = paste0(directory, "/D8fac.tif"),
                                  output = paste0(directory, "/streams_derived.tif"),
                                  threshold = threshold)
    streams_derived <- terra::rast(paste0(directory, "/streams_derived.tif"))
  } else {
    if (!is.null(streams)){
      print("Using pre-calculated derived layers for basin delineation. NOTE: you specified a streams shapefile which won't be used. If you want to incorporate it run this function again with overwrite = TRUE.")
    } else {
      print("Using pre-calculated derived layers for basin delineation. If you are trying to change the streams threshold you must specify overwrite = TRUE for this to take effect.")
    }
  }

  #Now snap the points and delineate watersheds, returning polygons.
  if(!is.null(points)){
    print("Snapping points according to the parameters selected...")
    suppressWarnings(dir.create(paste0(tempdir(), "/shapefiles")))
    unlink(list.files(paste0(tempdir(), "/shapefiles"), full.names=TRUE))
    if (snap == "nearest"){
      whitebox::wbt_jenson_snap_pour_points(pour_pts = paste0(tempdir(), "/temp_inputs/points.shp"),
                                            streams = paste0(directory, "/streams_derived.tif"),
                                            output = paste0(tempdir(), "/shapefiles/snapped_points.shp"),
                                            snap_dist = snap_dist)
    } else if (snap == "greatest"){
      whitebox::wbt_snap_pour_points(pour_pts = paste0(tempdir(), "/temp_inputs/points.shp"),
                                     flow_accum = paste0(directory, "/D8fac.tif"),
                                     output = paste0(tempdir(), "/shapefiles/snapped_points.shp"),
                                     snap_dist = snap_dist)
    }

    snapped_points <- terra::vect(paste0(tempdir(), "/shapefiles/snapped_points.shp")) #load to memory so as to iterate over each point, allowing for looping. Otherwise the tool creates non-overlapping rasters.
    suppressWarnings(dir.create(paste0(tempdir(), "/rasters"))) #watershed tool outputs rasters (it works off a grid), but polygons are desired output. These are not saved.
    unlink(list.files(paste0(tempdir(), "/rasters"), full.names=TRUE)) #ensure clear dir for repeat runs in same session
    suppressWarnings(dir.create(paste0(save_path, "/watersheds_", Sys.Date()))) #The desired outputs will go here
    unlink(list.files(paste0(save_path, "/watersheds_", Sys.Date()), full.names = TRUE), recursive = TRUE)
    count <- 0 #For 'together' shapefiles. Need a feature to create the R object, then features can be appended.
    failed <- character()
    cat(crayon::blue$bold("\n  Starting watershed delineation. This can take a long time so get yourself a tea/coffee.  \n"))
    for(i in 1:nrow(snapped_points)) {
      print(paste0("Delineating drainage basin for point ", as.data.frame(snapped_points[i, points_name_col]), " (", i, " of ", nrow(snapped_points), ")"))
      tryCatch({
        terra::writeVector(snapped_points[i, ], paste0(tempdir(), "/shapefiles/", i,".shp"), overwrite=TRUE)
        invisible(utils::capture.output(whitebox::wbt_watershed(d8_pntr = paste0(directory, "/D8pointer.tif"),
                                                                pour_pts = paste0(tempdir(), "/shapefiles/", i, ".shp"),
                                                                output = paste0(tempdir(), "/rasters/", i, ".tif")
        )))

        rast <- terra::rast(paste0(tempdir(), "/rasters/", i, ".tif"))
        poly <- terra::as.polygons(rast)
        if (!is.null(projection)){ #project if called for, otherwise restore original projection
          poly <- terra::project(poly, projection)
          snapped_pt <- snapped_points[i, ]
          snapped_pt <- terra::project(snapped_pt, projection)
          point <- points[i, ]
          point <- terra::project(point, projection)
        } else {
          poly <- terra::project(poly, original_projection)
          snapped_pt <- snapped_points[i, ]
          snapped_pt <- terra::project(snapped_pt, original_projection)
          point <- points[i, ]
          point <- terra::project(point, original_projection)
        }
        poly <- cbind(poly, as.data.frame(point)) #attach the point attributes to the polygon
        poly <- poly[, -1] #Remove a pesky identifier column that is now superfluous

        #and now that everything worked, create the directories and populate them.
        folder_name <- as.data.frame(snapped_pt[1, points_name_col])
        tryCatch({ #sometimes the identifier row is repeated (example duplicates, identified in another col). These are identified with a suffix.
          dir.create(paste0(save_path, "/watersheds_", Sys.Date(), "/", folder_name))
          save_watershed <- paste0(save_path, "/watersheds_", Sys.Date(), "/", folder_name)
        }, warning = function(w) {
          files <- list.files(paste0(save_path, "/watersheds_", Sys.Date(), "/"), pattern = paste0(folder_name, "*_duplicate"))
          nums <- integer()
          for (i in files){
            num <- sub(".*[^0-9]([0-9]{1,5})$", "\\1", i)
            if (grepl("[0-9]$", num)){
              nums <- c(nums, as.integer(num))
            }
          }
          dir.create(paste0(save_path, "/watersheds_", Sys.Date(), "/", folder_name, "_duplicate", if (length(nums) > 0) max(nums)+1 else ""))
          save_watershed <<- paste0(save_path, "/watersheds_", Sys.Date(), "/", as.data.frame(snapped_pt[1, points_name_col]), "_duplicate", if (length(nums) > 0) max(nums)+1 else "")
          folder_name <<- paste0(folder_name, "_duplicate", if (length(nums) > 0) max(nums)+1 else "")
        })

        terra::writeVector(snapped_pt, paste0(save_watershed, "/", folder_name, "_snapped_pour_point.shp"), overwrite=TRUE)
        terra::writeVector(point, paste0(save_watershed, "/", folder_name, "_input_point.shp"), overwrite=TRUE)
        terra::writeVector(poly, paste0(save_watershed, "/", folder_name, "_drainage_basin.shp"), overwrite=TRUE)

        #now create/append to the larger shapefiles
        if (count == 0){
          output_basins <- poly
          input_points <- point
          snapped_pts <- snapped_pt
          count <- 1
        } else {
          output_basins <- rbind(output_basins, poly)
          input_points <- rbind(input_points, point)
          snapped_pts <- rbind(snapped_pts, snapped_pt)
        }
        cat(crayon::blue("Success!  \n"))
      }, error = function(e) {
        cat(crayon::red(paste0("Failed to delineate watershed for point named ", as.data.frame(snapped_points[i, points_name_col]), "  \n")))
        failed <- c(failed, as.data.frame(snapped_points[i, points_name_col]))
      })
    }
    #Save the larger shapefiles to disc
    suppressWarnings(dir.create(paste0(save_path, "/watersheds_", Sys.Date(), "/")))
    terra::writeVector(output_basins, paste0(save_path, "/watersheds_", Sys.Date(), "/drainage_basins.shp"), overwrite=TRUE)
    terra::writeVector(input_points, paste0(save_path, "/watersheds_", Sys.Date(), "/input_points.shp"), overwrite=TRUE)
    terra::writeVector(snapped_points, paste0(save_path, "/watersheds_", Sys.Date(), "/snapped_points.shp"), overwrite=TRUE)

    if (length(failed) == nrow(snapped_points)){
      cat(crayon::red$bold("Failed to delineate all points. Re-check your inputs carefully, and if the issue persists troubleshoot by running the function line by line."))
    }
    if (length(failed) > 0){
      cat(crayon::red$bold(paste0("Failed to delineate points ", paste(failed, collapse = ", "), ".")))
    }

    result <- list(delineated_basins = output_basins, input_points = input_points, snapped_points = snapped_points, streams_derived = streams_derived)
    cat(crayon::blue$bold("Function complete: watersheds, points, and derived streams are returned and are saved to disk."))
  } else {
    result <- list(streams_derived = streams_derived, d8_flow_accumulation = d8fac, d8_flow_direction = d8pntr)
    cat(crayon::blue$bold("Function complete: derived flow accumulation, direction, and streams rasters are returned and saved to disk."))
  }

  return(result)

} #End of function
