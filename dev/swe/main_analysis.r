# ============================================================================
# SWE Analysis Script - ERA5 Land Reanalysis Processing
# ============================================================================
# Author: [Author Name]
# Created: [Date]
# Description: Processes ERA5 Land reanalysis data for Snow Water Equivalent
#              analysis across different basins, generates maps and statistics

# Load Required Libraries ----
library(AquaCache)
library(YGwater)
library(DBI)
library(plotly)
library(leaflet)
library(sf)
library(terra)
library(tools)

# Configuration ----
get_db_config <- function() {
    list(
        dbName = "aquacache",
        dbHost = Sys.getenv("aquacacheHostProd"),
        dbPort = Sys.getenv("aquacachePortProd"),
        dbUser = Sys.getenv("aquacacheUserProd"),
        dbPass = Sys.getenv("aquacachePassProd")
    )
}

# Database Connection ----
connect_to_database <- function(config = get_db_config()) {
    AquaConnect(
        name = config$dbName,
        host = config$dbHost,
        port = config$dbPort,
        user = config$dbUser,
        pass = config$dbPass
    )
}

# Database Helper Functions ----

#' Download spatial layer from database
#' @param con Database connection
#' @param layer_name Name of the spatial layer
#' @param additional_query Additional SQL query conditions
#' @return sf object with spatial data
download_spatial_layer <- function(con, layer_name, additional_query = NULL) {
    query <- sprintf(
        "SELECT *, ST_AsText(ST_Transform(geom, 4326)) as geom_4326 
         FROM spatial.vectors 
         WHERE layer_name = %s",
        DBI::dbQuoteString(con, layer_name)
    )

    if (!is.null(additional_query) && nzchar(additional_query)) {
        query <- paste(query, additional_query)
    }

    data <- DBI::dbGetQuery(con, query)
    if (nrow(data) == 0) {
        warning(sprintf("No data found for layer: %s", layer_name))
        return(NULL)
    }

    geom <- sf::st_as_sfc(data$geom_4326, crs = 4326)
    sf::st_sf(data, geometry = geom, crs = 4326)
}

#' Get ERA5 historical raster data for specified bounding box and date range
#' @param con Database connection
#' @param bbox Bounding box (xmin, ymin, xmax, ymax)
#' @param query_date Query date (for day-of-year filtering)
#' @param historical_start_year Start year for historical data (NULL for single year)
#' @param historical_end_year End year for historical data (or single year if start is NULL)
#' @return Data frame containing historical raster metadata
get_era5_historical_data <- function(
    con,
    bbox,
    query_date,
    historical_start_year,
    historical_end_year
) {
    xmin <- bbox["xmin"]
    xmax <- bbox["xmax"]
    ymin <- bbox["ymin"]
    ymax <- bbox["ymax"]

    print(sprintf(
        "Bounding box for raster query: xmin=%f, ymin=%f, xmax=%f, ymax=%f",
        xmin,
        ymin,
        xmax,
        ymax
    ))

    era5_query <- sprintf(
        "
        SELECT 
            r.reference_id,
            rr.valid_from as datetime
        FROM spatial.raster_series_index rsi
        JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
        JOIN spatial.rasters r ON r.reference_id = rr.reference_id
        WHERE rsi.model = 'ERA5_land'
        AND ST_Intersects(
            r.rast,
            ST_MakeEnvelope(%f, %f, %f, %f, 4326)
        )
        ORDER BY rr.valid_from, r.reference_id
        ",
        xmin,
        ymin,
        xmax,
        ymax
    )

    era5_raster_data <- DBI::dbGetQuery(con, era5_query)
    era5_raster_data$datetime <- as.POSIXct(era5_raster_data$datetime)

    # Filter by day of year and date range
    query_day_of_year <- as.numeric(format(query_date, "%j"))
    era5_raster_data$day_of_year <- as.numeric(format(
        era5_raster_data$datetime,
        "%j"
    ))
    era5_raster_data <- era5_raster_data[
        era5_raster_data$day_of_year == query_day_of_year,
    ]
    era5_raster_data$day_of_year <- NULL

    # Filter to historical years
    era5_raster_data$year <- as.numeric(format(era5_raster_data$datetime, "%Y"))

    if (is.null(historical_start_year)) {
        # Use only single year (historical_end_year)
        era5_raster_data <- era5_raster_data[
            era5_raster_data$year == historical_end_year,
        ]
        cat("Using single historical year:", historical_end_year, "\n")
    } else {
        # Use year range
        era5_raster_data <- era5_raster_data[
            era5_raster_data$year >= historical_start_year &
                era5_raster_data$year <= historical_end_year,
        ]
        cat(
            "Using historical year range:",
            historical_start_year,
            "to",
            historical_end_year,
            "\n"
        )
    }

    era5_raster_data$year <- NULL

    return(era5_raster_data)
}

#' Get ERA5 current year raster data for specified bounding box and date
#' @param con Database connection
#' @param bbox Bounding box (xmin, ymin, xmax, ymax)
#' @param query_date Query date
#' @return Data frame containing current year raster metadata
get_era5_current_data <- function(con, bbox, query_date) {
    xmin <- bbox["xmin"]
    xmax <- bbox["xmax"]
    ymin <- bbox["ymin"]
    ymax <- bbox["ymax"]

    era5_query <- sprintf(
        "
        SELECT 
            r.reference_id,
            rr.valid_from as datetime
        FROM spatial.raster_series_index rsi
        JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
        JOIN spatial.rasters r ON r.reference_id = rr.reference_id
        WHERE rsi.model = 'ERA5_land'
        AND ST_Intersects(
            r.rast,
            ST_MakeEnvelope(%f, %f, %f, %f, 4326)
        )
        AND DATE(rr.valid_from) = '%s'
        ORDER BY rr.valid_from, r.reference_id
        ",
        xmin,
        ymin,
        xmax,
        ymax,
        format(query_date, "%Y-%m-%d")
    )

    era5_current_data <- DBI::dbGetQuery(con, era5_query)
    era5_current_data$datetime <- as.POSIXct(era5_current_data$datetime)

    cat(
        "Found",
        nrow(era5_current_data),
        "current raster records for",
        format(query_date, "%Y-%m-%d"),
        "\n"
    )

    return(era5_current_data)
}

#' Retrieve raster objects from database
#' @param con Database connection
#' @param era5_raster_data Data frame with reference_ids
#' @param data_type Character string describing data type for logging
#' @return List of raster objects
retrieve_era5_rasters <- function(con, era5_raster_data, data_type = "raster") {
    era5_rasters <- list()
    for (i in 1:nrow(era5_raster_data)) {
        rid <- era5_raster_data$reference_id[i]
        rast <- YGwater::getRaster(
            con = con,
            clauses = paste0("WHERE reference_id = ", rid)
        )
        era5_rasters[[i]] <- rast
        cat(
            "Retrieved",
            data_type,
            i,
            "of",
            nrow(era5_raster_data),
            "for reference_id:",
            rid,
            "\n"
        )
    }
    return(era5_rasters)
}

#' Get the latest ERA5 data date from database
#' @param con Database connection
#' @return Date of most recent ERA5 data
get_latest_era5_date <- function(con) {
    era5_query <- sprintf(
        "
        SELECT 
            r.reference_id,
            rr.valid_from as datetime
        FROM spatial.raster_series_index rsi
        JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
        JOIN spatial.rasters r ON r.reference_id = rr.reference_id
        WHERE rsi.model = 'ERA5_land'
        ORDER BY rr.valid_from DESC, r.reference_id DESC
        LIMIT 1
        "
    )
    DBI::dbGetQuery(con, era5_query)$datetime
}

# Geometry and Plotting Helper Functions ----

#' Get plot limits from geometry with buffer
#' @param geom sf geometry object
#' @param buffer_m Buffer in meters
#' @return List with xlim and ylim
get_lims_from_geom <- function(geom, buffer_m = 5000) {
    geom_bbox <- sf::st_bbox(geom)

    # Convert to degrees (approximate conversion at this latitude)
    lat_center <- mean(c(geom_bbox["ymin"], geom_bbox["ymax"]))
    m_to_deg_lon <- 1 / (111320 * cos(lat_center * pi / 180))
    m_to_deg_lat <- 1 / 110540

    buffer_deg_lon <- buffer_m * m_to_deg_lon
    buffer_deg_lat <- buffer_m * m_to_deg_lat

    # Apply buffer to bounding box
    xlim <- c(
        geom_bbox["xmin"] - buffer_deg_lon,
        geom_bbox["xmax"] + buffer_deg_lon
    )
    ylim <- c(
        geom_bbox["ymin"] - buffer_deg_lat,
        geom_bbox["ymax"] + buffer_deg_lat
    )

    return(list(xlim = xlim, ylim = ylim))
}

#' Add scale bar to plot
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param bar_length_km Length of scale bar in kilometers
add_scale_bar <- function(xlim, ylim, bar_length_km = 20) {
    lon_range <- diff(xlim)
    lat_range <- diff(ylim)
    lat_center <- mean(ylim)
    km_per_deg_lon <- 111.32 * cos(lat_center * pi / 180)

    bar_length_deg <- bar_length_km / km_per_deg_lon

    x_start <- xlim[1] + 0.02 * lon_range
    x_end <- x_start + bar_length_deg
    y_pos <- ylim[1] + 0.03 * lat_range

    segments(x_start, y_pos, x_end, y_pos, lwd = 4, col = "black")
    text(
        mean(c(x_start, x_end)),
        y_pos - 0.01 * lat_range,
        paste(bar_length_km, "km"),
        cex = 1.3,
        adj = c(0.5, 1)
    )
}

#' Process ERA5 raster data separately for historical and current
#' @param historical_rasters List of historical raster objects
#' @param current_raster Single current raster object
#' @param historical_data Metadata for historical rasters
#' @param current_data Metadata for current raster
#' @param summary_stat Summary statistic to use ('median' or 'mean')
#' @param statistic Type of statistic to calculate ('relative', 'difference', 'percentile')
#' @return List containing processed rasters and time vectors
process_era5_rasters <- function(
    historical_rasters,
    current_raster,
    historical_data,
    current_data,
    summary_stat = "median",
    statistic = "relative"
) {
    # Process historical rasters
    historical_stack <- do.call(c, historical_rasters)
    historical_time_vector <- historical_data$datetime

    # Current raster is separate
    era5_recent <- current_raster[[1]] # Extract from single-item list
    current_time_vector <- current_data$datetime

    # Validate inputs
    if (!summary_stat %in% c("median", "mean")) {
        stop("summary_stat must be 'median' or 'mean'")
    }
    if (!statistic %in% c("relative", "difference", "percentile")) {
        stop("statistic must be 'relative', 'difference', or 'percentile'")
    }

    if (statistic %in% c("relative", "difference")) {
        # Calculate summary statistic across historical time dimension
        summary_fun <- switch(summary_stat, "median" = median, "mean" = mean)
        era5_summary <- terra::app(
            historical_stack,
            fun = summary_fun,
            na.rm = TRUE
        )

        # Calculate the requested statistic
        if (statistic == "relative") {
            # Calculate relative change: current divided by historical summary (as percentage)
            era5_result <- 100 * era5_recent / era5_summary
        } else if (statistic == "difference") {
            # Calculate absolute difference: current minus historical summary
            era5_result <- era5_recent - era5_summary
        }

        return(list(
            historical_stack = historical_stack,
            era5_summary = era5_summary,
            era5_recent = era5_recent,
            era5_result = era5_result,
            historical_time_vector = historical_time_vector,
            current_time_vector = current_time_vector,
            summary_stat = summary_stat,
            statistic = statistic
        ))
    } else if (statistic == "percentile") {
        # Calculate percentile: what percentile does current value represent in historical distribution
        cat(
            "Calculating percentiles for current values in historical distribution...\n"
        )

        # Create function to calculate percentile for each pixel
        calculate_percentile <- function(x) {
            if (all(is.na(x))) {
                return(NA)
            }
            current_val <- x[length(x)] # Last value is current
            historical_vals <- x[-length(x)] # All but last are historical

            if (is.na(current_val) || all(is.na(historical_vals))) {
                return(NA)
            }

            # Calculate percentile rank
            percentile <- sum(historical_vals <= current_val, na.rm = TRUE) /
                sum(!is.na(historical_vals)) *
                100
            return(percentile)
        }

        # Add current raster as additional layer to historical stack for percentile calculation
        stack_with_current <- c(historical_stack, era5_recent)
        era5_result <- terra::app(
            stack_with_current,
            fun = calculate_percentile
        )

        return(list(
            historical_stack = historical_stack,
            era5_recent = era5_recent,
            era5_result = era5_result,
            historical_time_vector = historical_time_vector,
            current_time_vector = current_time_vector,
            summary_stat = summary_stat,
            statistic = statistic
        ))
    }
}

# Main Analysis Functions ----

#' Generate SWE analysis map for a single basin
#' @param basin_shp sf object representing basin geometry
#' @param filename Output filename for the plot
#' @param era5_result Result raster (relative change, difference, or percentile)
#' @param communities sf object with communities data
#' @param crs Coordinate reference system
#' @param upsample_factor Factor for raster upsampling
#' @param historical_start_year Start year for historical period
#' @param historical_end_year End year for historical period
#' @param query_date Date of analysis
#' @param processed_rasters Processed raster results containing summary_stat and statistic
#' @return List with analysis results
generate_basin_swe_map <- function(
    basin_shp,
    filename,
    era5_result,
    communities,
    crs,
    upsample_factor,
    historical_start_year,
    historical_end_year,
    query_date,
    processed_rasters
) {
    selected_basin <- sf::st_transform(basin_shp, crs = crs)

    # Upsample raster to higher resolution
    raster_upsampled <- terra::disagg(
        era5_result,
        fact = upsample_factor,
        method = "bilinear"
    )

    # Clip (crop and mask) the upsampled raster by the polygon
    raster_clipped <- terra::crop(raster_upsampled, selected_basin)
    raster_clipped <- terra::mask(raster_clipped, selected_basin)

    # Get mean raster value within the polygon
    mean_value <- terra::global(raster_clipped, fun = "mean", na.rm = TRUE)$mean
    cat("Mean raster value within polygon:", mean_value, "\n")

    # Get plot limits
    lims <- get_lims_from_geom(selected_basin, buffer_m = 5000)
    xlim <- lims$xlim
    ylim <- lims$ylim

    # Generate plot
    png(filename = filename, width = 1200, height = 1050)
    old_par <- par(mar = c(11, 10, 4, 2) + 0.1)

    # Create period label based on whether we have a range or single year
    if (is.null(historical_start_year)) {
        period_label <- sprintf("(%d)", historical_end_year)
    } else {
        period_label <- sprintf(
            "(%d-%d)",
            historical_start_year,
            historical_end_year
        )
    }

    # Set plot parameters based on statistic type
    statistic_type <- processed_rasters$statistic

    if (statistic_type == "relative") {
        main_title <- paste(
            "ERA5 SWE Relative to historic",
            processed_rasters$summary_stat,
            period_label,
            "on",
            format(query_date, "%Y-%m-%d"),
            sprintf("(Mean in basin: %.1f%%)", mean_value)
        )
        range_vals <- c(0, 200)
        colors <- colorRampPalette(c("red", "grey", "blue"))(100)
    } else if (statistic_type == "percentile") {
        main_title <- paste(
            "ERA5 SWE Percentile rank vs historic",
            period_label,
            "on",
            format(query_date, "%Y-%m-%d"),
            sprintf("(Mean in basin: %.1f%%ile)", mean_value)
        )
        range_vals <- c(0, 100)
        colors <- colorRampPalette(c("red", "yellow", "green", "blue"))(100)
    } else if (statistic_type == "difference") {
        main_title <- paste(
            "ERA5 SWE Difference from historic",
            processed_rasters$summary_stat,
            period_label,
            "on",
            format(query_date, "%Y-%m-%d"),
            sprintf("(Mean in basin: %.2f mm)", mean_value)
        )
        range_vals <- c(-50, 50) # Adjust range as needed
        colors <- colorRampPalette(c("red", "white", "blue"))(100)
    }

    terra::plot(
        raster_upsampled,
        main = main_title,
        range = range_vals,
        xlim = xlim,
        ylim = ylim,
        col = colors,
        legend = TRUE,
        pax = list(cex.axis = 1.4, cex.lab = 1.8),
        plg = list(cex = 2)
    )

    # Add scale bar
    add_scale_bar(xlim, ylim)

    # Add basin and communities
    plot(selected_basin$geometry, border = "darkblue", lwd = 2, add = TRUE)
    plot(communities$geometry, pch = 16, bg = "orange", cex = 2.5, add = TRUE)
    text(
        sf::st_coordinates(communities),
        labels = communities$feature_name,
        pos = 1,
        cex = 1.2
    )

    par(old_par)
    dev.off()

    return(list(
        selected_basin = selected_basin,
        mean_value = mean_value,
        filename = filename,
        raster_clipped = raster_clipped
    ))
}

#' Main SWE analysis function
#' @param basin_shp_list List of sf objects representing basin geometries
#' @param filenames Vector of output filenames
#' @param con Database connection
#' @param crs Coordinate reference system (default: 4326)
#' @param historical_start_year Start year for historical data (NULL for single year, default: 1991)
#' @param historical_end_year End year for historical data (or single year if start is NULL, default: 2020)
#' @param query_date Date for analysis (default: latest available)
#' @param upsample_factor Raster upsampling factor (default: 16)
#' @param summary_stat Summary statistic for historical data ('median' or 'mean', default: 'median')
#' @param statistic Type of statistic to calculate ('relative', 'difference', 'percentile', default: 'relative')
#' @return List of analysis results for each basin
run_swe_analysis <- function(
    basin_shp_list,
    filenames,
    con,
    crs = 4326,
    historical_start_year = 1991,
    historical_end_year = 2020,
    query_date = NULL,
    upsample_factor = 16,
    summary_stat = "median",
    statistic = "relative"
) {
    # Get query date if not provided
    if (is.null(query_date)) {
        query_date <- as.Date(get_latest_era5_date(con))
    }

    # Download communities data
    communities <- download_spatial_layer(con, "Communities")

    # Compute bounding box for all basins
    bbox <- sf::st_bbox(
        sf::st_transform(
            do.call(sf::st_union, basin_shp_list),
            crs = 4326
        )
    )

    # Get ERA5 raster data separately for historical and current
    historical_data <- get_era5_historical_data(
        con,
        bbox,
        query_date,
        historical_start_year,
        historical_end_year
    )
    current_data <- get_era5_current_data(con, bbox, query_date)

    # Check if we have data
    if (nrow(historical_data) == 0) {
        stop(
            "No historical ERA5 data found for the specified date and time period"
        )
    }
    if (nrow(current_data) == 0) {
        stop("No current ERA5 data found for the specified date")
    }

    # Retrieve raster objects separately
    historical_rasters <- retrieve_era5_rasters(
        con,
        historical_data,
        "historical raster"
    )
    current_raster <- retrieve_era5_rasters(con, current_data, "current raster")

    # Process rasters
    processed_rasters <- process_era5_rasters(
        historical_rasters,
        current_raster,
        historical_data,
        current_data,
        summary_stat,
        statistic
    )

    # Generate maps for each basin
    results <- vector("list", length(basin_shp_list))

    # Create enhanced filenames with analysis parameters
    query_year <- format(query_date, "%Y")

    # Create historical period string
    if (is.null(historical_start_year)) {
        hist_period <- sprintf("%d", historical_end_year)
    } else {
        hist_period <- sprintf(
            "%d-%d",
            historical_start_year,
            historical_end_year
        )
    }

    for (i in seq_along(basin_shp_list)) {
        basin_shp <- basin_shp_list[[i]]
        base_filename <- filenames[i]

        # Extract directory and base name without extension
        file_dir <- dirname(base_filename)
        base_name <- tools::file_path_sans_ext(basename(base_filename))
        file_ext <- tools::file_ext(base_filename)

        # Create enhanced filename: basename_queryYear_stat_histPeriod_statistic.ext
        enhanced_filename <- file.path(
            file_dir,
            sprintf(
                "%s_%s_%s_%s_%s.%s",
                base_name,
                query_year,
                summary_stat,
                hist_period,
                statistic,
                file_ext
            )
        )

        basin_result <- generate_basin_swe_map(
            basin_shp = basin_shp,
            filename = enhanced_filename,
            era5_result = processed_rasters$era5_result,
            communities = communities,
            crs = crs,
            upsample_factor = upsample_factor,
            historical_start_year = historical_start_year,
            historical_end_year = historical_end_year,
            query_date = query_date,
            processed_rasters = processed_rasters
        )

        # Add processed raster data to results
        results[[i]] <- c(basin_result, processed_rasters)
    }

    return(results)
}


# Data Preparation Functions ----

#' Load SWE basin shapefiles and prepare for analysis
#' @return List containing basin geometries and filenames
prepare_swe_basins <- function() {
    basins_shp <- sf::st_read(
        system.file(
            "snow_survey/swe_basins/swe_basins.shp",
            package = "YGwater",
            mustWork = TRUE
        ),
        quiet = TRUE
    )

    basin_names <- unique(basins_shp$SWE_Basin)

    basin_shp_list <- lapply(basin_names, function(nm) {
        b <- basins_shp[basins_shp$SWE_Basin == nm, ]
        sf::st_make_valid(b)
    })

    filenames <- file.path(
        "dev/swe/exports",
        paste0(tolower(basin_names), ".png")
    )

    return(list(
        basin_shp_list = basin_shp_list,
        filenames = filenames,
        basin_names = basin_names
    ))
}

#' Prepare specific basin from external shapefile
#' @param shapefile_path Path to shapefile
#' @param station_id Station identifier for filtering
#' @param filename Output filename
#' @return List with basin data
prepare_external_basin <- function(shapefile_path, station_id, filename) {
    wsc_shapefile <- sf::st_read(shapefile_path)
    basin_shp <- wsc_shapefile[wsc_shapefile$StationNum == station_id, ]
    basin_shp <- sf::st_make_valid(basin_shp)

    return(list(
        basin_shp_list = list(basin_shp),
        filenames = c(filename)
    ))
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

# Initialize connection ----
con <- connect_to_database()
query_date <- get_latest_era5_date(con)

# Example 1: Analyze all SWE basins ----
cat("Analyzing SWE basins...\n")
swe_data <- prepare_swe_basins()

result_list_swe <- run_swe_analysis(
    basin_shp_list = swe_data$basin_shp_list,
    filenames = swe_data$filenames,
    con = con,
    query_date = as.Date(query_date)
)

# Example 2: Analyze specific WSC basin ----
cat("Analyzing WSC basin...\n")
wsc_data <- prepare_external_basin(
    shapefile_path = "H:/esniede/data/WSC/MDA_ADP_10/MDA_ADP_10_DrainageBasin_BassinDeDrainage.geojson",
    station_id = "10AA001",
    filename = "dev/swe/exports/upper_basin.png"
)

result_list_wsc <- run_swe_analysis(
    basin_shp_list = wsc_data$basin_shp_list,
    filenames = wsc_data$filenames,
    con = con,
    query_date = as.Date(query_date)
)

cat(
    "Analysis complete. Results saved to:",
    unique(dirname(c(swe_data$filenames, wsc_data$filenames))),
    "\n"
)
