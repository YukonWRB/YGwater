library(AquaCache)
library(YGwater)
library(DBI)
library(plotly)
library(leaflet)
library(sf)
library(terra)

# -----------------------------------------------------------------------------
# 1. Database Connection
# -----------------------------------------------------------------------------
#' Create a database connection using environment variables
#' @return DBI connection object
get_db_connection <- function() {
    config <- list(
        dbName = "aquacache",
        dbHost = Sys.getenv("aquacacheHostProd"),
        dbPort = Sys.getenv("aquacachePortProd"),
        dbUser = Sys.getenv("aquacacheUserProd"),
        dbPass = Sys.getenv("aquacachePassProd")
    )
    AquaConnect(
        name = config$dbName,
        host = config$dbHost,
        port = config$dbPort,
        user = config$dbUser,
        pass = config$dbPass
    )
}

# -----------------------------------------------------------------------------
# 2. Download Spatial Layers
# -----------------------------------------------------------------------------
#' Download a spatial vector layer from the database
#' @param con DBI connection
#' @param layer_name Name of the spatial layer
#' @param additional_query Optional SQL to append
#' @return sf object
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

# -----------------------------------------------------------------------------
# 3. Download ERA5 Raster Stack for a Bounding Box and Date
# -----------------------------------------------------------------------------
#' Download ERA5 raster stack for a bounding box and specified dates/month/year
#' @param con DBI connection
#' @param bbox Named vector with xmin, xmax, ymin, ymax
#' @param years Vector of years (default NULL)
#' @param months Vector of months (default NULL)
#' @param days Vector of days (default NULL)
#' @param query_date Date for the most recent raster (default NULL)
#' @return List with historical stack, query raster (if any), and time vectors
download_era5_stack <- function(
    con,
    bbox,
    years = NULL,
    months = NULL,
    days = NULL,
    query_date = NULL
) {
    # Build WHERE clause for historical rasters
    date_clauses <- c()
    if (!is.null(years)) {
        date_clauses <- c(
            date_clauses,
            sprintf(
                "EXTRACT(YEAR FROM rr.valid_from) IN (%s)",
                paste(years, collapse = ",")
            )
        )
    }
    if (!is.null(months)) {
        date_clauses <- c(
            date_clauses,
            sprintf(
                "EXTRACT(MONTH FROM rr.valid_from) IN (%s)",
                paste(months, collapse = ",")
            )
        )
    }
    if (!is.null(days)) {
        date_clauses <- c(
            date_clauses,
            sprintf(
                "EXTRACT(DAY FROM rr.valid_from) IN (%s)",
                paste(days, collapse = ",")
            )
        )
    }
    historical_where <- if (length(date_clauses) > 0) {
        paste(date_clauses, collapse = " AND ")
    } else {
        "TRUE"
    }

    # Query for historical rasters
    era5_query_hist <- sprintf(
        "
        SELECT r.reference_id, rr.valid_from as datetime
        FROM spatial.raster_series_index rsi
        JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
        JOIN spatial.rasters r ON r.reference_id = rr.reference_id
        WHERE rsi.model = 'ERA5_land'
        AND ST_Intersects(
            r.rast,
            ST_MakeEnvelope(%f, %f, %f, %f, 4326)
        )
        AND %s
        ORDER BY rr.valid_from, r.reference_id
        ",
        bbox["xmin"],
        bbox["ymin"],
        bbox["xmax"],
        bbox["ymax"],
        historical_where
    )
    era5_hist_data <- DBI::dbGetQuery(con, era5_query_hist)
    era5_hist_data$datetime <- as.POSIXct(era5_hist_data$datetime)

    # Download historical rasters with progress bar and suppress warnings/messages
    hist_reference_ids <- era5_hist_data$reference_id
    hist_time_vector <- era5_hist_data$datetime
    n_hist <- length(hist_reference_ids)
    era5_hist_rasters <- vector("list", n_hist)
    if (n_hist > 0) {
        pb <- utils::txtProgressBar(min = 0, max = n_hist, style = 3)
        for (k in seq_along(hist_reference_ids)) {
            era5_hist_rasters[[k]] <- suppressMessages(
                suppressWarnings(
                    YGwater::getRaster(
                        con = con,
                        clauses = paste0(
                            "WHERE reference_id = ",
                            hist_reference_ids[k]
                        )
                    )
                )
            )
            utils::setTxtProgressBar(pb, k)
        }
        close(pb)
    }
    if (length(era5_hist_rasters) == 0) {
        stop("No historical rasters found for the specified criteria.")
    }
    era5_hist_stack <- do.call(c, era5_hist_rasters)

    # Query and download the most recent raster (query_date), if provided
    era5_query_raster <- NULL
    era5_query_time <- NULL
    if (!is.null(query_date)) {
        era5_query_recent <- sprintf(
            "
            SELECT r.reference_id, rr.valid_from as datetime
            FROM spatial.raster_series_index rsi
            JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
            JOIN spatial.rasters r ON r.reference_id = rr.reference_id
            WHERE rsi.model = 'ERA5_land'
            AND ST_Intersects(
                r.rast,
                ST_MakeEnvelope(%f, %f, %f, %f, 4326)
            )
            AND rr.valid_from::date = '%s'
            ORDER BY rr.valid_from DESC, r.reference_id DESC
            LIMIT 1
            ",
            bbox["xmin"],
            bbox["ymin"],
            bbox["xmax"],
            bbox["ymax"],
            as.character(query_date)
        )
        era5_recent_data <- DBI::dbGetQuery(con, era5_query_recent)
        if (nrow(era5_recent_data) > 0) {
            era5_query_raster <- suppressMessages(
                suppressWarnings(
                    YGwater::getRaster(
                        con = con,
                        clauses = paste0(
                            "WHERE reference_id = ",
                            era5_recent_data$reference_id[1]
                        )
                    )
                )
            )
            era5_query_time <- as.POSIXct(era5_recent_data$datetime[1])
        }
    }

    list(
        historical_stack = era5_hist_stack,
        historical_time_vector = hist_time_vector,
        query_raster = era5_query_raster,
        query_time = era5_query_time
    )
}

# -----------------------------------------------------------------------------
# 4. Get raster reference ids for historical and query rasters
# -----------------------------------------------------------------------------
#' Get ERA5 raster reference ids for a bounding box and date constraints
#' @param con DBI connection
#' @param bbox Named vector with xmin, xmax, ymin, ymax
#' @param years, months, days Vectors for filtering
#' @param query_date Date for the most recent raster (default NULL)
#' @return list with $historical_ids, $historical_times, $query_id, $query_time
get_era5_raster_ids <- function(
    con,
    bbox,
    years = NULL,
    months = NULL,
    days = NULL,
    query_date = NULL
) {
    date_clauses <- c()
    if (!is.null(years)) {
        date_clauses <- c(
            date_clauses,
            sprintf(
                "EXTRACT(YEAR FROM rr.valid_from) IN (%s)",
                paste(years, collapse = ",")
            )
        )
    }
    if (!is.null(months)) {
        date_clauses <- c(
            date_clauses,
            sprintf(
                "EXTRACT(MONTH FROM rr.valid_from) IN (%s)",
                paste(months, collapse = ",")
            )
        )
    }
    if (!is.null(days)) {
        date_clauses <- c(
            date_clauses,
            sprintf(
                "EXTRACT(DAY FROM rr.valid_from) IN (%s)",
                paste(days, collapse = ",")
            )
        )
    }
    historical_where <- if (length(date_clauses) > 0) {
        paste(date_clauses, collapse = " AND ")
    } else {
        "TRUE"
    }

    # Historical rasters
    era5_query_hist <- sprintf(
        "
        SELECT r.reference_id, rr.valid_from as datetime
        FROM spatial.raster_series_index rsi
        JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
        JOIN spatial.rasters r ON r.reference_id = rr.reference_id
        WHERE rsi.model = 'ERA5_land'
        AND ST_Intersects(
            r.rast,
            ST_MakeEnvelope(%f, %f, %f, %f, 4326)
        )
        AND %s
        ORDER BY rr.valid_from, r.reference_id
        ",
        bbox["xmin"],
        bbox["ymin"],
        bbox["xmax"],
        bbox["ymax"],
        historical_where
    )
    era5_hist_data <- DBI::dbGetQuery(con, era5_query_hist)
    era5_hist_data$datetime <- as.POSIXct(era5_hist_data$datetime)

    # Query raster (if query_date provided)
    query_id <- NULL
    query_time <- NULL
    if (!is.null(query_date)) {
        era5_query_recent <- sprintf(
            "
            SELECT r.reference_id, rr.valid_from as datetime
            FROM spatial.raster_series_index rsi
            JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
            JOIN spatial.rasters r ON r.reference_id = rr.reference_id
            WHERE rsi.model = 'ERA5_land'
            AND ST_Intersects(
                r.rast,
                ST_MakeEnvelope(%f, %f, %f, %f, 4326)
            )
            AND rr.valid_from::date = '%s'
            ORDER BY rr.valid_from DESC, r.reference_id DESC
            LIMIT 1
            ",
            bbox["xmin"],
            bbox["ymin"],
            bbox["xmax"],
            bbox["ymax"],
            as.character(query_date)
        )
        era5_recent_data <- DBI::dbGetQuery(con, era5_query_recent)
        if (nrow(era5_recent_data) > 0) {
            query_id <- era5_recent_data$reference_id[1]
            query_time <- as.POSIXct(era5_recent_data$datetime[1])
        }
    }

    list(
        historical_ids = era5_hist_data$reference_id,
        historical_times = era5_hist_data$datetime,
        query_id = query_id,
        query_time = query_time
    )
}

# -----------------------------------------------------------------------------
# Download and calculate median raster (loads all into memory)
# -----------------------------------------------------------------------------
#' Download rasters by ids and calculate median
#' @param con DBI connection
#' @param raster_ids Vector of reference ids
#' @return SpatRaster median
get_raster_median <- function(con, raster_ids) {
    n <- length(raster_ids)
    rasters <- vector("list", n)
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    for (i in seq_along(raster_ids)) {
        rasters[[i]] <- suppressMessages(
            suppressWarnings(
                YGwater::getRaster(
                    con = con,
                    clauses = paste0("WHERE reference_id = ", raster_ids[i])
                )
            )
        )
        utils::setTxtProgressBar(pb, i)
    }
    close(pb)
    stack <- do.call(c, rasters)
    terra::app(stack, fun = median, na.rm = TRUE)
}

# -----------------------------------------------------------------------------
# Calculate basin-averaged SWE values (does not load all rasters into memory)
# -----------------------------------------------------------------------------
#' Calculate basin-averaged SWE for each raster id and basin
#' @param con DBI connection
#' @param basin_shp_list List of sf polygons
#' @param raster_ids Vector of reference ids
#' @return data.frame: columns = basin, datetime, swe
get_basin_averages <- function(
    con,
    basin_shp_list,
    raster_ids,
    datetimes,
    basin_names
) {
    n_basins <- length(basin_shp_list)
    n_layers <- length(raster_ids)
    result <- data.frame()
    pb <- utils::txtProgressBar(min = 0, max = n_layers, style = 3)
    for (j in seq_along(raster_ids)) {
        r <- suppressMessages(
            suppressWarnings(
                YGwater::getRaster(
                    con = con,
                    clauses = paste0("WHERE reference_id = ", raster_ids[j])
                )
            )
        )
        raster_crs <- terra::crs(r)
        vals <- numeric(n_basins)
        for (i in seq_along(basin_shp_list)) {
            basin <- basin_shp_list[[i]]
            if (!sf::st_crs(basin) == sf::st_crs(raster_crs)) {
                basin <- sf::st_transform(basin, crs = raster_crs)
            }
            vals[i] <- terra::global(
                terra::mask(terra::crop(r, basin), basin),
                fun = "mean",
                na.rm = TRUE
            )$mean
        }
        row <- data.frame(
            datetime = datetimes[j],
            t(vals)
        )
        names(row)[-1] <- basin_names
        result <- rbind(result, row)
        utils::setTxtProgressBar(pb, j)
    }
    close(pb)
    result
}

main <- function() {
    con <- get_db_connection()
    # Download communities
    communities <- download_spatial_layer(con, "Communities")
    # Prepare basin shapefiles
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
        sf::st_make_valid(basins_shp[basins_shp$SWE_Basin == nm, ])
    })
    filenames <- file.path(
        "dev/swe/exports",
        paste0(tolower(basin_names), ".png")
    )
    bbox <- sf::st_bbox(sf::st_transform(
        do.call(sf::st_union, basin_shp_list),
        crs = 4326
    ))
    # Get most recent ERA5 date
    era5_query <- "
        SELECT rr.valid_from as datetime
        FROM spatial.raster_series_index rsi
        JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
        JOIN spatial.rasters r ON r.reference_id = rr.reference_id
        WHERE rsi.model = 'ERA5_land'
        ORDER BY rr.valid_from DESC, r.reference_id DESC
        LIMIT 1
    "
    query_date <- as.Date(DBI::dbGetQuery(con, era5_query)$datetime)
    historical_start_year <- 1991
    historical_end_year <- 2020

    # Get raster ids
    ids <- get_era5_raster_ids(
        con,
        bbox,
        years = 2000:2026,
        months = 1:2,
        days = 1:30
    )

    # Calculate median raster (loads all into memory)
    era5_median <- get_raster_median(con, ids$historical_ids)

    # Calculate basin-averaged SWE (does not load all rasters into memory)
    historical_means_df <- get_basin_averages(
        con,
        basin_shp_list,
        ids$historical_ids,
        ids$historical_times,
        basin_names
    )

    # Query raster and relative change
    era5_relative_change <- NULL
    query_means <- NULL
    if (!is.null(ids$query_id)) {
        query_raster <- suppressMessages(
            suppressWarnings(
                YGwater::getRaster(
                    con = con,
                    clauses = paste0("WHERE reference_id = ", ids$query_id)
                )
            )
        )
        era5_relative_change <- 100 * query_raster / era5_median
        # Basin averages for query raster
        vals <- numeric(length(basin_shp_list))
        raster_crs <- terra::crs(query_raster)
        for (i in seq_along(basin_shp_list)) {
            basin <- basin_shp_list[[i]]
            if (!sf::st_crs(basin) == sf::st_crs(raster_crs)) {
                basin <- sf::st_transform(basin, crs = raster_crs)
            }
            vals[i] <- terra::global(
                terra::mask(terra::crop(query_raster, basin), basin),
                fun = "mean",
                na.rm = TRUE
            )$mean
        }
        query_means <- setNames(vals, basin_names)
    }

    # Convert historical_means matrix to a time series object
    # Use the historical_time_vector from era5 for the time index
    historical_means_df <- as.data.frame(t(swe_stats$historical_means))
    colnames(historical_means_df) <- basin_names
    historical_means_df$datetime <- era5$historical_time_vector
    # swe_stats$historical_means: matrix (n_basins x n_layers)
    # swe_stats$query_means: vector (n_basins) or NULL

    # Analyze and plot for each basin (only if relative change is available)
    if (!is.null(era5_relative_change)) {
        analyze_and_plot_basins(
            basin_shp_list,
            filenames,
            era5_relative_change,
            communities,
            upsample_factor = 16,
            crs = 4326,
            query_date = query_date,
            historical_start_year = historical_start_year,
            historical_end_year = historical_end_year
        )
    } else {
        warning("No query raster available for the specified query_date.")
    }

    return(list(
        historical_means_df = historical_means_df,
        query_means = query_means,
        era5_median = era5_median,
        era5_relative_change = era5_relative_change
    ))
}

# -----------------------------------------------------------------------------
# Run the main workflow
# -----------------------------------------------------------------------------
ok <- main()


library(ggplot2)
library(lubridate)
# Assume ok$historical_means_df exists and has columns: basin names, datetime

# Example: plot for the first basin
basin_to_plot <- names(ok$historical_means_df)[2] # e.g., "Big Salmon"
df <- ok$historical_means_df
df$year <- year(df$datetime)
df$doy <- yday(df$datetime)

# Gather into long format for plotting (if multiple basins)
# library(tidyr)
# df_long <- tidyr::pivot_longer(df, cols = basin_names, names_to = "basin", values_to = "swe")

# Plot each year as a trace, overlapping by day-of-year
p <- ggplot(
    df,
    aes(x = doy, y = .data[[basin_to_plot]], color = factor(year), group = year)
) +
    geom_line(size = 1) +
    labs(
        title = paste("SWE for", basin_to_plot, "by Year"),
        x = "Day of Year",
        y = "SWE (mm)",
        color = "Year"
    ) +
    theme_minimal()

df_input <- df[, c("datetime", basin_to_plot)]
names(df_input) <- c("datetime", "value")

ggplotOverlap(
    parameter = "swe",
    continuous_data = df_input,
    location = basin_to_plot,
    years = 2025,
    units = "m",
)
