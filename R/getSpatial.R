#' Retrieve raster files from the database
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param clauses Character SQL clauses to filter the raster data and return the results. Must begin with 'WHERE' and be formatted as a valid SQL WHERE clause. For example, 'WHERE reference_id = 1'. If NULL, all raster data is returned.
#' @param boundary Optional spatial boundary to limit the extent of the raster data returned, as a terra::spatVector object or a numeric vector c(top, bottom, right, left) indicating the projection-specific limits with which to clip the raste (by default, decimal degree lat/long).  If NULL, the full extent of the raster data is returned.
#' @param bands The raster bands to return. Default is 1, which returns the first band of the raster data. If you want to return all bands, set this to TRUE.
#' @param tbl_name The name of the schema and table containing the raster data. Default is c("spatial", "rasters").
#' @param col_name The name of the column containing the raster data. Default is "rast".
#' @param con A connection to the target database. If NULL, a new connection is created using [AquaConnect()] and automatically closed when the function exits.
#' @return A terra::rast object containing the raster data.
#' @export
#'

getRaster <- function(clauses = NULL,
                      boundary = NULL,
                      tbl_name = c("spatial", "rasters"), 
                      col_name = "rast", 
                      bands = 1,
                      con = NULL) 
{
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  if (!suppressMessages(rpostgis::pgPostGIS(con))) {
    cli::cli_abort("PostGIS is not enabled on this database.")
  }
  name1 <- rpostgis:::dbTableNameFix(con, tbl_name)
  nameque <- paste(name1, collapse = ".")
  namechar <- gsub("'", "''", paste(gsub("^\"|\"$", "", name1), 
                                    collapse = "."))
  rastque <- DBI::dbQuoteIdentifier(con, col_name)
  clauses2 <- sub("^where", "AND", clauses, ignore.case = TRUE)
  tmp.query <- paste0("SELECT r_raster_column AS geo FROM raster_columns\n  WHERE (r_table_schema||'.'||r_table_name) = '", 
                      namechar, "';")
  tab.list <- DBI::dbGetQuery(con, tmp.query)$geo
  if (is.null(tab.list)) {
    cli::cli_abort("Table '{namechar}' is not listed in raster_columns.")
  }
  else if (!col_name %in% tab.list) {
    cli::cli_abort("Table '{namechar}' raster column '{col_name}' not found. Available raster columns: {paste(tab.list, collapse = ', ')}")
  }
  tmp.query <- paste0("SELECT st_numbands(", rastque, ") FROM ", 
                      nameque, " WHERE ", rastque, " IS NOT NULL LIMIT 1;")
  nbs <- 1:DBI::dbGetQuery(con, tmp.query)[1, 1]
  if (isTRUE(bands)) {
    bands <- nbs
  }
  else if (!all(bands %in% nbs)) {
    cli::cli_abort("Selected band(s) do not exist in PostGIS raster: choose band numbers between {min(nbs)} and {max(nbs)}.")
  }
  # Add WHERE clause to restrict to non-null rasters
  tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", rastque, 
                      ")) FROM ", nameque, " WHERE ", rastque, " IS NOT NULL", if (!is.null(clauses2)) paste0(" ", clauses2) else "", ";")
  
  srid <- DBI::dbGetQuery(con, tmp.query)
  
  
  #if (nrow(srid) > 1) {
  #    cli::cli_abort("Multiple SRIDs in the raster")
  #}
  #else if (nrow(srid) < 1) {
  #    cli::cli_abort("Database table is empty.")
  #}
  
  
  p4s <- NA
  tmp.query.sr <- paste0("SELECT r_proj4 AS p4s FROM ", nameque, 
                         ";")
  try(db.proj4 <- DBI::dbGetQuery(con, tmp.query.sr)$p4s, silent = TRUE)
  if (!exists("db.proj4")) {
    tmp.query.sr <- paste0("SELECT proj4text AS p4s FROM spatial_ref_sys WHERE srid = ", 
                           srid$st_srid, ";")
    db.proj4 <- DBI::dbGetQuery(con, tmp.query.sr)$p4s
  }
  if (!is.null(db.proj4)) {
    try(p4s <- terra::crs(db.proj4[1]), silent = TRUE)
  }
  if (is.na(p4s)) {
    cli::cli_alert_warning("Table SRID not found. Projection will be undefined (NA)")
  }
  tmp.query <- paste0("SELECT ST_SameAlignment(", rastque, 
                      ") FROM ", nameque, ";")
  al <- FALSE
  try(al <- DBI::dbGetQuery(con, tmp.query)[1, 1])
  if (!al) {
    tmp.query <- paste0("SELECT min(ST_UpperLeftX(", rastque, 
                        ")) ux, max(ST_UpperLeftY(", rastque, ")) uy FROM ", 
                        nameque, ";")
    aligner <- DBI::dbGetQuery(con, tmp.query)
    aq <- c("ST_SnapToGrid(", paste0(aligner[1, 1], ","), 
            paste0(aligner[1, 2], "),"))
  }
  else {
    aq <- NULL
  }
  get_band <- function(band) {
    info <- DBI::dbGetQuery(con, paste0("select\n            st_xmax(st_envelope(rast)) as xmax,\n            st_xmin(st_envelope(rast)) as xmin,\n            st_ymax(st_envelope(rast)) as ymax,\n            st_ymin(st_envelope(rast)) as ymin,\n            st_width(rast) as cols,\n            st_height(rast) as rows\n            from\n            (select st_union(", 
                                         aq[1], rastque, ",", aq[2], aq[3], band, ") rast from ", 
                                         nameque, " ", clauses, ") as a;"))
    vals <- DBI::dbGetQuery(con, paste0("select\n          unnest(st_dumpvalues(rast, 1)) as vals\n          from\n          (select st_union(", 
                                         aq[1], rastque, ",", aq[2], aq[3], band, ") rast from ", 
                                         nameque, " ", clauses, ") as a;"))$vals
    rout <- terra::rast(nrows = info$rows, ncols = info$cols, 
                        xmin = info$xmin, xmax = info$xmax, ymin = info$ymin, 
                        ymax = info$ymax, crs = p4s, vals = vals)
    return(rout)
  }
  get_band_boundary <- function(band) {
    info <- DBI::dbGetQuery(con, paste0("select\n            st_xmax(st_envelope(rast)) as xmx,\n            st_xmin(st_envelope(rast)) as xmn,\n            st_ymax(st_envelope(rast)) as ymx,\n            st_ymin(st_envelope(rast)) as ymn,\n            st_width(rast) as cols,\n            st_height(rast) as rows\n            from\n            (select st_union(", 
                                         aq[1], rastque, ",", aq[2], aq[3], band, ") rast from ", 
                                         nameque, "\n\n            WHERE ST_Intersects(", 
                                         rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", 
                                         boundary[4], " ", boundary[1], ",", boundary[4], 
                                         " ", boundary[2], ",\n  ", boundary[3], " ", boundary[2], 
                                         ",", boundary[3], " ", boundary[1], ",", boundary[4], 
                                         " ", boundary[1], "))'),", srid, "))", clauses2, 
                                         ") as a;"))
    if (is.na(info$cols) & is.na(info$rows)) {
      stop("No data found within geographic subset defined by 'boundary'.")
    }
    vals <- DBI::dbGetQuery(con, paste0("select\n          unnest(st_dumpvalues(rast, 1)) as vals\n          from\n          (select st_union(", 
                                         aq[1], rastque, ",", aq[2], aq[3], band, ") rast from ", 
                                         nameque, "\n\n            WHERE ST_Intersects(", 
                                         rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", 
                                         boundary[4], " ", boundary[1], ",", boundary[4], 
                                         " ", boundary[2], ",\n  ", boundary[3], " ", boundary[2], 
                                         ",", boundary[3], " ", boundary[1], ",", boundary[4], 
                                         " ", boundary[1], "))'),", srid, "))", clauses2, 
                                         ") as a;"))$vals
    rout <- terra::rast(nrows = info$rows, ncols = info$cols, 
                        xmin = info$xmn, xmax = info$xmx, ymin = info$ymn, 
                        ymax = info$ymx, crs = p4s, vals = vals)
    return(rout)
  }
  
  
  
  cli::cli_alert_info("Reading {length(bands)} band{?s}...")
  download_pb <- cli::cli_progress_bar("Read bands", total = length(bands), 
                                       type = "tasks", format_done = "{.alert-success Read completed {.timestamp {cli::pb_elapsed}}}", 
                                       clear = FALSE)
  rout <- list()
  if (is.null(boundary)) {
    for (i in seq_along(bands)) {
      rout[[i]] <- get_band(bands[i])
      cli::cli_progress_update(id = download_pb)
    }
    rb <- terra::rast(rout)
  }
  else {
    if (inherits(boundary, "sf")) {
      boundary <- sf::st_bbox(boundary)
      boundary <- c(boundary[4], boundary[2], boundary[3], 
                    boundary[1])
    }
    else if (inherits(boundary, "SpatVector")) {
      boundary <- c(terra::ext(boundary)[4], terra::ext(boundary)[3], 
                    terra::ext(boundary)[2], terra::ext(boundary)[1])
    }
    extclip <- terra::ext(boundary[4], boundary[3], boundary[2], 
                          boundary[1])
    for (i in seq_along(bands)) {
      rout[[i]] <- get_band_boundary(bands[i])
      cli::cli_progress_update(id = download_pb)
    }
    rb <- terra::rast(rout)
  }
  
  cli::cli_process_done(id = download_pb)
  
  if ("band_names" %in% rpostgis::dbTableInfo(con, tbl_name)$column_name) {
    try({
      ct <- 1
      for (b in bands) {
        lnm <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT band_names[", 
                                            b, "][1] as nm FROM ", nameque, " ", clauses, 
                                            ";"))
        names(rb)[ct] <- lnm$nm
        ct <- ct + 1
      }
    })
  }
  
  return(rb)
  
}




#' Retrieve vector files from the database
#' 
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function formulates an SQL query to retrieve points, lines, or polygons from the database and returns them as a terra object. At minimum, only one of `geom_id`, `layer_name`, `feature_name`, or `geom_type` are required, though if the combination you enter results in more than one geom_type you will get a descriptive error.
#'
#' @param geom_id A numeric vector of geom_ids from the 'vectors' table.
#' @param layer_name A character vector (one or more elements) specifying the target layer_name from the 'vectors' table.
#' @param feature_name A character vector (one or more elements) specifying the target feature_name from the 'vectors' table.
#' @param geom_type One of c('ST_Point', 'ST_MultiPoint', 'ST_LineString', 'ST_MultiLineString', 'ST_Polygon', 'ST_MultiPolygon').
#' @param return_cols The names of columns to return. The default includes all columns.
#' @param bounds Optional bounds by which to limit the spatial extent returned. Currently returns the full extent of every feature intersecting the bounds. Refer to parameter 'boundary' of [rpostgis::pgGetGeom()] for details of how to specify this parameter.
#' @param table The target table in the database (as character string). See 'schema' if not under the 'spatial' schema.
#' @param schema The schema in which the target 'table' is located. Default is 'spatial'. Note that this is NOT the default for [rpostgis::pgGetGeom()].
#' @param geom_col The name of the database table column which holds the geometry object.
#' @param silent Should the function suppress all messages?
#' @param con A connection to the target database. NULL uses [AquaConnect()] and automatically disconnects.
#'
#' @return If successful, a terra object. If unsuccessful because the query targets more than 1 geometry types, a table showing you the result of the query.
#' @export
#'

getVector <- function(geom_id = NULL, layer_name = NULL, feature_name = NULL, geom_type = NULL, return_cols = c("geom_id", "geom_type", "layer_name", "feature_name", "description"), bounds = NULL, table = "vectors", schema = "spatial", geom_col = "geom", silent = FALSE, con = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  rlang::check_installed("rpostgis", reason = "required to use function getVector.")
  
  if (is.null(geom_id) & is.null(layer_name) & is.null(feature_name) & is.null(geom_type)) {
    stop("You need to specify at least one of the geom_id, layer_name, feature_name, or geom_type.")
  }
  if (!is.null(geom_type)) {
    if (length(geom_type) > 1) {
      stop("You can only select one geom_type at a time.")
    } else if (!(geom_type %in% c('ST_Point', 'ST_MultiPoint', 'ST_LineString', 'ST_MultiLineString', 'ST_Polygon', 'ST_MultiPolygon'))) {
      stop("Parameter geom_type is not one of the possible choices. Refer to the help file.")
    }
  }
  
  # build the query to see if data exists in the table
  query <- paste0("SELECT geom_id, layer_name, feature_name, geom_type FROM ", schema, ".", table, " WHERE")
  
  if (!is.null(geom_id)) {
    query <- paste0(query,  " geom_id IN (", paste(geom_id, collapse = ", "), ") AND")
  }
  if (!is.null(layer_name)) {
    query <- paste0(query,  " layer_name IN ('", paste(layer_name, collapse = "', '"), "') AND")
  }
  if (!is.null(feature_name)) {
    query <- paste0(query,  " feature_name IN ('", paste(feature_name, collapse = "', '"), "') AND")
  }
  if (!is.null(geom_type)) {
    query <- paste0(query,  " geom_type IN ('", paste(geom_type, collapse = "', '"), "') AND")
  }
  query <- gsub("\\s+AND$", "", query)
  tbl <- DBI::dbGetQuery(con, query)
  
  if (nrow(tbl) == 0) {
    stop("Your query returned no results (even before bounds were applied).")
  }
  
  # Check if query resulted in multiple geom_types
  if (length(unique(tbl$geom_type)) > 1) {
    if (!silent) {
      message("Your query resulted in more than one geometry type: ", paste(unique(tbl$geom_type), collapse = ", AND "), " were returned. This may result in a non-functional terra object if, for example, lines and polygons are returned together.")
    }
  }
  
  subquery <- paste0("WHERE")
  
  if (!is.null(geom_id)) {
    subquery <- paste0(subquery,  " geom_id IN (", paste(geom_id, collapse = ", "), ") AND")
  }
  if (!is.null(layer_name)) {
    subquery <- paste0(subquery,  " layer_name IN ('", paste(layer_name, collapse = "', '"), "') AND")
  }
  if (!is.null(feature_name)) {
    subquery <- paste0(subquery,  " feature_name IN ('", paste(feature_name, collapse = "', '"), "') AND")
  }
  if (!is.null(geom_type)) {
    subquery <- paste0(subquery,  " geom_type IN ('", paste(geom_type, collapse = "', '"), "') AND")
  }
  subquery <- gsub("\\s+AND$", "", subquery)
  
  if (silent) {
    res <- suppressMessages(rpostgis::pgGetGeom(con, name = c(schema, table), geom = "geom", gid = "geom_id", other.cols = TRUE, clauses = subquery, returnclass = "terra", boundary = bounds))
  } else {
    res <- rpostgis::pgGetGeom(con, name = c(schema, table), geom = "geom", gid = "geom_id", other.cols = TRUE, clauses = subquery, returnclass = "terra", boundary = bounds)
  }
  return(res)
}
