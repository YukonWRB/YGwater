#' Extract image from postgreSQL DB
#'
#' @description
#' Extracts an image stored in the aquacache as BYTEA type. For extracting documents see [getDocument()], for extracting vector files as *terra* rasters (points, lines polygons) see [getVector()], and for extracting rasters to R as *terra* objects see [getRaster()].
#' 
#' @details
#' If you need additional flexibility use function [getFile()] instead. This should not normally be needed with a database created by the AquaCache package but is made available for use with other databases.
#' 
#' @param id The ID number from column 'image_id' of table 'images'.
#' @param con A connection to the database. NULL will use [AquaConnect()] and disconnect automatically when done.
#' @param save_dir A directory in which to write the file.
#' @param save_name The name to give the file, *without* extension.
#'
#' @return A data.frame containing the row(s) identified by the specified ID. One of the columns will contain the binary object (file). The image will be saved to file if requested and possible.
#' @export
#'

getImage <- function(id, con = NULL, save_dir = NULL, save_name = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  res <- getFile(id = id, id_col = "image_id", ext = "format", table = "images", con = con, save_dir = save_dir, save_name = save_name)
  return(res)
}

#' Extract document from postgreSQL DB
#'
#' @description
#' Extracts a document stored in the aquacache as BYTEA type. For extracting images see [getImage()], for extracting vector files as *terra* rasters (points, lines polygons) see [getVector()], and for extracting rasters to R as *terra* objects see [getRaster()].
#'
#'@details
#' If you need additional flexibility use function [getFile()] instead. This should not normally be needed with a database created by the AquaCache package but is made available for use with other databases.
#' 
#' @param id The ID number from column 'document_id' of table 'documents'.
#' @param con A connection to the database. NULL will use [AquaConnect()] and disconnect automatically when done.
#' @param save_dir A directory in which to write the file.
#' @param save_name The name to give the file, *without* extension.
#'
#' @return A data.frame containing the row(s) identified by the specified ID. One of the columns will contain the binary object (file). The document will be saved to file if requested and possible.
#' @export
#'

getDocument <- function(id, con = NULL, save_dir = NULL, save_name = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  res <- getFile(id = id, id_col = "document_id", ext = "format", table = "documents", con = con, save_dir = save_dir, save_name = save_name)
  return(res)
}


#' Extract document stored as BYTEA from postgreSQL DB
#'
#' @description
#' *NOTE* Unless you really need the additional flexibility use functions [getDocument()] or [getImage()].
#'
#' Extracts a document/file stored in a postgreSQL database as BYTEA type. Should be flexible enough to work with most schemas, but was designed around the 'documents' and 'images' tables created with this package and with files uploaded to the database with functions from package AquaCache. For extracting vector files as *terra* rasters (points, lines polygons) see [getVector()], and for extracting rasters to R as *terra* objects see [getRaster()].
#'
#' @param id The unique ID from the column specified in `id_col` with which to identify the individual record containing the binary object.
#' @param id_col The column in which to look for the `id`
#' @param ext The column in which to look for the file extension, or a file extension (must be preceded by a period). Only used if save_path is not NULL
#' @param table The table to look in for the document.
#' @param con A connection to the database. NULL will use [AquaConnect()] and disconnect automatically when done.
#' @param save_dir A directory in which to write the file.
#' @param save_name The name to give the file, *without* extension (specify the extension in `ext`).
#'
#' @return A data.frame containing the row(s) identified by the specified ID. One of the columns will contain the binary object (file). The document will be saved to file if requested and possible.
#' @export

getFile <- function(id, id_col, ext, table, con = NULL, save_dir = NULL, save_name = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  res <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table, " WHERE ", id_col, " = '", id, "';"))
  if (nrow(res) > 1) {
    warning("The id you specified returned more than one record. If you specified a save directory and name they will be ignored. You can save the results yourself using function writeBin() on the returned objects.")
    save_dir <- NULL
  }
  
  if (!is.null(save_dir)) {
    if (!dir.exists(save_dir)) {
      stop("The directory you pointed to does not exist.")
    }
    save_dir <- sub("/$", "", save_dir)
    if (!is.null(save_name)) {
      if (startsWith(ext, ".")) {
        name <- paste0(save_dir, "/", save_name, ext)
      } else {
        format <- res[[ext]]
        if (length(format) != 1) {
          stop("Attempting to get the file extension from the database did not yield anything. Perhaps you are specifying the wrong column name?")
        }
        name <- paste0(save_dir, "/", save_name, ".", format)
      }
      #find blob data type column
      vect <- logical(0)
      for (i in 1:ncol(res)) {
        if ("blob" %in% class(res[[i]])) {
          vect <- c(vect, TRUE)
        } else {
          vect <- c(vect, FALSE)
        }
      }
      if (sum(vect) > 1) {
        stop("There is more than one column of type 'blob' in the output. Returning the data.frame without writing to disk.")
        return(res)
      } else if (sum(vect) < 1) {
        stop("There is no column of type 'blob' in the output. Returning the data.frame without writing to disk.")
        return(res)
      }
      document <- res[[which(vect)]][[1]]
      writeBin(document, name)
    } else {
      stop("You must specify a name for the file if you specify a save directory.")
    }
  }
  
  return(res)
}
