heif_jpeg_conversion_available <- function() {
  if (!requireNamespace("magick", quietly = TRUE)) {
    return(FALSE)
  }

  config <- tryCatch(
    magick::magick_config(),
    error = function(e) NULL
  )
  !is.null(config) && isTRUE(config$heic) && isTRUE(config$jpeg)
}

convert_heif_to_jpeg <- function(
  path,
  output_path = NULL,
  extension = tools::file_ext(path)
) {
  if (length(extension) != 1L || is.na(extension)) {
    stop("The HEIC/HEIF image extension could not be determined.", call. = FALSE)
  }
  extension <- tolower(sub("^\\.", "", extension))
  if (!extension %in% c("heic", "heif")) {
    stop("Only HEIC and HEIF images can be converted by this helper.", call. = FALSE)
  }
  if (!heif_jpeg_conversion_available()) {
    stop(
      "This server cannot convert HEIC/HEIF images to JPEG. Upload a JPEG instead.",
      call. = FALSE
    )
  }
  if (is.null(output_path)) {
    output_path <- tempfile(
      pattern = "ygwater-heif-",
      fileext = ".jpg"
    )
  }

  converted <- tryCatch(
    {
      image <- magick::image_read(path)
      if (length(image) != 1L) {
        stop("The uploaded HEIC/HEIF file must contain exactly one image.")
      }
      image <- magick::image_orient(image)
      magick::image_write(
        image,
        path = output_path,
        format = "jpeg",
        quality = 95
      )
      isTRUE(file.exists(output_path)) &&
        !is.na(file.info(output_path)$size) &&
        file.info(output_path)$size > 0
    },
    error = function(e) {
      unlink(output_path, force = TRUE)
      stop(
        sprintf(
          "This HEIC/HEIF image could not be converted to JPEG: %s. Upload a JPEG instead.",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  if (!converted) {
    unlink(output_path, force = TRUE)
    stop(
      "This HEIC/HEIF image could not be converted to JPEG. Upload a JPEG instead.",
      call. = FALSE
    )
  }

  normalizePath(output_path, winslash = "/", mustWork = TRUE)
}

with_heif_upload_as_jpeg <- function(
  path,
  callback,
  extension = tools::file_ext(path)
) {
  if (!is.function(callback)) {
    stop("The HEIC/HEIF upload callback must be a function.", call. = FALSE)
  }

  if (length(extension) != 1L || is.na(extension)) {
    stop("The HEIC/HEIF image extension could not be determined.", call. = FALSE)
  }
  extension <- tolower(sub("^\\.", "", extension))
  if (!extension %in% c("heic", "heif")) {
    return(callback(path))
  }

  jpeg_path <- tempfile(
    pattern = "ygwater-heif-upload-",
    fileext = ".jpg"
  )
  on.exit(unlink(jpeg_path, force = TRUE), add = TRUE)
  convert_heif_to_jpeg(path, jpeg_path, extension = extension)
  callback(jpeg_path)
}
