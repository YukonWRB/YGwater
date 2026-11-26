# Scan a folder of photos (including all sub-folders) and write a report
# folder describing them:
#
#   <output>/photo_manifest.csv   one row per image: file name, folder, size,
#                                 checksum/duplicates, the fields the AquaLink
#                                 image upload uses (Datetime, Latitude,
#                                 Longitude, Altitude, Azimuth), a geolocated
#                                 TRUE/FALSE flag and every other EXIF tag
#   <output>/photo_manifest.xlsx  the same table (only with xlsx = TRUE)
#   <output>/map.html             interactive map: photos, view directions,
#                                 photo preview in the pop-up
#   <output>/plots/map_photos.png       where the photos were taken
#   <output>/plots/map_directions.png   which way each camera was pointing
#   <output>/plots/timeline.png         photos over time
#   <output>/plots/photos_per_folder.png
#   <output>/summary.txt          counts, date range, cameras, problems
#   <output>/fews/                Delft-FEWS import (fews = TRUE): a location
#                                 set CSV (one location per geolocated photo)
#                                 and a PI-XML file with each photo's capture
#                                 time as a time series; see fews/README.txt
#
# The manifest is meant as the input to a bulk photo upload: review/fill in
# the summary columns, then feed it to the upload step.
#
# Usage (from the repo root or anywhere):
#   source("dev/scan_photo_folder.R")
#   photos <- scan_photo_folder("D:/photo_dump", "D:/photo_dump_report")
#
# Or from a terminal:
#   Rscript dev/scan_photo_folder.R "D:/photo_dump" "D:/photo_dump_report"
#
# Requires exifr (bundles ExifTool; needs Perl, which Rtools provides on
# Windows), ggplot2, sf and leaflet/htmlwidgets for the plots and map, and
# openxlsx for the optional .xlsx.

image_extensions <- c(
  "jpg",
  "jpeg",
  "jpe",
  "png",
  "tif",
  "tiff",
  "heic",
  "heif",
  "gif",
  "bmp",
  "webp",
  "dng",
  "cr2",
  "cr3",
  "nef",
  "arw",
  "orf",
  "rw2",
  "raf"
)

#' Scan a folder tree for images and write a manifest, plots and map
#'
#' @param folder Folder to scan. Sub-folders are included.
#' @param output Folder to write the report into (created if needed). NULL to
#'   skip writing and just return the table. Defaults to a
#'   "<folder>_photo_report" folder next to the scanned one.
#' @param extensions File extensions (case-insensitive) treated as images.
#' @param checksum Add an MD5 checksum and flag exact duplicate files. Reads
#'   every file in full, so it can be turned off for very large dumps.
#' @param xlsx Also write the manifest as an Excel file.
#' @param plots Write the PNG plots and the interactive map.
#' @param flight_gap_minutes Geolocated photos from the same camera are joined
#'   into an approximate flight path in capture-time order; a gap longer than
#'   this starts a new flight. Each photo gets a `flight_id`.
#' @param fews Write the Delft-FEWS location set and PI-XML time series.
#' @param utc_offset_hours Time zone of the cameras' clocks, in hours from UTC
#'   (EXIF times carry no zone). Written to the PI-XML <timeZone>. Default -7,
#'   Yukon time.
#' @param batch_size Files per ExifTool call (for progress messages and so one
#'   unreadable file does not stop the scan).
#' @return A data.frame, one row per image, invisibly.
scan_photo_folder <- function(
  folder,
  output = paste0(sub("[/\\\\]+$", "", folder), "_photo_report"),
  extensions = image_extensions,
  checksum = TRUE,
  xlsx = FALSE,
  plots = TRUE,
  flight_gap_minutes = 10,
  fews = TRUE,
  utc_offset_hours = -7,
  batch_size = 200L
) {
  if (!requireNamespace("exifr", quietly = TRUE)) {
    stop("Install the exifr package: install.packages('exifr')")
  }
  folder <- normalizePath(folder, winslash = "/", mustWork = TRUE)
  if (!dir.exists(folder)) {
    stop("Not a folder: ", folder)
  }
  force(output)
  # The photos are read only, never modified. The report must live outside
  # the photo folder so writing it can never overwrite or delete a photo (and
  # so later scans don't pick up the report's own PNGs).
  if (!is.null(output)) {
    check_output_outside(output, folder)
  }

  # Find images -----------------------------------------------------------
  paths <- list.files(
    folder,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    include.dirs = FALSE
  )
  ext <- tolower(tools::file_ext(paths))
  skipped <- sum(!ext %in% extensions)
  paths <- paths[ext %in% extensions]
  ext <- ext[ext %in% extensions]
  message(sprintf(
    "Found %d image(s) in %s (%d other file(s) skipped).",
    length(paths),
    folder,
    skipped
  ))
  if (!length(paths)) {
    return(invisible(data.frame()))
  }

  # File-system information -------------------------------------------------
  # (also the snapshot used at the end to confirm no photo was changed)
  info <- file.info(paths, extra_cols = FALSE)
  relative <- substring(paths, nchar(folder) + 2L)
  sub_folder <- dirname(relative)
  sub_folder[sub_folder == "."] <- ""

  files <- data.frame(
    file_name = basename(paths),
    sub_folder = sub_folder,
    relative_path = relative,
    full_path = paths,
    extension = ext,
    size_bytes = as.numeric(info$size),
    size_mb = round(as.numeric(info$size) / 1024^2, 3),
    file_modified = format(info$mtime, "%Y-%m-%d %H:%M:%S"),
    file_created = format(info$ctime, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )

  if (checksum) {
    message("Computing checksums...")
    files$md5 <- unname(tools::md5sum(paths))
    dup <- duplicated(files$md5) | duplicated(files$md5, fromLast = TRUE)
    first_copy <- files$relative_path[match(files$md5, files$md5)]
    files$duplicate_of <- ifelse(
      dup & first_copy != files$relative_path,
      first_copy,
      NA_character_
    )
  }

  # EXIF ------------------------------------------------------------------
  exif <- read_exif_batches(paths, batch_size)

  # Columns ExifTool repeats from the file system (we already have them)
  drop <- c(
    "SourceFile",
    "FileName",
    "Directory",
    "FileSize",
    "FileModifyDate",
    "FileAccessDate",
    "FileCreateDate",
    "FilePermissions",
    "FileTypeExtension"
  )
  exif <- exif[, setdiff(names(exif), drop), drop = FALSE]
  exif <- flatten_exif(exif)

  # Summary columns, named as the AquaLink image upload names them -------
  dt <- pick_datetime(exif, files$file_modified)
  lat <- num_col(exif, "GPSLatitude")
  lon <- num_col(exif, "GPSLongitude")
  # Phones write 0,0 when they have no fix; anything off the globe is junk
  bad_fix <- !is.na(lat) &
    !is.na(lon) &
    ((lat == 0 & lon == 0) | abs(lat) > 90 | abs(lon) > 180)
  lat[bad_fix] <- NA
  lon[bad_fix] <- NA
  alt <- num_col(exif, "GPSAltitude")
  alt[bad_fix] <- NA
  azimuth <- num_col(exif, "GPSImgDirection")
  azimuth[!is.na(azimuth) & (azimuth < 0 | azimuth > 360)] <- NA

  summary <- data.frame(
    Datetime = dt$value,
    datetime_source = dt$source,
    geolocated = !is.na(lat) & !is.na(lon),
    Latitude = lat,
    Longitude = lon,
    Altitude = alt,
    Azimuth = azimuth,
    camera_make = chr_col(exif, "Make"),
    camera_model = chr_col(exif, "Model"),
    width_px = num_col(exif, c("ImageWidth", "ExifImageWidth")),
    height_px = num_col(exif, c("ImageHeight", "ExifImageHeight")),
    orientation = num_col(exif, "Orientation"),
    # ExifTool's "Error" tag (e.g. corrupt or truncated file) or our own
    metadata_error = coalesce_chr(
      chr_col(exif, "Error"),
      chr_col(exif, "exif_error")
    ),
    stringsAsFactors = FALSE
  )
  exif$Error <- NULL
  exif$exif_error <- NULL

  fov <- horizontal_fov(exif, summary)
  summary$fov_deg <- fov$deg
  summary$fov_source <- fov$source
  summary$flight_id <- assign_flights(summary, flight_gap_minutes)
  # Put the field of view and flight_id right after Azimuth
  added <- c("fov_deg", "fov_source", "flight_id")
  cols <- setdiff(names(summary), added)
  summary <- summary[, append(cols, added, after = match("Azimuth", cols))]

  out <- cbind(files, summary, exif, stringsAsFactors = FALSE)
  out <- out[order(out$sub_folder, out$Datetime, out$file_name), ]
  rownames(out) <- NULL

  message(sprintf(
    "%d with a capture time, %d geolocated, %d with a direction, %d flight(s), %d duplicate file(s), %d unreadable.",
    sum(out$datetime_source %in% c("DateTimeOriginal", "CreateDate")),
    sum(out$geolocated),
    sum(out$geolocated & !is.na(out$Azimuth)),
    length(unique(stats::na.omit(out$flight_id))),
    if (checksum) sum(!is.na(out$duplicate_of)) else NA_integer_,
    sum(!is.na(out$metadata_error))
  ))

  if (!is.null(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    output <- normalizePath(output, winslash = "/")
    written <- write_csv_utf8(out, file.path(output, "photo_manifest.csv"))
    if (xlsx) {
      written <- c(
        written,
        write_xlsx(out, file.path(output, "photo_manifest.xlsx"))
      )
    }
    if (plots) {
      written <- c(written, write_plots(out, output))
    }
    if (fews) {
      written <- c(
        written,
        write_fews(out, file.path(output, "fews"), utc_offset_hours)
      )
    }
    written <- c(
      written,
      write_summary(
        out,
        folder,
        skipped,
        file.path(output, "summary.txt"),
        flight_gap_minutes
      )
    )
    message(
      "Wrote to ",
      output,
      ":\n  ",
      paste(substring(written, nchar(output) + 2L), collapse = "\n  ")
    )
  }

  # Confirm every photo still has the size and modified time it had before
  # the scan (and the same checksum, when checksums were computed)
  after <- file.info(paths, extra_cols = FALSE)
  changed <- is.na(after$size) |
    after$size != info$size |
    as.numeric(after$mtime) != as.numeric(info$mtime)
  if (checksum && !any(changed)) {
    changed <- unname(tools::md5sum(paths)) != files$md5
  }
  if (any(changed)) {
    warning(
      sum(changed),
      " photo(s) changed while the scan ran (by another program?): ",
      paste(utils::head(relative[changed], 10), collapse = ", ")
    )
  } else {
    message("Checked: all ", length(paths), " photos are unchanged.")
  }
  invisible(out)
}

# Stop if `output` is the photo folder or inside it
check_output_outside <- function(output, folder) {
  norm <- function(p) {
    p <- normalizePath(p, winslash = "/", mustWork = FALSE)
    p <- sub("/+$", "", p)
    if (.Platform$OS.type == "windows") tolower(p) else p
  }
  out_n <- norm(output)
  folder_n <- norm(folder)
  if (out_n == folder_n || startsWith(out_n, paste0(folder_n, "/"))) {
    stop(
      "The output folder must be outside the photo folder, so the report ",
      "can never overwrite a photo:\n  photos: ",
      folder,
      "\n  output: ",
      output
    )
  }
  invisible(TRUE)
}

# Read EXIF in batches; if a batch fails, retry its files one at a time so a
# single corrupt file only loses its own row. exifr runs ExifTool in read
# mode only (-n -j -q -b, no tag assignments), so photos are never written.
read_exif_batches <- function(paths, batch_size) {
  batches <- split(paths, ceiling(seq_along(paths) / batch_size))
  rows <- vector("list", length(batches))
  for (i in seq_along(batches)) {
    message(sprintf(
      "Reading metadata: batch %d of %d (%d files)",
      i,
      length(batches),
      length(batches[[i]])
    ))
    res <- tryCatch(
      exifr::read_exif(batches[[i]], args = c("-charset", "filename=utf8")),
      error = function(e) NULL
    )
    if (is.null(res) || nrow(res) != length(batches[[i]])) {
      res <- lapply(batches[[i]], function(p) {
        r <- tryCatch(
          exifr::read_exif(p, args = c("-charset", "filename=utf8")),
          error = function(e) NULL
        )
        if (is.null(r) || nrow(r) == 0L) {
          r <- data.frame(
            SourceFile = p,
            exif_error = "could not read metadata"
          )
        }
        r
      })
      res <- do.call(plyr::rbind.fill, res)
    }
    rows[[i]] <- res
  }
  exif <- as.data.frame(do.call(plyr::rbind.fill, rows))

  # Line rows back up with `paths` (ExifTool may reorder or normalise them)
  key <- normalizePath(exif$SourceFile, winslash = "/", mustWork = FALSE)
  exif <- exif[match(paths, key), , drop = FALSE]
  missing <- is.na(exif$SourceFile)
  if (any(missing)) {
    if (!"exif_error" %in% names(exif)) {
      exif$exif_error <- NA_character_
    }
    exif$exif_error[missing] <- "could not read metadata"
  }
  exif
}

# List-columns become "a; b; c", embedded thumbnails/binary blobs are dropped,
# and long strings are cut to fit an Excel cell.
flatten_exif <- function(exif) {
  for (nm in names(exif)) {
    col <- exif[[nm]]
    if (is.list(col)) {
      col <- vapply(
        col,
        function(v) {
          if (is.null(v) || !length(v)) {
            NA_character_
          } else {
            paste(v, collapse = "; ")
          }
        },
        character(1)
      )
    }
    if (is.character(col)) {
      if (any(startsWith(col[!is.na(col)], "base64:"))) {
        exif[[nm]] <- NULL
        next
      }
      too_long <- !is.na(col) & nchar(col) > 32000L
      col[too_long] <- paste0(substr(col[too_long], 1L, 32000L), "...")
    }
    exif[[nm]] <- col
  }
  # Drop tags that are empty for every file
  keep <- vapply(exif, function(v) !all(is.na(v) | v == ""), logical(1))
  exif[, keep, drop = FALSE]
}

# Capture time: DateTimeOriginal, then CreateDate, then the file's modified
# time. EXIF times are camera local time with no zone, so they are kept as
# text "YYYY-MM-DD HH:MM:SS"; the upload decides the time zone.
pick_datetime <- function(exif, fallback) {
  value <- rep(NA_character_, nrow(exif))
  source <- rep(NA_character_, nrow(exif))
  for (tag in c("DateTimeOriginal", "CreateDate")) {
    if (!tag %in% names(exif)) {
      next
    }
    raw <- as.character(exif[[tag]])
    parsed <- sub(
      "^(\\d{4}):(\\d{2}):(\\d{2}) (\\d{2}:\\d{2}:\\d{2}).*$",
      "\\1-\\2-\\3 \\4",
      raw
    )
    ok <- is.na(value) &
      !is.na(raw) &
      grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", parsed) &
      !startsWith(raw, "0000")
    value[ok] <- parsed[ok]
    source[ok] <- tag
  }
  none <- is.na(value)
  value[none] <- fallback[none]
  source[none] <- "file_modified"
  list(value = value, source = source)
}

num_col <- function(exif, tags) {
  out <- rep(NA_real_, nrow(exif))
  for (tag in tags) {
    if (!tag %in% names(exif)) {
      next
    }
    v <- suppressWarnings(as.numeric(exif[[tag]]))
    out[is.na(out)] <- v[is.na(out)]
  }
  out
}

coalesce_chr <- function(a, b) {
  a[is.na(a) | a == ""] <- b[is.na(a) | a == ""]
  a
}

chr_col <- function(exif, tag) {
  if (tag %in% names(exif)) {
    as.character(exif[[tag]])
  } else {
    rep(NA_character_, nrow(exif))
  }
}

# Output files ---------------------------------------------------------------

# UTF-8 with a byte-order mark so Excel shows accented names correctly
write_csv_utf8 <- function(out, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  cat(intToUtf8(0xFEFFL), file = con)
  utils::write.table(
    out,
    con,
    sep = ",",
    row.names = FALSE,
    na = "",
    qmethod = "double"
  )
  path
}

write_xlsx <- function(out, path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("openxlsx is not installed; skipping the .xlsx")
    return(character())
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "photos")
  openxlsx::writeDataTable(wb, "photos", out, tableStyle = "TableStyleLight9")
  openxlsx::freezePane(wb, "photos", firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths(wb, "photos", cols = seq_along(out), widths = "auto")
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

write_summary <- function(out, folder, skipped, path, flight_gap_minutes) {
  dts <- as.POSIXct(out$Datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  count_table <- function(x, label) {
    x[is.na(x) | x == ""] <- label
    tab <- sort(table(x), decreasing = TRUE)
    sprintf("  %6d  %s", as.integer(tab), names(tab))
  }
  geo <- out$geolocated
  lines <- c(
    "Photo folder scan",
    paste0("Folder:  ", folder),
    paste0("Scanned: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "",
    sprintf(
      "Images:                 %d (%d other files skipped)",
      nrow(out),
      skipped
    ),
    sprintf("Total size:             %.1f MB", sum(out$size_bytes) / 1024^2),
    sprintf(
      "Capture time from EXIF: %d (others use the file's modified time)",
      sum(out$datetime_source %in% c("DateTimeOriginal", "CreateDate"))
    ),
    sprintf(
      "Date range:             %s to %s",
      format(min(dts, na.rm = TRUE)),
      format(max(dts, na.rm = TRUE))
    ),
    sprintf("Geolocated:             %d of %d", sum(geo), nrow(out)),
    sprintf("With view direction:    %d", sum(geo & !is.na(out$Azimuth))),
    sprintf(
      "Flights (approximate):  %d (gaps over %g min or a camera change start a new one)",
      length(unique(stats::na.omit(out$flight_id))),
      flight_gap_minutes
    ),
    if (sum(geo)) {
      sprintf(
        "Extent:                 lat %.5f to %.5f, lon %.5f to %.5f",
        min(out$Latitude, na.rm = TRUE),
        max(out$Latitude, na.rm = TRUE),
        min(out$Longitude, na.rm = TRUE),
        max(out$Longitude, na.rm = TRUE)
      )
    },
    if ("duplicate_of" %in% names(out)) {
      sprintf("Duplicate files:        %d", sum(!is.na(out$duplicate_of)))
    },
    sprintf("Unreadable metadata:    %d", sum(!is.na(out$metadata_error))),
    "",
    "Photos per folder:",
    count_table(out$sub_folder, "(top folder)"),
    "",
    "Cameras:",
    count_table(
      trimws(paste(
        ifelse(is.na(out$camera_make), "", out$camera_make),
        ifelse(is.na(out$camera_model), "", out$camera_model)
      )),
      "(unknown)"
    )
  )
  if (any(!is.na(out$metadata_error))) {
    bad <- out[!is.na(out$metadata_error), ]
    lines <- c(
      lines,
      "",
      "Files with metadata errors:",
      sprintf("  %s: %s", bad$relative_path, bad$metadata_error)
    )
  }
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  path
}

# Plots and map ----------------------------------------------------------------

# Chart colours: fixed categorical order, muted grey for "no GPS"
plot_colours <- list(
  series = c(
    "#2a78d6",
    "#eb6834",
    "#1baf7a",
    "#eda100",
    "#e87ba4",
    "#008300",
    "#6250d6",
    "#e34948"
  ),
  muted = "#898781",
  ink = "#0b0b0b",
  ink_2 = "#52514e",
  grid = "#e1e0d9",
  surface = "#fcfcfb"
)

plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = plot_colours$surface,
        colour = NA
      ),
      panel.grid.major = ggplot2::element_line(
        colour = plot_colours$grid,
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(colour = plot_colours$ink_2),
      plot.title = ggplot2::element_text(
        colour = plot_colours$ink,
        face = "bold",
        size = 13
      ),
      plot.subtitle = ggplot2::element_text(colour = plot_colours$ink_2),
      axis.text = ggplot2::element_text(colour = plot_colours$muted),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(12, 16, 12, 12)
    )
}

# Folder label with at most 8 colours: biggest 7 folders, the rest "Other"
folder_groups <- function(sub_folder) {
  f <- ifelse(sub_folder == "", "(top folder)", sub_folder)
  counts <- sort(table(f), decreasing = TRUE)
  if (length(counts) > 8L) {
    keep <- names(counts)[1:7]
    f[!f %in% keep] <- "Other"
    levels <- c(keep, "Other")
  } else {
    levels <- names(counts)
  }
  factor(f, levels = levels)
}

# Approximate flights: geolocated photos with an EXIF capture time, per
# camera, in time order; a gap longer than `gap_minutes` starts a new flight.
# Flights (2+ photos) are numbered 1, 2, ... by start time; other photos
# get NA.
assign_flights <- function(s, gap_minutes) {
  id <- rep(NA_integer_, nrow(s))
  t <- as.POSIXct(s$Datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  use <- which(
    s$geolocated &
      s$datetime_source %in% c("DateTimeOriginal", "CreateDate") &
      !is.na(t)
  )
  if (!length(use)) {
    return(id)
  }
  camera <- paste(s$camera_make, s$camera_model)[use]
  ord <- use[order(camera, t[use])]
  cam_ord <- paste(s$camera_make, s$camera_model)[ord]
  gap <- c(Inf, as.numeric(diff(t[ord]), units = "mins"))
  new_flight <- gap > gap_minutes |
    c(TRUE, cam_ord[-1] != cam_ord[-length(ord)])
  raw <- cumsum(new_flight)
  # A flight needs at least two photos; lone photos keep NA
  sizes <- table(raw)
  keep <- raw %in% as.integer(names(sizes)[sizes >= 2L])
  if (!any(keep)) {
    return(id)
  }
  # Renumber by flight start time
  starts <- tapply(as.numeric(t[ord][keep]), raw[keep], min)
  id[ord[keep]] <- match(raw[keep], as.integer(names(starts))[order(starts)])
  id
}

# Approximate horizontal field of view (degrees) of each photo as it is
# displayed (portrait photos see a narrower slice of the horizon):
#   1. 35 mm-equivalent focal length (FocalLengthIn35mmFormat, or FocalLength x
#      ScaleFactor35efl), with the sensor shape taken from the pixel size
#   2. ExifTool's FOV tag (computed for the long side of the image)
#   3. otherwise an assumed 60 degrees, a typical phone/drone wide lens
horizontal_fov <- function(exif, s) {
  n <- nrow(exif)
  w <- s$width_px
  h <- s$height_px
  aspect <- ifelse(
    is.na(w) | is.na(h) | w <= 0 | h <= 0,
    4 / 3,
    pmax(w, h) / pmin(w, h)
  )
  rotated <- !is.na(s$orientation) & s$orientation %in% 5:8
  portrait <- (!is.na(w) & !is.na(h) & h > w) != rotated

  # 35 mm equivalence is defined on the 43.27 mm full-frame diagonal
  diag_mm <- sqrt(36^2 + 24^2)
  long_mm <- diag_mm * cos(atan(1 / aspect))
  short_mm <- diag_mm * sin(atan(1 / aspect))
  f35 <- num_col(exif, "FocalLengthIn35mmFormat")
  f35_calc <- num_col(exif, "FocalLength") * num_col(exif, "ScaleFactor35efl")
  f35[is.na(f35) | f35 <= 0] <- f35_calc[is.na(f35) | f35 <= 0]
  f35[!is.na(f35) & f35 <= 0] <- NA
  deg <- 2 * atan(ifelse(portrait, short_mm, long_mm) / (2 * f35)) * 180 / pi
  source <- ifelse(is.na(deg), NA_character_, "focal_length_35mm")

  exif_fov <- num_col(exif, "FOV")
  exif_fov[!is.na(exif_fov) & (exif_fov <= 0 | exif_fov >= 180)] <- NA
  from_tag <- is.na(deg) & !is.na(exif_fov)
  short_fov <- 2 * atan(tan(exif_fov / 2 * pi / 180) / aspect) * 180 / pi
  deg[from_tag] <- ifelse(portrait, short_fov, exif_fov)[from_tag]
  source[from_tag] <- "exif_fov"

  deg[is.na(deg)] <- 60
  source[is.na(source)] <- "assumed"
  list(deg = round(pmin(pmax(deg, 1), 179), 1), source = source)
}

# Field-of-view wedge for each photo: tip at the photo, opening along
# `bearing` with total angle `fov`, reaching `len` metres. Returns lon/lat
# vertices with a group id (one polygon per photo).
fov_wedges <- function(lat, lon, bearing, fov, len, steps = 16L) {
  do.call(
    rbind,
    lapply(seq_along(lat), function(i) {
      arc <- bearing[i] + seq(-fov[i] / 2, fov[i] / 2, length.out = steps)
      pts <- destination(rep(lat[i], steps), rep(lon[i], steps), arc, len)
      data.frame(
        lon = c(lon[i], pts[, "lon"]),
        lat = c(lat[i], pts[, "lat"]),
        group = i
      )
    })
  )
}

# Point `dist` metres from lat/lon along a true-north bearing (spherical earth)
destination <- function(lat, lon, bearing, dist) {
  r <- 6371008.8
  p1 <- lat * pi / 180
  l1 <- lon * pi / 180
  b <- bearing * pi / 180
  d <- dist / r
  p2 <- asin(sin(p1) * cos(d) + cos(p1) * sin(d) * cos(b))
  l2 <- l1 + atan2(sin(b) * sin(d) * cos(p1), cos(d) - sin(p1) * sin(p2))
  cbind(lon = l2 * 180 / pi, lat = p2 * 180 / pi)
}

# Start of the hour/day/week (Monday)/month containing each time (UTC clock)
floor_time <- function(t, unit) {
  d <- as.Date(t, tz = "UTC")
  start <- switch(
    unit,
    hour = return(as.POSIXct(
      format(t, "%Y-%m-%d %H:00:00", tz = "UTC"),
      tz = "UTC"
    )),
    day = d,
    week = d - (as.integer(format(d, "%u")) - 1L),
    month = as.Date(format(d, "%Y-%m-01"))
  )
  as.POSIXct(format(start), tz = "UTC")
}

save_png <- function(plot, path, width = 8, height = 6) {
  ggplot2::ggsave(
    path,
    plot,
    width = width,
    height = height,
    dpi = 150,
    bg = plot_colours$surface,
    device = if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png
    } else {
      "png"
    }
  )
  path
}

write_plots <- function(out, output) {
  for (p in c("ggplot2", "sf")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      warning("Package ", p, " is not installed; skipping plots")
      return(character())
    }
  }
  plot_dir <- file.path(output, "plots")
  dir.create(plot_dir, showWarnings = FALSE)
  written <- character()
  out$folder_group <- folder_groups(out$sub_folder)
  out$gps <- factor(
    ifelse(out$geolocated, "Geolocated", "No GPS"),
    levels = c("Geolocated", "No GPS")
  )
  gps_fill <- c(
    Geolocated = plot_colours$series[[1]],
    `No GPS` = plot_colours$muted
  )
  folder_fill <- stats::setNames(
    c(plot_colours$series, plot_colours$muted)[seq_along(levels(
      out$folder_group
    ))],
    levels(out$folder_group)
  )
  if ("Other" %in% names(folder_fill)) {
    folder_fill[["Other"]] <- plot_colours$muted
  }

  # Timeline ---------------------------------------------------------------
  dts <- as.POSIXct(out$Datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  ok <- !is.na(dts)
  if (any(ok)) {
    span_days <- as.numeric(difftime(
      max(dts[ok]),
      min(dts[ok]),
      units = "days"
    ))
    bin <- if (span_days <= 2) {
      list(unit = "hour", label = "hour", fmt = "%b %d %H:%M")
    } else if (span_days <= 90) {
      list(unit = "day", label = "day", fmt = "%b %d")
    } else if (span_days <= 3 * 365) {
      list(unit = "week", label = "week", fmt = "%b %Y")
    } else {
      list(unit = "month", label = "month", fmt = "%Y")
    }
    tl <- data.frame(bin = floor_time(dts[ok], bin$unit), gps = out$gps[ok])
    counts <- stats::aggregate(
      list(n = rep(1L, nrow(tl))),
      by = list(bin = tl$bin, gps = tl$gps),
      FUN = sum
    )
    width_s <- switch(
      bin$unit,
      hour = 3600,
      day = 86400,
      week = 7 * 86400,
      month = 30 * 86400
    )
    counts$bin <- counts$bin + width_s / 2
    p <- ggplot2::ggplot(counts, ggplot2::aes(bin, n, fill = gps)) +
      ggplot2::geom_col(
        width = width_s * 0.9,
        colour = plot_colours$surface,
        linewidth = 0.3
      ) +
      ggplot2::scale_fill_manual(values = gps_fill, name = NULL, drop = FALSE) +
      ggplot2::scale_x_datetime(
        date_labels = bin$fmt,
        expand = ggplot2::expansion(mult = 0.02)
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        title = "Photos over time",
        subtitle = sprintf(
          "%d photos, counted per %s; capture time from EXIF where present (camera local time)",
          sum(ok),
          bin$label
        ),
        x = NULL,
        y = paste("Photos per", bin$label)
      ) +
      plot_theme() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    written <- c(
      written,
      save_png(p, file.path(plot_dir, "timeline.png"), 9, 4.5)
    )
  }

  # Photos per folder ------------------------------------------------------
  pf <- as.data.frame(table(
    folder = ifelse(out$sub_folder == "", "(top folder)", out$sub_folder),
    gps = out$gps
  ))
  totals <- tapply(pf$Freq, pf$folder, sum)
  top <- names(sort(totals, decreasing = TRUE))[seq_len(min(
    30L,
    length(totals)
  ))]
  pf <- pf[pf$folder %in% top & pf$Freq > 0, ]
  pf$folder <- factor(pf$folder, levels = rev(top))
  p <- ggplot2::ggplot(pf, ggplot2::aes(Freq, folder, fill = gps)) +
    ggplot2::geom_col(
      width = 0.7,
      colour = plot_colours$surface,
      linewidth = 0.3
    ) +
    ggplot2::scale_fill_manual(values = gps_fill, name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      title = "Photos per folder",
      subtitle = if (length(totals) > 30L) {
        sprintf("Largest 30 of %d folders", length(totals))
      },
      x = "Photos",
      y = NULL
    ) +
    plot_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  written <- c(
    written,
    save_png(
      p,
      file.path(plot_dir, "photos_per_folder.png"),
      8,
      max(3, 1.2 + 0.28 * length(top))
    )
  )

  # Maps ---------------------------------------------------------------------
  geo <- out[out$geolocated, ]
  if (!nrow(geo)) {
    message("No geolocated photos: skipping maps.")
    return(written)
  }

  # Local azimuthal equidistant projection centred on the photos: true scale
  # and north-up around the area of interest, wherever it is
  crs <- sprintf(
    "+proj=aeqd +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m",
    mean(range(geo$Latitude)),
    mean(range(geo$Longitude))
  )
  pts <- sf::st_as_sf(
    geo,
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )
  xy <- sf::st_coordinates(sf::st_transform(pts, crs))
  geo$x <- xy[, 1]
  geo$y <- xy[, 2]
  diag <- sqrt(diff(range(geo$x))^2 + diff(range(geo$y))^2)
  pad <- max(diag * 0.08, 50)
  lims <- list(
    x = range(geo$x) + c(-pad, pad),
    y = range(geo$y) + c(-pad, pad)
  )
  scale_label <- function(m) {
    if (m >= 1000) {
      sprintf("%s km", format(m / 1000))
    } else {
      sprintf("%s m", format(m))
    }
  }
  nice_len <- function(target) {
    steps <- c(1, 2, 5) * rep(10^(0:6), each = 3)
    steps[max(which(steps <= target), 1)]
  }
  bar_len <- nice_len(diff(lims$x) / 5)
  scale_bar <- list(
    ggplot2::annotate(
      "segment",
      x = lims$x[1] + pad / 2,
      xend = lims$x[1] + pad / 2 + bar_len,
      y = lims$y[1] + pad / 2,
      yend = lims$y[1] + pad / 2,
      linewidth = 1,
      colour = plot_colours$ink_2
    ),
    ggplot2::annotate(
      "text",
      x = lims$x[1] + pad / 2 + bar_len / 2,
      y = lims$y[1] + pad / 2,
      label = scale_label(bar_len),
      vjust = -0.6,
      size = 3,
      colour = plot_colours$ink_2
    )
  )
  map_coord <- ggplot2::coord_sf(
    crs = crs,
    datum = sf::st_crs(4326),
    xlim = lims$x,
    ylim = lims$y,
    expand = FALSE
  )

  p <- ggplot2::ggplot(geo, ggplot2::aes(x, y, fill = folder_group)) +
    ggplot2::geom_point(
      shape = 21,
      size = 2.6,
      colour = plot_colours$surface,
      stroke = 0.5,
      alpha = 0.9
    ) +
    scale_bar +
    ggplot2::scale_fill_manual(values = folder_fill, name = "Folder") +
    map_coord +
    ggplot2::labs(
      title = "Where the photos were taken",
      subtitle = sprintf(
        "%d of %d photos geolocated",
        nrow(geo),
        nrow(out)
      ),
      x = NULL,
      y = NULL
    ) +
    plot_theme() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(size = 4),
        nrow = 2
      )
    )
  written <- c(
    written,
    save_png(p, file.path(plot_dir, "map_photos.png"), 8, 8)
  )

  # View directions: a field-of-view wedge from each photo, centred on its
  # azimuth (true north)
  dir_pts <- geo[!is.na(geo$Azimuth), ]
  if (nrow(dir_pts)) {
    tri <- fov_wedges(
      dir_pts$Latitude,
      dir_pts$Longitude,
      dir_pts$Azimuth,
      dir_pts$fov_deg,
      max(diag * 0.045, 20)
    )
    tri_xy <- sf::st_coordinates(sf::st_transform(
      sf::st_as_sf(tri, coords = c("lon", "lat"), crs = 4326),
      crs
    ))
    tri$x <- tri_xy[, 1]
    tri$y <- tri_xy[, 2]
    tri$folder_group <- dir_pts$folder_group[tri$group]
  }
  no_dir <- geo[is.na(geo$Azimuth), ]
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = no_dir,
      ggplot2::aes(x, y),
      shape = 21,
      size = 2.2,
      fill = NA,
      colour = plot_colours$muted,
      stroke = 0.6
    ) +
    scale_bar +
    map_coord +
    ggplot2::labs(
      title = "Which way the camera was pointing",
      subtitle = sprintf(
        "%d of %d geolocated photos record a direction; wedges show the approximate\nfield of view from true north (%d with the angle from EXIF)%s",
        nrow(dir_pts),
        nrow(geo),
        sum(dir_pts$fov_source != "assumed"),
        if (nrow(no_dir)) ", open circles have no direction" else ""
      ),
      x = NULL,
      y = NULL
    ) +
    plot_theme()
  if (nrow(dir_pts)) {
    p <- p +
      ggplot2::geom_polygon(
        data = tri,
        ggplot2::aes(
          x,
          y,
          group = group,
          fill = folder_group,
          colour = folder_group
        ),
        alpha = 0.3,
        linewidth = 0.35,
        show.legend = FALSE
      ) +
      ggplot2::scale_colour_manual(values = folder_fill, guide = "none") +
      ggplot2::geom_point(
        data = dir_pts,
        ggplot2::aes(x, y, fill = folder_group),
        shape = 21,
        size = 1.8,
        colour = plot_colours$surface,
        stroke = 0.4
      ) +
      ggplot2::scale_fill_manual(values = folder_fill, name = "Folder") +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          override.aes = list(shape = 21, size = 4),
          nrow = 2
        )
      )
  }
  written <- c(
    written,
    save_png(p, file.path(plot_dir, "map_directions.png"), 8, 8)
  )

  # Interactive map ------------------------------------------------------------
  html <- write_leaflet_map(geo, folder_fill, file.path(output, "map.html"))
  c(written, html)
}

# One line per flight (2+ photos) in capture-time order, with a hover label
# and a pop-up: photos, start/end, duration, distance, camera
flight_lines <- function(geo) {
  g <- geo[!is.na(geo$flight_id), ]
  ids <- as.integer(names(which(table(g$flight_id) >= 2L)))
  if (!length(ids)) {
    return(list(lines = NULL, label = NULL, popup = NULL))
  }
  haversine <- function(lat1, lon1, lat2, lon2) {
    to_rad <- pi / 180
    a <- sin((lat2 - lat1) * to_rad / 2)^2 +
      cos(lat1 * to_rad) *
        cos(lat2 * to_rad) *
        sin((lon2 - lon1) * to_rad / 2)^2
    2 * 6371008.8 * asin(pmin(1, sqrt(a)))
  }
  info <- lapply(ids, function(id) {
    f <- g[g$flight_id == id, ]
    f <- f[order(f$Datetime, f$file_name), ]
    n <- nrow(f)
    km <- sum(haversine(
      f$Latitude[-n],
      f$Longitude[-n],
      f$Latitude[-1],
      f$Longitude[-1]
    )) /
      1000
    t <- as.POSIXct(f$Datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    mins <- as.numeric(difftime(t[n], t[1], units = "mins"))
    camera <- trimws(paste(
      ifelse(is.na(f$camera_make[1]), "", f$camera_make[1]),
      ifelse(is.na(f$camera_model[1]), "", f$camera_model[1])
    ))
    folders <- unique(ifelse(f$sub_folder == "", "(top folder)", f$sub_folder))
    list(
      line = sf::st_linestring(cbind(f$Longitude, f$Latitude)),
      label = sprintf(
        "Flight %d: %s to %s, %d photos",
        id,
        format(t[1], "%Y-%m-%d %H:%M"),
        format(t[n], "%H:%M"),
        n
      ),
      popup = sprintf(
        paste0(
          "<b>Flight %d</b> (approximate)<br>%d photos<br>",
          "%s to %s (%.0f min)<br>%.2f km between photos%s<br>",
          "Camera: %s<br>Folder%s: %s"
        ),
        id,
        n,
        format(t[1], "%Y-%m-%d %H:%M:%S"),
        format(t[n], "%H:%M:%S"),
        mins,
        km,
        if (mins > 0) sprintf(" (avg %.0f km/h)", km / (mins / 60)) else "",
        if (nzchar(camera)) camera else "unknown",
        if (length(folders) > 1L) "s" else "",
        paste(utils::head(folders, 5), collapse = ", ")
      )
    )
  })
  list(
    lines = sf::st_sfc(lapply(info, `[[`, "line"), crs = 4326),
    label = vapply(info, `[[`, character(1), "label"),
    popup = vapply(info, `[[`, character(1), "popup")
  )
}

# Interactive map with satellite/topo base layers, folder colours, field of
# view wedges, approximate flight paths and a pop-up showing the photo
# (loaded from disk when opened locally)
write_leaflet_map <- function(geo, folder_fill, path) {
  if (
    !requireNamespace("leaflet", quietly = TRUE) ||
      !requireNamespace("htmlwidgets", quietly = TRUE)
  ) {
    warning("leaflet/htmlwidgets are not installed; skipping map.html")
    return(character())
  }
  esc <- function(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    gsub("\"", "&quot;", x, fixed = TRUE)
  }
  file_url <- paste0(
    "file:///",
    vapply(geo$full_path, utils::URLencode, character(1), USE.NAMES = FALSE)
  )
  browser_img <- geo$extension %in%
    c("jpg", "jpeg", "jpe", "png", "gif", "bmp", "webp")
  popup <- paste0(
    "<b>",
    esc(geo$relative_path),
    "</b><br>",
    esc(geo$Datetime),
    " (",
    esc(geo$datetime_source),
    ")<br>",
    sprintf("%.6f, %.6f", geo$Latitude, geo$Longitude),
    ifelse(is.na(geo$Altitude), "", sprintf(" &middot; %.0f m", geo$Altitude)),
    ifelse(
      is.na(geo$Azimuth),
      "",
      sprintf(
        " &middot; facing %.0f&deg;, ~%.0f&deg; view%s",
        geo$Azimuth,
        geo$fov_deg,
        ifelse(geo$fov_source == "assumed", " (assumed)", "")
      )
    ),
    ifelse(
      is.na(geo$flight_id),
      "",
      sprintf("<br>Flight %d (approximate)", geo$flight_id)
    ),
    "<br>",
    ifelse(
      browser_img,
      paste0(
        "<a href=\"",
        file_url,
        "\" target=\"_blank\"><img src=\"",
        file_url,
        "\" style=\"max-width:280px;max-height:210px;margin-top:6px\"></a>"
      ),
      paste0("<a href=\"", file_url, "\" target=\"_blank\">Open file</a>")
    )
  )
  pal <- leaflet::colorFactor(unname(folder_fill), levels = names(folder_fill))
  # Legend: only folders that have photos on the map
  folder_fill <- folder_fill[
    names(folder_fill) %in% as.character(geo$folder_group)
  ]

  m <- leaflet::leaflet(
    options = leaflet::leafletOptions(preferCanvas = TRUE)
  ) |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topographic") |>
    leaflet::addProviderTiles("OpenStreetMap", group = "Streets")

  # Approximate flight paths: photos of each flight joined in time order.
  # A dark casing under a white dashed line reads on every base map.
  flights <- flight_lines(geo)
  if (length(flights$lines)) {
    m <- m |>
      leaflet::addPolylines(
        data = flights$lines,
        color = "#0b0b0b",
        weight = 5,
        opacity = 0.45,
        group = "Flight paths"
      ) |>
      leaflet::addPolylines(
        data = flights$lines,
        color = "#ffffff",
        weight = 2.5,
        opacity = 0.95,
        dashArray = "8 5",
        label = flights$label,
        popup = flights$popup,
        highlightOptions = leaflet::highlightOptions(
          weight = 5,
          color = "#eda100",
          bringToFront = TRUE
        ),
        group = "Flight paths"
      )
  }

  m <- m |>
    leaflet::addCircleMarkers(
      lng = geo$Longitude,
      lat = geo$Latitude,
      radius = 6,
      color = "#ffffff",
      weight = 1.5,
      fillColor = pal(geo$folder_group),
      fillOpacity = 0.95,
      popup = popup,
      label = geo$relative_path,
      group = "Photos"
    )

  # View direction: a field-of-view wedge with its tip on the photo, opening
  # the way the camera faced. Drawn as screen-sized SVG (centred on the photo)
  # so it stays legible at every zoom; clicks pass through to the photo dot.
  has_dir <- !is.na(geo$Azimuth)
  if (any(has_dir)) {
    r <- 38
    half <- geo$fov_deg[has_dir] / 2 * pi / 180
    x <- r * sin(half)
    y <- -r * cos(half)
    tri_svg <- sprintf(
      paste0(
        "<svg width=\"%d\" height=\"%d\" viewBox=\"%d %d %d %d\" ",
        "style=\"display:block;overflow:visible;transform:rotate(%.1fdeg)\">",
        "<path d=\"M0,0 L%.2f,%.2f A%d,%d 0 %d 1 %.2f,%.2f Z\" ",
        "fill=\"%s\" fill-opacity=\"0.35\" stroke=\"%s\" stroke-width=\"1.5\" ",
        "stroke-linejoin=\"round\"/></svg>"
      ),
      2L * r,
      2L * r,
      -r,
      -r,
      2L * r,
      2L * r,
      geo$Azimuth[has_dir],
      -x,
      y,
      r,
      r,
      as.integer(geo$fov_deg[has_dir] > 180),
      x,
      y,
      pal(geo$folder_group[has_dir]),
      pal(geo$folder_group[has_dir])
    )
    m <- m |>
      leaflet::addLabelOnlyMarkers(
        lng = geo$Longitude[has_dir],
        lat = geo$Latitude[has_dir],
        label = lapply(tri_svg, htmltools::HTML),
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          offset = c(0, 0),
          className = "photo-dir"
        ),
        group = "View direction"
      ) |>
      htmlwidgets::prependContent(htmltools::tags$style(
        ".leaflet-tooltip.photo-dir { padding: 0; margin: 0; border: 0;",
        " background: none; box-shadow: none; line-height: 0; }",
        ".leaflet-tooltip.photo-dir:before { display: none; }"
      ))
  }

  m <- m |>
    leaflet::addLegend(
      "bottomright",
      colors = unname(folder_fill),
      labels = names(folder_fill),
      title = "Folder",
      opacity = 1
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Satellite", "Topographic", "Streets"),
      overlayGroups = c(
        "Photos",
        if (any(has_dir)) "View direction",
        if (length(flights$lines)) "Flight paths"
      ),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addScaleBar("bottomleft")

  selfcontained <- requireNamespace("rmarkdown", quietly = TRUE) &&
    rmarkdown::pandoc_available()
  htmlwidgets::saveWidget(
    m,
    path,
    selfcontained = selfcontained,
    libdir = if (!selfcontained) "map_files",
    title = "Photo map"
  )
  if (selfcontained) {
    # Everything is inlined; htmlwidgets can leave its staging folder behind
    unlink(file.path(dirname(path), "map_files"), recursive = TRUE)
  }
  path
}

# Delft-FEWS export ------------------------------------------------------------
#
# Every geolocated photo becomes a FEWS location (fixed position, attributes
# for folder, flight, direction, field of view and the photo's path), and gets
# short non-equidistant time series at its capture time so FEWS's time
# navigation shows a photo only when the view period covers it:
#   PHOTO          value 1 at the capture time (EXIF time only)
#   PHOTO_AZIMUTH  view direction in degrees from true north, at the same time
# Location ids come from the file checksum, so re-scans give the same ids.
write_fews <- function(out, dir, utc_offset_hours) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  g <- out[out$geolocated, ]
  # Exact duplicate files would share an id; keep the first copy only
  if ("duplicate_of" %in% names(g)) {
    g <- g[is.na(g$duplicate_of), ]
  }
  if (!nrow(g)) {
    message("No geolocated photos: skipping the FEWS export.")
    return(character())
  }
  if (!"md5" %in% names(g)) {
    g$md5 <- unname(tools::md5sum(g$full_path))
  }
  g$fews_id <- make.unique(paste0("PHOTO_", toupper(substr(g$md5, 1L, 12L))), sep = "_")
  g <- g[order(g$Datetime, g$relative_path), ]
  exif_time <- g$datetime_source %in% c("DateTimeOriginal", "CreateDate")
  camera <- trimws(paste(
    ifelse(is.na(g$camera_make), "", g$camera_make),
    ifelse(is.na(g$camera_model), "", g$camera_model)
  ))

  # 1. Location set -------------------------------------------------------
  loc <- data.frame(
    ID = g$fews_id,
    NAME = sprintf("%s (%s)", g$file_name, substr(g$Datetime, 1L, 16L)),
    LAT = round(g$Latitude, 7),
    LON = round(g$Longitude, 7),
    ALTITUDE = round(g$Altitude, 1),
    DATETIME = g$Datetime,
    DATETIME_SOURCE = g$datetime_source,
    FOLDER = ifelse(g$sub_folder == "", "(top folder)", g$sub_folder),
    FLIGHT = ifelse(is.na(g$flight_id), "", sprintf("Flight %d", g$flight_id)),
    FLIGHT_ID = g$flight_id,
    AZIMUTH = g$Azimuth,
    FOV = g$fov_deg,
    FOV_SOURCE = g$fov_source,
    CAMERA = camera,
    FILE_NAME = g$file_name,
    RELATIVE_PATH = g$relative_path,
    FILE_PATH = g$full_path,
    FILE_URL = paste0(
      "file:///",
      vapply(g$full_path, utils::URLencode, character(1), USE.NAMES = FALSE)
    ),
    MD5 = g$md5,
    stringsAsFactors = FALSE
  )
  loc_path <- file.path(dir, "photo_locations.csv")
  utils::write.csv(loc, loc_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")

  # 2. PI-XML time series ---------------------------------------------------
  xml_esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    gsub("'", "&apos;", x, fixed = TRUE)
  }
  series_xml <- function(rows, parameter, values, units) {
    d <- substr(g$Datetime[rows], 1L, 10L)
    t <- substr(g$Datetime[rows], 12L, 19L)
    sprintf(
      paste0(
        "  <series>\n",
        "    <header>\n",
        "      <type>instantaneous</type>\n",
        "      <locationId>%s</locationId>\n",
        "      <parameterId>%s</parameterId>\n",
        "      <timeStep unit=\"nonequidistant\"/>\n",
        "      <startDate date=\"%s\" time=\"%s\"/>\n",
        "      <endDate date=\"%s\" time=\"%s\"/>\n",
        "      <missVal>-999.0</missVal>\n",
        "      <stationName>%s</stationName>\n",
        "      <lat>%.7f</lat>\n",
        "      <lon>%.7f</lon>\n",
        "      <units>%s</units>\n",
        "    </header>\n",
        "    <event date=\"%s\" time=\"%s\" value=\"%s\" flag=\"0\"/>\n",
        "  </series>"
      ),
      g$fews_id[rows],
      parameter,
      d,
      t,
      d,
      t,
      xml_esc(loc$NAME[rows]),
      g$Latitude[rows],
      g$Longitude[rows],
      units,
      d,
      t,
      format(values, trim = TRUE, scientific = FALSE)
    )
  }
  has_az <- exif_time & !is.na(g$Azimuth)
  xml <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    paste0(
      "<TimeSeries xmlns=\"http://www.wldelft.nl/fews/PI\" ",
      "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ",
      "xsi:schemaLocation=\"http://www.wldelft.nl/fews/PI ",
      "http://fews.wldelft.nl/schemas/version1.0/pi-schemas/pi_timeseries.xsd\" ",
      "version=\"1.2\">"
    ),
    sprintf("  <timeZone>%.1f</timeZone>", utc_offset_hours),
    if (any(exif_time)) series_xml(which(exif_time), "PHOTO", rep(1, sum(exif_time)), "-"),
    if (any(has_az)) series_xml(which(has_az), "PHOTO_AZIMUTH", round(g$Azimuth[has_az], 1), "deg"),
    "</TimeSeries>"
  )
  xml_path <- file.path(dir, "photo_timeseries.xml")
  con <- file(xml_path, open = "w", encoding = "UTF-8")
  writeLines(xml, con)
  close(con)

  readme_path <- file.path(dir, "README.txt")
  write_fews_readme(readme_path, nrow(loc), sum(exif_time), sum(has_az), utc_offset_hours)
  message(sprintf(
    "FEWS: %d photo locations, %d PHOTO and %d PHOTO_AZIMUTH series%s.",
    nrow(loc),
    sum(exif_time),
    sum(has_az),
    if (any(!exif_time)) {
      sprintf(" (%d photos without an EXIF time have no series)", sum(!exif_time))
    } else {
      ""
    }
  ))
  c(loc_path, xml_path, readme_path)
}

write_fews_readme <- function(path, n_loc, n_photo, n_az, utc_offset_hours) {
  txt <- c(
    "Photo data for Delft-FEWS",
    "=========================",
    "",
    sprintf("photo_locations.csv   %d locations, one per geolocated photo", n_loc),
    "                      (exact duplicate files are left out)",
    sprintf("photo_timeseries.xml  PI-XML: %d PHOTO series (value 1 at the capture", n_photo),
    sprintf("                      time) and %d PHOTO_AZIMUTH series (degrees from", n_az),
    "                      true north). Photos without an EXIF capture time",
    "                      have a location but no series.",
    "",
    sprintf(
      "Times are the cameras' clock times, written with <timeZone>%.1f</timeZone>",
      utc_offset_hours
    ),
    "(change utc_offset_hours in scan_photo_folder() if the cameras used",
    "another zone). Location ids (PHOTO_<first 12 of the MD5>) come from the",
    "file contents, so re-scanning the same photos gives the same ids.",
    "",
    "Columns in photo_locations.csv",
    "  ID, NAME, LAT, LON (WGS 84), ALTITUDE (m), DATETIME, DATETIME_SOURCE,",
    "  FOLDER, FLIGHT (\"Flight 3\"), FLIGHT_ID, AZIMUTH (deg), FOV (deg),",
    "  FOV_SOURCE, CAMERA, FILE_NAME, RELATIVE_PATH, FILE_PATH, FILE_URL, MD5",
    "",
    "Example configuration",
    "---------------------",
    "These are starting points; check element names against the schemas and",
    "wiki for your FEWS version before using them.",
    "",
    "RegionConfigFiles/LocationSets.xml - all photos, with attributes:",
    "",
    "  <locationSet id=\"Photos\">",
    "    <csvFile>",
    "      <file>photo_locations.csv</file>",
    "      <geoDatum>WGS 1984</geoDatum>",
    "      <id>%ID%</id>",
    "      <name>%NAME%</name>",
    "      <x>%LON%</x>",
    "      <y>%LAT%</y>",
    "      <z>%ALTITUDE%</z>",
    "      <attribute id=\"FOLDER\"><text>%FOLDER%</text></attribute>",
    "      <attribute id=\"FLIGHT\"><text>%FLIGHT%</text></attribute>",
    "      <attribute id=\"DATETIME\"><text>%DATETIME%</text></attribute>",
    "      <attribute id=\"AZIMUTH\"><number>%AZIMUTH%</number></attribute>",
    "      <attribute id=\"FOV\"><number>%FOV%</number></attribute>",
    "      <attribute id=\"CAMERA\"><text>%CAMERA%</text></attribute>",
    "      <attribute id=\"FILE_PATH\"><text>%FILE_PATH%</text></attribute>",
    "      <attribute id=\"FILE_URL\"><text>%FILE_URL%</text></attribute>",
    "    </csvFile>",
    "  </locationSet>",
    "",
    "  (photo_locations.csv goes in MapLayerFiles, where FEWS looks for",
    "  location CSV files.)",
    "",
    "A sub-set per flight or folder, filtered on an attribute:",
    "",
    "  <locationSet id=\"Photos_Flight3\">",
    "    <locationSetId>Photos</locationSetId>",
    "    <constraints>",
    "      <attributeTextEquals id=\"FLIGHT\" equals=\"Flight 3\"/>",
    "    </constraints>",
    "  </locationSet>",
    "",
    "RegionConfigFiles/Parameters.xml - two parameters, e.g. in a \"Photos\"",
    "parameter group:",
    "",
    "  <parameterGroup id=\"Photos\">",
    "    <parameterType>instantaneous</parameterType>",
    "    <unit>-</unit>",
    "    <valueResolution>1</valueResolution>",
    "    <parameter id=\"PHOTO\"><shortName>Photo taken</shortName></parameter>",
    "  </parameterGroup>",
    "  <parameterGroup id=\"PhotoDirection\">",
    "    <parameterType>instantaneous</parameterType>",
    "    <unit>deg</unit>",
    "    <valueResolution>0.1</valueResolution>",
    "    <parameter id=\"PHOTO_AZIMUTH\"><shortName>Photo direction</shortName></parameter>",
    "  </parameterGroup>",
    "",
    "ModuleConfigFiles: a PI import, with a matching nonequidistant time series",
    "set for the Photos location set and each parameter:",
    "",
    "  <import>",
    "    <general>",
    "      <importType>PI</importType>",
    "      <folder>$IMPORT_FOLDER$/photos</folder>",
    "      <failedFolder>$IMPORT_FAILED_FOLDER$/photos</failedFolder>",
    "      <backupFolder>$IMPORT_BACKUP_FOLDER$/photos</backupFolder>",
    "      <idMapId>IdImportPhotos</idMapId>",
    "      <missingValue>-999</missingValue>",
    "    </general>",
    "    <timeSeriesSet>",
    "      <moduleInstanceId>ImportPhotos</moduleInstanceId>",
    "      <valueType>scalar</valueType>",
    "      <parameterId>PHOTO</parameterId>",
    "      <locationSetId>Photos</locationSetId>",
    "      <timeSeriesType>external historical</timeSeriesType>",
    "      <timeStep unit=\"nonequidistant\"/>",
    "      <readWriteMode>add originals</readWriteMode>",
    "    </timeSeriesSet>",
    "    (same again for PHOTO_AZIMUTH)",
    "  </import>",
    "",
    "  The ids in the PI file already match the location set and parameters,",
    "  so the id map can be a one-to-one map (or left out, if your FEWS",
    "  version allows importing without one).",
    "",
    "Opening a photo from FEWS: FILE_PATH / FILE_URL are location attributes.",
    "Use them wherever your FEWS version can link a location to a document or",
    "URL (e.g. a location info panel or web browser display); otherwise copy",
    "the path from the attribute table."
  )
  writeLines(txt, path)
  path
}

# PI diagnostics file for the Delft-FEWS General Adapter. FEWS levels:
# 1 error, 2 warning, 3 info.
write_pi_diagnostics <- function(path, lines) {
  if (is.null(path) || !nzchar(path)) {
    return(invisible())
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    gsub("\"", "&quot;", x, fixed = TRUE)
  }
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      paste0(
        "<Diag xmlns=\"http://www.wldelft.nl/fews/PI\" ",
        "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ",
        "xsi:schemaLocation=\"http://www.wldelft.nl/fews/PI ",
        "http://fews.wldelft.nl/schemas/version1.0/pi-schemas/pi_diag.xsd\" ",
        "version=\"1.2\">"
      ),
      sprintf(
        "  <line level=\"%d\" description=\"%s\"/>",
        vapply(lines, `[[`, integer(1), "level"),
        esc(gsub("[\r\n]+", " ", vapply(lines, `[[`, character(1), "text")))
      ),
      "</Diag>"
    ),
    con
  )
  invisible(path)
}

# Command-line use:
#   Rscript dev/scan_photo_folder.R <folder> [output folder] [options]
# Options:
#   --no-plots              skip the PNG plots and map.html
#   --utc-offset=<hours>    camera clock time zone (default -7)
#   --locations-csv=<file>  also copy fews/photo_locations.csv here (e.g. a
#                           FEWS Config/MapLayerFiles folder)
#   --diagnostics=<file>    write messages/warnings/errors as a PI
#                           diagnostics file (Delft-FEWS General Adapter)
if (sys.nframe() == 0L && !interactive()) {
  cli_args <- commandArgs(trailingOnly = TRUE)
  is_opt <- startsWith(cli_args, "--")
  opts <- cli_args[is_opt]
  pos <- cli_args[!is_opt]
  opt_value <- function(name) {
    hit <- opts[startsWith(opts, paste0("--", name, "="))]
    if (length(hit)) sub("^[^=]*=", "", hit[[1L]]) else NULL
  }
  diag_file <- opt_value("diagnostics")
  diag_lines <- list()
  add_diag <- function(level, text) {
    diag_lines[[length(diag_lines) + 1L]] <<- list(level = level, text = text)
  }
  ok <- withCallingHandlers(
    tryCatch(
      {
        if (length(pos) < 1L) {
          stop("Usage: Rscript scan_photo_folder.R <folder> [output folder] [options]")
        }
        utc <- opt_value("utc-offset")
        args <- list(
          folder = pos[[1L]],
          plots = !"--no-plots" %in% opts,
          utc_offset_hours = if (is.null(utc)) -7 else as.numeric(utc)
        )
        if (length(pos) >= 2L) {
          args$output <- pos[[2L]]
        }
        res <- do.call(scan_photo_folder, args)
        loc_target <- opt_value("locations-csv")
        if (!is.null(loc_target)) {
          out_dir <- if (length(pos) >= 2L) {
            pos[[2L]]
          } else {
            paste0(sub("[/\\\\]+$", "", pos[[1L]]), "_photo_report")
          }
          src <- file.path(out_dir, "fews", "photo_locations.csv")
          if (!file.exists(src)) {
            stop("No FEWS location file was written (no geolocated photos?)")
          }
          check_output_outside(dirname(loc_target), pos[[1L]])
          dir.create(dirname(loc_target), recursive = TRUE, showWarnings = FALSE)
          if (!file.copy(src, loc_target, overwrite = TRUE)) {
            stop("Could not copy the location file to ", loc_target)
          }
          message("Copied FEWS locations to ", loc_target)
        }
        TRUE
      },
      error = function(e) {
        add_diag(1L, conditionMessage(e))
        message("ERROR: ", conditionMessage(e))
        FALSE
      }
    ),
    message = function(m) add_diag(3L, trimws(conditionMessage(m))),
    warning = function(w) add_diag(2L, conditionMessage(w))
  )
  write_pi_diagnostics(diag_file, diag_lines)
  if (!ok) {
    quit(status = 1L, save = "no")
  }
}

# Interactive use only (e.g. sourcing in RStudio); not run by Rscript/FEWS
if (interactive()) {
  scan_photo_folder(
    folder = "G:\\water\\Hydrology\\21_Flood_Forecasting\\01-Imagery-Data-Forecasts\\2026\\01-Breakup",
    output = "C:\\Users\\esniede\\Pictures\\output"
  )
}
