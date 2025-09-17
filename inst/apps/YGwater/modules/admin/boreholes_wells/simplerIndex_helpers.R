# This file contains functions used in the simplerIndex module of the YGwater app.

# OCR helper functions #########################
concat_ocr_words_by_row <- function(ocr_df) {
  if (is.null(ocr_df) || nrow(ocr_df) == 0) {
    return(character(0))
  }
  # Parse bbox coordinates
  coords <- do.call(
    rbind,
    lapply(ocr_df$bbox, function(b) as.numeric(strsplit(b, ",")[[1]]))
  )
  ymin <- coords[, 2]
  ymax <- coords[, 4]
  words <- ocr_df$word

  result <- character(0)
  current_line <- ""
  prev_ymin <- ymin[1]
  prev_ymax <- ymax[1]

  for (i in seq_along(words)) {
    if (i == 1) {
      current_line <- words[i]
    } else {
      # If current ymax < previous ymin, start new line
      if (ymax[i] < prev_ymin) {
        result <- c(result, current_line)
        current_line <- words[i]
      } else {
        current_line <- paste(current_line, words[i], sep = " ")
      }
      prev_ymin <- ymin[i]
      prev_ymax <- ymax[i]
    }
  }
  result <- c(result, current_line)
  return(result)
}

filter_ocr_noise <- function(ocr_df) {
  if (is.null(ocr_df) || nrow(ocr_df) == 0) {
    return(ocr_df)
  }

  # Define common OCR error patterns and noise words
  noise_patterns <- c(
    # Empty strings and whitespace
    "^$", # Empty string
    "^\\s*$", # Only whitespace (spaces, tabs, newlines)
    "^\\s+$", # One or more whitespace characters

    # Single characters that are often OCR errors
    "^-+$", # Only dashes
    "^=+$", # Only equals signs
    "^\\|+$", # Only vertical bars/pipes
    "^_+$", # Only underscores
    "^\\++$", # Only plus signs
    "^\\*+$", # Only asterisks
    "^#+$", # Only hash symbols
    "^~+$", # Only tildes
    "^`+$", # Only backticks
    "^'+$", # Only single quotes
    "^\"+$", # Only double quotes
    "^\\^+$", # Only carets
    "^&+$", # Only ampersands
    "^%+$", # Only percent signs
    "^@+$", # Only at symbols
    "^\\$+$", # Only dollar signs

    # Common OCR misreads
    "^[\\|Il1]{1,3}$", # Vertical bars, I, l, 1 confusion (1-3 chars)
    "^[oO0]{1,2}$", # o, O, 0 confusion (1-2 chars)
    "^[cC]{1}$", # Single c or C
    "^[rR]{1}$", # Single r or R
    "^[nN]{1}$", # Single n or N
    "^[mM]{1}$", # Single m or M
    "^[uU]{1}$", # Single u or U
    "^[vV]{1}$", # Single v or V
    "^[wW]{1}$", # Single w or W
    "^[iI]{1,2}$", # Single i or ii (common OCR noise)

    # Repeated characters (likely errors)
    "^([a-zA-Z])\\1{3,}$", # Same letter repeated 4+ times

    # Pure punctuation strings
    "^[[:punct:]]+$", # Only punctuation marks

    # Very short meaningless combinations
    "^[a-zA-Z]{1}[0-9]{1}$", # Single letter + single digit
    "^[0-9]{1}[a-zA-Z]{1}$", # Single digit + single letter

    # Common OCR artifacts
    "^\\.[a-zA-Z]{1,2}$", # Dot followed by 1-2 letters
    "^[a-zA-Z]{1,2}\\.$", # 1-2 letters followed by dot
    "^[\\(\\)\\[\\]\\{\\}]+$" # Only brackets/parentheses
  )

  # Additional filter: words that are too short and likely meaningless
  # Keep single letters that could be meaningful (like "A", "I", etc.)
  meaningful_single_chars <- c("A", "a", "O", "o")

  # Create filter condition - first remove empty/whitespace strings
  keep_word <- rep(TRUE, nrow(ocr_df))

  # Remove empty strings and whitespace-only strings
  keep_word <- keep_word &
    !is.na(ocr_df$word) &
    ocr_df$word != "" &
    trimws(ocr_df$word) != ""

  for (pattern in noise_patterns) {
    keep_word <- keep_word & !grepl(pattern, ocr_df$word, perl = TRUE)
  }

  # Additional filters
  # Remove very short words that aren't meaningful single characters
  short_and_meaningless <- nchar(trimws(ocr_df$word)) == 1 &
    !trimws(ocr_df$word) %in% meaningful_single_chars &
    !grepl("^[0-9]$", trimws(ocr_df$word)) # Keep single digits

  keep_word <- keep_word & !short_and_meaningless

  # Remove words with very low confidence that are also short
  low_conf_short <- ocr_df$confidence < 30 & nchar(trimws(ocr_df$word)) <= 2
  keep_word <- keep_word & !low_conf_short

  # Filter the dataframe
  filtered_df <- ocr_df[keep_word, ]

  return(filtered_df)
}

# Helper function to apply different preprocessing methods
preprocess_image <- function(img, method = "default") {
  switch(
    method,
    "default" = {
      img %>%
        magick::image_convert(colorspace = "gray") %>%
        magick::image_contrast(sharpen = 1) %>%
        magick::image_modulate(
          brightness = 110,
          saturation = 100,
          hue = 100
        ) %>%
        magick::image_threshold(type = "black", threshold = "60%")
    },
    "enhance_dark" = {
      img %>%
        magick::image_convert(colorspace = "gray") %>%
        magick::image_contrast(sharpen = 2) %>%
        magick::image_modulate(brightness = 130, saturation = 0) %>%
        magick::image_threshold(type = "black", threshold = "45%")
    },
    "enhance_light" = {
      img %>%
        magick::image_convert(colorspace = "gray") %>%
        magick::image_contrast(sharpen = 1.5) %>%
        magick::image_modulate(brightness = 90, saturation = 0) %>%
        magick::image_threshold(type = "black", threshold = "70%")
    },
    "high_contrast" = {
      img %>%
        magick::image_convert(colorspace = "gray") %>%
        magick::image_contrast(sharpen = 3) %>%
        magick::image_normalize() %>%
        magick::image_threshold(type = "black", threshold = "50%")
    },
    "denoise" = {
      img %>%
        magick::image_convert(colorspace = "gray") %>%
        magick::image_enhance() %>%
        magick::image_threshold(type = "black", threshold = "60%")
    },
    "deskew" = {
      # Attempt to deskew (straighten) the image
      img %>%
        magick::image_convert(colorspace = "gray") %>%
        magick::image_deskew(threshold = 40) %>%
        magick::image_contrast(sharpen = 1) %>%
        magick::image_threshold(type = "black", threshold = "60%")
    },
    # Default fallback if method is not recognized
    {
      img %>%
        magick::image_convert(colorspace = "gray") %>%
        magick::image_contrast(sharpen = 1) %>%
        magick::image_modulate(
          brightness = 110,
          saturation = 100,
          hue = 100
        ) %>%
        magick::image_threshold(type = "black", threshold = "60%")
    }
  )
}

process_ocr_batch <- function(
  files_df,
  ocr_text_list,
  psm_mode,
  pre_processing_method
) {
  # Check if any OCR processing is needed
  needs_processing <- any(sapply(ocr_text_list, function(x) is.null(x)))

  if (!needs_processing) {
    # All OCR already processed
    showNotification(
      "OCR data loaded from cache",
      type = "message",
      duration = 2
    )
    return(ocr_text_list)
  }

  # Check if we have some cached OCR results
  has_some_cached <- any(sapply(ocr_text_list, function(x) !is.null(x)))

  if (has_some_cached) {
    showNotification(
      "Using cached OCR results where available",
      type = "message",
      duration = 2
    )
  } else {
    showNotification(
      "Starting OCR processing...",
      type = "message",
      duration = 2
    )
  }

  # Show progress notification for batch processing
  total_pages <- nrow(files_df)
  pages_to_process <- sum(sapply(ocr_text_list, function(x) is.null(x)))

  if (pages_to_process > 0) {
    # Loop through all images in files_df and run OCR
    for (i in seq_len(nrow(files_df))) {
      if (is.null(ocr_text_list[[i]])) {
        # Show progress for current page
        showNotification(
          paste("Processing page", i, "of", total_pages),
          type = "message",
          duration = 1
        )

        img_path <- files_df$Path[i]
        # Add error handling for image loading
        tryCatch(
          {
            img <- magick::image_read(img_path)

            # Apply selected preprocessing method
            img <- preprocess_image(img, pre_processing_method)

            # Create OCR engine options
            tessoptions <- list(
              tessedit_create_hocr = 1,
              tessedit_pageseg_mode = psm_mode
            )

            # Perform OCR on the preprocessed image
            ocr_result <- tesseract::ocr_data(
              img,
              engine = tesseract::tesseract(
                options = tessoptions
              )
            )

            # Filter out common OCR noise and error words
            ocr_result <- filter_ocr_noise(ocr_result)
            ocr_text_list[[i]] <- ocr_result
          },
          error = function(e) {
            message("Error processing OCR for page ", i, ": ", e$message)
            # Create an empty dataframe with the right structure instead of NULL
            ocr_text_list[[i]] <- data.frame(
              word = character(0),
              confidence = numeric(0),
              bbox = character(0),
              stringsAsFactors = FALSE
            )
          }
        )
      }
    }
    # Show completion notification
    showNotification(
      "OCR processing completed!",
      type = "message",
      duration = 3
    )
  }
  return(ocr_text_list)
}


# PDF helper functions #############################
split_pdf_to_pages <- function(pdf_path, output_dir = tempdir()) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The 'pdftools' package is required.")
  }
  n_pages <- pdftools::pdf_info(pdf_path)$pages
  out_files <- character(n_pages)
  for (i in seq_len(n_pages)) {
    out_file <- file.path(
      output_dir,
      sprintf(
        "%s_page_%d.pdf",
        tools::file_path_sans_ext(basename(pdf_path)),
        i
      )
    )
    pdftools::pdf_subset(pdf_path, pages = i, output = out_file)
    out_files[i] <- normalizePath(out_file)
  }
  out_files
}


# Generalized function to create PDF with redactions
create_pdf_with_redactions <- function(borehole_id, return_path = FALSE) {
  if (is.null(rv$files_df)) {
    return(NULL)
  }

  # Find all rows for this borehole_id
  same_bh_rows <- which(rv$files_df$borehole_id == borehole_id)
  if (length(same_bh_rows) == 0) {
    return(NULL)
  }

  img_paths <- rv$files_df$Path[same_bh_rows]

  # Create temp directory and file path
  temp_dir <- file.path(tempdir(), paste0("pdf_export_", borehole_id))
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  # Generate filename
  base_name <- tools::file_path_sans_ext(basename(rv$files_df$Name[same_bh_rows[
    1
  ]]))
  if (length(same_bh_rows) > 1) {
    filename <- paste0(
      base_name,
      "_",
      borehole_id,
      "_multi_with_redactions.pdf"
    )
  } else {
    page_num <- rv$files_df$Page[same_bh_rows[1]]
    filename <- paste0(base_name, "_page_", page_num, "_with_redactions.pdf")
  }

  temp_file_path <- file.path(temp_dir, filename)

  # Process images
  if (length(img_paths) == 1) {
    # Single page
    img <- magick::image_read(img_paths[1])
    info <- magick::image_info(img)
    img_width <- info$width
    img_height <- info$height

    # Get rectangles for this specific file path
    file_path <- img_paths[1]
    rectangles <- rv$rectangles[[file_path]]

    # Apply rectangles if there are any for this file
    if (!is.null(rectangles) && length(rectangles) > 0) {
      img <- magick::image_draw(img)
      for (rect in rectangles) {
        y_min_img <- img_height - rect$ymax
        y_max_img <- img_height - rect$ymin
        rect(
          rect$xmin,
          y_min_img,
          rect$xmax,
          y_max_img,
          border = "black",
          col = "black",
          lwd = 2
        )
      }
      dev.off()
    }
    magick::image_write(img, path = temp_file_path, format = "pdf")
  } else {
    # Multi-page PDF logic with file-specific redactions
    img_list <- list()
    for (i in seq_along(img_paths)) {
      file_path <- img_paths[i]
      img <- magick::image_read(file_path)
      info <- magick::image_info(img)
      img_width <- info$width
      img_height <- info$height

      # Get rectangles for this specific file path

      rectangles <- rv$rectangles[[file_path]]

      # Apply rectangles if there are any for this file
      if (!is.null(rectangles) && length(rectangles) > 0) {
        img <- magick::image_draw(img)
        for (rect in rectangles) {
          y_min_img <- img_height - rect$ymax
          y_max_img <- img_height - rect$ymin
          rect(
            rect$xmin,
            y_min_img,
            rect$xmax,
            y_max_img,
            border = "black",
            col = "black",
            lwd = 2
          )
        }
        dev.off()
      }
      img_list[[i]] <- img
    }
    # Combine all images into a single PDF
    img_joined <- do.call(magick::image_join, img_list)
    magick::image_write(img_joined, path = temp_file_path, format = "pdf")
  }

  # Create OCR text file if available
  ocr_words <- character(0)
  for (i in same_bh_rows) {
    if (!is.null(rv$ocr_text[[i]]) && nrow(rv$ocr_text[[i]]) > 0) {
      ocr_data <- rv$ocr_text[[i]]
      ocr_words <- c(ocr_words, ocr_data$word)
    }
  }
  if (length(ocr_words) > 0) {
    text_file <- file.path(
      temp_dir,
      paste0(tools::file_path_sans_ext(filename), "_ocr_text.txt")
    )
    writeLines(paste(ocr_words, collapse = " "), text_file)
  }

  if (return_path) {
    return(temp_file_path)
  } else {
    return(filename)
  }
}


# Other helper function #############
# Helper functions to safely get metadata values ##
# Text or selectizeInput multiple = FALSE
get_meta_value <- function(field, metadata, default = "") {
  val <- metadata[[field]]
  if (is.null(val) || is.na(val)) {
    return(default)
  }
  return(val)
}
# For selectize that can return vector length > 1 (like share_with)
get_meta_value_multiple <- function(field, metadata, default = "") {
  val <- metadata[[field]]
  if (is.null(val) || all(is.na(val))) {
    return(default)
  }
  return(val)
}
# Numeric inputs
get_meta_numeric <- function(field, metadata) {
  val <- metadata[[field]]
  if (is.null(val) || is.na(val)) {
    return(NULL)
  }
  return(val)
}
# Date inputs
get_meta_date <- function(field, metadata) {
  val <- metadata[[field]]
  if (is.null(val) || is.na(val)) {
    return(NULL)
  }
  return(val)
}
# boolean inputs
get_meta_boolean <- function(field, metadata, default = FALSE) {
  val <- metadata[[field]]
  if (is.null(val) || is.na(val)) {
    return(default)
  }
  return(as.logical(val))
}
