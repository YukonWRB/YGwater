concat_ocr_words_by_row <- function(ocr_df) {
  if (is.null(ocr_df) || nrow(ocr_df) == 0) return(character(0))
  coords <- do.call(rbind, lapply(ocr_df$bbox, function(b) as.numeric(strsplit(b, ",")[[1]])))
  ymin <- coords[,2]
  ymax <- coords[,4]
  words <- ocr_df$word

  result <- character(0)
  current_line <- ""
  prev_ymin <- ymin[1]
  prev_ymax <- ymax[1]

  for (i in seq_along(words)) {
    if (i == 1) {
      current_line <- words[i]
    } else {
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
  if (is.null(ocr_df) || nrow(ocr_df) == 0) return(ocr_df)

  noise_patterns <- c(
    "^$", "^\\s*$", "^\\s+$",
    "^-+$", "^=+$", "^\\|+$", "^_+$", "^\\++$", "^\\*+$", "^#+$", "^~+$", "^`+$", "^'+$", '^"+$', "^\\^+$", "^&+$", "^%+$", "^@+$", "^\\$+$",
    "^[\\|Il1]{1,3}$", "^[oO0]{1,2}$", "^[cC]{1}$", "^[rR]{1}$", "^[nN]{1}$", "^[mM]{1}$", "^[uU]{1}$", "^[vV]{1}$", "^[wW]{1}$", "^[iI]{1,2}$",
    "^([a-zA-Z])\\1{3,}$",
    "^[[:punct:]]+$",
    "^[a-zA-Z]{1}[0-9]{1}$", "^[0-9]{1}[a-zA-Z]{1}$",
    "^\\.[a-zA-Z]{1,2}$", "^[a-zA-Z]{1,2}\\.$", "^[()\\[\\]\\{\\}]+$"
  )

  meaningful_single_chars <- c("A", "a", "O", "o")
  keep_word <- rep(TRUE, nrow(ocr_df))

  keep_word <- keep_word & !is.na(ocr_df$word) &
    ocr_df$word != "" &
    trimws(ocr_df$word) != ""

  for (pattern in noise_patterns) {
    keep_word <- keep_word & !grepl(pattern, ocr_df$word, perl = TRUE)
  }

  short_and_meaningless <- nchar(trimws(ocr_df$word)) == 1 &
    !trimws(ocr_df$word) %in% meaningful_single_chars &
    !grepl("^[0-9]$", trimws(ocr_df$word))

  keep_word <- keep_word & !short_and_meaningless

  low_conf_short <- ocr_df$confidence < 30 & nchar(trimws(ocr_df$word)) <= 2
  keep_word <- keep_word & !low_conf_short

  filtered_df <- ocr_df[keep_word, ]

  return(filtered_df)
}
