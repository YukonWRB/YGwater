#' Read the unit conversion catalogue for mapping editors
#'
#' @param con An open AquaCache DBI connection.
#' @return A data.frame containing source and target unit names, multiplier
#'   `scale_a`, and offset `scale_b`.
#' @details Read once per mapping module and reuse for subsequent suggestions.
#'   Only explicit catalogue directions are read; chemical/reporting-basis
#'   conversions and aliases such as ppm and ppb are not inferred.
#' @keywords internal
#' @noRd
ac_mapping_unit_conversions <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT f.unit_name AS source_unit, t.unit_name AS target_unit,
            c.scale_a, c.scale_b
       FROM public.unit_conversions c
       JOIN public.units f ON f.unit_id = c.from_unit_id
       JOIN public.units t ON t.unit_id = c.to_unit_id"
  )
}

#' Suggest a unit conversion for a parameter mapping
#'
#' @param source_unit Source unit name, as a character scalar.
#' @param target_unit Target unit name, as a character scalar.
#' @param conversions A catalogue from `ac_mapping_unit_conversions()`.
#' @return A list with numeric `scale_a` and `scale_b` for the transformation
#'   `source_value * scale_a + scale_b`, or `NULL` when no unambiguous, finite
#'   conversion is available. Identical normalized units return 1 and 0.
#' @details Case, surrounding whitespace, and Unicode/ASCII micro prefixes are
#'   treated as equivalent. Missing units do not imply an identity conversion.
#'   Reverse conversions and conversions between reporting bases are not inferred.
#' @keywords internal
#' @noRd
ac_mapping_conversion <- function(source_unit, target_unit, conversions) {
  normalize <- function(x) {
    gsub(
      "\u00b5",
      "u",
      gsub("\u03bc", "u", tolower(trimws(as.character(x))), fixed = TRUE),
      fixed = TRUE
    )
  }
  source <- normalize(source_unit)
  target <- normalize(target_unit)
  if (
    length(source) != 1L ||
      length(target) != 1L ||
      is.na(source) ||
      is.na(target) ||
      !nzchar(source) ||
      !nzchar(target)
  ) {
    return(NULL)
  }
  if (identical(source, target)) {
    return(list(scale_a = 1, scale_b = 0))
  }
  rows <- unique(conversions[
    which(
      normalize(conversions$source_unit) == source &
        normalize(conversions$target_unit) == target
    ),
    c("scale_a", "scale_b"),
    drop = FALSE
  ])
  if (nrow(rows) != 1L || any(!is.finite(as.numeric(rows[1, ])))) {
    return(NULL)
  }
  list(scale_a = as.numeric(rows$scale_a), scale_b = as.numeric(rows$scale_b))
}
