# Shortcut script for (re)creating sysdata.rda

# IMPORTANT NOTE: spatial data doesn't behave well as internal package data. See the file data_load in the /R folder for a better way to do this. Non-spatial data can almost all be incorporated using  internal data though.

# DO NOT DO THE FOLLOWING FOR SPATIAL DATA:
# prov_buff <- sf::read_sf(dsn = "dev/prov_buffers", layer = "Provinces_buffered_300km")

# However, other data that can be simply reloaded as R environment objects can be made into internal data simply:
flow_returns_max <- read.csv("data-raw/flow_returns_max.csv")
level_returns_max <- read.csv("data-raw/level_returns_max.csv")
spatial_stns <- read.csv("data-raw/spatial_stns.csv")
peaks <- read.csv("data-raw/peaks.csv")
flow_level_flood <- read.csv("data-raw/flow_level_flood.csv")
snowcourse_factors <- read.csv("data-raw/snowcourse_factors.csv")
eq_std_calc_CCME_Mn <- read.csv(
  "data-raw/eq_std_calc_CCME_Mn.csv",
  check.names = FALSE
)
eq_std_calc_CCME_NH4 <- read.csv(
  "data-raw/eq_std_calc_CCME_NH4.csv",
  check.names = FALSE
)


# Creating translations from csv
translations_df <- data.table::fread(
  system.file(
    "data-raw/translations.csv",
    package = "YGwater"
  ),
  encoding = "UTF-8"
)

# Creating translations from csv
snowbull_translations_df <- data.table::fread(
  system.file(
    "data-raw/snowbull_translations.csv",
    package = "YGwater"
  ),
  encoding = "UTF-8"
)


#' Preprocess a translations data frame.
#'
#' Applies standard cleaning and transformation steps to a translations data frame
#' to ensure consistent structure and ready-to-use values.
#'
#' @param translations_df A data frame containing translation entries to be standardized.
#'
#' @return A data frame with cleaned and preprocessed translation records.
#'
#' @examples
#' cleaned_translations <- preprocess_translations(raw_translations)
#' @noRd
#' @keywords internal
preprocess_translations <- function(
  translations_df,
  check_for_duplicate_translations = TRUE
) {
  dup_ids <- translations_df$id[duplicated(translations_df$id)]
  if (length(dup_ids) > 0) {
    stop(
      paste0(
        "Duplicate translation IDs found in translations.csv: ",
        paste(dup_ids, collapse = ", ")
      )
    )
  }

  dup_rows_all <- duplicated(translations_df$id)
  if (any(dup_rows_all)) {
    stop(
      paste0(
        "Duplicate rows found in translations.csv. Duplicate rows: ",
        paste(which(dup_rows_all), collapse = ", ")
      )
    )
  }

  if (check_for_duplicate_translations) {
    dup_rows_no_id <- duplicated(translations_df[, -1])
    if (any(dup_rows_no_id)) {
      warning(
        paste0(
          "Identical translation rows found in translations.csv ",
          "(excluding IDs). Rows: ",
          paste(which(dup_rows_no_id), collapse = ", ")
        )
      )
    }
  }

  lang_cols <- setdiff(names(translations_df[, -2]), "id")
  translations <- lapply(
    lang_cols,
    function(lang) {
      stats::setNames(translations_df[[lang]], translations_df$id)
    }
  )
  names(translations) <- lang_cols

  for (lang in names(translations)) {
    for (id in names(translations[[lang]])) {
      tryCatch(
        {
          print(translations[[lang]][[id]])
        },
        error = function(e) {
          stop(
            paste0(
              "Error in translation for language '",
              lang,
              "', id '",
              id,
              "': ",
              e$message
            )
          )
        }
      )
    }
  }

  translations
}

translations <- preprocess_translations(
  translations_df = translations_df
)

# here we skip check for dupes, since we might have a many-to-one mapping
snowbull_translations <- preprocess_translations(
  translations_df = snowbull_translations_df,
  check_for_duplicate_translations = FALSE
)


data <- list(
  level_returns_max = level_returns_max,
  flow_returns_max = flow_returns_max,
  spatial_stns = spatial_stns,
  peaks = peaks,
  flow_level_flood = flow_level_flood,
  snowcourse_factors = snowcourse_factors,
  eq_std_calc_CCME_Mn = eq_std_calc_CCME_Mn,
  eq_std_calc_CCME_NH4 = eq_std_calc_CCME_NH4,
  translations = translations,
  snowbull_translations = snowbull_translations
)

usethis::use_data(data, internal = TRUE, overwrite = TRUE)

load_all()
