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

# Check for ids which are the same
dup_ids <- translations_df$id[duplicated(translations_df$id)]
if (length(dup_ids) > 0) {
  stop(
    "Duplicate translation IDs found in translations.csv: ",
    paste(dup_ids, collapse = ", ")
  )
}

# Check for completely duplicated rows
dups_rows_all <- duplicated(translations_df$id)
if (any(dups_rows_all)) {
  stop(
    "Duplicate rows found in translations.csv. Duplicate rows: ",
    paste(which(dups_rows_all), collapse = ", ")
  )
}

# Check for identical rows, not including the ids
dup_rows_no_id <- duplicated(translations_df[, -1])
if (any(dup_rows_no_id)) {
  warning(
    "Identical translation rows found in translations.csv (excluding IDs). Rows: ",
    paste(which(dup_rows_no_id), collapse = ", ")
  )
}


# Build a list from the data.frame
translations <- lapply(
  setdiff(names(translations_df[, -2]), "id"),
  function(lang) {
    # Removes the second, "description" column, builds lists for each language
    setNames(translations_df[[lang]], translations_df$id)
  }
)
names(translations) <- setdiff(names(translations_df)[-2], "id")


# Now call each element of all list elements to see if any fails:
for (lang in names(translations)) {
  for (id in names(translations[[lang]])) {
    tryCatch(
      {
        print(translations[[lang]][[id]])
      },
      error = function(e) {
        stop(
          "Error in translation for language '",
          lang,
          "', id '",
          id,
          "': ",
          e$message
        )
      }
    )
  }
}

data <- list(
  level_returns_max = level_returns_max,
  flow_returns_max = flow_returns_max,
  spatial_stns = spatial_stns,
  peaks = peaks,
  flow_level_flood = flow_level_flood,
  snowcourse_factors = snowcourse_factors,
  eq_std_calc_CCME_Mn = eq_std_calc_CCME_Mn,
  eq_std_calc_CCME_NH4 = eq_std_calc_CCME_NH4,
  translations = translations
)

usethis::use_data(data, internal = TRUE, overwrite = TRUE)

load_all()
