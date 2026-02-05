View(data$flow_level_flood)


flood_data <- data$flow_level_flood


library(tidyr)
# Remove the 'Flood_level_asl' column if it exists; levels in db are saved relative to datum
flood_data <- flood_data[, !(names(flood_data) %in% "Flood_level_asl")]
flood_long <- pivot_longer(
    flood_data,
    cols = c(Flood_level_ldatum, Flood_flow),
    names_to = "parameter",
    values_to = "value",
    values_drop_na = TRUE
)

# Rename parameter values to match those in the database
flood_long$parameter[
    flood_long$parameter == "Flood_level_ldatum"
] <- "water level"
flood_long$parameter[flood_long$parameter == "Flood_flow"] <- "water flow"


# query to get timeseries ids for stations and parameters
station_ids <- data$flow_level_flood$ID
query <- paste(
    "SELECT ts.timeseries_id, ts.location, p.param_name FROM timeseries AS ts",
    "JOIN parameters AS p ON ts.parameter_id = p.parameter_id",
    "WHERE p.param_name IN ('water level', 'water flow')",
    "AND ts.location IN (",
    paste(sprintf("'%s'", station_ids), collapse = ", "),
    ")",
    sep = " "
)
timeseries_ids <- DBI::dbGetQuery(con, query)

# get timeseires id by merging param/location
df <- merge(
    flood_long,
    merged_data[, c("ID", "timeseries_id", "param_name")],
    by.x = c("ID", "parameter"),
    by.y = c("ID", "param_name"),
    all.x = TRUE
)

df <- df[, c("timeseries_id", "value")]

names(df)[names(df) == "value"] <- "flood_minor"

df$created_by <- "YGwater"
