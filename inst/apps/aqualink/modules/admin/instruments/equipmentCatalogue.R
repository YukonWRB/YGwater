equipment_placeholder_make <- "[unmapped manufacturer]"

equipment_date_input <- function(id, label) {
  shinyWidgets::airDatepickerInput(
    id,
    label,
    value = NULL,
    dateFormat = "yyyy-MM-dd",
    clearButton = TRUE,
    autoClose = TRUE
  )
}

update_equipment_date_input <- function(session, id, value = NULL) {
  if (length(value) == 1L && !is.na(value)) {
    shinyWidgets::updateAirDateInput(session, id, value = value)
  } else {
    shinyWidgets::updateAirDateInput(session, id, clear = TRUE)
  }
}

read_equipment_manufacturers <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT make_id, make, description
     FROM instruments.instrument_makes
     WHERE lower(btrim(make)) <> $1
     ORDER BY make",
    params = list(equipment_placeholder_make)
  )
}

equipment_catalogue_id <- function(value, rows, id_column, label_column) {
  value <- trimws(as.character(value)[1L])
  if (is.na(value) || !nzchar(value) || is.null(rows) || !nrow(rows)) {
    return(NA_integer_)
  }
  ids <- rows[[id_column]]
  index <- match(value, as.character(ids))
  if (is.na(index)) {
    index <- match(tolower(value), tolower(trimws(rows[[label_column]])))
  }
  as.integer(ids[index])
}

equipment_models_for_make <- function(models, make_id) {
  if (is.null(models)) {
    return(data.frame(
      model_id = integer(),
      model = character(),
      make_id = integer()
    ))
  }
  matching <- which(models$make_id == make_id)
  models[matching, , drop = FALSE]
}

equipment_model_choices <- function(models) {
  stats::setNames(as.character(models$model_id), models$model)
}

resolve_equipment_sensor_make <- function(con, value, description, makes) {
  make_id <- equipment_catalogue_id(value, makes, "make_id", "make")
  if (!is.na(make_id)) {
    return(make_id)
  }
  make <- trimws(as.character(value)[1L])
  if (
    is.na(make) || !nzchar(make) || tolower(make) == equipment_placeholder_make
  ) {
    stop("Enter a manufacturer for the sensor.")
  }
  DBI::dbExecute(
    con,
    "INSERT INTO instruments.instrument_makes (make, description) VALUES ($1, $2)
     ON CONFLICT (lower(btrim(make))) DO NOTHING",
    params = list(make, description)
  )
  DBI::dbGetQuery(
    con,
    "SELECT make_id FROM instruments.instrument_makes WHERE lower(btrim(make)) = lower($1)",
    params = list(make)
  )$make_id[[1L]]
}

resolve_equipment_sensor_model <- function(
  con,
  value,
  description,
  make_id,
  models
) {
  matching <- equipment_models_for_make(models, make_id)
  model_id <- equipment_catalogue_id(value, matching, "model_id", "model")
  if (!is.na(model_id)) {
    return(model_id)
  }
  model <- trimws(as.character(value)[1L])
  if (
    is.na(model) || !nzchar(model) || model %in% as.character(models$model_id)
  ) {
    stop(
      "Select a model for the chosen manufacturer, or enter a new model name."
    )
  }
  DBI::dbExecute(
    con,
    "INSERT INTO instruments.sensor_models (make_id, model, description) VALUES ($1, $2, $3)
     ON CONFLICT (make_id, lower(btrim(model))) DO NOTHING",
    params = list(make_id, model, description)
  )
  DBI::dbGetQuery(
    con,
    "SELECT model_id FROM instruments.sensor_models
     WHERE make_id = $1 AND lower(btrim(model)) = lower($2)",
    params = list(make_id, model)
  )$model_id[[1L]]
}
