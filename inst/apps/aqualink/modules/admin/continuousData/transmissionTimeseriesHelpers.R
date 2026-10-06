# Provider-neutral helpers shared by the add/modify timeseries workflow.

timeseries_source_adapter_capability <- function(capabilities, source_fx) {
  source_fx <- trimws(as.character(source_fx))
  if (
    length(source_fx) != 1L ||
      is.na(source_fx) ||
      !nzchar(source_fx) ||
      is.null(capabilities) ||
      nrow(capabilities) == 0L
  ) {
    return(NULL)
  }

  matches <- capabilities$source_fx == source_fx
  rows <- as.data.frame(capabilities)[which(matches), , drop = FALSE]
  if (nrow(rows) != 1L) NULL else rows
}

timeseries_transmission_choices <- function(con, location_id, source_fx) {
  location_id <- suppressWarnings(as.integer(location_id))
  source_fx <- trimws(as.character(source_fx))
  if (is.na(location_id) || !nzchar(source_fx)) {
    return(list(
      routes = data.frame(),
      setups = data.frame(),
      loggers = data.frame(),
      methods = data.frame()
    ))
  }

  routes <- DBI::dbGetQuery(
    con,
    "SELECT
       r.transmission_route_id,
       r.transmission_setup_id,
       r.route_name,
       r.endpoint_identifier,
       r.message_format,
       r.schedule_reference_time_utc,
       r.transmit_interval_seconds,
       r.transmit_window_seconds,
       r.payload_size_bytes,
       r.route_config::text AS route_config,
       CONCAT(
         r.route_name,
         ' | ',
         tm.method_name,
         CASE
           WHEN s.platform_identifier IS NULL THEN ''
           ELSE CONCAT(' | ', s.platform_identifier)
         END,
         ' (#',
         r.transmission_route_id,
         ')'
       ) AS label
     FROM public.source_adapter_capabilities sac
     JOIN instruments.transmission_methods tm
       ON cardinality(sac.transmission_method_codes) = 0
       OR tm.method_code = ANY(sac.transmission_method_codes)
     JOIN public.locations_metadata_transmission_setups s
       ON s.transmission_method_id = tm.transmission_method_id
     JOIN public.locations_metadata_transmission_routes r
       ON r.transmission_setup_id = s.transmission_setup_id
     WHERE sac.source_fx = $1
       AND sac.data_domain = 'continuous'
       AND sac.enabled
       AND sac.requires_transmission_mapping
       AND s.location_id = $2
       AND s.start_datetime <= CURRENT_TIMESTAMP
       AND (s.end_datetime IS NULL OR s.end_datetime > CURRENT_TIMESTAMP)
     ORDER BY r.route_name, r.transmission_route_id",
    params = list(source_fx, location_id)
  )

  setups <- DBI::dbGetQuery(
    con,
    "SELECT
       s.transmission_setup_id,
       CONCAT(
         tm.method_name,
         CASE
           WHEN s.provider_name IS NULL THEN ''
           ELSE CONCAT(' | ', s.provider_name)
         END,
         CASE
           WHEN s.platform_identifier IS NULL THEN ''
           ELSE CONCAT(' | ', s.platform_identifier)
         END,
         ' (#',
         s.transmission_setup_id,
         ')'
       ) AS label
     FROM public.source_adapter_capabilities sac
     JOIN instruments.transmission_methods tm
       ON cardinality(sac.transmission_method_codes) = 0
       OR tm.method_code = ANY(sac.transmission_method_codes)
     JOIN public.locations_metadata_transmission_setups s
       ON s.transmission_method_id = tm.transmission_method_id
     WHERE sac.source_fx = $1
       AND sac.data_domain = 'continuous'
       AND sac.enabled
       AND sac.requires_transmission_mapping
       AND s.location_id = $2
       AND s.start_datetime <= CURRENT_TIMESTAMP
       AND (s.end_datetime IS NULL OR s.end_datetime > CURRENT_TIMESTAMP)
     ORDER BY s.start_datetime DESC, s.transmission_setup_id DESC",
    params = list(source_fx, location_id)
  )

  loggers <- DBI::dbGetQuery(
    con,
    "SELECT
       lmi.metadata_id,
       lmi.start_datetime,
       CONCAT(
         COALESCE(mk.make, ''),
         CASE WHEN mk.make IS NULL THEN '' ELSE ' ' END,
         COALESCE(mdl.model, ''),
         CASE
           WHEN i.serial_no IS NULL THEN ''
           ELSE CONCAT(' | ', i.serial_no)
         END,
         ' (#',
         lmi.metadata_id,
         ')'
       ) AS label
     FROM public.locations_metadata_instruments lmi
     JOIN instruments.instruments i
       ON i.instrument_id = lmi.instrument_id
     LEFT JOIN instruments.instrument_makes mk ON mk.make_id = i.make
     LEFT JOIN instruments.instrument_models mdl ON mdl.model_id = i.model
     WHERE lmi.location_id = $1
       AND i.can_be_logger
       AND lmi.start_datetime <= CURRENT_TIMESTAMP
       AND (lmi.end_datetime IS NULL OR lmi.end_datetime > CURRENT_TIMESTAMP)
     ORDER BY i.serial_no, lmi.metadata_id",
    params = list(location_id)
  )

  methods <- DBI::dbGetQuery(
    con,
    "SELECT tm.transmission_method_id, tm.method_code, tm.method_name
     FROM public.source_adapter_capabilities sac
     JOIN instruments.transmission_methods tm
       ON cardinality(sac.transmission_method_codes) = 0
       OR tm.method_code = ANY(sac.transmission_method_codes)
     WHERE sac.source_fx = $1
       AND sac.data_domain = 'continuous'
       AND sac.enabled
       AND sac.requires_transmission_mapping
     ORDER BY tm.method_name",
    params = list(source_fx)
  )

  list(routes = routes, setups = setups, loggers = loggers, methods = methods)
}

timeseries_route_config_fields <- function(capability, message_format = NULL) {
  if (is.null(capability) || nrow(capability) != 1L) {
    return(list())
  }
  ui_config <- capability$ui_config[[1L]]
  fields <- ui_config$route_config_fields
  if (is.null(fields) || !is.list(fields)) {
    return(list())
  }

  filter_by_format <- !is.null(message_format) && length(message_format) &&
    !is.na(message_format[[1L]]) && nzchar(trimws(message_format[[1L]]))
  normalized_format <- if (filter_by_format) {
    toupper(trimws(as.character(message_format[[1L]])))
  } else {
    ""
  }
  Filter(
    function(field) {
      if (!is.list(field) || is.null(field$name) || is.null(field$path)) {
        return(FALSE)
      }
      formats <- as.character(unlist(
        field$message_formats,
        use.names = FALSE
      ))
      !filter_by_format || !length(formats) ||
        normalized_format %in% toupper(trimws(formats))
    },
    fields
  )
}

timeseries_route_config_field_value <- function(route_config, path) {
  route_config <- trimws(as.character(route_config))
  if (
    length(route_config) != 1L ||
      is.na(route_config) ||
      !nzchar(route_config) ||
      !jsonlite::validate(route_config)
  ) {
    return(NULL)
  }
  value <- jsonlite::fromJSON(route_config, simplifyVector = FALSE)
  for (key in as.character(unlist(path, use.names = FALSE))) {
    if (!is.list(value) || is.null(value[[key]])) {
      return(NULL)
    }
    value <- value[[key]]
  }
  value
}

timeseries_route_config_with_ui_fields <- function(
  route_config,
  fields,
  values
) {
  route_config <- trimws(as.character(route_config))
  if (!length(route_config) || is.na(route_config) || !nzchar(route_config)) {
    route_config <- "{}"
  }
  if (!jsonlite::validate(route_config)) {
    stop("Advanced route configuration must be valid JSON.")
  }
  config <- jsonlite::fromJSON(route_config, simplifyVector = FALSE)
  if (!is.list(config) || (length(config) && is.null(names(config)))) {
    stop("Advanced route configuration must be a JSON object.")
  }

  set_path <- function(object, path, value) {
    key <- path[[1L]]
    if (length(path) == 1L) {
      object[[key]] <- value
      return(object)
    }
    child <- object[[key]]
    if (is.null(child) || !is.list(child)) {
      child <- list()
    }
    child <- set_path(child, path[-1L], value)
    if (!length(child)) {
      object[[key]] <- NULL
    } else {
      object[[key]] <- child
    }
    object
  }

  for (field in fields) {
    name <- as.character(field$name[[1L]])
    path <- as.character(unlist(field$path, use.names = FALSE))
    if (!length(path) || any(!nzchar(path))) {
      stop("Route configuration field '", name, "' has an invalid JSON path.")
    }
    value <- values[[name]]
    value_type <- as.character(field$value_type[[1L]])
    missing <- is.null(value) || !length(value) || is.na(value[[1L]]) ||
      (is.character(value[[1L]]) && !nzchar(trimws(value[[1L]])))
    if (missing) {
      config <- set_path(config, path, NULL)
      next
    }

    value <- switch(
      value_type,
      integer = suppressWarnings(as.integer(value[[1L]])),
      numeric = suppressWarnings(as.numeric(value[[1L]])),
      logical = as.logical(value[[1L]]),
      character = trimws(as.character(value[[1L]])),
      stop("Unsupported route configuration value type '", value_type, "'.")
    )
    if (length(value) != 1L || is.na(value)) {
      stop("Enter a valid value for ", field$label[[1L]], ".")
    }
    minimum <- suppressWarnings(as.numeric(field$minimum))
    maximum <- suppressWarnings(as.numeric(field$maximum))
    if (length(minimum) && !is.na(minimum) && value < minimum) {
      stop(field$label[[1L]], " must be at least ", minimum, ".")
    }
    if (length(maximum) && !is.na(maximum) && value > maximum) {
      stop(field$label[[1L]], " must be no more than ", maximum, ".")
    }
    config <- set_path(config, path, value)
  }

  as.character(jsonlite::toJSON(
    config,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
}

timeseries_transmission_mapping <- function(con, timeseries_id) {
  timeseries_id <- suppressWarnings(as.integer(timeseries_id))
  if (is.na(timeseries_id)) {
    return(data.frame())
  }

  DBI::dbGetQuery(
    con,
    "SELECT
       m.transmission_mapping_id,
       m.transmission_route_id,
       m.source_field,
       m.value_multiplier,
       m.value_offset,
       m.missing_values::text AS missing_values,
       m.mapping_config::text AS mapping_config,
       m.enabled,
       s.location_id AS route_location_id,
       CONCAT(r.route_name, ' (#', r.transmission_route_id, ')')
         AS route_label
     FROM continuous.transmission_timeseries_mappings m
     JOIN public.locations_metadata_transmission_routes r
       ON r.transmission_route_id = m.transmission_route_id
     JOIN public.locations_metadata_transmission_setups s
       ON s.transmission_setup_id = r.transmission_setup_id
     WHERE m.timeseries_id = $1
     ORDER BY m.enabled DESC, m.transmission_mapping_id",
    params = list(timeseries_id)
  )
}

timeseries_normalize_transmission_mapping <- function(
  route_id,
  source_field,
  value_multiplier = 1,
  value_offset = 0,
  missing_values = "",
  mapping_config = "{}"
) {
  route_id <- suppressWarnings(as.integer(route_id))
  source_field <- trimws(as.character(source_field))
  value_multiplier <- suppressWarnings(as.numeric(value_multiplier))
  value_offset <- suppressWarnings(as.numeric(value_offset))
  mapping_config <- trimws(as.character(mapping_config))

  if (is.na(route_id) || route_id <= 0L) {
    stop("Select a transmission route.")
  }
  if (length(source_field) != 1L || is.na(source_field) ||
      !nzchar(source_field)) {
    stop("Enter the provider or payload field for this timeseries.")
  }
  if (is.na(value_multiplier) || value_multiplier == 0) {
    stop("The transmission value multiplier must be a non-zero number.")
  }
  if (is.na(value_offset)) {
    stop("The transmission value offset must be numeric.")
  }
  if (!nzchar(mapping_config)) {
    mapping_config <- "{}"
  }
  if (!jsonlite::validate(mapping_config)) {
    stop("Mapping configuration must be valid JSON.")
  }
  if (!startsWith(mapping_config, "{")) {
    stop("Mapping configuration must be a JSON object.")
  }
  parsed_config <- jsonlite::fromJSON(
    mapping_config,
    simplifyVector = FALSE
  )
  if (!is.list(parsed_config) || is.null(names(parsed_config))) {
    if (!identical(parsed_config, list())) {
      stop("Mapping configuration must be a JSON object.")
    }
  }

  missing_values <- trimws(as.character(missing_values))
  missing_values <- if (!nzchar(missing_values)) {
    character()
  } else {
    values <- trimws(strsplit(missing_values, ",", fixed = TRUE)[[1]])
    unique(values[nzchar(values)])
  }

  list(
    transmission_route_id = route_id,
    source_field = source_field,
    value_multiplier = value_multiplier,
    value_offset = value_offset,
    missing_values = jsonlite::toJSON(
      missing_values,
      auto_unbox = FALSE
    ),
    mapping_config = jsonlite::toJSON(
      parsed_config,
      auto_unbox = TRUE,
      null = "null"
    )
  )
}

timeseries_source_args_with_transmission_route <- function(
  source_fx_args,
  transmission_route_id
) {
  transmission_route_id <- suppressWarnings(as.integer(transmission_route_id))
  if (
    length(transmission_route_id) != 1L ||
      is.na(transmission_route_id) ||
      transmission_route_id <= 0L
  ) {
    stop("A valid transmission route is required for this source assignment.")
  }

  source_fx_args <- trimws(as.character(source_fx_args))
  if (
    length(source_fx_args) != 1L ||
      is.na(source_fx_args) ||
      !nzchar(source_fx_args)
  ) {
    source_fx_args <- "{}"
  }
  if (!jsonlite::validate(source_fx_args)) {
    stop("Source arguments must be valid JSON.")
  }
  args <- jsonlite::fromJSON(source_fx_args, simplifyVector = FALSE)
  if (!is.list(args) || (length(args) > 0L && is.null(names(args)))) {
    stop("Source arguments must be a JSON object.")
  }

  args$transmission_route_id <- transmission_route_id
  as.character(jsonlite::toJSON(
    args,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
}

timeseries_sync_transmission_mapping <- function(
  con,
  timeseries_id,
  mapping = NULL
) {
  timeseries_id <- suppressWarnings(as.integer(timeseries_id))
  if (is.na(timeseries_id) || timeseries_id <= 0L) {
    stop("A valid timeseries ID is required for transmission mapping.")
  }

  mappings <- if (is.null(mapping)) {
    list()
  } else if (
    is.list(mapping) &&
      all(c("transmission_route_id", "source_field") %in% names(mapping))
  ) {
    list(mapping)
  } else {
    mapping
  }
  if (!is.list(mappings)) {
    stop("Transmission mappings must be supplied as a list.")
  }
  if (length(mappings)) {
    valid_mapping <- vapply(
      mappings,
      function(x) {
        is.list(x) &&
          all(c("transmission_route_id", "source_field") %in% names(x))
      },
      logical(1)
    )
    if (!all(valid_mapping)) {
      stop("Every transmission mapping must contain a route and source field.")
    }
    route_ids <- vapply(
      mappings,
      function(x) as.integer(x$transmission_route_id),
      integer(1)
    )
    if (anyNA(route_ids) || anyDuplicated(route_ids)) {
      stop("Transmission mappings cannot repeat a transmission route.")
    }
  }

  DBI::dbExecute(
    con,
    "DELETE FROM continuous.transmission_timeseries_mappings
     WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )

  for (mapping_row in mappings) {
    DBI::dbExecute(
      con,
      "INSERT INTO continuous.transmission_timeseries_mappings (
         transmission_route_id,
         source_field,
         timeseries_id,
         value_multiplier,
         value_offset,
         missing_values,
         mapping_config,
         enabled
       ) VALUES ($1, $2, $3, $4, $5, $6::jsonb, $7::jsonb, TRUE)",
      params = list(
        mapping_row$transmission_route_id,
        mapping_row$source_field,
        timeseries_id,
        mapping_row$value_multiplier,
        mapping_row$value_offset,
        mapping_row$missing_values,
        mapping_row$mapping_config
      )
    )
  }

  invisible(NULL)
}

timeseries_parse_utc_datetime <- function(x, label) {
  x <- trimws(as.character(x))
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(label, " must be a valid UTC datetime.")
  }
  value <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  if (is.na(value)) {
    stop(label, " must be a valid UTC datetime.")
  }
  value
}

timeseries_save_transmission_route <- function(
  con,
  location_id,
  setup_id,
  logger_metadata_id,
  transmission_method_id,
  provider_name,
  platform_identifier,
  setup_start_datetime,
  transmission_config,
  route_name,
  endpoint_identifier,
  message_format,
  schedule_reference_time_utc,
  transmit_interval_seconds,
  transmit_window_seconds,
  payload_size_bytes,
  route_config,
  route_id = NULL
) {
  nullable_integer <- function(x) {
    value <- suppressWarnings(as.integer(x))
    if (length(value) == 0L || is.na(value)) NA_integer_ else value
  }
  nullable_text <- function(x) {
    value <- trimws(as.character(x))
    if (length(value) == 0L || is.na(value) || !nzchar(value)) {
      NA_character_
    } else {
      value
    }
  }
  location_id <- nullable_integer(location_id)
  setup_id <- nullable_integer(setup_id)
  route_id <- nullable_integer(route_id)
  if (is.na(location_id)) {
    stop("Select a location before creating a transmission route.")
  }
  transmission_config <- nullable_text(transmission_config)
  route_config <- nullable_text(route_config)
  if (is.na(transmission_config)) transmission_config <- "{}"
  if (is.na(route_config)) route_config <- "{}"
  for (entry in list(
    transmission_config = transmission_config,
    route_config = route_config
  )) {
    if (!jsonlite::validate(entry)) {
      stop("Transmission and route configuration must be valid JSON objects.")
    }
    if (!startsWith(entry, "{")) {
      stop("Transmission and route configuration must be JSON objects.")
    }
    parsed <- jsonlite::fromJSON(entry, simplifyVector = FALSE)
    if (!is.list(parsed) || (length(parsed) > 0L && is.null(names(parsed)))) {
      stop("Transmission and route configuration must be JSON objects.")
    }
  }

  route_name <- nullable_text(route_name)
  if (is.na(route_name)) {
    stop("Enter a route name.")
  }

  DBI::dbBegin(con)
  active <- TRUE
  on.exit({
    if (active) DBI::dbRollback(con)
  }, add = TRUE)

  if (!is.na(route_id)) {
    route_id <- DBI::dbGetQuery(
      con,
      "UPDATE public.locations_metadata_transmission_routes r
       SET route_name = $3,
           endpoint_identifier = $4,
           message_format = $5,
           schedule_reference_time_utc = $6,
           transmit_interval_seconds = $7,
           transmit_window_seconds = $8,
           payload_size_bytes = $9,
           route_config = $10::jsonb
       FROM public.locations_metadata_transmission_setups s
       WHERE r.transmission_route_id = $1
         AND r.transmission_setup_id = s.transmission_setup_id
         AND s.location_id = $2
       RETURNING r.transmission_route_id",
      params = list(
        route_id,
        location_id,
        route_name,
        nullable_text(endpoint_identifier),
        nullable_text(message_format),
        nullable_text(schedule_reference_time_utc),
        nullable_integer(transmit_interval_seconds),
        nullable_integer(transmit_window_seconds),
        nullable_integer(payload_size_bytes),
        route_config
      )
    )$transmission_route_id
    if (length(route_id) != 1L) {
      stop("The selected transmission route no longer exists at this location.")
    }
    DBI::dbCommit(con)
    active <- FALSE
    return(as.integer(route_id[[1L]]))
  }

  if (is.na(setup_id)) {
    logger_metadata_id <- nullable_integer(logger_metadata_id)
    transmission_method_id <- suppressWarnings(
      as.integer(transmission_method_id)
    )
    if (is.na(transmission_method_id)) {
      stop("Select a transmission method.")
    }
    setup_start_datetime <- timeseries_parse_utc_datetime(
      setup_start_datetime,
      "Setup start"
    )
    setup_result <- DBI::dbGetQuery(
      con,
      "INSERT INTO public.locations_metadata_transmission_setups (
         location_id,
         logger_metadata_id,
         transmission_method_id,
         provider_name,
         platform_identifier,
         transmission_config,
         start_datetime
       )
       VALUES ($1, $2, $3, $4, $5, $6::jsonb, $7)
       RETURNING transmission_setup_id",
      params = list(
        location_id,
        logger_metadata_id,
        transmission_method_id,
        nullable_text(provider_name),
        nullable_text(platform_identifier),
        transmission_config,
        setup_start_datetime
      )
    )
    setup_id <- setup_result$transmission_setup_id[[1]]
  }

  route_id <- DBI::dbGetQuery(
    con,
    "INSERT INTO public.locations_metadata_transmission_routes (
       transmission_setup_id,
       route_name,
       endpoint_identifier,
       message_format,
       schedule_reference_time_utc,
       transmit_interval_seconds,
       transmit_window_seconds,
       payload_size_bytes,
       route_config
     ) VALUES (
       $1, $2, $3, $4, $5, $6, $7, $8, $9::jsonb
     )
     RETURNING transmission_route_id",
    params = list(
      setup_id,
      route_name,
      nullable_text(endpoint_identifier),
      nullable_text(message_format),
      nullable_text(schedule_reference_time_utc),
      nullable_integer(transmit_interval_seconds),
      nullable_integer(transmit_window_seconds),
      nullable_integer(payload_size_bytes),
      route_config
    )
  )$transmission_route_id[[1]]

  DBI::dbCommit(con)
  active <- FALSE
  as.integer(route_id)
}
