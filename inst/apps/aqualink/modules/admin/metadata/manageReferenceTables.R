reference_default <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

reference_form_input_id <- function(field_name) {
  paste0("field_", field_name)
}

reference_text_field <- function(
  name,
  label,
  required = FALSE,
  default = NULL,
  storage = "character"
) {
  list(
    name = name,
    label = label,
    input = "text",
    storage = storage,
    required = required,
    default = default
  )
}

reference_textarea_field <- function(
  name,
  label,
  required = FALSE,
  rows = 4,
  default = NULL
) {
  list(
    name = name,
    label = label,
    input = "textarea",
    required = required,
    rows = rows,
    default = default
  )
}

reference_select_field <- function(
  name,
  label,
  choices_query = NULL,
  choices = NULL,
  required = FALSE,
  storage = "integer",
  default = NULL
) {
  list(
    name = name,
    label = label,
    input = "select",
    required = required,
    storage = storage,
    choices_query = choices_query,
    choices = choices,
    default = default
  )
}

reference_numeric_field <- function(
  name,
  label,
  required = FALSE,
  step = "any",
  default = NULL
) {
  list(
    name = name,
    label = label,
    input = "numeric",
    required = required,
    step = step,
    default = default
  )
}

reference_checkbox_field <- function(name, label, default = FALSE) {
  list(
    name = name,
    label = label,
    input = "checkbox",
    required = FALSE,
    default = isTRUE(default)
  )
}

reference_field_default_value <- function(field) {
  if (!"default" %in% names(field) || is.null(field$default)) {
    return(NA)
  }

  field$default
}

reference_deployment_choices_query <- function(
  require_logger = FALSE,
  require_telemetry_component = FALSE
) {
  filters <- character(0)

  if (isTRUE(require_logger)) {
    filters <- c(filters, "COALESCE(i.can_be_logger, FALSE) = TRUE")
  }
  if (isTRUE(require_telemetry_component)) {
    filters <- c(
      filters,
      paste(
        "COALESCE(i.can_be_telemetry_component, FALSE) = TRUE",
        "OR COALESCE(i.can_be_logger, FALSE) = TRUE"
      )
    )
  }

  where_sql <- if (length(filters)) {
    paste("WHERE", paste(filters, collapse = " AND "))
  } else {
    ""
  }

  paste(
    "SELECT lmi.metadata_id AS value,",
    "CONCAT(",
    "  '#', lmi.metadata_id::text,",
    "  ' | ', COALESCE(l.location_code, '?'),",
    "  ' | ', COALESCE(i.serial_no, '[no serial]'),",
    "  CASE WHEN mk.make IS NOT NULL THEN CONCAT(' | ', mk.make) ELSE '' END,",
    "  CASE WHEN mdl.model IS NOT NULL THEN CONCAT(' ', mdl.model) ELSE '' END,",
    "  CASE WHEN it.type IS NOT NULL THEN CONCAT(' | ', it.type) ELSE '' END,",
    "  ' | ', to_char(lmi.start_datetime AT TIME ZONE 'UTC', 'YYYY-MM-DD HH24:MI'), ' UTC'",
    ") AS label",
    "FROM public.locations_metadata_instruments AS lmi",
    "LEFT JOIN public.locations AS l ON lmi.location_id = l.location_id",
    "LEFT JOIN instruments.instruments AS i ON lmi.instrument_id = i.instrument_id",
    "LEFT JOIN instruments.instrument_makes AS mk ON i.make = mk.make_id",
    "LEFT JOIN instruments.instrument_models AS mdl ON i.model = mdl.model_id",
    "LEFT JOIN instruments.instrument_types AS it ON i.type = it.type_id",
    where_sql,
    "ORDER BY l.location_code, i.serial_no, lmi.start_datetime DESC, lmi.metadata_id DESC"
  )
}

reference_timeseries_choices_query <- function(require_basic = FALSE) {
  where_sql <- if (isTRUE(require_basic)) {
    "WHERE ts.timeseries_type = 'basic'"
  } else {
    ""
  }

  paste(
    "SELECT ts.timeseries_id AS value,",
    "CONCAT(",
    "  '#', ts.timeseries_id::text,",
    "  ' | ', COALESCE(l.location_code, '?'),",
    "  ' | ', COALESCE(p.param_name, '?'),",
    "  CASE WHEN ms.matrix_state_name IS NOT NULL THEN CONCAT(' | ', ms.matrix_state_name) ELSE '' END,",
    "  CASE WHEN mt.media_type IS NOT NULL THEN CONCAT(' | ', mt.media_type) ELSE '' END,",
    "  CASE WHEN source.source_fx IS NOT NULL THEN CONCAT(' | ', source.source_fx) ELSE '' END",
    ") AS label",
    "FROM continuous.timeseries AS ts",
    "LEFT JOIN public.locations AS l ON ts.location_id = l.location_id",
    "LEFT JOIN public.parameters AS p ON ts.parameter_id = p.parameter_id",
    "LEFT JOIN public.matrix_states AS ms ON ts.matrix_state_id = ms.matrix_state_id",
    "LEFT JOIN public.media_types AS mt ON ts.media_id = mt.media_id",
    "LEFT JOIN LATERAL (",
    "  SELECT tsa.source_fx",
    "  FROM continuous.timeseries_source_adapters AS tsa",
    "  WHERE tsa.timeseries_id = ts.timeseries_id",
    "    AND tsa.active",
    "    AND tsa.fetch_priority IS NOT NULL",
    "  ORDER BY tsa.fetch_priority, tsa.timeseries_source_adapter_id",
    "  LIMIT 1",
    ") AS source ON TRUE",
    where_sql,
    "ORDER BY l.location_code, p.param_name, ms.matrix_state_name, mt.media_type, ts.timeseries_id"
  )
}

reference_connection_choices_query <- function() {
  paste(
    "SELECT c.connection_id AS value,",
    "CONCAT(",
    "  '#', c.connection_id::text,",
    "  ' | ', COALESCE(ii.serial_no, '[instrument]'),",
    "  ' -> ', COALESCE(li.serial_no, '[logger]'),",
    "  ' | ', COALESCE(cp.protocol_name, '?'),",
    "  ' | ', to_char(c.start_datetime AT TIME ZONE 'UTC', 'YYYY-MM-DD HH24:MI'), ' UTC'",
    ") AS label",
    "FROM public.locations_metadata_instrument_connections AS c",
    "LEFT JOIN public.locations_metadata_instruments AS imi",
    "  ON c.instrument_metadata_id = imi.metadata_id",
    "LEFT JOIN public.locations_metadata_instruments AS lmi",
    "  ON c.logger_metadata_id = lmi.metadata_id",
    "LEFT JOIN instruments.instruments AS ii ON imi.instrument_id = ii.instrument_id",
    "LEFT JOIN instruments.instruments AS li ON lmi.instrument_id = li.instrument_id",
    "LEFT JOIN instruments.communication_protocols AS cp ON c.protocol_id = cp.protocol_id",
    "ORDER BY c.start_datetime DESC, c.connection_id DESC"
  )
}

reference_transmission_setup_choices_query <- function() {
  paste(
    "SELECT s.transmission_setup_id AS value,",
    "CONCAT(",
    "  '#', s.transmission_setup_id::text,",
    "  ' | ', COALESCE(l.location_code, l.name, '[location]'),",
    "  ' | ', COALESCE(i.serial_no, '[no logger]'),",
    "  ' | ', COALESCE(tm.method_name, '?'),",
    "  CASE WHEN s.provider_name IS NOT NULL THEN CONCAT(' | ', s.provider_name) ELSE '' END,",
    "  CASE WHEN s.platform_identifier IS NOT NULL THEN CONCAT(' | ', s.platform_identifier) ELSE '' END,",
    "  ' | ', to_char(s.start_datetime AT TIME ZONE 'UTC', 'YYYY-MM-DD HH24:MI'), ' UTC'",
    ") AS label",
    "FROM public.locations_metadata_transmission_setups AS s",
    "LEFT JOIN public.locations AS l ON s.location_id = l.location_id",
    "LEFT JOIN public.locations_metadata_instruments AS lmi",
    "  ON s.logger_metadata_id = lmi.metadata_id",
    "LEFT JOIN instruments.instruments AS i ON lmi.instrument_id = i.instrument_id",
    "LEFT JOIN instruments.transmission_methods AS tm",
    "  ON s.transmission_method_id = tm.transmission_method_id",
    "ORDER BY s.start_datetime DESC, s.transmission_setup_id DESC"
  )
}

reference_transmission_route_choices_query <- function() {
  paste(
    "SELECT r.transmission_route_id AS value,",
    "CONCAT(",
    "  '#', r.transmission_route_id::text,",
    "  ' | ', COALESCE(l.location_code, '?'),",
    "  ' | ', COALESCE(r.route_name, '[unnamed route]'),",
    "  ' | ', COALESCE(tm.method_name, '?'),",
    "  CASE WHEN s.provider_name IS NOT NULL THEN CONCAT(' | ', s.provider_name) ELSE '' END,",
    "  CASE WHEN s.platform_identifier IS NOT NULL THEN CONCAT(' | ', s.platform_identifier) ELSE '' END",
    ") AS label",
    "FROM public.locations_metadata_transmission_routes AS r",
    "JOIN public.locations_metadata_transmission_setups AS s",
    "  ON r.transmission_setup_id = s.transmission_setup_id",
    "LEFT JOIN public.locations AS l ON s.location_id = l.location_id",
    "LEFT JOIN instruments.transmission_methods AS tm",
    "  ON s.transmission_method_id = tm.transmission_method_id",
    "ORDER BY l.location_code, r.route_name, r.transmission_route_id"
  )
}

reference_table_configs <- function() {
  audit_cols <- c("created", "created_by", "modified", "modified_by")

  list(
    reporting_bases = list(key = "reporting_bases", title = "Reporting bases",
      description = "Retire entries to preserve historical meaning; stable codes identify the analytical definition.",
      schema = "discrete", table = "reporting_bases", pk = "reporting_basis_id", id_label = "ID",
      order_by = "reporting_basis", audit_columns = audit_cols,
      insert_only = c("reporting_basis_code", "reporting_basis", "description", "basis_kind"),
      columns = list(reference_text_field("reporting_basis_code", "Stable code", required = TRUE), reference_text_field("reporting_basis", "Name", required = TRUE), reference_text_field("reporting_basis_fr", "Name (French)"), reference_text_field("basis_kind", "Kind", required = TRUE), reference_textarea_field("description", "Definition", required = TRUE), reference_textarea_field("description_fr", "Definition (French)"), reference_checkbox_field("active", "Active for new selections", default = TRUE))),
    result_speciations = list(key = "result_speciations", title = "Chemical forms and scopes",
      description = "Retire entries to preserve historical meaning; stable codes identify the analytical definition.",
      schema = "discrete", table = "result_speciations", pk = "result_speciation_id", id_label = "ID",
      order_by = "result_speciation", audit_columns = audit_cols,
      insert_only = c("result_speciation_code", "result_speciation", "description"),
      columns = list(reference_text_field("result_speciation_code", "Stable code", required = TRUE), reference_text_field("result_speciation", "Name", required = TRUE), reference_text_field("result_speciation_fr", "Name (French)"), reference_textarea_field("description", "Definition", required = TRUE), reference_textarea_field("description_fr", "Definition (French)"), reference_checkbox_field("active", "Active for new selections", default = TRUE))),
    result_targets = list(key = "result_targets", title = "Analytical targets and materials",
      description = "Retire entries to preserve historical meaning; stable codes identify the analytical definition.",
      schema = "discrete", table = "result_targets", pk = "result_target_id", id_label = "ID",
      order_by = "result_target", audit_columns = audit_cols,
      insert_only = c("result_target_code", "result_target", "description", "target_kind"),
      columns = list(reference_text_field("result_target_code", "Stable code", required = TRUE), reference_text_field("result_target", "Name", required = TRUE), reference_text_field("result_target_fr", "Name (French)"), reference_text_field("target_kind", "Kind", required = TRUE), reference_textarea_field("description", "Definition", required = TRUE), reference_textarea_field("description_fr", "Definition (French)"), reference_checkbox_field("active", "Active for new selections", default = TRUE))),
    organizations = list(
      key = "organizations",
      title = "Organizations",
      description = paste(
        "Create and update owner and contributor organizations used",
        "across locations, timeseries, samples, documents, images,",
        "and instruments."
      ),
      schema = "public",
      table = "organizations",
      pk = "organization_id",
      id_label = "Organization ID",
      order_by = "name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("name", "Name", required = TRUE),
        reference_text_field("name_fr", "Name (French)"),
        reference_text_field("contact_name", "Contact name"),
        reference_text_field("phone", "Phone"),
        reference_text_field("email", "Email"),
        reference_textarea_field("note", "Note")
      )
    ),
    networks = list(
      key = "networks",
      title = "Networks",
      description = paste(
        "Manage the named monitoring networks used for location grouping",
        "and filtering throughout the app."
      ),
      schema = "public",
      table = "networks",
      pk = "network_id",
      id_label = "Network ID",
      order_by = "name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("name", "Name", required = TRUE),
        reference_text_field("name_fr", "Name (French)"),
        reference_select_field(
          "type",
          "Type",
          choices_query = paste(
            "SELECT id AS value, name AS label",
            "FROM public.network_project_types",
            "ORDER BY name"
          ),
          required = TRUE
        ),
        reference_textarea_field(
          "description",
          "Description",
          required = TRUE
        ),
        reference_textarea_field(
          "description_fr",
          "Description (French)"
        )
      )
    ),
    projects = list(
      key = "projects",
      title = "Projects",
      description = paste(
        "Manage the named projects used for location grouping and",
        "filtering throughout the app."
      ),
      schema = "public",
      table = "projects",
      pk = "project_id",
      id_label = "Project ID",
      order_by = "name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("name", "Name", required = TRUE),
        reference_text_field("name_fr", "Name (French)"),
        reference_select_field(
          "type",
          "Type",
          choices_query = paste(
            "SELECT id AS value, name AS label",
            "FROM public.network_project_types",
            "ORDER BY name"
          ),
          required = TRUE
        ),
        reference_textarea_field(
          "description",
          "Description",
          required = TRUE
        ),
        reference_textarea_field(
          "description_fr",
          "Description (French)"
        )
      )
    ),
    network_project_types = list(
      key = "network_project_types",
      title = "Network / Project Types",
      description = paste(
        "Manage the shared type list used by both networks and projects."
      ),
      schema = "public",
      table = "network_project_types",
      pk = "id",
      id_label = "Type ID",
      order_by = "name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("name", "Name", required = TRUE),
        reference_text_field("name_fr", "Name (French)"),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)")
      )
    ),
    location_types = list(
      key = "location_types",
      title = "Location Types",
      description = paste(
        "Manage the location type list used when creating and editing",
        "monitoring locations."
      ),
      schema = "public",
      table = "location_types",
      pk = "type_id",
      id_label = "Type ID",
      order_by = "type",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("type", "Type", required = TRUE),
        reference_text_field("type_fr", "Type (French)"),
        reference_text_field(
          "type_suffix",
          "Type suffix (necessary to generate location codes)"
        )
      )
    ),
    media_types = list(
      key = "media_types",
      title = "Media Types",
      description = paste(
        "Manage the media categories referenced by samples, timeseries,",
        "guidelines, rasters, and related filters."
      ),
      schema = "public",
      table = "media_types",
      pk = "media_id",
      id_label = "Media ID",
      order_by = "media_type",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("media_type", "Media type", required = TRUE),
        reference_text_field(
          "media_type_fr",
          "Media type (French)",
          required = TRUE
        ),
        reference_select_field(
          "default_matrix_state_id",
          "Default matrix state",
          choices_query = paste(
            "SELECT matrix_state_id AS value, matrix_state_name AS label",
            "FROM public.matrix_states",
            "ORDER BY matrix_state_name"
          )
        ),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)")
      )
    ),
    matrix_states = list(
      key = "matrix_states",
      title = "Matrix States",
      description = paste(
        "Manage the physical matrix states used to resolve parameter units",
        "across continuous, discrete, and raster data."
      ),
      schema = "public",
      table = "matrix_states",
      pk = "matrix_state_id",
      id_label = "Matrix State ID",
      order_by = "matrix_state_name",
      audit_columns = character(0),
      columns = list(
        reference_text_field(
          "matrix_state_code",
          "Matrix state code",
          required = TRUE
        ),
        reference_text_field(
          "matrix_state_name",
          "Matrix state name",
          required = TRUE
        ),
        reference_text_field(
          "matrix_state_name_fr",
          "Matrix state name (French)",
          required = TRUE
        ),
        reference_textarea_field("description", "Description")
      )
    ),
    parameter_groups = list(
      key = "parameter_groups",
      title = "Parameter Groups",
      description = paste(
        "Manage the higher-level parameter groups used by mapping and",
        "data-filtering modules."
      ),
      schema = "public",
      table = "parameter_groups",
      pk = "group_id",
      id_label = "Group ID",
      order_by = "group_name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("group_name", "Group name", required = TRUE),
        reference_text_field("group_name_fr", "Group name (French)"),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)")
      )
    ),
    parameter_sub_groups = list(
      key = "parameter_sub_groups",
      title = "Parameter Sub-Groups",
      description = paste(
        "Manage the parameter sub-groups used by mapping and",
        "data-filtering modules."
      ),
      schema = "public",
      table = "parameter_sub_groups",
      pk = "sub_group_id",
      id_label = "Sub-group ID",
      order_by = "sub_group_name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field(
          "sub_group_name",
          "Sub-group name",
          required = TRUE
        ),
        reference_text_field(
          "sub_group_name_fr",
          "Sub-group name (French)"
        ),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)")
      )
    ),
    parameters = list(
      key = "parameters",
      title = "Parameters",
      description = paste(
        "Manage parameter definitions and the parameter group / sub-group",
        "links that drive map and filter groupings."
      ),
      schema = "public",
      table = "parameters",
      pk = "parameter_id",
      id_label = "Parameter ID",
      order_by = "param_name",
      audit_columns = audit_cols,
      display_columns = "relationship_summary",
      column_labels = c(relationship_summary = "Group links"),
      columns = list(
        reference_text_field("param_name", "Parameter name", required = TRUE),
        reference_text_field("param_name_fr", "Parameter name (French)"),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)"),
        reference_select_field(
          "units_liquid",
          "Liquid unit",
          choices_query = paste(
            "SELECT unit_id AS value, unit_name AS label",
            "FROM public.units",
            "ORDER BY unit_name"
          )
        ),
        reference_select_field(
          "units_solid",
          "Solid unit",
          choices_query = paste(
            "SELECT unit_id AS value, unit_name AS label",
            "FROM public.units",
            "ORDER BY unit_name"
          )
        ),
        reference_select_field(
          "units_gas",
          "Gas unit",
          choices_query = paste(
            "SELECT unit_id AS value, unit_name AS label",
            "FROM public.units",
            "ORDER BY unit_name"
          )
        ),
        reference_select_field(
          "units_na",
          "Not applicable unit",
          choices_query = paste(
            "SELECT unit_id AS value, unit_name AS label",
            "FROM public.units",
            "ORDER BY unit_name"
          )
        ),
        reference_text_field("cas_number", "CAS number"),
        reference_checkbox_field(
          "reporting_basis",
          "Reporting basis required"
        ),
        reference_checkbox_field("result_speciation", "Chemical form / species scope required"),
        reference_checkbox_field("result_target", "Analytical target required"),
        reference_checkbox_field("endpoint", "Endpoint determination required"),
        reference_text_field("analysis_conditions_required_keys", "Required analysis condition keys (comma separated)", default = "{}", storage = "text_array"),
        utils::modifyList(reference_textarea_field("analysis_conditions_required_keys_by_matrix", "Required condition keys by matrix (JSON object)", default = "{}"), list(storage = "json_object")),
        reference_checkbox_field(
          "sample_fraction",
          "Sample fraction required"
        ),
        reference_select_field(
          "plot_default_y_orientation",
          "Default plot y-axis orientation",
          choices = data.frame(
            value = c("normal", "inverted"),
            label = c("Normal", "Inverted"),
            stringsAsFactors = FALSE
          ),
          required = TRUE,
          storage = "character"
        ),
        reference_numeric_field("plot_default_floor", "Default plot floor"),
        reference_numeric_field(
          "plot_default_ceiling",
          "Default plot ceiling"
        )
      )
    ),
    communication_protocol_families = list(
      key = "communication_protocol_families",
      title = "Communication Protocol Families",
      description = paste(
        "Manage high-level protocol families used to organize",
        "instrument to logger connection metadata."
      ),
      schema = "instruments",
      table = "communication_protocol_families",
      pk = "protocol_family_id",
      id_label = "Protocol Family ID",
      order_by = "family_name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("family_code", "Family code", required = TRUE),
        reference_text_field("family_name", "Family name", required = TRUE),
        reference_textarea_field("description", "Description")
      )
    ),
    communication_protocols = list(
      key = "communication_protocols",
      title = "Communication Protocols",
      description = paste(
        "Manage specific instrument to logger protocols such as SDI-12,",
        "Modbus, analog, and serial variants."
      ),
      schema = "instruments",
      table = "communication_protocols",
      pk = "protocol_id",
      id_label = "Protocol ID",
      order_by = "protocol_name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("protocol_code", "Protocol code", required = TRUE),
        reference_text_field("protocol_name", "Protocol name", required = TRUE),
        reference_select_field(
          "protocol_family_id",
          "Protocol family",
          choices_query = paste(
            "SELECT protocol_family_id AS value, family_name AS label",
            "FROM instruments.communication_protocol_families",
            "ORDER BY family_name"
          ),
          required = TRUE
        ),
        reference_checkbox_field("uses_logger_port", "Uses logger port"),
        reference_checkbox_field("uses_device_address", "Uses device address"),
        reference_checkbox_field("uses_signal_index", "Uses signal index"),
        reference_checkbox_field("uses_poll_command", "Uses poll command"),
        reference_textarea_field("description", "Description")
      )
    ),
    transmission_method_families = list(
      key = "transmission_method_families",
      title = "Transmission Method Families",
      description = paste(
        "Manage broad telemetry families used when logger data leaves",
        "a site."
      ),
      schema = "instruments",
      table = "transmission_method_families",
      pk = "transmission_method_family_id",
      id_label = "Transmission Family ID",
      order_by = "family_name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("family_code", "Family code", required = TRUE),
        reference_text_field("family_name", "Family name", required = TRUE),
        reference_textarea_field("description", "Description")
      )
    ),
    transmission_methods = list(
      key = "transmission_methods",
      title = "Transmission Methods",
      description = paste(
        "Manage specific telemetry methods such as GOES, cellular IP,",
        "and satellite messaging."
      ),
      schema = "instruments",
      table = "transmission_methods",
      pk = "transmission_method_id",
      id_label = "Transmission Method ID",
      order_by = "method_name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("method_code", "Method code", required = TRUE),
        reference_text_field("method_name", "Method name", required = TRUE),
        reference_select_field(
          "transmission_method_family_id",
          "Transmission family",
          choices_query = paste(
            "SELECT transmission_method_family_id AS value, family_name AS label",
            "FROM instruments.transmission_method_families",
            "ORDER BY family_name"
          ),
          required = TRUE
        ),
        reference_checkbox_field("uses_provider", "Uses provider"),
        reference_checkbox_field(
          "uses_platform_identifier",
          "Uses platform identifier"
        ),
        reference_checkbox_field(
          "uses_route_schedule",
          "Uses route schedule"
        ),
        reference_checkbox_field(
          "uses_payload_format",
          "Uses payload format"
        ),
        reference_textarea_field("description", "Description")
      )
    ),
    transmission_component_roles = list(
      key = "transmission_component_roles",
      title = "Transmission Component Roles",
      description = paste(
        "Manage telemetry component roles such as transmitter, antenna,",
        "battery, and solar panel."
      ),
      schema = "instruments",
      table = "transmission_component_roles",
      pk = "transmission_component_role_id",
      id_label = "Transmission Component Role ID",
      order_by = "role_name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("role_code", "Role code", required = TRUE),
        reference_text_field("role_name", "Role name", required = TRUE),
        reference_textarea_field("description", "Description")
      )
    ),
    instrument_connections = list(
      key = "instrument_connections",
      title = "Instrument / Logger Connections",
      description = paste(
        "Manage time-bounded communication links between deployed",
        "instruments and deployed loggers. Enter timestamps as UTC ISO",
        "values such as 2026-03-26 14:30:00+00."
      ),
      schema = "public",
      table = "locations_metadata_instrument_connections",
      pk = "connection_id",
      id_label = "Connection ID",
      order_by = c("start_datetime", "connection_id"),
      audit_columns = audit_cols,
      columns = list(
        reference_select_field(
          "instrument_metadata_id",
          "Instrument deployment",
          choices_query = reference_deployment_choices_query(),
          required = TRUE
        ),
        reference_select_field(
          "logger_metadata_id",
          "Logger deployment",
          choices_query = reference_deployment_choices_query(
            require_logger = TRUE
          ),
          required = TRUE
        ),
        reference_select_field(
          "protocol_id",
          "Communication protocol",
          choices_query = paste(
            "SELECT protocol_id AS value, protocol_name AS label",
            "FROM instruments.communication_protocols",
            "ORDER BY protocol_name"
          ),
          required = TRUE
        ),
        reference_text_field("logger_port", "Logger port / interface"),
        reference_text_field("address_scope", "Address scope"),
        reference_text_field("device_address", "Device address"),
        reference_textarea_field(
          "protocol_config",
          "Protocol config JSON",
          required = TRUE,
          rows = 5,
          default = "{}"
        ),
        reference_textarea_field("note", "Note"),
        reference_text_field(
          "start_datetime",
          "Start datetime (UTC ISO timestamp)",
          required = TRUE
        ),
        reference_text_field(
          "end_datetime",
          "End datetime (UTC ISO timestamp)"
        )
      )
    ),
    instrument_connection_signals = list(
      key = "instrument_connection_signals",
      title = "Connection Signals",
      description = paste(
        "Manage per-signal mappings for instrument/logger connections,",
        "including optional links to continuous timeseries."
      ),
      schema = "public",
      table = "locations_metadata_instrument_connection_signals",
      pk = "connection_signal_id",
      id_label = "Connection Signal ID",
      order_by = c("connection_id", "signal_order", "connection_signal_id"),
      audit_columns = audit_cols,
      columns = list(
        reference_select_field(
          "connection_id",
          "Connection",
          choices_query = reference_connection_choices_query(),
          required = TRUE
        ),
        reference_text_field("signal_name", "Signal name", required = TRUE),
        reference_select_field(
          "parameter_id",
          "Parameter",
          choices_query = paste(
            "SELECT parameter_id AS value, param_name AS label",
            "FROM public.parameters",
            "ORDER BY param_name"
          )
        ),
        reference_numeric_field(
          "signal_order",
          "Signal order",
          step = 1
        ),
        reference_text_field("logger_input_label", "Logger input label"),
        reference_text_field(
          "protocol_signal_ref",
          "Protocol signal reference"
        ),
        reference_text_field("acquisition_command", "Acquisition command"),
        reference_select_field(
          "timeseries_id",
          "Timeseries",
          choices_query = reference_timeseries_choices_query()
        ),
        reference_select_field(
          "signal_units",
          "Signal units",
          choices_query = paste(
            "SELECT unit_id AS value, unit_name AS label",
            "FROM public.units",
            "ORDER BY unit_name"
          )
        ),
        reference_numeric_field(
          "scale_multiplier",
          "Scale multiplier",
          required = TRUE,
          default = 1
        ),
        reference_numeric_field(
          "scale_offset",
          "Scale offset",
          required = TRUE,
          default = 0
        ),
        reference_textarea_field("note", "Note")
      )
    ),
    transmission_setups = list(
      key = "transmission_setups",
      title = "Transmission Setups",
      description = paste(
        "Manage time-bounded telemetry setups for locations.",
        "A deployed logger can be recorded when known.",
        "Enter timestamps as UTC ISO values."
      ),
      schema = "public",
      table = "locations_metadata_transmission_setups",
      pk = "transmission_setup_id",
      id_label = "Transmission Setup ID",
      order_by = c("start_datetime", "transmission_setup_id"),
      audit_columns = audit_cols,
      columns = list(
        reference_select_field(
          "location_id",
          "Location",
          choices_query = paste(
            "SELECT location_id AS value,",
            "CONCAT(COALESCE(location_code, '[no code]'), ' | ', name) AS label",
            "FROM public.locations",
            "ORDER BY location_code, name"
          ),
          required = TRUE
        ),
        reference_select_field(
          "logger_metadata_id",
          "Logger deployment (optional)",
          choices_query = reference_deployment_choices_query(
            require_logger = TRUE
          ),
          required = FALSE
        ),
        reference_select_field(
          "transmission_method_id",
          "Transmission method",
          choices_query = paste(
            "SELECT transmission_method_id AS value, method_name AS label",
            "FROM instruments.transmission_methods",
            "ORDER BY method_name"
          ),
          required = TRUE
        ),
        reference_text_field("provider_name", "Provider name"),
        reference_text_field("platform_identifier", "Platform identifier"),
        reference_textarea_field(
          "transmission_config",
          "Transmission config JSON",
          required = TRUE,
          rows = 5,
          default = "{}"
        ),
        reference_textarea_field("note", "Note"),
        reference_text_field(
          "start_datetime",
          "Start datetime (UTC ISO timestamp)",
          required = TRUE
        ),
        reference_text_field(
          "end_datetime",
          "End datetime (UTC ISO timestamp)"
        )
      )
    ),
    transmission_routes = list(
      key = "transmission_routes",
      title = "Transmission Routes",
      description = paste(
        "Manage route-level transmission schedules and endpoints for each",
        "telemetry setup."
      ),
      schema = "public",
      table = "locations_metadata_transmission_routes",
      pk = "transmission_route_id",
      id_label = "Transmission Route ID",
      order_by = c("transmission_setup_id", "route_name"),
      audit_columns = audit_cols,
      columns = list(
        reference_select_field(
          "transmission_setup_id",
          "Transmission setup",
          choices_query = reference_transmission_setup_choices_query(),
          required = TRUE
        ),
        reference_text_field("route_name", "Route name", required = TRUE),
        reference_text_field("endpoint_identifier", "Endpoint identifier"),
        reference_text_field("message_format", "Message format"),
        reference_text_field(
          "schedule_reference_time_utc",
          "Schedule reference time (UTC HH:MM[:SS])"
        ),
        reference_numeric_field(
          "transmit_interval_seconds",
          "Transmit interval (seconds)",
          step = 1
        ),
        reference_numeric_field(
          "transmit_window_seconds",
          "Transmit window (seconds)",
          step = 1
        ),
        reference_numeric_field(
          "payload_size_bytes",
          "Payload size (bytes)",
          step = 1
        ),
        reference_textarea_field(
          "route_config",
          "Route config JSON",
          required = TRUE,
          rows = 5,
          default = "{}"
        ),
        reference_textarea_field("note", "Note")
      )
    ),
    transmission_timeseries_mappings = list(
      key = "transmission_timeseries_mappings",
      title = "Transmission Timeseries Mappings",
      description = paste(
        "Map provider or payload fields from a transmission route to basic",
        "continuous timeseries. The target timeseries must belong to the",
        "same location as the route's logger deployment."
      ),
      schema = "continuous",
      table = "transmission_timeseries_mappings",
      pk = "transmission_mapping_id",
      id_label = "Transmission Mapping ID",
      order_by = c(
        "transmission_route_id",
        "source_field",
        "transmission_mapping_id"
      ),
      audit_columns = audit_cols,
      columns = list(
        reference_select_field(
          "transmission_route_id",
          "Transmission route",
          choices_query = reference_transmission_route_choices_query(),
          required = TRUE
        ),
        reference_text_field(
          "source_field",
          "Source field",
          required = TRUE
        ),
        reference_select_field(
          "timeseries_id",
          "Basic timeseries",
          choices_query = reference_timeseries_choices_query(
            require_basic = TRUE
          ),
          required = TRUE
        ),
        reference_numeric_field(
          "value_multiplier",
          "Value multiplier",
          required = TRUE,
          default = 1
        ),
        reference_numeric_field(
          "value_offset",
          "Value offset",
          required = TRUE,
          default = 0
        ),
        reference_textarea_field(
          "missing_values",
          "Missing values JSON array",
          required = TRUE,
          rows = 3,
          default = "[]"
        ),
        reference_textarea_field(
          "mapping_config",
          "Mapping config JSON object",
          required = TRUE,
          rows = 5,
          default = "{}"
        ),
        reference_checkbox_field("enabled", "Enabled", default = TRUE),
        reference_textarea_field("note", "Note")
      )
    ),
    transmission_import_runs = list(
      key = "transmission_import_runs",
      title = "Transmission Import History",
      description = paste(
        "Review provider-neutral transmission retrieval history, query",
        "windows, source metadata, parsed measurements, and failures.",
        "These operational records are written by importers and are read-only."
      ),
      schema = "continuous",
      table = "transmission_import_runs",
      pk = "transmission_import_run_id",
      id_label = "Transmission Import Run ID",
      order_by = c("started", "transmission_import_run_id"),
      order_by_desc = c("started", "transmission_import_run_id"),
      row_limit = 1000L,
      audit_columns = audit_cols,
      editable = FALSE,
      columns = list(
        reference_select_field(
          "transmission_route_id",
          "Transmission route",
          choices_query = reference_transmission_route_choices_query(),
          required = TRUE
        ),
        reference_text_field("query_since", "Query since (UTC)"),
        reference_text_field("query_until", "Query until (UTC)"),
        reference_text_field("importer", "Importer"),
        reference_text_field("source_server", "Source server"),
        reference_text_field("status", "Status"),
        reference_numeric_field("payload_bytes", "Payload bytes"),
        reference_numeric_field(
          "transmissions_received",
          "Transmissions received"
        ),
        reference_numeric_field(
          "measurements_parsed",
          "Measurements parsed"
        ),
        reference_numeric_field(
          "measurements_inserted",
          "Measurements inserted"
        ),
        reference_text_field(
          "last_message_datetime",
          "Last message datetime (UTC)"
        ),
        reference_text_field("payload_reference", "Payload reference"),
        reference_textarea_field(
          "source_metadata",
          "Source metadata JSON",
          rows = 5
        ),
        reference_textarea_field("error_message", "Error message", rows = 5),
        reference_text_field("started", "Started (UTC)"),
        reference_text_field("completed", "Completed (UTC)")
      )
    ),
    transmission_components = list(
      key = "transmission_components",
      title = "Transmission Components",
      description = paste(
        "Manage deployed telemetry hardware components attached to each",
        "transmission setup."
      ),
      schema = "public",
      table = "locations_metadata_transmission_components",
      pk = "transmission_component_id",
      id_label = "Transmission Component ID",
      order_by = c(
        "transmission_setup_id",
        "transmission_component_role_id",
        "component_metadata_id"
      ),
      audit_columns = audit_cols,
      columns = list(
        reference_select_field(
          "transmission_setup_id",
          "Transmission setup",
          choices_query = reference_transmission_setup_choices_query(),
          required = TRUE
        ),
        reference_select_field(
          "component_metadata_id",
          "Component deployment",
          choices_query = reference_deployment_choices_query(
            require_telemetry_component = TRUE
          ),
          required = TRUE
        ),
        reference_select_field(
          "transmission_component_role_id",
          "Component role",
          choices_query = paste(
            "SELECT transmission_component_role_id AS value, role_name AS label",
            "FROM instruments.transmission_component_roles",
            "ORDER BY role_name"
          ),
          required = TRUE
        ),
        reference_checkbox_field("is_primary", "Primary for this role"),
        reference_textarea_field("note", "Note")
      )
    )
  )
}

reference_quote_identifier_chr <- function(con, value) {
  as.character(DBI::dbQuoteIdentifier(con, value))
}

reference_qualified_table_chr <- function(con, config) {
  as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = config$schema, table = config$table)
  ))
}

reference_choice_df <- function(field, con = NULL) {
  choice_df <- if (!is.null(field$choices_query)) {
    DBI::dbGetQuery(con, field$choices_query)
  } else {
    field$choices
  }

  if (!all(c("value", "label") %in% names(choice_df))) {
    stop(
      "Select field choices must provide columns named 'value' and 'label'."
    )
  }

  choice_df$value <- as.character(choice_df$value)
  choice_df$label <- as.character(choice_df$label)
  missing_label <- is.na(choice_df$label) | !nzchar(choice_df$label)
  choice_df$label[missing_label] <- choice_df$value[missing_label]
  choice_df
}

reference_load_choices <- function(con, fields) {
  out <- list()

  for (field in fields) {
    if (!identical(field$input, "select")) {
      next
    }

    out[[field$name]] <- reference_choice_df(field, con = con)
  }

  out
}

reference_make_select_choices <- function(choice_df) {
  if (is.null(choice_df) || !nrow(choice_df)) {
    return(character(0))
  }

  stats::setNames(choice_df$value, choice_df$label)
}

reference_make_input_choices <- function(choice_df, blank_label = "") {
  c(stats::setNames("", blank_label), reference_make_select_choices(choice_df))
}

reference_empty_choices <- function(fields) {
  stats::setNames(
    rep(list(NULL), length(fields)),
    vapply(fields, `[[`, character(1), "name")
  )
}

reference_empty_row <- function(fields) {
  empty <- as.data.frame(
    setNames(
      lapply(fields, reference_field_default_value),
      vapply(fields, `[[`, character(1), "name")
    ),
    stringsAsFactors = FALSE
  )

  empty
}

reference_render_fields_ui <- function(ns, fields, choices, values = NULL) {
  tagList(lapply(fields, function(field) {
    input_id <- ns(reference_form_input_id(field$name))
    field_value <- if (
      !is.null(values) &&
        field$name %in% names(values) &&
        length(values[[field$name]])
    ) {
      values[[field$name]][1]
    } else {
      reference_field_default_value(field)
    }

    if (identical(field$storage, "text_array")) {
      field_value <- paste(unlist(values[[field$name]]), collapse = ", ")
      if (startsWith(field_value, "{") && endsWith(field_value, "}")) field_value <- substr(field_value, 2L, nchar(field_value) - 1L)
    }
    if (identical(field$input, "text")) {
      return(textInput(
        input_id,
        field$label,
        value = if (is.na(field_value)) "" else as.character(field_value)
      ))
    }

    if (identical(field$input, "textarea")) {
      return(textAreaInput(
        input_id,
        field$label,
        value = if (is.na(field_value)) "" else as.character(field_value),
        width = "100%",
        height = paste0(reference_default(field$rows, 4) * 22, "px")
      ))
    }

    if (identical(field$input, "select")) {
      return(selectInput(
        input_id,
        field$label,
        choices = reference_make_input_choices(choices[[field$name]]),
        selected = if (is.na(field_value)) "" else as.character(field_value),
        selectize = FALSE
      ))
    }

    if (identical(field$input, "numeric")) {
      return(numericInput(
        input_id,
        field$label,
        value = if (is.na(field_value)) NA_real_ else as.numeric(field_value),
        step = reference_default(field$step, "any")
      ))
    }

    if (identical(field$input, "checkbox")) {
      return(checkboxInput(
        input_id,
        field$label,
        value = isTRUE(field_value)
      ))
    }

    NULL
  }))
}

reference_normalize_value <- function(value, field) {
  if (identical(field$input, "checkbox")) {
    return(isTRUE(value))
  }

  if (identical(field$input, "numeric")) {
    if (is.null(value) || !length(value) || is.na(value[[1]])) {
      return(NA_real_)
    }
    return(as.numeric(value[[1]]))
  }

  if (identical(field$storage, "json_object")) {
    text <- as.character(reference_default(value, "{}"))
    if (!length(text) || !nzchar(trimws(text))) text <- "{}"
    if (!grepl("^[[:space:]]*\\{", text) || !jsonlite::validate(text)) stop("Enter a JSON object of matrix codes and required condition keys.")
    return(text)
  }
  if (identical(field$storage, "text_array")) {
    keys <- unique(trimws(strsplit(as.character(reference_default(value, "")), ",", fixed = TRUE)[[1]]))
    keys <- keys[nzchar(keys) & keys != "{}"]
    if (!length(keys)) return("{}")
    # Keys use the usual stable identifier vocabulary. Bound PostgreSQL array
    # literals avoid multi-row binding and support arbitrary additional keys.
    if (any(!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", keys))) stop("Enter condition keys separated by commas.")
    return(paste0("{", paste(keys, collapse = ","), "}"))
  }
  first_value <- if (is.null(value) || !length(value)) {
    NA
  } else {
    value[[1]]
  }

  if (identical(field$input, "select")) {
    selected_value <- as.character(reference_default(
      first_value,
      NA_character_
    ))
    if (is.na(selected_value) || !nzchar(selected_value)) {
      return(
        if (identical(field$storage, "integer")) {
          NA_integer_
        } else {
          NA_character_
        }
      )
    }

    return(
      if (identical(field$storage, "integer")) {
        as.integer(selected_value)
      } else {
        selected_value
      }
    )
  }

  if (identical(field$input, "text") || identical(field$input, "textarea")) {
    text_value <- as.character(reference_default(first_value, NA_character_))
    text_value <- trimws(text_value)
    if (is.na(text_value) || !nzchar(text_value)) {
      return(NA_character_)
    }
    return(text_value)
  }

  first_value
}

# Shared by the generic and parameter editors so malformed JSON/arrays are
# handled as recoverable form errors before attempting a database write.
reference_collect_values <- function(input, fields) {
  values <- lapply(fields, function(field)
    reference_normalize_value(input[[reference_form_input_id(field$name)]], field))
  names(values) <- vapply(fields, `[[`, character(1), "name")
  values
}

reference_is_missing_value <- function(value) {
  if (is.logical(value)) {
    return(FALSE)
  }
  if (is.numeric(value) || is.integer(value)) {
    return(is.na(value))
  }
  is.na(value) || (is.character(value) && !nzchar(value))
}

reference_update_fields <- function(session, fields, row, choices) {
  for (field in fields) {
    input_id <- reference_form_input_id(field$name)
    value <- row[[field$name]][1]
    if (identical(field$storage, "text_array")) {
      value <- paste(unlist(row[[field$name]]), collapse = ", ")
      if (startsWith(value, "{") && endsWith(value, "}")) value <- substr(value, 2L, nchar(value) - 1L)
    }

    if (identical(field$input, "text")) {
      updateTextInput(
        session,
        input_id,
        value = if (is.na(value)) "" else as.character(value)
      )
      next
    }

    if (identical(field$input, "textarea")) {
      updateTextAreaInput(
        session,
        input_id,
        value = if (is.na(value)) "" else as.character(value)
      )
      next
    }

    if (identical(field$input, "select")) {
      updateSelectInput(
        session,
        input_id,
        choices = reference_make_input_choices(choices[[field$name]]),
        selected = if (is.na(value)) "" else as.character(value)
      )
      next
    }

    if (identical(field$input, "numeric")) {
      updateNumericInput(
        session,
        input_id,
        value = if (is.na(value)) NA_real_ else as.numeric(value)
      )
      next
    }

    if (identical(field$input, "checkbox")) {
      updateCheckboxInput(
        session,
        input_id,
        value = isTRUE(value)
      )
    }
  }
}

reference_clear_fields <- function(session, fields, choices) {
  reference_update_fields(
    session,
    fields,
    reference_empty_row(fields),
    choices
  )
}

reference_field_label <- function(config, column_name) {
  if (
    !is.null(config$column_labels) &&
      column_name %in% names(config$column_labels)
  ) {
    return(unname(config$column_labels[[column_name]]))
  }

  field_names <- vapply(config$columns, `[[`, character(1), "name")
  field_idx <- match(column_name, field_names)
  if (!is.na(field_idx)) {
    return(config$columns[[field_idx]]$label)
  }

  switch(
    column_name,
    created = "Created",
    created_by = "Created by",
    modified = "Modified",
    modified_by = "Modified by",
    column_name
  )
}

reference_display_labels <- function(config, column_names) {
  vapply(
    column_names,
    function(column_name) {
      if (identical(column_name, config$pk)) {
        return(reference_default(config$id_label, column_name))
      }
      reference_field_label(config, column_name)
    },
    character(1)
  )
}

reference_build_display_data <- function(raw, config, choices) {
  display_cols <- unique(c(
    config$pk,
    vapply(config$columns, `[[`, character(1), "name"),
    reference_default(config$display_columns, character(0)),
    reference_default(config$audit_columns, character(0))
  ))
  display_cols <- intersect(display_cols, names(raw))

  if (!length(display_cols)) {
    return(data.frame())
  }

  display <- raw[, display_cols, drop = FALSE]

  for (field in config$columns) {
    if (
      !identical(field$input, "select") ||
        !field$name %in% names(display)
    ) {
      next
    }

    choice_df <- choices[[field$name]]
    if (is.null(choice_df) || !nrow(choice_df)) {
      next
    }

    display[[field$name]] <- choice_df$label[match(
      as.character(display[[field$name]]),
      choice_df$value
    )]
  }

  display
}

reference_prepare_for_dt <- function(df) {
  if (!ncol(df)) {
    return(df)
  }

  out <- df

  for (col_name in names(out)) {
    column <- out[[col_name]]

    if (inherits(column, c("POSIXct", "POSIXt", "Date"))) {
      out[[col_name]] <- as.character(column)
      next
    }

    if (is.logical(column)) {
      out[[col_name]] <- ifelse(
        is.na(column),
        "",
        ifelse(column, "TRUE", "FALSE")
      )
      next
    }

    if (inherits(column, "integer64")) {
      out[[col_name]] <- as.character(column)
      next
    }

    if (is.list(column)) {
      out[[col_name]] <- vapply(
        column,
        function(value) {
          if (length(value) == 0 || all(is.na(value))) {
            ""
          } else {
            paste(as.character(value), collapse = ", ")
          }
        },
        character(1)
      )
    }
  }

  out
}

reference_fetch_records <- function(con, config) {
  order_cols <- reference_default(config$order_by, config$pk)
  descending_cols <- reference_default(
    config$order_by_desc,
    character(0)
  )
  order_sql <- paste(vapply(
    order_cols,
    function(column_name) {
      paste0(
        reference_quote_identifier_chr(con, column_name),
        if (column_name %in% descending_cols) " DESC" else ""
      )
    },
    character(1)
  ), collapse = ", ")
  row_limit <- suppressWarnings(as.integer(reference_default(
    config$row_limit,
    NA_integer_
  )))
  limit_sql <- if (!is.na(row_limit) && row_limit > 0L) {
    paste("LIMIT", row_limit)
  } else {
    ""
  }

  DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM %s ORDER BY %s %s",
      reference_qualified_table_chr(con, config),
      order_sql,
      limit_sql
    )
  )
}

reference_sql_params <- function(values) {
  unname(as.list(values))
}

reference_save_record <- function(
  con,
  config,
  values,
  selected_id = NULL,
  existing_columns = character(0)
) {
  # Definitions keep their scientific meaning once an ID has been created.
  if (!is.null(selected_id)) values <- values[setdiff(names(values), config$insert_only)]
  column_names <- names(values)
  quoted_columns <- reference_quote_identifier_chr(con, column_names)

  if (is.null(selected_id)) {
    insert_sql <- sprintf(
      "INSERT INTO %s (%s) VALUES (%s) RETURNING %s",
      reference_qualified_table_chr(con, config),
      paste(quoted_columns, collapse = ", "),
      paste0("$", seq_along(column_names), collapse = ", "),
      reference_quote_identifier_chr(con, config$pk)
    )

    inserted <- DBI::dbGetQuery(
      con,
      insert_sql,
      params = reference_sql_params(values)
    )
    return(inserted[[config$pk]][1])
  }

  set_sql <- paste(
    sprintf("%s = $%d", quoted_columns, seq_along(column_names)),
    collapse = ", "
  )

  if ("modified" %in% existing_columns) {
    set_sql <- paste(
      set_sql,
      sprintf(
        "%s = CURRENT_TIMESTAMP",
        reference_quote_identifier_chr(con, "modified")
      ),
      sep = ", "
    )
  }
  if ("modified_by" %in% existing_columns) {
    set_sql <- paste(
      set_sql,
      sprintf(
        "%s = CURRENT_USER",
        reference_quote_identifier_chr(con, "modified_by")
      ),
      sep = ", "
    )
  }

  update_sql <- sprintf(
    "UPDATE %s SET %s WHERE %s = $%d",
    reference_qualified_table_chr(con, config),
    set_sql,
    reference_quote_identifier_chr(con, config$pk),
    length(column_names) + 1
  )

  DBI::dbExecute(
    con,
    update_sql,
    params = reference_sql_params(c(
      values,
      list(type.convert(as.character(selected_id), as.is = TRUE))
    ))
  )

  type.convert(as.character(selected_id), as.is = TRUE)
}

clear_reference_caches <- function(session) {
  cache_keys <- c(
    "cont_data.plot_module_data",
    "disc_data_module_data",
    "disc_plot_module_data_chemistry",
    "map_params_module_data",
    "map_location_module_data"
  )

  cache_envs <- list(.GlobalEnv)
  if (is.environment(session$userData$app_cache)) {
    cache_envs[[2L]] <- session$userData$app_cache
  }
  # Reference edits affect all reader roles. Clear both unscoped and scoped
  # copies in the shared public cache and the editor's private cache.
  for (env in cache_envs) {
    for (cache_key in cache_keys) {
      clear_cached(cache_key, env = env, all_scopes = TRUE)
    }
  }
}

referenceTableManagerUI <- function(id, config, title = NULL) {
  ns <- NS(id)
  title <- reference_default(title, config$title)
  editable <- !identical(config$editable, FALSE)

  bslib::layout_sidebar(
    YGwater:::shiny_ui_ready(ns("ui_ready")),
    sidebar = bslib::sidebar(
      width = 430,
      open = "always",
      title = title,
      uiOutput(ns("panel_intro")),
      uiOutput(ns("record_summary")),
      if (editable) {
        tagList(
          div(
            class = "d-flex gap-2 flex-wrap mb-3",
            actionButton(ns("save"), "Save", class = "btn-primary"),
            actionButton(ns("new_record"), "New record"),
            actionButton(ns("reload"), "Reload")
          ),
          reference_render_fields_ui(
            ns,
            config$columns,
            choices = reference_empty_choices(config$columns),
            values = reference_empty_row(config$columns)
          )
        )
      } else {
        actionButton(ns("reload"), "Reload")
      }
    ),
    uiOutput(ns("banner")),
    uiOutput(ns("table_heading")),
    DT::DTOutput(ns("records_table"))
  )
}

referenceTableManager <- function(
  id,
  language,
  config,
  notification_module_id
) {
  moduleServer(id, function(input, output, session) {
    con <- session$userData$AquaCache
    editable <- !identical(config$editable, FALSE)
    table_proxy <- DT::dataTableProxy("records_table", session = session)

    editor_data <- reactiveValues(
      choices = list(),
      records_raw = data.frame(),
      records_display = data.frame()
    )
    current_record_id <- reactiveVal(NULL)
    current_row <- reactiveVal(reference_empty_row(config$columns))

    observe({
      for (field in config$insert_only) {
        shinyjs::toggleState(reference_form_input_id(field), condition = is.null(current_record_id()))
      }
    })

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = session$ns,
        lang = language$language,
        con = con,
        module_id = notification_module_id
      )
    })
    outputOptions(output, "banner", suspendWhenHidden = FALSE)

    refresh_editor <- function(selected_id = NULL) {
      selected_id <- if (is.null(selected_id) || !length(selected_id)) {
        NULL
      } else {
        as.character(selected_id)[1]
      }

      choices <- reference_load_choices(con, config$columns)
      raw <- reference_fetch_records(con, config)
      display <- reference_build_display_data(raw, config, choices)

      editor_data$choices <- choices
      editor_data$records_raw <- raw
      editor_data$records_display <- display

      session$onFlushed(
        function() {
          if (is.null(selected_id)) {
            empty_row <- reference_empty_row(config$columns)
            current_record_id(NULL)
            current_row(empty_row)
            if (editable) {
              reference_clear_fields(session, config$columns, choices)
            }
            DT::selectRows(table_proxy, NULL)
            return()
          }

          selected_idx <- match(
            as.character(selected_id),
            as.character(raw[[config$pk]])
          )

          if (is.na(selected_idx)) {
            empty_row <- reference_empty_row(config$columns)
            current_record_id(NULL)
            current_row(empty_row)
            if (editable) {
              reference_clear_fields(session, config$columns, choices)
            }
            DT::selectRows(table_proxy, NULL)
            return()
          }

          selected_row <- raw[selected_idx, , drop = FALSE]
          current_record_id(as.character(selected_id))
          current_row(selected_row)
          if (editable) {
            reference_update_fields(
              session,
              config$columns,
              selected_row,
              choices
            )
          }
          DT::selectRows(table_proxy, selected_idx)
        },
        once = TRUE
      )
    }

    output$panel_intro <- renderUI({
      tagList(
        tags$p(class = "text-muted", config$description),
        if (length(config$insert_only)) tags$p(class = "text-muted small",
          "Definitions are set when creating an entry. For existing entries, edit French translations or active status; create a new entry for a different definition."),
        tags$p(
          class = "text-muted small",
          if (editable) {
            paste(
              "This editor supports adds and updates.",
              "Deletes are intentionally excluded because these tables are",
              "widely referenced elsewhere in the application."
            )
          } else {
            paste(
              "This view is read-only.",
              "Records are maintained by their responsible workflow."
            )
          }
        )
      )
    })
    outputOptions(output, "panel_intro", suspendWhenHidden = FALSE)

    output$record_summary <- renderUI({
      record_id <- current_record_id()

      tags$p(
        tags$strong("Mode: "),
        if (!editable) {
          if (is.null(record_id)) "Reviewing history" else paste(
            "Selected",
            reference_default(config$id_label, config$pk),
            record_id
          )
        } else if (is.null(record_id)) {
          paste("New", tolower(config$title), "record")
        } else {
          paste(
            "Editing",
            reference_default(config$id_label, config$pk),
            record_id
          )
        }
      )
    })
    outputOptions(output, "record_summary", suspendWhenHidden = FALSE)

    output$table_heading <- renderUI({
      row_count <- nrow(editor_data$records_display)

      tags$div(
        class = "mb-3",
        tags$h4(paste(config$title, "records")),
        tags$p(
          class = "text-muted",
          if (editable) {
            paste(
              "Select a row to load it into the form.",
              row_count,
              "record(s) currently available."
            )
          } else {
            paste(
              "Use the column filters to review",
              row_count,
              "recent record(s)."
            )
          }
        )
      )
    })
    outputOptions(output, "table_heading", suspendWhenHidden = FALSE)

    output$records_table <- DT::renderDT(
      {
        display <- reference_prepare_for_dt(editor_data$records_display)
        DT::datatable(
          display,
          rownames = FALSE,
          selection = if (editable) "single" else "none",
          filter = "top",
          colnames = unname(reference_display_labels(config, names(display))),
          options = list(
            pageLength = 10,
            lengthChange = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE
          )
        )
      },
      server = FALSE
    )
    outputOptions(output, "records_table", suspendWhenHidden = FALSE)

    observeEvent(input$reload, {
      selected_id <- current_record_id()
      refresh_editor(selected_id = selected_id)
    })

    observeEvent(input$new_record, {
      if (!editable) {
        return()
      }
      empty_row <- reference_empty_row(config$columns)
      current_record_id(NULL)
      current_row(empty_row)
      reference_clear_fields(session, config$columns, editor_data$choices)
      DT::selectRows(table_proxy, NULL)
    })

    observeEvent(
      input$records_table_rows_selected,
      {
        req(length(input$records_table_rows_selected) == 1)
        selected_row <- input$records_table_rows_selected[[1]]
        req(nrow(editor_data$records_raw) >= selected_row)

        selected_id <- as.character(
          editor_data$records_raw[[config$pk]][selected_row]
        )

        form_row <- editor_data$records_raw[selected_row, , drop = FALSE]
        current_record_id(selected_id)
        current_row(form_row)
        if (editable) {
          reference_update_fields(
            session,
            config$columns,
            form_row,
            editor_data$choices
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$save,
      {
        if (!editable) {
          showNotification(
            paste(config$title, "is read-only."),
            type = "error"
          )
          return()
        }
        values <- tryCatch(reference_collect_values(input, config$columns), error = function(e) e)
        if (inherits(values, "error")) {
          showNotification(paste("Save failed:", conditionMessage(values)), type = "error", duration = 15)
          return()
        }

        validation_errors <- character(0)
        for (field in config$columns) {
          if (
            isTRUE(field$required) &&
              reference_is_missing_value(values[[field$name]])
          ) {
            validation_errors <- c(
              validation_errors,
              paste(field$label, "is required.")
            )
          }
        }

        if (length(validation_errors)) {
          showNotification(
            paste(validation_errors, collapse = " "),
            type = "error"
          )
          return()
        }

        tryCatch(
          {
            saved_id <- reference_save_record(
              con = con,
              config = config,
              values = values,
              selected_id = current_record_id(),
              existing_columns = names(editor_data$records_raw)
            )

            clear_reference_caches(session)
            refresh_editor(selected_id = saved_id)
            showNotification(
              paste(config$title, "saved."),
              type = "message"
            )
          },
          error = function(e) {
            showNotification(
              paste("Save failed:", conditionMessage(e)),
              type = "error",
              duration = 10
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$ui_ready, refresh_editor(), once = TRUE, ignoreInit = TRUE)
  })
}

parameter_relationships_empty <- function() {
  data.frame(
    relationship_id = integer(0),
    group_id = integer(0),
    group_name = character(0),
    sub_group_id = integer(0),
    sub_group_name = character(0),
    stringsAsFactors = FALSE
  )
}

parameter_relationship_keys <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(character(0))
  }

  paste(
    as.character(df$group_id),
    ifelse(is.na(df$sub_group_id), "NA", as.character(df$sub_group_id)),
    sep = "::"
  )
}

fetch_parameter_records <- function(con, config) {
  raw <- reference_fetch_records(con, config)
  raw$relationship_summary <- ""

  relationships <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  pr.parameter_id,",
      "  pg.group_name,",
      "  sg.sub_group_name",
      "FROM public.parameter_relationships pr",
      "JOIN public.parameter_groups pg ON pg.group_id = pr.group_id",
      "LEFT JOIN public.parameter_sub_groups sg",
      "  ON sg.sub_group_id = pr.sub_group_id",
      "ORDER BY pr.parameter_id, pg.group_name, sg.sub_group_name"
    )
  )

  if (!nrow(raw) || !nrow(relationships)) {
    return(raw)
  }

  relationship_labels <- ifelse(
    is.na(relationships$sub_group_name),
    relationships$group_name,
    paste(relationships$group_name, relationships$sub_group_name, sep = " / ")
  )

  relationship_summary <- tapply(
    relationship_labels,
    relationships$parameter_id,
    function(values) paste(unique(values), collapse = "; ")
  )

  matched_summary <- relationship_summary[match(
    as.character(raw$parameter_id),
    names(relationship_summary)
  )]
  raw$relationship_summary <- ifelse(
    is.na(matched_summary),
    "",
    as.character(matched_summary)
  )

  raw
}

fetch_parameter_relationships <- function(con, parameter_id) {
  if (is.null(parameter_id) || !length(parameter_id) || is.na(parameter_id)) {
    return(parameter_relationships_empty())
  }

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  pr.relationship_id,",
      "  pr.group_id,",
      "  pg.group_name,",
      "  pr.sub_group_id,",
      "  sg.sub_group_name",
      "FROM public.parameter_relationships pr",
      "JOIN public.parameter_groups pg ON pg.group_id = pr.group_id",
      "LEFT JOIN public.parameter_sub_groups sg",
      "  ON sg.sub_group_id = pr.sub_group_id",
      "WHERE pr.parameter_id = $1",
      "ORDER BY pg.group_name, sg.sub_group_name"
    ),
    params = list(as.integer(parameter_id))
  )
}

fetch_parameter_relationship_choices <- function(con) {
  list(
    groups = DBI::dbGetQuery(
      con,
      paste(
        "SELECT group_id AS value, group_name AS label",
        "FROM public.parameter_groups",
        "ORDER BY group_name"
      )
    ),
    sub_groups = DBI::dbGetQuery(
      con,
      paste(
        "SELECT sub_group_id AS value, sub_group_name AS label",
        "FROM public.parameter_sub_groups",
        "ORDER BY sub_group_name"
      )
    )
  )
}

parameter_unit_fields <- function() {
  data.frame(
    unit_column = c("units_liquid", "units_solid", "units_gas", "units_na"),
    unit_label = c("Liquid unit", "Solid unit", "Gas unit", "Not applicable unit"),
    matrix_state_code = c("liquid", "solid", "gas", "not_applicable"),
    stringsAsFactors = FALSE
  )
}

parameter_unit_value_key <- function(value) {
  if (is.null(value) || !length(value) || all(is.na(value))) {
    return(NA_character_)
  }

  as.character(value[[1]])
}

fetch_parameter_unit_values <- function(con, parameter_id) {
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT units_liquid, units_solid, units_gas, units_na",
      "FROM public.parameters",
      "WHERE parameter_id = $1"
    ),
    params = list(as.integer(parameter_id))
  )
}

parameter_changed_assigned_unit_fields <- function(old_values, new_values) {
  fields <- parameter_unit_fields()
  if (is.null(old_values) || !nrow(old_values)) {
    return(fields[FALSE, , drop = FALSE])
  }

  changed <- vapply(
    fields$unit_column,
    function(unit_column) {
      old_key <- parameter_unit_value_key(old_values[[unit_column]])
      new_key <- parameter_unit_value_key(new_values[[unit_column]])

      !identical(old_key, new_key)
    },
    logical(1)
  )

  out <- fields[changed, , drop = FALSE]
  if (!nrow(out)) {
    return(out)
  }

  out$old_unit_id <- vapply(
    out$unit_column,
    function(column) {
      value <- parameter_unit_value_key(old_values[[column]])
      if (is.na(value)) NA_integer_ else as.integer(value)
    },
    integer(1)
  )
  out$new_unit_id <- vapply(
    out$unit_column,
    function(column) {
      value <- parameter_unit_value_key(new_values[[column]])
      if (is.na(value)) NA_integer_ else as.integer(value)
    },
    integer(1)
  )
  out
}

parameter_unit_relation_exists <- function(con, schema, table) {
  relation <- paste(schema, table, sep = ".")
  DBI::dbGetQuery(
    con,
    "SELECT to_regclass($1) IS NOT NULL AS available",
    params = list(relation)
  )$available[[1]] %in% TRUE
}

parameter_unit_count <- function(con, schema, table, where, params) {
  if (!parameter_unit_relation_exists(con, schema, table)) {
    return(0)
  }

  relation <- as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = schema, table = table)
  ))
  # Return the count as text because RPostgres represents BIGINT values using
  # bit64's integer64 encoding. Coercing that representation directly to a
  # double can silently produce a tiny, incorrect count.
  query <- sprintf("SELECT count(*)::TEXT AS n FROM %s WHERE %s", relation, where)
  as.numeric(DBI::dbGetQuery(con, query, params = params)$n[[1]])
}

parameter_unit_conversion <- function(con, from_unit_id, to_unit_id) {
  if (is.na(from_unit_id) || is.na(to_unit_id)) {
    return(NULL)
  }
  if (identical(as.integer(from_unit_id), as.integer(to_unit_id))) {
    return(data.frame(
      conversion_type = "factor",
      scale_a = 1,
      scale_b = 0,
      stringsAsFactors = FALSE
    ))
  }

  direct <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT conversion_type, scale_a, scale_b",
      "FROM public.unit_conversions",
      "WHERE from_unit_id = $1 AND to_unit_id = $2"
    ),
    params = list(as.integer(from_unit_id), as.integer(to_unit_id))
  )
  if (nrow(direct)) {
    return(direct)
  }

  reverse <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT conversion_type, scale_a, scale_b",
      "FROM public.unit_conversions",
      "WHERE from_unit_id = $1 AND to_unit_id = $2"
    ),
    params = list(as.integer(to_unit_id), as.integer(from_unit_id))
  )
  if (!nrow(reverse) || isTRUE(all.equal(as.numeric(reverse$scale_a[[1]]), 0))) {
    return(NULL)
  }

  data.frame(
    conversion_type = reverse$conversion_type[[1]],
    scale_a = 1 / as.numeric(reverse$scale_a[[1]]),
    scale_b = -as.numeric(reverse$scale_b[[1]]) / as.numeric(reverse$scale_a[[1]]),
    stringsAsFactors = FALSE
  )
}

parameter_unit_matrix_state_choices <- function(con, new_values) {
  fields <- parameter_unit_fields()
  assigned <- vapply(
    fields$unit_column,
    function(column) {
      !reference_is_missing_value(new_values[[column]])
    },
    logical(1)
  )
  available_codes <- fields$matrix_state_code[assigned]
  if (!length(available_codes)) {
    return(data.frame(
      matrix_state_id = integer(),
      matrix_state_code = character(),
      matrix_state_name = character(),
      stringsAsFactors = FALSE
    ))
  }

  placeholders <- paste0("$", seq_along(available_codes))
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT matrix_state_id, matrix_state_code,",
      "COALESCE(matrix_state_name, matrix_state_code) AS matrix_state_name",
      "FROM public.matrix_states WHERE matrix_state_code IN (",
      paste(placeholders, collapse = ", "), ") ORDER BY matrix_state_id"
    ),
    params = as.list(available_codes)
  )
}

fetch_parameter_unit_usage <- function(con, parameter_id, changed_units, new_values) {
  if (is.null(changed_units) || !nrow(changed_units)) {
    return(list(
      usage = data.frame(),
      mapping_sets = data.frame(),
      parameter_refs = data.frame(
        schema_name = character(), table_name = character(),
        column_name = character(), stringsAsFactors = FALSE
      ),
      timeseries_refs = data.frame(
        schema_name = character(), table_name = character(),
        column_name = character(), stringsAsFactors = FALSE
      ),
      state_choices = data.frame(),
      unit_choices = data.frame(),
      blockers = character(0)
    ))
  }

  usage_rows <- list()
  add_usage <- function(state_code, table, count, detail = "", supported = TRUE) {
    if (length(count) && is.finite(count) && count > 0) {
      usage_rows[[length(usage_rows) + 1L]] <<- data.frame(
        matrix_state_code = state_code,
        table = table,
        count = as.numeric(count),
        detail = detail,
        supported = isTRUE(supported),
        stringsAsFactors = FALSE
      )
    }
  }

  state_sql <- paste0(
    "SELECT matrix_state_id, matrix_state_code, ",
    "COALESCE(matrix_state_name, matrix_state_code) AS matrix_state_name ",
    "FROM public.matrix_states WHERE matrix_state_code IN (",
    paste0("$", seq_len(nrow(changed_units)), collapse = ", "), ")"
  )
  state_ids <- DBI::dbGetQuery(
    con,
    state_sql,
    params = as.list(changed_units$matrix_state_code)
  )
  state_idx <- match(changed_units$matrix_state_code, state_ids$matrix_state_code)
  changed_units$matrix_state_id <- state_ids$matrix_state_id[state_idx]
  changed_units$matrix_state_name <- state_ids$matrix_state_name[state_idx]
  blockers <- character(0)

  for (row_idx in seq_len(nrow(changed_units))) {
    row <- changed_units[row_idx, , drop = FALSE]
    state_id <- as.integer(row$matrix_state_id[[1]])
    code <- row$matrix_state_code[[1]]
    if (is.na(state_id)) {
      next
    }
    state_params <- list(as.integer(parameter_id), state_id)

    result_count <- parameter_unit_count(
      con, "discrete", "results",
      "parameter_id = $1 AND matrix_state_id = $2", state_params
    )
    add_usage(code, "discrete.results", result_count)

    component_count <- parameter_unit_count(
      con, "discrete", "result_components",
      paste(
        "result_id IN (SELECT result_id FROM discrete.results",
        "WHERE parameter_id = $1 AND matrix_state_id = $2)"
      ),
      state_params
    )
    add_usage(code, "discrete.result_components", component_count)

    aggregation_count <- parameter_unit_count(
      con, "discrete", "result_aggregations",
      "result_id IN (SELECT result_id FROM discrete.results WHERE parameter_id = $1 AND matrix_state_id = $2)",
      state_params
    )
    add_usage(
      code, "discrete.result_aggregations", aggregation_count,
      "Stored calculation arguments and rounding need an algorithm-specific review.",
      supported = aggregation_count == 0
    )
    if (aggregation_count > 0) {
      blockers <- c(blockers, "discrete.result_aggregations")
    }
    for (view in c("result_aggregation_summary", "stale_result_aggregations")) {
      view_count <- parameter_unit_count(
        con, "discrete", view,
        "result_id IN (SELECT result_id FROM discrete.results WHERE parameter_id = $1 AND matrix_state_id = $2)",
        state_params
      )
      add_usage(
        code, paste0("discrete.", view), view_count,
        "Derived view; it will reflect the migrated canonical result and components."
      )
    }

    timeseries_count <- parameter_unit_count(
      con, "continuous", "timeseries",
      "parameter_id = $1 AND matrix_state_id = $2", state_params
    )
    add_usage(code, "continuous.timeseries", timeseries_count)

    timeseries_clause <- paste(
      "timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries",
      "WHERE parameter_id = $1 AND matrix_state_id = $2)"
    )
    for (table in c(
      "measurements_continuous", "measurements_calculated_daily", "corrections",
      "extrema", "forecasts", "thresholds"
    )) {
      count <- parameter_unit_count(
        con, "continuous", table, timeseries_clause, state_params
      )
      add_usage(code, paste0("continuous.", table), count)
    }

    for (table in c("transmission_timeseries_mappings")) {
      count <- parameter_unit_count(
        con, "continuous", table, timeseries_clause, state_params
      )
      add_usage(code, paste0("continuous.", table), count)
    }

    aggregation_method_clause <- paste(
      "aggregation_method_id IN (SELECT aggregation_method_id",
      "FROM continuous.timeseries_aggregation_methods",
      "WHERE timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries",
      "WHERE parameter_id = $1 AND matrix_state_id = $2))"
    )
    for (table in c(
      "timeseries_aggregation_methods",
      "timeseries_aggregation_method_steps",
      "timeseries_aggregation_method_step_inputs"
    )) {
      where <- if (identical(table, "timeseries_aggregation_methods")) {
        timeseries_clause
      } else {
        aggregation_method_clause
      }
      count <- parameter_unit_count(
        con, "continuous", table, where, state_params
      )
      add_usage(
        code, paste0("continuous.", table), count,
        "Versioned aggregation recipes and their steps require recalculation review after a unit change.",
        supported = count == 0
      )
      if (count > 0) {
        blockers <- c(blockers, paste0("continuous.", table))
      }
    }

    guideline_count <- parameter_unit_count(
      con, "criteria", "guidelines",
      "parameter_id = $1 AND matrix_state_id = $2", state_params
    )
    add_usage(code, "criteria.guidelines", guideline_count)

    guideline_input_count <- parameter_unit_count(
      con, "criteria", "guideline_rule_inputs",
      "parameter_id = $1 AND matrix_state_id = $2", state_params
    )
    add_usage(code, "criteria.guideline_rule_inputs", guideline_input_count)

    # A parameter may feed a formula for a different guideline parameter.
    # Converting that input without rewriting the consumer changes its meaning.
    dependent_rule_count <- parameter_unit_count(
      con, "criteria", "guideline_value_rules",
      "rule_id IN (
         SELECT rule_id FROM criteria.guideline_rule_inputs
         WHERE parameter_id = $1 AND matrix_state_id = $2
       ) AND (algorithm_code NOT IN ('constant', 'narrative')
         OR NULLIF(btrim(formula_sql), '') IS NOT NULL)",
      state_params
    )
    if (dependent_rule_count > 0) {
      add_usage(
        code, "criteria.guideline_value_rules (dependent input)",
        dependent_rule_count,
        "Formulas consuming this parameter need an input-unit review before migration.",
        supported = FALSE
      )
      blockers <- c(blockers, "criteria.guideline_value_rules (dependent input)")
    }

    helper_input_count <- parameter_unit_count(
      con, "criteria", "guideline_rule_inputs",
      "parameter_id = $1 AND matrix_state_id = $2 AND input_source = 'hardness_helper'",
      state_params
    )
    if (helper_input_count > 0) {
      add_usage(
        code, "criteria.guideline_rule_inputs (hardness_helper)", helper_input_count,
        "Derived hardness inputs need a helper-specific unit migration.",
        supported = FALSE
      )
      blockers <- c(blockers, "criteria.guideline_rule_inputs (hardness_helper)")
    }

    guideline_rule_count <- parameter_unit_count(
      con, "criteria", "guideline_value_rules",
      "guideline_id IN (SELECT guideline_id FROM criteria.guidelines WHERE parameter_id = $1 AND matrix_state_id = $2)",
      state_params
    )
    add_usage(code, "criteria.guideline_value_rules", guideline_rule_count)

    guideline_coefficient_count <- parameter_unit_count(
      con, "criteria", "guideline_rule_coefficients",
      "rule_id IN (SELECT rule.rule_id FROM criteria.guideline_value_rules rule JOIN criteria.guidelines guideline USING (guideline_id) WHERE guideline.parameter_id = $1 AND guideline.matrix_state_id = $2)",
      state_params
    )
    add_usage(
      code, "criteria.guideline_rule_coefficients", guideline_coefficient_count,
      "Rule coefficients require a parameter-specific review.",
      supported = guideline_coefficient_count == 0
    )
    if (guideline_coefficient_count > 0) {
      blockers <- c(blockers, "criteria.guideline_rule_coefficients")
    }

    formula_rule_count <- parameter_unit_count(
      con, "criteria", "guideline_value_rules",
      "guideline_id IN (SELECT guideline_id FROM criteria.guidelines WHERE parameter_id = $1 AND matrix_state_id = $2) AND formula_sql IS NOT NULL AND btrim(formula_sql) <> ''",
      state_params
    )
    if (formula_rule_count > 0) {
      add_usage(
        code, "criteria.guideline_value_rules (formula_sql)", formula_rule_count,
        "Stored formulas cannot be rewritten safely by a unit conversion.",
        supported = FALSE
      )
      blockers <- c(blockers, "criteria.guideline_value_rules (formula_sql)")
    }

    computed_rule_count <- parameter_unit_count(
      con, "criteria", "guideline_value_rules",
      "guideline_id IN (SELECT guideline_id FROM criteria.guidelines WHERE parameter_id = $1 AND matrix_state_id = $2) AND algorithm_code <> 'constant'",
      state_params
    )
    if (computed_rule_count > 0) {
      add_usage(
        code, "criteria.guideline_value_rules (computed algorithm)", computed_rule_count,
        "Computed guideline algorithms require an algorithm-specific unit migration.",
        supported = FALSE
      )
      blockers <- c(blockers, "criteria.guideline_value_rules (computed algorithm)")
    }

    rounded_rule_count <- parameter_unit_count(
      con, "criteria", "guideline_value_rules",
      "guideline_id IN (SELECT guideline_id FROM criteria.guidelines
       WHERE parameter_id = $1 AND matrix_state_id = $2)
       AND rounding_method <> 'none'",
      state_params
    )
    if (rounded_rule_count > 0) {
      add_usage(
        code, "criteria.guideline_value_rules (rounding)", rounded_rule_count,
        "Output rounding precision must be reviewed in the destination unit.",
        supported = FALSE
      )
      blockers <- c(blockers, "criteria.guideline_value_rules (rounding)")
    }

    has_mapping_sets <- parameter_unit_relation_exists(con, "discrete", "import_mapping_sets")
    mapping_count <- if (has_mapping_sets) parameter_unit_count(
      con, "discrete", "import_parameter_mappings",
      paste(
        "parameter_id = $1 AND matrix_state_id = $2 AND import_mapping_set_id IN (",
        "SELECT import_mapping_set_id FROM discrete.import_mapping_sets",
        "WHERE status IN ('draft', 'published'))"
      ),
      state_params
    ) else 0
    add_usage(code, "discrete.import_parameter_mappings", mapping_count)

  }

  mapping_missing_state_count <- parameter_unit_count(
    con, "discrete", "import_parameter_mappings",
    "parameter_id = $1 AND matrix_state_id IS NULL AND import_mapping_set_id IN (SELECT import_mapping_set_id FROM discrete.import_mapping_sets WHERE status IN ('draft', 'published'))",
    list(as.integer(parameter_id))
  )
  if (mapping_missing_state_count > 0) {
    add_usage(
      NA_character_, "discrete.import_parameter_mappings (missing matrix state)",
      mapping_missing_state_count,
      "The mapping has no matrix state, so its target unit is ambiguous.",
      supported = FALSE
    )
    blockers <- c(blockers, "discrete.import_parameter_mappings (missing matrix state)")
  }

  # Curves reference a parameter independently on their input and output axes.
  curve_count <- parameter_unit_count(
    con, "continuous", "rating_curves",
    "input_parameter_id = $1 OR output_parameter_id = $1",
    list(as.integer(parameter_id))
  )
  add_usage(NA_character_, "continuous.rating_curves", curve_count)
  if (curve_count > 0) {
    old_units <- fetch_parameter_unit_values(con, parameter_id)
    fields <- parameter_unit_fields()$unit_column
    old_assigned <- vapply(
      fields,
      function(column) !reference_is_missing_value(old_units[[column]]),
      logical(1)
    )
    new_assigned <- vapply(
      fields,
      function(column) !reference_is_missing_value(new_values[[column]]),
      logical(1)
    )
    if (sum(old_assigned) != 1L || sum(new_assigned) != 1L || nrow(changed_units) != 1L) {
      add_usage(
        NA_character_, "continuous.rating_curves (ambiguous matrix state)",
        curve_count,
        "Rating curves do not store a matrix state; this parameter has multiple or changing unit states.",
        supported = FALSE
      )
      blockers <- c(blockers, "continuous.rating_curves (ambiguous matrix state)")
    }
  }

  for (table in c("rating_curve_points", "rating_curve_shifts")) {
    count <- parameter_unit_count(
      con, "continuous", table,
      "curve_id IN (SELECT curve_id FROM continuous.rating_curves WHERE input_parameter_id = $1 OR output_parameter_id = $1)",
      list(as.integer(parameter_id))
    )
    add_usage(NA_character_, paste0("continuous.", table), count)
  }

  unsupported_correction_count <- parameter_unit_count(
    con, "continuous", "corrections",
    paste(
      "timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries WHERE parameter_id = $1)",
      "AND correction_type IN (SELECT correction_type_id FROM continuous.correction_types",
      "WHERE correction_type = 'drift equation')"
    ),
    list(as.integer(parameter_id))
  )
  if (unsupported_correction_count > 0) {
    add_usage(
      NA_character_, "continuous.corrections (drift equation)",
      unsupported_correction_count,
      "Drift equations cannot be transformed safely.",
      supported = FALSE
    )
    blockers <- c(blockers, "continuous.corrections (drift equation)")
  }

  custom_transmission_config_count <- parameter_unit_count(
    con, "continuous", "transmission_timeseries_mappings",
    paste(
      "timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries WHERE parameter_id = $1)",
      "AND mapping_config <> '{}'::jsonb"
    ),
    list(as.integer(parameter_id))
  )
  if (custom_transmission_config_count > 0) {
    add_usage(
      NA_character_, "continuous.transmission_timeseries_mappings (mapping_config)",
      custom_transmission_config_count,
      "Configured preprocessing such as counter differencing or rounding needs a separate unit review.",
      supported = FALSE
    )
    blockers <- c(blockers, "continuous.transmission_timeseries_mappings (mapping_config)")
  }

  custom_adapter_count <- parameter_unit_count(
    con, "continuous", "timeseries_source_adapters",
    "timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries WHERE parameter_id = $1)",
    list(as.integer(parameter_id))
  )
  if (custom_adapter_count > 0) {
    add_usage(
      NA_character_, "continuous.timeseries_source_adapters", custom_adapter_count,
      "Source adapter outputs need an adapter-specific unit review.",
      supported = FALSE
    )
    blockers <- c(blockers, "continuous.timeseries_source_adapters")
  }

  compound_count <- parameter_unit_count(
    con, "continuous", "timeseries_compounds",
    "timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries WHERE parameter_id = $1)",
    list(as.integer(parameter_id))
  )
  if (compound_count > 0) {
    add_usage(
      NA_character_, "continuous.timeseries_compounds", compound_count,
      "Compound expressions require a formula-specific unit review.",
      supported = FALSE
    )
    blockers <- c(blockers, "continuous.timeseries_compounds")
  }

  compound_member_count <- parameter_unit_count(
    con, "continuous", "timeseries_compound_members",
    "member_timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries WHERE parameter_id = $1)",
    list(as.integer(parameter_id))
  )
  if (compound_member_count > 0) {
    add_usage(
      NA_character_, "continuous.timeseries_compound_members", compound_member_count,
      "The parameter is an input to a compound series.",
      supported = FALSE
    )
    blockers <- c(blockers, "continuous.timeseries_compound_members")
  }

  # Report parameter foreign keys not yet handled by the migration registry.
  # These are blockers until their value semantics have an explicit policy.
  parameter_refs <- DBI::dbGetQuery(
    con,
    "SELECT n.nspname AS schema_name, c.relname AS table_name,
       a.attname AS column_name
     FROM pg_constraint f
     JOIN pg_class c ON c.oid = f.conrelid
     JOIN pg_namespace n ON n.oid = c.relnamespace
     JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY(f.conkey)
     WHERE f.contype = 'f' AND f.confrelid = 'public.parameters'::regclass
     ORDER BY n.nspname, c.relname, a.attname"
  )
  safe_refs <- c(
    "public.parameter_relationships.parameter_id",
    "public.parameter_reporting_basis_suggestions.parameter_id",
    "public.parameter_speciation_suggestions.parameter_id",
    "public.parameter_target_suggestions.parameter_id",
    "discrete.results.parameter_id",
    "discrete.import_parameter_mappings.parameter_id",
    "criteria.guidelines.parameter_id",
    "criteria.guideline_rule_inputs.parameter_id",
    "continuous.timeseries.parameter_id",
    "continuous.rating_curves.input_parameter_id",
    "continuous.rating_curves.output_parameter_id"
  )
  for (ref_idx in seq_len(nrow(parameter_refs))) {
    ref <- parameter_refs[ref_idx, , drop = FALSE]
    ref_key <- paste(ref$schema_name, ref$table_name, ref$column_name, sep = ".")
    if (ref_key %in% safe_refs) {
      next
    }
    count <- parameter_unit_count(
      con,
      ref$schema_name[[1]],
      ref$table_name[[1]],
      sprintf("%s = $1", as.character(DBI::dbQuoteIdentifier(con, ref$column_name[[1]]))),
      list(as.integer(parameter_id))
    )
    if (count > 0) {
      add_usage(NA_character_, ref_key, count, "Requires a reviewed migration policy.", supported = FALSE)
      blockers <- c(blockers, ref_key)
    }
  }

  timeseries_refs <- DBI::dbGetQuery(
    con,
    "SELECT n.nspname AS schema_name, c.relname AS table_name,
       a.attname AS column_name
     FROM pg_constraint f
     JOIN pg_class c ON c.oid = f.conrelid
     JOIN pg_namespace n ON n.oid = c.relnamespace
     JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY(f.conkey)
     WHERE f.contype = 'f'
       AND f.confrelid = 'continuous.timeseries'::regclass
     ORDER BY n.nspname, c.relname, a.attname"
  )
  safe_timeseries_refs <- c(
    "continuous.timeseries.timeseries_id",
    # These segments describe provenance, access, or review status. Preserve
    # their values and time ranges; they do not carry measurement units.
    "continuous.approvals.timeseries_id",
    "continuous.contributors.timeseries_id",
    "continuous.grades.timeseries_id",
    "continuous.owners.timeseries_id",
    "continuous.qualifiers.timeseries_id",
    "continuous.notes.timeseries_id",
    "continuous.timeseries_data_sharing_agreements.timeseries_id",
    "continuous.measurements_continuous.timeseries_id",
    "continuous.measurements_calculated_daily.timeseries_id",
    "continuous.corrections.timeseries_id",
    "continuous.extrema.timeseries_id",
    "continuous.forecasts.timeseries_id",
    "continuous.thresholds.timeseries_id",
    "continuous.transmission_timeseries_mappings.timeseries_id",
    "continuous.timeseries_aggregation_methods.timeseries_id",
    "continuous.timeseries_compounds.timeseries_id",
    "continuous.timeseries_compound_members.timeseries_id",
    "continuous.timeseries_compound_members.member_timeseries_id",
    "continuous.timeseries_source_adapters.timeseries_id"
  )
  for (ref_idx in seq_len(nrow(timeseries_refs))) {
    ref <- timeseries_refs[ref_idx, , drop = FALSE]
    ref_key <- paste(ref$schema_name, ref$table_name, ref$column_name, sep = ".")
    if (ref_key %in% safe_timeseries_refs) {
      next
    }
    for (state_idx in seq_len(nrow(changed_units))) {
      code <- changed_units$matrix_state_code[[state_idx]]
      state_id <- as.integer(changed_units$matrix_state_id[[state_idx]])
      count <- parameter_unit_count(
        con,
        ref$schema_name[[1]],
        ref$table_name[[1]],
        paste(
          as.character(DBI::dbQuoteIdentifier(con, ref$column_name[[1]])),
          "IN (SELECT timeseries_id FROM continuous.timeseries",
          "WHERE parameter_id = $1 AND matrix_state_id = $2)"
        ),
        list(as.integer(parameter_id), state_id)
      )
      if (count > 0) {
        add_usage(
          code, ref_key, count,
          "An unhandled timeseries foreign key requires a reviewed migration policy.",
          supported = FALSE
        )
        blockers <- c(blockers, ref_key)
      }
    }
  }

  mapping_sets <- if (parameter_unit_relation_exists(con, "discrete", "import_parameter_mappings")) {
    DBI::dbGetQuery(
      con,
      "SELECT DISTINCT mapping_set.import_mapping_set_id,
         source.source_code, mapping_set.version, mapping_set.status,
         mapping_set.import_source_id, mapping_set.import_profile_id,
         count(*) OVER (PARTITION BY mapping_set.import_mapping_set_id)::INTEGER AS mapping_count,
         draft.import_mapping_set_id AS open_draft_id,
         draft.version AS open_draft_version
       FROM discrete.import_parameter_mappings mapping
       JOIN discrete.import_mapping_sets mapping_set USING (import_mapping_set_id)
       JOIN discrete.import_sources source USING (import_source_id)
       LEFT JOIN LATERAL (
         SELECT draft_set.import_mapping_set_id, draft_set.version
         FROM discrete.import_mapping_sets draft_set
         WHERE draft_set.import_source_id = mapping_set.import_source_id
           AND draft_set.import_profile_id IS NOT DISTINCT FROM mapping_set.import_profile_id
           AND draft_set.status = 'draft'
         ORDER BY draft_set.version DESC
         LIMIT 1
       ) draft ON TRUE
       WHERE mapping.parameter_id = $1 AND mapping_set.status IN ('draft', 'published')
       ORDER BY mapping_set.status, source.source_code, mapping_set.version",
      params = list(as.integer(parameter_id))
    )
  } else {
    data.frame()
  }

  flag_count <- if (nrow(mapping_sets) && parameter_unit_relation_exists(con, "discrete", "import_result_flag_mappings")) {
    set_ids <- unique(as.integer(mapping_sets$import_mapping_set_id))
    placeholders <- paste0("$", seq_along(set_ids))
    as.numeric(DBI::dbGetQuery(
      con,
      paste(
        "SELECT count(*)::TEXT AS n FROM discrete.import_result_flag_mappings",
        "WHERE import_mapping_set_id IN (", paste(placeholders, collapse = ", "), ")",
        "AND result_condition_value_literal IS NOT NULL"
      ),
      params = as.list(set_ids)
    )$n[[1]])
  } else {
    0
  }
  if (flag_count > 0) {
    add_usage(
      NA_character_, "discrete.import_result_flag_mappings", flag_count,
      "These set-level numeric conditions may apply to multiple parameters.",
      supported = FALSE
    )
    blockers <- c(blockers, "discrete.import_result_flag_mappings")
  }

  usage <- if (length(usage_rows)) {
    do.call(rbind, usage_rows)
  } else {
    data.frame(
      matrix_state_code = character(), table = character(), count = numeric(),
      detail = character(), supported = logical(), stringsAsFactors = FALSE
    )
  }

  list(
    usage = usage,
    mapping_sets = mapping_sets,
    parameter_refs = parameter_refs,
    timeseries_refs = timeseries_refs,
    state_choices = parameter_unit_matrix_state_choices(con, new_values),
    unit_choices = DBI::dbGetQuery(
      con,
      "SELECT unit_id, unit_name FROM public.units ORDER BY unit_name"
    ),
    blockers = unique(blockers),
    changed_units = changed_units
  )
}

parameter_unit_usage_messages <- function(usage) {
  if (is.null(usage) || !nrow(usage$usage)) {
    return(character(0))
  }

  usage <- usage$usage
  apply(usage, 1L, function(row) {
    state <- if (is.na(row[["matrix_state_code"]])) "parameter-linked" else row[["matrix_state_code"]]
    detail <- if (nzchar(row[["detail"]])) paste0(" — ", row[["detail"]]) else ""
    paste0(
      row[["table"]], ": ", format(as.numeric(row[["count"]]), big.mark = ","),
      " row(s) (", state, ")", detail
    )
  })
}

parameter_unit_change_defaults <- function(con, changed_units, new_values, state_choices, usage = NULL) {
  fields <- parameter_unit_fields()
  out <- changed_units
  if (!is.null(usage)) {
    # A newly assigned destination with no existing records needs no source
    # migration. Keep unassigned sources only when they actually contain data.
    unscoped <- anyNA(usage$usage$matrix_state_code)
    needed <- vapply(seq_len(nrow(out)), function(i) {
      parameter_unit_data_has_state_rows(usage, out$matrix_state_code[[i]]) ||
        (unscoped && !is.na(out$old_unit_id[[i]]))
    }, logical(1))
    out <- out[needed, , drop = FALSE]
  }
  out$destination_code <- rep(NA_character_, nrow(out))
  out$destination_id <- rep(NA_integer_, nrow(out))
  out$destination_unit_id <- rep(NA_integer_, nrow(out))
  # Unknown conversions need an explicit choice to keep numbers or supply a
  # custom conversion; never inherit a factor from a different unit pair.
  out$conversion_a <- rep(NA_real_, nrow(out))
  out$conversion_b <- rep(NA_real_, nrow(out))
  out$conversion_known <- rep(FALSE, nrow(out))
  out$transmission_a <- rep(NA_real_, nrow(out))
  out$transmission_b <- rep(NA_real_, nrow(out))

  for (row_idx in seq_len(nrow(out))) {
    source_code <- out$matrix_state_code[[row_idx]]
    if (nrow(state_choices)) {
      destination_code <- if (source_code %in% state_choices$matrix_state_code) {
        source_code
      } else {
        state_choices$matrix_state_code[[1]]
      }
      destination_idx <- match(destination_code, state_choices$matrix_state_code)
      destination_field <- fields$unit_column[
        match(destination_code, fields$matrix_state_code)
      ]
      destination_unit <- parameter_unit_value_key(new_values[[destination_field]])
      out$destination_code[[row_idx]] <- destination_code
      out$destination_id[[row_idx]] <- as.integer(
        state_choices$matrix_state_id[[destination_idx]]
      )
      out$destination_unit_id[[row_idx]] <- if (is.na(destination_unit)) {
        NA_integer_
      } else {
        as.integer(destination_unit)
      }

      conversion <- parameter_unit_conversion(
        con,
        out$old_unit_id[[row_idx]],
        out$destination_unit_id[[row_idx]]
      )
      if (!is.null(conversion) && nrow(conversion)) {
        out$conversion_a[[row_idx]] <- as.numeric(conversion$scale_a[[1]])
        out$conversion_b[[row_idx]] <- as.numeric(conversion$scale_b[[1]])
        out$conversion_known[[row_idx]] <- TRUE
      }
      out$transmission_a[[row_idx]] <- out$conversion_a[[row_idx]]
      out$transmission_b[[row_idx]] <- out$conversion_b[[row_idx]]
    }
  }
  out
}

parameter_unit_data_has_state_rows <- function(usage, state_code) {
  rows <- usage$usage
  nrow(rows) > 0L && any(!is.na(rows$matrix_state_code) & rows$matrix_state_code == state_code)
}

parameter_unit_affine_update <- function(
  con,
  schema,
  table,
  value_columns,
  where,
  where_params,
  scale_a,
  scale_b,
  extra_assignments = list()
) {
  if (
    identical(as.numeric(scale_a), 1) && identical(as.numeric(scale_b), 0) &&
      !length(extra_assignments)
  ) {
    return(invisible(0L))
  }

  relation <- as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = schema, table = table)
  ))
  fields <- character(0)
  if (length(value_columns)) {
    value_columns <- intersect(value_columns, DBI::dbListFields(
      con,
      DBI::Id(schema = schema, table = table)
    ))
    scale_idx <- length(where_params) + 1L
    offset_idx <- scale_idx + 1L
    fields <- vapply(value_columns, function(column) {
      quoted <- as.character(DBI::dbQuoteIdentifier(con, column))
      paste0(
        quoted, " = CASE WHEN ", quoted, " IS NULL THEN NULL ELSE $",
        scale_idx, " * ", quoted, " + $", offset_idx, " END"
      )
    }, character(1))
  } else {
    scale_idx <- length(where_params) + 1L
    offset_idx <- scale_idx + 1L
  }

  extra_params <- list()
  if (length(extra_assignments)) {
    for (column in names(extra_assignments)) {
      extra_params[[length(extra_params) + 1L]] <- extra_assignments[[column]]
      fields <- c(
        fields,
        paste0(
          as.character(DBI::dbQuoteIdentifier(con, column)),
          " = $", length(where_params) + 2L + length(extra_params)
        )
      )
    }
  }
  if (!length(fields)) {
    return(invisible(0L))
  }

  query <- sprintf(
    "UPDATE %s SET %s WHERE %s",
    relation,
    paste(fields, collapse = ", "),
    where
  )
  DBI::dbExecute(
    con,
    query,
    params = c(where_params, list(as.numeric(scale_a), as.numeric(scale_b)), extra_params)
  )
}

parameter_unit_update_matrix_states <- function(
  con,
  schema,
  table,
  parameter_id,
  plan,
  extra_where = NULL,
  extra_params = list()
) {
  source_ids <- unique(as.integer(plan$matrix_state_id))
  if (!length(source_ids) || anyNA(source_ids)) {
    return(invisible(0L))
  }
  if (all(plan$matrix_state_id == plan$destination_id)) {
    return(invisible(0L))
  }
  relation <- as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = schema, table = table)
  ))
  pair_params <- list()
  case_parts <- character(0)
  source_placeholders <- character(0)
  for (source_id in source_ids) {
    destination_id <- as.integer(
      plan$destination_id[match(source_id, plan$matrix_state_id)]
    )
    pair_params <- c(pair_params, list(source_id, destination_id))
    source_placeholder <- length(pair_params)
    destination_placeholder <- source_placeholder + 1L
    case_parts <- c(
      case_parts,
      sprintf(
        "WHEN $%d::INTEGER THEN $%d::INTEGER",
        source_placeholder, destination_placeholder
      )
    )
    source_placeholders <- c(
      source_placeholders,
      paste0("$", source_placeholder, "::INTEGER")
    )
  }
  params <- c(list(as.integer(parameter_id)), pair_params, extra_params)
  where <- paste0(
    "parameter_id = $1 AND matrix_state_id IN (",
    paste(source_placeholders, collapse = ", "), ")"
  )
  if (!is.null(extra_where)) {
    where <- paste("(", where, ") AND (", extra_where, ")")
  }
  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE %s SET matrix_state_id = CASE matrix_state_id %s END WHERE %s",
      relation,
      paste(case_parts, collapse = " "),
      where
    ),
    params = params
  )
}

parameter_unit_json_object <- function(values, fields) {
  out <- lapply(fields, function(field) {
    value <- values[[field]]
    if (reference_is_missing_value(value)) NA_integer_ else as.integer(value[[1]])
  })
  names(out) <- fields
  out
}

parameter_unit_inventory_signature <- function(usage) {
  jsonlite::toJSON(
    list(
      usage = usage$usage[order(usage$usage$table, usage$usage$matrix_state_code), , drop = FALSE],
      mapping_sets = usage$mapping_sets[order(
        usage$mapping_sets$import_source_id,
        usage$mapping_sets$import_profile_id,
        usage$mapping_sets$version
      ), , drop = FALSE],
      parameter_refs = usage$parameter_refs[order(
        usage$parameter_refs$schema_name,
        usage$parameter_refs$table_name,
        usage$parameter_refs$column_name
      ), , drop = FALSE],
      timeseries_refs = usage$timeseries_refs[order(
        usage$timeseries_refs$schema_name,
        usage$timeseries_refs$table_name,
        usage$timeseries_refs$column_name
      ), , drop = FALSE],
      blockers = sort(usage$blockers)
    ),
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null"
  )
}

parameter_unit_apply_migration <- function(con, parameter_id, plan, mapping_sets) {
  scale_columns <- c("discrete_a", "continuous_a", "transmission_a", "mapping_a")
  offset_columns <- c("discrete_b", "continuous_b", "transmission_b", "mapping_b")
  if (
    any(!is.finite(as.matrix(plan[, scale_columns, drop = FALSE]))) ||
      any(as.matrix(plan[, scale_columns, drop = FALSE]) <= 0) ||
      any(!is.finite(as.matrix(plan[, offset_columns, drop = FALSE])))
  ) {
    stop("Unit-change scale factors must be finite and positive, and offsets must be finite.")
  }

  previous_daily_refresh <- DBI::dbGetQuery(
    con,
    "SELECT COALESCE(current_setting('continuous.skip_daily_refresh', TRUE), '') AS setting"
  )$setting[[1]]
  invisible(DBI::dbGetQuery(
    con,
    "SELECT set_config('continuous.skip_daily_refresh', 'on', TRUE)"
  ))
  restore_daily_refresh <- function() {
    invisible(DBI::dbGetQuery(
      con,
      "SELECT set_config('continuous.skip_daily_refresh', $1, TRUE)",
      params = list(previous_daily_refresh)
    ))
  }
  # A SQL error leaves the transaction aborted; let the caller roll it back
  # without masking the original error with a failed setting restoration.
  on.exit(try(restore_daily_refresh(), silent = TRUE), add = TRUE)

  for (row_idx in seq_len(nrow(plan))) {
    row <- plan[row_idx, , drop = FALSE]
    source_id <- as.integer(row$matrix_state_id[[1]])
    destination_id <- as.integer(row$destination_id[[1]])
    state_params <- list(as.integer(parameter_id), source_id)
    ts_where <- paste(
      "timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries",
      "WHERE parameter_id = $1 AND matrix_state_id = $2)"
    )
    result_where <- "parameter_id = $1 AND matrix_state_id = $2"

    parameter_unit_affine_update(
      con, "discrete", "results",
      c("result", "result_condition_value"), result_where, state_params,
      row$discrete_a[[1]], row$discrete_b[[1]]
    )
    parameter_unit_affine_update(
      con, "discrete", "result_components",
      c("result", "result_condition_value"),
      "result_id IN (SELECT result_id FROM discrete.results WHERE parameter_id = $1 AND matrix_state_id = $2)",
      state_params, row$discrete_a[[1]], row$discrete_b[[1]]
    )

    parameter_unit_affine_update(
      con, "continuous", "measurements_continuous", "value", ts_where,
      state_params, row$continuous_a[[1]], row$continuous_b[[1]]
    )
    parameter_unit_affine_update(
      con, "continuous", "measurements_calculated_daily",
      c(
        "value", "max", "min", "q90", "q75", "q50", "q25", "q10", "mean",
        "max_30yr", "min_30yr", "q90_30yr", "q75_30yr", "q50_30yr",
        "q25_30yr", "q10_30yr", "mean_30yr"
      ),
      ts_where, state_params, row$continuous_a[[1]], row$continuous_b[[1]]
    )
    parameter_unit_affine_update(
      con, "continuous", "extrema", "value", ts_where,
      state_params, row$continuous_a[[1]], row$continuous_b[[1]]
    )
    parameter_unit_affine_update(
      con, "continuous", "extrema",
      c("uncertainty_lower", "uncertainty_upper"), ts_where, state_params,
      abs(as.numeric(row$continuous_a[[1]])), 0
    )
    parameter_unit_affine_update(
      con, "continuous", "forecasts",
      c("value", "min", "q10", "q25", "q50", "q75", "q90", "max"),
      ts_where, state_params, row$continuous_a[[1]], row$continuous_b[[1]]
    )
    parameter_unit_affine_update(
      con, "continuous", "thresholds",
      c(
        "high_advisory", "high_watch", "high_warning", "flood_minor", "flood_major",
        "high_first_human_impacts", "low_advisory", "low_watch", "low_warning",
        "low_first_human_impacts", "low_aquatic_life_impacts_minor",
        "low_aquatic_life_impacts_major", "high_aquatic_life_impacts_minor",
        "high_aquatic_life_impacts_major", "fsl", "lsl"
      ),
      ts_where, state_params, row$continuous_a[[1]], row$continuous_b[[1]]
    )

    correction_types <- DBI::dbGetQuery(
      con,
      "SELECT correction_type_id, correction_type
       FROM continuous.correction_types"
    )
    type_ids <- setNames(correction_types$correction_type_id, correction_types$correction_type)
    correction_where <- paste(
      "timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries",
      "WHERE parameter_id = $1 AND matrix_state_id = $2)",
      "AND correction_type = ANY($3)"
    )
    if (row$continuous_a[[1]] != 1 || row$continuous_b[[1]] != 0) {
      if (row$continuous_a[[1]] <= 0) {
        stop("Continuous conversions with a non-positive scale cannot preserve correction trim bounds.")
      }
      if (row$continuous_b[[1]] != 0 && "scale" %in% names(type_ids)) {
        scale_count <- parameter_unit_count(
          con, "continuous", "corrections",
          paste(
            ts_where,
            "AND correction_type = $3"
          ),
          c(state_params, list(as.integer(type_ids[["scale"]])))
        )
        if (scale_count > 0) {
          stop("Affine offsets cannot be applied safely to existing scale corrections.")
        }
      }
      if ("trim" %in% names(type_ids)) {
        DBI::dbExecute(
          con,
          "UPDATE continuous.corrections
           SET value1 = $4 * value1 + $5,
               value2 = $4 * value2 + $5
           WHERE timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries
             WHERE parameter_id = $1 AND matrix_state_id = $2)
             AND correction_type = $3",
          params = list(
            as.integer(parameter_id), source_id, as.integer(type_ids[["trim"]]),
            as.numeric(row$continuous_a[[1]]), as.numeric(row$continuous_b[[1]])
          )
        )
      }
      for (correction_name in c("offset linear", "offset two-point", "drift linear")) {
        if (!(correction_name %in% names(type_ids))) {
          next
        }
        DBI::dbExecute(
          con,
          paste(
            "UPDATE continuous.corrections",
            if (identical(correction_name, "offset two-point")) {
              "SET value1 = $4 * value1, value2 = $4 * value2"
            } else {
              "SET value1 = $4 * value1"
            },
            "WHERE timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries",
            "WHERE parameter_id = $1 AND matrix_state_id = $2)",
            "AND correction_type = $3"
          ),
          params = list(
            as.integer(parameter_id), source_id,
            as.integer(type_ids[[correction_name]]),
            as.numeric(row$continuous_a[[1]])
          )
        )
      }
    }

    # Timeseries delivery mappings feed future values into the stored unit.
    transmission_where <- paste(
      "timeseries_id IN (SELECT timeseries_id FROM continuous.timeseries",
      "WHERE parameter_id = $1 AND matrix_state_id = $2)"
    )
    if (row$transmission_a[[1]] != 1 || row$transmission_b[[1]] != 0) {
      DBI::dbExecute(
        con,
        paste(
          "UPDATE continuous.transmission_timeseries_mappings",
          "SET value_multiplier = COALESCE(value_multiplier, 1) * $3,",
          "    value_offset = COALESCE(value_offset, 0) * $3 + $4",
          "WHERE", transmission_where
        ),
        params = c(state_params, list(
          as.numeric(row$transmission_a[[1]]),
          as.numeric(row$transmission_b[[1]])
        ))
      )
    }

    guideline_where <- "parameter_id = $1 AND matrix_state_id = $2"
    parameter_unit_affine_update(
      con, "criteria", "guideline_value_rules",
      c("fixed_value", "min_output_value", "max_output_value"),
      "guideline_id IN (SELECT guideline_id FROM criteria.guidelines WHERE parameter_id = $1 AND matrix_state_id = $2)",
      state_params, row$discrete_a[[1]], row$discrete_b[[1]]
    )
    parameter_unit_affine_update(
      con, "criteria", "guideline_rule_inputs",
      c("lower_calibrated_bound", "upper_calibrated_bound"),
      "parameter_id = $1 AND matrix_state_id = $2",
      state_params, row$discrete_a[[1]], row$discrete_b[[1]]
    )

    # Rating curves store both axes as numbers. Curve offsets and shifts are
    # expressed in output units and therefore scale with the output axis.
    if (row$continuous_a[[1]] != 1 || row$continuous_b[[1]] != 0) {
      if (DBI::dbGetQuery(
        con,
        "SELECT EXISTS (SELECT 1 FROM continuous.rating_curves
         WHERE output_parameter_id = $1) AS used_output",
        params = list(as.integer(parameter_id))
      )$used_output[[1]]) {
        DBI::dbExecute(
          con,
          "UPDATE continuous.rating_curves
           SET offset_value = $2 * offset_value + $3
           WHERE output_parameter_id = $1",
          params = list(
            as.integer(parameter_id), as.numeric(row$continuous_a[[1]]),
            as.numeric(row$continuous_b[[1]])
          )
        )
        DBI::dbExecute(
          con,
          "UPDATE continuous.rating_curve_shifts
           SET shift_value = shift_value * $2
           WHERE curve_id IN (SELECT curve_id FROM continuous.rating_curves
             WHERE output_parameter_id = $1)",
          params = list(as.integer(parameter_id), as.numeric(row$continuous_a[[1]]))
        )
      }
      DBI::dbExecute(
        con,
        "UPDATE continuous.rating_curve_points point
         SET input_value = CASE WHEN curve.input_parameter_id = $1
               THEN $2 * point.input_value + $3 ELSE point.input_value END,
             output_value = CASE WHEN curve.output_parameter_id = $1
               THEN $2 * point.output_value + $3 ELSE point.output_value END
         FROM continuous.rating_curves curve
         WHERE curve.curve_id = point.curve_id
           AND (curve.input_parameter_id = $1 OR curve.output_parameter_id = $1)",
        params = list(
          as.integer(parameter_id), as.numeric(row$continuous_a[[1]]),
          as.numeric(row$continuous_b[[1]])
        )
      )
    }

  }

  restore_daily_refresh()
  on.exit(NULL, add = FALSE)

  parameter_unit_update_matrix_states(
    con, "discrete", "results", parameter_id, plan
  )
  parameter_unit_update_matrix_states(
    con, "continuous", "timeseries", parameter_id, plan
  )
  parameter_unit_update_matrix_states(
    con, "criteria", "guidelines", parameter_id, plan
  )
  parameter_unit_update_matrix_states(
    con, "criteria", "guideline_rule_inputs", parameter_id, plan
  )

  # Draft mappings can be adjusted directly. Published snapshots still need a
  # revision; unpublished edits must not be included in that publication.
  if (nrow(mapping_sets)) {
    if (!exists("import_mapping_prepare_revision", envir = asNamespace("AquaCache"), inherits = FALSE)) {
      stop("This AquaCache installation cannot create import mapping revisions.")
    }
    prepare_revision <- getFromNamespace("import_mapping_prepare_revision", "AquaCache")
    publish_revision <- getFromNamespace("import_mapping_publish_revision", "AquaCache")
    migrate_mapping_set <- function(draft_id) {
      for (row_idx in seq_len(nrow(plan))) {
        row <- plan[row_idx, , drop = FALSE]
        if (row$mapping_a[[1]] != 1 || row$mapping_b[[1]] != 0) {
          DBI::dbExecute(
            con,
            "UPDATE discrete.import_parameter_mappings
             SET conversion = COALESCE(conversion, 1) * $3,
                 result_offset = COALESCE(result_offset, 0) * $3 + $4
             WHERE import_mapping_set_id = $1
               AND parameter_id = $2
               AND matrix_state_id = $5",
            params = list(
              draft_id, as.integer(parameter_id),
              as.numeric(row$mapping_a[[1]]), as.numeric(row$mapping_b[[1]]),
              as.integer(row$matrix_state_id[[1]])
            )
          )
        }
      }
      mapping_sources <- unique(as.integer(plan$matrix_state_id))
      mapping_cases <- character(0)
      mapping_params <- list(draft_id, as.integer(parameter_id))
      mapping_source_placeholders <- character(0)
      for (source_id in mapping_sources) {
        destination_id <- as.integer(
          plan$destination_id[match(source_id, plan$matrix_state_id)]
        )
        mapping_params <- c(mapping_params, list(source_id, destination_id))
        source_placeholder <- length(mapping_params) - 1L
        destination_placeholder <- length(mapping_params)
        mapping_cases <- c(
          mapping_cases,
          sprintf(
            "WHEN $%d::INTEGER THEN $%d::INTEGER",
            source_placeholder, destination_placeholder
          )
        )
        mapping_source_placeholders <- c(
          mapping_source_placeholders,
          paste0("$", source_placeholder, "::INTEGER")
        )
      }
      if (!all(plan$matrix_state_id == plan$destination_id)) {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE discrete.import_parameter_mappings SET matrix_state_id = CASE matrix_state_id ",
            paste(mapping_cases, collapse = " "),
            " END WHERE import_mapping_set_id = $1 AND parameter_id = $2 AND matrix_state_id IN (",
            paste(mapping_source_placeholders, collapse = ", "), ")"
          ),
          params = mapping_params
        )
      }
    }
    draft_sets <- mapping_sets[mapping_sets$status == "draft", , drop = FALSE]
    for (draft_id in unique(draft_sets$import_mapping_set_id)) {
      migrate_mapping_set(as.integer(draft_id))
    }

    mapping_tables <- c(
      import_parameter_mappings = "import_mapping_id",
      import_result_flag_mappings = "import_result_flag_mapping_id",
      import_location_mappings = "import_location_mapping_id"
    )
    # Used both when cloning published evidence and when restoring unfinished
    # work. Let identities and generated analytical keys be rebuilt normally.
    copy_mapping_rows <- function(table, from_relation, from_id, to_id) {
      columns <- DBI::dbGetQuery(con,
        "SELECT column_name FROM information_schema.columns
         WHERE table_schema = 'discrete' AND table_name = $1
           AND is_generated = 'NEVER' AND is_identity = 'NO'
         ORDER BY ordinal_position", params = list(table))$column_name
      columns <- setdiff(columns, c("import_mapping_set_id", mapping_tables[[table]]))
      columns_sql <- paste(as.character(DBI::dbQuoteIdentifier(con, columns)), collapse = ", ")
      target <- as.character(DBI::dbQuoteIdentifier(con, DBI::Id(schema = "discrete", table = table)))
      DBI::dbExecute(con, sprintf(
        "INSERT INTO %s (import_mapping_set_id, %s)
         SELECT $1, %s FROM %s WHERE import_mapping_set_id = $2",
        target, columns_sql, columns_sql, from_relation), params = list(to_id, from_id))
    }
    published_sets <- mapping_sets[mapping_sets$status == "published", , drop = FALSE]
    for (set_idx in seq_len(nrow(published_sets))) {
      set <- published_sets[set_idx, , drop = FALSE]
      source_id <- as.integer(set$import_source_id[[1]])
      profile_id <- as.integer(set$import_profile_id[[1]])
      draft_id <- prepare_revision(con, source_id, profile_id)
      preserve_draft <- !is.na(set$open_draft_id[[1]])
      if (preserve_draft) {
        draft_note <- DBI::dbGetQuery(con,
          "SELECT note FROM discrete.import_mapping_sets WHERE import_mapping_set_id = $1",
          params = list(draft_id))$note[[1]]
        # Only one draft is allowed per scope. Save its already-adjusted rows,
        # publish a clean revision of the published snapshot, then carry the
        # unfinished work forward into a new draft in this same transaction.
        for (i in seq_along(mapping_tables)) {
          table <- names(mapping_tables)[[i]]
          relation <- as.character(DBI::dbQuoteIdentifier(con, DBI::Id(schema = "discrete", table = table)))
          temp <- as.character(DBI::dbQuoteIdentifier(con, paste0("parameter_unit_draft_", i)))
          DBI::dbExecute(con, sprintf(
            "CREATE TEMP TABLE %s ON COMMIT DROP AS SELECT * FROM %s WHERE import_mapping_set_id = $1",
            temp, relation), params = list(draft_id))
          DBI::dbExecute(con, paste("DELETE FROM", relation, "WHERE import_mapping_set_id = $1"),
            params = list(draft_id))
          copy_mapping_rows(table, relation, as.integer(set$import_mapping_set_id[[1]]), draft_id)
        }
      }
      migrate_mapping_set(draft_id)
      publish_revision(con, draft_id)
      if (preserve_draft) {
        next_draft <- DBI::dbGetQuery(con,
          "INSERT INTO discrete.import_mapping_sets
             (import_source_id, import_profile_id, version, status, note)
           SELECT $1, $2, COALESCE(max(version), 0) + 1, 'draft', $3
           FROM discrete.import_mapping_sets
           WHERE import_source_id = $1 AND import_profile_id IS NOT DISTINCT FROM $2
           RETURNING import_mapping_set_id", params = list(source_id, profile_id, draft_note))$import_mapping_set_id[[1]]
        for (i in seq_along(mapping_tables)) {
          temp <- as.character(DBI::dbQuoteIdentifier(con, paste0("parameter_unit_draft_", i)))
          copy_mapping_rows(names(mapping_tables)[[i]], temp, draft_id, as.integer(next_draft))
          DBI::dbExecute(con, paste("DROP TABLE", temp))
        }
      }
    }
  }
  invisible(TRUE)
}

# Advisory associations are edited with their parameter, within the same
# transaction as its metadata and group links. Selection order is display order.
parameter_suggestion_tables <- function() c(
  reporting_bases = "parameter_reporting_basis_suggestions",
  result_speciations = "parameter_speciation_suggestions",
  result_targets = "parameter_target_suggestions"
)

update_parameter_suggestions <- function(session, con, parameter_id = NULL) {
  configs <- reference_table_configs()
  tables <- parameter_suggestion_tables()
  for (catalogue in names(tables)) {
    config <- configs[[catalogue]]
    rows <- DBI::dbGetQuery(con, sprintf(
      "SELECT c.%1$s AS id, c.%2$s AS label, c.active, s.display_order
       FROM discrete.%3$s c LEFT JOIN public.%4$s s
         ON s.%1$s = c.%1$s AND s.parameter_id = $1
       ORDER BY s.display_order NULLS LAST, c.%2$s",
      config$pk, config$order_by, catalogue, tables[[catalogue]]),
      params = list(if (is.null(parameter_id)) NA_integer_ else as.integer(parameter_id)))
    labels <- paste0(rows$label, ifelse(rows$active, "", " (inactive)"))
    updateSelectizeInput(session, paste0("suggest_", catalogue),
      choices = stats::setNames(as.character(rows$id), labels),
      selected = as.character(rows$id[!is.na(rows$display_order)]))
  }
}

save_parameter_suggestions <- function(con, parameter_id, input) {
  configs <- reference_table_configs()
  tables <- parameter_suggestion_tables()
  for (catalogue in names(tables)) {
    ids <- input[[paste0("suggest_", catalogue)]]
    if (is.null(ids)) ids <- character()
    numbers <- suppressWarnings(as.numeric(ids))
    if (any(!is.finite(numbers) | numbers < 1 | numbers != trunc(numbers) |
            numbers > .Machine$integer.max)) stop("Choose valid suggested catalogue entries.")
    ids <- unique(as.integer(numbers))
    DBI::dbExecute(con, sprintf("DELETE FROM public.%s WHERE parameter_id = $1", tables[[catalogue]]),
      params = list(as.integer(parameter_id)))
    if (length(ids)) DBI::dbExecute(con, sprintf(
      "INSERT INTO public.%s (parameter_id, %s, display_order)
       SELECT $1, value::INTEGER, ordinality::INTEGER
       FROM jsonb_array_elements_text($2::JSONB) WITH ORDINALITY AS selected(value, ordinality)",
      tables[[catalogue]], configs[[catalogue]]$pk),
      params = list(as.integer(parameter_id), jsonlite::toJSON(ids, auto_unbox = FALSE)))
  }
}

parameter_save_button <- function(ns, id, label = "Save", busy_label = "Checking affected records...") {
  actionButton(ns(id), label, class = "btn-primary", onclick = sprintf(
    "if (this.dataset.parameterBusy === 'true') { event.preventDefault(); event.stopImmediatePropagation(); return false; }
     window.aqualinkParameterSaveState(%s, true, %s, %s);",
    jsonlite::toJSON(ns(c("save", "save_bottom", "confirm_unit_change"))),
    jsonlite::toJSON(busy_label, auto_unbox = TRUE),
    jsonlite::toJSON(ns("save_status"), auto_unbox = TRUE)))
}

parameterManagerUI <- function(id) {
  ns <- NS(id)
  config <- reference_table_configs()[["parameters"]]

  bslib::layout_sidebar(
    YGwater:::shiny_ui_ready(ns("ui_ready")),
    sidebar = bslib::sidebar(
      width = 430,
      open = "always",
      title = config$title,
      # A synchronous database check cannot flush reactive UI updates while it
      # is running. Acknowledge and disable submissions in the browser itself.
      tags$script(HTML("window.aqualinkParameterSaveState = function(ids, busy, label, statusId) {
        ids.forEach(function(id) {
          var button = document.getElementById(id);
          if (!button) return;
          if (!button.dataset.parameterIdleLabel) button.dataset.parameterIdleLabel = button.textContent.trim();
          button.disabled = busy;
          button.dataset.parameterBusy = busy ? 'true' : 'false';
          button.setAttribute('aria-busy', busy ? 'true' : 'false');
          button.textContent = busy ? label : button.dataset.parameterIdleLabel;
          if (busy) {
            var spinner = document.createElement('span');
            spinner.className = 'spinner-border spinner-border-sm me-2';
            spinner.setAttribute('aria-hidden', 'true');
            button.insertBefore(spinner, button.firstChild);
          }
        });
        var status = document.getElementById(statusId);
        if (status) { status.textContent = busy ? label : ''; status.hidden = !busy; }
      };
      Shiny.addCustomMessageHandler('aqualink-parameter-save-state', function(message) {
        window.aqualinkParameterSaveState(message.ids, message.busy, message.label, message.statusId);
      });")),
      uiOutput(ns("panel_intro")),
      uiOutput(ns("record_summary")),
      div(
        class = "d-flex gap-2 flex-wrap mb-3",
        parameter_save_button(ns, "save"),
        actionButton(ns("new_record"), "New record"),
        actionButton(ns("reload"), "Reload")
      ),
      tags$div(id = ns("save_status"), role = "status", `aria-live` = "polite", hidden = NA),
      reference_render_fields_ui(
        ns,
        config$columns,
        choices = reference_empty_choices(config$columns),
        values = reference_empty_row(config$columns)
      ),
      tags$hr(class = "my-3"),
      tags$h5("Suggested analytical choices"),
      tags$p(class = "text-muted small",
        "Advisory choices only; other valid catalogue entries remain allowed. Choices appear in selection order."),
      lapply(names(parameter_suggestion_tables()), function(catalogue) {
        selectizeInput(ns(paste0("suggest_", catalogue)), reference_table_configs()[[catalogue]]$title,
          choices = character(), multiple = TRUE, options = list(plugins = list("remove_button")))
      }),
      tags$hr(class = "my-3"),
      tags$h5("Group links"),
      tags$p(
        class = "text-muted small",
        paste(
          "Add one or more parameter group / sub-group combinations.",
          "Sub-group is optional."
        )
      ),
      selectInput(
        ns("relationship_group_id"),
        "Parameter group",
        choices = "",
        selected = "",
        selectize = FALSE
      ),
      selectInput(
        ns("relationship_sub_group_id"),
        "Parameter sub-group",
        choices = stats::setNames("", "Optional"),
        selected = "",
        selectize = FALSE
      ),
      div(
        class = "d-flex gap-2 flex-wrap mb-2",
        actionButton(ns("add_relationship"), "Add group link"),
        actionButton(ns("remove_relationship"), "Remove selected")
      ),
      DT::DTOutput(ns("relationships_table")),
      div(
        class = "d-grid mt-3",
        parameter_save_button(ns, "save_bottom")
      )
    ),
    uiOutput(ns("banner")),
    uiOutput(ns("table_heading")),
    DT::DTOutput(ns("records_table"))
  )
}

manageParameters <- function(id, language) {
  configs <- reference_table_configs()
  for (table in names(parameter_suggestion_tables())) {
    referenceTableManager(id = paste0(id, "_", table), language = language,
      config = configs[[table]], notification_module_id = "manageParameters")
  }
  moduleServer(id, function(input, output, session) {
    con <- session$userData$AquaCache
    config <- reference_table_configs()[["parameters"]]
    table_proxy <- DT::dataTableProxy("records_table", session = session)
    records_table_state_key <- as.character(jsonlite::toJSON(
      session$ns("records_table_state"),
      auto_unbox = TRUE
    ))
    records_table_state_save <- htmlwidgets::JS(sprintf(
      "function(settings, data) {
        try { sessionStorage.setItem(%s, JSON.stringify(data)); }
        catch (e) {}
      }",
      records_table_state_key
    ))
    records_table_state_load <- htmlwidgets::JS(sprintf(
      "function(settings) {
        try {
          var state = sessionStorage.getItem(%s);
          return state ? JSON.parse(state) : null;
        } catch (e) { return null; }
      }",
      records_table_state_key
    ))
    records_table_init <- htmlwidgets::JS(
      "function(settings, json) {
        var api = this.api();
        setTimeout(function() {
          var container = api.table().container();
          var filterInputs = $(container).find(
            '.dataTables_scrollHead thead tr:last input'
          );
          api.columns().every(function(index) {
            filterInputs.eq(index).val(this.search());
          });
          $(container).find('.dataTables_filter input[type=search]')
            .val(api.search());
        }, 0);
      }"
    )
    relationship_proxy <- DT::dataTableProxy(
      "relationships_table",
      session = session
    )

    editor_data <- reactiveValues(
      field_choices = list(),
      relationship_choices = list(),
      records_raw = data.frame(),
      records_display = data.frame()
    )
    current_record_id <- reactiveVal(NULL)
    current_relationships <- reactiveVal(parameter_relationships_empty())
    current_row <- reactiveVal(reference_empty_row(config$columns))
    pending_unit_change <- reactiveVal(NULL)
    unit_conversion_auto_keys <- reactiveVal(character(0))
    parameter_save_busy <- FALSE
    set_parameter_save_busy <- function(busy, label = "Checking affected records...") {
      session$sendCustomMessage("aqualink-parameter-save-state", list(
        ids = session$ns(c("save", "save_bottom", "confirm_unit_change")),
        busy = busy, label = label, statusId = session$ns("save_status")))
    }

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = session$ns,
        lang = language$language,
        con = con,
        module_id = "manageParameters"
      )
    })
    outputOptions(output, "banner", suspendWhenHidden = FALSE)

    update_relationship_selectors <- function(relationship_choices) {
      updateSelectInput(
        session,
        "relationship_group_id",
        choices = reference_make_input_choices(relationship_choices$groups),
        selected = ""
      )
      updateSelectInput(
        session,
        "relationship_sub_group_id",
        choices = reference_make_input_choices(
          relationship_choices$sub_groups,
          blank_label = "Optional"
        ),
        selected = ""
      )
    }

    clear_parameter_editor <- function(field_choices, relationship_choices) {
      empty_row <- reference_empty_row(config$columns)
      current_record_id(NULL)
      current_row(empty_row)
      reference_clear_fields(session, config$columns, field_choices)
      current_relationships(parameter_relationships_empty())
      update_parameter_suggestions(session, con)
      update_relationship_selectors(relationship_choices)
      DT::selectRows(table_proxy, NULL)
      DT::selectRows(relationship_proxy, NULL)
    }

    populate_parameter_editor <- function(
      selected_id,
      raw_row,
      field_choices,
      relationship_choices
    ) {
      current_record_id(as.character(selected_id))
      current_row(raw_row)
      reference_update_fields(session, config$columns, raw_row, field_choices)
      current_relationships(fetch_parameter_relationships(con, selected_id))
      update_parameter_suggestions(session, con, selected_id)
      update_relationship_selectors(relationship_choices)
      DT::selectRows(relationship_proxy, NULL)
    }

    refresh_editor <- function(selected_id = NULL) {
      selected_id <- if (is.null(selected_id) || !length(selected_id)) {
        NULL
      } else {
        as.character(selected_id)[1]
      }

      field_choices <- reference_load_choices(con, config$columns)
      relationship_choices <- fetch_parameter_relationship_choices(con)
      raw <- fetch_parameter_records(con, config)
      display <- reference_build_display_data(raw, config, field_choices)

      editor_data$field_choices <- field_choices
      editor_data$relationship_choices <- relationship_choices
      editor_data$records_raw <- raw
      editor_data$records_display <- display

      session$onFlushed(
        function() {
          update_relationship_selectors(relationship_choices)

          if (is.null(selected_id)) {
            clear_parameter_editor(field_choices, relationship_choices)
            return()
          }

          selected_idx <- match(
            as.character(selected_id),
            as.character(raw[[config$pk]])
          )

          if (is.na(selected_idx)) {
            clear_parameter_editor(field_choices, relationship_choices)
            return()
          }

          populate_parameter_editor(
            selected_id = selected_id,
            raw_row = raw[selected_idx, , drop = FALSE],
            field_choices = field_choices,
            relationship_choices = relationship_choices
          )
          DT::selectRows(table_proxy, selected_idx)
        },
        once = TRUE
      )
    }

    output$panel_intro <- renderUI({
      tagList(
        tags$p(class = "text-muted", config$description),
        tags$p(
          class = "text-muted small",
          paste(
            "Save updates the selected parameter and synchronizes its",
            "parameter group / sub-group links and suggested analytical choices."
          )
        )
      )
    })
    outputOptions(output, "panel_intro", suspendWhenHidden = FALSE)

    output$record_summary <- renderUI({
      record_id <- current_record_id()

      tags$p(
        tags$strong("Mode: "),
        if (is.null(record_id)) {
          "New parameter record"
        } else {
          paste("Editing", config$id_label, record_id)
        }
      )
    })
    outputOptions(output, "record_summary", suspendWhenHidden = FALSE)

    output$table_heading <- renderUI({
      row_count <- nrow(editor_data$records_display)

      tags$div(
        class = "mb-3",
        tags$h4("Parameter records"),
        tags$p(
          class = "text-muted",
          paste(
            "Select a row to load it into the form.",
            row_count,
            "record(s) currently available."
          )
        )
      )
    })
    outputOptions(output, "table_heading", suspendWhenHidden = FALSE)

    output$records_table <- DT::renderDT(
      {
        # Rebuild against the refreshed server-side data and restore the user's
        # filters, ordering, and page from DataTables' session state.
        display <- reference_prepare_for_dt(
          editor_data$records_display
        )
        DT::datatable(
          display,
          rownames = FALSE,
          selection = "single",
          filter = "top",
          colnames = unname(reference_display_labels(config, names(display))),
          options = list(
            pageLength = 10,
            lengthChange = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE,
            stateSave = TRUE,
            stateDuration = 0,
            stateSaveCallback = records_table_state_save,
            stateLoadCallback = records_table_state_load,
            initComplete = records_table_init
          )
        )
      },
      server = TRUE
    )
    outputOptions(output, "records_table", suspendWhenHidden = FALSE)

    output$relationships_table <- DT::renderDT(
      {
        relationship_display <- current_relationships()[,
          c(
            "group_name",
            "sub_group_name"
          ),
          drop = FALSE
        ]

        names(relationship_display) <- c(
          "Parameter group",
          "Parameter sub-group"
        )

        DT::datatable(
          relationship_display,
          rownames = FALSE,
          selection = "single",
          options = list(
            dom = "t",
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            autoWidth = TRUE,
            scrollX = TRUE
          )
        )
      },
      server = FALSE
    )
    outputOptions(output, "relationships_table", suspendWhenHidden = FALSE)

    observeEvent(input$reload, {
      selected_id <- current_record_id()
      refresh_editor(selected_id = selected_id)
    })

    observeEvent(input$new_record, {
      clear_parameter_editor(
        editor_data$field_choices,
        editor_data$relationship_choices
      )
    })

    observeEvent(
      input$records_table_rows_selected,
      {
        req(length(input$records_table_rows_selected) == 1)
        selected_row <- input$records_table_rows_selected[[1]]
        req(nrow(editor_data$records_raw) >= selected_row)

        selected_id <- as.character(
          editor_data$records_raw[[config$pk]][selected_row]
        )

        populate_parameter_editor(
          selected_id = selected_id,
          raw_row = editor_data$records_raw[selected_row, , drop = FALSE],
          field_choices = editor_data$field_choices,
          relationship_choices = editor_data$relationship_choices
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_relationship,
      {
        relationship_choices <- editor_data$relationship_choices
        group_id <- suppressWarnings(as.integer(input$relationship_group_id))
        sub_group_id <- suppressWarnings(as.integer(
          input$relationship_sub_group_id
        ))

        if (is.na(group_id)) {
          showNotification(
            "Select a parameter group before adding a link.",
            type = "error"
          )
          return()
        }

        new_relationship <- data.frame(
          relationship_id = NA_integer_,
          group_id = group_id,
          group_name = relationship_choices$groups$label[match(
            as.character(group_id),
            relationship_choices$groups$value
          )],
          sub_group_id = if (is.na(sub_group_id)) NA_integer_ else sub_group_id,
          sub_group_name = if (is.na(sub_group_id)) {
            NA_character_
          } else {
            relationship_choices$sub_groups$label[match(
              as.character(sub_group_id),
              relationship_choices$sub_groups$value
            )]
          },
          stringsAsFactors = FALSE
        )

        existing_relationships <- current_relationships()
        existing_keys <- parameter_relationship_keys(existing_relationships)
        new_key <- parameter_relationship_keys(new_relationship)

        if (new_key %in% existing_keys) {
          showNotification(
            "That group link is already present for this parameter.",
            type = "warning"
          )
          return()
        }

        updated_relationships <- rbind(existing_relationships, new_relationship)
        order_idx <- order(
          tolower(updated_relationships$group_name),
          tolower(ifelse(
            is.na(updated_relationships$sub_group_name),
            "",
            updated_relationships$sub_group_name
          ))
        )
        current_relationships(updated_relationships[order_idx, , drop = FALSE])
        update_relationship_selectors(relationship_choices)
        DT::selectRows(relationship_proxy, NULL)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$remove_relationship,
      {
        selected_row <- input$relationships_table_rows_selected
        if (!length(selected_row)) {
          showNotification(
            "Select a group link to remove.",
            type = "warning"
          )
          return()
        }

        relationships <- current_relationships()
        relationships <- relationships[-selected_row, , drop = FALSE]
        current_relationships(relationships)
        DT::selectRows(relationship_proxy, NULL)
      },
      ignoreInit = TRUE
    )

    parameter_unit_numeric_input <- function(prefix, index, label, value) {
      numericInput(
        session$ns(paste0(prefix, "_", index)),
        label = label,
        value = as.numeric(value)
      )
    }

    show_parameter_unit_change_modal <- function(pending) {
      usage <- pending$usage
      choices <- usage$state_choices
      unit_choices <- usage$unit_choices
      defaults <- pending$defaults
      unit_name <- function(unit_id) {
        if (is.null(unit_id) || !length(unit_id) || is.na(unit_id)) {
          return("no unit assigned")
        }
        idx <- match(as.character(unit_id), as.character(unit_choices$unit_id))
        if (is.na(idx)) as.character(unit_id) else unit_choices$unit_name[[idx]]
      }
      state_choices <- if (nrow(choices)) {
        destination_unit_ids <- vapply(choices$matrix_state_code, function(code) {
          field <- parameter_unit_fields()$unit_column[
            match(code, parameter_unit_fields()$matrix_state_code)
          ]
          parameter_unit_value_key(pending$values[[field]])
        }, character(1))
        labels <- paste0(
          choices$matrix_state_name, " — ",
          vapply(destination_unit_ids, unit_name, character(1))
        )
        stats::setNames(choices$matrix_state_code, labels)
      } else {
        c("No unit-bearing matrix state is available" = "")
      }
      old_unit_choices <- c(
        "Choose the unit used by existing values" = "",
        stats::setNames(
          as.character(unit_choices$unit_id),
          unit_choices$unit_name
        )
      )
      impact_rows <- if (nrow(usage$usage)) {
        lapply(seq_len(nrow(usage$usage)), function(i) {
          row <- usage$usage[i, , drop = FALSE]
          tags$tr(
            tags$td(row$table[[1]]),
            tags$td(format(as.numeric(row$count[[1]]), big.mark = ",")),
            tags$td(if (isTRUE(row$supported[[1]])) "Will be updated" else "Review required"),
            tags$td(row$detail[[1]])
          )
        })
      } else {
        list(tags$tr(tags$td(colspan = 4, "No dependent rows were found.")))
      }
      map_rows <- if (nrow(usage$mapping_sets)) {
        lapply(seq_len(nrow(usage$mapping_sets)), function(i) {
          row <- usage$mapping_sets[i, , drop = FALSE]
          tags$tr(
            tags$td(row$source_code[[1]]),
            tags$td(if (is.na(row$import_profile_id[[1]])) "Source-wide" else as.character(row$import_profile_id[[1]])),
            tags$td(paste0("v", row$version[[1]], " (", row$status[[1]], ")")),
            tags$td(if (identical(row$status[[1]], "draft")) {
              "Update draft; keep unpublished"
            } else if (is.na(row$open_draft_id[[1]])) {
              "Publish a unit-adjusted revision"
            } else {
              "Publish a unit-adjusted revision; carry unfinished edits forward as a draft"
            })
          )
        })
      } else {
        NULL
      }
      state_controls <- lapply(seq_len(nrow(defaults)), function(i) {
        row <- defaults[i, , drop = FALSE]
        old_unit <- if (is.na(row$old_unit_id[[1]])) {
          tags$div(
            selectInput(
              session$ns(paste0("unit_assumed_", i)),
              "Unit represented by existing values",
              choices = old_unit_choices,
              selected = ""
            )
          )
        } else {
          tags$p(paste("Existing unit:", unit_name(row$old_unit_id[[1]])))
        }
        source_id <- row$old_unit_id[[1]]
        key <- paste(if (is.na(source_id)) "" else source_id, row$destination_unit_id[[1]], sep = ":")
        mode_id <- paste0("unit_conversion_mode_", i)
        tags$div(
          class = "panel panel-default",
          tags$div(class = "panel-heading", tags$strong(row$unit_label[[1]])),
          tags$div(
            class = "panel-body",
            old_unit,
            selectInput(
              session$ns(paste0("unit_destination_", i)),
              "Move existing matrix state to",
              choices = state_choices,
              selected = row$destination_code[[1]]
            ),
            selectInput(
              session$ns(mode_id), "How should existing numbers be handled?",
              choices = c(
                "Choose an action" = "",
                "Convert values (database or custom conversion)" = paste0("convert:", key),
                "Keep existing numbers; change the unit only" = paste0("keep:", key)
              ),
              selected = if (isTRUE(row$conversion_known[[1]])) paste0("convert:", key) else ""
            ),
            tags$p(
              class = "text-muted small",
              "Known conversions are filled in automatically. If none exists, choose to keep existing numbers or enter a custom conversion. For example, correcting a unit from % to m can keep all numbers unchanged."
            ),
            conditionalPanel(
              condition = sprintf("input.%s && input.%s.indexOf('convert:') === 0", mode_id, mode_id),
              ns = session$ns,
              fluidRow(
                column(4, parameter_unit_numeric_input("discrete_a", i, "Discrete results scale (a)", row$conversion_a[[1]])),
                column(4, parameter_unit_numeric_input("discrete_b", i, "Discrete results offset (b)", row$conversion_b[[1]])),
                column(4, parameter_unit_numeric_input("continuous_a", i, "Continuous data scale (a)", row$conversion_a[[1]]))
              ),
              fluidRow(
                column(4, parameter_unit_numeric_input("continuous_b", i, "Continuous data offset (b)", row$conversion_b[[1]])),
                column(4, parameter_unit_numeric_input("transmission_a", i, "Future continuous delivery scale (a)", row$transmission_a[[1]])),
                column(4, parameter_unit_numeric_input("transmission_b", i, "Future continuous delivery offset (b)", row$transmission_b[[1]]))
              ),
              fluidRow(
                column(4, parameter_unit_numeric_input("mapping_a", i, "Future import mapping scale (a)", row$conversion_a[[1]])),
                column(4, parameter_unit_numeric_input("mapping_b", i, "Future import mapping offset (b)", row$conversion_b[[1]]))
              )
            )
          )
        )
      })
      has_blockers <- length(usage$blockers) > 0L
      blocker_ui <- if (has_blockers) {
        tags$div(
          class = "alert alert-danger",
          tags$strong("This change cannot be applied yet."),
          tags$p("Resolve these items first; the affected rows will not be changed."),
          tags$ul(lapply(usage$blockers, tags$li))
        )
      } else {
        NULL
      }
      map_ui <- if (length(map_rows)) {
        tags$div(
          tags$h4("Import mapping revisions"),
          tags$p(
            "Draft mappings will be adjusted and kept unpublished. Published mappings will receive a new revision in the same transaction. If an unfinished draft also exists, its edits will be carried forward into a new unpublished draft."
          ),
          tags$table(
            class = "table table-condensed",
            tags$thead(tags$tr(
              tags$th("Source"), tags$th("Scope"), tags$th("Current revision"),
              tags$th("Action")
            )),
            tags$tbody(map_rows)
          )
        )
      } else {
        NULL
      }
      footer <- if (has_blockers) {
        modalButton("Close")
      } else {
        tagList(
          actionButton(session$ns("cancel_unit_change"), "Cancel"),
          parameter_save_button(session$ns, "confirm_unit_change", "Apply unit change", "Applying unit change...")
        )
      }

      showModal(modalDialog(
        title = "Review parameter unit change",
        tags$p(
          "Changing these units affects stored values, matrix states, guideline limits, rating curves, and future imports. Review the row counts and choose conversions before applying the change."
        ),
        tags$div(
          class = "alert alert-warning",
          "If you started an upload before changing units, return to that upload module and reload it, then preview or enter the data again in the new unit. Previously staged or queued uploads may still contain values in the old unit."
        ),
        tags$table(
          class = "table table-condensed",
          tags$thead(tags$tr(
            tags$th("Affected table"), tags$th("Rows"),
            tags$th("Migration"), tags$th("Details")
          )),
          tags$tbody(impact_rows)
        ),
        blocker_ui,
        map_ui,
        tags$h4("Matrix-state destinations and conversions"),
        tags$p(
          "Choose 'Keep existing numbers' to correct the unit without changing stored values or future import conversions. Otherwise, use an offset when the conversion is affine (for example, temperature)."
        ),
        tags$p(
          "Discrete controls convert stored results, result components, and fixed guideline limits. Continuous data controls convert measurements, daily summaries, forecasts, extrema values and uncertainty bounds, thresholds, corrections, and rating-curve values. Continuous delivery controls adjust future transmissions. Import-map controls adjust future discrete imports. Each path can use its own conversion."
        ),
        tags$p("For every control, the conversion is new value = a × old value + b."),
        tagList(state_controls),
        footer = footer,
        size = "l",
        easyClose = FALSE
      ))
    }

    commit_parameter_save <- function(values, selected_id, migration = NULL, unit_change = !is.null(migration)) {
      tryCatch(
        {
          saved_id <- DBI::dbWithTransaction(con, {
            if (!is.null(migration)) {
              DBI::dbExecute(con, "SET LOCAL lock_timeout = '30s'")
              lock_tables <- c(
                "continuous.corrections",
                "continuous.extrema",
                "continuous.forecasts",
                "continuous.measurements_calculated_daily",
                "continuous.measurements_continuous",
                "continuous.rating_curve_points",
                "continuous.rating_curve_shifts",
                "continuous.rating_curves",
                "continuous.timeseries",
                "continuous.timeseries_aggregation_methods",
                "continuous.timeseries_aggregation_method_steps",
                "continuous.timeseries_aggregation_method_step_inputs",
                "continuous.thresholds",
                "continuous.transmission_timeseries_mappings",
                "criteria.guideline_rule_coefficients",
                "criteria.guideline_rule_inputs",
                "criteria.guideline_value_rules",
                "criteria.guidelines",
                "discrete.import_mapping_sets",
                "discrete.import_location_mappings",
                "discrete.import_parameter_mappings",
                "discrete.import_result_flag_mappings",
                "discrete.result_aggregations",
                "discrete.results",
                "discrete.result_components"
              )
              parameter_ref_tables <- migration$usage$parameter_refs
              if (!is.null(parameter_ref_tables) && nrow(parameter_ref_tables)) {
                lock_tables <- c(
                  lock_tables,
                  paste(
                    parameter_ref_tables$schema_name,
                    parameter_ref_tables$table_name,
                    sep = "."
                  )
                )
              }
              timeseries_ref_tables <- migration$usage$timeseries_refs
              if (!is.null(timeseries_ref_tables) && nrow(timeseries_ref_tables)) {
                lock_tables <- c(
                  lock_tables,
                  paste(
                    timeseries_ref_tables$schema_name,
                    timeseries_ref_tables$table_name,
                    sep = "."
                  )
                )
              }
              lock_tables <- unique(lock_tables)
              for (relation_name in sort(lock_tables)) {
                parts <- strsplit(relation_name, "\\.")[[1]]
                if (!parameter_unit_relation_exists(con, parts[[1]], parts[[2]])) {
                  next
                }
                relation <- as.character(DBI::dbQuoteIdentifier(
                  con,
                  DBI::Id(schema = parts[[1]], table = parts[[2]])
                ))
                DBI::dbExecute(
                  con,
                  paste("LOCK TABLE", relation, "IN SHARE ROW EXCLUSIVE MODE")
                )
              }

              if (!identical(as.character(current_record_id()), as.character(selected_id))) {
                stop("The selected parameter changed while the unit-change review was open. Reopen the review and try again.")
              }
              current_units <- fetch_parameter_unit_values(con, selected_id)
              current_units_json <- as.character(jsonlite::toJSON(
                parameter_unit_json_object(current_units[1, , drop = FALSE], names(migration$old_units)),
                auto_unbox = TRUE,
                na = "null"
              ))
              if (!identical(current_units_json, migration$old_units_json)) {
                stop("The parameter units changed after the review opened. Reopen the unit-change review.")
              }
              current_usage <- fetch_parameter_unit_usage(
                con, selected_id, migration$changed_units, values
              )
              if (!identical(
                as.character(parameter_unit_inventory_signature(current_usage)),
                migration$usage_signature
              )) {
                stop("Affected rows or import mapping revisions changed after the review opened. Reopen the unit-change review.")
              }
              if (length(current_usage$blockers)) {
                stop(paste(
                  "The unit change is blocked by:",
                  paste(current_usage$blockers, collapse = "; ")
                ))
              }

              old_unit_object <- parameter_unit_json_object(
                current_units[1, , drop = FALSE], names(migration$old_units)
              )
              new_unit_object <- parameter_unit_json_object(values, names(migration$old_units))
              plan_json <- as.character(jsonlite::toJSON(
                list(
                  plan = migration$plan,
                  affected_tables = current_usage$usage,
                  parameter_references = current_usage$parameter_refs,
                  timeseries_references = current_usage$timeseries_refs,
                  mapping_sets = current_usage$mapping_sets[
                    , intersect(
                      c("import_mapping_set_id", "source_code", "version", "status"),
                      names(current_usage$mapping_sets)
                    ), drop = FALSE
                  ]
                ),
                dataframe = "rows",
                auto_unbox = TRUE,
                na = "null"
              ))
              DBI::dbGetQuery(
                con,
                "SELECT public.begin_parameter_unit_change(
                   $1, $2::jsonb, $3::jsonb, $4::jsonb
                 ) AS change_id",
                params = list(
                  as.integer(selected_id),
                  as.character(jsonlite::toJSON(old_unit_object, auto_unbox = TRUE, na = "null")),
                  as.character(jsonlite::toJSON(new_unit_object, auto_unbox = TRUE, na = "null")),
                  plan_json
                )
              )
              saved_parameter_id <- reference_save_record(
                con = con,
                config = config,
                values = values,
                selected_id = selected_id,
                existing_columns = names(editor_data$records_raw)
              )
              parameter_unit_apply_migration(
                con,
                as.integer(selected_id),
                migration$plan,
                current_usage$mapping_sets
              )
            }

            if (is.null(migration)) {
              saved_parameter_id <- reference_save_record(
                con = con,
                config = config,
                values = values,
                selected_id = selected_id,
                existing_columns = names(editor_data$records_raw)
              )
            }

            existing_relationships <- fetch_parameter_relationships(
              con,
              saved_parameter_id
            )
            desired_relationships <- current_relationships()[,
              c("group_id", "sub_group_id"),
              drop = FALSE
            ]
            existing_keys <- parameter_relationship_keys(existing_relationships)
            desired_keys <- parameter_relationship_keys(desired_relationships)
            delete_ids <- existing_relationships$relationship_id[
              !(existing_keys %in% desired_keys)
            ]
            if (length(delete_ids)) {
              for (relationship_id in delete_ids) {
                DBI::dbExecute(
                  con,
                  "DELETE FROM public.parameter_relationships WHERE relationship_id = $1",
                  params = list(as.integer(relationship_id))
                )
              }
            }
            new_relationships <- desired_relationships[
              !(desired_keys %in% existing_keys), , drop = FALSE
            ]
            if (nrow(new_relationships)) {
              for (row_idx in seq_len(nrow(new_relationships))) {
                DBI::dbExecute(
                  con,
                  paste(
                    "INSERT INTO public.parameter_relationships",
                    "(parameter_id, group_id, sub_group_id) VALUES ($1, $2, $3)"
                  ),
                  params = list(
                    as.integer(saved_parameter_id),
                    as.integer(new_relationships$group_id[row_idx]),
                    if (is.na(new_relationships$sub_group_id[row_idx])) {
                      NA_integer_
                    } else {
                      as.integer(new_relationships$sub_group_id[row_idx])
                    }
                  )
                )
              }
            }
            save_parameter_suggestions(con, saved_parameter_id, input)
            saved_parameter_id
          })

          pending_unit_change(NULL)
          if (!is.null(migration)) {
            removeModal()
          }
          clear_reference_caches(session)
          refresh_editor(selected_id = saved_id)
          showNotification("Parameters saved.", type = "message")
          if (isTRUE(unit_change)) {
            showNotification(
              "Units changed. If you previously started an upload, reload that upload module and preview or enter the data again in the new unit. Staged or queued uploads may still contain values in the old unit.",
              type = "warning", duration = NULL
            )
          }
          invisible(TRUE)
        },
        error = function(e) {
          showNotification(
            paste("Save failed:", conditionMessage(e)),
            type = "error",
            duration = 15
          )
          invisible(FALSE)
        }
      )
    }

    validate_parameter_save <- function(values) {
      errors <- character(0)
      for (field in config$columns) {
        if (
          isTRUE(field$required) &&
            reference_is_missing_value(values[[field$name]])
        ) {
          errors <- c(errors, paste(field$label, "is required."))
        }
      }
      unit_fields <- c("units_liquid", "units_solid", "units_gas", "units_na")
      assigned_units <- !vapply(
        values[unit_fields], reference_is_missing_value, logical(1)
      )
      has_regular_units <- any(assigned_units[c("units_liquid", "units_solid", "units_gas")])
      has_not_applicable_unit <- assigned_units[["units_na"]]
      if (!any(assigned_units)) {
        errors <- c(errors, "Assign at least one unit: liquid, solid, gas, or not applicable.")
      } else if (has_regular_units && has_not_applicable_unit) {
        errors <- c(errors, "Choose either a not applicable unit or one or more liquid, solid, or gas units, not both.")
      }
      if (
        !is.na(values$plot_default_floor) && !is.na(values$plot_default_ceiling) &&
          values$plot_default_floor > values$plot_default_ceiling
      ) {
        errors <- c(errors, "Default plot floor cannot be greater than the default plot ceiling.")
      }
      errors
    }

    save_parameter <- function() {
      if (parameter_save_busy) return(invisible(FALSE))
      parameter_save_busy <<- TRUE
      set_parameter_save_busy(TRUE)
      on.exit({
        parameter_save_busy <<- FALSE
        set_parameter_save_busy(FALSE)
      }, add = TRUE)
      values <- tryCatch(reference_collect_values(input, config$columns), error = function(e) e)
      if (inherits(values, "error")) {
        showNotification(paste("Save failed:", conditionMessage(values)), type = "error", duration = 15)
        return(invisible(FALSE))
      }
      validation_errors <- validate_parameter_save(values)
      if (length(validation_errors)) {
        showNotification(paste(validation_errors, collapse = " "), type = "error")
        return(invisible(FALSE))
      }

      selected_id <- current_record_id()
      if (!is.null(selected_id)) {
        tryCatch({
          existing_units <- fetch_parameter_unit_values(con, selected_id)
          changed_units <- parameter_changed_assigned_unit_fields(existing_units, values)
          if (nrow(changed_units)) {
            usage <- fetch_parameter_unit_usage(con, selected_id, changed_units, values)
            if (nrow(usage$usage)) {
              defaults <- parameter_unit_change_defaults(
                con, usage$changed_units, values, usage$state_choices, usage
              )
              old_units_json <- as.character(jsonlite::toJSON(
                parameter_unit_json_object(existing_units[1, , drop = FALSE], names(existing_units)),
                auto_unbox = TRUE,
                na = "null"
              ))
              pending <- list(
                selected_id = as.character(selected_id),
                values = values,
                old_units = as.list(existing_units[1, names(existing_units), drop = FALSE]),
                old_units_json = old_units_json,
                changed_units = usage$changed_units,
                usage = usage,
                defaults = defaults,
                usage_signature = as.character(parameter_unit_inventory_signature(usage))
              )
              pending_unit_change(pending)
              initial_keys <- vapply(seq_len(nrow(defaults)), function(i) {
                source_id <- defaults$old_unit_id[[i]]
                paste(if (is.na(source_id)) "" else source_id, defaults$destination_unit_id[[i]], sep = ":")
              }, character(1))
              unit_conversion_auto_keys(initial_keys)
              show_parameter_unit_change_modal(pending)
              return(invisible(FALSE))
            }
          }
          commit_parameter_save(values, selected_id, unit_change = nrow(changed_units) > 0L)
        }, error = function(e) {
          showNotification(paste("Save failed:", conditionMessage(e)), type = "error", duration = 15)
          invisible(FALSE)
        })
      } else {
        commit_parameter_save(values, NULL)
      }
    }

    observe({
      pending <- pending_unit_change()
      if (is.null(pending)) {
        return()
      }
      defaults <- pending$defaults
      old_keys <- isolate(unit_conversion_auto_keys())
      new_keys <- character(nrow(defaults))
      for (i in seq_len(nrow(defaults))) {
        source_id <- defaults$old_unit_id[[i]]
        if (is.na(source_id)) {
          assumed <- input[[paste0("unit_assumed_", i)]]
          source_id <- if (is.null(assumed) || !nzchar(as.character(assumed))) {
            NA_integer_
          } else {
            as.integer(assumed)
          }
        }
        destination_code <- input[[paste0("unit_destination_", i)]]
        if (is.null(destination_code) || !nzchar(as.character(destination_code))) {
          destination_code <- defaults$destination_code[[i]]
        }
        fields <- parameter_unit_fields()
        destination_field <- fields$unit_column[
          match(destination_code, fields$matrix_state_code)
        ]
        destination_id <- if (length(destination_field) && !is.na(destination_field)) {
          as.integer(parameter_unit_value_key(pending$values[[destination_field]]))
        } else {
          NA_integer_
        }
        key <- paste(if (is.na(source_id)) "" else source_id, destination_id, sep = ":")
        new_keys[[i]] <- key
        old_key <- if (length(old_keys) >= i) old_keys[[i]] else ""
        if (identical(key, old_key)) {
          next
        }
        conversion <- parameter_unit_conversion(con, source_id, destination_id)
        known <- !is.null(conversion) && nrow(conversion) > 0L
        # Encode the unit pair in each choice so confirmation cannot accept
        # an old action while the browser is receiving new control values.
        updateSelectInput(session, paste0("unit_conversion_mode_", i),
          choices = c(
            "Choose an action" = "",
            "Convert values (database or custom conversion)" = paste0("convert:", key),
            "Keep existing numbers; change the unit only" = paste0("keep:", key)
          ),
          selected = if (known) paste0("convert:", key) else "")
        for (prefix in c("discrete_a", "continuous_a", "transmission_a", "mapping_a")) {
          updateNumericInput(session, paste0(prefix, "_", i),
            value = if (known) as.numeric(conversion$scale_a[[1]]) else NA_real_)
        }
        for (prefix in c("discrete_b", "continuous_b", "transmission_b", "mapping_b")) {
          updateNumericInput(session, paste0(prefix, "_", i),
            value = if (known) as.numeric(conversion$scale_b[[1]]) else NA_real_)
        }
      }
      unit_conversion_auto_keys(new_keys)
    })

    observeEvent(input$cancel_unit_change, {
      pending_unit_change(NULL)
      removeModal()
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_unit_change, {
      if (parameter_save_busy) return()
      parameter_save_busy <<- TRUE
      set_parameter_save_busy(TRUE, "Applying unit change...")
      on.exit({
        parameter_save_busy <<- FALSE
        set_parameter_save_busy(FALSE)
      }, add = TRUE)
      pending <- pending_unit_change()
      if (is.null(pending)) {
        removeModal()
        return()
      }
      if (length(pending$usage$blockers)) {
        showNotification("Resolve the listed blockers before applying this unit change.", type = "error")
        return()
      }
      plan <- pending$defaults
      plan$conversion_action <- rep(NA_character_, nrow(plan))
      for (metric in c("discrete_a", "discrete_b", "continuous_a", "continuous_b", "transmission_a", "transmission_b", "mapping_a", "mapping_b")) {
        plan[[metric]] <- rep(NA_real_, nrow(plan))
      }
      validation_errors <- character(0)
      for (i in seq_len(nrow(plan))) {
        destination_code <- input[[paste0("unit_destination_", i)]]
        if (is.null(destination_code) || !nzchar(as.character(destination_code))) {
          validation_errors <- c(validation_errors, paste(plan$unit_label[[i]], "needs a destination matrix state."))
          next
        }
        destination_idx <- match(destination_code, pending$usage$state_choices$matrix_state_code)
        if (is.na(destination_idx)) {
          validation_errors <- c(validation_errors, paste(plan$unit_label[[i]], "has an invalid destination matrix state."))
          next
        }
        destination_field <- parameter_unit_fields()$unit_column[
          match(destination_code, parameter_unit_fields()$matrix_state_code)
        ]
        destination_unit <- parameter_unit_value_key(pending$values[[destination_field]])
        plan$destination_code[[i]] <- as.character(destination_code)
        plan$destination_id[[i]] <- as.integer(pending$usage$state_choices$matrix_state_id[[destination_idx]])
        plan$destination_unit_id[[i]] <- as.integer(destination_unit)

        needs_assumption <- FALSE
        if (is.na(plan$old_unit_id[[i]])) {
          assumed <- input[[paste0("unit_assumed_", i)]]
          has_unscoped_rows <- nrow(pending$usage$usage) > 0L &&
            any(is.na(pending$usage$usage$matrix_state_code))
          needs_assumption <- parameter_unit_data_has_state_rows(
            pending$usage,
            plan$matrix_state_code[[i]]
          ) || has_unscoped_rows
          if (!is.null(assumed) && nzchar(as.character(assumed))) {
            plan$old_unit_id[[i]] <- as.integer(assumed)
          }
        }
        source_id <- plan$old_unit_id[[i]]
        key <- paste(if (is.na(source_id)) "" else source_id, plan$destination_unit_id[[i]], sep = ":")
        mode <- input[[paste0("unit_conversion_mode_", i)]]
        if (is.null(mode) || !mode %in% paste0(c("convert:", "keep:"), key)) {
          validation_errors <- c(validation_errors, paste(plan$unit_label[[i]], "needs a conversion or keep-numbers choice for the selected units."))
          next
        }
        keep_numbers <- identical(mode, paste0("keep:", key))
        plan$conversion_action[[i]] <- if (keep_numbers) "keep" else "convert"
        if (!keep_numbers && is.na(source_id) && needs_assumption) {
          validation_errors <- c(validation_errors, paste(plan$unit_label[[i]], "needs the unit currently represented by its stored values."))
        }
        for (metric in c("discrete_a", "discrete_b", "continuous_a", "continuous_b", "transmission_a", "transmission_b", "mapping_a", "mapping_b")) {
          value <- if (keep_numbers) {
            if (grepl("_a$", metric)) 1 else 0
          } else {
            suppressWarnings(as.numeric(input[[paste0(metric, "_", i)]]))
          }
          if (length(value) != 1L || !is.finite(value)) {
            validation_errors <- c(validation_errors, paste(plan$unit_label[[i]], "has an invalid", metric, "value."))
          } else {
            plan[[metric]][[i]] <- value
          }
        }
        for (metric in c("discrete_a", "continuous_a", "transmission_a", "mapping_a")) {
          if (is.finite(plan[[metric]][[i]]) && plan[[metric]][[i]] <= 0) {
            validation_errors <- c(validation_errors, paste(plan$unit_label[[i]], metric, "must be positive."))
          }
        }
      }
      if (length(validation_errors)) {
        showNotification(paste(unique(validation_errors), collapse = " "), type = "error", duration = 15)
        return()
      }
      migration <- pending
      migration$plan <- plan
      commit_parameter_save(pending$values, pending$selected_id, migration)
    }, ignoreInit = TRUE)

    observeEvent(input$save, {
      save_parameter()
    }, ignoreInit = TRUE)

    observeEvent(input$save_bottom, {
      save_parameter()
    }, ignoreInit = TRUE)

    observeEvent(input$ui_ready, refresh_editor(), once = TRUE, ignoreInit = TRUE)
  })
}

manageOrganizationsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["organizations"]]
  )
}

manageOrganizations <- function(id, language) {
  config <- reference_table_configs()[["organizations"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageOrganizations"
  )
}

manageNetworksUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["networks"]]
  )
}

manageNetworks <- function(id, language) {
  config <- reference_table_configs()[["networks"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageNetworks"
  )
}

manageProjectsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["projects"]]
  )
}

manageProjects <- function(id, language) {
  config <- reference_table_configs()[["projects"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageProjects"
  )
}

manageNetworkProjectTypesUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["network_project_types"]]
  )
}

manageNetworkProjectTypes <- function(id, language) {
  config <- reference_table_configs()[["network_project_types"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageNetworkProjectTypes"
  )
}

manageLocationTypesUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["location_types"]]
  )
}

manageLocationTypes <- function(id, language) {
  config <- reference_table_configs()[["location_types"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageLocationTypes"
  )
}

manageMediaTypesUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["media_types"]]
  )
}

manageMediaTypes <- function(id, language) {
  config <- reference_table_configs()[["media_types"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageMediaTypes"
  )
}

manageMatrixStatesUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["matrix_states"]]
  )
}

manageMatrixStates <- function(id, language) {
  config <- reference_table_configs()[["matrix_states"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageMatrixStates"
  )
}

manageParameterGroupsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["parameter_groups"]]
  )
}

manageParameterGroups <- function(id, language) {
  config <- reference_table_configs()[["parameter_groups"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageParameterGroups"
  )
}

manageParameterSubGroupsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["parameter_sub_groups"]]
  )
}

manageParameterSubGroups <- function(id, language) {
  config <- reference_table_configs()[["parameter_sub_groups"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageParameterSubGroups"
  )
}

manageParametersUI <- function(id) {
  configs <- reference_table_configs()
  panels <- lapply(names(parameter_suggestion_tables()), function(table) {
    bslib::nav_panel(configs[[table]]$title,
      referenceTableManagerUI(paste0(id, "_", table), config = configs[[table]]))
  })
  do.call(bslib::navset_tab, c(list(bslib::nav_panel("Parameters", parameterManagerUI(id))), panels))
}

manageCommunicationProtocolFamiliesUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["communication_protocol_families"]]
  )
}

manageCommunicationProtocolFamilies <- function(id, language) {
  config <- reference_table_configs()[["communication_protocol_families"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageCommunicationProtocolFamilies"
  )
}

manageCommunicationProtocolsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["communication_protocols"]]
  )
}

manageCommunicationProtocols <- function(id, language) {
  config <- reference_table_configs()[["communication_protocols"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageCommunicationProtocols"
  )
}

manageTransmissionMethodFamiliesUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["transmission_method_families"]]
  )
}

manageTransmissionMethodFamilies <- function(id, language) {
  config <- reference_table_configs()[["transmission_method_families"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageTransmissionMethodFamilies"
  )
}

manageTransmissionMethodsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["transmission_methods"]]
  )
}

manageTransmissionMethods <- function(id, language) {
  config <- reference_table_configs()[["transmission_methods"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageTransmissionMethods"
  )
}

manageTransmissionComponentRolesUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["transmission_component_roles"]]
  )
}

manageTransmissionComponentRoles <- function(id, language) {
  config <- reference_table_configs()[["transmission_component_roles"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageTransmissionComponentRoles"
  )
}

manageInstrumentConnectionsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["instrument_connections"]]
  )
}

manageInstrumentConnections <- function(id, language) {
  config <- reference_table_configs()[["instrument_connections"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageInstrumentConnections"
  )
}

manageInstrumentConnectionSignalsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["instrument_connection_signals"]]
  )
}

manageInstrumentConnectionSignals <- function(id, language) {
  config <- reference_table_configs()[["instrument_connection_signals"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageInstrumentConnectionSignals"
  )
}

manageTransmissionSetupsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["transmission_setups"]]
  )
}

manageTransmissionSetups <- function(id, language) {
  config <- reference_table_configs()[["transmission_setups"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageTransmissionSetups"
  )
}

manageTransmissionRoutesUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["transmission_routes"]]
  )
}

manageTransmissionRoutes <- function(id, language) {
  config <- reference_table_configs()[["transmission_routes"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageTransmissionRoutes"
  )
}

manageTransmissionTimeseriesMappingsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[[
      "transmission_timeseries_mappings"
    ]]
  )
}

manageTransmissionTimeseriesMappings <- function(id, language) {
  config <- reference_table_configs()[[
    "transmission_timeseries_mappings"
  ]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageTransmissionTimeseriesMappings"
  )
}

viewTransmissionImportRunsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["transmission_import_runs"]]
  )
}

viewTransmissionImportRuns <- function(id, language) {
  config <- reference_table_configs()[["transmission_import_runs"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "viewTransmissionImportRuns"
  )
}

manageTransmissionComponentsUI <- function(id) {
  referenceTableManagerUI(
    id,
    config = reference_table_configs()[["transmission_components"]]
  )
}

manageTransmissionComponents <- function(id, language) {
  config <- reference_table_configs()[["transmission_components"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageTransmissionComponents"
  )
}
