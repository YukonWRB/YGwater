module_privilege_requirement <- function(
  tables = character(0),
  privileges = list(c("DELETE", "INSERT", "UPDATE")),
  visibility = "all",
  role_attributes = character(0)
) {
  if (length(tables) > 0) {
    if (length(privileges) == 1) {
      privileges <- rep(privileges, length(tables))
    }
    if (length(privileges) != length(tables)) {
      stop("Privileges must have length one or match the number of tables.")
    }
    privileges <- lapply(privileges, function(x) unique(toupper(x)))
  } else {
    privileges <- list()
  }

  list(
    tables = as.character(tables),
    privileges = privileges,
    visibility = match.arg(visibility, c("all", "any")),
    role_attributes = unique(toupper(role_attributes))
  )
}

# Keep this catalogue as the single source of truth for admin-page visibility.
# Full-functionality checks also require USAGE on every schema represented here.
ygwater_module_privilege_requirements <- function() {
  req <- module_privilege_requirement
  default_write <- list(c("DELETE", "INSERT", "UPDATE"))

  list(
    addLocation = req(
      c(
        "public.locations",
        "public.locations_networks",
        "public.locations_projects",
        "public.networks",
        "public.projects"
      ),
      list(
        c("INSERT", "UPDATE"),
        c("INSERT", "UPDATE"),
        c("INSERT", "UPDATE"),
        "INSERT",
        "INSERT"
      )
    ),
    addSubLocation = req(
      c("public.sub_locations", "public.locations"),
      list(c("INSERT", "UPDATE"), "SELECT")
    ),
    calibrate = req(
      c(
        "instruments.calibrations",
        "instruments.calibrate_ph",
        "instruments.calibrate_temperature",
        "instruments.calibrate_orp",
        "instruments.calibrate_specific_conductance",
        "instruments.calibrate_turbidity",
        "instruments.calibrate_dissolved_oxygen",
        "instruments.calibrate_depth",
        "instruments.instruments",
        "instruments.instrument_makes",
        "instruments.instrument_models",
        "instruments.instrument_types",
        "instruments.observers",
        "public.organizations"
      ),
      c(
        rep(default_write, 8),
        list(c("INSERT", "UPDATE")),
        rep(list("INSERT"), 5)
      )
    ),
    manageInstruments = req(
      c(
        "instruments.instruments",
        "instruments.observers",
        "instruments.instrument_makes",
        "instruments.instrument_models",
        "instruments.instrument_types",
        "public.organizations",
        "instruments.suppliers"
      ),
      c(
        list(c("SELECT", "INSERT", "UPDATE")),
        rep(list(c("SELECT", "INSERT")), 6)
      )
    ),
    manageSensors = req(
      c(
        "instruments.sensors",
        "instruments.sensor_types",
        "instruments.sensor_makes",
        "instruments.sensor_models",
        "public.organizations",
        "instruments.suppliers"
      ),
      c(
        list(c("SELECT", "INSERT", "UPDATE")),
        rep(list(c("SELECT", "INSERT")), 3),
        rep(list("SELECT"), 2)
      )
    ),
    instrumentMaintenance = req(
      c(
        "instruments.instruments",
        "instruments.instrument_maintenance",
        "instruments.instrument_maintenance_due",
        "instruments.instrument_sensor_events",
        "instruments.instrument_sensor_event_slots",
        "instruments.sensors",
        "instruments.sensor_types",
        "instruments.sensor_makes",
        "instruments.sensor_models",
        "instruments.observers",
        "instruments.instrument_makes",
        "instruments.instrument_models",
        "instruments.instrument_types",
        "public.organizations",
        "instruments.suppliers"
      ),
      list(
        "SELECT",
        c("SELECT", "INSERT", "UPDATE"),
        c("SELECT", "INSERT", "UPDATE", "DELETE"),
        c("SELECT", "INSERT"),
        c("SELECT", "INSERT"),
        c("SELECT", "INSERT", "UPDATE"),
        "SELECT",
        c("SELECT", "INSERT"),
        c("SELECT", "INSERT"),
        c("SELECT", "INSERT"),
        "SELECT",
        "SELECT",
        "SELECT",
        "SELECT",
        "SELECT"
      )
    ),
    deploy_recover = req(
      c(
        "public.locations_metadata_instruments",
        "public.locations",
        "public.sub_locations",
        "public.locations_z",
        "instruments.instruments",
        "instruments.instrument_makes",
        "instruments.instrument_models",
        "instruments.instrument_types",
        "public.locations_metadata_transmission_setups",
        "public.locations_metadata_transmission_routes",
        "public.locations_metadata_transmission_components",
        "continuous.transmission_timeseries_mappings",
        "continuous.transmission_import_runs"
      ),
      c(list(c("SELECT", "INSERT", "UPDATE")), rep(list("SELECT"), 12))
    ),
    addContData = req(
      "continuous.measurements_continuous",
      list(c("INSERT", "UPDATE"))
    ),
    imputeMissing = req("continuous.measurements_continuous"),
    continuousDataReview = req(
      c(
        "continuous.grades",
        "continuous.approvals",
        "continuous.qualifiers",
        "continuous.corrections"
      ),
      visibility = "any"
    ),
    addTimeseries = req(
      c(
        "continuous.timeseries",
        "public.locations_z",
        "public.organizations",
        "continuous.timeseries_source_adapters",
        "continuous.transmission_timeseries_mappings",
        "public.source_adapter_capabilities",
        "public.locations_metadata_instrument_timeseries",
        "continuous.corrections"
      ),
      list(
        c("INSERT", "UPDATE"),
        c("DELETE", "INSERT", "UPDATE"),
        "INSERT",
        c("DELETE", "INSERT", "UPDATE"),
        c("DELETE", "INSERT", "UPDATE"),
        "SELECT",
        c("DELETE", "INSERT"),
        "INSERT"
      )
    ),
    addCompoundTimeseries = req(
      c(
        "continuous.timeseries",
        "continuous.timeseries_compounds",
        "continuous.timeseries_compound_members",
        "public.locations_z"
      ),
      list(
        c("INSERT", "UPDATE"),
        c("INSERT", "UPDATE", "DELETE"),
        c("INSERT", "DELETE"),
        "INSERT"
      )
    ),
    syncCont = req(
      c(
        "continuous.measurements_continuous",
        "continuous.measurements_calculated_daily",
        "continuous.timeseries"
      ),
      list(default_write[[1]], default_write[[1]], "UPDATE")
    ),
    addDiscData = req(
      c("files.documents", "discrete.samples", "discrete.results"),
      list(c("INSERT", "UPDATE"), "INSERT", "INSERT")
    ),
    editSamples = req("discrete.samples", list(c("INSERT", "UPDATE"))),
    addSampleSeries = req(
      c(
        "discrete.sample_series",
        "public.organizations",
        "public.source_adapter_capabilities",
        "discrete.sample_series_source_adapters"
      ),
      list(
        c("SELECT", "INSERT", "UPDATE"),
        c("SELECT", "INSERT"),
        "SELECT",
        c("SELECT", "INSERT", "UPDATE")
      )
    ),
    syncDisc = req(
      c(
        "discrete.sample_series",
        "discrete.samples",
        "discrete.results"
      ),
      list("UPDATE", default_write[[1]], default_write[[1]])
    ),
    addGuidelines = req(
      c(
        "criteria.guidelines",
        "criteria.guideline_value_rules",
        "criteria.guideline_rule_inputs",
        "criteria.guideline_rule_coefficients",
        "criteria.guideline_narrative_values",
        "criteria.guidelines_fractions",
        "criteria.guidelines_media_types",
        "criteria.guideline_locations",
        "criteria.guideline_publishers",
        "criteria.guideline_series",
        "criteria.guideline_jurisdictions",
        "criteria.guideline_protection_goals",
        "criteria.guideline_exposure_durations",
        "criteria.guideline_averaging_periods"
      ),
      c(rep(default_write, 8), rep(list(c("INSERT", "UPDATE")), 6))
    ),
    addDocs = req(
      c("files.documents", "spatial.vectors"),
      list(c("INSERT", "UPDATE"), c("INSERT", "UPDATE"))
    ),
    addImgs = req("files.images", list(c("INSERT", "UPDATE"))),
    addImgSeries = req(
      c(
        "files.image_series",
        "public.organizations",
        "public.source_adapter_capabilities",
        "files.image_series_source_adapters"
      ),
      list(
        c("SELECT", "INSERT", "UPDATE"),
        c("SELECT", "INSERT", "UPDATE"),
        "SELECT",
        c("SELECT", "INSERT", "UPDATE")
      )
    ),
    boreholes_wells = req(c(
      "boreholes.boreholes",
      "boreholes.wells",
      "boreholes.drillers",
      "boreholes.borehole_well_purposes"
    )),
    simplerIndex = req(
      c(
        "boreholes.boreholes",
        "boreholes.wells",
        "boreholes.drillers",
        "boreholes.borehole_well_purposes",
        "public.approval_types"
      ),
      list(
        c("SELECT", "INSERT", "UPDATE"),
        c("SELECT", "INSERT", "UPDATE"),
        c("SELECT", "INSERT", "UPDATE"),
        c("SELECT", "INSERT", "UPDATE"),
        c("SELECT")
      )
    ),
    editBoreholesWells = req(c(
      "boreholes.boreholes",
      "boreholes.wells",
      "boreholes.drillers",
      "boreholes.borehole_well_purposes"
    )),
    manageBoreholeDocuments = req(c(
      "boreholes.boreholes",
      "boreholes.wells",
      "boreholes.drillers",
      "boreholes.borehole_well_purposes"
    )),
    manageOrganizations = req(
      "public.organizations",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageNetworks = req(
      c("public.networks", "public.network_project_types"),
      list(c("SELECT", "INSERT", "UPDATE"), "SELECT")
    ),
    manageProjects = req(
      c("public.projects", "public.network_project_types"),
      list(c("SELECT", "INSERT", "UPDATE"), "SELECT")
    ),
    manageNetworkProjectTypes = req(
      "public.network_project_types",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageLocationTypes = req(
      "public.location_types",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageMediaTypes = req(
      "public.media_types",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageMatrixStates = req(
      "public.matrix_states",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageParameterGroups = req(
      "public.parameter_groups",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageParameterSubGroups = req(
      "public.parameter_sub_groups",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageParameters = req(
      c(
        "public.parameters",
        "public.parameter_relationships",
        "public.parameter_groups",
        "public.parameter_sub_groups"
      ),
      list(
        c("SELECT", "INSERT", "UPDATE"),
        c("SELECT", "INSERT", "DELETE"),
        "SELECT",
        "SELECT"
      )
    ),
    manageCommunicationProtocolFamilies = req(
      "instruments.communication_protocol_families",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageCommunicationProtocols = req(
      c(
        "instruments.communication_protocols",
        "instruments.communication_protocol_families"
      ),
      list(c("SELECT", "INSERT", "UPDATE"), "SELECT")
    ),
    manageTransmissionMethodFamilies = req(
      "instruments.transmission_method_families",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageTransmissionMethods = req(
      c(
        "instruments.transmission_methods",
        "instruments.transmission_method_families"
      ),
      list(c("SELECT", "INSERT", "UPDATE"), "SELECT")
    ),
    manageTransmissionComponentRoles = req(
      "instruments.transmission_component_roles",
      list(c("SELECT", "INSERT", "UPDATE"))
    ),
    manageInstrumentConnections = req(
      c(
        "public.locations_metadata_instrument_connections",
        "public.locations_metadata_instruments",
        "public.locations",
        "instruments.instruments",
        "instruments.instrument_makes",
        "instruments.instrument_models",
        "instruments.instrument_types",
        "instruments.communication_protocols"
      ),
      c(list(c("SELECT", "INSERT", "UPDATE")), rep(list("SELECT"), 7))
    ),
    manageInstrumentConnectionSignals = req(
      c(
        "public.locations_metadata_instrument_connection_signals",
        "public.locations_metadata_instrument_connections",
        "public.locations_metadata_instruments",
        "instruments.instruments",
        "instruments.communication_protocols",
        "public.parameters",
        "continuous.timeseries",
        "public.locations",
        "public.media_types",
        "public.matrix_states",
        "public.units"
      ),
      c(list(c("SELECT", "INSERT", "UPDATE")), rep(list("SELECT"), 10))
    ),
    manageTransmissionSetups = req(
      c(
        "public.locations_metadata_transmission_setups",
        "public.locations_metadata_instruments",
        "instruments.instruments",
        "instruments.transmission_methods"
      ),
      c(list(c("SELECT", "INSERT", "UPDATE")), rep(list("SELECT"), 3))
    ),
    manageTransmissionRoutes = req(
      c(
        "public.locations_metadata_transmission_routes",
        "public.locations_metadata_transmission_setups",
        "public.locations_metadata_instruments",
        "instruments.instruments",
        "instruments.transmission_methods"
      ),
      c(list(c("SELECT", "INSERT", "UPDATE")), rep(list("SELECT"), 4))
    ),
    manageTransmissionTimeseriesMappings = req(
      c(
        "continuous.transmission_timeseries_mappings",
        "public.locations_metadata_transmission_routes",
        "public.locations_metadata_transmission_setups",
        "public.locations_metadata_instruments",
        "public.locations",
        "instruments.transmission_methods",
        "continuous.timeseries",
        "public.parameters",
        "public.matrix_states",
        "public.media_types"
      ),
      c(list(c("SELECT", "INSERT", "UPDATE")), rep(list("SELECT"), 9))
    ),
    viewTransmissionImportRuns = req(
      c(
        "continuous.transmission_import_runs",
        "public.locations_metadata_transmission_routes",
        "public.locations_metadata_transmission_setups",
        "public.locations_metadata_instruments",
        "public.locations",
        "instruments.transmission_methods"
      ),
      rep(list("SELECT"), 6)
    ),
    manageTransmissionComponents = req(
      c(
        "public.locations_metadata_transmission_components",
        "public.locations_metadata_transmission_setups",
        "public.locations_metadata_instruments",
        "public.locations",
        "instruments.instruments",
        "instruments.instrument_makes",
        "instruments.instrument_models",
        "instruments.instrument_types",
        "instruments.transmission_methods",
        "instruments.transmission_component_roles"
      ),
      c(list(c("SELECT", "INSERT", "UPDATE")), rep(list("SELECT"), 9))
    ),
    visit = req(
      c("field.field_visits", "field.field_visit_instruments"),
      list(c("INSERT", "UPDATE"), c("DELETE", "INSERT"))
    ),
    manageNewsContent = req(
      c(
        "application.images",
        "application.text",
        "application.page_content"
      )
    ),
    manageNotifications = req(
      "application.notifications",
      list(c("INSERT", "SELECT", "UPDATE"))
    ),
    viewFeedback = req(
      "application.feedback",
      list(c("SELECT", "INSERT", "UPDATE", "DELETE"))
    ),
    manageUsers = req(role_attributes = "CREATEROLE")
  )
}

module_table_privilege_status <- function(table_privs, requirement) {
  vapply(
    seq_along(requirement$tables),
    function(i) {
      granted <- table_privs$extra_privileges[
        table_privs$qual_name == requirement$tables[[i]]
      ]
      if (length(granted) == 0) {
        return(FALSE)
      }
      granted <- unique(unlist(strsplit(granted, ",\\s*")))
      all(requirement$privileges[[i]] %in% granted)
    },
    logical(1)
  )
}

ygwater_admin_privileges <- function(table_privs, requirements) {
  table_requirements <- requirements[vapply(
    requirements,
    function(x) length(x$tables) > 0,
    logical(1)
  )]

  lapply(table_requirements, function(requirement) {
    granted <- module_table_privilege_status(table_privs, requirement)
    if (requirement$visibility == "any") any(granted) else all(granted)
  })
}

module_access_status_query <- function(con, role, module_ids, requirements) {
  role <- as.character(role)[1]
  if (is.na(role) || !nzchar(role)) {
    stop("Please select a user or group.")
  }

  registered <- requirements[intersect(module_ids, names(requirements))]
  required_tables <- unique(unlist(lapply(registered, `[[`, "tables")))
  table_rows <- do.call(
    rbind,
    lapply(required_tables, function(table_name) {
      privileges <- unique(unlist(lapply(registered, function(requirement) {
        idx <- which(requirement$tables == table_name)
        unlist(requirement$privileges[idx], use.names = FALSE)
      })))
      data.frame(
        table = table_name,
        privilege = privileges,
        stringsAsFactors = FALSE
      )
    })
  )

  if (is.null(table_rows)) {
    table_rows <- data.frame(
      table = character(0),
      privilege = character(0),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(table_rows) > 0) {
    values_sql <- paste(
      vapply(
        seq_len(nrow(table_rows)),
        function(i) {
          sprintf(
            "(%s, %s)",
            DBI::dbQuoteString(con, table_rows$table[[i]]),
            DBI::dbQuoteString(con, table_rows$privilege[[i]])
          )
        },
        character(1)
      ),
      collapse = ", "
    )
    role_sql <- DBI::dbQuoteString(con, role)
    table_status <- DBI::dbGetQuery(
      con,
      sprintf(
        "WITH required(table_name, privilege) AS (VALUES %s)
SELECT table_name AS table,
       privilege,
       to_regclass(table_name) IS NOT NULL AS object_exists,
       COALESCE(
         has_table_privilege(%s, to_regclass(table_name), privilege),
         false
       ) AS granted
FROM required
ORDER BY table_name, privilege;",
        values_sql,
        role_sql
      )
    )
  } else {
    table_status <- data.frame(
      table = character(0),
      privilege = character(0),
      object_exists = logical(0),
      granted = logical(0),
      stringsAsFactors = FALSE
    )
  }

  schemas <- unique(sub("\\..*$", "", required_tables))
  if (length(schemas) > 0) {
    schema_sql <- paste(DBI::dbQuoteString(con, schemas), collapse = ", ")
    role_sql <- DBI::dbQuoteString(con, role)
    schema_status <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT schema_name,
       to_regnamespace(schema_name) IS NOT NULL AS object_exists,
       COALESCE(has_schema_privilege(%s, schema_name, 'USAGE'), false) AS granted
FROM unnest(ARRAY[%s]) AS schema_name
ORDER BY schema_name;",
        role_sql,
        schema_sql
      )
    )
  } else {
    schema_status <- data.frame(
      schema_name = character(0),
      object_exists = logical(0),
      granted = logical(0),
      stringsAsFactors = FALSE
    )
  }

  role_status <- DBI::dbGetQuery(
    con,
    "SELECT rolcreaterole FROM pg_catalog.pg_roles WHERE rolname = $1",
    params = list(role)
  )
  if (nrow(role_status) == 0) {
    stop(sprintf("Role '%s' no longer exists.", role))
  }

  module_access_summary(
    module_ids,
    requirements,
    table_status,
    schema_status,
    role_attributes = c(CREATEROLE = isTRUE(role_status$rolcreaterole[[1]]))
  )
}

module_access_summary <- function(
  module_ids,
  requirements,
  table_status,
  schema_status,
  role_attributes = logical(0)
) {
  module_ids <- setdiff(unique(as.character(module_ids)), "all")

  rows <- lapply(module_ids, function(module_id) {
    requirement <- requirements[[module_id]]
    if (is.null(requirement)) {
      return(data.frame(
        module = module_id,
        visible = TRUE,
        `full functionality` = TRUE,
        `missing privileges` = "",
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }

    missing <- character(0)
    table_ok <- logical(length(requirement$tables))
    missing_by_table <- vector("list", length(requirement$tables))

    for (i in seq_along(requirement$tables)) {
      table_name <- requirement$tables[[i]]
      status <- table_status[table_status$table == table_name, , drop = FALSE]
      if (nrow(status) == 0 || !all(status$object_exists)) {
        table_ok[[i]] <- FALSE
        missing_by_table[[i]] <- sprintf("TABLE %s does not exist", table_name)
      } else {
        needed <- requirement$privileges[[i]]
        missing_privileges <- needed[
          !vapply(
            needed,
            function(privilege) {
              any(status$privilege == privilege & status$granted)
            },
            logical(1)
          )
        ]
        table_ok[[i]] <- length(missing_privileges) == 0
        missing_by_table[[i]] <- sprintf(
          "%s ON TABLE %s",
          missing_privileges,
          table_name
        )
      }
    }

    attributes_ok <- vapply(
      requirement$role_attributes,
      function(attribute) {
        isTRUE(role_attributes[[attribute]])
      },
      logical(1)
    )
    if (any(!attributes_ok)) {
      missing <- c(missing, requirement$role_attributes[!attributes_ok])
    }

    visible_tables <- if (length(table_ok) == 0) {
      TRUE
    } else if (requirement$visibility == "any") {
      any(table_ok)
    } else {
      all(table_ok)
    }
    visible <- visible_tables && all(attributes_ok)

    required_schemas <- unique(sub("\\..*$", "", requirement$tables))
    schema_ok <- vapply(
      required_schemas,
      function(schema_name) {
        status <- schema_status[
          schema_status$schema_name == schema_name,
          ,
          drop = FALSE
        ]
        nrow(status) > 0 && all(status$object_exists) && all(status$granted)
      },
      logical(1)
    )
    if (any(!schema_ok)) {
      missing <- c(
        missing,
        sprintf("USAGE ON SCHEMA %s", required_schemas[!schema_ok])
      )
    }
    missing <- c(missing, unlist(missing_by_table, use.names = FALSE))

    data.frame(
      module = module_id,
      visible = visible,
      `full functionality` = all(table_ok) &&
        all(schema_ok) &&
        all(attributes_ok),
      `missing privileges` = paste(unique(missing), collapse = "; "),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
