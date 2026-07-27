# UI and server code for managing discrete samples

editSamplesUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ",
        ns("accordion1")
      )),
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #E5F4F6;
        /* collapsed header */
        --bs-accordion-btn-bg:      #0097A9;
        /* expanded header */
        --bs-accordion-active-bg:   #0097A9;
      }
    ",
        ns("accordion2")
      )),
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #F2F7EC;
        /* collapsed header */
        --bs-accordion-btn-bg:      #83A95C;
        /* expanded header */
        --bs-accordion-active-bg:   #83A95C;
      }
    ",
        ns("accordion3")
      ))
    ),
    page_fluid(
      uiOutput(ns("banner")),
      uiOutput(ns("ui"))
    )
  )
}

editSamples <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "editSamples"
      )
    })

    moduleData <- reactiveValues()
    selected_sample_ids <- reactiveVal(integer())
    selected_result_id <- reactiveVal(NA_integer_)
    result_edit_mode <- reactiveVal("add")

    multi_editable_fields <- list(
      collection_method = list(
        label = "Collection method",
        column = "collection_method"
      ),
      sample_type = list(label = "Sample type", column = "sample_type"),
      linked_with = list(label = "Linked with", column = "linked_with"),
      sample_volume_ml = list(
        label = "Sample volume (mL)",
        column = "sample_volume_ml"
      ),
      purge_volume_l = list(
        label = "Purge volume (L)",
        column = "purge_volume_l"
      ),
      purge_time_min = list(
        label = "Purge time (minutes)",
        column = "purge_time_min"
      ),
      flow_rate_l_min = list(
        label = "Flow rate (L/min)",
        column = "flow_rate_l_min"
      ),
      wave_hgt_m = list(label = "Wave height (m)", column = "wave_hgt_m"),
      sample_grade = list(label = "Sample grade", column = "sample_grade"),
      sample_approval = list(
        label = "Sample approval",
        column = "sample_approval"
      ),
      sample_qualifier = list(
        label = "Sample qualifier",
        column = "sample_qualifier"
      ),
      owner = list(label = "Owner", column = "owner"),
      contributor = list(label = "Contributor", column = "contributor"),
      comissioning_org = list(
        label = "Comissioning organization",
        column = "comissioning_org"
      ),
      sampling_org = list(
        label = "Sampling organization",
        column = "sampling_org"
      ),
      documents = list(
        label = "Documents",
        column = "documents",
        cast = "::integer[]"
      ),
      share_with = list(
        label = "Share with",
        column = "share_with",
        cast = "::text[]"
      ),
      import_source = list(label = "Import source", column = "import_source"),
      no_update = list(
        label = "Lock sample from updates",
        column = "no_update"
      ),
      note = list(label = "Notes", column = "note")
    )
    multi_edit_field_choices <- stats::setNames(
      names(multi_editable_fields),
      vapply(multi_editable_fields, `[[`, character(1), "label")
    )

    named_choices <- function(values, labels) {
      values <- as.character(values)
      labels <- as.character(labels)
      if (!length(values)) {
        return(character())
      }
      if (length(labels) != length(values)) {
        labels <- values
      }
      stats::setNames(values, labels)
    }

    format_datetime_input <- function(value) {
      if (is.null(value) || !length(value)) {
        return("")
      }
      if (all(is.na(value))) {
        return("")
      }
      value <- as.POSIXct(value, tz = "UTC")
      if (any(!is.na(value))) {
        value <- format(value, "%Y-%m-%d %H:%M")
      } else {
        value <- ""
      }
      if (length(value) == 1) {
        value[[1]]
      } else {
        value
      }
    }

    shift_sample_datetime_inputs <- function(tz_name) {
      shift_air_datetime_input_timezone(
        session,
        input,
        "datetime",
        tz_name
      )
      shift_air_datetime_input_timezone(
        session,
        input,
        "target_datetime",
        tz_name
      )
    }

    normalize_document_ids <- function(values) {
      if (is.null(values) || !length(values)) {
        return(integer())
      }
      if (is.list(values)) {
        values <- values[[1]]
      }
      if (is.character(values) && any(grepl("[{}]", values))) {
        values <- gsub("[{}]", "", values)
        values <- trimws(unlist(strsplit(values, ",")))
      }
      values <- as.integer(values)
      values <- values[!is.na(values)]
      unique(values)
    }

    format_integer_array <- function(values) {
      values <- normalize_document_ids(values)
      if (!length(values)) {
        return(NA)
      }
      paste0("{", paste(values, collapse = ","), "}")
    }

    parse_integer_array <- function(value) {
      normalize_document_ids(value)
    }

    sample_documents_table_exists <- function(con) {
      res <- DBI::dbGetQuery(
        con,
        "SELECT to_regclass('discrete.sample_documents') IS NOT NULL AS exists;"
      )
      isTRUE(res$exists[[1]])
    }

    current_sample_document_ids <- function(sample_id) {
      details <- moduleData$samples[
        moduleData$samples$sample_id == sample_id,
        ,
        drop = FALSE
      ]
      legacy_ids <- if (nrow(details)) {
        parse_integer_array(details$documents)
      } else {
        integer()
      }

      if (
        isTRUE(moduleData$has_sample_documents) &&
          !is.null(moduleData$sample_documents) &&
          nrow(moduleData$sample_documents)
      ) {
        linked_ids <- moduleData$sample_documents$document_id[
          moduleData$sample_documents$sample_id == sample_id
        ]
        return(unique(c(legacy_ids, as.integer(linked_ids))))
      }

      legacy_ids
    }

    sync_sample_document_links <- function(con, sample_id, document_ids) {
      document_ids <- normalize_document_ids(document_ids)
      DBI::dbExecute(
        con,
        "UPDATE discrete.samples
         SET documents = $1::integer[]
         WHERE sample_id = $2;",
        params = list(format_integer_array(document_ids), as.integer(sample_id))
      )

      if (!sample_documents_table_exists(con)) {
        return(invisible(NULL))
      }

      DBI::dbExecute(
        con,
        "DELETE FROM discrete.sample_documents WHERE sample_id = $1;",
        params = list(as.integer(sample_id))
      )

      for (document_id in document_ids) {
        DBI::dbExecute(
          con,
          "INSERT INTO discrete.sample_documents (
             sample_id,
             document_id,
             document_role,
             link_source
           ) VALUES ($1, $2, 'supporting', 'editSamples')
           ON CONFLICT (sample_id, document_id) DO NOTHING;",
          params = list(as.integer(sample_id), as.integer(document_id))
        )
      }

      invisible(NULL)
    }

    document_reference_summary <- function(con, document_id) {
      out <- data.frame(
        location = character(),
        count = integer(),
        stringsAsFactors = FALSE
      )

      fk_tables <- DBI::dbGetQuery(
        con,
        "SELECT ns.nspname AS schema_name,
                cls.relname AS table_name,
                att.attname AS column_name
         FROM pg_catalog.pg_constraint con
         JOIN pg_catalog.pg_class cls ON cls.oid = con.conrelid
         JOIN pg_catalog.pg_namespace ns ON ns.oid = cls.relnamespace
         JOIN unnest(con.conkey) WITH ORDINALITY AS cols(attnum, ord) ON TRUE
         JOIN pg_catalog.pg_attribute att
           ON att.attrelid = con.conrelid
          AND att.attnum = cols.attnum
         WHERE con.contype = 'f'
           AND con.confrelid = 'files.documents'::regclass;"
      )

      if (nrow(fk_tables)) {
        for (i in seq_len(nrow(fk_tables))) {
          q <- paste0(
            "SELECT count(*)::integer AS n FROM ",
            DBI::dbQuoteIdentifier(con, fk_tables$schema_name[i]),
            ".",
            DBI::dbQuoteIdentifier(con, fk_tables$table_name[i]),
            " WHERE ",
            DBI::dbQuoteIdentifier(con, fk_tables$column_name[i]),
            " = $1;"
          )
          n <- DBI::dbGetQuery(
            con,
            q,
            params = list(as.integer(document_id))
          )$n[[1]]
          if (n > 0L) {
            out <- rbind(
              out,
              data.frame(
                location = paste(
                  fk_tables$schema_name[i],
                  fk_tables$table_name[i],
                  sep = "."
                ),
                count = as.integer(n),
                stringsAsFactors = FALSE
              )
            )
          }
        }
      }

      arr_cols <- DBI::dbGetQuery(
        con,
        "SELECT table_schema, table_name, column_name
         FROM information_schema.columns
         WHERE udt_name = '_int4'
           AND column_name = 'documents';"
      )
      if (nrow(arr_cols)) {
        for (i in seq_len(nrow(arr_cols))) {
          q <- paste0(
            "SELECT count(*)::integer AS n FROM ",
            DBI::dbQuoteIdentifier(con, arr_cols$table_schema[i]),
            ".",
            DBI::dbQuoteIdentifier(con, arr_cols$table_name[i]),
            " WHERE $1 = ANY(",
            DBI::dbQuoteIdentifier(con, arr_cols$column_name[i]),
            ");"
          )
          n <- DBI::dbGetQuery(
            con,
            q,
            params = list(as.integer(document_id))
          )$n[[1]]
          if (n > 0L) {
            out <- rbind(
              out,
              data.frame(
                location = paste(
                  arr_cols$table_schema[i],
                  arr_cols$table_name[i],
                  sep = "."
                ),
                count = as.integer(n),
                stringsAsFactors = FALSE
              )
            )
          }
        }
      }

      if (!nrow(out)) {
        return(out)
      }
      stats::aggregate(count ~ location, out, sum)
    }

    cleanup_removed_documents <- function(con, document_ids) {
      document_ids <- normalize_document_ids(document_ids)
      messages <- character()

      for (document_id in document_ids) {
        refs <- document_reference_summary(con, document_id)
        if (!nrow(refs)) {
          DBI::dbExecute(
            con,
            "DELETE FROM files.documents WHERE document_id = $1;",
            params = list(as.integer(document_id))
          )
          messages <- c(
            messages,
            sprintf("Document %s was unlinked and deleted.", document_id)
          )
        } else {
          uses <- paste(
            sprintf("%s (%s)", refs$location, refs$count),
            collapse = ", "
          )
          messages <- c(
            messages,
            sprintf(
              "Document %s was unlinked but retained because it is still used by: %s.",
              document_id,
              uses
            )
          )
        }
      }

      messages
    }

    show_document_cleanup_messages <- function(messages) {
      for (message in messages) {
        showNotification(
          message,
          type = if (grepl("retained", message, fixed = TRUE)) {
            "warning"
          } else {
            "message"
          },
          duration = 15
        )
      }
    }

    find_existing_document <- function(con, path) {
      document <- readBin(path, "raw", file.info(path)$size)
      DBI::dbGetQuery(
        con,
        "SELECT document_id, name
         FROM files.documents
         WHERE file_hash = md5(encode($1::bytea, 'hex'))
         LIMIT 1;",
        params = list(list(document))
      )
    }

    insert_or_find_document <- function(con, file, sample_id, share_with) {
      existing <- find_existing_document(con, file$datapath)
      if (nrow(existing)) {
        return(as.integer(existing$document_id[[1]]))
      }

      document_type <- input$new_document_type
      if (
        !length(document_type) || is.na(document_type) || !nzchar(document_type)
      ) {
        stop("Select a document type before uploading documents.")
      }

      result <- AquaCache::insertACDocument(
        path = file$datapath,
        name = file$name,
        type = document_type,
        description = sprintf(
          "Uploaded %s for discrete sample %s from the sample editor.",
          file$name,
          sample_id
        ),
        tags = c("discrete sample", paste0("sample_id:", sample_id)),
        share_with = share_with,
        geoms = NULL,
        con = con
      )

      as.integer(result$new_document_id)
    }

    collect_sample_inputs <- function() {
      location_id <- if (length(input$location)) {
        as.integer(input$location[[1]])
      } else {
        NA_integer_
      }
      sub_location_id <- if (length(input$sub_location)) {
        as.integer(input$sub_location[[1]])
      } else {
        NA_integer_
      }
      media_id <- if (length(input$media)) {
        as.integer(input$media[[1]])
      } else {
        NA_integer_
      }
      collection_method <- if (length(input$collection_method)) {
        as.integer(input$collection_method[[1]])
      } else {
        NA_integer_
      }
      sample_type <- if (length(input$sample_type)) {
        as.integer(input$sample_type[[1]])
      } else {
        NA_integer_
      }
      linked_with <- if (length(input$linked_with)) {
        as.integer(input$linked_with[[1]])
      } else {
        NA_integer_
      }
      owner <- if (length(input$owner)) {
        as.integer(input$owner[[1]])
      } else {
        NA_integer_
      }
      contributor <- if (length(input$contributor)) {
        as.integer(input$contributor[[1]])
      } else {
        NA_integer_
      }
      comissioning_org <- if (length(input$comissioning_org)) {
        as.integer(input$comissioning_org[[1]])
      } else {
        NA_integer_
      }
      sampling_org <- if (length(input$sampling_org)) {
        as.integer(input$sampling_org[[1]])
      } else {
        NA_integer_
      }
      sample_grade <- if (length(input$sample_grade)) {
        as.integer(input$sample_grade[[1]])
      } else {
        NA_integer_
      }
      sample_approval <- if (length(input$sample_approval)) {
        as.integer(input$sample_approval[[1]])
      } else {
        NA_integer_
      }
      sample_qualifier <- if (length(input$sample_qualifier)) {
        as.integer(input$sample_qualifier[[1]])
      } else {
        NA_integer_
      }

      document_ids <- normalize_document_ids(input$documents)

      list(
        location_id = location_id,
        sub_location_id = sub_location_id,
        media_id = media_id,
        z = if (!length(input$z) || is.na(input$z)) {
          NA_real_
        } else {
          as.numeric(input$z)
        },
        datetime = scalar_utc_datetime(input$datetime),
        target_datetime = scalar_utc_datetime(input$target_datetime),
        collection_method = collection_method,
        sample_type = sample_type,
        linked_with = linked_with,
        sample_volume_ml = if (
          !length(input$sample_volume_ml) || is.na(input$sample_volume_ml)
        ) {
          NA_real_
        } else {
          as.numeric(input$sample_volume_ml)
        },
        purge_volume_l = if (
          !length(input$purge_volume_l) || is.na(input$purge_volume_l)
        ) {
          NA_real_
        } else {
          as.numeric(input$purge_volume_l)
        },
        purge_time_min = if (
          !length(input$purge_time_min) || is.na(input$purge_time_min)
        ) {
          NA_real_
        } else {
          as.numeric(input$purge_time_min)
        },
        flow_rate_l_min = if (
          !length(input$flow_rate_l_min) || is.na(input$flow_rate_l_min)
        ) {
          NA_real_
        } else {
          as.numeric(input$flow_rate_l_min)
        },
        wave_hgt_m = if (!length(input$wave_hgt_m) || is.na(input$wave_hgt_m)) {
          NA_real_
        } else {
          as.numeric(input$wave_hgt_m)
        },
        sample_grade = sample_grade,
        sample_approval = sample_approval,
        sample_qualifier = sample_qualifier,
        owner = owner,
        contributor = contributor,
        comissioning_org = comissioning_org,
        sampling_org = sampling_org,
        document_ids = document_ids,
        documents = format_integer_array(document_ids),
        share_with = share_with_to_array(input$share_with),
        import_source = if (isTruthy(input$import_source)) {
          input$import_source
        } else {
          NA_character_
        },
        no_update = isTRUE(input$no_update),
        note = if (isTruthy(input$note)) input$note else NA_character_,
        import_source_id = if (isTruthy(input$import_source_id)) {
          input$import_source_id
        } else {
          NA_character_
        }
      )
    }

    integer_input <- function(input_id) {
      value <- input[[input_id]]
      if (!length(value) || is.na(value[[1]]) || !nzchar(value[[1]])) {
        return(NA_integer_)
      }
      as.integer(value[[1]])
    }

    numeric_input <- function(input_id) {
      value <- input[[input_id]]
      if (!length(value) || is.na(value)) {
        return(NA_real_)
      }
      as.numeric(value)
    }

    date_input <- function(input_id) {
      value <- input[[input_id]]
      if (!length(value) || is.na(value)) {
        return(NA)
      }
      value
    }

    collect_result_inputs <- function(sample_id) {
      result_entry_type <- input$result_entry_type
      if (!identical(result_entry_type, "conditional")) {
        result_entry_type <- "exact"
      }
      list(
        sample_id = sample_id,
        result_entry_type = result_entry_type,
        result_type = integer_input("result_type"),
        parameter_id = integer_input("result_parameter"),
        sample_fraction_id = integer_input("result_sample_fraction"),
        result = if (identical(result_entry_type, "exact")) {
          numeric_input("result_value")
        } else {
          NA_real_
        },
        result_condition = if (identical(result_entry_type, "conditional")) {
          integer_input("result_condition")
        } else {
          NA_integer_
        },
        result_condition_value = if (
          identical(result_entry_type, "conditional")
        ) {
          numeric_input("result_condition_value")
        } else {
          NA_real_
        },
        result_value_type = integer_input("result_value_type"),
        result_speciation_id = integer_input("result_speciation"),
        protocol_method = integer_input("result_protocol_method"),
        laboratory = integer_input("result_laboratory"),
        analysis_datetime = scalar_utc_datetime(input$result_analysis_datetime),
        share_with = share_with_to_array(input$result_share_with),
        no_update = isTRUE(input$result_no_update),
        private_expiry = date_input("result_private_expiry"),
        matrix_state_id = integer_input("result_matrix_state"),
        note = if (isTruthy(input$result_note)) {
          input$result_note
        } else {
          NA_character_
        }
      )
    }

    result_lookup_choices <- function(data, id_col, label_col) {
      if (is.null(data) || !nrow(data)) {
        return(character())
      }
      named_choices(data[[id_col]], data[[label_col]])
    }

    result_parameter_choices <- function() {
      params <- moduleData$parameters
      if (is.null(params) || !nrow(params)) {
        return(character())
      }
      labels <- params$param_name
      has_units <- !is.na(params$unit_default) & nzchar(params$unit_default)
      labels[has_units] <- paste0(
        labels[has_units],
        " (",
        params$unit_default[has_units],
        ")"
      )
      named_choices(params$parameter_id, labels)
    }

    selected_parameter_row <- function(parameter_id) {
      parameter_id <- suppressWarnings(as.integer(parameter_id))
      if (
        !length(parameter_id) ||
          is.na(parameter_id[[1]]) ||
          is.null(moduleData$parameters) ||
          !nrow(moduleData$parameters)
      ) {
        return(NULL)
      }
      parameter_id <- parameter_id[[1]]
      row <- moduleData$parameters[
        moduleData$parameters$parameter_id == parameter_id,
        ,
        drop = FALSE
      ]
      if (nrow(row)) row[1, , drop = FALSE] else NULL
    }

    parameter_requires_sample_fraction <- function(parameter_id) {
      row <- selected_parameter_row(parameter_id)
      if (is.null(row) || !"sample_fraction" %in% names(row)) {
        return(FALSE)
      }
      isTRUE(row$sample_fraction[[1]])
    }

    parameter_requires_speciation <- function(parameter_id) {
      row <- selected_parameter_row(parameter_id)
      if (is.null(row) || !"result_speciation" %in% names(row)) {
        return(FALSE)
      }
      isTRUE(row$result_speciation[[1]])
    }

    supported_matrix_state_ids <- function(parameter_id) {
      row <- selected_parameter_row(parameter_id)
      if (
        is.null(row) ||
          is.null(moduleData$matrix_states) ||
          !nrow(moduleData$matrix_states)
      ) {
        return(integer())
      }
      supported <- vapply(
        seq_len(nrow(moduleData$matrix_states)),
        function(i) {
          unit_col <- paste0(
            "units_",
            moduleData$matrix_states$matrix_state_code[[i]]
          )
          unit_col %in%
            names(row) &&
            !is.na(row[[unit_col]][[1]]) &&
            nzchar(row[[unit_col]][[1]])
        },
        logical(1)
      )
      as.integer(moduleData$matrix_states$matrix_state_id[supported])
    }

    matrix_state_choices_for_parameter <- function(parameter_id) {
      available_ids <- supported_matrix_state_ids(parameter_id)
      rows <- moduleData$matrix_states[
        moduleData$matrix_states$matrix_state_id %in% available_ids,
        ,
        drop = FALSE
      ]
      rows <- rows[order(rows$matrix_state_name), , drop = FALSE]
      named_choices(rows$matrix_state_id, rows$matrix_state_name)
    }

    update_result_parameter_dependents <- function(
      parameter_id = integer_input("result_parameter"),
      selected_matrix_state = integer_input("result_matrix_state"),
      selected_fraction = integer_input("result_sample_fraction"),
      selected_speciation = integer_input("result_speciation")
    ) {
      matrix_choices <- matrix_state_choices_for_parameter(parameter_id)
      if (
        is.na(selected_matrix_state) ||
          !(as.character(selected_matrix_state) %in% unname(matrix_choices))
      ) {
        selected_matrix_state <- character(0)
      } else {
        selected_matrix_state <- as.character(selected_matrix_state)
      }

      updateSelectizeInput(
        session,
        "result_matrix_state",
        choices = matrix_choices,
        selected = selected_matrix_state,
        options = list(maxItems = 1, placeholder = "Select matrix state")
      )
      updateSelectizeInput(
        session,
        "result_sample_fraction",
        choices = result_lookup_choices(
          moduleData$sample_fractions,
          "sample_fraction_id",
          "sample_fraction"
        ),
        selected = if (is.na(selected_fraction)) {
          character(0)
        } else {
          as.character(selected_fraction)
        },
        options = list(
          maxItems = 1,
          placeholder = if (parameter_requires_sample_fraction(parameter_id)) {
            "Required for selected parameter"
          } else {
            "Optional"
          }
        )
      )
      updateSelectizeInput(
        session,
        "result_speciation",
        choices = result_lookup_choices(
          moduleData$result_speciations,
          "result_speciation_id",
          "result_speciation"
        ),
        selected = if (is.na(selected_speciation)) {
          character(0)
        } else {
          as.character(selected_speciation)
        },
        options = list(
          maxItems = 1,
          placeholder = if (parameter_requires_speciation(parameter_id)) {
            "Required for selected parameter"
          } else {
            "Optional"
          }
        )
      )
    }

    selected_result_row <- function() {
      result_id <- selected_result_id()
      results <- moduleData$sample_results
      if (
        is.na(result_id) ||
          is.null(results) ||
          !nrow(results) ||
          !(result_id %in% results$result_id)
      ) {
        return(NULL)
      }
      results[results$result_id == result_id, , drop = FALSE][1, ]
    }

    load_sample_results <- function(sample_id) {
      if (!length(sample_id) || is.na(sample_id)) {
        moduleData$sample_results <- data.frame()
        selected_result_id(NA_integer_)
        return(invisible(NULL))
      }

      moduleData$sample_results <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT
          r.result_id,
          r.sample_id,
          r.result_type,
          rt.result_type AS result_type_name,
          r.parameter_id,
          p.param_name AS parameter,
          public.get_parameter_unit_name(
            r.parameter_id,
            r.matrix_state_id
          ) AS units,
          r.sample_fraction_id,
          sf.sample_fraction,
          r.result,
          r.result_condition,
          rc.result_condition AS result_condition_name,
          r.result_condition_value,
          r.result_value_type,
          rvt.result_value_type AS result_value_type_name,
          r.result_speciation_id,
          rs.result_speciation,
          r.protocol_method,
          pm.protocol_name,
          r.laboratory,
          lab.lab_name AS laboratory_name,
          r.analysis_datetime,
          r.share_with,
          r.no_update,
          r.private_expiry,
          r.matrix_state_id,
          ms.matrix_state_name,
          r.note
        FROM discrete.results r
        JOIN public.parameters p
          ON r.parameter_id = p.parameter_id
        JOIN discrete.result_types rt
          ON r.result_type = rt.result_type_id
        LEFT JOIN discrete.sample_fractions sf
          ON r.sample_fraction_id = sf.sample_fraction_id
        LEFT JOIN discrete.result_conditions rc
          ON r.result_condition = rc.result_condition_id
        LEFT JOIN discrete.result_value_types rvt
          ON r.result_value_type = rvt.result_value_type_id
        LEFT JOIN discrete.result_speciations rs
          ON r.result_speciation_id = rs.result_speciation_id
        LEFT JOIN discrete.protocols_methods pm
          ON r.protocol_method = pm.protocol_id
        LEFT JOIN discrete.laboratories lab
          ON r.laboratory = lab.lab_id
        JOIN public.matrix_states ms
          ON r.matrix_state_id = ms.matrix_state_id
        WHERE r.sample_id = $1
        ORDER BY p.param_name, r.result_id;
        ",
        params = list(sample_id)
      )
      selected_result_id(NA_integer_)
      invisible(NULL)
    }

    show_result_modal <- function(mode = c("add", "edit")) {
      mode <- match.arg(mode)
      result_edit_mode(mode)
      sample_ids <- selected_sample_ids()
      if (length(sample_ids) != 1 || isTRUE(input$multi_edit)) {
        showNotification(
          "Select one sample before editing results.",
          type = "error"
        )
        return()
      }

      row <- if (identical(mode, "edit")) selected_result_row() else NULL
      if (identical(mode, "edit") && is.null(row)) {
        showNotification("Select one result to edit.", type = "error")
        return()
      }

      selected_value <- function(column, default = character(0)) {
        if (is.null(row) || is.na(row[[column]])) {
          default
        } else {
          as.character(row[[column]])
        }
      }
      numeric_value <- function(column) {
        if (is.null(row) || is.na(row[[column]])) NA else row[[column]]
      }
      first_lookup_id <- function(data, id_col) {
        if (is.null(data) || !nrow(data)) {
          character(0)
        } else {
          as.character(data[[id_col]][[1]])
        }
      }
      selected_parameter <- selected_value("parameter_id")
      parameter_choices <- result_parameter_choices()
      result_entry_type <- if (
        !is.null(row) &&
          (!is.na(row$result_condition) ||
            !is.na(row$result_condition_value))
      ) {
        "conditional"
      } else {
        "exact"
      }
      selected_matrix_state <- selected_value("matrix_state_id")
      matrix_choices <- matrix_state_choices_for_parameter(selected_parameter)

      showModal(modalDialog(
        title = if (identical(mode, "add")) "Add result" else "Edit result",
        size = "l",
        easyClose = FALSE,
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("result_parameter"),
              "Parameter",
              choices = NULL,
              selected = selected_parameter,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select parameter")
            )
          ),
          column(
            3,
            selectizeInput(
              ns("result_type"),
              "Result type",
              choices = result_lookup_choices(
                moduleData$result_types,
                "result_type_id",
                "result_type"
              ),
              selected = selected_value(
                "result_type",
                first_lookup_id(moduleData$result_types, "result_type_id")
              ),
              multiple = TRUE,
              options = list(maxItems = 1)
            )
          ),
          column(
            3,
            selectizeInput(
              ns("result_matrix_state"),
              tagList(
                "Matrix state ",
                tooltip(
                  trigger = list(
                    tags$span(
                      "Why can't I see other states?",
                      style = paste(
                        "font-weight: normal;",
                        "font-size: 85%;",
                        "margin-left: 4px;"
                      )
                    ),
                    bsicons::bs_icon("info-circle-fill")
                  ),
                  paste(
                    "Matrix states are only visible if units have been",
                    "specified for this parameter in these states. Go to",
                    "Reference data -> Parameters to add units, then reload",
                    "this module."
                  )
                )
              ),
              choices = matrix_choices,
              selected = if (
                length(selected_matrix_state) &&
                  selected_matrix_state %in% unname(matrix_choices)
              ) {
                selected_matrix_state
              } else {
                character(0)
              },
              multiple = TRUE,
              options = list(maxItems = 1)
            )
          )
        ),
        radioButtons(
          ns("result_entry_type"),
          "Result entry",
          choices = c(
            "Exact result" = "exact",
            "Conditional result" = "conditional"
          ),
          selected = result_entry_type,
          inline = TRUE
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.result_entry_type == 'exact'",
            ns = ns,
            column(
              4,
              numericInput(
                ns("result_value"),
                "Result",
                value = numeric_value("result"),
                width = "100%"
              )
            )
          ),
          conditionalPanel(
            condition = "input.result_entry_type == 'conditional'",
            ns = ns,
            column(
              4,
              selectizeInput(
                ns("result_condition"),
                "Result condition",
                choices = result_lookup_choices(
                  moduleData$result_conditions,
                  "result_condition_id",
                  "result_condition"
                ),
                selected = selected_value("result_condition"),
                multiple = TRUE,
                options = list(maxItems = 1, placeholder = "Select condition")
              )
            )
          ),
          conditionalPanel(
            condition = "input.result_entry_type == 'conditional'",
            ns = ns,
            column(
              4,
              numericInput(
                ns("result_condition_value"),
                "Condition value",
                value = numeric_value("result_condition_value"),
                width = "100%"
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectizeInput(
              ns("result_sample_fraction"),
              "Sample fraction",
              choices = result_lookup_choices(
                moduleData$sample_fractions,
                "sample_fraction_id",
                "sample_fraction"
              ),
              selected = selected_value("sample_fraction_id"),
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = if (
                  parameter_requires_sample_fraction(selected_parameter)
                ) {
                  "Required for selected parameter"
                } else {
                  "Optional"
                }
              )
            )
          ),
          column(
            4,
            selectizeInput(
              ns("result_value_type"),
              "Value type",
              choices = result_lookup_choices(
                moduleData$result_value_types,
                "result_value_type_id",
                "result_value_type"
              ),
              selected = selected_value("result_value_type"),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional")
            )
          ),
          column(
            4,
            selectizeInput(
              ns("result_speciation"),
              "Speciation",
              choices = result_lookup_choices(
                moduleData$result_speciations,
                "result_speciation_id",
                "result_speciation"
              ),
              selected = selected_value("result_speciation_id"),
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = if (
                  parameter_requires_speciation(selected_parameter)
                ) {
                  "Required for selected parameter"
                } else {
                  "Optional"
                }
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectizeInput(
              ns("result_protocol_method"),
              "Protocol/method",
              choices = result_lookup_choices(
                moduleData$protocols_methods,
                "protocol_id",
                "protocol_name"
              ),
              selected = selected_value("protocol_method"),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional")
            )
          ),
          column(
            4,
            selectizeInput(
              ns("result_laboratory"),
              "Laboratory",
              choices = result_lookup_choices(
                moduleData$laboratories,
                "lab_id",
                "lab_name"
              ),
              selected = selected_value("laboratory"),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional")
            )
          ),
          column(
            4,
            shinyWidgets::airDatepickerInput(
              ns("result_analysis_datetime"),
              "Analysis datetime",
              value = if (
                is.null(row) ||
                  is.na(row$analysis_datetime)
              ) {
                NULL
              } else {
                coerce_utc_datetime(row$analysis_datetime)
              },
              range = FALSE,
              multiple = FALSE,
              timepicker = TRUE,
              update_on = "change",
              tz = air_datetime_widget_timezone(input$timezone),
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 15,
                timeFormat = "HH:mm"
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectizeInput(
              ns("result_share_with"),
              "Share with",
              choices = moduleData$result_share_groups$role_name,
              selected = if (is.null(row)) {
                "public_reader"
              } else {
                array_to_text(row$share_with)
              },
              multiple = TRUE,
              options = list(placeholder = "Select groups to share with")
            )
          ),
          column(
            4,
            dateInput(
              ns("result_private_expiry"),
              "Private expiry",
              value = if (
                is.null(row) ||
                  is.na(row$private_expiry)
              ) {
                NULL
              } else {
                row$private_expiry
              }
            )
          ),
          column(
            4,
            checkboxInput(
              ns("result_no_update"),
              "Lock result from updates",
              value = if (is.null(row)) FALSE else isTRUE(row$no_update)
            )
          )
        ),
        textAreaInput(
          ns("result_note"),
          "Notes",
          value = if (is.null(row) || is.na(row$note)) "" else row$note,
          rows = 3,
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          bslib::input_task_button(
            ns("save_result"),
            label = if (identical(mode, "add")) "Add result" else "Save result"
          )
        )
      ))
      session$onFlushed(
        function() {
          updateSelectizeInput(
            session,
            "result_parameter",
            choices = parameter_choices,
            selected = selected_parameter,
            server = TRUE
          )
        },
        once = TRUE
      )
    }

    observeEvent(
      input$result_parameter,
      {
        update_result_parameter_dependents(
          parameter_id = integer_input("result_parameter"),
          selected_matrix_state = integer_input("result_matrix_state"),
          selected_fraction = integer_input("result_sample_fraction"),
          selected_speciation = integer_input("result_speciation")
        )
      },
      ignoreInit = TRUE
    )

    reset_form <- function() {
      updateSelectizeInput(session, "location", selected = character(0))
      updateSelectizeInput(session, "sub_location", selected = character(0))
      updateSelectizeInput(session, "media", selected = character(0))
      updateNumericInput(session, "z", value = NA)
      shinyWidgets::updateAirDateInput(
        session,
        "datetime",
        clear = TRUE
      )
      shinyWidgets::updateAirDateInput(
        session,
        "target_datetime",
        clear = TRUE
      )
      updateSelectizeInput(
        session,
        "collection_method",
        selected = character(0)
      )
      updateSelectizeInput(session, "sample_type", selected = character(0))
      updateSelectizeInput(session, "linked_with", selected = character(0))
      updateNumericInput(session, "sample_volume_ml", value = NA)
      updateNumericInput(session, "purge_volume_l", value = NA)
      updateNumericInput(session, "purge_time_min", value = NA)
      updateNumericInput(session, "flow_rate_l_min", value = NA)
      updateNumericInput(session, "wave_hgt_m", value = NA)
      updateSelectizeInput(session, "sample_grade", selected = character(0))
      updateSelectizeInput(session, "sample_approval", selected = character(0))
      updateSelectizeInput(session, "sample_qualifier", selected = character(0))
      updateSelectizeInput(session, "owner", selected = character(0))
      updateSelectizeInput(session, "contributor", selected = character(0))
      updateSelectizeInput(session, "comissioning_org", selected = character(0))
      updateSelectizeInput(session, "sampling_org", selected = character(0))
      updateSelectizeInput(session, "documents", selected = character(0))
      updateSelectizeInput(session, "share_with", selected = "public_reader")
      updateTextInput(session, "import_source", value = "")
      updateTextInput(session, "import_source_id", value = "")
      updateTextAreaInput(session, "note", value = "")
      updateCheckboxInput(session, "no_update", value = FALSE)
      if (!is.null(input$multi_fields)) {
        updateCheckboxGroupInput(
          session,
          "multi_fields",
          selected = character(0)
        )
      }
    }

    update_form_from_sample <- function(sample_id) {
      details <- moduleData$samples[
        moduleData$samples$sample_id == sample_id,
        ,
        drop = FALSE
      ]
      if (!nrow(details)) {
        return()
      }
      details <- details[1, ]
      updateSelectizeInput(
        session,
        "location",
        selected = as.character(details$location_id)
      )
      updateSelectizeInput(
        session,
        "sub_location",
        selected = if (is.na(details$sub_location_id)) {
          character(0)
        } else {
          as.character(details$sub_location_id)
        }
      )
      updateSelectizeInput(
        session,
        "media",
        selected = as.character(details$media_id)
      )
      updateNumericInput(session, "z", value = details$z)
      shinyWidgets::updateAirDateInput(
        session,
        "datetime",
        value = coerce_utc_datetime(details$datetime),
        tz = air_datetime_widget_timezone(input$timezone)
      )
      if (is.na(details$target_datetime)) {
        shinyWidgets::updateAirDateInput(
          session,
          "target_datetime",
          clear = TRUE
        )
      } else {
        shinyWidgets::updateAirDateInput(
          session,
          "target_datetime",
          value = coerce_utc_datetime(details$target_datetime),
          tz = air_datetime_widget_timezone(input$timezone)
        )
      }
      updateSelectizeInput(
        session,
        "collection_method",
        selected = as.character(details$collection_method)
      )
      updateSelectizeInput(
        session,
        "sample_type",
        selected = as.character(details$sample_type)
      )
      updateSelectizeInput(
        session,
        "linked_with",
        selected = if (is.na(details$linked_with)) {
          character(0)
        } else {
          as.character(details$linked_with)
        }
      )
      updateNumericInput(
        session,
        "sample_volume_ml",
        value = details$sample_volume_ml
      )
      updateNumericInput(
        session,
        "purge_volume_l",
        value = details$purge_volume_l
      )
      updateNumericInput(
        session,
        "purge_time_min",
        value = details$purge_time_min
      )
      updateNumericInput(
        session,
        "flow_rate_l_min",
        value = details$flow_rate_l_min
      )
      updateNumericInput(session, "wave_hgt_m", value = details$wave_hgt_m)
      updateSelectizeInput(
        session,
        "sample_grade",
        selected = if (is.na(details$sample_grade)) {
          character(0)
        } else {
          as.character(details$sample_grade)
        }
      )
      updateSelectizeInput(
        session,
        "sample_approval",
        selected = if (is.na(details$sample_approval)) {
          character(0)
        } else {
          as.character(details$sample_approval)
        }
      )
      updateSelectizeInput(
        session,
        "sample_qualifier",
        selected = if (is.na(details$sample_qualifier)) {
          character(0)
        } else {
          as.character(details$sample_qualifier)
        }
      )
      updateSelectizeInput(
        session,
        "owner",
        selected = as.character(details$owner)
      )
      updateSelectizeInput(
        session,
        "contributor",
        selected = if (is.na(details$contributor)) {
          character(0)
        } else {
          as.character(details$contributor)
        }
      )
      updateSelectizeInput(
        session,
        "comissioning_org",
        selected = if (is.na(details$comissioning_org)) {
          character(0)
        } else {
          as.character(details$comissioning_org)
        }
      )
      updateSelectizeInput(
        session,
        "sampling_org",
        selected = if (is.na(details$sampling_org)) {
          character(0)
        } else {
          as.character(details$sampling_org)
        }
      )
      updateSelectizeInput(
        session,
        "documents",
        selected = as.character(current_sample_document_ids(sample_id))
      )
      updateSelectizeInput(
        session,
        "share_with",
        selected = array_to_text(details$share_with)
      )
      updateTextInput(
        session,
        "import_source",
        value = if (is.na(details$import_source)) "" else details$import_source
      )
      updateTextInput(
        session,
        "import_source_id",
        value = if (is.na(details$import_source_id)) {
          ""
        } else {
          details$import_source_id
        }
      )
      updateTextAreaInput(
        session,
        "note",
        value = if (is.na(details$note)) "" else details$note
      )
      updateCheckboxInput(
        session,
        "no_update",
        value = isTRUE(details$no_update)
      )
    }

    getModuleData <- function() {
      con <- session$userData$AquaCache
      moduleData$samples <- DBI::dbGetQuery(
        con,
        "SELECT sample_id, location_id, sub_location_id, media_id, z, datetime, target_datetime, collection_method, sample_type, linked_with, sample_volume_ml, purge_volume_l, purge_time_min, flow_rate_l_min, wave_hgt_m, sample_grade, sample_approval, sample_qualifier, owner, contributor, comissioning_org, sampling_org, share_with, import_source, no_update, note, import_source_id FROM discrete.samples ORDER BY datetime DESC"
      )
      moduleData$samples_display <- DBI::dbGetQuery(
        con,
        "SELECT s.sample_id, l.name AS location, COALESCE(sl.sub_location_name, '') AS sub_location, m.media_type, st.sample_type, cm.collection_method, s.datetime, s.target_datetime, o.name AS owner, c.name AS contributor, s.sample_volume_ml, s.purge_volume_l, s.share_with FROM discrete.samples s JOIN public.locations l ON s.location_id = l.location_id LEFT JOIN public.sub_locations sl ON s.sub_location_id = sl.sub_location_id JOIN public.media_types m ON s.media_id = m.media_id JOIN discrete.sample_types st ON s.sample_type = st.sample_type_id JOIN discrete.collection_methods cm ON s.collection_method = cm.collection_method_id LEFT JOIN public.organizations o ON s.owner = o.organization_id LEFT JOIN public.organizations c ON s.contributor = c.organization_id ORDER BY s.datetime DESC"
      )
      moduleData$locations <- DBI::dbGetQuery(
        con,
        "SELECT location_id, name FROM public.locations ORDER BY name ASC"
      )
      moduleData$sub_locations <- DBI::dbGetQuery(
        con,
        "SELECT sub_location_id, location_id, sub_location_name FROM public.sub_locations ORDER BY sub_location_name ASC"
      )
      moduleData$media <- DBI::dbGetQuery(
        con,
        "SELECT media_id, media_type FROM public.media_types ORDER BY media_type ASC"
      )
      moduleData$collection_methods <- DBI::dbGetQuery(
        con,
        "SELECT collection_method_id, collection_method FROM discrete.collection_methods ORDER BY collection_method ASC"
      )
      moduleData$sample_types <- DBI::dbGetQuery(
        con,
        "SELECT sample_type_id, sample_type FROM discrete.sample_types ORDER BY sample_type ASC"
      )
      moduleData$grades <- DBI::dbGetQuery(
        con,
        "SELECT grade_type_id, grade_type_description FROM public.grade_types ORDER BY grade_type_description ASC"
      )
      moduleData$approvals <- DBI::dbGetQuery(
        con,
        "SELECT approval_type_id, approval_type_description FROM public.approval_types ORDER BY approval_type_description ASC"
      )
      moduleData$qualifiers <- DBI::dbGetQuery(
        con,
        "SELECT qualifier_type_id, qualifier_type_description FROM public.qualifier_types ORDER BY qualifier_type_description ASC"
      )
      moduleData$organizations <- DBI::dbGetQuery(
        con,
        "SELECT organization_id, name FROM public.organizations ORDER BY name ASC"
      )
      moduleData$documents <- DBI::dbGetQuery(
        con,
        "SELECT document_id, name FROM files.documents ORDER BY name ASC"
      )
      moduleData$document_types <- DBI::dbGetQuery(
        con,
        "SELECT document_type_id, document_type_en
         FROM files.document_types
         ORDER BY document_type_en ASC"
      )
      moduleData$has_sample_documents <- sample_documents_table_exists(con)
      moduleData$sample_documents <- if (
        isTRUE(moduleData$has_sample_documents)
      ) {
        DBI::dbGetQuery(
          con,
          "SELECT sample_id, document_id
           FROM discrete.sample_documents
           ORDER BY sample_id, document_id"
        )
      } else {
        data.frame(
          sample_id = integer(),
          document_id = integer(),
          stringsAsFactors = FALSE
        )
      }
      moduleData$share_groups <- DBI::dbGetQuery(
        con,
        "SELECT * FROM public.get_shareable_principals_for('discrete.samples') ORDER BY role_name ASC;"
      )
      moduleData$result_share_groups <- DBI::dbGetQuery(
        con,
        "SELECT * FROM public.get_shareable_principals_for('discrete.results') ORDER BY role_name ASC;"
      )
      moduleData$parameters <- DBI::dbGetQuery(
        con,
        "
        SELECT
          p.parameter_id,
          p.param_name,
          p.result_speciation,
          p.sample_fraction,
          ul.unit_name AS units_liquid,
          us.unit_name AS units_solid,
          ug.unit_name AS units_gas,
          public.get_parameter_unit_name(p.parameter_id, NULL) AS unit_default
        FROM public.parameters p
        LEFT JOIN public.units ul
          ON p.units_liquid = ul.unit_id
        LEFT JOIN public.units us
          ON p.units_solid = us.unit_id
        LEFT JOIN public.units ug
          ON p.units_gas = ug.unit_id
        ORDER BY p.param_name ASC;
        "
      )
      moduleData$result_types <- DBI::dbGetQuery(
        con,
        "SELECT result_type_id, result_type FROM discrete.result_types ORDER BY result_type ASC"
      )
      moduleData$sample_fractions <- DBI::dbGetQuery(
        con,
        "SELECT sample_fraction_id, sample_fraction FROM discrete.sample_fractions ORDER BY sample_fraction ASC"
      )
      moduleData$result_conditions <- DBI::dbGetQuery(
        con,
        "SELECT result_condition_id, result_condition FROM discrete.result_conditions ORDER BY result_condition ASC"
      )
      moduleData$result_value_types <- DBI::dbGetQuery(
        con,
        "SELECT result_value_type_id, result_value_type FROM discrete.result_value_types ORDER BY result_value_type ASC"
      )
      moduleData$result_speciations <- DBI::dbGetQuery(
        con,
        "SELECT result_speciation_id, result_speciation FROM discrete.result_speciations ORDER BY result_speciation ASC"
      )
      moduleData$protocols_methods <- DBI::dbGetQuery(
        con,
        "SELECT protocol_id, protocol_name FROM discrete.protocols_methods ORDER BY protocol_name ASC"
      )
      moduleData$laboratories <- DBI::dbGetQuery(
        con,
        "SELECT lab_id, lab_name FROM discrete.laboratories ORDER BY lab_name ASC"
      )
      moduleData$matrix_states <- DBI::dbGetQuery(
        con,
        "SELECT matrix_state_id, matrix_state_code, matrix_state_name FROM public.matrix_states ORDER BY matrix_state_name ASC"
      )
    }

    getModuleData()

    observeEvent(input$reload_module, {
      getModuleData()
      selected_sample_ids(integer())
      selected_result_id(NA_integer_)
      moduleData$sample_results <- data.frame()
      reset_form()
      DT::dataTableProxy("sample_table") |> DT::selectRows(NULL)
    })

    observeEvent(
      input$timezone,
      {
        shift_sample_datetime_inputs(normalize_input_timezone(input$timezone))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$location,
      {
        req(moduleData$sub_locations)
        loc_id <- if (length(input$location)) {
          as.integer(input$location[[1]])
        } else {
          NA_integer_
        }
        if (is.na(loc_id)) {
          updateSelectizeInput(
            session,
            "sub_location",
            choices = named_choices(
              moduleData$sub_locations$sub_location_id,
              moduleData$sub_locations$sub_location_name
            )
          )
        } else {
          available <- moduleData$sub_locations[
            moduleData$sub_locations$location_id == loc_id,
          ]
          updateSelectizeInput(
            session,
            "sub_location",
            choices = named_choices(
              available$sub_location_id,
              available$sub_location_name
            )
          )
        }
      },
      ignoreNULL = FALSE
    )

    observeEvent(
      input$share_with,
      {
        if (
          length(input$share_with) > 1 && "public_reader" %in% input$share_with
        ) {
          updateSelectizeInput(
            session,
            "share_with",
            selected = "public_reader"
          )
        }
      },
      ignoreNULL = TRUE
    )

    output$ui <- renderUI({
      req(
        moduleData$locations,
        moduleData$media,
        moduleData$collection_methods,
        moduleData$sample_types,
        moduleData$organizations,
        moduleData$share_groups,
        moduleData$samples,
        moduleData$documents,
        moduleData$document_types,
        moduleData$grades,
        moduleData$approvals,
        moduleData$qualifiers,
        moduleData$result_share_groups,
        moduleData$parameters,
        moduleData$result_types,
        moduleData$sample_fractions,
        moduleData$result_conditions,
        moduleData$result_value_types,
        moduleData$result_speciations,
        moduleData$protocols_methods,
        moduleData$laboratories,
        moduleData$matrix_states
      )

      multi_field_ui <- function(field, ui) {
        conditionalPanel(
          condition = sprintf(
            "!input.multi_edit || (input.multi_fields || []).indexOf('%s') >= 0",
            field
          ),
          ns = ns,
          ui
        )
      }
      single_only_ui <- function(ui) {
        conditionalPanel(condition = "!input.multi_edit", ns = ns, ui)
      }

      tagList(
        actionButton(
          ns("reload_module"),
          "Reload module data",
          icon = icon("refresh")
        ),
        accordion(
          id = ns("accordion1"),
          open = "sample_table_panel",
          accordion_panel(
            title = "Select samples to modify",
            value = "sample_table_panel",
            checkboxInput(
              ns("multi_edit"),
              "Enable multi-sample edit",
              value = FALSE
            ) |>
              tooltip(
                "Allows selection of multiple samples to update fields simultaneously."
              ),
            DT::DTOutput(ns("sample_table"))
          )
        ),
        conditionalPanel(
          condition = "input.multi_edit",
          ns = ns,
          accordion(
            id = ns("accordion2"),
            open = "multi_edit_fields_panel",
            accordion_panel(
              title = "Multi-sample edit options",
              value = "multi_edit_fields_panel",
              tags$div(
                class = "alert alert-warning",
                "Only selected fields will be shown and updated for all chosen samples. Location, sub-location, elevation/depth, sample datetime, target datetime, and import source ID remain single-sample edits."
              ),
              checkboxGroupInput(
                ns("multi_fields"),
                "Fields to update across all selected samples",
                choices = multi_edit_field_choices,
                selected = character(0),
                inline = TRUE,
                width = "100%"
              )
            )
          )
        ),
        accordion(
          id = ns("accordion3"),
          open = "sample_metadata_panel",
          accordion_panel(
            title = "Sample metadata",
            value = "sample_metadata_panel",
            fluidRow(
              single_only_ui(
                column(
                  6,
                  selectizeInput(
                    ns("location"),
                    "Location",
                    choices = named_choices(
                      moduleData$locations$location_id,
                      moduleData$locations$name
                    ),
                    multiple = TRUE,
                    options = list(
                      maxItems = 1,
                      placeholder = "Select a location"
                    ),
                    width = "100%"
                  )
                )
              ),
              single_only_ui(
                column(
                  6,
                  selectizeInput(
                    ns("sub_location"),
                    "Sub-location",
                    choices = named_choices(
                      moduleData$sub_locations$sub_location_id,
                      moduleData$sub_locations$sub_location_name
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Optional"),
                    width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              single_only_ui(
                column(
                  6,
                  selectizeInput(
                    ns("media"),
                    "Media",
                    choices = named_choices(
                      moduleData$media$media_id,
                      moduleData$media$media_type
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Select media"),
                    width = "100%"
                  )
                )
              ),
              single_only_ui(
                column(
                  3,
                  numericInput(
                    ns("z"),
                    "Sample depth/elevation (m)",
                    value = NA,
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "no_update",
                column(
                  3,
                  checkboxInput(
                    ns("no_update"),
                    "Lock sample from updates",
                    value = FALSE
                  )
                )
              )
            ),
            fluidRow(
              single_only_ui(
                column(
                  3,
                  selectizeInput(
                    ns("timezone"),
                    "Input timezone",
                    choices = input_timezone_choices(),
                    selected = default_input_timezone(),
                    multiple = FALSE,
                    width = "100%"
                  )
                )
              ),
              single_only_ui(
                column(
                  4,
                  shinyWidgets::airDatepickerInput(
                    ns("datetime"),
                    "Sample datetime",
                    value = NULL,
                    range = FALSE,
                    multiple = FALSE,
                    timepicker = TRUE,
                    update_on = "change",
                    tz = air_datetime_widget_timezone(default_input_timezone()),
                    timepickerOpts = shinyWidgets::timepickerOptions(
                      minutesStep = 15,
                      timeFormat = "HH:mm"
                    )
                  )
                )
              ),
              single_only_ui(
                column(
                  5,
                  shinyWidgets::airDatepickerInput(
                    ns("target_datetime"),
                    "Target datetime (optional)",
                    value = NULL,
                    range = FALSE,
                    multiple = FALSE,
                    timepicker = TRUE,
                    update_on = "change",
                    tz = air_datetime_widget_timezone(default_input_timezone()),
                    timepickerOpts = shinyWidgets::timepickerOptions(
                      minutesStep = 15,
                      timeFormat = "HH:mm"
                    )
                  )
                )
              )
            ),
            fluidRow(
              multi_field_ui(
                "collection_method",
                column(
                  6,
                  selectizeInput(
                    ns("collection_method"),
                    "Collection method",
                    choices = named_choices(
                      moduleData$collection_methods$collection_method_id,
                      moduleData$collection_methods$collection_method
                    ),
                    multiple = TRUE,
                    options = list(
                      maxItems = 1,
                      placeholder = "Select collection method"
                    ),
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "sample_type",
                column(
                  6,
                  selectizeInput(
                    ns("sample_type"),
                    "Sample type",
                    choices = named_choices(
                      moduleData$sample_types$sample_type_id,
                      moduleData$sample_types$sample_type
                    ),
                    multiple = TRUE,
                    options = list(
                      maxItems = 1,
                      placeholder = "Select sample type"
                    ),
                    width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              multi_field_ui(
                "linked_with",
                column(
                  6,
                  selectizeInput(
                    ns("linked_with"),
                    "Linked sample (optional)",
                    choices = named_choices(
                      moduleData$samples$sample_id,
                      paste0(
                        moduleData$samples$sample_id,
                        " – ",
                        format(
                          as.POSIXct(moduleData$samples$datetime, tz = "UTC"),
                          "%Y-%m-%d %H:%M"
                        )
                      )
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Optional"),
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "documents",
                column(
                  6,
                  selectizeInput(
                    ns("documents"),
                    "Associated documents",
                    choices = named_choices(
                      moduleData$documents$document_id,
                      paste0(
                        moduleData$documents$name,
                        " (",
                        moduleData$documents$document_id,
                        ")"
                      )
                    ),
                    multiple = TRUE,
                    options = list(placeholder = "Optional"),
                    width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              single_only_ui(
                column(
                  5,
                  fileInput(
                    ns("new_documents"),
                    "Upload and link documents",
                    multiple = TRUE
                  )
                )
              ),
              single_only_ui(
                column(
                  4,
                  selectizeInput(
                    ns("new_document_type"),
                    "Uploaded document type",
                    choices = named_choices(
                      moduleData$document_types$document_type_en,
                      moduleData$document_types$document_type_en
                    ),
                    selected = if (!nrow(moduleData$document_types)) {
                      character(0)
                    } else if (
                      "report" %in% moduleData$document_types$document_type_en
                    ) {
                      "report"
                    } else {
                      moduleData$document_types$document_type_en[[1]]
                    },
                    multiple = FALSE,
                    width = "100%"
                  )
                )
              ),
              single_only_ui(
                column(
                  3,
                  br(),
                  actionButton(
                    ns("add_documents"),
                    "Upload/link"
                  )
                )
              )
            ),
            fluidRow(
              multi_field_ui(
                "sample_volume_ml",
                column(
                  3,
                  numericInput(
                    ns("sample_volume_ml"),
                    "Sample volume (mL)",
                    value = NA,
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "purge_volume_l",
                column(
                  3,
                  numericInput(
                    ns("purge_volume_l"),
                    "Purge volume (L)",
                    value = NA,
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "purge_time_min",
                column(
                  3,
                  numericInput(
                    ns("purge_time_min"),
                    "Purge time (min)",
                    value = NA,
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "flow_rate_l_min",
                column(
                  3,
                  numericInput(
                    ns("flow_rate_l_min"),
                    "Flow rate (L/min)",
                    value = NA,
                    width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              multi_field_ui(
                "wave_hgt_m",
                column(
                  4,
                  numericInput(
                    ns("wave_hgt_m"),
                    "Wave height (m)",
                    value = NA,
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "sample_grade",
                column(
                  4,
                  selectizeInput(
                    ns("sample_grade"),
                    "Sample grade",
                    choices = named_choices(
                      moduleData$grades$grade_type_id,
                      moduleData$grades$grade_type_description
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Optional"),
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "sample_approval",
                column(
                  4,
                  selectizeInput(
                    ns("sample_approval"),
                    "Sample approval",
                    choices = named_choices(
                      moduleData$approvals$approval_type_id,
                      moduleData$approvals$approval_type_description
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Optional"),
                    width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              multi_field_ui(
                "sample_qualifier",
                column(
                  6,
                  selectizeInput(
                    ns("sample_qualifier"),
                    "Sample qualifier",
                    choices = named_choices(
                      moduleData$qualifiers$qualifier_type_id,
                      moduleData$qualifiers$qualifier_type_description
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Optional"),
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "owner",
                column(
                  6,
                  selectizeInput(
                    ns("owner"),
                    "Owner",
                    choices = named_choices(
                      moduleData$organizations$organization_id,
                      moduleData$organizations$name
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Select owner"),
                    width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              multi_field_ui(
                "contributor",
                column(
                  6,
                  selectizeInput(
                    ns("contributor"),
                    "Contributor",
                    choices = named_choices(
                      moduleData$organizations$organization_id,
                      moduleData$organizations$name
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Optional"),
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "comissioning_org",
                column(
                  6,
                  selectizeInput(
                    ns("comissioning_org"),
                    "Comissioning organization",
                    choices = named_choices(
                      moduleData$organizations$organization_id,
                      moduleData$organizations$name
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Optional"),
                    width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              multi_field_ui(
                "sampling_org",
                column(
                  6,
                  selectizeInput(
                    ns("sampling_org"),
                    "Sampling organization",
                    choices = named_choices(
                      moduleData$organizations$organization_id,
                      moduleData$organizations$name
                    ),
                    multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Optional"),
                    width = "100%"
                  )
                )
              ),
              multi_field_ui(
                "share_with",
                column(
                  6,
                  selectizeInput(
                    ns("share_with"),
                    "Share with",
                    choices = moduleData$share_groups$role_name,
                    selected = "public_reader",
                    multiple = TRUE,
                    options = list(placeholder = "Select groups to share with")
                  )
                )
              )
            ),
            fluidRow(
              multi_field_ui(
                "import_source",
                column(
                  6,
                  textInput(
                    ns("import_source"),
                    "Import source",
                    placeholder = "Optional"
                  )
                )
              ),
              single_only_ui(
                column(
                  6,
                  textInput(
                    ns("import_source_id"),
                    "Import source ID",
                    placeholder = "Optional"
                  )
                )
              )
            ),
            multi_field_ui(
              "note",
              textAreaInput(
                ns("note"),
                "Notes",
                rows = 3,
                placeholder = "Optional",
                width = "100%"
              )
            ),
            bslib::input_task_button(
              ns("update_sample"),
              label = "Update sample metadata"
            )
          )
        ),
        conditionalPanel(
          condition = "!input.multi_edit",
          ns = ns,
          accordion(
            id = ns("accordion4"),
            open = FALSE,
            accordion_panel(
              title = "Sample results",
              value = "sample_results_panel",
              uiOutput(ns("results_status")),
              fluidRow(
                column(
                  6,
                  actionButton(
                    ns("add_result"),
                    "Add result",
                    icon = icon("plus")
                  )
                ),
                column(
                  6,
                  actionButton(
                    ns("edit_result"),
                    "Edit selected result",
                    icon = icon("edit")
                  )
                )
              ),
              DT::DTOutput(ns("results_table"))
            )
          )
        )
      )
    })

    output$sample_table <- DT::renderDT({
      req(moduleData$samples_display)
      display <- moduleData$samples_display
      display$datetime <- format(
        as.POSIXct(display$datetime, tz = "UTC"),
        "%Y-%m-%d %H:%M"
      )
      display$target_datetime <- format(
        as.POSIXct(display$target_datetime, tz = "UTC"),
        "%Y-%m-%d %H:%M"
      )
      display$share_with <- gsub("[{}]", "", display$share_with)
      # Make several columns 'factors' for better filtering in DT
      factor_cols <- c(
        "location",
        "sub_location",
        "media_type",
        "sample_type",
        "collection_method",
        "owner",
        "contributor"
      )
      for (col in factor_cols) {
        display[[col]] <- as.factor(display[[col]])
      }
      DT::datatable(
        display,
        selection = if (isTRUE(input$multi_edit)) {
          list(mode = "multiple", selected = NULL, target = "row")
        } else {
          list(mode = "single", selected = NULL, target = "row")
        },
        filter = "top",
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'background-color': '#079',",
            "  'color': '#fff',",
            "  'font-size': '100%',",
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '90%',",
            "});",
            "}"
          )
        )
      )
    })

    output$results_status <- renderUI({
      sample_ids <- selected_sample_ids()
      if (isTRUE(input$multi_edit)) {
        return(tags$div(
          class = "alert alert-info",
          "Result editing is available only when one sample is selected."
        ))
      }
      if (length(sample_ids) != 1) {
        return(tags$div(
          class = "alert alert-info",
          "Select one sample to view or edit results."
        ))
      }
      result_count <- if (is.null(moduleData$sample_results)) {
        0L
      } else {
        nrow(moduleData$sample_results)
      }
      tags$div(
        class = "alert alert-info",
        sprintf(
          "Sample %s has %d result%s.",
          sample_ids[[1]],
          result_count,
          if (identical(result_count, 1L)) "" else "s"
        )
      )
    })

    output$results_table <- DT::renderDT({
      results <- moduleData$sample_results
      if (is.null(results) || !nrow(results)) {
        results <- data.frame(
          result_id = integer(),
          parameter = character(),
          units = character(),
          result = numeric(),
          result_type = character(),
          matrix_state = character(),
          stringsAsFactors = FALSE
        )
      } else {
        results <- data.frame(
          result_id = results$result_id,
          parameter = results$parameter,
          units = results$units,
          result = results$result,
          result_type = results$result_type_name,
          condition = results$result_condition_name,
          value_type = results$result_value_type_name,
          fraction = results$sample_fraction,
          speciation = results$result_speciation,
          matrix_state = results$matrix_state_name,
          laboratory = results$laboratory_name,
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(
        results,
        selection = list(mode = "single", selected = NULL, target = "row"),
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    observeEvent(input$results_table_rows_selected, {
      idx <- input$results_table_rows_selected
      results <- moduleData$sample_results
      if (is.null(results) || !nrow(results) || !length(idx)) {
        selected_result_id(NA_integer_)
        return()
      }
      selected_result_id(results$result_id[idx[[1]]])
    })

    observeEvent(input$add_result, {
      selected_result_id(NA_integer_)
      show_result_modal("add")
    })

    observeEvent(input$edit_result, {
      show_result_modal("edit")
    })

    observeEvent(input$save_result, {
      sample_ids <- selected_sample_ids()
      if (length(sample_ids) != 1 || isTRUE(input$multi_edit)) {
        showNotification(
          "Select one sample before saving a result.",
          type = "error"
        )
        return()
      }

      form <- collect_result_inputs(sample_ids[[1]])
      if (is.na(form$parameter_id)) {
        showNotification("Parameter is required.", type = "error")
        return()
      }
      if (is.na(form$result_type)) {
        showNotification("Result type is required.", type = "error")
        return()
      }
      if (is.na(form$matrix_state_id)) {
        showNotification("Matrix state is required.", type = "error")
        return()
      }
      if (
        !(form$matrix_state_id %in%
          supported_matrix_state_ids(form$parameter_id))
      ) {
        showNotification(
          "The selected matrix state does not have units configured for this parameter.",
          type = "error"
        )
        return()
      }
      if (
        identical(form$result_entry_type, "exact") &&
          is.na(form$result)
      ) {
        showNotification(
          "Result is required for an exact result.",
          type = "error"
        )
        return()
      }
      if (
        identical(form$result_entry_type, "conditional") &&
          is.na(form$result_condition)
      ) {
        showNotification(
          "Result condition is required for a conditional result.",
          type = "error"
        )
        return()
      }
      if (
        parameter_requires_sample_fraction(form$parameter_id) &&
          is.na(form$sample_fraction_id)
      ) {
        showNotification(
          "Sample fraction is required for the selected parameter.",
          type = "error"
        )
        return()
      }
      if (
        parameter_requires_speciation(form$parameter_id) &&
          is.na(form$result_speciation_id)
      ) {
        showNotification(
          "Speciation is required for the selected parameter.",
          type = "error"
        )
        return()
      }

      params <- list(
        form$result_type,
        form$parameter_id,
        form$sample_fraction_id,
        form$result,
        form$result_condition,
        form$result_condition_value,
        form$result_value_type,
        form$result_speciation_id,
        form$protocol_method,
        form$laboratory,
        form$analysis_datetime,
        form$share_with,
        form$no_update,
        form$private_expiry,
        form$matrix_state_id,
        form$note
      )

      mode <- result_edit_mode()
      if (identical(mode, "add")) {
        sql <- "
          INSERT INTO discrete.results (
            sample_id,
            result_type,
            parameter_id,
            sample_fraction_id,
            result,
            result_condition,
            result_condition_value,
            result_value_type,
            result_speciation_id,
            protocol_method,
            laboratory,
            analysis_datetime,
            share_with,
            no_update,
            private_expiry,
            matrix_state_id,
            note
          ) VALUES (
            $17,
            $1,
            $2,
            $3,
            $4,
            $5,
            $6,
            $7,
            $8,
            $9,
            $10,
            $11,
            $12::text[],
            $13,
            $14,
            $15,
            $16
          ) RETURNING result_id;
        "
        params <- c(params, list(form$sample_id))
      } else {
        result_id <- selected_result_id()
        if (is.na(result_id)) {
          showNotification("Select one result to edit.", type = "error")
          return()
        }
        sql <- "
          UPDATE discrete.results
          SET
            result_type = $1,
            parameter_id = $2,
            sample_fraction_id = $3,
            result = $4,
            result_condition = $5,
            result_condition_value = $6,
            result_value_type = $7,
            result_speciation_id = $8,
            protocol_method = $9,
            laboratory = $10,
            analysis_datetime = $11,
            share_with = $12::text[],
            no_update = $13,
            private_expiry = $14,
            matrix_state_id = $15,
            note = $16
          WHERE result_id = $17
            AND sample_id = $18;
        "
        params <- c(params, list(result_id, form$sample_id))
      }

      tryCatch(
        {
          res <- if (identical(mode, "add")) {
            DBI::dbGetQuery(
              session$userData$AquaCache,
              sql,
              params = params
            )
          } else {
            DBI::dbExecute(
              session$userData$AquaCache,
              sql,
              params = params
            )
          }
          load_sample_results(form$sample_id)
          if (identical(mode, "add") && nrow(res)) {
            selected_result_id(res$result_id[[1]])
          }
          removeModal()
          showNotification(
            if (identical(mode, "add")) {
              "Result added successfully."
            } else {
              "Result updated successfully."
            },
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Failed to save result:", e$message),
            type = "error"
          )
        }
      )
    })

    observeEvent(input$sample_table_rows_selected, {
      idx <- input$sample_table_rows_selected
      multi_enabled <- isTRUE(input$multi_edit)
      if (!multi_enabled && length(idx) > 1) {
        idx <- idx[1]
        DT::dataTableProxy("sample_table") |> DT::selectRows(idx)
      }
      if (!length(idx)) {
        selected_sample_ids(integer())
        selected_result_id(NA_integer_)
        moduleData$sample_results <- data.frame()
        reset_form()
        return()
      }
      ids <- moduleData$samples_display$sample_id[idx]
      selected_sample_ids(ids)
      if (!multi_enabled && length(ids) == 1) {
        update_form_from_sample(ids)
        load_sample_results(ids)
      } else {
        selected_result_id(NA_integer_)
        moduleData$sample_results <- data.frame()
      }
    })

    observeEvent(input$multi_edit, {
      if (!isTRUE(input$multi_edit)) {
        ids <- selected_sample_ids()
        if (length(ids) > 1) {
          ids <- ids[1]
          selected_sample_ids(ids)
          row_idx <- match(ids, moduleData$samples_display$sample_id)
          DT::dataTableProxy("sample_table") |> DT::selectRows(row_idx)
          update_form_from_sample(ids)
          load_sample_results(ids)
        } else if (length(ids) == 1) {
          update_form_from_sample(ids)
          load_sample_results(ids)
        }
        if (!is.null(input$multi_fields)) {
          updateCheckboxGroupInput(
            session,
            "multi_fields",
            selected = character(0)
          )
        }
      } else {
        selected_result_id(NA_integer_)
        moduleData$sample_results <- data.frame()
      }
    })

    observeEvent(input$add_documents, {
      sample_ids <- selected_sample_ids()
      if (isTRUE(input$multi_edit) || length(sample_ids) != 1L) {
        showNotification(
          "Select one sample before uploading documents.",
          type = "error"
        )
        return()
      }
      if (is.null(input$new_documents) || !nrow(input$new_documents)) {
        showNotification(
          "Choose at least one document to upload.",
          type = "error"
        )
        return()
      }

      sample_id <- as.integer(sample_ids[[1]])
      details <- moduleData$samples[
        moduleData$samples$sample_id == sample_id,
        ,
        drop = FALSE
      ]
      share_with <- if (nrow(details)) {
        array_to_text(details$share_with)
      } else {
        character()
      }
      if (!length(share_with)) {
        share_with <- "public_reader"
      }

      con <- session$userData$AquaCache
      active <- FALSE
      tryCatch(
        {
          DBI::dbExecute(con, "BEGIN")
          active <- TRUE

          existing_ids <- normalize_document_ids(input$documents)
          uploaded_ids <- integer()
          for (ii in seq_len(nrow(input$new_documents))) {
            uploaded_ids <- c(
              uploaded_ids,
              insert_or_find_document(
                con,
                list(
                  name = input$new_documents$name[ii],
                  datapath = input$new_documents$datapath[ii]
                ),
                sample_id,
                share_with
              )
            )
          }

          selected_ids <- unique(c(existing_ids, uploaded_ids))
          sync_sample_document_links(con, sample_id, selected_ids)

          DBI::dbExecute(con, "COMMIT")
          active <- FALSE

          getModuleData()
          updateSelectizeInput(
            session,
            "documents",
            choices = named_choices(
              moduleData$documents$document_id,
              paste0(
                moduleData$documents$name,
                " (",
                moduleData$documents$document_id,
                ")"
              )
            ),
            selected = as.character(selected_ids)
          )
          showNotification(
            sprintf(
              "Linked %d uploaded document(s) to sample %s.",
              length(unique(uploaded_ids)),
              sample_id
            ),
            type = "message"
          )
        },
        error = function(e) {
          if (isTRUE(active)) {
            try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
          }
          showNotification(
            paste("Failed to upload/link document:", e$message),
            type = "error"
          )
        }
      )
    })

    observeEvent(input$update_sample, {
      sample_ids <- selected_sample_ids()
      if (!length(sample_ids)) {
        showNotification(
          "Select at least one sample from the table to modify.",
          type = "error"
        )
        return()
      }
      form <- collect_sample_inputs()
      multi_mode <- isTRUE(input$multi_edit)

      if (multi_mode) {
        selected_fields <- intersect(
          input$multi_fields,
          names(multi_editable_fields)
        )
        if (!length(selected_fields)) {
          showNotification(
            "Choose at least one field to update in multi-sample mode.",
            type = "error"
          )
          return()
        }

        documents_updated <- "documents" %in% selected_fields
        previous_documents <- if (documents_updated) {
          stats::setNames(
            lapply(sample_ids, current_sample_document_ids),
            as.character(sample_ids)
          )
        } else {
          list()
        }
        params <- list()
        set_clauses <- character()
        validation_errors <- character()
        for (field in selected_fields) {
          spec <- multi_editable_fields[[field]]
          value <- form[[field]]
          if (
            field %in%
              c("collection_method", "sample_type", "owner") &&
              is.na(value)
          ) {
            validation_errors <- c(
              validation_errors,
              sprintf("%s must be specified.", spec$label)
            )
          }
          params[[length(params) + 1]] <- value
          placeholder <- paste0("$", length(params))
          cast <- if (!is.null(spec$cast)) spec$cast else ""
          set_clauses <- c(
            set_clauses,
            sprintf("%s = %s%s", spec$column, placeholder, cast)
          )
        }

        if (length(validation_errors)) {
          showNotification(
            paste(validation_errors, collapse = " "),
            type = "error"
          )
          return()
        }

        if (!length(set_clauses)) {
          showNotification("No fields selected for update.", type = "error")
          return()
        }

        set_sql <- paste(set_clauses, collapse = ",\n          ")
        update_sql <- sprintf(
          "UPDATE discrete.samples\n        SET\n          %s\n        WHERE sample_id = $%d;",
          set_sql,
          length(params) + 1
        )

        errors <- character()
        cleanup_messages <- character()
        for (id in sample_ids) {
          res <- try(
            {
              DBI::dbExecute(
                session$userData$AquaCache,
                update_sql,
                params = c(params, list(id))
              )
              if (documents_updated) {
                sync_sample_document_links(
                  session$userData$AquaCache,
                  id,
                  form$document_ids
                )
                cleanup_messages <- c(
                  cleanup_messages,
                  cleanup_removed_documents(
                    session$userData$AquaCache,
                    setdiff(
                      previous_documents[[as.character(id)]],
                      form$document_ids
                    )
                  )
                )
              }
            },
            silent = TRUE
          )
          if (inherits(res, "try-error")) {
            err_condition <- attr(res, "condition")
            message <- if (!is.null(err_condition)) {
              conditionMessage(err_condition)
            } else {
              as.character(res)
            }
            errors <- c(errors, sprintf("Sample %s: %s", id, message))
          }
        }

        if (length(errors)) {
          showNotification(
            paste(c("Failed to update some samples:", errors), collapse = " "),
            type = "error"
          )
          return()
        }

        getModuleData()
        selected_rows <- which(
          moduleData$samples_display$sample_id %in% sample_ids
        )
        proxy <- DT::dataTableProxy("sample_table")
        if (length(selected_rows)) {
          proxy |> DT::selectRows(selected_rows)
        } else {
          proxy |> DT::selectRows(NULL)
        }
        showNotification(
          sprintf("Updated %d samples successfully.", length(sample_ids)),
          type = "message"
        )
        show_document_cleanup_messages(cleanup_messages)
      } else {
        sample_id <- sample_ids[[1]]
        previous_documents <- current_sample_document_ids(sample_id)
        if (is.na(form$location_id)) {
          showNotification("Location is required.", type = "error")
          return()
        }
        if (is.na(form$media_id)) {
          showNotification("Media is required.", type = "error")
          return()
        }
        if (is.na(form$collection_method)) {
          showNotification("Collection method is required.", type = "error")
          return()
        }
        if (is.na(form$sample_type)) {
          showNotification("Sample type is required.", type = "error")
          return()
        }
        if (is.na(form$owner)) {
          showNotification("Owner is required.", type = "error")
          return()
        }
        if (is.na(form$datetime)) {
          showNotification(
            "Sample datetime is required.",
            type = "error"
          )
          return()
        }

        update_sql <- "
        UPDATE discrete.samples
        SET
          location_id = $1,
          sub_location_id = $2,
          media_id = $3,
          z = $4,
          datetime = $5,
          target_datetime = $6,
          collection_method = $7,
          sample_type = $8,
          linked_with = $9,
          sample_volume_ml = $10,
          purge_volume_l = $11,
          purge_time_min = $12,
          flow_rate_l_min = $13,
          wave_hgt_m = $14,
          sample_grade = $15,
          sample_approval = $16,
          sample_qualifier = $17,
          owner = $18,
          contributor = $19,
          comissioning_org = $20,
          sampling_org = $21,
          documents = $22::integer[],
          share_with = $23::text[],
          import_source = $24,
          no_update = $25,
          note = $26,
          import_source_id = $27
        WHERE sample_id = $28;
      "

        params <- list(
          form$location_id,
          form$sub_location_id,
          form$media_id,
          form$z,
          form$datetime,
          form$target_datetime,
          form$collection_method,
          form$sample_type,
          form$linked_with,
          form$sample_volume_ml,
          form$purge_volume_l,
          form$purge_time_min,
          form$flow_rate_l_min,
          form$wave_hgt_m,
          form$sample_grade,
          form$sample_approval,
          form$sample_qualifier,
          form$owner,
          form$contributor,
          form$comissioning_org,
          form$sampling_org,
          form$documents,
          form$share_with,
          form$import_source,
          form$no_update,
          form$note,
          form$import_source_id,
          sample_id
        )

        tryCatch(
          {
            DBI::dbExecute(
              session$userData$AquaCache,
              update_sql,
              params = params
            )
            sync_sample_document_links(
              session$userData$AquaCache,
              sample_id,
              form$document_ids
            )
            cleanup_messages <- cleanup_removed_documents(
              session$userData$AquaCache,
              setdiff(previous_documents, form$document_ids)
            )
            getModuleData()
            showNotification("Sample updated successfully.", type = "message")
            show_document_cleanup_messages(cleanup_messages)
          },
          error = function(e) {
            showNotification(
              paste("Failed to update sample:", e$message),
              type = "error"
            )
          }
        )
      }
    })
  })
}
