# UI and server code for managing discrete samples

addSamplesUI <- function(id) {
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
      ))
    ),
    page_fluid(
      uiOutput(ns("ui"))
    )
  )
}

addSamples <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    moduleData <- reactiveValues()
    selected_sample_ids <- reactiveVal(integer())

    multi_editable_fields <- stats::setNames(
      c(
        "collection_method",
        "sample_type",
        "linked_with",
        "sample_volume_ml",
        "purge_volume_l",
        "purge_time_min",
        "flow_rate_l_min",
        "wave_hgt_m",
        "sample_grade",
        "sample_approval",
        "sample_qualifier",
        "owner",
        "contributor",
        "comissioning_org",
        "sampling_org",
        "documents",
        "share_with",
        "import_source",
        "no_update",
        "note"
      ),
      c(
        "Collection method",
        "Sample type",
        "Linked with",
        "Sample volume (mL)",
        "Purge volume (L)",
        "Purge time (minutes)",
        "Flow rate (L/min)",
        "Wave height (m)",
        "Sample grade",
        "Sample approval",
        "Sample qualifier",
        "Owner",
        "Contributor",
        "Comissioning organization",
        "Sampling organization",
        "Documents",
        "Share with",
        "Import source",
        "Lock sample from updates",
        "Notes"
      )
    )

    parse_datetime_input <- function(value) {
      if (is.null(value) || !nzchar(value)) {
        return(NA)
      }
      formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")
      for (fmt in formats) {
        parsed <- as.POSIXct(value, format = fmt, tz = "UTC")
        if (!is.na(parsed)) {
          return(parsed)
        }
      }
      NA
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

    format_share_with <- function(groups) {
      if (is.null(groups) || !length(groups) || all(!nzchar(groups))) {
        groups <- "public_reader"
      }
      groups <- gsub('"', '\\"', groups, fixed = TRUE)
      paste0("{", paste(sprintf('"%s"', groups), collapse = ","), "}")
    }

    parse_share_with <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(character())
      }
      if (is.list(value)) {
        value <- value[[1]]
      }
      value <- gsub("[{}\"]", "", value)
      out <- trimws(unlist(strsplit(value, ",")))
      out[nzchar(out)]
    }

    format_integer_array <- function(values) {
      if (is.null(values) || !length(values)) {
        return(NA)
      }
      values <- as.integer(values)
      values <- values[!is.na(values)]
      if (!length(values)) {
        return(NA)
      }
      paste0("{", paste(values, collapse = ","), "}")
    }

    parse_integer_array <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(integer())
      }
      if (is.list(value)) {
        value <- value[[1]]
      }
      value <- gsub("[{}]", "", value)
      out <- trimws(unlist(strsplit(value, ",")))
      out <- out[nzchar(out)]
      as.integer(out)
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

      list(
        location_id = location_id,
        sub_location_id = sub_location_id,
        media_id = media_id,
        z = if (!length(input$z) || is.na(input$z)) {
          NA_real_
        } else {
          as.numeric(input$z)
        },
        datetime = parse_datetime_input(input$datetime),
        target_datetime = parse_datetime_input(input$target_datetime),
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
        documents = format_integer_array(input$documents),
        share_with = format_share_with(input$share_with),
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

    reset_form <- function() {
      updateSelectizeInput(session, "location", selected = character(0))
      updateSelectizeInput(session, "sub_location", selected = character(0))
      updateSelectizeInput(session, "media", selected = character(0))
      updateNumericInput(session, "z", value = NA)
      updateTextInput(session, "datetime", value = "")
      updateTextInput(session, "target_datetime", value = "")
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
      updateTextInput(
        session,
        "datetime",
        value = format_datetime_input(details$datetime)
      )
      updateTextInput(
        session,
        "target_datetime",
        value = format_datetime_input(details$target_datetime)
      )
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
        selected = as.character(parse_integer_array(details$documents))
      )
      updateSelectizeInput(
        session,
        "share_with",
        selected = parse_share_with(details$share_with)
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
        "SELECT sample_id, location_id, sub_location_id, media_id, z, datetime, target_datetime, collection_method, sample_type, linked_with, sample_volume_ml, purge_volume_l, purge_time_min, flow_rate_l_min, wave_hgt_m, sample_grade, sample_approval, sample_qualifier, owner, contributor, comissioning_org, sampling_org, documents, share_with, import_source, no_update, note, import_source_id FROM discrete.samples ORDER BY datetime DESC"
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
      moduleData$share_groups <- DBI::dbGetQuery(
        con,
        "SELECT * FROM public.get_shareable_principals_for('discrete.samples') ORDER BY role_name ASC;"
      )
    }

    getModuleData()

    observeEvent(input$reload_module, {
      getModuleData()
      selected_sample_ids(integer())
      reset_form()
      DT::dataTableProxy(ns("sample_table")) |> DT::selectRows(NULL)
    })

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
            choices = stats::setNames(
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
            choices = stats::setNames(
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

    observeEvent(input$mode, {
      if (identical(input$mode, "add")) {
        selected_sample_ids(integer())
        reset_form()
        DT::dataTableProxy(ns("sample_table")) |> DT::selectRows(NULL)
      }
    })

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
        moduleData$grades,
        moduleData$approvals,
        moduleData$qualifiers
      )
      tagList(
        actionButton(
          ns("reload_module"),
          "Reload module data",
          icon = icon("refresh")
        ),
        radioButtons(
          ns("mode"),
          NULL,
          choices = c("Add new" = "add", "Modify existing" = "modify"),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          accordion(
            id = ns("accordion1"),
            open = "sample_table_panel",
            accordion_panel(
              id = ns("sample_table_panel"),
              title = "Select samples to modify",
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
                id = ns("multi_edit_fields_panel"),
                title = "Multi-sample edit options",
                tags$div(
                  class = "alert alert-warning",
                  "Only selected fields will be updated for all chosen samples. Location, sub-location, elevation/depth, sample datetime, target datetime, and import source ID remain single-sample edits."
                ),
                checkboxGroupInput(
                  ns("multi_fields"),
                  "Fields to update across all selected samples",
                  choices = multi_editable_fields,
                  selected = character(0),
                  inline = TRUE,
                  width = "100%"
                )
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          tags$div(
            class = "alert alert-info",
            "Provide details for a new sample. Required fields include location, media, collection method, sample type, owner, and datetime."
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("location"),
              "Location",
              choices = stats::setNames(
                moduleData$locations$location_id,
                moduleData$locations$name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select a location"),
              width = "100%"
            )
          ),
          column(
            6,
            selectizeInput(
              ns("sub_location"),
              "Sub-location",
              choices = stats::setNames(
                moduleData$sub_locations$sub_location_id,
                moduleData$sub_locations$sub_location_name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("media"),
              "Media",
              choices = stats::setNames(
                moduleData$media$media_id,
                moduleData$media$media_type
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select media"),
              width = "100%"
            )
          ),
          column(
            3,
            numericInput(
              ns("z"),
              "Elevation/depth (m)",
              value = NA,
              width = "100%"
            )
          ),
          column(
            3,
            checkboxInput(
              ns("no_update"),
              "Lock sample from updates",
              value = FALSE
            )
          )
        ),
        fluidRow(
          column(
            6,
            textInput(
              ns("datetime"),
              "Sample datetime (UTC)",
              placeholder = "YYYY-MM-DD HH:MM"
            )
          ),
          column(
            6,
            textInput(
              ns("target_datetime"),
              "Target datetime (UTC, optional)",
              placeholder = "YYYY-MM-DD HH:MM"
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("collection_method"),
              "Collection method",
              choices = stats::setNames(
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
          ),
          column(
            6,
            selectizeInput(
              ns("sample_type"),
              "Sample type",
              choices = stats::setNames(
                moduleData$sample_types$sample_type_id,
                moduleData$sample_types$sample_type
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select sample type"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("linked_with"),
              "Linked sample (optional)",
              choices = stats::setNames(
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
          ),
          column(
            6,
            selectizeInput(
              ns("documents"),
              "Associated documents",
              choices = stats::setNames(
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
        ),
        fluidRow(
          column(
            3,
            numericInput(
              ns("sample_volume_ml"),
              "Sample volume (mL)",
              value = NA,
              width = "100%"
            )
          ),
          column(
            3,
            numericInput(
              ns("purge_volume_l"),
              "Purge volume (L)",
              value = NA,
              width = "100%"
            )
          ),
          column(
            3,
            numericInput(
              ns("purge_time_min"),
              "Purge time (min)",
              value = NA,
              width = "100%"
            )
          ),
          column(
            3,
            numericInput(
              ns("flow_rate_l_min"),
              "Flow rate (L/min)",
              value = NA,
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            4,
            numericInput(
              ns("wave_hgt_m"),
              "Wave height (m)",
              value = NA,
              width = "100%"
            )
          ),
          column(
            4,
            selectizeInput(
              ns("sample_grade"),
              "Sample grade",
              choices = stats::setNames(
                moduleData$grades$grade_type_id,
                moduleData$grades$grade_type_description
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            )
          ),
          column(
            4,
            selectizeInput(
              ns("sample_approval"),
              "Sample approval",
              choices = stats::setNames(
                moduleData$approvals$approval_type_id,
                moduleData$approvals$approval_type_description
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("sample_qualifier"),
              "Sample qualifier",
              choices = stats::setNames(
                moduleData$qualifiers$qualifier_type_id,
                moduleData$qualifiers$qualifier_type_description
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            )
          ),
          column(
            6,
            selectizeInput(
              ns("owner"),
              "Owner",
              choices = stats::setNames(
                moduleData$organizations$organization_id,
                moduleData$organizations$name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select owner"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("contributor"),
              "Contributor",
              choices = stats::setNames(
                moduleData$organizations$organization_id,
                moduleData$organizations$name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            )
          ),
          column(
            6,
            selectizeInput(
              ns("comissioning_org"),
              "Comissioning organization",
              choices = stats::setNames(
                moduleData$organizations$organization_id,
                moduleData$organizations$name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("sampling_org"),
              "Sampling organization",
              choices = stats::setNames(
                moduleData$organizations$organization_id,
                moduleData$organizations$name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            )
          ),
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
        ),
        fluidRow(
          column(
            6,
            textInput(
              ns("import_source"),
              "Import source",
              placeholder = "Optional"
            )
          ),
          column(
            6,
            textInput(
              ns("import_source_id"),
              "Import source ID",
              placeholder = "Optional"
            )
          )
        ),
        textAreaInput(
          ns("note"),
          "Notes",
          rows = 3,
          placeholder = "Optional",
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          bslib::input_task_button(
            ns("add_sample"),
            label = "Add sample"
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          bslib::input_task_button(
            ns("update_sample"),
            label = "Update sample"
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

    observeEvent(input$sample_table_rows_selected, {
      if (!identical(input$mode, "modify")) {
        return()
      }
      idx <- input$sample_table_rows_selected
      multi_enabled <- isTRUE(input$multi_edit)
      if (!multi_enabled && length(idx) > 1) {
        idx <- idx[1]
        DT::dataTableProxy(ns("sample_table")) |> DT::selectRows(idx)
      }
      if (!length(idx)) {
        selected_sample_ids(integer())
        reset_form()
        return()
      }
      ids <- moduleData$samples_display$sample_id[idx]
      selected_sample_ids(ids)
      if (!multi_enabled && length(ids) == 1) {
        update_form_from_sample(ids)
      }
    })

    observeEvent(input$multi_edit, {
      if (!identical(input$mode, "modify")) {
        return()
      }
      if (!isTRUE(input$multi_edit)) {
        ids <- selected_sample_ids()
        if (length(ids) > 1) {
          ids <- ids[1]
          selected_sample_ids(ids)
          row_idx <- match(ids, moduleData$samples_display$sample_id)
          DT::dataTableProxy(ns("sample_table")) |> DT::selectRows(row_idx)
          update_form_from_sample(ids)
        } else if (length(ids) == 1) {
          update_form_from_sample(ids)
        }
        if (!is.null(input$multi_fields)) {
          updateCheckboxGroupInput(
            session,
            "multi_fields",
            selected = character(0)
          )
        }
      }
    })

    observeEvent(input$add_sample, {
      if (!identical(input$mode, "add")) {
        showNotification(
          "Switch to 'Add new' mode to create a sample.",
          type = "error"
        )
        return()
      }
      form <- collect_sample_inputs()

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
          "Sample datetime is required and must be in YYYY-MM-DD HH:MM format.",
          type = "error"
        )
        return()
      }

      insert_sql <- "
        INSERT INTO discrete.samples (
          location_id, sub_location_id, media_id, z, datetime, target_datetime,
          collection_method, sample_type, linked_with, sample_volume_ml,
          purge_volume_l, purge_time_min, flow_rate_l_min, wave_hgt_m,
          sample_grade, sample_approval, sample_qualifier, owner, contributor,
          comissioning_org, sampling_org, documents, share_with, import_source,
          no_update, note, import_source_id
        ) VALUES (
          $1, $2, $3, $4, $5, $6,
          $7, $8, $9, $10,
          $11, $12, $13, $14,
          $15, $16, $17, $18, $19,
          $20, $21, $22::integer[], $23::text[], $24,
          $25, $26, $27
        ) RETURNING sample_id;
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
        form$import_source_id
      )

      tryCatch(
        {
          res <- DBI::dbGetQuery(
            session$userData$AquaCache,
            insert_sql,
            params = params
          )
          getModuleData()
          reset_form()
          selected_sample_ids(integer())
          DT::dataTableProxy(ns("sample_table")) |> DT::selectRows(NULL)
          showNotification(
            sprintf("Sample %s added successfully.", res$sample_id[1]),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Failed to add sample:", e$message),
            type = "error"
          )
        }
      )
    })

    observeEvent(input$update_sample, {
      if (!identical(input$mode, "modify")) {
        showNotification(
          "Switch to 'Modify existing' mode to update a sample.",
          type = "error"
        )
        return()
      }
      sample_ids <- selected_sample_ids()
      if (!length(sample_ids)) {
        showNotification(
          "Select at least one sample from the table to modify.",
          type = "error"
        )
        return()
      }
      form <- collect_sample_inputs()
      multi_mode <- isTRUE(input$multi_edit) && length(sample_ids) > 1

      if (multi_mode) {
        selected_fields <- intersect(
          input$multi_fields,
          multi_editable_fields
        )
        if (!length(selected_fields)) {
          showNotification(
            "Choose at least one field to update in multi-sample mode.",
            type = "error"
          )
          return()
        }

        params <- list()
        set_clauses <- character()
        validation_errors <- character()
        for (field in selected_fields) {
          spec <- multi_editable_fields[multi_editable_fields == field][[1]]
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
        for (id in sample_ids) {
          res <- try(
            DBI::dbExecute(
              session$userData$AquaCache,
              update_sql,
              params = c(params, list(id))
            ),
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
        proxy <- DT::dataTableProxy(ns("sample_table"))
        if (length(selected_rows)) {
          proxy |> DT::selectRows(selected_rows)
        } else {
          proxy |> DT::selectRows(NULL)
        }
        showNotification(
          sprintf("Updated %d samples successfully.", length(sample_ids)),
          type = "message"
        )
      } else {
        sample_id <- sample_ids[[1]]
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
            "Sample datetime is required and must be in YYYY-MM-DD HH:MM format.",
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
            getModuleData()
            showNotification("Sample updated successfully.", type = "message")
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
