# UI and server code for managing discrete sample series

addSampleSeriesUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML(
      ".shiny-split-layout > div {overflow: visible;}"
    ))),
    page_fluid(
      uiOutput(ns("banner")),
      uiOutput(ns("ui"))
    )
  )
}

addSampleSeries <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addSampleSeries"
      )
    })

    moduleData <- reactiveValues()
    selected_series <- reactiveVal(NULL)
    source_args_existing <- reactiveVal(NA_character_)
    source_args_existing_source <- reactiveVal(NA_character_)
    source_args_secondary_existing <- reactiveVal(NA_character_)
    source_args_secondary_existing_source <- reactiveVal(NA_character_)
    moduleData$org_modal_target <- NULL
    pending_owner_selection <- reactiveVal(character(0))
    pending_owner_new <- reactiveVal(NULL)
    pending_contributor_selection <- reactiveVal(character(0))
    pending_contributor_new <- reactiveVal(NULL)

    format_datetime_input <- function(value) {
      if (is.null(value)) {
        return("")
      }
      if (length(value) == 0) {
        return(character(0))
      }
      if (all(is.na(value))) {
        return(rep("", length(value)))
      }
      value <- as.POSIXct(value, tz = "UTC")
      out <- rep("", length(value))
      valid <- !is.na(value)
      out[valid] <- format(value[valid], "%Y-%m-%d %H:%M")
      if (length(out) == 1) {
        out <- out[[1]]
      }
      out
    }

    shift_sync_datetime_inputs <- function(tz_name) {
      shift_air_datetime_input_timezone(
        session,
        input,
        "synch_from",
        tz_name
      )
      shift_air_datetime_input_timezone(
        session,
        input,
        "synch_to",
        tz_name
      )
    }

    update_org_selectize <- function(input_id, selected = NULL) {
      args <- list(
        session = session,
        inputId = input_id,
        choices = stats::setNames(
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        )
      )
      if (!is.null(selected)) {
        args$selected <- normalize_selectize_values(selected)
      }
      do.call(updateSelectizeInput, args)
    }

    getModuleData <- function() {
      moduleData$sample_series <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sample_series_id, location_id, sub_location_id, synch_from, synch_to, default_owner, default_contributor, active, note FROM discrete.sample_series ORDER BY sample_series_id"
      )
      moduleData$sample_series_source_assignments <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sample_series_source_adapter_id, sample_series_id,
                source_fx, source_fx_args, fetch_priority,
                synchronize_priority, active, note
         FROM discrete.sample_series_source_adapters
         ORDER BY sample_series_id,
                  COALESCE(fetch_priority, 32767),
                  COALESCE(synchronize_priority, 32767),
                  sample_series_source_adapter_id"
      )
      moduleData$sample_series_display <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT ss.sample_series_id, loc.name AS location,
                sl.sub_location_name AS sub_location, ss.synch_from,
                ss.synch_to, ss.active, source.source_fx,
                owner.name AS default_owner,
                contrib.name AS default_contributor, ss.last_new_data,
                ss.last_synchronize, ss.note
         FROM discrete.sample_series ss
         JOIN public.locations loc ON ss.location_id = loc.location_id
         LEFT JOIN public.sub_locations sl
           ON ss.sub_location_id = sl.sub_location_id
         LEFT JOIN public.organizations owner
           ON ss.default_owner = owner.organization_id
         LEFT JOIN public.organizations contrib
           ON ss.default_contributor = contrib.organization_id
         LEFT JOIN LATERAL (
           SELECT ssa.source_fx
           FROM discrete.sample_series_source_adapters ssa
           WHERE ssa.sample_series_id = ss.sample_series_id
           ORDER BY COALESCE(ssa.fetch_priority, 32767),
                    COALESCE(ssa.synchronize_priority, 32767),
                    ssa.sample_series_source_adapter_id
           LIMIT 1
         ) source ON TRUE
         ORDER BY loc.name ASC, sl.sub_location_name ASC"
      )
      moduleData$locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT location_id, name FROM public.locations ORDER BY name ASC"
      )
      moduleData$sub_locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sub_location_id, sub_location_name, location_id FROM public.sub_locations ORDER BY sub_location_name ASC"
      )
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM public.organizations ORDER BY name ASC"
      )
      moduleData$source_adapters <- AquaCache::getSourceAdapterCapabilities(
        con = session$userData$AquaCache,
        data_domain = "discrete"
      )
      moduleData$source_fx <- sort(unique(
        as.character(moduleData$source_adapters$source_fx)
      ))
    }

    getModuleData()

    current_source_capability <- reactive({
      source_adapter_capability_row(moduleData$source_adapters, input$source_fx)
    })

    current_stored_source_args <- reactive({
      source_fx <- input$source_fx
      if (
        length(source_fx) == 1L &&
          !is.na(source_fx) &&
          identical(source_fx, source_args_existing_source())
      ) {
        source_args_existing()
      } else {
        NA_character_
      }
    })

    output$source_fx_args_ui <- renderUI({
      capability <- current_source_capability()
      if (is.null(capability)) {
        return(tags$div(
          class = "alert alert-secondary",
          "Select a source function to see its arguments."
        ))
      }
      source_adapter_argument_ui(
        ns,
        capability,
        current_stored_source_args()
      )
    })

    collect_source_fx_args <- function() {
      source_adapter_args_json(source_adapter_collect_args(
        input,
        current_source_capability(),
        current_stored_source_args()
      ))
    }

    current_secondary_source_capability <- reactive({
      source_adapter_capability_row(
        moduleData$source_adapters,
        input$source_fx_secondary
      )
    })

    current_stored_secondary_source_args <- reactive({
      source_fx <- input$source_fx_secondary
      if (
        length(source_fx) == 1L &&
          !is.na(source_fx) &&
          identical(source_fx, source_args_secondary_existing_source())
      ) {
        source_args_secondary_existing()
      } else {
        NA_character_
      }
    })

    output$source_fx_secondary_args_ui <- renderUI({
      capability <- current_secondary_source_capability()
      if (is.null(capability)) {
        return(tags$div(
          class = "alert alert-secondary",
          "Select a secondary source function to see its arguments."
        ))
      }
      source_adapter_argument_ui(
        ns,
        capability,
        current_stored_secondary_source_args(),
        input_prefix = "secondary_"
      )
    })

    collect_secondary_source_fx_args <- function() {
      source_adapter_args_json(source_adapter_collect_args(
        input,
        current_secondary_source_capability(),
        current_stored_secondary_source_args(),
        input_prefix = "secondary_"
      ))
    }

    collect_source_assignments <- function() {
      rows <- list()
      add_assignment <- function(
        source_fx,
        args,
        active,
        use_fetch,
        fetch_priority,
        use_synchronize,
        synchronize_priority
      ) {
        if (
          is.null(source_fx) ||
            !length(source_fx) ||
            is.na(source_fx[[1L]]) ||
            !nzchar(source_fx[[1L]])
        ) {
          return()
        }
        if (!isTRUE(use_fetch) && !isTRUE(use_synchronize)) {
          stop(
            "Each source assignment must be used for fetching, synchronizing, or both."
          )
        }
        rows[[length(rows) + 1L]] <<- data.frame(
          source_fx = as.character(source_fx[[1L]]),
          source_fx_args = args,
          fetch_priority = if (isTRUE(use_fetch)) {
            as.integer(fetch_priority)
          } else {
            NA_integer_
          },
          synchronize_priority = if (isTRUE(use_synchronize)) {
            as.integer(synchronize_priority)
          } else {
            NA_integer_
          },
          active = isTRUE(active),
          note = NA_character_,
          stringsAsFactors = FALSE
        )
      }
      add_assignment(
        input$source_fx,
        collect_source_fx_args(),
        input$source_assignment_active,
        input$source_use_fetch,
        input$source_fetch_priority,
        input$source_use_synchronize,
        input$source_synchronize_priority
      )
      add_assignment(
        input$source_fx_secondary,
        collect_secondary_source_fx_args(),
        input$source_secondary_active,
        input$source_secondary_use_fetch,
        input$source_secondary_fetch_priority,
        input$source_secondary_use_synchronize,
        input$source_secondary_synchronize_priority
      )
      if (!length(rows)) {
        stop("Please configure at least one source assignment.")
      }
      assignments <- do.call(rbind, rows)
      active <- assignments[assignments$active, , drop = FALSE]
      for (priority in c("fetch_priority", "synchronize_priority")) {
        values <- active[[priority]][!is.na(active[[priority]])]
        if (anyDuplicated(values)) {
          stop(
            "Active source assignments must have unique ",
            gsub("_", " ", priority),
            " values."
          )
        }
      }
      assignments
    }

    insert_source_assignments <- function(sample_series_id, assignments) {
      for (i in seq_len(nrow(assignments))) {
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO discrete.sample_series_source_adapters (
             sample_series_id, source_fx, source_fx_args, fetch_priority,
             synchronize_priority, active, note
           ) VALUES ($1, $2, $3::jsonb, $4, $5, $6, $7)",
          params = list(
            sample_series_id,
            assignments$source_fx[[i]],
            assignments$source_fx_args[[i]],
            assignments$fetch_priority[[i]],
            assignments$synchronize_priority[[i]],
            assignments$active[[i]],
            assignments$note[[i]]
          )
        )
      }
    }

    output$ui <- renderUI({
      req(moduleData$locations, moduleData$organizations)
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
            open = "sampleseries_table_panel",
            accordion_panel(
              id = ns("sampleseries_table_panel"),
              title = "Select a sample series to modify",
              DT::DTOutput(ns("ss_table"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          tags$div(
            class = "alert alert-info",
            "Provide the connection details for a new sample series. The source function determines how samples are retrieved when synchronizing."
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectizeInput(
              ns("location"),
              "Location (add new under the 'locations' menu)",
              choices = stats::setNames(
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
          ),
          column(
            width = 6,
            selectizeInput(
              ns("sub_location"),
              "Sub-location",
              choices = stats::setNames(
                moduleData$sub_locations$sub_location_id,
                moduleData$sub_locations$sub_location_name
              ),
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Optional"
              ),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            selectizeInput(
              ns("timezone"),
              "Input timezone",
              choices = input_timezone_choices(),
              selected = default_input_timezone(),
              multiple = FALSE,
              width = "100%"
            )
          ),
          column(
            width = 4,
            shinyWidgets::airDatepickerInput(
              ns("synch_from"),
              "Synchronize from (optional)",
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
          ),
          column(
            width = 5,
            shinyWidgets::airDatepickerInput(
              ns("synch_to"),
              "Synchronize to (optional)",
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
        fluidRow(
          column(
            width = 6,
            selectizeInput(
              ns("default_owner"),
              "Default owner (applies if no sample owner specified)",
              choices = stats::setNames(
                moduleData$organizations$organization_id,
                moduleData$organizations$name
              ),
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Select owner",
                create = TRUE
              ),
              width = "100%"
            )
          ),
          column(
            width = 6,
            selectizeInput(
              ns("default_contributor"),
              "Default contributor (applies if no sample contributor specified)",
              choices = stats::setNames(
                moduleData$organizations$organization_id,
                moduleData$organizations$name
              ),
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Optional",
                create = TRUE
              ),
              width = "100%"
            )
          )
        ),
        checkboxInput(
          ns("active"),
          "Active",
          value = TRUE
        ),
        tags$div(
          class = "alert alert-info",
          "Configure one or two source assignments. Active assignments are selected independently for routine fetching and full synchronization by their priority; lower numbers run first."
        ),
        fluidRow(
          column(
            width = 6,
            tags$h5("Primary source assignment"),
            tags$div(
              class = "alert alert-secondary",
              "Usually the routine incremental import route."
            ),
            selectizeInput(
              ns("source_fx"),
              "Source function (see AquaCache package documentation for details)",
              choices = moduleData$source_fx,
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Select source function"
              ),
              width = "100%"
            ),
            tags$p(
              class = "text-muted small",
              "Missing download function? Download functions must be ",
              "registered in the database's ",
              tags$code("public.source_adapter_capabilities"),
              " table for the discrete domain to show up here. Developers: see AquaCache::registerSourceAdapterArguments()."
            ),
            actionButton(
              ns("source_fx_doc"),
              "Open function documentation"
            ),
            checkboxInput(
              ns("source_assignment_active"),
              "Assignment active",
              TRUE
            ),
            fluidRow(
              column(
                6,
                checkboxInput(ns("source_use_fetch"), "Use for fetching", TRUE)
              ),
              column(
                6,
                numericInput(
                  ns("source_fetch_priority"),
                  "Fetch priority",
                  1,
                  min = 1,
                  step = 1
                )
              )
            ),
            fluidRow(
              column(
                6,
                checkboxInput(
                  ns("source_use_synchronize"),
                  "Use for synchronization",
                  TRUE
                )
              ),
              column(
                6,
                numericInput(
                  ns("source_synchronize_priority"),
                  "Synchronization priority",
                  1,
                  min = 1,
                  step = 1
                )
              )
            ),
            uiOutput(ns("source_fx_args_ui")),
            actionButton(
              ns("args_example"),
              "Show example arguments"
            )
          ),
          column(
            width = 6,
            tags$h5("Secondary source assignment (optional)"),
            tags$div(
              class = "alert alert-secondary",
              "Useful for retaining an alternate provider or a synchronization-specific route."
            ),
            selectizeInput(
              ns("source_fx_secondary"),
              "Secondary source function",
              choices = moduleData$source_fx,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            ),
            checkboxInput(
              ns("source_secondary_active"),
              "Assignment active",
              FALSE
            ),
            fluidRow(
              column(
                6,
                checkboxInput(
                  ns("source_secondary_use_fetch"),
                  "Use for fetching",
                  FALSE
                )
              ),
              column(
                6,
                numericInput(
                  ns("source_secondary_fetch_priority"),
                  "Fetch priority",
                  2,
                  min = 1,
                  step = 1
                )
              )
            ),
            fluidRow(
              column(
                6,
                checkboxInput(
                  ns("source_secondary_use_synchronize"),
                  "Use for synchronization",
                  FALSE
                )
              ),
              column(
                6,
                numericInput(
                  ns("source_secondary_synchronize_priority"),
                  "Synchronization priority",
                  2,
                  min = 1,
                  step = 1
                )
              )
            ),
            uiOutput(ns("source_fx_secondary_args_ui"))
          )
        ),
        textAreaInput(
          ns("note"),
          "Note (optional)",
          rows = 3,
          placeholder = "Optional",
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          bslib::input_task_button(
            ns("add_sample_series"),
            label = "Add sample series"
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          bslib::input_task_button(
            ns("modify_sample_series"),
            label = "Modify sample series"
          )
        )
      )
    })

    output$ss_table <- DT::renderDT({
      req(moduleData$sample_series_display)
      df <- moduleData$sample_series_display
      if (nrow(df) > 0) {
        df$synch_from <- format_datetime_input(df$synch_from)
        df$synch_to <- format_datetime_input(df$synch_to)
        df$last_new_data <- format_datetime_input(df$last_new_data)
        df$last_synchronize <- format_datetime_input(df$last_synchronize)
        df$location <- as.factor(df$location)
        df$sub_location <- as.factor(df$sub_location)
        df$source_fx <- as.factor(df$source_fx)
        df$default_owner <- as.factor(df$default_owner)
        df$default_contributor <- as.factor(df$default_contributor)
      }
      DT::datatable(
        df,
        selection = "single",
        options = list(
          columnDefs = list(
            list(targets = 0, visible = FALSE)
          ),
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'background-color': '#079',",
            "  'color': '#fff',",
            "  'font-size': '90%',",
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '80%',",
            "});",
            "}"
          )
        ),
        filter = 'top',
        rownames = FALSE
      )
    })

    table_proxy <- DT::dataTableProxy("ss_table")

    observeEvent(input$reload_module, {
      getModuleData()
      updateSelectizeInput(
        session,
        "source_fx",
        choices = moduleData$source_fx
      )
      updateSelectizeInput(
        session,
        "source_fx_secondary",
        choices = moduleData$source_fx
      )
      showNotification("Module reloaded", type = "message")
    })

    observeEvent(
      input$timezone,
      {
        shift_sync_datetime_inputs(normalize_input_timezone(input$timezone))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$location,
      {
        req(moduleData$sub_locations)
        possibilities <- moduleData$sub_locations[
          moduleData$sub_locations$location_id == input$location,
        ]
        updateSelectizeInput(
          session,
          "sub_location",
          choices = stats::setNames(
            possibilities$sub_location_id,
            possibilities$sub_location_name
          )
        )
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      input$mode,
      {
        if (input$mode == "add") {
          selected_series(NULL)
          DT::selectRows(table_proxy, NULL)
        }
      },
      ignoreNULL = TRUE
    )

    prompt_new_org <- function(value, target) {
      moduleData$org_modal_target <- target
      if (identical(target, "default_owner")) {
        pending_owner_new(value)
      } else {
        pending_contributor_new(value)
      }
      showModal(modalDialog(
        title = if (identical(target, "default_owner")) {
          "Add owner"
        } else {
          "Add contributor"
        },
        textInput(ns("org_name"), "Organization name", value = value),
        textInput(ns("org_name_fr"), "Organization name (French, optional)"),
        textInput(ns("contact_name"), "Contact name (optional)"),
        textInput(ns("contact_phone"), "Contact phone (optional)"),
        textInput(ns("contact_email"), "Contact email (optional)"),
        textInput(ns("contact_note"), "Contact note (optional)"),
        footer = tagList(
          actionButton(ns("cancel_org_modal"), "Cancel"),
          actionButton(ns("save_org"), "Add organization")
        ),
        easyClose = FALSE
      ))
    }

    observeEvent(
      input$default_owner,
      {
        resolved <- resolve_selectize_lookup_values(
          input$default_owner,
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        )
        pending_owner_selection(resolved$existing_selection)

        if (!length(resolved$submitted_values)) {
          pending_owner_new(NULL)
          return()
        }

        if (!length(resolved$new_values)) {
          pending_owner_new(NULL)
          if (resolved$used_label_match) {
            update_org_selectize("default_owner", resolved$existing_selection)
          }
          return()
        }

        prompt_new_org(resolved$last_new_value, "default_owner")
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      input$default_contributor,
      {
        resolved <- resolve_selectize_lookup_values(
          input$default_contributor,
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        )
        pending_contributor_selection(resolved$existing_selection)

        if (!length(resolved$submitted_values)) {
          pending_contributor_new(NULL)
          return()
        }

        if (!length(resolved$new_values)) {
          pending_contributor_new(NULL)
          if (resolved$used_label_match) {
            update_org_selectize(
              "default_contributor",
              resolved$existing_selection
            )
          }
          return()
        }

        prompt_new_org(resolved$last_new_value, "default_contributor")
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      input$cancel_org_modal,
      {
        if (identical(moduleData$org_modal_target, "default_owner")) {
          update_org_selectize("default_owner", pending_owner_selection())
          pending_owner_new(NULL)
        } else if (
          identical(moduleData$org_modal_target, "default_contributor")
        ) {
          update_org_selectize(
            "default_contributor",
            pending_contributor_selection()
          )
          pending_contributor_new(NULL)
        }
        moduleData$org_modal_target <- NULL
        removeModal()
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      input$save_org,
      {
        if (!isTruthy(input$org_name)) {
          shinyjs::js$backgroundCol(ns("org_name"), "#fdd")
          return()
        } else {
          shinyjs::js$backgroundCol(ns("org_name"), "#fff")
        }
        org_name <- trimws(input$org_name)
        existing_id <- match_lookup_id_by_label(
          org_name,
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        )
        if (length(existing_id)) {
          if (identical(moduleData$org_modal_target, "default_owner")) {
            update_org_selectize("default_owner", existing_id[[1]])
            pending_owner_selection(existing_id[[1]])
            pending_owner_new(NULL)
          } else if (
            identical(moduleData$org_modal_target, "default_contributor")
          ) {
            update_org_selectize("default_contributor", existing_id[[1]])
            pending_contributor_selection(existing_id[[1]])
            pending_contributor_new(NULL)
          }
          moduleData$org_modal_target <- NULL
          removeModal()
          showNotification("Existing organization selected.", type = "message")
          return()
        }
        df <- data.frame(
          name = org_name,
          name_fr = if (isTruthy(input$org_name_fr)) {
            trimws(input$org_name_fr)
          } else {
            NA
          },
          contact_name = if (isTruthy(input$contact_name)) {
            trimws(input$contact_name)
          } else {
            NA
          },
          phone = if (isTruthy(input$contact_phone)) {
            trimws(input$contact_phone)
          } else {
            NA
          },
          email = if (isTruthy(input$contact_email)) {
            trimws(input$contact_email)
          } else {
            NA
          },
          note = if (isTruthy(input$contact_note)) {
            trimws(input$contact_note)
          } else {
            NA
          }
        )

        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO public.organizations (name, name_fr, contact_name, phone, email, note) VALUES ($1, $2, $3, $4, $5, $6);",
          params = list(
            df$name,
            df$name_fr,
            df$contact_name,
            df$phone,
            df$email,
            df$note
          )
        )

        moduleData$organizations <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT organization_id, name FROM public.organizations ORDER BY name ASC"
        )
        update_org_selectize("default_owner")
        update_org_selectize("default_contributor")
        new_id <- match_lookup_id_by_label(
          df$name,
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        )
        if (!length(new_id)) {
          new_id <- moduleData$organizations$organization_id[1]
        }
        if (identical(moduleData$org_modal_target, "default_owner")) {
          update_org_selectize("default_owner", new_id)
          pending_owner_selection(new_id)
          pending_owner_new(NULL)
        } else if (
          identical(moduleData$org_modal_target, "default_contributor")
        ) {
          update_org_selectize("default_contributor", new_id)
          pending_contributor_selection(new_id)
          pending_contributor_new(NULL)
        }
        moduleData$org_modal_target <- NULL
        removeModal()
        showNotification("Organization added", type = "message")
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      input$ss_table_rows_selected,
      {
        sel <- input$ss_table_rows_selected
        if (length(sel) == 0) {
          selected_series(NULL)
          source_args_existing(NA_character_)
          source_args_existing_source(NA_character_)
          source_args_secondary_existing(NA_character_)
          source_args_secondary_existing_source(NA_character_)
          return()
        }
        ssid <- moduleData$sample_series_display[sel, "sample_series_id"]
        selected_series(ssid)
        details <- moduleData$sample_series[
          moduleData$sample_series$sample_series_id == ssid,
        ]
        if (nrow(details) == 0) {
          showNotification("Selected sample series not found.", type = "error")
          return()
        }
        updateSelectizeInput(
          session,
          "location",
          selected = details$location_id
        )
        updateSelectizeInput(
          session,
          "sub_location",
          selected = if (is.na(details$sub_location_id)) {
            character(0)
          } else {
            details$sub_location_id
          }
        )
        shinyWidgets::updateAirDateInput(
          session,
          "synch_from",
          value = if (is.na(details$synch_from)) {
            NULL
          } else {
            coerce_utc_datetime(details$synch_from)
          },
          tz = air_datetime_widget_timezone(input$timezone),
          clear = is.na(details$synch_from)
        )
        shinyWidgets::updateAirDateInput(
          session,
          "synch_to",
          value = if (is.na(details$synch_to)) {
            NULL
          } else {
            coerce_utc_datetime(details$synch_to)
          },
          tz = air_datetime_widget_timezone(input$timezone),
          clear = is.na(details$synch_to)
        )
        updateSelectizeInput(
          session,
          "default_owner",
          selected = details$default_owner
        )
        updateSelectizeInput(
          session,
          "default_contributor",
          selected = if (is.na(details$default_contributor)) {
            character(0)
          } else {
            details$default_contributor
          }
        )
        updateCheckboxInput(session, "active", value = isTRUE(details$active))
        assignments <- moduleData$sample_series_source_assignments[
          moduleData$sample_series_source_assignments$sample_series_id == ssid,
          ,
          drop = FALSE
        ]
        if (nrow(assignments) > 2L) {
          showNotification(
            "This sample series has more than two source assignments. The first two are shown; remove or consolidate the additional assignments before modifying it here.",
            type = "warning",
            duration = 10
          )
        }
        primary <- if (nrow(assignments)) {
          assignments[1, , drop = FALSE]
        } else {
          assignments[0, , drop = FALSE]
        }
        secondary <- if (nrow(assignments) >= 2L) {
          assignments[2, , drop = FALSE]
        } else {
          assignments[0, , drop = FALSE]
        }

        if (nrow(primary)) {
          source_args_existing(primary$source_fx_args)
          source_args_existing_source(as.character(primary$source_fx))
        } else {
          source_args_existing(NA_character_)
          source_args_existing_source(NA_character_)
        }
        updateSelectizeInput(
          session,
          "source_fx",
          selected = if (!nrow(primary)) {
            character(0)
          } else {
            primary$source_fx
          }
        )
        updateCheckboxInput(
          session,
          "source_assignment_active",
          value = !nrow(primary) || isTRUE(primary$active)
        )
        updateCheckboxInput(
          session,
          "source_use_fetch",
          value = nrow(primary) && !is.na(primary$fetch_priority)
        )
        updateNumericInput(
          session,
          "source_fetch_priority",
          value = if (nrow(primary) && !is.na(primary$fetch_priority)) {
            primary$fetch_priority
          } else {
            1
          }
        )
        updateCheckboxInput(
          session,
          "source_use_synchronize",
          value = nrow(primary) && !is.na(primary$synchronize_priority)
        )
        updateNumericInput(
          session,
          "source_synchronize_priority",
          value = if (nrow(primary) && !is.na(primary$synchronize_priority)) {
            primary$synchronize_priority
          } else {
            1
          }
        )

        if (nrow(secondary)) {
          source_args_secondary_existing(secondary$source_fx_args)
          source_args_secondary_existing_source(as.character(
            secondary$source_fx
          ))
        } else {
          source_args_secondary_existing(NA_character_)
          source_args_secondary_existing_source(NA_character_)
        }
        updateSelectizeInput(
          session,
          "source_fx_secondary",
          selected = if (nrow(secondary)) secondary$source_fx else character(0)
        )
        updateCheckboxInput(
          session,
          "source_secondary_active",
          value = !nrow(secondary) || isTRUE(secondary$active)
        )
        updateCheckboxInput(
          session,
          "source_secondary_use_fetch",
          value = nrow(secondary) && !is.na(secondary$fetch_priority)
        )
        updateNumericInput(
          session,
          "source_secondary_fetch_priority",
          value = if (nrow(secondary) && !is.na(secondary$fetch_priority)) {
            secondary$fetch_priority
          } else {
            2
          }
        )
        updateCheckboxInput(
          session,
          "source_secondary_use_synchronize",
          value = nrow(secondary) && !is.na(secondary$synchronize_priority)
        )
        updateNumericInput(
          session,
          "source_secondary_synchronize_priority",
          value = if (
            nrow(secondary) && !is.na(secondary$synchronize_priority)
          ) {
            secondary$synchronize_priority
          } else {
            1
          }
        )
        updateTextAreaInput(
          session,
          "note",
          value = ifelse(is.na(details$note), "", details$note)
        )
      },
      ignoreNULL = TRUE
    )

    observeEvent(input$args_example, {
      if (is.null(input$source_fx) || input$source_fx == "") {
        showModal(modalDialog(
          "Select a source function to view example arguments.",
          easyClose = TRUE
        ))
        return()
      }
      ex_args <- moduleData$sample_series_source_assignments[
        moduleData$sample_series_source_assignments$source_fx ==
          input$source_fx,
        "source_fx_args"
      ]
      ex_args <- ex_args[!is.na(ex_args)][1:10]
      ex_args <- ex_args[nzchar(ex_args)]
      # strip the [], {}, and "" from the json strings
      ex_args <- gsub("\\[|\\]|\\{|\\}|\"", "", ex_args)
      showModal(modalDialog(
        title = paste("Example arguments for", input$source_fx),
        if (length(ex_args) > 0) {
          tags$pre(paste(unique(ex_args), collapse = "\n"))
        } else {
          "No example arguments found in existing timeseries. Please refer to the AquaCache package documentation for details on the required arguments."
        },
        easyClose = TRUE
      ))
    })

    observeEvent(input$source_fx_doc, {
      if (is.null(input$source_fx) || input$source_fx == "") {
        showModal(modalDialog(
          "Select a source function to open its documentation.",
          easyClose = TRUE
        ))
        return()
      }
      package <- tools::Rd_db("AquaCache")
      file <- paste0(input$source_fx, ".Rd")
      if (!file %in% names(package)) {
        showModal(modalDialog(
          "Documentation not found for the selected function.",
          easyClose = TRUE
        ))
        return()
      }
      # output path under the served directory, set up in globals
      out <- file.path(.rd_dir, paste0(input$source_fx, ".html"))
      tools::Rd2HTML(
        package[[file]],
        out,
        no_links = TRUE,
        package = "AquaCache"
      )
      # URL that the client can reach
      rdoc_url <- function(session, filename) {
        path <- session$clientData$url_pathname
        if (is.null(path) || !nzchar(path)) {
          path <- "/"
        }
        if (!grepl("/$", path)) {
          path <- paste0(path, "/")
        }
        paste0(path, "rdocs/", filename)
      }
      url <- rdoc_url(session, basename(out)) # not namespaced
      shinyjs::runjs(sprintf("window.open('%s','_blank');", url))
    })

    observeEvent(input$add_sample_series, {
      if (input$mode != "add") {
        showNotification(
          "Switch to 'Add new' mode before adding a sample series.",
          type = "error"
        )
        return()
      }
      validate(
        need(input$location, "Please select a location."),
        need(input$source_fx, "Please select a source function."),
        need(input$default_owner, "Please select a default owner.")
      )

      sub_loc <- if (is.null(input$sub_location)) {
        NA
      } else if (
        length(input$sub_location) == 0 || !nzchar(input$sub_location[1])
      ) {
        NA
      } else {
        as.numeric(input$sub_location[1])
      }

      # Ensure that there is not an existing sample series for this location + sub_location combo
      exist <- if (is.na(sub_loc)) {
        moduleData$sample_series[
          moduleData$sample_series$location_id == as.numeric(input$location),
        ]
      } else {
        moduleData$sample_series[
          moduleData$sample_series$location_id == as.numeric(input$location) &
            moduleData$sample_series$sub_location_id == sub_loc,
        ]
      }
      if (nrow(exist) > 0) {
        showNotification(
          "There is alraedy a sample series for this location and sub_location combo. Please modify the existing sample series.",
          type = "error",
          duration = 8
        )
        return()
      }

      synch_from_input <- input$synch_from
      synch_from <- scalar_utc_datetime(synch_from_input)
      if (
        !is.null(synch_from_input) &&
          length(synch_from_input) &&
          any(!is.na(synch_from_input)) &&
          is.na(synch_from)
      ) {
        showNotification("Invalid 'synchronize from' value.", type = "error")
        return()
      }
      synch_to_input <- input$synch_to
      synch_to <- scalar_utc_datetime(synch_to_input)
      if (
        !is.null(synch_to_input) &&
          length(synch_to_input) &&
          any(!is.na(synch_to_input)) &&
          is.na(synch_to)
      ) {
        showNotification("Invalid 'synchronize to' value.", type = "error")
        return()
      }
      source_assignments <- tryCatch(
        collect_source_assignments(),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
      if (is.null(source_assignments)) {
        return()
      }

      contributor <- if (is.null(input$default_contributor)) {
        NA
      } else if (
        length(input$default_contributor) == 0 ||
          !nzchar(input$default_contributor[1])
      ) {
        NA
      } else {
        as.numeric(input$default_contributor[1])
      }

      DBI::dbBegin(session$userData$AquaCache)
      res <- tryCatch(
        {
          new <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "INSERT INTO discrete.sample_series (
               location_id, sub_location_id, synch_from, synch_to,
               default_owner, default_contributor, active, note
             ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
             RETURNING sample_series_id;",
            params = list(
              input$location,
              sub_loc,
              synch_from,
              synch_to,
              input$default_owner,
              contributor,
              isTRUE(input$active),
              input$note
            )
          )
          insert_source_assignments(
            new$sample_series_id[[1L]],
            source_assignments
          )
          # Try to get new discrete data
          AquaCache::getNewDiscrete(
            con = session$userData$AquaCache,
            sample_series_id = new$sample_series_id[[1]]
          )
        },
        error = function(e) {
          DBI::dbRollback(session$userData$AquaCache)
          showNotification(
            paste("Error adding sample series:", e$message),
            type = "error"
          )
          return(NULL)
        }
      )
      if (!is.null(res)) {
        DBI::dbCommit(session$userData$AquaCache)
        showNotification("Sample series added successfully.", type = "message")
        getModuleData()
        updateSelectizeInput(session, "location", selected = character(0))
        updateSelectizeInput(session, "sub_location", selected = character(0))
        shinyWidgets::updateAirDateInput(
          session,
          "synch_from",
          clear = TRUE
        )
        shinyWidgets::updateAirDateInput(
          session,
          "synch_to",
          clear = TRUE
        )
        updateSelectizeInput(session, "default_owner", selected = character(0))
        updateSelectizeInput(
          session,
          "default_contributor",
          selected = character(0)
        )
        updateCheckboxInput(session, "active", value = TRUE)
        updateSelectizeInput(session, "source_fx", selected = character(0))
        updateSelectizeInput(
          session,
          "source_fx_secondary",
          selected = character(0)
        )
        source_args_existing(NA_character_)
        source_args_existing_source(NA_character_)
        source_args_secondary_existing(NA_character_)
        source_args_secondary_existing_source(NA_character_)
        updateTextAreaInput(session, "note", value = "")
      }
    })

    observeEvent(input$modify_sample_series, {
      if (input$mode != "modify") {
        showNotification(
          "Switch to 'Modify existing' mode before updating a sample series.",
          type = "error"
        )
        return()
      }
      if (is.null(selected_series())) {
        showNotification("Select a sample series to modify.", type = "error")
        return()
      }
      validate(
        need(input$location, "Please select a location."),
        need(input$source_fx, "Please select a source function."),
        need(input$default_owner, "Please select a default owner.")
      )
      synch_from_input <- input$synch_from
      synch_from <- scalar_utc_datetime(synch_from_input)
      if (
        !is.null(synch_from_input) &&
          length(synch_from_input) &&
          any(!is.na(synch_from_input)) &&
          is.na(synch_from)
      ) {
        showNotification("Invalid 'synchronize from' value.", type = "error")
        return()
      }
      synch_to_input <- input$synch_to
      synch_to <- scalar_utc_datetime(synch_to_input)
      if (
        !is.null(synch_to_input) &&
          length(synch_to_input) &&
          any(!is.na(synch_to_input)) &&
          is.na(synch_to)
      ) {
        showNotification("Invalid 'synchronize to' value.", type = "error")
        return()
      }
      source_assignments <- tryCatch(
        collect_source_assignments(),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
      if (is.null(source_assignments)) {
        return()
      }
      existing_assignment_count <- sum(
        moduleData$sample_series_source_assignments$sample_series_id ==
          selected_series()
      )
      if (existing_assignment_count > 2L) {
        showNotification(
          "This editor supports two assignments and will not overwrite a series that currently has more than two.",
          type = "error",
          duration = 10
        )
        return()
      }

      sub_loc <- if (is.null(input$sub_location)) {
        NA
      } else if (
        length(input$sub_location) == 0 || !nzchar(input$sub_location[1])
      ) {
        NA
      } else {
        as.numeric(input$sub_location[1])
      }

      contributor <- if (is.null(input$default_contributor)) {
        NA
      } else if (
        length(input$default_contributor) == 0 ||
          !nzchar(input$default_contributor[1])
      ) {
        NA
      } else {
        as.numeric(input$default_contributor[1])
      }

      DBI::dbBegin(session$userData$AquaCache)
      res <- tryCatch(
        {
          DBI::dbExecute(
            session$userData$AquaCache,
            "UPDATE discrete.sample_series
             SET location_id = $1, sub_location_id = $2, synch_from = $3,
                 synch_to = $4, default_owner = $5,
                 default_contributor = $6, active = $7, note = $8
             WHERE sample_series_id = $9;",
            params = list(
              input$location,
              sub_loc,
              synch_from,
              synch_to,
              input$default_owner,
              contributor,
              isTRUE(input$active),
              input$note,
              selected_series()
            )
          )
          DBI::dbExecute(
            session$userData$AquaCache,
            "DELETE FROM discrete.sample_series_source_adapters
             WHERE sample_series_id = $1",
            params = list(selected_series())
          )
          insert_source_assignments(selected_series(), source_assignments)

          # Re-synch the sample series
          AquaCache::synchronize_discrete(
            con = session$userData$AquaCache,
            sample_series_id = selected_series()
          )
          TRUE
        },
        error = function(e) {
          DBI::dbRollback(session$userData$AquaCache)
          showNotification(
            paste("Error modifying sample series:", e$message),
            type = "error"
          )
          FALSE
        }
      )
      if (isTRUE(res)) {
        DBI::dbCommit(session$userData$AquaCache)
        showNotification(
          "Sample series updated successfully.",
          type = "message"
        )
        getModuleData()
      }
    })
  })
}
