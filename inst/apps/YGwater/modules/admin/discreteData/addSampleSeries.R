# UI and server code for managing discrete sample series

addSampleSeriesUI <- function(id) {
  ns <- NS(id)

  tagList(
    page_fluid(
      uiOutput(ns("ui"))
    )
  )
}

addSampleSeries <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    moduleData <- reactiveValues()
    selected_series <- reactiveVal(NULL)
    moduleData$org_modal_target <- NULL

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

    parse_source_args <- function(arg_string) {
      if (is.null(arg_string) || !nzchar(arg_string)) {
        return(list(json = NA_character_, error = NULL))
      }
      parts <- strsplit(arg_string, ",")[[1]]
      args <- list()
      for (part in parts) {
        part <- trimws(part)
        colon <- regexpr(":", part, fixed = TRUE)
        if (colon == -1) {
          return(list(
            json = NULL,
            error = sprintf("Argument '%s' is missing a ':' separator.", part)
          ))
        }
        key <- trimws(substr(part, 1, colon - 1))
        value <- trimws(substr(part, colon + 1, nchar(part)))
        if (!nzchar(key) || !nzchar(value)) {
          return(list(
            json = NULL,
            error = sprintf(
              "Argument '%s' must include both key and value.",
              part
            )
          ))
        }
        args[[key]] <- value
      }
      list(json = jsonlite::toJSON(args, auto_unbox = TRUE), error = NULL)
    }

    format_source_args <- function(value) {
      if (
        is.null(value) ||
          length(value) == 0 ||
          all(is.na(value)) ||
          !nzchar(value)
      ) {
        return("")
      }
      parsed <- tryCatch(jsonlite::fromJSON(value), error = function(e) NULL)
      if (is.null(parsed)) {
        return(value)
      }
      if (length(parsed) == 0) {
        return("")
      }
      if (is.list(parsed) && !is.data.frame(parsed)) {
        parsed <- unlist(parsed)
      }
      if (is.null(names(parsed))) {
        return(paste(parsed, collapse = ", "))
      }
      entries <- paste(names(parsed), parsed, sep = ": ")
      paste(entries, collapse = ", ")
    }

    getModuleData <- function() {
      moduleData$sample_series <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sample_series_id, location_id, sub_location_id, synch_from, synch_to, default_owner, default_contributor, active, source_fx, source_fx_args, note FROM discrete.sample_series ORDER BY sample_series_id"
      )
      moduleData$sample_series_display <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT ss.sample_series_id, loc.name AS location, sl.sub_location_name AS sub_location, ss.synch_from, ss.synch_to, ss.active, ss.source_fx, owner.name AS default_owner, contrib.name AS default_contributor, ss.last_new_data, ss.last_synchronize, ss.note FROM discrete.sample_series ss JOIN public.locations loc ON ss.location_id = loc.location_id LEFT JOIN public.sub_locations sl ON ss.sub_location_id = sl.sub_location_id LEFT JOIN public.organizations owner ON ss.default_owner = owner.organization_id LEFT JOIN public.organizations contrib ON ss.default_contributor = contrib.organization_id ORDER BY loc.name ASC, sl.sub_location_name ASC"
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
    }

    getModuleData()

    choices <- ls(getNamespace("AquaCache"))
    moduleData$source_fx <- choices[grepl("^download", choices)]

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
          DT::DTOutput(ns("ss_table"))
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
              "Location",
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
            width = 6,
            textInput(
              ns("synch_from"),
              "Synchronize from (UTC, optional)",
              placeholder = "YYYY-MM-DD HH:MM"
            )
          ),
          column(
            width = 6,
            textInput(
              ns("synch_to"),
              "Synchronize to (UTC, optional)",
              placeholder = "YYYY-MM-DD HH:MM"
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
        splitLayout(
          cellWidths = c("50%", "50%"),
          verticalLayout(
            tags$div(
              class = "alert alert-info",
              "The source function is required and should correspond to a download-transform function provided by the AquaCache package."
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
            actionButton(
              ns("source_fx_doc"),
              "Open function documentation"
            )
          ),
          verticalLayout(
            tags$div(
              class = "alert alert-info",
              "Arguments must be formatted as key-value pairs for conversion to JSON, e.g. 'arg1: value1, arg2: value2'. Leave blank if not using a source_fx, otherwise refer to the function documentation in AquaCache."
            ),
            textInput(
              ns("source_fx_args"),
              "Source function arguments",
              value = "",
              placeholder = "arg1: value1, arg2: value2",
              width = "100%"
            ),
            actionButton(
              ns("args_example"),
              "Show example arguments"
            )
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

    table_proxy <- DT::dataTableProxy("ss_table")

    observeEvent(input$reload_module, {
      getModuleData()
      choices <- ls(getNamespace("AquaCache"))
      moduleData$source_fx <- choices[grepl("^download", choices)]
      updateSelectizeInput(
        session,
        "source_fx",
        choices = moduleData$source_fx
      )
      showNotification("Module reloaded", type = "message")
    })

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
          modalButton("Cancel"),
          actionButton(ns("save_org"), "Add organization")
        )
      ))
    }

    observeEvent(
      input$default_owner,
      {
        if (
          length(input$default_owner) == 0 || identical(input$default_owner, "")
        ) {
          return()
        }
        if (input$default_owner %in% moduleData$organizations$organization_id) {
          return()
        }
        prompt_new_org(input$default_owner, "default_owner")
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      input$default_contributor,
      {
        if (
          length(input$default_contributor) == 0 ||
            identical(input$default_contributor, "")
        ) {
          return()
        }
        if (
          input$default_contributor %in%
            moduleData$organizations$organization_id
        ) {
          return()
        }
        prompt_new_org(input$default_contributor, "default_contributor")
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
        df <- data.frame(
          name = input$org_name,
          name_fr = if (isTruthy(input$org_name_fr)) input$org_name_fr else NA,
          contact_name = if (isTruthy(input$contact_name)) {
            input$contact_name
          } else {
            NA
          },
          phone = if (isTruthy(input$contact_phone)) {
            input$contact_phone
          } else {
            NA
          },
          email = if (isTruthy(input$contact_email)) {
            input$contact_email
          } else {
            NA
          },
          note = if (isTruthy(input$contact_note)) input$contact_note else NA
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
          "SELECT organization_id, name FROM organizations ORDER BY name ASC"
        )
        updateSelectizeInput(
          session,
          "default_owner",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          )
        )
        updateSelectizeInput(
          session,
          "default_contributor",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          )
        )
        new_id <- moduleData$organizations[
          moduleData$organizations$name == df$name,
          "organization_id"
        ]
        if (!length(new_id)) {
          new_id <- moduleData$organizations$organization_id[1]
        }
        if (identical(moduleData$org_modal_target, "default_owner")) {
          updateSelectizeInput(session, "default_owner", selected = new_id)
        } else if (
          identical(moduleData$org_modal_target, "default_contributor")
        ) {
          updateSelectizeInput(
            session,
            "default_contributor",
            selected = new_id
          )
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
        updateTextInput(
          session,
          "synch_from",
          value = format_datetime_input(details$synch_from)
        )
        updateTextInput(
          session,
          "synch_to",
          value = format_datetime_input(details$synch_to)
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
        updateSelectizeInput(
          session,
          "source_fx",
          selected = if (is.na(details$source_fx)) {
            character(0)
          } else {
            details$source_fx
          }
        )
        updateTextInput(
          session,
          "source_fx_args",
          value = format_source_args(details$source_fx_args)
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
      examples <- moduleData$sample_series[
        moduleData$sample_series$source_fx == input$source_fx,
        "source_fx_args"
      ]
      examples <- examples[!is.na(examples)][1:5]
      if (!length(examples)) {
        showModal(modalDialog(
          "No example arguments found in existing sample series. Refer to the AquaCache package documentation for details on the required arguments.",
          easyClose = TRUE
        ))
        return()
      }
      examples <- unique(vapply(examples, format_source_args, character(1)))
      showModal(modalDialog(
        title = paste("Example arguments for", input$source_fx),
        paste(examples, collapse = "\n\n"),
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
      out <- file.path(.rd_dir, paste0(input$source_fx, ".html"))
      tools::Rd2HTML(
        package[[file]],
        out,
        no_links = TRUE,
        package = "AquaCache"
      )
      url <- paste0("/rdocs/", basename(out))
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
      synch_from_input <- input$synch_from
      if (is.null(synch_from_input)) {
        synch_from_input <- ""
      }
      synch_from <- parse_datetime_input(synch_from_input)
      if (nzchar(synch_from_input) && is.na(synch_from)) {
        showNotification("Invalid 'synchronize from' value.", type = "error")
        return()
      }
      synch_to_input <- input$synch_to
      if (is.null(synch_to_input)) {
        synch_to_input <- ""
      }
      synch_to <- parse_datetime_input(synch_to_input)
      if (nzchar(synch_to_input) && is.na(synch_to)) {
        showNotification("Invalid 'synchronize to' value.", type = "error")
        return()
      }
      args <- parse_source_args(input$source_fx_args)
      if (!is.null(args$error)) {
        showNotification(args$error, type = "error")
        return()
      }
      DBI::dbBegin(session$userData$AquaCache)
      sub_loc <- if (
        length(input$sub_location) == 0 || !nzchar(input$sub_location[1])
      ) {
        NA
      } else {
        as.numeric(input$sub_location[1])
      }
      sql <- DBI::sqlInterpolate(
        session$userData$AquaCache,
        "INSERT INTO discrete.sample_series (location_id, sub_location_id, synch_from, synch_to, default_owner, default_contributor, active, source_fx, source_fx_args, note) VALUES (?loc, ?sub_loc, ?synch_from, ?synch_to, ?default_owner, ?default_contributor, ?active, ?source_fx, CAST(?source_fx_args AS jsonb), ?note) RETURNING sample_series_id;",
        loc = as.numeric(input$location),
        sub_loc = sub_loc,
        synch_from = synch_from,
        synch_to = synch_to,
        default_owner = as.numeric(input$default_owner),
        default_contributor = if (
          length(input$default_contributor) == 0 ||
            !nzchar(input$default_contributor[1])
        ) {
          NA
        } else {
          as.numeric(input$default_contributor[1])
        },
        active = isTRUE(input$active),
        source_fx = input$source_fx,
        source_fx_args = args$json,
        note = if (isTruthy(input$note)) input$note else NA
      )
      res <- tryCatch(
        DBI::dbGetQuery(session$userData$AquaCache, sql),
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
        updateTextInput(session, "synch_from", value = "")
        updateTextInput(session, "synch_to", value = "")
        updateSelectizeInput(session, "default_owner", selected = character(0))
        updateSelectizeInput(
          session,
          "default_contributor",
          selected = character(0)
        )
        updateCheckboxInput(session, "active", value = TRUE)
        updateSelectizeInput(session, "source_fx", selected = character(0))
        updateTextInput(session, "source_fx_args", value = "")
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
      if (is.null(synch_from_input)) {
        synch_from_input <- ""
      }
      synch_from <- parse_datetime_input(synch_from_input)
      if (nzchar(synch_from_input) && is.na(synch_from)) {
        showNotification("Invalid 'synchronize from' value.", type = "error")
        return()
      }
      synch_to_input <- input$synch_to
      if (is.null(synch_to_input)) {
        synch_to_input <- ""
      }
      synch_to <- parse_datetime_input(synch_to_input)
      if (nzchar(synch_to_input) && is.na(synch_to)) {
        showNotification("Invalid 'synchronize to' value.", type = "error")
        return()
      }
      args <- parse_source_args(input$source_fx_args)
      if (!is.null(args$error)) {
        showNotification(args$error, type = "error")
        return()
      }
      DBI::dbBegin(session$userData$AquaCache)
      sub_loc <- if (
        length(input$sub_location) == 0 || !nzchar(input$sub_location[1])
      ) {
        NA
      } else {
        as.numeric(input$sub_location[1])
      }
      sql <- DBI::sqlInterpolate(
        session$userData$AquaCache,
        "UPDATE discrete.sample_series SET location_id = ?loc, sub_location_id = ?sub_loc, synch_from = ?synch_from, synch_to = ?synch_to, default_owner = ?default_owner, default_contributor = ?default_contributor, active = ?active, source_fx = ?source_fx, source_fx_args = CAST(?source_fx_args AS jsonb), note = ?note WHERE sample_series_id = ?id;",
        loc = as.numeric(input$location),
        sub_loc = sub_loc,
        synch_from = synch_from,
        synch_to = synch_to,
        default_owner = as.numeric(input$default_owner),
        default_contributor = if (
          length(input$default_contributor) == 0 ||
            !nzchar(input$default_contributor[1])
        ) {
          NA
        } else {
          as.numeric(input$default_contributor[1])
        },
        active = isTRUE(input$active),
        source_fx = input$source_fx,
        source_fx_args = args$json,
        note = if (isTruthy(input$note)) input$note else NA,
        id = as.numeric(selected_series())
      )
      res <- tryCatch(
        {
          DBI::dbExecute(session$userData$AquaCache, sql)
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
