# UI and server code for adding/modifying image series

addImgSeriesUI <- function(id) {
  ns <- NS(id)

  tagList(
    page_fluid(
      uiOutput(ns("banner")),
      uiOutput(ns("ui"))
    )
  )
}

addImgSeries <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addImgSeries"
      )
    })

    moduleData <- reactiveValues()
    source_args_existing <- reactiveVal(NA_character_)
    source_args_existing_source <- reactiveVal(NA_character_)
    source_args_secondary_existing <- reactiveVal(NA_character_)
    source_args_secondary_existing_source <- reactiveVal(NA_character_)

    getModuleData <- function() {
      moduleData$image_series <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT img_series_id, description, location_id, active, share_with,
                owner
         FROM files.image_series;"
      )
      moduleData$image_series_source_assignments <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT image_series_source_adapter_id, img_series_id, source_fx,
                source_fx_args, fetch_priority, active, note
         FROM files.image_series_source_adapters
         ORDER BY img_series_id, fetch_priority,
                  image_series_source_adapter_id"
      )
      moduleData$image_series_display <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT i.img_series_id, l.name AS location, o.name AS owner,
                source.source_fx, i.active
         FROM files.image_series i
         INNER JOIN public.locations l ON i.location_id = l.location_id
         INNER JOIN public.organizations o ON i.owner = o.organization_id
         LEFT JOIN LATERAL (
           SELECT isa.source_fx
           FROM files.image_series_source_adapters isa
           WHERE isa.img_series_id = i.img_series_id AND isa.active
           ORDER BY isa.fetch_priority, isa.image_series_source_adapter_id
           LIMIT 1
         ) source ON TRUE;"
      )
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM public.organizations"
      )
      moduleData$locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT location_id, location_code AS location, name, latitude, longitude FROM public.locations"
      )
      moduleData$users <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('files.images');"
      ) # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table
      moduleData$source_adapters <- AquaCache::getSourceAdapterCapabilities(
        con = session$userData$AquaCache,
        data_domain = "image"
      )
      moduleData$source_fx <- sort(unique(
        as.character(moduleData$source_adapters$source_fx)
      ))
    }

    getModuleData() # Initial data load

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

    secondary_source_capability <- reactive({
      source_adapter_capability_row(
        moduleData$source_adapters,
        input$source_fx_secondary
      )
    })

    secondary_stored_source_args <- reactive({
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
      capability <- secondary_source_capability()
      if (is.null(capability)) {
        return(tags$div(
          class = "alert alert-secondary",
          "Select a secondary source function to see its arguments."
        ))
      }
      source_adapter_argument_ui(
        ns,
        capability,
        secondary_stored_source_args(),
        input_prefix = "secondary_"
      )
    })

    collect_source_assignments <- function() {
      rows <- list()
      add_assignment <- function(source_fx, args, priority, active) {
        if (
          is.null(source_fx) ||
            !length(source_fx) ||
            is.na(source_fx[[1L]]) ||
            !nzchar(source_fx[[1L]])
        ) {
          return()
        }
        rows[[length(rows) + 1L]] <<- data.frame(
          source_fx = as.character(source_fx[[1L]]),
          source_fx_args = args,
          fetch_priority = as.integer(priority),
          active = isTRUE(active),
          note = NA_character_,
          stringsAsFactors = FALSE
        )
      }
      add_assignment(
        input$source_fx,
        collect_source_fx_args(),
        input$source_fetch_priority,
        input$source_assignment_active
      )
      add_assignment(
        input$source_fx_secondary,
        source_adapter_args_json(source_adapter_collect_args(
          input,
          secondary_source_capability(),
          secondary_stored_source_args(),
          input_prefix = "secondary_"
        )),
        input$source_secondary_fetch_priority,
        input$source_secondary_assignment_active
      )
      if (!length(rows)) {
        stop("Please configure at least one image source assignment.")
      }
      assignments <- do.call(rbind, rows)
      active_priorities <- assignments$fetch_priority[assignments$active]
      if (anyDuplicated(active_priorities)) {
        stop(
          "Active image source assignments must have unique fetch priorities."
        )
      }
      assignments
    }

    insert_source_assignments <- function(con, img_series_id, assignments) {
      for (i in seq_len(nrow(assignments))) {
        DBI::dbExecute(
          con,
          "INSERT INTO files.image_series_source_adapters (
             img_series_id, source_fx, source_fx_args, fetch_priority,
             active, note
           ) VALUES ($1, $2, $3::jsonb, $4, $5, $6)",
          params = list(
            img_series_id,
            assignments$source_fx[[i]],
            assignments$source_fx_args[[i]],
            assignments$fetch_priority[[i]],
            assignments$active[[i]],
            assignments$note[[i]]
          )
        )
      }
    }

    output$ui <- renderUI({
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
          DT::DTOutput(ns("series_table"))
        ),
        tags$head(tags$style(HTML(
          ".shiny-split-layout > div {overflow: visible;}"
        ))),
        selectizeInput(
          ns("location"),
          "Location (add new under the 'locations' menu)",
          choices = stats::setNames(
            moduleData$locations$location_id,
            moduleData$locations$name
          ),
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = 'Select a location'),
          width = "100%"
        ),
        selectizeInput(
          ns("owner"),
          "Owner",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = 'Select owner'),
          width = "100%"
        ),
        selectizeInput(
          ns("share_with"),
          "Share with groups (1 or more, type your own if not in list)",
          choices = moduleData$users$role_name,
          selected = "public_reader",
          multiple = TRUE,
          width = "100%"
        ),
        tags$div(
          class = "alert alert-info",
          "Configure a primary image source and, optionally, retain a secondary route. The active assignment with the highest fetch priority (lowest number) is used. Leave blank if entering data manually or using other methods. For more information refer to the AquaCache package documentation."
        ),
        fluidRow(
          column(
            width = 6,
            tags$h5("Primary source assignment"),
            selectizeInput(
              ns("source_fx"),
              "Source function (see AquaCache package documentation for details)",
              choices = moduleData$source_fx,
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = 'Select source function (optional)'
              ),
              width = "100%"
            ),
            tags$p(
              class = "text-muted small",
              "Missing download function? Download functions must be ",
              "registered in the database's ",
              tags$code("public.source_adapter_capabilities"),
              " table for the image domain to show up here. Developers: see AquaCache::registerSourceAdapterArguments()."
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
            numericInput(
              ns("source_fetch_priority"),
              "Fetch priority",
              1,
              min = 1,
              step = 1
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
            selectizeInput(
              ns("source_fx_secondary"),
              "Secondary source function",
              choices = moduleData$source_fx,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            ),
            checkboxInput(
              ns("source_secondary_assignment_active"),
              "Assignment active",
              FALSE
            ),
            numericInput(
              ns("source_secondary_fetch_priority"),
              "Fetch priority",
              2,
              min = 1,
              step = 1
            ),
            uiOutput(ns("source_fx_secondary_args_ui"))
          )
        ),
        textAreaInput(
          ns("description"),
          "Description (optional)",
          value = "",
          rows = 3,
          placeholder = "Any additional information about this image series (optional)",
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          dateInput(
            ns("start_date"),
            "Start date to search for images",
            value = Sys.Date() - 30,
            format = "yyyy-mm-dd",
            width = "100%"
          ),
          bslib::input_task_button(ns("add_series"), label = "Add image series")
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          checkboxInput(ns("active"), "Active", value = FALSE),
          bslib::input_task_button(
            ns("modify_series"),
            label = "Modify image series"
          )
        )
      )
    })

    # Render the timeseries table for modification
    output$series_table <- DT::renderDT({
      DT::datatable(
        moduleData$image_series_display,
        selection = "single",
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)), # Hide the id column
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
        ),
        rownames = FALSE
      )
    }) |>
      bindEvent(moduleData$image_series_display)

    selected_series <- reactiveVal(NULL)

    observeEvent(input$reload_module, {
      getModuleData()
      selected_series(NULL)
      # Clear table row selection
      DT::dataTableProxy("series_table") |> DT::selectRows(NULL)
      updateSelectizeInput(
        session,
        "location",
        choices = stats::setNames(
          moduleData$locations$location_id,
          moduleData$locations$name
        )
      )
      updateSelectizeInput(
        session,
        "owner",
        choices = stats::setNames(
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        )
      )
      updateSelectizeInput(
        session,
        "share_with",
        choices = moduleData$users$role_name
      )
      updateSelectizeInput(session, "source_fx", choices = moduleData$source_fx)
      updateSelectizeInput(
        session,
        "source_fx_secondary",
        choices = moduleData$source_fx
      )
      showNotification("Module reloaded", type = "message")
    })

    observeEvent(input$series_table_rows_selected, {
      sel <- input$series_table_rows_selected
      if (length(sel) > 0) {
        selected_series(moduleData$image_series_display[sel, ])
        image_series_id <- selected_series()$img_series_id[[1L]]
        # Fetch details for the selected series
        details <- moduleData$image_series[
          moduleData$image_series$img_series_id == image_series_id,
          ,
          drop = FALSE
        ]

        updateSelectizeInput(
          session,
          "location",
          selected = details$location_id
        )
        updateSelectizeInput(session, "owner", selected = details$owner)
        updateSelectizeInput(
          session,
          "share_with",
          selected = details$share_with
        )
        updateTextAreaInput(session, "description", value = details$description)
        assignments <- moduleData$image_series_source_assignments[
          moduleData$image_series_source_assignments$img_series_id ==
            image_series_id,
          ,
          drop = FALSE
        ]
        if (nrow(assignments) > 2L) {
          showNotification(
            "This image series has more than two source assignments. The first two are shown; consolidate them before modifying this series here.",
            type = "warning",
            duration = 10
          )
        }
        primary <- if (nrow(assignments)) {
          assignments[1, , drop = FALSE]
        } else {
          NULL
        }
        secondary <- if (nrow(assignments) >= 2L) {
          assignments[2, , drop = FALSE]
        } else {
          NULL
        }
        source_args_existing(
          if (is.null(primary)) NA_character_ else primary$source_fx_args
        )
        source_args_existing_source(
          if (is.null(primary)) {
            NA_character_
          } else {
            as.character(primary$source_fx)
          }
        )
        updateSelectizeInput(
          session,
          "source_fx",
          selected = if (is.null(primary)) character(0) else primary$source_fx
        )
        updateCheckboxInput(
          session,
          "source_assignment_active",
          value = is.null(primary) || isTRUE(primary$active)
        )
        updateNumericInput(
          session,
          "source_fetch_priority",
          value = if (is.null(primary)) 1 else primary$fetch_priority
        )
        source_args_secondary_existing(
          if (is.null(secondary)) NA_character_ else secondary$source_fx_args
        )
        source_args_secondary_existing_source(
          if (is.null(secondary)) {
            NA_character_
          } else {
            as.character(secondary$source_fx)
          }
        )
        updateSelectizeInput(
          session,
          "source_fx_secondary",
          selected = if (is.null(secondary)) {
            character(0)
          } else {
            secondary$source_fx
          }
        )
        updateCheckboxInput(
          session,
          "source_secondary_assignment_active",
          value = is.null(secondary) || isTRUE(secondary$active)
        )
        updateNumericInput(
          session,
          "source_secondary_fetch_priority",
          value = if (is.null(secondary)) 2 else secondary$fetch_priority
        )
        updateCheckboxInput(
          session,
          "active",
          value = as.logical(details$active)
        )
      } else {
        selected_series(NULL)
        source_args_existing(NA_character_)
        source_args_existing_source(NA_character_)
        source_args_secondary_existing(NA_character_)
        source_args_secondary_existing_source(NA_character_)
      }
    })

    ### Observe the owner selectizeInput for new owners ############
    observeEvent(
      input$owner,
      {
        if (
          input$owner %in%
            moduleData$organizations$organization_id ||
            nchar(input$owner) == 0
        ) {
          return()
        }
        showModal(modalDialog(
          textInput(ns("owner_name"), "Owner name", value = input$owner),
          textInput(ns("owner_name_fr"), "Owner name French (optional)"),
          textInput(ns("contact_name"), "Contact name (optional)"),
          textInput(ns("contact_phone"), "Contact phone (optional)"),
          textInput(ns("contact_email"), "Contact email (optional)"),
          textInput(ns("contact_note"), "Contact note (optional, for context)"),
          actionButton(ns("add_owner"), "Add owner")
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$add_owner,
      {
        # Check that mandatory fields are filled in
        if (!isTruthy(input$owner_name)) {
          shinyjs::js$backgroundCol(ns("owner_name"), "#fdd")
          return()
        }
        # Add the owner to the database
        df <- data.frame(
          name = input$owner_name,
          name_fr = if (isTruthy(input$owner_name_fr)) {
            input$owner_name_fr
          } else {
            NA
          },
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

        # Update the moduleData reactiveValues
        moduleData$organizations <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT organization_id, name FROM public.organizations"
        )
        # Update the selectizeInput to the new value
        updateSelectizeInput(
          session,
          "owner",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          selected = moduleData$organizations[
            moduleData$organizations$name == df$name,
            "organization_id"
          ]
        )
        removeModal()
        showModal(modalDialog(
          "New owner added.",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ### Observe the share_with selectizeInput ##############################
    observeEvent(
      input$share_with,
      {
        if (
          length(input$share_with) > 1 & 'public_reader' %in% input$share_with
        ) {
          showModal(modalDialog(
            "If public_reader is selected it must be the only group selected.",
            easyClose = TRUE
          ))
          updateSelectizeInput(
            session,
            "share_with",
            selected = "public_reader"
          )
        }
      },
      ignoreInit = TRUE,
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

      ex_args <- moduleData$image_series_source_assignments[
        moduleData$image_series_source_assignments$source_fx == input$source_fx,
        "source_fx_args"
      ]
      ex_args <- ex_args[!is.na(ex_args)]
      ex_args <- ex_args[nzchar(ex_args)]
      ex_args <- utils::head(ex_args, 10)
      ex_args <- unique(vapply(ex_args, parse_source_args, character(1)))
      ex_args <- ex_args[nzchar(ex_args)]

      showModal(modalDialog(
        title = paste("Example arguments for", input$source_fx),
        if (length(ex_args) > 0) {
          tags$pre(paste(ex_args, collapse = "\n"))
        } else {
          paste(
            "No example arguments found in existing image series.",
            "Please refer to the AquaCache package documentation for",
            "details on the required arguments."
          )
        },
        easyClose = TRUE
      ))
    })

    observeEvent(input$source_fx_doc, {
      if (is.null(input$source_fx) || input$source_fx == "") {
        showModal(modalDialog(
          "Please select a source function to see its documentation.",
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

      url <- rdoc_url(session, basename(out))
      shinyjs::runjs(sprintf("window.open('%s','_blank');", url))
    })

    # Add a new image series
    # Create an extendedTask to add new, since the data pull might take a very long time
    addNewSeries <- ExtendedTask$new(
      function(
        config,
        loc,
        share_with,
        owner,
        description,
        source_assignments,
        start
      ) {
        promises::future_promise({
          tryCatch(
            {
              # Make a connection
              con <- AquaConnect(
                name = config$dbName,
                host = config$dbHost,
                port = config$dbPort,
                username = config$dbUser,
                password = config$dbPass,
                silent = TRUE
              )
              on.exit(DBI::dbDisconnect(con)) # Disconnect when done

              # start a transaction
              DBI::dbBegin(con)

              df <- data.frame(
                location_id = loc,
                owner = owner,
                description = if (isTruthy(description)) description else NA,
                share_with = paste0(
                  "{",
                  paste(share_with, collapse = ", "),
                  "}"
                ),
                active = TRUE,
                last_img = start
              )
              print(df)

              new_id <- DBI::dbGetQuery(
                con,
                "INSERT INTO files.image_series (
                   location_id, owner, description, share_with, active, last_img
                 ) VALUES ($1, $2, $3, $4, $5, $6)
                 RETURNING img_series_id;",
                params = list(
                  df$location_id,
                  df$owner,
                  df$description,
                  DBI::SQL(df$share_with),
                  df$active,
                  df$last_img
                )
              )[1, 1]
              for (i in seq_len(nrow(source_assignments))) {
                DBI::dbExecute(
                  con,
                  "INSERT INTO files.image_series_source_adapters (
                     img_series_id, source_fx, source_fx_args,
                     fetch_priority, active, note
                   ) VALUES ($1, $2, $3::jsonb, $4, $5, $6)",
                  params = list(
                    new_id,
                    source_assignments$source_fx[[i]],
                    source_assignments$source_fx_args[[i]],
                    source_assignments$fetch_priority[[i]],
                    source_assignments$active[[i]],
                    source_assignments$note[[i]]
                  )
                )
              }

              # fetch images
              AquaCache::getNewImages(image_series_ids = new_id, con = con)

              # Find the actual earliest found image and update image_series$first_img with that datetime
              earliest <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT MIN(datetime) FROM files.images WHERE img_series_id = ",
                  new_id
                )
              )[1, 1]

              if (is.na(earliest)) {
                stop("No images could be found")
              } else {
                DBI::dbExecute(
                  con,
                  paste0(
                    "UPDATE files.image_series SET first_img = '",
                    earliest,
                    "' WHERE img_series_id = ",
                    new_id
                  )
                )
              }

              DBI::dbCommit(con)
              return("success")
            },
            error = function(e) {
              DBI::dbRollback(con)
              DBI::dbDisconnect(con)
              return(paste("Error adding image series:", e$message))
            },
            warning = function(w) {
              DBI::dbRollback(con)
              DBI::dbDisconnect(con)
              return(paste("Error adding image series:", w$message))
            }
          ) # End of tryCatch
        }) # End of promise
      } # End of extendedTask$new
    ) |>
      bslib::bind_task_button("add_series")

    observeEvent(input$add_series, {
      # validate inputs
      validate(
        need(input$location, "Please select a location"),
        need(input$owner, "Please select an owner"),
        need(
          input$share_with,
          "Please select at least one group to share with"
        ),
        need(input$source_fx, "Please select a source function")
      )

      if (input$mode != "add") {
        showNotification(
          "Please select 'Add new' mode to add a timeseries.",
          type = "error"
        )
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

      showNotification(
        "Please be patient. This could take a **very** long time is fetching many images",
        type = "message"
      )

      # Call the extendedTask to add new image series
      addNewSeries$invoke(
        config = session$userData$config,
        loc = input$location,
        owner = input$owner,
        description = input$description,
        share_with = input$share_with,
        source_assignments = source_assignments,
        start = as.character(input$start_date)
      )
    })

    # Observe the result of the ExtendedTask
    observeEvent(addNewSeries$result(), {
      if (is.null(addNewSeries$result())) {
        return() # No result yet, do nothing
      } else if (addNewSeries$result() != "success") {
        # If the result is not "success", show an error notification
        showNotification(addNewSeries$result(), type = "error")
        return()
      } else {
        # If the result is "success", show a success notification
        showNotification("Image series added successfully!", type = "message")

        getModuleData()

        # Reset all fields
        updateSelectizeInput(session, "location", selected = character(0))
        updateSelectizeInput(session, "owner", selected = character(0))
        updateSelectizeInput(session, "share_with", selected = "public_reader")
        updateTextAreaInput(session, "description", value = "")
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
        updateCheckboxInput(session, "active", value = FALSE)
      }
    })

    # modify existing image series
    observeEvent(
      input$modify_series,
      {
        if (input$mode != "modify") {
          # This is an error: show the user a notification to select 'modify' mode
          showNotification(
            "Please select 'Modify existing' mode to modify a series",
            type = "error"
          )
          return()
        }
        # If we are modifying an existing timeseries, we need to check if it exists
        selected_row <- input$series_table_rows_selected
        if (is.null(selected_row) || length(selected_row) != 1) {
          showNotification(
            "Please select a single timeseries to modify.",
            type = "error"
          )
          return()
        }
        id <- moduleData$image_series_display[selected_row, "img_series_id"]
        existing_assignment_count <- sum(
          moduleData$image_series_source_assignments$img_series_id == id
        )
        if (existing_assignment_count > 2L) {
          showNotification(
            "This editor supports two assignments and will not overwrite an image series that currently has more than two.",
            type = "error",
            duration = 10
          )
          return()
        }
        selected_series <- moduleData$image_series[
          moduleData$image_series$img_series_id == id,
        ]
        # Check if the series already exists
        existing_timeseries <- DBI::dbGetQuery(
          session$userData$AquaCache,
          paste0(
            "SELECT * FROM files.image_series WHERE img_series_id = ",
            selected_series$img_series_id
          )
        )
        if (nrow(existing_timeseries) == 0) {
          showNotification(
            "Selected image_series does not exist in the database.",
            type = "error"
          )
          return()
        }

        submitted_source_assignments <- tryCatch(
          collect_source_assignments(),
          error = function(e) {
            showNotification(e$message, type = "error")
            NULL
          }
        )
        if (is.null(submitted_source_assignments)) {
          return()
        }

        # If it exists, update the image series
        DBI::dbBegin(session$userData$AquaCache)

        tryCatch(
          {
            if (input$location != selected_series$location_id) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE files.image_series SET location_id = $1 WHERE img_series_id = $2;",
                params = list(input$location, selected_series$img_series_id)
              )
            }

            if (input$owner != selected_series$owner) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE files.image_series SET owner = $1 WHERE img_series_id = $2;",
                params = list(input$owner, selected_series$img_series_id)
              )
            }

            if (input$description != selected_series$description) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE files.image_series SET description = $1 WHERE img_series_id = $2;",
                params = list(
                  if (isTruthy(input$description)) input$description else NA,
                  selected_series$img_series_id
                )
              )
            }

            # Changes to share_with
            if (
              !paste0("{", paste(input$share_with, collapse = ","), "}") ==
                selected_series$share_with
            ) {
              share_with_sql <- DBI::SQL(paste0(
                "{",
                paste(input$share_with, collapse = ", "),
                "}"
              ))
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE files.image_series SET share_with = {share_with_sql} WHERE img_series_id = {selected_series$img_series_id};",
                  .con = session$userData$AquaCache
                )
              )
            }

            DBI::dbExecute(
              session$userData$AquaCache,
              "DELETE FROM files.image_series_source_adapters
               WHERE img_series_id = $1",
              params = list(selected_series$img_series_id)
            )
            insert_source_assignments(
              session$userData$AquaCache,
              selected_series$img_series_id,
              submitted_source_assignments
            )

            DBI::dbCommit(session$userData$AquaCache)
            showNotification(
              "Image series updated successfully!",
              type = "message"
            )
            getModuleData()
          },
          error = function(e) {
            DBI::dbRollback(session$userData$AquaCache)
            showNotification(
              paste("Error updating image series:", e$message),
              type = "error"
            )
          }
        )
      },
      ignoreInit = TRUE
    )
  }) # End of moduleServer
}
