# UI and server code for add new location module

addTimeseriesUI <- function(id) {
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
      uiOutput(ns("banner")),
      uiOutput(ns("ui"))
    )
  )
}

addTimeseries <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addTimeseries"
      )
    })

    moduleData <- reactiveValues()

    getModuleData <- function() {
      moduleData$timeseries <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT ts.timeseries_id, ts.location_id, ts.sub_location_id, ts.timezone_daily_calc, lz.z_meters AS z, ts.z_id, ts.media_id, ts.parameter_id, ts.aggregation_type_id, ts.sensor_priority, ts.default_owner, ts.record_rate, ts.share_with, ts.source_fx, ts.source_fx_args, ts.note FROM timeseries ts LEFT JOIN public.locations_z lz ON ts.location_id = lz.location_id"
      )
      moduleData$locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT l.location_id, l.location, l.name, lt.type, l.latitude, l.longitude FROM locations l INNER JOIN location_types lt ON l.location_type = lt.type_id ORDER BY l.name ASC"
      )
      moduleData$sub_locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sub_location_id, sub_location_name, location_id FROM sub_locations ORDER BY sub_location_name ASC"
      )
      moduleData$parameters <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT parameter_id, param_name FROM parameters ORDER BY param_name ASC"
      )
      moduleData$media <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT media_id, media_type FROM media_types ORDER BY media_type ASC"
      )
      moduleData$aggregation_types <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT aggregation_type_id, aggregation_type FROM aggregation_types ORDER BY aggregation_type ASC"
      )
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM organizations ORDER BY name ASC"
      )
      moduleData$users <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('continuous.timeseries') ORDER BY role_name ASC;"
      ) # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table

      moduleData$timeseries_display <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT ts.timeseries_id, l.name AS location_name, sl.sub_location_name, ts.timezone_daily_calc AS time_zone, p.param_name AS parameter, m.media_type AS media, at.aggregation_type, lz.z_meters AS depth_height_m, ts.sensor_priority, o.name AS owner, ts.record_rate
        FROM timeseries ts
        INNER JOIN locations l ON ts.location_id = l.location_id
        LEFT JOIN sub_locations sl ON ts.sub_location_id = sl.sub_location_id
        LEFT JOIN locations_z lz ON ts.z_id = lz.z_id
        INNER JOIN parameters p ON ts.parameter_id = p.parameter_id
        INNER JOIN media_types m ON ts.media_id = m.media_id
        INNER JOIN aggregation_types at ON ts.aggregation_type_id = at.aggregation_type_id
        INNER JOIN organizations o ON ts.default_owner = o.organization_id
        "
      )
    }

    getModuleData() # Initial data load

    choices <- ls(getNamespace("AquaCache"))
    moduleData$source_fx <- choices[grepl("^download", choices)]

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
          accordion(
            id = ns("accordion1"),
            open = "timeseries_table_panel",
            accordion_panel(
              id = ns("timeseries_table_panel"),
              title = "Select timeseries to modify",
              DT::DTOutput(ns("ts_table"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          tags$div(
            class = "alert alert-info",
            "Tip: if you add a new timeseries with a source_fx and appropriate arguments, data will automatically be fetched from the source when you click 'Add timeseries'. If you leave the source_fx blank, you can enter data manually or use other methods. Note that WSC timeseries will get daily mean measurements as well as realtime measurements as far back as exist."
          ),
        ),
        fluidRow(
          column(
            4,
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
            )
          ),
          column(
            4,
            selectizeInput(
              ns("sub_location"),
              "Sub-location (add new under the 'locations' menu)",
              choices = stats::setNames(
                moduleData$sub_locations$sub_location_id,
                moduleData$sub_locations$sub_location_name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = 'Optional'),
              width = "100%"
            )
          ),
          column(
            4,
            selectizeInput(
              ns("tz"),
              "Timezone for daily aggregation",
              choices = c(-12:14),
              selected = -7 # Default to MST (UTC-7)
            )
          )
        ),
        checkboxInput(
          ns("z_specify"),
          "Specify an elevation or depth?",
          value = FALSE
        ),
        numericInput(
          ns("z"),
          "Elevation or depth, m (signed appropriately)",
          value = NA,
          min = -1000,
          max = 10000,
          width = "100%"
        ),

        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
          selectizeInput(
            ns("parameter"),
            "Parameter",
            choices = stats::setNames(
              moduleData$parameters$parameter_id,
              moduleData$parameters$param_name
            ),
            multiple = TRUE,
            options = list(maxItems = 1, placeholder = 'Select a parameter'),
            width = "100%"
          ),
          selectizeInput(
            ns("media"),
            "Media",
            choices = stats::setNames(
              moduleData$media$media_id,
              moduleData$media$media_type
            ),
            multiple = TRUE,
            options = list(maxItems = 1, placeholder = 'Select media type'),
            width = "100%"
          )
        ),
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
          selectizeInput(
            ns("aggregation_type"),
            "Aggregation type",
            choices = stats::setNames(
              moduleData$aggregation_types$aggregation_type_id,
              moduleData$aggregation_types$aggregation_type
            ),
            multiple = TRUE,
            options = list(
              maxItems = 1,
              placeholder = 'Select aggregation type'
            ),
            width = "100%"
          ),
          textInput(
            ns("record_rate"),
            "Rough record rate (5 minutes, 1 hour, 1 day, 1 week, etc.)",
            value = "",
            width = "100%"
          )
        ),
        selectizeInput(
          ns("sensor_priority"),
          "Sensor priority",
          choices = c("Primary" = 1, "Secondary" = 2, "Tertiary" = 3),
          selected = 1,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = 'Select sensor priority'),
          width = "100%"
        ),
        selectizeInput(
          ns("default_owner"),
          "Default owner (type your own if not in list)",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = 'Select default owner',
            create = TRUE
          ),
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
        splitLayout(
          cellWidths = c("50%", "50%"),
          verticalLayout(
            # htmlOutput to tell the user when they should use the source functions and what the arguments are
            tags$div(
              class = "alert alert-info",
              "The source function is used to download data using the AquaCache R package. Leave blank if entering data manually or using other methods. For more information refer to the AquaCache package documentation."
            ),
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
            actionButton(
              ns("source_fx_doc"),
              "Open function documentation",
            )
          ),
          verticalLayout(
            # htmlOutput to tell the user how the source function arguments should be formatted
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
          value = "",
          rows = 3,
          placeholder = "Any additional information about this timeseries (optional)",
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          bslib::input_task_button(
            ns("add_timeseries"),
            label = "Add timeseries"
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          bslib::input_task_button(
            ns("modify_timeseries"),
            label = "Modify timeseries"
          )
        )
      )
    })

    # Render the timeseries table for modification
    output$ts_table <- DT::renderDT({
      # Convert some data types to factors for better filtering in DT
      df <- moduleData$timeseries_display
      df$location_name <- as.factor(df$location_name)
      df$record_rate <- as.factor(df$record_rate)
      df$media <- as.factor(df$media)
      df$aggregation_type <- as.factor(df$aggregation_type)
      df$parameter <- as.factor(df$parameter)
      df$owner <- as.factor(df$owner)
      df$sensor_priority <- as.factor(df$sensor_priority)
      df$time_zone <- as.factor(df$time_zone)
      df$z_id <- NULL # remove z_id as it's not useful to the user

      DT::datatable(
        df,
        selection = "single",
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)), # hide the id column
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
        filter = 'top',
        rownames = FALSE
      )
    }) |>
      bindEvent(moduleData$timeseries_display)

    selected_tsid <- reactiveVal(NULL)

    observeEvent(
      input$reload_module,
      {
        getModuleData()
        selected_tsid(NULL)
        # Clear table row selection
        DT::dataTableProxy("ts_table") |> DT::selectRows(NULL)
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
          "parameter",
          choices = stats::setNames(
            moduleData$parameters$parameter_id,
            moduleData$parameters$param_name
          )
        )
        updateSelectizeInput(
          session,
          "media",
          choices = stats::setNames(
            moduleData$media$media_id,
            moduleData$media$media_type
          )
        )
        updateSelectizeInput(
          session,
          "aggregation_type",
          choices = stats::setNames(
            moduleData$aggregation_types$aggregation_type_id,
            moduleData$aggregation_types$aggregation_type
          )
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
          "share_with",
          choices = moduleData$users$role_name
        )
        updateSelectizeInput(
          session,
          "source_fx",
          choices = moduleData$source_fx
        )
        showNotification("Module reloaded", type = "message")
      },
      ignoreInit = TRUE
    )

    observeEvent(input$z_specify, {
      if (input$z_specify) {
        shinyjs::show("z")
      } else {
        shinyjs::hide("z")
      }
    })

    # observe the location and limit the sub-locations based on those already existing
    observeEvent(
      input$location,
      {
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
      ignoreInit = TRUE
    )

    # Make sure share_with is either public_reader or other groups, not both
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

    # Show the user a modal with example arguments for the selected source function
    observeEvent(input$args_example, {
      if (is.null(input$source_fx) || input$source_fx == "") {
        showModal(modalDialog(
          "Select a source function to view example arguments.",
          easyClose = TRUE
        ))
        return()
      }
      ex_args <- moduleData$timeseries[
        moduleData$timeseries$source_fx == input$source_fx,
        "source_fx_args"
      ]
      ex_args <- ex_args[!is.na(ex_args)][1:5]
      # strip the [], {}, and "" from the json strings
      ex_args <- gsub("\\[|\\]|\\{|\\}|\"", "", ex_args)
      showModal(modalDialog(
        title = paste("Example arguments for", input$source_fx),
        if (length(ex_args) > 0) {
          paste(unique(ex_args), collapse = "\n\n")
        } else {
          "No example arguments found in existing timeseries. Please refer to the AquaCache package documentation for details on the required arguments."
        },
        easyClose = TRUE
      ))
    })

    # Open the documentation for the selected source function in a new browser tab
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

      # output path under the served directory, set up in globals
      out <- file.path(.rd_dir, paste0(input$source_fx, ".html"))

      tools::Rd2HTML(
        package[[file]],
        out,
        no_links = TRUE,
        package = "AquaCache"
      )

      # URL that the client can reach
      url <- paste0("/rdocs/", basename(out)) # not namespaced

      shinyjs::runjs(sprintf("window.open('%s','_blank');", url))
    })

    # Observe row selection and update inputs accordingly
    observeEvent(input$ts_table_rows_selected, {
      sel <- input$ts_table_rows_selected
      if (length(sel) > 0) {
        tsid <- moduleData$timeseries_display[sel, "timeseries_id"]
        selected_tsid(tsid)
        # Fetch the record from the basic timeseries table, not the timeseries_display as we need the numeric keys
        details <- moduleData$timeseries[
          moduleData$timeseries$timeseries_id == tsid,
        ]
        if (nrow(details) > 0) {
          # Update inputs with the selected timeseries details
          updateSelectizeInput(
            session,
            "location",
            selected = details$location_id
          )
          updateSelectizeInput(
            session,
            "sub_location",
            selected = details$sub_location_id
          )
          updateSelectizeInput(
            session,
            "tz",
            selected = details$timezone_daily_calc
          )
          updateCheckboxInput(
            session,
            "z_specify",
            value = if (!is.na(details$z)) TRUE else FALSE
          )
          updateNumericInput(
            session,
            "z",
            value = ifelse(is.na(details$z), NA, details$z)
          )
          updateSelectizeInput(
            session,
            "parameter",
            selected = details$parameter_id
          )
          updateSelectizeInput(session, "media", selected = details$media_id)
          updateSelectizeInput(
            session,
            "aggregation_type",
            selected = details$aggregation_type_id
          )
          updateTextInput(
            session,
            "record_rate",
            value = ifelse(is.na(details$record_rate), "", details$record_rate)
          )
          updateSelectizeInput(
            session,
            "sensor_priority",
            selected = details$sensor_priority
          )
          updateSelectizeInput(
            session,
            "default_owner",
            selected = details$default_owner
          )
          updateSelectizeInput(
            session,
            "share_with",
            selected = parse_share_with(details$share_with)
          )
          updateSelectizeInput(
            session,
            "source_fx",
            selected = details$source_fx
          )
          updateTextInput(
            session,
            "source_fx_args",
            value = ifelse(
              is.na(details$source_fx_args),
              "",
              details$source_fx_args
            )
          )
          updateTextAreaInput(
            session,
            "note",
            value = ifelse(is.na(details$note), "", details$note)
          )
        } else {
          showNotification(
            "Selected timeseries not found in the database.",
            type = "error"
          )
        }
      } else {
        selected_tsid(NULL)
      }
    })

    ### Observe the owner selectizeInput for new owners ############
    observeEvent(
      input$default_owner,
      {
        if (
          input$default_owner %in%
            moduleData$organizations$organization_id ||
            nchar(input$default_owner) == 0
        ) {
          return()
        }
        showModal(modalDialog(
          textInput(
            ns("owner_name"),
            "Owner name",
            value = input$default_owner
          ),
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
          "INSERT INTO organizations (name, name_fr, contact_name, phone, email, note) VALUES ($1, $2, $3, $4, $5, $6);",
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
          "SELECT organization_id, name FROM organizations"
        )
        # Update the selectizeInput to the new value
        updateSelectizeInput(
          session,
          "default_owner",
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

    # Add a new timeseries #############
    # Create an extendedTask to add a new timeseries
    addNewTimeseries <- ExtendedTask$new(
      function(
        config,
        loc,
        sub_loc,
        tz,
        z,
        z_specify,
        parameter,
        media,
        priority,
        agg_type,
        rate,
        owner,
        note,
        source_fx,
        source_fx_args,
        data,
        share_with
      ) {
        promises::future_promise(seed = TRUE, expr = {
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

              if (is.null(sub_loc)) {
                sub_loc <- NA
              } else if (nzchar(sub_loc)) {
                sub_loc <- as.numeric(sub_loc)
              } else {
                sub_loc <- NA
              }

              # Get the location code from table locations
              loc_code <- data$locations[
                data$locations$location_id == loc,
                "location"
              ]

              if (!is.null(source_fx_args)) {
                if (nzchar(source_fx_args)) {
                  # Make the json object for source_fx_args
                  # Make the source_fx_args a json object
                  args <- source_fx_args
                  # split into "argument1: value1" etc.
                  args <- strsplit(args, ",\\s*")[[1]]

                  # split only on first colon
                  keys <- sub(":.*", "", args)
                  vals <- sub("^[^:]+:\\s*", "", args)

                  # build named list
                  args <- stats::setNames(as.list(vals), keys)

                  # convert to JSON
                  args <- jsonlite::toJSON(args, auto_unbox = TRUE)
                } else {
                  args <- NA
                }
              } else {
                # if the source_fx_args is NULL, we set it to NA
                args <- NA
              }

              if (!is.null(source_fx)) {
                if (nzchar(source_fx)) {
                  source_fx <- source_fx
                } else {
                  source_fx <- NA
                }
                if (source_fx == "downloadNWIS") {
                  # NWIS data is only available from 2007 onwards, and errors if a date in the 1900s or earlier is specified.
                  end_datetime <- "2000-01-01"
                } else {
                  end_datetime <- "1800-01-01"
                }
              } else {
                source_fx <- NA
                end_datetime <- NA
              }

              existing_z <- NA
              if (z_specify) {
                # Create a new entry in locations_z if needed
                existing_z <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT * FROM locations_z WHERE location_id = ",
                    loc,
                    " AND sub_location_id ",
                    ifelse(
                      is.na(sub_loc),
                      "IS NULL",
                      paste0("= ", sub_loc)
                    ),
                    " AND z_meters = ",
                    z
                  )
                )[1, 1]
                if (length(existing_z) == 0) {
                  existing_z <- DBI::dbGetQuery(
                    con,
                    "INSERT INTO locations_z (location_id, sub_location_id, z_meters) VALUES ($1, $2, $3) RETURNING z_id;",
                    params = list(loc, sub_loc, z)
                  )[1, 1]
                }
              }

              # Make a new entry to the timeseries table
              new_timeseries_id <- DBI::dbGetQuery(
                con,
                "INSERT INTO continuous.timeseries (location, location_id, sub_location_id, timezone_daily_calc, z_id, parameter_id, media_id, sensor_priority, aggregation_type_id, record_rate, default_owner, share_with, source_fx, source_fx_args, note, end_datetime) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16) RETURNING timeseries_id;",
                params = list(
                  loc_code,
                  as.numeric(loc),
                  ifelse(is.na(sub_loc), NA, sub_loc),
                  as.numeric(tz),
                  ifelse(is.na(existing_z), NA, existing_z),
                  as.numeric(parameter),
                  as.numeric(media),
                  as.numeric(priority),
                  as.numeric(agg_type),
                  rate,
                  as.numeric(owner),
                  format_share_with(share_with),
                  ifelse(is.na(source_fx), NA, source_fx),
                  ifelse(is.na(args), NA, args),
                  if (nzchar(note)) note else NA,
                  ifelse(is.na(end_datetime), NA, end_datetime)
                )
              )[1, 1]

              # Fetch historical data if source_fx is provided
              if (!is.na(source_fx)) {
                AquaCache::getNewContinuous(
                  con = con,
                  timeseries_id = new_timeseries_id
                )
                new_start <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ",
                    new_timeseries_id,
                    ";"
                  )
                )[1, 1]
                # If new_start is NA if means there's no data, so set it to end_datetime
                if (!is.na(new_start)) {
                  DBI::dbExecute(
                    con,
                    paste0(
                      "UPDATE timeseries SET start_datetime = '",
                      new_start,
                      "' WHERE timeseries_id = ",
                      new_timeseries_id,
                      ";"
                    )
                  )
                }

                # Now conditionally check for HYDAT historical data
                if (source_fx == "downloadWSC") {
                  param_name <- data$parameters[
                    data$parameters$parameter_id == parameter,
                    "param_name"
                  ]
                  if (param_name %in% c("water level", "water flow")) {
                    suppressMessages(AquaCache::update_hydat(
                      timeseries_id = new_timeseries_id,
                      force_update = TRUE,
                      con = con
                    ))
                  }
                }

                # Ensure that there are records in measurements_continuous and/or measurements_calculated_daily
                mcd <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
                    new_timeseries_id
                  )
                )[1, 1]
                mc <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ",
                    new_timeseries_id
                  )
                )[1, 1]

                if (is.na(mcd) && is.na(mc)) {
                  stop(
                    "Could not find any data for this timeseries. Try different parameters."
                  )
                }

                # Now calculate stats
                if (lubridate::period(rate) <= lubridate::period("1 day")) {
                  AquaCache::calculate_stats(
                    timeseries_id = new_timeseries_id,
                    con = con,
                    start_recalc = NULL
                  )
                }
                DBI::dbCommit(con)
                return("success")
              } else {
                DBI::dbCommit(con)
                return("successNoData")
              }
            },
            error = function(e) {
              DBI::dbRollback(con)
              return(paste("Error adding timeseries:", e$message))
            },
            warning = function(w) {
              DBI::dbRollback(con)
              return(paste(
                "Failure due to warning when adding timeseries:",
                w$message
              ))
            }
          )
        })
      } # end of ExtendedTask$new
    ) |>
      bslib::bind_task_button("add_timeseries")
    # End of ExtendedTask$new

    observeEvent(input$add_timeseries, {
      # validate inputs
      validate(
        need(input$location, "Please select a location."),
        need(input$parameter, "Please select a parameter."),
        need(input$media, "Please select a media type."),
        need(input$aggregation_type, "Please select an aggregation type."),
        need(input$default_owner, "Please select a default owner."),
        need(input$sensor_priority, "Please select a sensor priority."),
        need(input$record_rate, "Please specify a record rate."),
      )

      if (input$mode != "add") {
        # This is an error: show the user a notification to select 'add' mode
        showNotification(
          "Please select 'Add new' mode to add a timeseries.",
          type = "error",
          duration = 8,
        )
        return()
      }

      # if input$source_fx_args is not blank, validate that it is in the correct format.
      # Should have no =, no "" or '', and have : separating key and value
      if (nzchar(input$source_fx_args)) {
        if (grepl("=", input$source_fx_args)) {
          showNotification(
            "Source function arguments should use ':' to separate keys and values, not '='.",
            type = "error",
            duration = 8,
          )
          return()
        }
        if (grepl("\"|'", input$source_fx_args)) {
          showNotification(
            "Source function arguments should not contain quotes (\") or (').",
            type = "error",
            duration = 8,
          )
          return()
        }
        if (!all(grepl(":", unlist(strsplit(input$source_fx_args, ",\\s*"))))) {
          showNotification(
            "Source function arguments should use ':' to separate keys and values.",
            type = "error",
            duration = 8,
          )
          return()
        }
      }

      # Call the extendedTask to add a new timeseries
      addNewTimeseries$invoke(
        config = session$userData$config,
        loc = input$location,
        sub_loc = input$sub_location,
        tz = input$tz,
        z = input$z,
        z_specify = input$z_specify,
        parameter = input$parameter,
        media = input$media,
        priority = input$sensor_priority,
        agg_type = input$aggregation_type,
        rate = input$record_rate,
        owner = input$default_owner,
        note = input$note,
        source_fx = input$source_fx,
        source_fx_args = input$source_fx_args,
        data = reactiveValuesToList(moduleData),
        share_with = input$share_with
      )
    })

    # Observe the result of the ExtendedTask
    observeEvent(addNewTimeseries$result(), {
      if (is.null(addNewTimeseries$result())) {
        return() # No result yet, do nothing
      } else if (
        !addNewTimeseries$result() %in% c("successNoData", "success")
      ) {
        # If the result is not "success", show an error notification
        showNotification(addNewTimeseries$result(), type = "error")
        return()
      } else if (addNewTimeseries$result() == "successNoData") {
        showNotification(
          "Timeseries added successfully with no data fetched. REMEMBER TO ADD DATA NOW.",
          type = "warning",
          duration = 10,
        )
      } else if (addNewTimeseries$result() == "success") {
        # If the result is "success", show a success notification
        showNotification(
          "Timeseries added successfully! Historical data was fetched and daily means calculated if you provided a source_fx.",
          type = "message",
          duration = 8,
        )

        getModuleData()

        # Reset all fields
        updateSelectizeInput(session, "location", selected = character(0))
        updateSelectizeInput(session, "sub_location", selected = character(0))
        updateSelectizeInput(session, "tz", selected = -7)
        updateCheckboxInput(session, "z_specify", value = FALSE)
        updateNumericInput(session, "z", value = NA)
        updateSelectizeInput(session, "parameter", selected = character(0))
        updateSelectizeInput(session, "media", selected = character(0))
        updateSelectizeInput(
          session,
          "aggregation_type",
          selected = character(0)
        )
        updateTextInput(session, "record_rate", value = "")
        updateSelectizeInput(session, "sensor_priority", selected = 1)
        updateSelectizeInput(session, "default_owner", selected = character(0))
        updateSelectizeInput(session, "share_with", selected = "public_reader")
        updateSelectizeInput(session, "source_fx", selected = character(0))
        updateTextInput(session, "source_fx_args", value = "")
        updateTextAreaInput(session, "note", value = "")
      }
    })

    # Modify existing timeseries ###############
    observeEvent(
      input$modify_timeseries,
      {
        if (input$mode != "modify") {
          # This is an error: show the user a notification to select 'modify' mode
          showNotification(
            "Please select 'Modify existing' mode to modify a timeseries.",
            type = "error",
            duration = 8,
          )
          return()
        }
        # If we are modifying an existing timeseries, we need to check if it exists
        selected_row <- input$ts_table_rows_selected
        if (is.null(selected_row) || length(selected_row) != 1) {
          showNotification(
            "Please select a single timeseries to modify.",
            type = "error",
            duration = 8,
          )
          return()
        }
        tsid <- moduleData$timeseries_display[selected_row, "timeseries_id"]
        selected_timeseries <- moduleData$timeseries[
          moduleData$timeseries$timeseries_id == tsid,
        ]
        # Check if the timeseries already exists
        existing_timeseries <- DBI::dbGetQuery(
          session$userData$AquaCache,
          paste0(
            "SELECT * FROM timeseries WHERE timeseries_id = ",
            selected_timeseries$timeseries_id
          )
        )
        if (nrow(existing_timeseries) == 0) {
          showNotification(
            "Selected timeseries does not exist in the database.",
            type = "error",
            duration = 8,
          )
          return()
        }

        # If it exists, update the timeseries
        DBI::dbBegin(session$userData$AquaCache)

        tryCatch(
          {
            if (input$location != selected_timeseries$location_id) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE timeseries SET location_id = '",
                  input$location,
                  "' WHERE timeseries_id = ",
                  selected_timeseries$timeseries_id
                )
              )
            }

            # Is there an existing sub_location_id? If TRUE, then:
            if (!is.na(selected_timeseries$sub_location_id)) {
              if (is.null(input$sub_location)) {
                # If the new input is NULL or length 0, set NULL
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET sub_location_id = NULL WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              } else if (!nzchar(sub_location)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET sub_location_id = NULL WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              } else if (
                input$sub_location != selected_timeseries$sub_location_id
              ) {
                # If old and new are not the same, update with new value
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET sub_location_id = '",
                    input$sub_location,
                    "' WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              }
            } else {
              # If there is no pre-existing sub_location_id:
              if (!is.null(input$sub_location_id)) {
                # could be NULL if not touched
                if (!is.na(input$sub_location) && !nzchar(input$sub_location)) {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    paste0(
                      "UPDATE timeseries SET sub_location_id = '",
                      input$sub_location,
                      "' WHERE timeseries_id = ",
                      selected_timeseries$timeseries_id
                    )
                  )
                }
              }
            }

            # If a change is made to tz, AquaCache::calculate_stats will need to be rerun from the beginning of the timeseries
            recalc_stats <- FALSE
            if (input$tz != selected_timeseries$timezone_daily_calc) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE timeseries SET timezone_daily_calc = ",
                  input$tz,
                  " WHERE timeseries_id = ",
                  selected_timeseries$timeseries_id
                )
              )
              recalc_stats <- TRUE
            }

            if (input$z_specify) {
              if (!is.na(selected_timeseries$z_id)) {
                if (input$z != selected_timeseries$z) {
                  # Delete the existing entry in locations_z and redo. This takes care of cases where a user changes both the location/sub-location AND z
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    "DELETE FROM locations_z WHERE z_id = ",
                    selected_timeseries$z_id
                  )
                  # Create a new entry in locations_z
                  new_z_id <- DBI::dbGetQuery(
                    session$userData$AquaCache,
                    "INSERT INTO locations_z (location_id, sub_location_id, z_meters) VALUES ($1, $2, $3) RETURNING z_id;",
                    params = list(
                      as.numeric(input$location),
                      if (
                        !is.na(input$sub_location) && nzchar(input$sub_location)
                      ) {
                        as.numeric(input$sub_location)
                      } else {
                        NA
                      },
                      input$z
                    )
                  )[1, 1]
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    paste0(
                      "UPDATE timeseries SET z_id = ",
                      new_z_id,
                      " WHERE timeseries_id = ",
                      selected_timeseries$timeseries_id
                    )
                  )
                }
              } else {
                # No existing entry
                # Create a new entry in locations_z
                new_z_id <- DBI::dbGetQuery(
                  session$userData$AquaCache,
                  "INSERT INTO locations_z (location_id, sub_location_id, z_meters) VALUES ($1, $2, $3) RETURNING z_id;",
                  params = list(
                    as.numeric(input$location),
                    if (
                      !is.na(input$sub_location) && nzchar(input$sub_location)
                    ) {
                      as.numeric(input$sub_location)
                    } else {
                      NA
                    },
                    input$z
                  )
                )[1, 1]
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET z_id = $1 WHERE timeseries_id = $2",
                  params = list(new_z_id, selected_timeseries$timeseries_id)
                )
              }
            } else {
              # Delete the entry in table locations_z if it exists, which will cascade delete the z_id in timeseries
              exists <- DBI::dbGetQuery(
                session$userData$AquaCache,
                "SELECT z_id FROM locations_z WHERE z_id = $1",
                params = list(selected_timeseries$z_id)
              )
              if (nrow(exists)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "DELETE FROM locations_z WHERE z_id = $1",
                  params = list(selected_timeseries$z_id)
                )
              }
            }

            if (input$parameter != selected_timeseries$parameter_id) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE timeseries SET parameter_id = $1 WHERE timeseries_id = $2",
                params = list(
                  input$parameter,
                  selected_timeseries$timeseries_id
                )
              )
            }

            if (input$media != selected_timeseries$media_id) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE timeseries SET media_id = '",
                  input$media,
                  "' WHERE timeseries_id = ",
                  selected_timeseries$timeseries_id
                )
              )
            }

            if (
              input$aggregation_type != selected_timeseries$aggregation_type_id
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE timeseries SET aggregation_type_id = '",
                  input$aggregation_type,
                  "' WHERE timeseries_id = ",
                  selected_timeseries$timeseries_id
                )
              )
            }

            if (!is.na(selected_timeseries$record_rate)) {
              if (input$record_rate != selected_timeseries$record_rate) {
                if (input$record_rate != "") {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    paste0(
                      "UPDATE timeseries SET record_rate = '",
                      input$record_rate,
                      "' WHERE timeseries_id = ",
                      selected_timeseries$timeseries_id
                    )
                  )
                } else {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    paste0(
                      "UPDATE timeseries SET record_rate = NULL WHERE timeseries_id = ",
                      selected_timeseries$timeseries_id
                    )
                  )
                }
              }
            } else {
              if (input$record_rate != "") {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET record_rate = '",
                    input$record_rate,
                    "' WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET record_rate = NULL WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              }
            }

            if (!is.na(selected_timeseries$sensor_priority)) {
              if (
                input$sensor_priority != selected_timeseries$sensor_priority
              ) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET sensor_priority = ",
                    input$sensor_priority,
                    " WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              }
            } else {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE timeseries SET sensor_priority = ",
                  input$sensor_priority,
                  " WHERE timeseries_id = ",
                  selected_timeseries$timeseries_id
                )
              )
            }

            if (!is.na(selected_timeseries$default_owner)) {
              if (input$default_owner != selected_timeseries$default_owner) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET default_owner = '",
                    input$default_owner,
                    "' WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              }
            } else {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE timeseries SET default_owner = '",
                  input$default_owner,
                  "' WHERE timeseries_id = ",
                  selected_timeseries$timeseries_id
                )
              )
            }

            # Changes to share_with
            print(input$share_with)
            print(selected_timeseries$share_with)
            input_share_with <- format_share_with(input$share_with)
            parsed_exist_share_with <- parse_share_with(
              selected_timeseries$share_with
            )
            print(input_share_with)
            print(parsed_exist_share_with)
            print("done printing for now")
            if (any(input$share_with != parsed_exist_share_with)) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE timeseries SET share_with = $1 WHERE timeseries_id = $2;",
                params = list(
                  input_share_with,
                  selected_timeseries$timeseries_id
                )
              )
            }

            # Changes to source_fx and source_fx_args
            if (!is.na(selected_timeseries$source_fx)) {
              if (input$source_fx != selected_timeseries$source_fx) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET source_fx = '",
                    input$source_fx,
                    "' WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              }
            } else {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE timeseries SET source_fx = NULL WHERE timeseries_id = ",
                  selected_timeseries$timeseries_id
                )
              )
            }

            if (!is.na(selected_timeseries$source_fx_args)) {
              if (!nzchar(input$source_fx_args)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET source_fx_args = NULL WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
                return()
              }

              # source_fx_args are fetched from DB as json, so we need to handle them accordingly for comparison
              # The following gives us a data.frame with column names as the keys and values as the values
              parsed_json <- jsonlite::fromJSON(
                selected_timeseries$source_fx_args,
                simplifyVector = TRUE,
                flatten = TRUE
              )

              # Now work the input$source_fx_args into a data.frame with the same shape as parsed_json
              # split into “arg:value” pairs, trim whitespace
              arg_pairs <- strsplit(input$source_fx_args, ",")[[1]] # e.g. c("arg1: value1", "arg2: value2")
              arg_pairs <- trimws(arg_pairs) # remove leading/trailing spaces
              # split each on “:”, extract keys & values
              kv <- strsplit(arg_pairs, ":")
              keys <- sapply(kv, `[`, 1)
              vals <- sapply(kv, `[`, 2)
              # build a named list and then a one-row data.frame
              input_df <- setNames(as.list(vals), keys)
              input_df <- as.data.frame(input_df, stringsAsFactors = FALSE)
              # now `parsed_json` and `input_df` have the same shape

              if (!identical(parsed_json, input_df)) {
                # Make the source_fx_args a json object
                args <- input$source_fx_args
                # split into "argument1: value1" etc.
                args <- strsplit(args, ",\\s*")[[1]]

                # split only on first colon
                keys <- sub(":.*", "", args)
                vals <- sub("^[^:]+:\\s*", "", args)

                # build named list
                args <- stats::setNames(as.list(vals), keys)

                # convert to JSON
                args <- jsonlite::toJSON(args, auto_unbox = TRUE)

                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET source_fx_args = '",
                    args,
                    "' WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              }
            }

            if (!is.na(selected_timeseries$note)) {
              if (input$note != selected_timeseries$note) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET note = '",
                    input$note,
                    "' WHERE timeseries_id = ",
                    selected_timeseries$timeseries_id
                  )
                )
              }
            } else {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE timeseries SET note = NULL WHERE timeseries_id = ",
                  selected_timeseries$timeseries_id
                )
              )
            }

            # If recalc_stats is TRUE, we need to recalculate stats from the beginning of the timeseries
            if (recalc_stats) {
              showNotification(
                "Recalculating statistics from the beginning of the timeseries due to timezone change. Please be patient.",
                type = "message",
                duration = 8,
              )
              earliest <- DBI::dbGetQuery(
                session$userData$AquaCache,
                "SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = $1",
                params = selected_timeseries$timeseries_id
              )[1, 1]
              AquaCache::calculate_stats(
                timeseries_id = selected_timeseries$timeseries_id,
                con = session$userData$AquaCache,
                start_recalc = earliest
              )
            }

            DBI::dbCommit(session$userData$AquaCache)
            showNotification(
              "Timeseries updated successfully!",
              type = "message"
            )
            getModuleData()
          },
          error = function(e) {
            DBI::dbRollback(session$userData$AquaCache)
            showNotification(
              paste("Error updating timeseries:", e$message),
              type = "error",
              duration = 10,
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    # HEADS UP! find the modules which depend on timeseries. These will have cached data, which will need to be cleared when a new location or timeseries is added using the clear_cached function (R/app_cache.R)
  }) # End of moduleServer
}
