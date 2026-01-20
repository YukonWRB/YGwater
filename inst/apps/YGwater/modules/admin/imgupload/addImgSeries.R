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

    getModuleData <- function() {
      moduleData$image_series <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT img_series_id, source_fx, source_fx_args, description, location_id, active, share_with, owner FROM files.image_series;"
      )
      moduleData$image_series_display <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT i.img_series_id, l.name AS location, o.name AS owner, i.source_fx, i.active
                                                         FROM files.image_series i
                                                         INNER JOIN locations l ON i.location_id = l.location_id
                                                         INNER JOIN organizations o ON i.owner = o.organization_id;"
      )
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM organizations"
      )
      moduleData$locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT location_id, location, name, latitude, longitude FROM locations"
      )
      moduleData$users <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('files.images');"
      ) # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table
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
        splitLayout(
          cellWidths = c("50%", "50%"),
          verticalLayout(
            # htmlOutput to tell the user when they should use the source functions and what the arguments are
            tags$div(
              class = "alert alert-info",
              "The source function is used to download images using the AquaCache R package. Leave blank if entering data manually or using other methods. For more information refer to the AquaCache package documentation."
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
            )
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
      showNotification("Module reloaded", type = "message")
    })

    observeEvent(input$series_table_rows_selected, {
      sel <- input$series_table_rows_selected
      if (length(sel) > 0) {
        selected_series(moduleData$image_series_display[sel, ])
        # Fetch details for the selected series
        details <- moduleData$image_series[
          moduleData$image_series == selected_series(),
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
        updateSelectizeInput(session, "source_fx", selected = details$source_fx)
        updateTextInput(
          session,
          "source_fx_args",
          value = details$source_fx_args
        )
        updateCheckboxInput(
          session,
          "active",
          value = as.logical(details$active)
        )
      } else {
        selected_series(NULL)
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

    # Add a new image series
    # Create an extendedTask to add new, since the data pull might take a very long time
    addNewSeries <- ExtendedTask$new(
      function(
        config,
        loc,
        share_with,
        owner,
        description,
        source_fx,
        source_fx_args,
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

              df <- data.frame(
                location_id = loc,
                owner = owner,
                description = if (isTruthy(description)) description else NA,
                share_with = paste0(
                  "{",
                  paste(share_with, collapse = ", "),
                  "}"
                ),
                source_fx = source_fx,
                source_fx_args = args,
                active = TRUE,
                last_img = start
              )

              new_id <- DBI::dbGetQuery(
                con,
                "INSERT INTO image_series (location_id, owner, description, share_with, source_fx, source_fx_args, active, last_img) VALUES ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING img_series_id;",
                params = list(
                  df$location_id,
                  df$owner,
                  df$description,
                  DBI::SQL(df$share_with),
                  df$source_fx,
                  if (nzchar(source_fx_args)) df$source_fx_args else NA,
                  df$active,
                  df$last_img
                )
              )[1, 1]

              # fetch images
              AquaCache::getNewImages(image_meta_ids = new_id, con = con)

              # Find the actual earliest found image and update image_series$first_img with that datetime
              earliest <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT MIN(datetime) FROM images WHERE img_series_id = ",
                  new_id
                )
              )[1, 1]

              if (is.na(earliest)) {
                stop("No images could be found")
              } else {
                DBI::dbExecute(
                  con,
                  paste0(
                    "UPDATE image_series SET first_img = '",
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
        need(input$source_fx, "Please select a source function"),
        need(
          !isTruthy(input$source_fx_args) || grepl(":", input$source_fx_args),
          "Source function arguments must be formatted as key-value pairs, e.g. 'arg1: value1, arg2: value2'"
        )
      )

      if (input$mode != "add") {
        showNotification(
          "Please select 'Add new' mode to add a timeseries.",
          type = "error"
        )
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
        source_fx = input$source_fx,
        source_fx_args = input$source_fx_args,
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
        updateTextInput(session, "source_fx_args", value = "")
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
        selected_series <- moduleData$image_series[
          moduleData$image_series$img_series_id == id,
        ]
        # Check if the series already exists
        existing_timeseries <- DBI::dbGetQuery(
          session$userData$AquaCache,
          paste0(
            "SELECT * FROM image_series WHERE img_series_id = ",
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
                  "UPDATE image_series SET share_with = {share_with_sql} WHERE img_series_id = {selected_series$img_series_id};",
                  .con = session$userData$AquaCache
                )
              )
            }

            # Changes to source_fx
            if (input$source_fx != selected_series$source_fx) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE image_series SET source_fx = '",
                  input$source_fx,
                  "' WHERE img_series_id = ",
                  selected_series$img_series_id
                )
              )
            }

            if (!is.na(selected_series$source_fx_args)) {
              if (!nzchar(input$source_fx_args)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE timeseries SET source_fx_args = NULL WHERE img_series_id = ",
                    selected_series$img_series_id
                  )
                )
                return()
              }

              # source_fx_args are fetched from DB as json, so we need to handle them accordingly for comparison
              # The following gives us a data.frame with column names as the keys and values as the values
              parsed_json <- jsonlite::fromJSON(
                selected_series$source_fx_args,
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
                    "UPDATE image_series SET source_fx_args = '",
                    args,
                    "' WHERE img_series_id = ",
                    selected_series$img_series_id
                  )
                )
              }
            } else {
              if (!nzchar(input$source_fx_args)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE image_series SET source_fx_args = NULL WHERE img_series_id = ",
                    selected_series$img_series_id
                  )
                )
                return()
              }
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
                  "UPDATE image_series SET source_fx_args = '",
                  args,
                  "' WHERE img_series_id = ",
                  selected_series$img_series_id
                )
              )
            }

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
