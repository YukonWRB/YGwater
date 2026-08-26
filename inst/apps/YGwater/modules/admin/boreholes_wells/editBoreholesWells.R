# UI and server code for editing existing borehole and well records

editBoreholesWellsUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    fluidRow(
      column(
        12,
        h4("Existing borehole/well records"),
        actionButton(ns("reload"), "Reload", icon = icon("refresh")),
        DT::DTOutput(ns("records_table"))
      )
    ),
    tags$hr(),
    fluidRow(
      column(
        4,
        selectizeInput(
          ns("record_id"),
          "Select borehole/well",
          choices = NULL,
          multiple = FALSE,
          options = list(placeholder = "Choose a record")
        ),
        textInput(ns("borehole_name"), "Borehole name"),
        selectizeInput(
          ns("borehole_approval_type_id"),
          "Borehole approval level",
          choices = NULL,
          multiple = FALSE
        ),
        dateInput(ns("completion_date"), "Completion date", value = NULL),
        numericInput(ns("latitude"), "Latitude", value = NA_real_),
        numericInput(ns("longitude"), "Longitude", value = NA_real_),
        numericInput(ns("depth_m"), "Borehole depth (m)", value = NA_real_),
        radioButtons(
          ns("bedrock_reached"),
          "Bedrock reached?",
          choices = list("Yes" = "yes", "No" = "no", "Unknown" = "unknown"),
          selected = "unknown",
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.bedrock_reached == 'yes'",
          ns = ns,
          numericInput(
            ns("depth_to_bedrock_m"),
            "Depth to bedrock (m)",
            value = NA_real_,
            min = 0
          )
        ),
        selectizeInput(
          ns("drilled_by"),
          "Driller",
          choices = NULL,
          multiple = FALSE,
          options = list(placeholder = "Optional")
        ),
        textAreaInput(ns("borehole_notes"), "Borehole notes", width = "100%"),
        selectizeInput(
          ns("share_with_borehole"),
          "Share borehole with groups",
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        )
      ),
      column(
        4,
        h4("Permafrost"),
        checkboxInput(
          ns("permafrost_present"),
          "Permafrost present",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.permafrost_present == true",
          ns = ns,
          numericInput(
            ns("permafrost_top"),
            "Depth to top of permafrost (m)",
            value = NA_real_
          ),
          numericInput(
            ns("permafrost_bot"),
            "Depth to bottom of permafrost (m)",
            value = NA_real_
          ),
          textAreaInput(
            ns("permafrost_ice_description"),
            "Permafrost notes/ice description",
            width = "100%"
          )
        ),
        tags$hr(),
        checkboxInput(
          ns("is_well"),
          "Selected row includes a well record",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.is_well == true",
          ns = ns,
          actionButton(
            ns("add_well"),
            "Add another well to this borehole",
            icon = icon("plus")
          ),
          textInput(ns("well_name"), "Well name"),
          selectizeInput(
            ns("well_approval_type_id"),
            "Well approval level",
            choices = NULL,
            multiple = FALSE
          ),
          selectizeInput(
            ns("well_purpose_id"),
            "Well purpose",
            choices = NULL,
            multiple = FALSE,
            options = list(placeholder = "Optional")
          ),
          numericInput(
            ns("casing_diameter_mm"),
            "Casing diameter (mm)",
            value = NA_real_
          ),
          numericInput(
            ns("casing_depth_to_m"),
            "Casing depth to (m)",
            value = NA_real_
          ),
          numericInput(
            ns("screen_top_depth_m"),
            "Screen top depth (m)",
            value = NA_real_
          ),
          numericInput(
            ns("screen_bottom_depth_m"),
            "Screen bottom depth (m)",
            value = NA_real_
          ),
          numericInput(
            ns("static_water_level_m"),
            "Static water level (m)",
            value = NA_real_
          ),
          numericInput(
            ns("estimated_yield_lps"),
            "Estimated yield (L/s)",
            value = NA_real_
          ),
          textAreaInput(ns("well_notes"), "Well notes", width = "100%"),
          selectizeInput(
            ns("share_with_well"),
            "Share well with groups",
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          )
        )
      ),
      column(
        4,
        h4("Actions"),
        actionButton(ns("save"), "Save changes", class = "btn-primary"),
        actionButton(ns("clear"), "Clear form")
      )
    )
  )
}

editBoreholesWells <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "editBoreholesWells"
      )
    })

    moduleData <- reactiveValues()

    na_if_blank <- function(value) {
      if (is.null(value)) {
        return(NA)
      } else if (
        !length(value) ||
          is.na(value) ||
          !nzchar(trimws(as.character(value)))
      ) {
        return(NA)
      }
      value
    }

    maybe_num <- function(value) {
      if (is.null(value)) {
        return(NA)
      } else if (!length(value) || is.na(value)) {
        return(NA)
      }
      as.numeric(value)
    }

    round_sig <- function(x, digits = 2) {
      if (!is.numeric(x)) {
        return(x)
      }
      out <- round(x, digits = digits)
      out[is.na(x)] <- NA_real_
      out
    }

    is_true <- function(x) {
      identical(x, TRUE)
    }

    format_bedrock_reached_input <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x[1])) {
        return("unknown")
      }
      if (isTRUE(x[1])) {
        return("yes")
      }
      "no"
    }

    parse_bedrock_reached <- function(x) {
      if (identical(x, "yes")) {
        return(TRUE)
      }
      if (identical(x, "no")) {
        return(FALSE)
      }
      NA
    }

    load_data <- function() {
      moduleData$records <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT b.borehole_id,
                b.borehole_name,
                b.approval_type_id AS borehole_approval_type_id,
                ba.approval_type_description AS borehole_approval,
                b.latitude,
                b.longitude,
                b.completion_date,
                b.depth_m,
                b.bedrock_reached,
                b.depth_to_bedrock_m,
                b.drilled_by,
                d.name AS driller_name,
                pf.depth_from_m AS permafrost_top,
                pf.depth_to_m AS permafrost_bot,
                pf.ice_description AS permafrost_ice_description,
                b.notes AS borehole_notes,
                b.share_with AS borehole_share_with,
                w.well_id,
                w.well_id IS NOT NULL AS is_well,
                w.well_name,
                w.approval_type_id AS well_approval_type_id,
                wa.approval_type_description AS well_approval,
                w.well_purpose_id,
                w.casing_diameter_mm,
                w.casing_depth_to_m,
                w.screen_top_depth_m,
                w.screen_bottom_depth_m,
                w.static_water_level_m,
                w.estimated_yield_lps,
                w.notes AS well_notes,
                w.share_with AS well_share_with
         FROM boreholes.boreholes b
         LEFT JOIN boreholes.wells w ON w.borehole_id = b.borehole_id
         LEFT JOIN public.approval_types ba
           ON ba.approval_type_id = b.approval_type_id
         LEFT JOIN public.approval_types wa
           ON wa.approval_type_id = w.approval_type_id
         LEFT JOIN boreholes.drillers d ON d.driller_id = b.drilled_by
         LEFT JOIN LATERAL (
           SELECT depth_from_m, depth_to_m, ice_description
           FROM boreholes.permafrost p
           WHERE p.borehole_id = b.borehole_id
           ORDER BY p.permafrost_record_id DESC
           LIMIT 1
         ) pf ON TRUE
         ORDER BY b.borehole_name ASC, w.well_id ASC;"
      )
      moduleData$records$record_key <- ifelse(
        is.na(moduleData$records$well_id),
        paste0("b", moduleData$records$borehole_id),
        paste0(
          "b",
          moduleData$records$borehole_id,
          "w",
          moduleData$records$well_id
        )
      )
      moduleData$drillers <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT driller_id, name FROM boreholes.drillers ORDER BY name ASC;"
      )
      moduleData$purposes <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT borehole_well_purpose_id, purpose_name FROM boreholes.borehole_well_purposes ORDER BY purpose_name ASC;"
      )
      moduleData$approvals <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT approval_type_id, approval_type_code,
                approval_type_description
         FROM public.approval_types
         ORDER BY approval_type_id;"
      )
      moduleData$share_with_boreholes <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT role_name FROM public.get_shareable_principals_for('boreholes.boreholes');"
      )
      moduleData$share_with_wells <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT role_name FROM public.get_shareable_principals_for('boreholes.wells');"
      )
    }

    active_borehole_id <- reactiveVal(NULL)
    active_well_id <- reactiveVal(NULL)

    record_choice_labels <- function() {
      sprintf(
        "%s (borehole %s; %s)",
        moduleData$records$borehole_name,
        moduleData$records$borehole_id,
        ifelse(
          is.na(moduleData$records$well_id),
          "no well",
          paste0(
            "well ",
            moduleData$records$well_id,
            ": ",
            moduleData$records$well_name
          )
        )
      )
    }

    update_record_selector <- function(selected = isolate(input$record_id)) {
      updateSelectizeInput(
        session,
        "record_id",
        choices = stats::setNames(
          moduleData$records$record_key,
          record_choice_labels()
        ),
        selected = selected,
        server = TRUE
      )
    }

    default_approval_id <- function() {
      req(moduleData$approvals)
      id <- moduleData$approvals$approval_type_id[
        moduleData$approvals$approval_type_code == "N"
      ]
      if (length(id) != 1L) {
        stop("Expected exactly one approval type with code 'N'.")
      }
      as.integer(id[[1]])
    }

    load_data()

    observe({
      req(moduleData$records)
      update_record_selector()

      updateSelectizeInput(
        session,
        "drilled_by",
        choices = stats::setNames(
          moduleData$drillers$driller_id,
          moduleData$drillers$name
        ),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "well_purpose_id",
        choices = stats::setNames(
          moduleData$purposes$borehole_well_purpose_id,
          moduleData$purposes$purpose_name
        ),
        server = TRUE
      )
      approval_choices <- stats::setNames(
        moduleData$approvals$approval_type_id,
        moduleData$approvals$approval_type_description
      )
      updateSelectizeInput(
        session,
        "borehole_approval_type_id",
        choices = approval_choices,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "well_approval_type_id",
        choices = approval_choices,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "share_with_borehole",
        choices = moduleData$share_with_boreholes$role_name
      )
      updateSelectizeInput(
        session,
        "share_with_well",
        choices = moduleData$share_with_wells$role_name
      )
    })

    observeEvent(input$record_id, {
      req(moduleData$records)
      rec <- moduleData$records[
        moduleData$records$record_key == input$record_id,
      ]
      req(nrow(rec) == 1)
      active_borehole_id(as.integer(rec$borehole_id))
      active_well_id(
        if (is.na(rec$well_id)) NULL else as.integer(rec$well_id)
      )
      updateTextInput(session, "borehole_name", value = rec$borehole_name)
      updateSelectizeInput(
        session,
        "borehole_approval_type_id",
        selected = as.character(rec$borehole_approval_type_id)
      )
      updateDateInput(session, "completion_date", value = rec$completion_date)
      updateNumericInput(session, "latitude", value = rec$latitude)
      updateNumericInput(session, "longitude", value = rec$longitude)
      updateNumericInput(session, "depth_m", value = rec$depth_m)
      updateRadioButtons(
        session,
        "bedrock_reached",
        selected = format_bedrock_reached_input(rec$bedrock_reached)
      )
      updateNumericInput(
        session,
        "depth_to_bedrock_m",
        value = rec$depth_to_bedrock_m
      )
      updateSelectizeInput(
        session,
        "drilled_by",
        selected = as.character(rec$drilled_by)
      )
      updateTextAreaInput(session, "borehole_notes", value = rec$borehole_notes)
      updateSelectizeInput(
        session,
        "share_with_borehole",
        selected = array_to_text(rec$borehole_share_with)
      )
      updateCheckboxInput(
        session,
        "permafrost_present",
        value = is_true(
          !is.na(rec$permafrost_top) || !is.na(rec$permafrost_bot)
        )
      )
      updateNumericInput(session, "permafrost_top", value = rec$permafrost_top)
      updateNumericInput(session, "permafrost_bot", value = rec$permafrost_bot)
      updateTextAreaInput(
        session,
        "permafrost_ice_description",
        value = rec$permafrost_ice_description
      )
      updateCheckboxInput(session, "is_well", value = isTRUE(rec$is_well))
      updateTextInput(
        session,
        "well_name",
        value = if (is.na(rec$well_name)) "" else rec$well_name
      )
      updateSelectizeInput(
        session,
        "well_approval_type_id",
        selected = if (is.na(rec$well_approval_type_id)) {
          character(0)
        } else {
          as.character(rec$well_approval_type_id)
        }
      )
      updateSelectizeInput(
        session,
        "well_purpose_id",
        selected = as.character(rec$well_purpose_id)
      )
      updateNumericInput(
        session,
        "casing_diameter_mm",
        value = rec$casing_diameter_mm
      )
      updateNumericInput(
        session,
        "casing_depth_to_m",
        value = rec$casing_depth_to_m
      )
      updateNumericInput(
        session,
        "screen_top_depth_m",
        value = rec$screen_top_depth_m
      )
      updateNumericInput(
        session,
        "screen_bottom_depth_m",
        value = rec$screen_bottom_depth_m
      )
      updateNumericInput(
        session,
        "static_water_level_m",
        value = rec$static_water_level_m
      )
      updateNumericInput(
        session,
        "estimated_yield_lps",
        value = rec$estimated_yield_lps
      )
      updateTextAreaInput(session, "well_notes", value = rec$well_notes)
      updateSelectizeInput(
        session,
        "share_with_well",
        selected = array_to_text(rec$well_share_with)
      )
    })

    observeEvent(input$clear, {
      active_borehole_id(NULL)
      active_well_id(NULL)
      updateSelectizeInput(session, "record_id", selected = "")
      updateTextInput(session, "borehole_name", value = "")
      updateSelectizeInput(
        session,
        "borehole_approval_type_id",
        selected = character(0)
      )
      updateDateInput(session, "completion_date", value = as.Date(NA))
      updateNumericInput(session, "latitude", value = NA_real_)
      updateNumericInput(session, "longitude", value = NA_real_)
      updateNumericInput(session, "depth_m", value = NA_real_)
      updateRadioButtons(session, "bedrock_reached", selected = "unknown")
      updateNumericInput(session, "depth_to_bedrock_m", value = NA_real_)
      updateSelectizeInput(session, "drilled_by", selected = "")
      updateTextAreaInput(session, "borehole_notes", value = "")
      updateSelectizeInput(
        session,
        "share_with_borehole",
        selected = character(0)
      )
      updateCheckboxInput(session, "permafrost_present", value = FALSE)
      updateNumericInput(session, "permafrost_top", value = NA_real_)
      updateNumericInput(session, "permafrost_bot", value = NA_real_)
      updateTextAreaInput(session, "permafrost_ice_description", value = "")
      updateCheckboxInput(session, "is_well", value = FALSE)
      updateTextInput(session, "well_name", value = "")
      updateSelectizeInput(
        session,
        "well_approval_type_id",
        selected = character(0)
      )
      updateSelectizeInput(session, "well_purpose_id", selected = "")
      updateNumericInput(session, "casing_diameter_mm", value = NA_real_)
      updateNumericInput(session, "casing_depth_to_m", value = NA_real_)
      updateNumericInput(session, "screen_top_depth_m", value = NA_real_)
      updateNumericInput(session, "screen_bottom_depth_m", value = NA_real_)
      updateNumericInput(session, "static_water_level_m", value = NA_real_)
      updateNumericInput(session, "estimated_yield_lps", value = NA_real_)
      updateTextAreaInput(session, "well_notes", value = "")
      updateSelectizeInput(session, "share_with_well", selected = character(0))
    })

    observeEvent(input$add_well, {
      req(active_borehole_id())
      active_well_id(NULL)
      updateCheckboxInput(session, "is_well", value = TRUE)
      existing_well_count <- sum(
        moduleData$records$borehole_id == active_borehole_id() &
          !is.na(moduleData$records$well_id)
      )
      default_well_name <- if (existing_well_count == 0L) {
        trimws(input$borehole_name)
      } else {
        paste(trimws(input$borehole_name), existing_well_count + 1L)
      }
      updateTextInput(session, "well_name", value = default_well_name)
      updateSelectizeInput(
        session,
        "well_approval_type_id",
        selected = as.character(default_approval_id())
      )
      updateSelectizeInput(session, "well_purpose_id", selected = "")
      updateNumericInput(session, "casing_diameter_mm", value = NA_real_)
      updateNumericInput(session, "casing_depth_to_m", value = NA_real_)
      updateNumericInput(session, "screen_top_depth_m", value = NA_real_)
      updateNumericInput(session, "screen_bottom_depth_m", value = NA_real_)
      updateNumericInput(session, "static_water_level_m", value = NA_real_)
      updateNumericInput(session, "estimated_yield_lps", value = NA_real_)
      updateTextAreaInput(session, "well_notes", value = "")
      updateSelectizeInput(
        session,
        "share_with_well",
        selected = input$share_with_borehole
      )
      showNotification(
        "Enter the additional well details, then save changes.",
        type = "message"
      )
    })

    observeEvent(input$reload, {
      load_data()
    })

    observeEvent(input$save, {
      req(input$record_id)
      borehole_id <- active_borehole_id()
      req(!is.null(borehole_id), !is.na(borehole_id))
      req(nzchar(trimws(input$borehole_name)))
      borehole_approval_type_id <- suppressWarnings(as.integer(
        na_if_blank(input$borehole_approval_type_id)
      ))
      req(!is.na(borehole_approval_type_id))
      selected_well_id <- active_well_id()
      if (!is_true(input$is_well) && !is.null(selected_well_id)) {
        showNotification(
          "Unchecking the well field does not delete a well. Reload the row to continue editing it.",
          type = "error",
          duration = 8
        )
        return()
      }

      well_approval_type_id <- suppressWarnings(as.integer(
        na_if_blank(input$well_approval_type_id)
      ))
      if (is_true(input$is_well) && is.na(well_approval_type_id)) {
        showNotification("Select a well approval level.", type = "error")
        return()
      }
      if (
        is_true(input$is_well) &&
          (is.null(input$well_name) || !nzchar(trimws(input$well_name)))
      ) {
        showNotification("Enter a well name.", type = "error")
        return()
      }

      borehole_share <- unique(trimws(input$share_with_borehole))
      borehole_share <- borehole_share[nzchar(borehole_share)]
      if (!length(borehole_share)) {
        borehole_share <- "public_reader"
      }
      borehole_share <- share_with_to_array(borehole_share)

      well_share <- unique(trimws(input$share_with_well))
      well_share <- well_share[nzchar(well_share)]
      if (!length(well_share)) {
        well_share <- borehole_share
      }
      well_share <- share_with_to_array(well_share)

      permafrost_top <- maybe_num(input$permafrost_top)
      permafrost_bot <- maybe_num(input$permafrost_bot)
      if (!is_true(input$permafrost_present)) {
        permafrost_top <- NULL
        permafrost_bot <- NULL
      }

      bedrock_reached <- parse_bedrock_reached(input$bedrock_reached)
      depth_to_bedrock_m <- maybe_num(input$depth_to_bedrock_m)
      if (!isTRUE(bedrock_reached)) {
        depth_to_bedrock_m <- NA_real_
      } else if (is.na(depth_to_bedrock_m)) {
        showNotification(
          "Depth to bedrock is required when bedrock was reached.",
          type = "error"
        )
        return()
      }

      completion_date <- if (is.null(input$completion_date)) {
        NA
      } else if (length(input$completion_date) == 0) {
        NA
      } else if (nchar(input$completion_date) == 0) {
        NA
      } else if (is.na(input$completion_date)) {
        NA
      } else {
        input$completion_date
      }

      tryCatch(
        {
          DBI::dbExecute(session$userData$AquaCache, "BEGIN")

          DBI::dbExecute(
            session$userData$AquaCache,
            "UPDATE boreholes.boreholes
             SET borehole_name = $1,
                 completion_date = $2,
                 latitude = $3,
                 longitude = $4,
                 depth_m = $5,
                 bedrock_reached = $6,
                 depth_to_bedrock_m = $7,
                 drilled_by = $8,
                 notes = $9,
                 share_with = $10::text[],
                 approval_type_id = $11
             WHERE borehole_id = $12;",
            params = list(
              trimws(input$borehole_name),
              completion_date,
              maybe_num(input$latitude),
              maybe_num(input$longitude),
              maybe_num(input$depth_m),
              bedrock_reached,
              depth_to_bedrock_m,
              suppressWarnings(as.integer(na_if_blank(input$drilled_by))),
              na_if_blank(input$borehole_notes),
              borehole_share,
              borehole_approval_type_id,
              borehole_id
            )
          )

          if (is_true(input$permafrost_present)) {
            DBI::dbExecute(
              session$userData$AquaCache,
              "DELETE FROM boreholes.permafrost WHERE borehole_id = $1;",
              params = list(borehole_id)
            )
            DBI::dbExecute(
              session$userData$AquaCache,
              "INSERT INTO boreholes.permafrost (
                  borehole_id,
                  depth_from_m,
                  depth_to_m,
                  ice_description
               ) VALUES ($1, $2, $3, $4);",
              params = list(
                borehole_id,
                permafrost_top,
                permafrost_bot,
                na_if_blank(input$permafrost_ice_description)
              )
            )
          } else {
            DBI::dbExecute(
              session$userData$AquaCache,
              "DELETE FROM boreholes.permafrost WHERE borehole_id = $1;",
              params = list(borehole_id)
            )
          }

          has_well <- !is.null(selected_well_id)
          saved_well_id <- selected_well_id

          if (is_true(input$is_well)) {
            if (is_true(has_well)) {
              updated <- DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE boreholes.wells
                 SET well_name = $1,
                     well_purpose_id = $2,
                     casing_diameter_mm = $3,
                     casing_depth_to_m = $4,
                     screen_top_depth_m = $5,
                     screen_bottom_depth_m = $6,
                     static_water_level_m = $7,
                     estimated_yield_lps = $8,
                     notes = $9,
                     share_with = $10::text[],
                     approval_type_id = $11
                 WHERE well_id = $12
                   AND borehole_id = $13;",
                params = list(
                  trimws(input$well_name),
                  suppressWarnings(as.integer(na_if_blank(
                    input$well_purpose_id
                  ))),
                  maybe_num(input$casing_diameter_mm),
                  maybe_num(input$casing_depth_to_m),
                  maybe_num(input$screen_top_depth_m),
                  maybe_num(input$screen_bottom_depth_m),
                  maybe_num(input$static_water_level_m),
                  maybe_num(input$estimated_yield_lps),
                  na_if_blank(input$well_notes),
                  well_share,
                  well_approval_type_id,
                  selected_well_id,
                  borehole_id
                )
              )
              if (updated != 1L) {
                stop("The selected well no longer exists or is not editable.")
              }
            } else {
              existing_wells <- DBI::dbGetQuery(
                session$userData$AquaCache,
                "SELECT well_id, well_name
                 FROM boreholes.wells
                 WHERE borehole_id = $1
                 ORDER BY well_id
                 FOR UPDATE;",
                params = list(borehole_id)
              )
              if (
                nrow(existing_wells) == 1L &&
                  identical(
                    trimws(existing_wells$well_name[[1]]),
                    trimws(input$borehole_name)
                  )
              ) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE boreholes.wells
                   SET well_name = $1
                   WHERE well_id = $2;",
                  params = list(
                    paste(trimws(input$borehole_name), 1L),
                    existing_wells$well_id[[1]]
                  )
                )
              }
              inserted_well <- DBI::dbGetQuery(
                session$userData$AquaCache,
                "INSERT INTO boreholes.wells (
                    borehole_id,
                    well_name,
                    well_purpose_id,
                    casing_diameter_mm,
                    casing_depth_to_m,
                    screen_top_depth_m,
                    screen_bottom_depth_m,
                    static_water_level_m,
                    estimated_yield_lps,
                    notes,
                    share_with,
                    approval_type_id
                 ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11::text[],$12)
                 RETURNING well_id;",
                params = list(
                  borehole_id,
                  trimws(input$well_name),
                  suppressWarnings(as.integer(na_if_blank(
                    input$well_purpose_id
                  ))),
                  maybe_num(input$casing_diameter_mm),
                  maybe_num(input$casing_depth_to_m),
                  maybe_num(input$screen_top_depth_m),
                  maybe_num(input$screen_bottom_depth_m),
                  maybe_num(input$static_water_level_m),
                  maybe_num(input$estimated_yield_lps),
                  na_if_blank(input$well_notes),
                  well_share,
                  well_approval_type_id
                )
              )
              saved_well_id <- as.integer(inserted_well$well_id[[1]])
            }
          }

          DBI::dbExecute(session$userData$AquaCache, "COMMIT")
          load_data()
          selected_rows <- which(
            moduleData$records$borehole_id == borehole_id &
              if (is.null(saved_well_id)) {
                TRUE
              } else {
                moduleData$records$well_id == saved_well_id
              }
          )
          if (length(selected_rows)) {
            updateSelectizeInput(
              session,
              "record_id",
              selected = moduleData$records$record_key[selected_rows[[1]]]
            )
          }
          showNotification("Borehole/well updated.", type = "message")
        },
        error = function(e) {
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
          showNotification(
            paste("Failed to save changes:", e$message),
            type = "error"
          )
        }
      )
    })

    output$records_table <- DT::renderDT({
      req(moduleData$records)
      tbl <- moduleData$records
      tbl$permafrost_present <- !is.na(tbl$permafrost_top) |
        !is.na(tbl$permafrost_bot)
      tbl$borehole_approval <- as.factor(tbl$borehole_approval)
      tbl$well_approval <- as.factor(tbl$well_approval)
      tbl$driller_name <- as.factor(tbl$driller_name)
      tbl$bedrock_reached <- as.factor(tbl$bedrock_reached)
      tbl <- tbl[, c(
        "record_key",
        "borehole_id",
        "well_id",
        "borehole_name",
        "well_name",
        "borehole_approval",
        "well_approval",
        "driller_name",
        "completion_date",
        "latitude",
        "longitude",
        "depth_m",
        "bedrock_reached",
        "depth_to_bedrock_m",
        "permafrost_present",
        "permafrost_top",
        "permafrost_bot",
        "permafrost_ice_description",
        "is_well",
        "well_purpose_id",
        "static_water_level_m",
        "estimated_yield_lps"
      )]

      numeric_cols <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
      for (col in numeric_cols) {
        tbl[[col]] <- round_sig(tbl[[col]], digits = 2)
      }

      DT::datatable(
        tbl,
        rownames = FALSE,
        selection = "single",
        filter = "top",
        callback = DT::JS(sprintf(
          "table.on('click', 'tbody tr', function() {
            var rowData = table.row(this).data();
            if (rowData && rowData.length > 0) {
              Shiny.setInputValue(
                '%s',
                rowData[0],
                {priority: 'event'}
              );
            }
          });",
          ns("records_table_record_key")
        )),
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE))
        )
      )
    })

    observeEvent(input$records_table_record_key, {
      req(moduleData$records)
      record_key <- input$records_table_record_key
      if (
        length(record_key) == 1L &&
          record_key %in% moduleData$records$record_key
      ) {
        # Server-backed selectize inputs only keep a subset of choices in the
        # browser. Re-register choices so a table-selected value can be set
        # even when that record has not previously been loaded by selectize.
        update_record_selector(selected = record_key)
      }
    })
  })
}
