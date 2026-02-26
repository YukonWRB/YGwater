# UI and server code for editing existing borehole and well records

editBoreholesWellsUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    fluidRow(
      column(
        4,
        actionButton(ns("reload"), "Reload", icon = icon("refresh")),
        selectizeInput(
          ns("record_id"),
          "Select borehole/well",
          choices = NULL,
          multiple = FALSE,
          options = list(placeholder = "Choose a record")
        ),
        textInput(ns("borehole_name"), "Borehole/well name"),
        dateInput(ns("completion_date"), "Completion date", value = NULL),
        numericInput(ns("latitude"), "Latitude", value = NA_real_),
        numericInput(ns("longitude"), "Longitude", value = NA_real_),
        numericInput(ns("depth_m"), "Borehole depth (m)", value = NA_real_),
        numericInput(
          ns("depth_to_bedrock_m"),
          "Depth to bedrock (m)",
          value = NA_real_
        ),
        selectizeInput(
          ns("driller_id"),
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
        checkboxInput(ns("is_well"), "Has associated well record", value = FALSE),
        conditionalPanel(
          condition = "input.is_well == true",
          ns = ns,
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
        ),
        actionButton(ns("save"), "Save changes", class = "btn-primary"),
        actionButton(ns("clear"), "Clear form")
      ),
      column(
        4,
        h4("Current values"),
        DT::DTOutput(ns("records_table"))
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

    parse_role_array <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(character(0))
      }
      value <- gsub("[{}\"]", "", as.character(value))
      bits <- trimws(unlist(strsplit(value, ",")))
      bits <- bits[nzchar(bits)]
      unique(bits)
    }

    null_if_blank <- function(value) {
      if (is.null(value) || !length(value) || is.na(value) || !nzchar(trimws(as.character(value)))) {
        return(NULL)
      }
      value
    }

    maybe_num <- function(value) {
      if (is.null(value) || !length(value) || is.na(value)) {
        return(NULL)
      }
      as.numeric(value)
    }

    load_data <- function() {
      moduleData$records <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT b.borehole_id,
                b.borehole_name,
                b.latitude,
                b.longitude,
                b.completion_date,
                b.depth_m,
                b.depth_to_bedrock_m,
                b.driller_id,
                b.notes AS borehole_notes,
                b.share_with AS borehole_share_with,
                w.borehole_id IS NOT NULL AS is_well,
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
         ORDER BY b.borehole_name ASC;"
      )
      moduleData$drillers <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT driller_id, name FROM boreholes.drillers ORDER BY name ASC;"
      )
      moduleData$purposes <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT borehole_well_purpose_id, purpose_name FROM boreholes.borehole_well_purposes ORDER BY purpose_name ASC;"
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

    load_data()

    observe({
      req(moduleData$records)
      labels <- sprintf(
        "%s (ID %s)",
        moduleData$records$borehole_name,
        moduleData$records$borehole_id
      )
      updateSelectizeInput(
        session,
        "record_id",
        choices = stats::setNames(moduleData$records$borehole_id, labels),
        selected = isolate(input$record_id),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "driller_id",
        choices = stats::setNames(moduleData$drillers$driller_id, moduleData$drillers$name),
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
      rec <- moduleData$records[moduleData$records$borehole_id == as.integer(input$record_id), ]
      req(nrow(rec) == 1)
      updateTextInput(session, "borehole_name", value = rec$borehole_name)
      updateDateInput(session, "completion_date", value = rec$completion_date)
      updateNumericInput(session, "latitude", value = rec$latitude)
      updateNumericInput(session, "longitude", value = rec$longitude)
      updateNumericInput(session, "depth_m", value = rec$depth_m)
      updateNumericInput(session, "depth_to_bedrock_m", value = rec$depth_to_bedrock_m)
      updateSelectizeInput(session, "driller_id", selected = as.character(rec$driller_id))
      updateTextAreaInput(session, "borehole_notes", value = rec$borehole_notes)
      updateSelectizeInput(
        session,
        "share_with_borehole",
        selected = parse_role_array(rec$borehole_share_with)
      )
      updateCheckboxInput(session, "is_well", value = isTRUE(rec$is_well))
      updateSelectizeInput(session, "well_purpose_id", selected = as.character(rec$well_purpose_id))
      updateNumericInput(session, "casing_diameter_mm", value = rec$casing_diameter_mm)
      updateNumericInput(session, "casing_depth_to_m", value = rec$casing_depth_to_m)
      updateNumericInput(session, "screen_top_depth_m", value = rec$screen_top_depth_m)
      updateNumericInput(session, "screen_bottom_depth_m", value = rec$screen_bottom_depth_m)
      updateNumericInput(session, "static_water_level_m", value = rec$static_water_level_m)
      updateNumericInput(session, "estimated_yield_lps", value = rec$estimated_yield_lps)
      updateTextAreaInput(session, "well_notes", value = rec$well_notes)
      updateSelectizeInput(
        session,
        "share_with_well",
        selected = parse_role_array(rec$well_share_with)
      )
    })

    observeEvent(input$clear, {
      updateSelectizeInput(session, "record_id", selected = "")
      updateTextInput(session, "borehole_name", value = "")
      updateDateInput(session, "completion_date", value = as.Date(NA))
      updateNumericInput(session, "latitude", value = NA_real_)
      updateNumericInput(session, "longitude", value = NA_real_)
      updateNumericInput(session, "depth_m", value = NA_real_)
      updateNumericInput(session, "depth_to_bedrock_m", value = NA_real_)
      updateSelectizeInput(session, "driller_id", selected = "")
      updateTextAreaInput(session, "borehole_notes", value = "")
      updateSelectizeInput(session, "share_with_borehole", selected = character(0))
      updateCheckboxInput(session, "is_well", value = FALSE)
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

    observeEvent(input$reload, {
      load_data()
    })

    observeEvent(input$save, {
      req(input$record_id)
      borehole_id <- as.integer(input$record_id)
      req(!is.na(borehole_id))
      req(nzchar(trimws(input$borehole_name)))

      borehole_share <- unique(trimws(input$share_with_borehole))
      borehole_share <- borehole_share[nzchar(borehole_share)]
      if (!length(borehole_share)) borehole_share <- "public_reader"

      well_share <- unique(trimws(input$share_with_well))
      well_share <- well_share[nzchar(well_share)]
      if (!length(well_share)) {
        well_share <- borehole_share
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
                 depth_to_bedrock_m = $6,
                 driller_id = $7,
                 notes = $8,
                 share_with = $9::text[]
             WHERE borehole_id = $10;",
            params = list(
              trimws(input$borehole_name),
              if (is.null(input$completion_date) || is.na(input$completion_date)) NULL else as.Date(input$completion_date),
              maybe_num(input$latitude),
              maybe_num(input$longitude),
              maybe_num(input$depth_m),
              maybe_num(input$depth_to_bedrock_m),
              suppressWarnings(as.integer(null_if_blank(input$driller_id))),
              null_if_blank(input$borehole_notes),
              borehole_share,
              borehole_id
            )
          )

          has_well <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT EXISTS(SELECT 1 FROM boreholes.wells WHERE borehole_id = $1) AS exists;",
            params = list(borehole_id)
          )$exists[[1]]

          if (isTRUE(input$is_well)) {
            if (isTRUE(has_well)) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE boreholes.wells
                 SET well_purpose_id = $1,
                     casing_diameter_mm = $2,
                     casing_depth_to_m = $3,
                     screen_top_depth_m = $4,
                     screen_bottom_depth_m = $5,
                     static_water_level_m = $6,
                     estimated_yield_lps = $7,
                     notes = $8,
                     share_with = $9::text[]
                 WHERE borehole_id = $10;",
                params = list(
                  suppressWarnings(as.integer(null_if_blank(input$well_purpose_id))),
                  maybe_num(input$casing_diameter_mm),
                  maybe_num(input$casing_depth_to_m),
                  maybe_num(input$screen_top_depth_m),
                  maybe_num(input$screen_bottom_depth_m),
                  maybe_num(input$static_water_level_m),
                  maybe_num(input$estimated_yield_lps),
                  null_if_blank(input$well_notes),
                  well_share,
                  borehole_id
                )
              )
            } else {
              DBI::dbExecute(
                session$userData$AquaCache,
                "INSERT INTO boreholes.wells (
                    borehole_id,
                    well_purpose_id,
                    casing_diameter_mm,
                    casing_depth_to_m,
                    screen_top_depth_m,
                    screen_bottom_depth_m,
                    static_water_level_m,
                    estimated_yield_lps,
                    notes,
                    share_with
                 ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10::text[]);",
                params = list(
                  borehole_id,
                  suppressWarnings(as.integer(null_if_blank(input$well_purpose_id))),
                  maybe_num(input$casing_diameter_mm),
                  maybe_num(input$casing_depth_to_m),
                  maybe_num(input$screen_top_depth_m),
                  maybe_num(input$screen_bottom_depth_m),
                  maybe_num(input$static_water_level_m),
                  maybe_num(input$estimated_yield_lps),
                  null_if_blank(input$well_notes),
                  well_share
                )
              )
            }
          } else if (isTRUE(has_well)) {
            DBI::dbExecute(
              session$userData$AquaCache,
              "DELETE FROM boreholes.wells WHERE borehole_id = $1;",
              params = list(borehole_id)
            )
          }

          DBI::dbExecute(session$userData$AquaCache, "COMMIT")
          load_data()
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
      tbl <- moduleData$records[, c(
        "borehole_id", "borehole_name", "completion_date", "latitude", "longitude",
        "depth_m", "is_well", "well_purpose_id", "static_water_level_m"
      )]
      DT::datatable(tbl, rownames = FALSE, selection = "single", options = list(pageLength = 8))
    })

    observeEvent(input$records_table_rows_selected, {
      req(moduleData$records)
      idx <- input$records_table_rows_selected
      if (length(idx) == 1) {
        updateSelectizeInput(
          session,
          "record_id",
          selected = as.character(moduleData$records$borehole_id[idx])
        )
      }
    })
  })
}
