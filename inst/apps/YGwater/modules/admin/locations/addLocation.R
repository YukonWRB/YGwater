# UI and server code for add new location module

addLocationUI <- function(id) {
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


addLocation <- function(id, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Assign the input value to a reactive right away (passed in from the main server) as it's reset to NULL as soon as this module is loaded
    moduleInputs <- reactiveValues(
      location = if (!is.null(inputs$location)) inputs$location else NULL
    )

    shinyjs::hide("hydat_fill") # Hide the button right away, it's shown if applicable

    # Get some data from aquacache
    moduleData <- reactiveValues()

    ensure_character <- function(x) {
      if (is.null(x)) {
        character(0)
      } else {
        as.character(x)
      }
    }

    parse_ids <- function(x) {
      vals <- ensure_character(x)
      vals <- vals[nzchar(vals)]
      if (!length(vals)) {
        integer(0)
      } else {
        out <- suppressWarnings(as.integer(vals))
        unique(out[!is.na(out)])
      }
    }

    pending_network_selection <- reactiveVal(character(0))
    pending_network_new <- reactiveVal(NULL)
    pending_project_selection <- reactiveVal(character(0))
    pending_project_new <- reactiveVal(NULL)
    ownership_refresh <- reactiveVal(0)

    getModuleData <- function() {
      moduleData$exist_locs = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT l.location_id, l.location, l.name, l.name_fr, l.latitude, l.longitude, l.note, l.contact, l.share_with, l.location_type AS location_type_id, lt.type AS location_type, l.data_sharing_agreement_id, l.install_purpose, l.current_purpose, l.jurisdictional_relevance, l.anthropogenic_influence, l.sentinel_location, lmo.owner AS owner, COALESCE(string_agg(DISTINCT n.name, ', ' ORDER BY n.name), '') AS network 
        FROM locations l
        LEFT JOIN location_types lt ON l.location_type = lt.type_id
        LEFT JOIN LATERAL (
          SELECT lmoo.owner
          FROM locations_metadata_owners_operators lmoo
          WHERE lmoo.location_id = l.location_id
            AND lmoo.start_datetime <= NOW()
            AND (lmoo.end_datetime IS NULL OR lmoo.end_datetime > NOW())
            AND sub_location_id IS NULL
          ORDER BY lmoo.start_datetime DESC
          LIMIT 1
        ) lmo ON TRUE
        LEFT JOIN locations_networks ln ON l.location_id = ln.location_id
        LEFT JOIN networks n ON ln.network_id = n.network_id
        GROUP BY l.location_id, l.location, l.name, l.name_fr, l.latitude, l.longitude, l.note, l.contact, l.share_with, l.location_type, lt.type, l.data_sharing_agreement_id, l.install_purpose, l.current_purpose, l.jurisdictional_relevance, l.anthropogenic_influence, l.sentinel_location, lmo.owner"
      )
      moduleData$exist_locs$network <- factor(
        ifelse(
          is.na(moduleData$exist_locs$network),
          "",
          moduleData$exist_locs$network
        )
      )
      moduleData$loc_types = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM location_types"
      )
      moduleData$organizations = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM organizations"
      )
      # limit documents to those that are data sharing agreements, which requires a join on table document_types
      moduleData$agreements = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT document_id, name, description FROM documents WHERE type = (SELECT document_type_id FROM document_types WHERE document_type_en = 'data sharing agreement')"
      )
      moduleData$datums = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT datum_id, datum_name_en FROM datum_list"
      )
      moduleData$datum_conversions = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM datum_conversions WHERE current IS TRUE"
      )
      moduleData$networks = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT network_id, name FROM networks"
      )
      moduleData$projects = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT project_id, name FROM projects"
      )
      moduleData$users = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('public.locations');"
      ) # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table
    }

    getModuleData() # Initial data load

    current_owner_for_location <- function(location_id) {
      if (!isTruthy(location_id)) {
        return(NA_integer_)
      }
      owner_row <- DBI::dbGetQuery(
        session$userData$AquaCache,
        glue::glue_sql(
          "SELECT lmoo.owner
           FROM locations_metadata_owners_operators lmoo
           WHERE lmoo.location_id = {location_id}
             AND lmoo.start_datetime <= NOW()
             AND (lmoo.end_datetime IS NULL OR lmoo.end_datetime > NOW())
             AND sub_location_id IS NULL
           ORDER BY lmoo.start_datetime DESC
           LIMIT 1;",
          .con = session$userData$AquaCache
        )
      )
      if (nrow(owner_row) == 0) {
        NA_integer_
      } else {
        owner_row$owner[1]
      }
    }

    update_current_owner <- function(location_id) {
      updateSelectizeInput(
        session,
        "loc_owner",
        selected = current_owner_for_location(location_id)
      )
    }

    output$ui <- renderUI({
      req(
        moduleData$exist_locs,
        moduleData$loc_types,
        moduleData$organizations,
        moduleData$agreements,
        moduleData$datums,
        moduleData$networks,
        moduleData$projects,
        moduleData$users
      )
      tagList(
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
            open = "locations_table_panel",
            accordion_panel(
              id = ns("locations_table_panel"),
              title = "Select location to modify",
              DT::DTOutput(ns("loc_table"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          htmlOutput(ns("hydat_note"))
        ),
        textInput(
          ns("loc_code"),
          "Location code (must not exist already)",
          width = "100%"
        ),
        actionButton(ns("hydat_fill"), "Auto-fill from HYDAT"),
        splitLayout(
          cellWidths = c("50%", "50%"),
          textInput(
            ns("loc_name"),
            "Location name (must not exist already)",
            if (isTruthy(moduleInputs$location)) {
              moduleInputs$location
            } else {
              NULL
            },
            width = "100%"
          ),
          textInput(
            ns("loc_name_fr"),
            "French location name (must not exist already)",
            width = "100%"
          )
        ),
        splitLayout(
          cellWidths = c("40%", "40%", "20%"),
          numericInput(
            ns("lat"),
            "Latitude (decimal degrees, WGS84)",
            value = NA,
            width = "100%"
          ) |>
            tooltip(
              "Latitude in decimal degrees, e.g. 62.1234. Positive values indicate northern hemisphere."
            ),
          numericInput(
            ns("lon"),
            "Longitude (decimal degrees, WGS84)",
            value = NA,
            width = "100%",
          ) |>
            tooltip(
              "Longitude in decimal degrees, e.g. -135.1234. Negative values indicate western hemisphere."
            ),
          actionButton(
            ns("open_map"),
            "Choose or show coordinates on map",
            icon = icon("map-location-dot"),
            width = "100%",
            # Bump it down a bit to align with numericInputs
            style = "margin-top: 30px;"
          )
        ),
        splitLayout(
          cellWidths = c("40%", "40%", "20%"),
          uiOutput(ns("lat_warning")),
          uiOutput(ns("lon_warning"))
        ),

        selectizeInput(
          ns("loc_type"),
          "Location type",
          choices = stats::setNames(
            moduleData$loc_types$type_id,
            moduleData$loc_types$type
          ),
          multiple = TRUE, # This is to force a default of nothing selected - overridden with options
          options = list(maxItems = 1),
          width = "100%"
        ),
        selectizeInput(
          ns("share_with"),
          "Share with groups (1 or more, type your own if not in list)",
          choices = moduleData$users$role_name,
          selected = "public_reader",
          multiple = TRUE,
          options = list(create = TRUE),
          width = "100%"
        ),

        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
          selectizeInput(
            ns("loc_owner"),
            "Owner (type your own if not in list)",
            choices = stats::setNames(
              moduleData$organizations$organization_id,
              moduleData$organizations$name
            ),
            multiple = TRUE, # This is to force a default of nothing selected - overridden with options
            options = list(maxItems = 1, create = TRUE),
            width = "100%"
          ),
          textInput(
            ns("loc_contact"),
            "Contact details if different than owner default (optional)",
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          actionButton(
            ns("manage_ownership"),
            "Manage ownership history",
            icon = icon("clock-rotate-left"),
            width = "100%"
          )
        ),

        selectizeInput(
          ns("data_sharing_agreement"),
          "Data sharing agreement",
          choices = stats::setNames(
            moduleData$agreements$document_id,
            moduleData$agreements$name
          ),
          options = list(
            placeholder = "Optional - add the document first if needed"
          ),
          width = "100%",
          multiple = FALSE
        ),

        splitLayout(
          cellWidths = c("0%", "33.3%", "33.3%", "33.3%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
          selectizeInput(
            ns("datum_id_from"),
            "Vertical datum from (Assumed datum is station 0)",
            choices = stats::setNames(
              moduleData$datums$datum_id,
              titleCase(moduleData$datums$datum_name_en, "en")
            ),
            selected = 10,
            width = "100%",
            multiple = FALSE
          ) |>
            tooltip(
              "This should almost always be 'Assumed Datum', the local measurements."
            ),
          selectizeInput(
            ns("datum_id_to"),
            "Vertical datum to (Use assumed datum if no conversion to apply)",
            choices = stats::setNames(
              moduleData$datums$datum_id,
              titleCase(moduleData$datums$datum_name_en, "en")
            ),
            selected = 10,
            width = "100%"
          ) |>
            tooltip(
              "This is the datum you want to convert to. Use 'Assumed Datum' if no conversion is needed."
            ),
          numericInput(
            ns("elev"),
            "Elevation conversion (meters, use 0 if not converting)",
            value = 0,
            width = "100%"
          )
        ),
        uiOutput(ns("elev_warning")),

        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
          selectizeInput(
            ns("network"),
            "Network(s) (type your own if not in list)",
            choices = stats::setNames(
              moduleData$networks$network_id,
              moduleData$networks$name
            ),
            multiple = TRUE,
            options = list(
              create = TRUE,
              placeholder = "Optional but recommended"
            ), # With a choice to allow users to add a network
            width = "100%"
          ),
          selectizeInput(
            ns("project"),
            "Project(s) (type your own if not in list)",
            choices = stats::setNames(
              moduleData$projects$project_id,
              moduleData$projects$name
            ),
            multiple = TRUE,
            options = list(
              create = TRUE,
              placeholder = "Optional"
            ), # With a choice to allow users to add a project
            width = "100%"
          )
        ),

        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
          checkboxInput(
            ns("loc_jurisdictional_relevance"),
            "Publicly relevant (i.e. should be seen by the public)",
            value = TRUE
          ),
          checkboxInput(
            ns("loc_anthropogenic_influence"),
            "Influenced by human activity (dams, upstream mining, etc.)",
            value = FALSE
          )
        ),

        textInput(
          ns("loc_install_purpose"),
          "Installation or establishment purpose (optional)",
          placeholder = "Optional",
          width = "100%"
        ),
        textInput(
          ns("loc_current_purpose"),
          "Current purpose (optional)",
          placeholder = "Optional",
          width = "100%"
        ),
        textInput(
          ns("loc_note"),
          "Location note",
          placeholder = "Optional",
          width = "100%"
        ),
        actionButton(ns("add_loc"), "Add location", width = "100%")
      )
    })

    ## Observers to modify existing entry ##########################################
    selected_loc <- reactiveVal(NULL)
    ownership_edit_id <- reactiveVal(NULL)

    shinyjs::disable("manage_ownership")

    observe({
      if (input$mode == "modify" && !is.null(selected_loc())) {
        shinyjs::enable("manage_ownership")
      } else {
        shinyjs::disable("manage_ownership")
      }
    })

    output$loc_table <- DT::renderDT({
      tbl <- moduleData$exist_locs
      tbl$location <- as.factor(tbl$location)
      tbl$name <- as.factor(tbl$name)
      tbl$name_fr <- as.factor(tbl$name_fr)
      tbl$location_type <- as.factor(tbl$location_type)
      # Truncate the notes to 30 characters
      tbl$note <- paste0(substr(tbl$note, 1, 30), "...")
      DT::datatable(
        tbl,
        selection = "single",
        options = list(
          columnDefs = list(list(targets = c(0, 9), visible = FALSE)), # Hide location_id and location_type_id columns
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
    }) |>
      bindEvent(moduleData$exist_locs)

    # Observe row selection and update inputs accordingly
    observeEvent(input$loc_table_rows_selected, {
      sel <- input$loc_table_rows_selected
      if (length(sel) > 0) {
        loc_id <- moduleData$exist_locs[sel, "location_id"]
        selected_loc(loc_id)
        details <- moduleData$exist_locs[
          moduleData$exist_locs$location_id == loc_id,
        ]
        datum_details <- moduleData$datum_conversions[
          moduleData$datum_conversions$location_id == loc_id,
        ]

        if (nrow(details) > 0) {
          updateTextInput(session, "loc_code", value = details$location)
          updateTextInput(session, "loc_name", value = details$name)
          updateTextInput(session, "loc_name_fr", value = details$name_fr)
          updateSelectizeInput(
            session,
            "loc_type",
            selected = details$location_type_id
          )
          updateNumericInput(session, "lat", value = details$latitude)
          updateNumericInput(session, "lon", value = details$longitude)
          updateSelectizeInput(
            session,
            "share_with",
            selected = parse_share_with(details$share_with)
          )

          update_current_owner(loc_id)
          updateTextInput(session, "loc_contact", value = details$contact)
          updateSelectizeInput(
            session,
            "data_sharing_agreement",
            selected = details$data_sharing_agreement_id
          )
          updateSelectizeInput(
            session,
            "datum_id_from",
            selected = datum_details$datum_id_from
          )
          updateSelectizeInput(
            session,
            "datum_id_to",
            selected = datum_details$datum_id_to
          )
          updateNumericInput(
            session,
            "elev",
            value = datum_details$conversion_m
          )
          nids <- DBI::dbGetQuery(
            session$userData$AquaCache,
            sprintf(
              "SELECT network_id FROM locations_networks WHERE location_id = %d",
              loc_id
            )
          )
          updateSelectizeInput(session, "network", selected = nids$network_id)
          pids <- DBI::dbGetQuery(
            session$userData$AquaCache,
            sprintf(
              "SELECT project_id FROM locations_projects WHERE location_id = %d",
              loc_id
            )
          )
          updateSelectizeInput(session, "project", selected = pids$project_id)
          updateCheckboxInput(
            session,
            "loc_jurisdictional_relevance",
            value = details$jurisdictional_relevance
          )
          updateCheckboxInput(
            session,
            "loc_anthropogenic_influence",
            value = details$anthropogenic_influence
          )
          updateTextInput(
            session,
            "loc_install_purpose",
            value = details$install_purpose
          )
          updateTextInput(
            session,
            "loc_current_purpose",
            value = details$current_purpose
          )
          updateTextInput(session, "loc_note", value = details$note)
        }
      } else {
        selected_loc(NULL)
      }
    })

    ownership_records <- reactive({
      ownership_refresh()
      req(selected_loc())
      DBI::dbGetQuery(
        session$userData$AquaCache,
        glue::glue_sql(
          "SELECT lmoo.id,
                  lmoo.owner,
                  owner_org.name AS owner_name,
                  lmoo.operator,
                  operator_org.name AS operator_name,
                  lmoo.start_datetime,
                  lmoo.end_datetime,
                  lmoo.note
           FROM locations_metadata_owners_operators lmoo
           LEFT JOIN organizations owner_org ON owner_org.organization_id = lmoo.owner
           LEFT JOIN organizations operator_org ON operator_org.organization_id = lmoo.operator
           WHERE lmoo.location_id = {selected_loc()}
           AND sub_location_id IS NULL
           ORDER BY lmoo.start_datetime;",
          .con = session$userData$AquaCache
        )
      )
    })

    output$ownership_table <- DT::renderDT({
      tbl <- ownership_records()
      DT::datatable(
        tbl,
        selection = "single",
        options = list(
          columnDefs = list(
            list(targets = c(0, 1, 3), visible = FALSE)
          ),
          pageLength = 8,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    show_ownership_history_modal <- function() {
      showModal(modalDialog(
        title = "Ownership history",
        DT::DTOutput(ns("ownership_table")),
        footer = tagList(
          actionButton(ns("add_ownership"), "Add period"),
          actionButton(ns("edit_ownership"), "Edit selected"),
          modalButton("Close")
        ),
        size = "l",
        easyClose = TRUE
      ))
    }

    observeEvent(input$manage_ownership, {
      req(selected_loc())
      show_ownership_history_modal()
    })

    observeEvent(input$add_ownership, {
      ownership_edit_id(NULL)
      showModal(modalDialog(
        title = "Add ownership period",
        selectizeInput(
          ns("ownership_owner"),
          "Owner",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          multiple = TRUE,
          options = list(maxItems = 1),
          width = "100%"
        ),
        selectizeInput(
          ns("ownership_operator"),
          "Operator",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          multiple = TRUE,
          options = list(maxItems = 1),
          width = "100%"
        ),
        dateInput(
          ns("ownership_start"),
          "Start date",
          value = Sys.Date()
        ),
        dateInput(
          ns("ownership_end"),
          "End date (leave blank if ongoing)",
          value = NA
        ),
        textInput(
          ns("ownership_note"),
          "Note (optional)"
        ),
        footer = tagList(
          actionButton(ns("save_ownership"), "Save"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$edit_ownership, {
      req(selected_loc())
      selected_row <- input$ownership_table_rows_selected
      if (length(selected_row) == 0) {
        showModal(modalDialog(
          "Select an ownership period to edit.",
          easyClose = TRUE
        ))
        return()
      }
      record <- ownership_records()[selected_row, ]
      ownership_edit_id(record$id)
      showModal(modalDialog(
        title = "Edit ownership period",
        selectizeInput(
          ns("ownership_owner"),
          "Owner",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          selected = record$owner,
          options = list(maxItems = 1),
          width = "100%"
        ),
        selectizeInput(
          ns("ownership_operator"),
          "Operator",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          selected = record$operator,
          options = list(maxItems = 1),
          width = "100%"
        ),
        dateInput(
          ns("ownership_start"),
          "Start date",
          value = as.Date(record$start_datetime)
        ),
        dateInput(
          ns("ownership_end"),
          "End date (leave blank if ongoing)",
          value = as.Date(record$end_datetime)
        ),
        textInput(
          ns("ownership_note"),
          "Note (optional)",
          value = record$note
        ),
        footer = tagList(
          actionButton(ns("save_ownership"), "Save"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$save_ownership, {
      req(selected_loc())
      if (!isTruthy(input$ownership_owner)) {
        showModal(modalDialog(
          "Owner is required.",
          easyClose = TRUE
        ))
        return()
      }

      owner_id <- as.integer(input$ownership_owner)
      operator_id <- if (isTruthy(input$ownership_operator)) {
        as.integer(input$ownership_operator)
      } else {
        owner_id
      }
      start_dt <- as.POSIXct(input$ownership_start, tz = "UTC")
      end_dt <- if (isTruthy(input$ownership_end)) {
        as.POSIXct(input$ownership_end, tz = "UTC")
      } else {
        NA
      }

      if (!is.na(end_dt) && end_dt <= start_dt) {
        showModal(modalDialog(
          "End date must be after the start date.",
          easyClose = TRUE
        ))
        return()
      }

      note_sql <- if (isTruthy(input$ownership_note)) {
        input$ownership_note
      } else {
        DBI::SQL("NULL")
      }
      end_dt_sql <- if (is.na(end_dt)) {
        DBI::SQL("NULL")
      } else {
        end_dt
      }

      if (is.null(ownership_edit_id())) {
        DBI::dbExecute(
          session$userData$AquaCache,
          glue::glue_sql(
            "INSERT INTO locations_metadata_owners_operators
              (location_id, owner, operator, start_datetime, end_datetime, note)
             VALUES
              ({selected_loc()}, {owner_id}, {operator_id}, {start_dt}, {end_dt_sql}, {note_sql});",
            .con = session$userData$AquaCache
          )
        )
      } else {
        DBI::dbExecute(
          session$userData$AquaCache,
          glue::glue_sql(
            "UPDATE locations_metadata_owners_operators
             SET owner = {owner_id},
                 operator = {operator_id},
                 start_datetime = {start_dt},
                 end_datetime = {end_dt_sql},
                 note = {note_sql}
             WHERE id = {ownership_edit_id()};",
            .con = session$userData$AquaCache
          )
        )
      }

      ownership_refresh(ownership_refresh() + 1)
      update_current_owner(selected_loc())
      ownership_edit_id(NULL)
      show_ownership_history_modal()
    })

    observeEvent(input$mode, {
      if (input$mode == "modify") {
        updateActionButton(session, "add_loc", label = "Update location")
        updateTextInput(session, "loc_code", label = "Location code")
        updateTextInput(session, "loc_name", label = "Location name")
        updateTextInput(session, "loc_name_fr", label = "French location name")
      } else {
        # Adding a new station
        updateActionButton(session, "add_loc", label = "Add location")
        updateTextInput(
          session,
          "loc_code",
          label = "Location code (must not exist already)"
        )
        updateTextInput(
          session,
          "loc_name",
          label = "Location name (must not exist already)"
        )
        updateTextInput(
          session,
          "loc_name_fr",
          label = "French location name (must not exist already)"
        )

        # If was on 'modify' prior, show a modal to the user asking if they want to clear fields
        if (!is.null(selected_loc())) {
          if (!just_updated()) {
            showModal(modalDialog(
              title = "Clear fields?",
              "You have switched to 'add new' mode. Do you want to clear all fields to add a new location?",
              easyClose = TRUE,
              footer = tagList(
                actionButton(ns("close"), "No, keep current values"),
                actionButton(
                  ns("confirm_clear_fields"),
                  "Yes, clear fields"
                )
              )
            ))
          } else {
            just_updated(FALSE)
          }
        }
      }
    })

    observeEvent(input$confirm_clear_fields, {
      # Clear all fields
      updateTextInput(session, "loc_code", value = "")
      updateTextInput(session, "loc_name", value = "")
      updateTextInput(session, "loc_name_fr", value = "")
      updateNumericInput(session, "lat", value = NA)
      updateNumericInput(session, "lon", value = NA)
      updateSelectizeInput(session, "loc_type", selected = character(0))
      updateSelectizeInput(
        session,
        "share_with",
        selected = "public_reader"
      )
      updateSelectizeInput(session, "loc_owner", selected = character(0))
      updateTextInput(session, "loc_contact", value = "")
      updateSelectizeInput(
        session,
        "data_sharing_agreement",
        selected = character(0)
      )
      updateSelectizeInput(session, "datum_id_from", selected = 10)
      updateSelectizeInput(session, "datum_id_to", selected = character(0))
      updateNumericInput(session, "elev", value = NA)
      updateSelectizeInput(session, "network", selected = character(0))
      updateSelectizeInput(session, "project", selected = character(0))
      updateCheckboxInput(
        session,
        "loc_jurisdictional_relevance",
        value = TRUE
      )
      updateCheckboxInput(
        session,
        "loc_anthropogenic_influence",
        value = FALSE
      )
      updateTextInput(session, "loc_install_purpose", value = "")
      updateTextInput(session, "loc_current_purpose", value = "")
      updateTextInput(session, "loc_note", value = "")
      removeModal()
      selected_loc(NULL)
    })

    observeEvent(input$close, {
      removeModal()
    })

    ## Hydat fill ###############################################################
    # Detect if the user's location code is present in hydat. If so, show a button to enable them to auto-populate fields with hydat info
    hydat <- reactiveValues(exists = FALSE, stns = NULL)
    shinyjs::hide("hydat_fill")

    safe <- function(expr) tryCatch(expr, error = function(e) NULL)

    # Download hydat from Shiny, bypassing tidyhydat downloader because it's blocked on a HEAD request
    download_hydat <- function() {
      dest_dir <- tidyhydat::hy_dir()
      dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
      remote_ver <- tidyhydat::hy_remote() # e.g. "20250701"
      # hy_base_url() is internal; fetch it safely
      hy_base_url <- get("hy_base_url", asNamespace("tidyhydat"))()
      url <- paste0(hy_base_url, "Hydat_sqlite3_", remote_ver, ".zip")

      tmp_zip <- tempfile("hydat_", fileext = ".zip")
      ex_dir <- file.path(tempdir(), "hydat_extracted")
      dir.create(ex_dir, showWarnings = FALSE)

      # Use GET (works when HEAD is blocked)
      curl::curl_download(
        url,
        destfile = tmp_zip,
        handle = curl::new_handle(
          useragent = "Mozilla/5.0",
          followlocation = TRUE
        )
      )
      utils::unzip(tmp_zip, exdir = ex_dir, overwrite = TRUE)

      # Copy the sqlite DB into place (name inside the zip can vary)
      sqlite_src <- list.files(
        ex_dir,
        pattern = "\\.sqlite3$",
        full.names = TRUE
      )
      if (length(sqlite_src) == 0) {
        stop("HYDAT zip did not contain a .sqlite3 file")
      }
      hydat_path <- file.path(dest_dir, "Hydat.sqlite3")
      file.copy(sqlite_src[1], hydat_path, overwrite = TRUE)
      unlink(c(tmp_zip, ex_dir), recursive = TRUE)
    }

    db_path <- safe(tidyhydat::hy_downloaded_db())
    download_new_hydat <- FALSE
    if (!is.null(db_path)) {
      if (file.exists(db_path)) {
        # Compare versions safely
        local_ver <- safe(as.Date(tidyhydat::hy_version(db_path)$Date))
        local_ver <- gsub("-", "", as.character(local_ver))
        remote_ver <- safe(tidyhydat::hy_remote())
        if (!is.null(local_ver) && !is.null(remote_ver)) {
          if (local_ver != remote_ver) {
            showNotification(
              "A newer HYDAT is available. Attempting update, please be patient. A success message will appear once the download is complete.",
              type = "warning",
              duration = 20
            )
            safe(download_hydat()) # Download new version
            # try to refresh version
            db_path <- safe(tidyhydat::hy_downloaded_db())
            local_ver <- if (!is.null(db_path)) {
              safe(as.Date(tidyhydat::hy_version(db_path)$Date))
            } else {
              NULL
            }
            if (!is.null(local_ver) && !is.null(remote_ver)) {
              if (identical(local_ver, remote_ver)) {
                showNotification("HYDAT updated.", type = "message")
              } else {
                showNotification(
                  "HYDAT update failed; using existing local copy.",
                  type = "error"
                )
              }
            } else {
              showNotification(
                "HYDAT update failed; using existing local copy.",
                type = "error"
              )
            }
          }
        }
        stns <- safe(tidyhydat::hy_stations())
        if (!is.null(stns)) {
          hydat$stns <- stns$STATION_NUMBER
          hydat$exists <- TRUE
        } else {
          hydat$stns <- character(0)
          hydat$exists <- TRUE
        }
      } else {
        download_new_hydat <- TRUE
      }
    } else {
      download_new_hydat <- TRUE
    }

    if (download_new_hydat) {
      showNotification(
        "No local HYDAT found. Attempting download, please be patient. A success message will appear once the download is complete.",
        type = "warning",
        duration = 20
      )
      safe(download_hydat()) # Download new version
      db_path <- safe(tidyhydat::hy_downloaded_db())
      if (!is.null(db_path)) {
        if (file.exists(db_path)) {
          stns <- safe(tidyhydat::hy_stations())
          if (!is.null(stns)) {
            hydat$stns <- stns$STATION_NUMBER
            hydat$exists <- TRUE
            showNotification("HYDAT downloaded.", type = "message")
          } else {
            hydat$stns <- character(0)
            hydat$exists <- TRUE
            showNotification(
              "HYDAT download failed; HYDAT functions will not be available.",
              type = "error"
            )
          }
        }
      } else {
        hydat$stns <- character(0)
        hydat$exists <- FALSE
        showNotification(
          "HYDAT download failed; HYDAT functions will not be available.",
          type = "error"
        )
      }
    }

    if (hydat$exists) {
      output$hydat_note <- renderUI({
        HTML(
          "<b>Entering a WSC code will allow you to auto-populate fields with HYDAT information.</b><br>"
        )
      })
    }

    observeEvent(
      input$loc_code,
      {
        # Observe loc_code inputs and, if possible, show the button to auto-populate fields
        req(input$loc_code)
        if (hydat$exists) {
          if (input$loc_code %in% hydat$stns) {
            shinyjs::show("hydat_fill")
          } else {
            shinyjs::hide("hydat_fill")
          }
        } else {
          shinyjs::hide("hydat_fill") # If HYDAT is not available, hide the button
        }

        # Check if the location code already exists in the database. If yes, make the selectizeInput pink
        if (input$mode == "modify") {
          shinyjs::js$backgroundCol(ns("loc_code"), "#fff")
        } else {
          if (input$loc_code %in% moduleData$exist_locs$location) {
            shinyjs::js$backgroundCol(ns("loc_code"), "#fdd")
            showNotification(
              "This location code already exists and you're on the 'add new timeseries' mode.",
              type = "warning",
              duration = 10
            )
          } else {
            shinyjs::js$backgroundCol(ns("loc_code"), "#fff")
          }
        }
      },
      ignoreInit = TRUE
    )

    ## Validation helpers for other inputs -----------------------------------
    observeEvent(
      input$loc_name,
      {
        req(input$loc_name)
        if (input$mode == "modify") {
          shinyjs::js$backgroundCol(ns("loc_name"), "#fff")
        } else {
          if (input$loc_name %in% moduleData$exist_locs$name) {
            shinyjs::js$backgroundCol(ns("loc_name"), "#fdd")
          } else {
            shinyjs::js$backgroundCol(ns("loc_name"), "#fff")
          }
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$loc_name_fr,
      {
        req(input$loc_name_fr)
        if (input$mode == "modify") {
          shinyjs::js$backgroundCol(ns("loc_name_fr"), "#fff")
        } else {
          if (input$loc_name_fr %in% moduleData$exist_locs$name_fr) {
            shinyjs::js$backgroundCol(ns("loc_name_fr"), "#fdd")
          } else {
            shinyjs::js$backgroundCol(ns("loc_name_fr"), "#fff")
          }
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$hydat_fill,
      {
        req(input$loc_code, hydat$exists)
        # Get the station info from hydat
        stn <- tidyhydat::hy_stations(input$loc_code)
        if (nrow(stn) == 0) {
          return()
        }
        if (hydat$exists) {
          datum <- tidyhydat::hy_stn_datum_conv(input$loc_code)
        } else {
          datum <- data.frame()
        }
        if (nrow(datum) == 0) {
          showModal(modalDialog(
            "No datum conversion found for this station in HYDAT."
          ))
          updateSelectizeInput(session, "datum_id_from", selected = 10)
          updateSelectizeInput(session, "datum_id_to", selected = 10)
          updateNumericInput(session, "elev", value = 0)
        } else {
          datum_list <- tidyhydat::hy_datum_list()
          # Replace DATUM_FROM with DATUM_ID
          datum$DATUM_FROM_ID <- datum_list$DATUM_ID[match(
            datum$DATUM_FROM,
            datum_list$DATUM_EN
          )]
          # Replace DATUM_TO with DATUM_ID
          datum$DATUM_TO_ID <- datum_list$DATUM_ID[match(
            datum$DATUM_TO,
            datum_list$DATUM_EN
          )]

          # Drop original DATUM_FROM and DATUM_TO columns
          datum <- datum[, c(
            "STATION_NUMBER",
            "DATUM_FROM_ID",
            "DATUM_TO_ID",
            "CONVERSION_FACTOR"
          )]
          updateSelectizeInput(
            session,
            "datum_id_from",
            selected = datum$DATUM_FROM_ID[nrow(datum)]
          )
          updateSelectizeInput(
            session,
            "datum_id_to",
            selected = datum$DATUM_TO_ID[nrow(datum)]
          )
          updateNumericInput(
            session,
            "elev",
            value = datum$CONVERSION_FACTOR[nrow(datum)]
          )
        }

        updateTextInput(
          session,
          "loc_name",
          value = titleCase(stn$STATION_NAME, "en")
        )
        updateNumericInput(session, "lat", value = stn$LATITUDE)
        updateNumericInput(session, "lon", value = stn$LONGITUDE)

        updateSelectizeInput(
          session,
          "loc_owner",
          selected = moduleData$organizations[
            moduleData$organizations$name == "Water Survey of Canada",
            "organization_id"
          ]
        )
        updateTextInput(
          session,
          "loc_note",
          value = paste0(
            "Station metadata from HYDAT version ",
            substr(tidyhydat::hy_version()$Date[1], 1, 10)
          )
        )
        updateSelectizeInput(
          session,
          "network",
          selected = moduleData$networks[
            moduleData$networks$name == "Canada Yukon Hydrometric Network",
            "network_id"
          ]
        )
      },
      ignoreInit = TRUE
    )

    ## Make messages for lat/lon warnings #########################################
    # Reactive values to track warnings
    warnings <- reactiveValues(lat = NULL, lon = NULL, elev = NULL)

    # Update reactive values for latitude warning
    observe({
      req(input$lat)
      if (input$lat < 0) {
        warnings$lat <- "Warning: Latitude is negative. Are you sure your location is in the southern hemisphere?"
        shinyjs::js$backgroundCol(ns("lat"), "#fdd")
      } else if (input$lat > 90 || input$lat < -90) {
        warnings$lat <- "Error: Latitude cannot exceed + or - 90 degrees."
        shinyjs::js$backgroundCol(ns("lat"), "#fdd")
      } else {
        warnings$lat <- NULL
        shinyjs::js$backgroundCol(ns("lat"), "#fff")
      }
    })
    # Update reactive values for longitude warning
    observe({
      req(input$lon)
      if (input$lon < -180 || input$lon > 180) {
        warnings$lon <- "Error: Longitude must be between -180 and 180 degrees."
        shinyjs::js$backgroundCol(ns("lon"), "#fdd")
      } else if (input$lon > 0) {
        warnings$lon <- "Warning: Longitude is positive. Are you sure your location is east of the prime meridian?"
        shinyjs::js$backgroundCol(ns("lon"), "#fdd")
      } else {
        warnings$lon <- NULL
        shinyjs::js$backgroundCol(ns("lon"), "#fff")
      }
    })

    # Render latitude and longitude warnings dynamically
    output$lat_warning <- renderUI({
      if (!is.null(warnings$lat)) {
        div(
          style = "color: red; font-size: 12px; margin-top: -10px; margin-bottom: 10px",
          warnings$lat
        )
      }
    })
    output$lon_warning <- renderUI({
      if (!is.null(warnings$lon)) {
        div(
          style = "color: red; font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          warnings$lon
        )
      }
    })

    ## Map picker ##############################################################
    map_center <- reactiveVal(list(lat = 64.0, lon = -135.0, zoom = 4))
    map_selection <- reactiveVal(NULL)

    output$location_map <- leaflet::renderLeaflet({
      center <- map_center()
      sel <- isolate(map_selection())

      m <- leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 19)) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satellite"
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c("Esri.WorldTopoMap", "Satellite"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addScaleBar(
          options = leaflet::scaleBarOptions(imperial = FALSE)
        ) %>%
        leaflet::setView(lng = center$lon, lat = center$lat, zoom = center$zoom)

      if (!is.null(sel)) {
        m <- m %>%
          leaflet::addCircleMarkers(
            lng = sel$lon,
            lat = sel$lat,
            radius = 6,
            color = "#007B8A",
            fillOpacity = 0.9,
            group = "selected_point"
          )
      }

      m
    }) %>%
      bindEvent(input$open_map)

    draw_selected_point <- function() {
      sel <- isolate(map_selection())
      if (is.null(sel)) {
        return(invisible(NULL))
      }

      leaflet::leafletProxy(ns("location_map"), session = session) %>%
        leaflet::clearGroup("selected_point") %>%
        leaflet::addCircleMarkers(
          lng = sel$lon,
          lat = sel$lat,
          radius = 6,
          color = "#007B8A",
          fillOpacity = 0.9,
          group = "selected_point"
        )
    }

    output$map_zoom_note <- renderUI({
      zoom <- input$location_map_zoom
      if (is.null(zoom)) {
        return(NULL)
      }
      if (zoom < 14) {
        div(
          style = "color: #b42318; font-size: 14px; margin-top: 8px;",
          "Zoom in to level 14 or higher to save this location."
        )
      } else {
        div(
          style = "color: #027a48; font-size: 14px; margin-top: 8px;",
          "Zoom level is sufficient to save."
        )
      }
    })

    observeEvent(input$open_map, {
      current_lat <- input$lat
      current_lon <- input$lon

      if (isTruthy(current_lat) && isTruthy(current_lon)) {
        map_center(list(lat = current_lat, lon = current_lon, zoom = 12))
        map_selection(list(lat = current_lat, lon = current_lon))
      } else {
        map_center(list(lat = 64.0, lon = -135.0, zoom = 4))
        map_selection(NULL)
      }

      showModal(modalDialog(
        title = "Select location on map",
        leaflet::leafletOutput(ns("location_map"), height = "400px"),
        uiOutput(ns("map_zoom_note")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_location_map"), "Use selected location")
        ),
        size = "l",
        easyClose = TRUE
      ))
    })

    observeEvent(input$location_map_click, {
      click <- input$location_map_click
      map_selection(list(lat = click$lat, lon = click$lng))
      draw_selected_point()
    })

    observeEvent(input$location_map_zoom, {
      req(input$location_map_zoom)
      if (input$location_map_zoom < 14) {
        shinyjs::disable("save_location_map")
      } else {
        shinyjs::enable("save_location_map")
      }
    })

    observeEvent(input$save_location_map, {
      if (is.null(input$location_map_zoom) || input$location_map_zoom < 14) {
        showNotification(
          "Zoom in to level 14 or higher before saving.",
          type = "warning"
        )
        return()
      }
      selection <- map_selection()
      if (is.null(selection)) {
        showNotification("Click a point on the map to select a location.")
        return()
      }
      updateNumericInput(session, "lat", value = selection$lat)
      updateNumericInput(session, "lon", value = selection$lon)
      removeModal()
    })

    # Elevation conversion warning ################################################
    observe({
      req(input$datum_id_from, input$datum_id_to, input$elev)
      if (input$datum_id_from == input$datum_id_to && input$elev != 0) {
        shinyjs::js$backgroundCol(ns("elev"), "#fdd")
        warnings$elev <- "Warning: Elevation conversion is set to a non-zero value but the from/to datums are the same. Are you sure you want to do this?"
      } else {
        shinyjs::js$backgroundCol(ns("elev"), "#fff")
        warnings$elev <- NULL
      }
    })
    output$elev_warning <- renderUI({
      if (!is.null(warnings$elev)) {
        div(
          style = "color: red; font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          warnings$elev
        )
      }
    })

    ## Allow users to add a few things to the DB besides locations ###################################
    ## If user types in their own network/project/owner/share_with, bring up a modal to add it to the database. This requires updating moduleData and the selectizeInput choices

    ### Observe the network selectizeInput for new networks #######################
    observeEvent(
      input$network,
      {
        vals <- ensure_character(input$network)
        pending_network_selection(vals)

        existing_ids <- ensure_character(moduleData$networks$network_id)
        new_vals <- setdiff(vals, existing_ids)
        new_vals <- new_vals[nzchar(new_vals)]

        if (!length(new_vals)) {
          pending_network_new(NULL)
          return()
        }

        new_val <- new_vals[length(new_vals)]
        pending_network_new(new_val)

        net_types <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT id, name FROM network_project_types"
        )
        showModal(modalDialog(
          textInput(
            ns("network_name"),
            "Network name",
            value = if (nzchar(new_val)) new_val else input$loc_name
          ),
          textInput(ns("network_name_fr"), "Network name French (optional)"),
          textInput(ns("network_description"), "Network description"),
          textInput(
            ns("network_description_fr"),
            "Network description French (optional)"
          ),
          selectizeInput(
            ns("network_type"),
            "Network type",
            stats::setNames(net_types$id, net_types$name),
            multiple = FALSE
          ),
          actionButton(ns("add_network"), "Add network")
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$add_network,
      {
        # Close the modal dialog
        # Check that mandatory fields are filled in
        if (!isTruthy(input$network_name)) {
          shinyjs::js$backgroundCol(ns("network_name"), "#fdd")
          return()
        }
        if (!isTruthy(input$network_description)) {
          shinyjs::js$backgroundCol(ns("network_description"), "#fdd")
          return()
        }
        # Add the network to the database
        df <- data.frame(
          name = input$network_name,
          name_fr = if (isTruthy(input$network_name_fr)) {
            input$network_name_fr
          } else {
            NA
          },
          description = input$network_description,
          description_fr = if (isTruthy(input$network_description_fr)) {
            input$network_description_fr
          } else {
            NA
          },
          type = input$network_type
        )
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO networks (name, name_fr, description, description_fr, type) VALUES ($1, $2, $3, $4, $5)",
          params = list(
            df$name,
            ifelse(is.na(df$name_fr), NA, df$name_fr),
            df$description,
            ifelse(is.na(df$description_fr), NA, df$description_fr),
            df$type
          )
        )

        # Update the moduleData reactiveValues
        moduleData$networks <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT network_id, name FROM networks"
        )
        # Update the selectizeInput to the new value
        new_id <- moduleData$networks$network_id[
          moduleData$networks$name == df$name
        ]
        prior_selection <- ensure_character(pending_network_selection())
        new_value <- pending_network_new()
        retained <- prior_selection[prior_selection != new_value]
        retained <- retained[nzchar(retained)]
        selected_values <- unique(c(retained, ensure_character(new_id)))
        updateSelectizeInput(
          session,
          "network",
          choices = stats::setNames(
            moduleData$networks$network_id,
            moduleData$networks$name
          ),
          selected = selected_values
        )
        pending_network_selection(selected_values)
        pending_network_new(NULL)
        removeModal()
        showModal(modalDialog(
          "New network added.",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ### Observe the project selectizeInput for new projects #######################
    observeEvent(
      input$project,
      {
        vals <- ensure_character(input$project)
        pending_project_selection(vals)

        existing_ids <- ensure_character(moduleData$projects$project_id)
        new_vals <- setdiff(vals, existing_ids)
        new_vals <- new_vals[nzchar(new_vals)]

        if (!length(new_vals)) {
          pending_project_new(NULL)
          return()
        }

        new_val <- new_vals[length(new_vals)]
        pending_project_new(new_val)

        proj_types <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT id, name FROM network_project_types"
        )

        showModal(modalDialog(
          textInput(
            ns("project_name"),
            "Project name",
            value = if (nzchar(new_val)) new_val else input$loc_name
          ),
          textInput(ns("project_name_fr"), "Project name French (optional)"),
          textInput(ns("project_description"), "Project description"),
          textInput(
            ns("project_description_fr"),
            "Project description French (optional)"
          ),
          selectizeInput(
            ns("project_type"),
            "Project type",
            stats::setNames(proj_types$id, proj_types$name),
            multiple = FALSE
          ),
          actionButton(ns("add_project"), "Add project")
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$add_project,
      {
        # Check that mandatory fields are filled in
        if (!isTruthy(input$project_name)) {
          shinyjs::js$backgroundCol(ns("project_name"), "#fdd")
          return()
        }
        if (!isTruthy(input$project_description)) {
          shinyjs::js$backgroundCol(ns("project_description"), "#fdd")
          return()
        }
        # Add the project to the database
        df <- data.frame(
          name = input$project_name,
          name_fr = if (isTruthy(input$project_name_fr)) {
            input$project_name_fr
          } else {
            NA
          },
          description = input$project_description,
          description_fr = if (isTruthy(input$project_description_fr)) {
            input$project_description_fr
          } else {
            NA
          },
          type = input$project_type
        )
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO projects (name, name_fr, description, description_fr, type) VALUES ($1, $2, $3, $4, $5)",
          params = list(
            df$name,
            ifelse(is.na(df$name_fr), NA, df$name_fr),
            df$description,
            ifelse(is.na(df$description_fr), NA, df$description_fr),
            df$type
          )
        )

        # Update the moduleData reactiveValues
        moduleData$projects <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT project_id, name FROM projects"
        )
        # Update the selectizeInput to the new value
        new_id <- moduleData$projects$project_id[
          moduleData$projects$name == df$name
        ]
        prior_selection <- ensure_character(pending_project_selection())
        new_value <- pending_project_new()
        retained <- prior_selection[prior_selection != new_value]
        retained <- retained[nzchar(retained)]
        selected_values <- unique(c(retained, ensure_character(new_id)))
        updateSelectizeInput(
          session,
          "project",
          choices = stats::setNames(
            moduleData$projects$project_id,
            moduleData$projects$name
          ),
          selected = selected_values
        )
        pending_project_selection(selected_values)
        pending_project_new(NULL)
        removeModal()
        showModal(modalDialog(
          "New project added.",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ### Observe the owner selectizeInput for new owners ############
    observeEvent(
      input$loc_owner,
      {
        if (
          input$loc_owner %in%
            moduleData$organizations$organization_id ||
            nchar(input$loc_owner) == 0
        ) {
          return()
        }
        showModal(modalDialog(
          textInput(ns("owner_name"), "Owner name", value = input$loc_owner),
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
          "INSERT INTO organizations (name, name_fr, contact_name, phone, email, note) VALUES ($1, $2, $3, $4, $5, $6)",
          params = list(
            df$name,
            ifelse(is.na(df$name_fr), NA, df$name_fr),
            ifelse(is.na(df$contact_name), NA, df$contact_name),
            ifelse(is.na(df$phone), NA, df$phone),
            ifelse(is.na(df$email), NA, df$email),
            ifelse(is.na(df$note), NA, df$note)
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
          "loc_owner",
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

    ### Observe the share_with selectizeInput for new user groups ##############################
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

        if (
          input$share_with[length(input$share_with)] %in%
            moduleData$users$role_name ||
            nchar(input$share_with[length(input$share_with)]) == 0
        ) {
          return()
        }
        showModal(modalDialog(
          "Ask a database admin to create a new user or user group"
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ## Observe the add_location click #################
    # Run checks, if everything passes call AquaCache::addACLocation or update the location details
    just_updated <- reactiveVal(FALSE) # Prevents showing superfluous message of mode change after modification
    observeEvent(input$add_loc, {
      # Disable the button to prevent multiple clicks
      shinyjs::disable("add_loc")
      on.exit(shinyjs::enable("add_loc")) # Re-enable the button when the observer exits

      # Ensure lat + lon are truthy
      if (!isTruthy(input$lat) || !isTruthy(input$lon)) {
        showModal(modalDialog(
          "Latitude and longitude are mandatory",
          easyClose = TRUE
        ))
        return()
      }
      # Check that lat and lon are within bounds. For lat, -90 to 90. For lon, -180 to 180
      if (input$lat < -90 || input$lat > 90) {
        showModal(modalDialog(
          "Latitude must be between -90 and 90 degrees",
          easyClose = TRUE
        ))
        return()
      }
      if (input$lon < -180 || input$lon > 180) {
        showModal(modalDialog(
          "Longitude must be between -180 and 180 degrees",
          easyClose = TRUE
        ))
        return()
      }

      # Ensure that datum_id_from and datum_id_to are truthy
      if (!isTruthy(input$datum_id_from) || !isTruthy(input$datum_id_to)) {
        showModal(modalDialog(
          "Datum ID from and to are mandatory (use assumed datum for both if there is no conversion to apply)",
          easyClose = TRUE
        ))
        return()
      }

      # If datums are both the same make sure elevation is 0
      if (input$datum_id_from == input$datum_id_to && input$elev != 0) {
        showModal(modalDialog(
          "Elevation conversion must be 0 if the datums are the same",
          easyClose = TRUE
        ))
        return()
      }

      if (input$mode == "modify") {
        req(selected_loc())

        # Start a transaction
        DBI::dbBegin(session$userData$AquaCache)
        tryCatch(
          {
            # Check each field to see if it's been modified; if so, update the DB entry by targeting the location_id and appropriate column name
            # Changes to the location code
            if (
              input$loc_code !=
                moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "location"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE locations SET location = {input$loc_code} WHERE location_id = {selected_loc()};",
                  .con = session$userData$AquaCache
                )
              )
              # Update the corresponding entry in the 'vectors' table. the layer_name is 'Locations', should match on 'feature_name' = input$loc_code
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE vectors SET feature_name = {input$loc_code} WHERE layer_name = 'Locations' AND feature_name = {moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), 'location']};",
                  .con = session$userData$AquaCache
                )
              )
            }

            # Changes to the location english name
            if (
              input$loc_name !=
                moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "name"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE locations SET name = {input$loc_name} WHERE location_id = {selected_loc()};",
                  .con = session$userData$AquaCache
                )
              )
              # Update the corresponding entry in the 'vectors' table. the layer_name is 'Locations', should match on 'feature_name' = input$loc_code
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE vectors SET description = {input$loc_name} WHERE layer_name = 'Locations' AND feature_name = {input$loc_code};",
                  .con = session$userData$AquaCache
                )
              )
            }

            # Changes to the location french name
            if (
              input$loc_name_fr !=
                moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "name_fr"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE locations SET name_fr = {input$loc_name_fr} WHERE location_id = {selected_loc()};",
                  .con = session$userData$AquaCache
                )
              )
            }

            # Changes to the location type
            if (
              input$loc_type !=
                moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "location_type"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE locations SET location_type = {input$loc_type} WHERE location_id = {selected_loc()};",
                  .con = session$userData$AquaCache
                )
              )
            }

            # Changes to coordinates
            updated_coords <- FALSE
            if (
              input$lat !=
                moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "latitude"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE locations SET latitude = {input$lat} WHERE location_id = {selected_loc()};",
                  .con = session$userData$AquaCache
                )
              )
              updated_coords <- TRUE
            }
            if (
              input$lon !=
                moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "longitude"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE locations SET longitude = {input$lon} WHERE location_id = {selected_loc()};",
                  .con = session$userData$AquaCache
                )
              )
              updated_coords <- TRUE
            }
            if (updated_coords) {
              # Update the corresponding entry in the 'vectors' table. the layer_name is 'Locations', match it on 'feature_name' = input$loc_code. the 'geom' field (geometry data type) will be updated with the new coordinates
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE vectors SET geom = ST_SetSRID(ST_MakePoint({input$lon}, {input$lat}), 4269) WHERE layer_name = 'Locations' AND feature_name = {input$loc_code};",
                  .con = session$userData$AquaCache
                )
              )
            }

            # Changes to share_with
            if (
              !paste0("{", paste(input$share_with, collapse = ","), "}") ==
                moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "share_with"
                ]
            ) {
              share_with_sql <- DBI::SQL(paste0(
                "{",
                paste(input$share_with, collapse = ", "),
                "}"
              ))
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE locations SET share_with = {share_with_sql} WHERE location_id = {selected_loc()};",
                  .con = session$userData$AquaCache
                )
              )
            }

            # Changes to owner

            # TODO: this needs to touch the location_metadata_owner table, not the locations table
            # if (isTruthy(input$loc_owner)) {
            #   if (input$loc_owner != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "owner"]) {
            #     DBI::dbExecute(session$userData$AquaCache,
            #                    sprintf("UPDATE locations SET owner = %d WHERE location_id = %d", as.numeric(input$loc_owner), selected_loc()))
            #   }
            # }

            # Changes to contact
            if (isTruthy(input$loc_contact)) {
              if (
                !is.na(moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "contact"
                ])
              ) {
                # If the contact is not empty, update it
                if (
                  input$loc_contact !=
                    moduleData$exist_locs[
                      which(
                        moduleData$exist_locs$location_id == selected_loc()
                      ),
                      "contact"
                    ]
                ) {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    sprintf(
                      "UPDATE locations SET contact = '%s' WHERE location_id = %d",
                      input$loc_contact,
                      selected_loc()
                    )
                  )
                }
              } else {
                # If the contact is empty, insert it
                DBI::dbExecute(
                  session$userData$AquaCache,
                  sprintf(
                    "UPDATE locations SET contact = '%s' WHERE location_id = %d",
                    input$loc_contact,
                    selected_loc()
                  )
                )
              }
            }

            # Changes to data sharing agreement
            if (isTruthy(input$data_sharing_agreement)) {
              if (
                input$data_sharing_agreement !=
                  moduleData$exist_locs[
                    which(moduleData$exist_locs$location_id == selected_loc()),
                    "data_sharing_agreement_id"
                  ]
              ) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  sprintf(
                    "UPDATE locations SET data_sharing_agreement_id = %d WHERE location_id = %d",
                    as.numeric(input$data_sharing_agreement),
                    selected_loc()
                  )
                )
              }
            }

            # datum and elevation changes
            if (
              input$datum_id_from !=
                moduleData$datum_conversions[
                  which(
                    moduleData$datum_conversions$location_id == selected_loc()
                  ),
                  "datum_id_from"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                sprintf(
                  "UPDATE datum_conversions SET datum_id_from = %d WHERE location_id = %d",
                  as.numeric(input$datum_id_from),
                  selected_loc()
                )
              )
            }
            if (
              input$datum_id_to !=
                moduleData$datum_conversions[
                  which(
                    moduleData$datum_conversions$location_id == selected_loc()
                  ),
                  "datum_id_to"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                sprintf(
                  "UPDATE datum_conversions SET datum_id_to = %d WHERE location_id = %d",
                  as.numeric(input$datum_id_to),
                  selected_loc()
                )
              )
            }
            if (
              input$elev !=
                moduleData$datum_conversions[
                  which(
                    moduleData$datum_conversions$location_id == selected_loc()
                  ),
                  "conversion_m"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                sprintf(
                  "UPDATE datum_conversions SET conversion_m = %f WHERE location_id = %d",
                  input$elev,
                  selected_loc()
                )
              )
            }

            # Changes to network
            desired_networks <- parse_ids(input$network)
            existing_networks <- DBI::dbGetQuery(
              session$userData$AquaCache,
              sprintf(
                "SELECT network_id FROM locations_networks WHERE location_id = %d",
                selected_loc()
              )
            )$network_id
            existing_networks <- parse_ids(existing_networks)
            if (!identical(sort(existing_networks), sort(desired_networks))) {
              # NOTE: This will fail if the user doesn't have DELETE privileges on locations_networks. The main server checks DO NOT verify for this privilege to not otherwise block the rest of the module's functionality.
              DBI::dbExecute(
                session$userData$AquaCache,
                sprintf(
                  "DELETE FROM locations_networks WHERE location_id = %d",
                  selected_loc()
                )
              )
              if (length(desired_networks)) {
                for (i in seq_along(desired_networks)) {
                  net <- desired_networks[i]
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    "INSERT INTO locations_networks (network_id, location_id) VALUES ($1, $2)",
                    params = list(
                      net,
                      selected_loc()
                    )
                  )
                }
              }
            }

            # Changes to project
            desired_projects <- parse_ids(input$project)
            existing_projects <- DBI::dbGetQuery(
              session$userData$AquaCache,
              sprintf(
                "SELECT project_id FROM locations_projects WHERE location_id = %d",
                selected_loc()
              )
            )$project_id
            existing_projects <- parse_ids(existing_projects)
            if (!identical(sort(existing_projects), sort(desired_projects))) {
              # NOTE: This will fail if the user doesn't have DELETE privileges on locations_projects. The main server checks DO NOT verify for this privilege to not otherwise block the rest of the module's functionality.
              DBI::dbExecute(
                session$userData$AquaCache,
                sprintf(
                  "DELETE FROM locations_projects WHERE location_id = %d",
                  selected_loc()
                )
              )
              if (length(desired_projects)) {
                for (i in seq_along(desired_projects)) {
                  proj <- desired_projects[i]
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    "INSERT INTO locations_projects (project_id, location_id) VALUES ($1, $2)",
                    params = list(
                      proj,
                      selected_loc()
                    )
                  )
                }
              }
            }

            # Changes to jurisdictional relevance
            if (isTruthy(input$loc_jurisdictional_relevance)) {
              if (
                input$loc_jurisdictional_relevance !=
                  moduleData$exist_locs[
                    which(moduleData$exist_locs$location_id == selected_loc()),
                    "jurisdictional_relevance"
                  ]
              ) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  sprintf(
                    "UPDATE locations SET jurisdictional_relevance = %s WHERE location_id = %d",
                    input$loc_jurisdictional_relevance,
                    selected_loc()
                  )
                )
              }
            }

            # Changes to anthropogenic influence
            if (isTruthy(input$loc_anthropogenic_influence)) {
              if (
                input$loc_anthropogenic_influence !=
                  moduleData$exist_locs[
                    which(moduleData$exist_locs$location_id == selected_loc()),
                    "anthropogenic_influence"
                  ]
              ) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  sprintf(
                    "UPDATE locations SET anthropogenic_influence = %s WHERE location_id = %d",
                    input$loc_anthropogenic_influence,
                    selected_loc()
                  )
                )
              }
            }

            # Changes to install purpose
            if (isTruthy(input$loc_install_purpose)) {
              # If the current install_purpose is not NA, check if it has changed
              if (
                !is.na(moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "install_purpose"
                ])
              ) {
                if (
                  input$loc_install_purpose !=
                    moduleData$exist_locs[
                      which(
                        moduleData$exist_locs$location_id == selected_loc()
                      ),
                      "install_purpose"
                    ]
                ) {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    sprintf(
                      "UPDATE locations SET install_purpose = '%s' WHERE location_id = %d",
                      input$loc_install_purpose,
                      selected_loc()
                    )
                  )
                }
              } else {
                # If the install_purpose was NA, just set it
                DBI::dbExecute(
                  session$userData$AquaCache,
                  sprintf(
                    "UPDATE locations SET install_purpose = '%s' WHERE location_id = %d",
                    input$loc_install_purpose,
                    selected_loc()
                  )
                )
              }
            }

            # Changes to current purpose
            if (isTruthy(input$loc_current_purpose)) {
              # If the current purpose is not NA, check if it has changed
              if (
                !is.na(moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "current_purpose"
                ])
              ) {
                if (
                  input$loc_current_purpose !=
                    moduleData$exist_locs[
                      which(
                        moduleData$exist_locs$location_id == selected_loc()
                      ),
                      "current_purpose"
                    ]
                ) {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    sprintf(
                      "UPDATE locations SET current_purpose = '%s' WHERE location_id = %d",
                      input$loc_current_purpose,
                      selected_loc()
                    )
                  )
                }
              } else {
                # If the current purpose was NA, just set it
                DBI::dbExecute(
                  session$userData$AquaCache,
                  sprintf(
                    "UPDATE locations SET current_purpose = '%s' WHERE location_id = %d",
                    input$loc_current_purpose,
                    selected_loc()
                  )
                )
              }
            }

            # Changes to note
            if (isTruthy(input$loc_note)) {
              if (
                !is.na(moduleData$exist_locs[
                  which(moduleData$exist_locs$location_id == selected_loc()),
                  "note"
                ])
              ) {
                # There might not be a note already
                if (
                  input$loc_note !=
                    moduleData$exist_locs[
                      which(
                        moduleData$exist_locs$location_id == selected_loc()
                      ),
                      "note"
                    ]
                ) {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    sprintf(
                      "UPDATE locations SET note = '%s' WHERE location_id = %d",
                      input$loc_note,
                      selected_loc()
                    )
                  )
                }
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  sprintf(
                    "UPDATE locations SET note = '%s' WHERE location_id = %d",
                    input$loc_note,
                    selected_loc()
                  )
                )
              }
            }

            # Show a notification that the location was updated
            showNotification("Location updated successfully", type = "message")
            # Commit the transaction
            DBI::dbCommit(session$userData$AquaCache)

            # Update the moduleData reactiveValues
            just_updated(TRUE) # Prevents superfluous message of mode change and reset
            getModuleData() # This should trigger an update to the table
          },
          error = function(e) {
            # If there was an error, rollback the transaction
            DBI::dbRollback(session$userData$AquaCache)
            showModal(modalDialog(
              "Error updating location: ",
              e$message
            ))
          }
        )

        return()
      }

      # At this point we're not modifying, we're creating
      if (!isTruthy(input$loc_code)) {
        showModal(modalDialog(
          "Location code is mandatory",
          easyClose = TRUE
        ))
        return()
      } else {
        if (
          input$loc_code %in%
            moduleData$exist_locs[
              moduleData$exist_locs$location != input$loc_code,
              "location"
            ]
        ) {
          showModal(modalDialog(
            "Location code already exists",
            easyClose = TRUE
          ))
          return()
        }
      }
      if (!isTruthy(input$loc_name)) {
        showModal(modalDialog(
          "Location name is mandatory",
          easyClose = TRUE
        ))
        return()
      } else {
        if (input$loc_name %in% moduleData$exist_locs$name) {
          showModal(modalDialog(
            "Location name already exists",
            easyClose = TRUE
          ))
          return()
        }
      }
      if (!isTruthy(input$loc_name_fr)) {
        showModal(modalDialog(
          "Location name (French) is mandatory",
          easyClose = TRUE
        ))
        return()
      } else {
        if (input$loc_name_fr %in% moduleData$exist_locs$name_fr) {
          showModal(modalDialog(
            "Location name (French) already exists",
            easyClose = TRUE
          ))
          return()
        }
      }
      if (!isTruthy(input$loc_type)) {
        showModal(modalDialog(
          "Location type is mandatory",
          easyClose = TRUE
        ))
        return()
      }

      # If we are here, we are adding a new location
      network_ids <- parse_ids(input$network)
      project_ids <- parse_ids(input$project)
      # Make a data.frame to pass to addACLocation
      df <- data.frame(
        location = input$loc_code,
        name = input$loc_name,
        name_fr = input$loc_name_fr,
        latitude = input$lat,
        longitude = input$lon,
        share_with = input$share_with,
        owner = if (isTruthy(input$loc_owner)) {
          as.numeric(input$loc_owner)
        } else {
          NA
        },
        data_sharing_agreement_id = if (
          isTruthy(input$data_sharing_agreement)
        ) {
          as.numeric(input$data_sharing_agreement)
        } else {
          NA
        },
        location_type = as.numeric(input$loc_type),
        note = if (isTruthy(input$loc_note)) input$loc_note else NA,
        contact = if (isTruthy(input$loc_contact)) input$loc_contact else NA,
        datum_id_from = as.numeric(input$datum_id_from),
        datum_id_to = as.numeric(input$datum_id_to),
        conversion_m = input$elev,
        current = TRUE,
        network = if (length(network_ids)) {
          network_ids[1]
        } else {
          NA_integer_
        },
        project = if (length(project_ids)) {
          project_ids[1]
        } else {
          NA_integer_
        },
        jurisdictional_relevance = if (
          isTruthy(input$loc_jurisdictional_relevance)
        ) {
          input$loc_jurisdictional_relevance
        } else {
          NA
        },
        anthropogenic_influence = if (
          isTruthy(input$loc_anthropogenic_influence)
        ) {
          input$loc_anthropogenic_influence
        } else {
          NA
        },
        install_purpose = if (isTruthy(input$loc_install_purpose)) {
          input$loc_install_purpose
        } else {
          NA
        },
        current_purpose = if (isTruthy(input$loc_current_purpose)) {
          input$loc_current_purpose
        } else {
          NA
        }
      )

      tryCatch(
        {
          # addACLocation is all done within a transaction, including additions to accessory tables
          AquaCache::addACLocation(con = session$userData$AquaCache, df = df)

          # Show a modal to the user that the location was added
          showModal(modalDialog(
            "Location added successfully",
            easyClose = TRUE
          ))

          # Update the moduleData reactiveValues
          getModuleData() # This should trigger an update to the table

          # Reset all fields
          updateTextInput(session, "loc_code", value = character(0))
          updateTextInput(session, "loc_name", value = character(0))
          updateTextInput(session, "loc_name_fr", value = character(0))
          updateSelectizeInput(session, "loc_type", selected = character(0))
          updateNumericInput(session, "lat", value = NA)
          updateNumericInput(session, "lon", value = NA)
          updateSelectizeInput(session, "viz", selected = "exact")
          updateSelectizeInput(
            session,
            "share_with",
            selected = "public_reader"
          )
          updateSelectizeInput(session, "loc_owner", selected = character(0))
          updateTextInput(session, "loc_contact", value = character(0))
          updateSelectizeInput(
            session,
            "data_sharing_agreement",
            selected = character(0)
          )
          updateSelectizeInput(session, "datum_id_from", selected = 10)
          updateSelectizeInput(session, "datum_id_to", selected = 10)
          updateNumericInput(session, "elev", value = 0)
          updateSelectizeInput(session, "network", selected = character(0))
          updateSelectizeInput(session, "project", selected = character(0))
          updateTextInput(session, "loc_note", value = character(0))
          pending_network_selection(character(0))
          pending_network_new(NULL)
          pending_project_selection(character(0))
          pending_project_new(NULL)
        },
        error = function(e) {
          # Rollback the transaction if there was an error
          DBI::dbRollback(session$userData$AquaCache)
          showModal(modalDialog(
            "Error adding location: ",
            e$message
          ))
        }
      )
    })
  }) # End of moduleServer
}
