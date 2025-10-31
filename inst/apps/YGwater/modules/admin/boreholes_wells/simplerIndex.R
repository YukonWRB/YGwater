# UI and server code for simplerIndex

simplerIndexUI <- function(id) {
  ns <- NS(id)
  css_file <- system.file(
    "apps/YGwater/www/css/simplerIndex.css",
    package = "YGwater"
  )
  css <- gsub("%1$s", ns("pdf-container"), readLines(css_file), fixed = TRUE)

  tagList(
    tags$head(
      tags$style(HTML(paste(css, collapse = "\n"))),
      tags$script(src = "js/sidebar_resize.js")
    ),
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      div(
        id = ns("logo-container"),
        # Try to load the logo image with error handling
        tags$img(
          src = "imgs/simplerIndex.png",
          style = "height: 40px; width: 60px; object-fit: contain; border-radius: 6px; background: #fff;",
          srcset = "logo@2x.png 2x, logo@3x.png 3x",
          onerror = sprintf(
            "this.onerror=null; this.style.display='none'; document.getElementById('%s').style.display='flex';",
            ns("text-logo")
          )
        ),
        # Fallback text logo that appears if image fails to load
        div(
          id = ns("text-logo"),
          style = "width: 60px; height: 40px; background: linear-gradient(135deg, #007bff, #0056b3); border-radius: 6px; display: none; align-items: center; justify-content: center; color: white; font-weight: bold; font-size: 14px;",
          "YWRR"
        )
      ),
      "Simpler Index",
      hr()
    ),

    div(
      class = "sidebar-layout",
      div(
        class = "sidebar-panel",
        id = ns("sidebar"),
        div(class = "resize-handle", id = ns("resize-handle")),
        fileInput(
          ns("pdf_file"),
          "Upload PDF(s)",
          accept = ".pdf",
          multiple = TRUE
        ),
        numericInput(
          ns("num_boreholes"),
          "Number of boreholes",
          value = 1,
          min = 1
        ),
        # Navigation buttons
        fluidRow(
          column(
            12,
            actionButton(
              ns("prev_pdf"),
              icon("arrow-left"),
              class = "nav-btn",
              title = "Previous"
            ),
            actionButton(
              ns("next_pdf"),
              icon("arrow-right"),
              class = "nav-btn",
              title = "Next"
            ),
            actionButton(
              ns("remove_pdf"),
              icon("trash"),
              title = "Remove Selected",
              class = "nav-btn"
            )
          )
        ),
        br(),
        DT::DTOutput(ns("pdf_table"))
      ),
      div(
        class = "main-panel",
        # First row: select, redact, clear, save, zoom
        div(
          class = "control-row",
          div(
            class = "control-group",
            actionButton(
              ns("brush_select"),
              "Select",
              icon("mouse-pointer"),
              class = "btn-toggle"
            ) |>
              tooltip(
                "Enable the selection tool for OCR and content redaction."
              ),
            actionButton(
              ns("draw_rectangle"),
              "Redact",
              icon("rectangle-xmark"),
              class = "btn-toggle"
            ) |>
              tooltip(
                "Redact the selected area. Boxes are transparent for usability but will be made opaque on upload."
              ),
            actionButton(
              ns("clear_rectangles"),
              "Clear",
              icon("eraser"),
              class = "btn btn-outline-secondary",
              title = "Clear Rectangles"
            ),
            downloadButton(
              ns("save_image"),
              "Export PDF",
              class = "btn btn-outline-primary",
              title = "Export PDF with redactions and OCR text"
            ) |>
              tooltip(
                "Download a redacted copy for your records (does not send the PDF to the database)"
              ),
            # Zoom control - wrap in a container div
            div(
              class = "slider-container",
              sliderInput(
                ns("zoom_level"),
                "Zoom",
                min = 0.5,
                max = 4.0,
                value = 1.0,
                step = 0.1,
                width = "150px"
              )
            )
          )
        ),

        # Replace the Second row with simplified OCR controls
        accordion(
          id = ns("ocr-controls-accordion"),
          open = FALSE,
          accordion_panel(
            title = "OCR Controls",
            div(
              class = "control-row",
              style = "margin-top: 10px;",
              div(
                class = "control-group",
                selectizeInput(
                  ns("ocr_display_mode"),
                  "OCR Display Mode",
                  choices = list(
                    "None" = "none",
                    "Highlight Boxes" = "highlight",
                    "Text Overlay" = "text"
                  ),
                  selected = "none"
                ),
                div(
                  class = "slider-container",
                  sliderInput(
                    ns("confidence_threshold"),
                    "Confidence %",
                    min = 40,
                    max = 100,
                    value = 70,
                    step = 10,
                    width = "150px"
                  )
                ) |>
                  tooltip(
                    "Set the minimum confidence level for displaying OCR results. Higher values show only the most certain text."
                  ),
                selectizeInput(
                  ns("psm_mode"),
                  "PSM Mode",
                  choices = list(
                    "Auto" = "3",
                    "Auto + OSD" = "1",
                    "Sparse Text" = "11",
                    "Sparse Text + OSD" = "12"
                  ),
                  selected = "1"
                ) |>
                  tooltip(
                    "Page Segmentation Mode (PSM) controls how Tesseract splits the image into text blocks. 'Auto + OSD' is a good general choice for documents with mixed layouts."
                  ),
                selectizeInput(
                  ns("pre_processing_method"),
                  "Pre-processing",
                  choices = list(
                    "Default" = "default",
                    "Enhance Dark" = "enhance_dark",
                    "Enhance Light" = "enhance_light",
                    "High Contrast" = "high_contrast",
                    "Denoise" = "denoise",
                    "Deskew" = "deskew"
                  ),
                  selected = "default"
                ),

                # OCR Text Display
                div(
                  style = "margin-left: 20px; width: 300px;",
                  h6(
                    "Extracted Text",
                    style = "margin-bottom: 5px; color: #495057;"
                  ),
                  div(
                    style = "max-height: 120px; overflow-y: auto; border: 1px solid #ccc; padding: 8px; background: white; font-family: monospace; font-size: 11px; font-weight: bold; color: #007bff;",
                    verbatimTextOutput(ns("ocr_text_display"))
                  )
                )
              ),
            )
          )
        ),

        div(
          id = ns("pdf-container"),
          style = "width:100%; max-width:100%; height:calc(100vh - 200px); min-height:500px; border:1px solid #ccc; margin:10px auto; overflow-y: scroll; overflow-x: scroll; background:white; position:relative; display:block; padding:0;",

          plotOutput(
            ns("plot"),
            brush = brushOpts(
              id = ns("pdf_brush"),
              resetOnNew = TRUE,
              direction = "xy",
              opacity = 0.3,
              fill = "#007bff"
            ),
            height = "1000px"
          )
        )
      ),
      div(
        class = "right-panel",
        id = ns("right-sidebar"),
        div(class = "resize-handle-right", id = ns("resize-handle-right")),
        # Scrollable content area
        div(
          class = "scrollable-content",
          style = "overflow-y: auto; padding: 15px;",
          # Borehole linking controls in scrollable area
          fluidRow(
            column(
              12,
              selectizeInput(
                ns("borehole_details_selector"),
                "Select borehole to edit:",
                choices = NULL,
                selected = NULL,
                options = list(
                  placeholder = "Choose borehole",
                  maxItems = 1
                )
              ) |>
                tooltip(
                  "Choose which borehole's details to view and edit."
                )
            )
          ),
          br(),

          # Well identification
          textInput(
            ns("name"),
            "Borehole/well name *",
            placeholder = "Enter name"
          ),
          textInput(
            ns("notes_borehole"),
            "Boreholes notes",
            placeholder = "Enter borehole-specific notes"
          ),
          selectizeInput(
            ns("share_with_borehole"),
            "Share borehole with groups",
            choices = "public_reader", # Rest populated in server
            selected = "public_reader",
            multiple = TRUE,
            width = "100%"
          ) |>
            tooltip(
              "Select user groups to share this borehole with. 'public_reader' = shared with everyone."
            ),
          # Add 'drilled by' selectize input
          selectizeInput(
            ns("drilled_by"),
            "Driller *",
            choices = NULL, # Populated in server
            selected = NULL,
            multiple = TRUE,
            options = list(
              create = TRUE,
              placeholder = "Select driller",
              maxItems = 1
            )
          ) |>
            tooltip(
              "Add a new driller by typing the name in."
            ),

          # Location information section - remove surveyed_location_top_casing field
          radioButtons(
            ns("coordinate_system"),
            "Coordinate system *",
            choices = list("UTM" = "utm", "Lat/Lon" = "latlon"),
            selected = "utm",
            inline = TRUE
          ) |>
            tooltip(
              "UTM converted to Lat/Lon on upload."
            ),
          conditionalPanel(
            condition = "input.coordinate_system == 'utm'",
            ns = ns,
            numericInput(ns("easting"), "Easting *", value = NULL, min = 0),
            numericInput(ns("northing"), "Northing *", value = NULL, min = 0),
            selectizeInput(
              ns("utm_zone"),
              "UTM Zone*",
              choices = list(
                "7N" = "7N",
                "8N" = "8N",
                "9N" = "9N",
                "10N" = "10N",
                "11N" = "11N",
                "12N" = "12N",
                "13N" = "13N"
              ),
              selected = "8N",
              options = list(
                placeholder = "Select UTM zone",
                maxItems = 1
              )
            )
          ),
          conditionalPanel(
            condition = "input.coordinate_system == 'latlon'",
            ns = ns,
            numericInput(
              ns("latitude"),
              "Latitude *",
              value = NULL,
              min = 40,
              max = 85,
              step = 0.000001
            ),
            numericInput(
              ns("longitude"),
              "Longitude *",
              value = NULL,
              min = -141,
              max = -60,
              step = 0.000001
            )
          ),
          selectizeInput(
            ns("location_source"),
            "Location source *",
            choices = c(
              "GPS, uncorrected",
              "GPS, corrected",
              "Optical survey (benchmark)",
              "Map",
              "Satellite imagery",
              "Unknown"
            ),
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Select location source",
              maxItems = 1
            )
          ),
          selectizeInput(
            ns("purpose_of_borehole"),
            "Purpose of borehole",
            choices = NULL, # Populated in server
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Select purpose",
              maxItems = 1
            )
          ),
          radioButtons(
            ns("purpose_borehole_inferred"),
            "Purpose inferred or explicit?",
            choices = list("Inferred" = TRUE, "Explicit" = FALSE),
            selected = TRUE,
            inline = TRUE
          ),

          # Well construction details
          # Drill Depth and unit
          fluidRow(
            column(
              8,
              numericInput(
                ns("drill_depth"),
                "Drill depth *",
                value = NULL,
                min = 0,
                step = 0.1
              )
            ),
            column(
              4,
              radioButtons(
                ns("drill_depth_unit"),
                "",
                choices = list("m" = "m", "ft" = "ft"),
                selected = "ft",
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              8,
              numericInput(
                ns("surveyed_ground_elev"),
                "Surveyed ground elevation",
                value = NULL,
                step = 0.01
              ) |>
                tooltip(
                  "Elevation relative to sea level. Can be empty, but please make an effort and try to fill it in."
                )
            ),
            column(
              4,
              radioButtons(
                ns("surveyed_ground_elev_unit"),
                "",
                choices = list("m" = "m", "ft" = "ft"),
                selected = "ft",
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              8,
              numericInput(
                ns("depth_to_bedrock"),
                "Depth to bedrock",
                value = NULL,
                min = 0,
                step = 0.1
              ) |>
                tooltip(
                  "If bedrock was not reached or if unknown, leave empty."
                )
            ),
            column(
              4,
              radioButtons(
                ns("depth_to_bedrock_unit"),
                "",
                choices = list("m" = "m", "ft" = "ft"),
                selected = "ft",
                inline = TRUE
              )
            )
          ),

          # Add permafrost checkbox and conditional inputs
          checkboxInput(
            ns("permafrost_present"),
            "Permafrost present",
            value = FALSE
          ),

          ## IS PERMAFROST present conditional panel ##################
          conditionalPanel(
            condition = "input.permafrost_present == true",
            ns = ns,
            fluidRow(
              column(
                8,
                numericInput(
                  ns("permafrost_top"),
                  "Depth to top of permafrost",
                  value = NULL,
                  min = 0,
                  step = 0.1
                )
              ),
              column(
                4,
                radioButtons(
                  ns("permafrost_top_unit"),
                  "",
                  choices = list("m" = "m", "ft" = "ft"),
                  selected = "ft",
                  inline = TRUE
                )
              )
            ),
            fluidRow(
              column(
                8,
                numericInput(
                  ns("permafrost_bot"),
                  "Depth to bottom of permafrost",
                  value = NULL,
                  min = 0,
                  step = 0.1
                )
              ),
              column(
                4,
                radioButtons(
                  ns("permafrost_bot_unit"),
                  "",
                  choices = list("m" = "m", "ft" = "ft"),
                  selected = "ft",
                  inline = TRUE
                )
              )
            )
          ),

          dateInput(ns("date_drilled"), "Date drilled *", value = NULL),

          ## IS WELL conditional panel ##################
          checkboxInput(ns("is_well"), "Well constructed", value = FALSE),

          # Show well construction fields only if 'is_well' is checked
          conditionalPanel(
            condition = "input.is_well == true",
            ns = ns,
            selectizeInput(
              ns("share_with_well"),
              "Share well with groups",
              choices = "public_reader", # Rest populated in server
              selected = "public_reader",
              multiple = TRUE,
              width = "100%"
            ) |>
              tooltip(
                "Select user groups to share this well with. 'public_reader' = shared with everyone. Can be different from borehole sharing."
              ),
            # Casing Outside Diameter
            fluidRow(
              column(
                8,
                numericInput(
                  ns("casing_od"),
                  "Casing outside diameter",
                  value = NULL,
                  min = 0,
                  step = 1
                )
              ),
              column(
                4,
                radioButtons(
                  ns("casing_od_unit"),
                  "",
                  choices = list("cm" = "cm", "inch" = "inch"),
                  selected = "inch",
                  inline = TRUE
                )
              )
            ),
            # Top of Screen
            fluidRow(
              column(
                8,
                numericInput(
                  ns("top_of_screen"),
                  "Top of screen",
                  value = NULL,
                  min = 0,
                  step = 0.1
                )
              ),
              column(
                4,
                radioButtons(
                  ns("top_of_screen_unit"),
                  "",
                  choices = list("m" = "m", "ft" = "ft"),
                  selected = "ft",
                  inline = TRUE
                )
              )
            ),
            # Bottom of Screen
            fluidRow(
              column(
                8,
                numericInput(
                  ns("bottom_of_screen"),
                  "Bottom of screen",
                  value = NULL,
                  min = 0,
                  step = 0.1
                )
              ),
              column(
                4,
                radioButtons(
                  ns("bottom_of_screen_unit"),
                  "",
                  choices = list("m" = "m", "ft" = "ft"),
                  selected = "ft",
                  inline = TRUE
                )
              )
            ),
            # Well Head Stick Up
            fluidRow(
              column(
                8,
                numericInput(
                  ns("well_head_stick_up"),
                  "Well stick up",
                  value = NULL,
                  step = 0.01
                )
              ),
              column(
                4,
                radioButtons(
                  ns("well_head_stick_up_unit"),
                  "",
                  choices = list("m" = "m", "ft" = "ft"),
                  selected = "ft",
                  inline = TRUE
                )
              )
            ),
            # Static Water Level
            fluidRow(
              column(
                8,
                numericInput(
                  ns("static_water_level"),
                  "Static water level BTOC",
                  value = NULL,
                  step = 0.01
                ) |>
                  tooltip(
                    "Convert elevations BGS to BTOC!"
                  )
              ),
              column(
                4,
                radioButtons(
                  ns("static_water_level_unit"),
                  "",
                  choices = list("m" = "m", "ft" = "ft"),
                  selected = "ft",
                  inline = TRUE
                )
              )
            ),
            # Estimated Yield
            fluidRow(
              column(
                8,
                numericInput(
                  ns("estimated_yield"),
                  "Estimated yield",
                  value = NULL,
                  min = 0,
                  step = 0.1
                )
              ),
              column(
                4,
                radioButtons(
                  ns("estimated_yield_unit"),
                  "",
                  choices = list("L/s" = "L/s", "G/min" = "G/min"),
                  selected = "G/min",
                  inline = TRUE
                )
              )
            ),

            selectizeInput(
              ns("purpose_of_well"),
              "Purpose of well",
              choices = NULL, # Populated in server
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Enter if different from borehole purpose",
                maxItems = 1
              )
            ),
            textInput(
              ns("notes_well"),
              "Well notes",
              placeholder = "Screen type, filter pack, development, etc."
            ),
            radioButtons(
              ns("purpose_well_inferred"),
              "Purpose inferred or explicit?",
              choices = list("Inferred" = TRUE, "Explicit" = FALSE),
              selected = TRUE,
              inline = TRUE
            )
          ), # End of is_well conditional panel

          # Add upload buttons at the bottom of the scrollable content
          div(
            style = "margin-top: 30px; padding-top: 15px; border-top: 1px solid #dee2e6;",
            fluidRow(
              column(
                6,
                actionButton(
                  ns("upload_selected"),
                  "Upload selected",
                  class = "btn btn-primary btn-block",
                  icon = icon("upload")
                )
              ),
              column(
                6,
                actionButton(
                  ns("upload_all"),
                  "Upload all",
                  class = "btn btn-success btn-block",
                  icon = icon("cloud-upload-alt")
                )
              )
            )
          )
        )
      )
    ),

    # script to resize sidebars and reattach handlers after Shiny redraws UI
    tags$script(HTML(sprintf(
      "$(function(){ initSidebarResize({leftId:'%s', rightId:'%s', leftHandle:'%s', rightHandle:'%s', ids:[%s]}); });",
      ns('sidebar'),
      ns('right-sidebar'),
      ns('resize-handle'),
      ns('resize-handle-right'),
      paste(
        sprintf(
          "'%s'",
          ns(c(
            'name',
            'notes_borehole',
            'share_with_borehole',
            'easting',
            'northing',
            'latitude',
            'longitude',
            'location_source',
            'depth_to_bedrock',
            'permafrost_top',
            'permafrost_bot',
            'date_drilled',
            'casing_od',
            'drill_depth',
            'surveyed_ground_elev',
            'top_of_screen',
            'bottom_of_screen',
            'well_head_stick_up',
            'static_water_level',
            'estimated_yield',
            'notes_well',
            'share_with_well'
          ))
        ),
        collapse = ','
      )
    )))
  )
} # End of UI function

simplerIndex <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load the helper functions
    # local = TRUE ensures the functions are loaded into this module's environment only
    source(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/simplerIndex_helpers.R",
        package = "YGwater"
      ),
      local = TRUE
    )

    # Create reactiveValues to store data input by user or derived during session #################
    # reactiveValues to store data input by user or derived during session
    rv <- reactiveValues(
      files_df = NULL, # Data frame with one row per uploaded PDF page
      borehole_data = list(), # Named list organized by borehole ID
      pdf_index = 1, # Index of currently viewed PDF page
      ocr_text = list(),
      ocr_display_mode = "none",
      selected_text = NULL,
      rectangles = list(),
      assign_observers = list()
    )

    # Reactive expression to get the borehole selected for editing
    current_borehole_id <- reactive({
      selection <- input$borehole_details_selector
      if (is.null(selection) || length(selection) == 0) {
        return(NULL)
      }
      selection <- as.character(selection)[1]
      if (!nzchar(selection) || !selection %in% names(rv$borehole_data)) {
        return(NULL)
      }
      selection
    })
    # Reactive value to control brush mode
    brush_enabled <- reactiveVal(FALSE)
    # Flag to prevent circular updates when loading metadata
    loading_metadata <- reactiveVal(FALSE)

    # Module data loaded from Aquacache
    moduleData <- reactiveValues(
      drillers = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT driller_id, name FROM boreholes.drillers"
      ),
      purposes = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT borehole_well_purpose_id, purpose_name FROM boreholes.borehole_well_purposes"
      ),
      share_with_boreholes = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('boreholes.boreholes');"
      ), # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table
      share_with_wells = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('boreholes.wells');"
      )
    )

    # Update the 'drillers', 'purpose', and 'share_with' list based on the data loaded from Aquacache
    observeEvent(moduleData, {
      req(
        moduleData$drillers,
        moduleData$purposes,
        moduleData$share_with_boreholes,
        moduleData$share_with_wells
      )
      updateSelectizeInput(
        session,
        "drilled_by",
        choices = stats::setNames(
          moduleData$drillers$driller_id,
          moduleData$drillers$name
        ),
        selected = NULL
      )
      updateSelectizeInput(
        session,
        "purpose_of_borehole",
        choices = stats::setNames(
          moduleData$purposes$borehole_well_purpose_id,
          moduleData$purposes$purpose_name
        ),
        selected = NULL
      )
      updateSelectizeInput(
        session,
        "purpose_of_well",
        choices = stats::setNames(
          moduleData$purposes$borehole_well_purpose_id,
          moduleData$purposes$purpose_name
        ),
        selected = NULL
      )
      updateSelectizeInput(
        session,
        "share_with_borehole",
        choices = moduleData$share_with_boreholes$role_name,
        selected = "public_reader"
      )
      updateSelectizeInput(
        session,
        "share_with_well",
        choices = moduleData$share_with_wells$role_name,
        selected = "public_reader"
      )
    })

    sort_files_df <- function() {
      if (is.null(rv$files_df)) {
        return()
      }
      current_tag <- if (
        !is.null(rv$pdf_index) && nrow(rv$files_df) >= rv$pdf_index
      ) {
        rv$files_df$tag[rv$pdf_index]
      } else {
        NULL
      }
      assigned_flag <- ifelse(
        is.na(rv$files_df$borehole_id) | rv$files_df$borehole_id == "",
        1,
        0
      )
      rv$files_df <- rv$files_df[
        order(assigned_flag, rv$files_df$borehole_id, decreasing = TRUE),
      ]
      if (!is.null(current_tag)) {
        rv$pdf_index <- match(current_tag, rv$files_df$tag)
      }
    }

    well_fields <- c(
      "name",
      "notes_well",
      "notes_borehole",
      "share_with_well",
      "share_with_borehole",
      "coordinate_system",
      "easting",
      "northing",
      "utm_zone",
      "latitude",
      "longitude",
      "location_source",
      "purpose_of_borehole",
      "purpose_borehole_inferred",
      "depth_to_bedrock",
      "depth_to_bedrock_unit",
      "date_drilled",
      "casing_od",
      "casing_od_unit",
      "drill_depth",
      "drill_depth_unit",
      "top_of_screen",
      "top_of_screen_unit",
      "bottom_of_screen",
      "bottom_of_screen_unit",
      "well_head_stick_up",
      "well_head_stick_up_unit",
      "static_water_level",
      "static_water_level_unit",
      "estimated_yield",
      "estimated_yield_unit",
      "surveyed_ground_elev",
      "surveyed_ground_elev_unit",
      "permafrost_present",
      "permafrost_top",
      "permafrost_top_unit",
      "permafrost_bot",
      "permafrost_bot_unit",
      "is_well",
      "drilled_by",
      "purpose_of_well",
      "purpose_well_inferred"
    )

    empty_well_entry <- function() {
      metadata <- setNames(as.list(rep(NA, length(well_fields))), well_fields)
      list(files = character(), metadata = metadata)
    }

    sanitize_metadata_for_insert <- function(metadata) {
      if (is.null(metadata) || !is.list(metadata)) {
        metadata <- empty_well_entry()$metadata
      }

      null_if_empty <- function(x) {
        if (is.null(x) || length(x) == 0) {
          return(NULL)
        }
        if (all(is.na(x))) {
          return(NULL)
        }
        x
      }

      parse_numeric <- function(x) {
        x <- null_if_empty(x)
        if (is.null(x)) {
          return(NULL)
        }
        out <- suppressWarnings(as.numeric(x[1]))
        if (length(out) == 0 || is.na(out)) {
          return(NULL)
        }
        out
      }

      parse_character_scalar <- function(x, empty_to_null = TRUE) {
        x <- null_if_empty(x)
        if (is.null(x)) {
          return(if (empty_to_null) NULL else "")
        }
        val <- as.character(x[1])
        if (empty_to_null) {
          val <- trimws(val)
          if (!nzchar(val)) {
            return(NULL)
          }
        }
        val
      }

      parse_character_vector <- function(x, default = NULL) {
        x <- null_if_empty(x)
        if (is.null(x)) {
          if (is.null(default)) {
            return(character(0))
          }
          return(as.character(default))
        }
        out <- as.character(x)
        out <- trimws(out)
        out <- out[nzchar(out)]
        if (length(out) == 0) {
          if (is.null(default)) {
            return(character(0))
          }
          return(as.character(default))
        }
        out
      }

      parse_logical <- function(x, default = FALSE) {
        x <- null_if_empty(x)
        if (is.null(x)) {
          return(default)
        }
        if (is.logical(x)) {
          return(isTRUE(x))
        }
        if (is.numeric(x)) {
          return(!is.na(x[1]) && x[1] != 0)
        }
        if (is.character(x)) {
          val <- trimws(tolower(x[1]))
          if (!nzchar(val)) {
            return(default)
          }
          return(val %in% c("true", "t", "1", "yes", "y"))
        }
        default
      }

      parse_date <- function(x) {
        x <- null_if_empty(x)
        if (is.null(x)) {
          return(NULL)
        }
        if (inherits(x, "Date")) {
          return(x[1])
        }
        if (inherits(x, "POSIXt")) {
          return(as.Date(x[1]))
        }
        if (is.character(x)) {
          val <- trimws(x[1])
          if (!nzchar(val)) {
            return(NULL)
          }
          parsed <- try(as.Date(val), silent = TRUE)
          if (inherits(parsed, "try-error") || is.na(parsed)) {
            return(NULL)
          }
          return(parsed)
        }
        NULL
      }

      normalize_unit <- function(unit) {
        unit <- null_if_empty(unit)
        if (is.null(unit)) {
          return(NULL)
        }
        unit_val <- unit[1]
        if (inherits(unit_val, "factor")) {
          unit_val <- as.character(unit_val)
        }
        unit_val <- trimws(as.character(unit_val))
        if (!nzchar(unit_val)) {
          return(NULL)
        }
        unit_val
      }

      convert_utm_to_ll <- function(easting, northing, zone) {
        easting <- null_if_empty(easting)
        northing <- null_if_empty(northing)
        zone <- null_if_empty(zone)
        if (is.null(easting) || is.null(northing) || is.null(zone)) {
          return(list(latitude = NULL, longitude = NULL))
        }
        easting_num <- suppressWarnings(as.numeric(easting[1]))
        northing_num <- suppressWarnings(as.numeric(northing[1]))
        zone_val <- toupper(trimws(as.character(zone[1])))
        if (
          length(easting_num) == 0 ||
            is.na(easting_num) ||
            length(northing_num) == 0 ||
            is.na(northing_num) ||
            !grepl("^[0-9]{1,2}[C-HJ-NP-X]$", zone_val)
        ) {
          return(list(latitude = NULL, longitude = NULL))
        }

        # Only return the number part of the UTM zone (strip N/S)
        zone_val <- sub("([0-9]{1,2})([C-HJ-NP-X])", "\\1", zone_val)
        v <- data.frame(lon = easting_num, lat = northing_num) |>
          terra::vect(
            crs = paste0(
              "+proj=utm +zone=",
              zone_val,
              " +datum=WGS84 +units=m +no_defs"
            )
          ) |>
          terra::project("epsg:4326")
        lonlat <- terra::geom(v)[, c("x", "y")]
        return(list(latitude = lonlat$y, longitude = lonlat$x))
      }

      convert_length_to_m <- function(value, unit) {
        value <- null_if_empty(value)
        if (is.null(value)) {
          return(NULL)
        }
        unit_val <- normalize_unit(unit)
        if (is.null(unit_val)) {
          return(value)
        }
        unit_lower <- tolower(unit_val)
        if (unit_lower %in% c("ft", "foot", "feet")) {
          return(value * 0.3048)
        }
        if (
          unit_lower %in%
            c(
              "cm",
              "centimeter",
              "centimetre",
              "centimeters",
              "centimetres"
            )
        ) {
          return(value / 100)
        }
        if (
          unit_lower %in%
            c(
              "mm",
              "millimeter",
              "millimetre",
              "millimeters",
              "millimetres"
            )
        ) {
          return(value / 1000)
        }
        if (
          unit_lower %in%
            c(
              "km",
              "kilometer",
              "kilometre",
              "kilometers",
              "kilometres"
            )
        ) {
          return(value * 1000)
        }
        value
      }

      convert_length_to_mm <- function(value, unit) {
        value <- null_if_empty(value)
        if (is.null(value)) {
          return(NULL)
        }
        unit_val <- normalize_unit(unit)
        if (is.null(unit_val)) {
          return(value)
        }
        unit_lower <- tolower(unit_val)
        if (unit_lower %in% c("inch", "in", "\"")) {
          return(value * 25.4)
        }
        if (unit_lower %in% c("ft", "foot", "feet")) {
          return(value * 304.8)
        }
        if (
          unit_lower %in%
            c(
              "cm",
              "centimeter",
              "centimetre",
              "centimeters",
              "centimetres"
            )
        ) {
          return(value * 10)
        }
        if (
          unit_lower %in%
            c(
              "m",
              "meter",
              "metre",
              "meters",
              "metres"
            )
        ) {
          return(value * 1000)
        }
        value
      }

      convert_flow_to_lpm <- function(value, unit) {
        value <- null_if_empty(value)
        if (is.null(value)) {
          return(NULL)
        }
        unit_val <- normalize_unit(unit)
        if (is.null(unit_val)) {
          return(value)
        }
        unit_lower <- tolower(unit_val)
        if (unit_lower %in% c("l/s", "lps", "l per s", "l/sec")) {
          return(value * 60)
        }
        if (unit_lower %in% c("l/min", "lpm", "l per min", "l/minute")) {
          return(value)
        }
        if (
          unit_lower %in%
            c(
              "g/min",
              "gpm",
              "gal/min",
              "gallon/min",
              "gallons/min",
              "gallons per minute"
            )
        ) {
          return(value * 3.785411784)
        }
        if (unit_lower %in% c("g/s", "gal/s", "gallons per second")) {
          return(value * 3.785411784 * 60)
        }
        value
      }

      sanitized <- metadata
      sanitized$name <- parse_character_scalar(
        metadata$name,
        empty_to_null = TRUE
      )
      sanitized$notes_borehole <- parse_character_scalar(
        metadata$notes_borehole,
        empty_to_null = TRUE
      )
      sanitized$notes_well <- parse_character_scalar(
        metadata$notes_well,
        empty_to_null = TRUE
      )
      sanitized$location_source <- parse_character_scalar(
        metadata$location_source,
        empty_to_null = TRUE
      )

      sanitized$share_with_borehole <- parse_character_vector(
        metadata$share_with_borehole,
        default = "public_reader"
      )
      sanitized$share_with_well <- parse_character_vector(
        metadata$share_with_well,
        default = sanitized$share_with_borehole
      )
      if (length(sanitized$share_with_well) == 0) {
        sanitized$share_with_well <- sanitized$share_with_borehole
      }

      sanitized$drilled_by <- parse_numeric(metadata$drilled_by)
      sanitized$purpose_of_borehole <- parse_numeric(
        metadata$purpose_of_borehole
      )
      sanitized$purpose_of_well <- parse_numeric(metadata$purpose_of_well)

      sanitized$purpose_borehole_inferred <- parse_logical(
        metadata$purpose_borehole_inferred,
        default = TRUE
      )
      inferred_well <- metadata$purpose_well_inferred
      if (is.null(inferred_well)) {
        inferred_well <- metadata$purpose_of_well_inferred
      }
      sanitized$purpose_well_inferred <- parse_logical(
        inferred_well,
        default = sanitized$purpose_borehole_inferred
      )

      sanitized$permafrost_present <- parse_logical(
        metadata$permafrost_present,
        default = FALSE
      )
      sanitized$is_well <- parse_logical(metadata$is_well, default = FALSE)

      sanitized$date_drilled <- parse_date(metadata$date_drilled)

      numeric_fields <- c(
        "latitude",
        "longitude",
        "surveyed_ground_elev",
        "depth_to_bedrock",
        "permafrost_top",
        "permafrost_bot",
        "casing_od",
        "drill_depth",
        "top_of_screen",
        "bottom_of_screen",
        "well_head_stick_up",
        "static_water_level",
        "estimated_yield"
      )
      for (field in numeric_fields) {
        sanitized[[field]] <- parse_numeric(metadata[[field]])
      }

      sanitized$surveyed_ground_elev <- convert_length_to_m(
        sanitized[["surveyed_ground_elev"]],
        metadata$surveyed_ground_elev_unit
      )
      sanitized$depth_to_bedrock <- convert_length_to_m(
        sanitized[["depth_to_bedrock"]],
        metadata$depth_to_bedrock_unit
      )
      sanitized$permafrost_top <- convert_length_to_m(
        sanitized[["permafrost_top"]],
        metadata$permafrost_top_unit
      )
      sanitized$permafrost_bot <- convert_length_to_m(
        sanitized[["permafrost_bot"]],
        metadata$permafrost_bot_unit
      )
      sanitized$drill_depth <- convert_length_to_m(
        sanitized[["drill_depth"]],
        metadata$drill_depth_unit
      )
      sanitized$top_of_screen <- convert_length_to_m(
        sanitized[["top_of_screen"]],
        metadata$top_of_screen_unit
      )
      sanitized$bottom_of_screen <- convert_length_to_m(
        sanitized[["bottom_of_screen"]],
        metadata$bottom_of_screen_unit
      )
      sanitized$well_head_stick_up <- convert_length_to_m(
        sanitized[["well_head_stick_up"]],
        metadata$well_head_stick_up_unit
      )
      sanitized$static_water_level <- convert_length_to_m(
        sanitized[["static_water_level"]],
        metadata$static_water_level_unit
      )
      sanitized$casing_od <- convert_length_to_mm(
        sanitized[["casing_od"]],
        metadata$casing_od_unit
      )
      sanitized$estimated_yield <- convert_flow_to_lpm(
        sanitized[["estimated_yield"]],
        metadata$estimated_yield_unit
      )

      if (is.null(sanitized$latitude) || is.null(sanitized$longitude)) {
        if (identical(tolower(metadata$coordinate_system), "utm")) {
          sanitized$latitude <- NULL
          sanitized$longitude <- NULL
          if (
            !is.null(metadata$easting) &&
              !is.null(metadata$northing) &&
              !is.null(metadata$utm_zone)
          ) {
            latlon <- convert_utm_to_ll(
              metadata$easting,
              metadata$northing,
              metadata$utm_zone
            )
            sanitized$latitude <- latlon$latitude
            sanitized$longitude <- latlon$longitude
          }
        }
      }

      sanitized
    }

    validate_metadata_for_upload <- function(metadata) {
      if (is.null(metadata$name)) {
        showNotification(
          "Please provide a borehole or well name before uploading.",
          type = "error",
          duration = 5
        )
        return(FALSE)
      }

      if (is.null(metadata$latitude) || is.null(metadata$longitude)) {
        showNotification(
          "Latitude and longitude are required before uploading a borehole.",
          type = "error",
          duration = 5
        )
        return(FALSE)
      }

      # Check to ensure that the well name does not already exist in the database
      existing_names <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT borehole_name FROM boreholes.boreholes WHERE borehole_name = $1;",
        params = list(metadata$name)
      )$borehole_name

      if (length(existing_names) > 0) {
        showNotification(
          sprintf(
            "A borehole or well with the name '%s' already exists in the database. Please choose a different name.",
            metadata$name
          ),
          type = "error",
          duration = 10
        )
        return(FALSE)
      }

      # If we got to here, return TRUE
      return(TRUE)
    }

    update_borehole_details_selector <- function(preferred = NULL) {
      choices <- names(rv$borehole_data)
      labelled_choices <- if (length(choices) > 0) {
        stats::setNames(choices, paste("Borehole", choices))
      } else {
        NULL
      }

      selected <- preferred
      if (is.null(selected) || length(selected) == 0) {
        selected <- isolate(input$borehole_details_selector)
      }

      if (
        is.null(selected) || length(selected) == 0 || !selected %in% choices
      ) {
        if (length(choices) > 0) {
          selected <- choices[1]
        } else {
          selected <- NULL
        }
      }

      updateSelectizeInput(
        session,
        "borehole_details_selector",
        choices = labelled_choices,
        selected = selected,
        options = list(
          placeholder = "Choose borehole",
          maxItems = 1
        )
      )
    }

    all_pages_assigned <- function() {
      if (is.null(rv$files_df) || nrow(rv$files_df) == 0) {
        return(TRUE)
      }
      assigned <- rv$files_df$borehole_id
      if (length(assigned) == 0) {
        return(TRUE)
      }
      all(!is.na(assigned) & nzchar(assigned))
    }

    # Add observer for brush_select button
    observeEvent(input$brush_select, {
      # Toggle brush_enabled value
      brush_enabled(!brush_enabled())

      # Update button appearance based on new state
      if (brush_enabled()) {
        shinyjs::runjs(sprintf(
          "$('#%s').addClass('btn-active');",
          ns('brush_select')
        ))

        shinyjs::runjs(sprintf(
          "$('#%s').css('pointer-events', 'auto');",
          ns('plot')
        ))
      } else {
        shinyjs::runjs(sprintf(
          "$('#%s').removeClass('btn-active');",
          ns('brush_select')
        ))

        shinyjs::runjs(sprintf(
          "$('#%s').css('pointer-events', 'none');",
          ns('plot')
        ))
      }
    })

    # Make sure share_with is either public_reader or other groups, not both
    observeEvent(
      input$share_with_borehole,
      {
        if (
          length(input$share_with_borehole) > 1 &
            'public_reader' %in% input$share_with_borehole
        ) {
          showModal(modalDialog(
            "If public_reader is selected it must be the only group selected.",
            easyClose = TRUE
          ))
          updateSelectizeInput(
            session,
            "share_with_borehole",
            selected = "public_reader"
          )
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$share_with_well,
      {
        if (
          length(input$share_with_well) > 1 &
            'public_reader' %in% input$share_with_well
        ) {
          showModal(modalDialog(
            "If public_reader is selected it must be the only group selected.",
            easyClose = TRUE
          ))
          updateSelectizeInput(
            session,
            "share_with_well",
            selected = "public_reader"
          )
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$num_boreholes,
      {
        req(input$num_boreholes > 0)
        num <- input$num_boreholes
        new_ids <- as.character(seq_len(num))
        existing <- rv$borehole_data
        existing_files <- if (!is.null(rv$files_df)) {
          rv$files_df$NewFilename
        } else {
          character()
        }

        new_borehole_data <- lapply(new_ids, function(id) {
          if (!is.null(existing) && id %in% names(existing)) {
            entry <- existing[[id]]
            if (is.null(entry$files)) {
              entry$files <- character()
            }
            if (length(existing_files) > 0) {
              entry$files <- intersect(entry$files, existing_files)
            }
            if (is.null(entry$metadata)) {
              entry$metadata <- empty_well_entry()$metadata
            }
            entry$metadata$borehole_id <- id
            entry
          } else {
            entry <- empty_well_entry()
            entry$metadata$borehole_id <- id
            entry
          }
        })
        names(new_borehole_data) <- new_ids
        rv$borehole_data <- new_borehole_data

        if (!is.null(rv$files_df)) {
          rv$files_df$borehole_id <- ifelse(
            rv$files_df$borehole_id %in% new_ids,
            rv$files_df$borehole_id,
            NA_character_
          )
          rv$files_df$borehole_id <- as.character(rv$files_df$borehole_id)
          sort_files_df()
        }
        update_borehole_details_selector(isolate(
          input$borehole_details_selector
        ))
      },
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )

    # Observe change to share_with_borehole and update share_with_well to match if it doesn't already. Give user a notification that they can change it back if needed
    observeEvent(
      input$share_with_borehole,
      {
        req(input$share_with_borehole)
        # Reset it to 'public_reader' if length 0
        if (
          is.null(input$share_with_borehole) ||
            length(input$share_with_borehole) == 0
        ) {
          updateSelectizeInput(
            session,
            "share_with_borehole",
            selected = 'public_reader'
          )
          return()
        }
        # Only update well share with if it doesn't already match
        if (!setequal(input$share_with_borehole, input$share_with_well)) {
          updateSelectizeInput(
            session,
            "share_with_well",
            selected = input$share_with_borehole
          )
          showNotification(
            "Well sharing options updated to match borehole sharing. You can change well sharing separately if needed.",
            type = "message",
            duration = 10
          )
        }
      },
      ignoreInit = TRUE
    )

    # Enfore minimum 1 selection for well_share_with
    observeEvent(
      input$share_with_well,
      {
        req(input$share_with_well)
        if (
          is.null(input$share_with_well) || length(input$share_with_well) == 0
        ) {
          updateSelectizeInput(
            session,
            "share_with_well",
            selected = 'public_reader'
          )
        }
      },
      ignoreInit = TRUE
    )

    # Enforce drill_depth > depth_to_bedrock
    observeEvent(list(input$depth_to_bedrock, input$drill_depth), {
      req(input$depth_to_bedrock, input$drill_depth)
      if (input$depth_to_bedrock > input$drill_depth) {
        showNotification(
          "Depth to bedrock cannot be greater than drill depth",
          type = "error",
          duration = 5
        )
        updateNumericInput(session, "depth_to_bedrock", value = NA)
      }
    })

    # Observer for new driller creation
    observeEvent(input$drilled_by, {
      req(input$drilled_by)

      # If not in existing IDs or names, it's a new driller
      if (
        !(input$drilled_by %in% moduleData$drillers$driller_id) &&
          !(input$drilled_by %in% moduleData$drillers$name)
      ) {
        # Create modal dialog
        showModal(modalDialog(
          title = "New Driller Information",

          textInput(ns("new_driller_name"), "Name", value = input$drilled_by),
          textInput(ns("new_driller_address"), "Address"),
          textInput(ns("new_driller_phone"), "Phone"),
          textInput(ns("new_driller_email"), "Email"),

          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_new_driller"), "Save", class = "btn-primary")
          )
        ))
      }
    })

    # Handle the save button for new drillers in the modal
    observeEvent(input$save_new_driller, {
      # Generate a unique driller ID
      # Get values from the form
      new_driller_name <- input$new_driller_name
      new_driller_address <- input$new_driller_address
      new_driller_phone <- input$new_driller_phone
      new_driller_email <- input$new_driller_email

      # Validate phone number format
      if (!is.null(new_driller_phone) && trimws(new_driller_phone) != "") {
        # Remove any non-digit characters
        clean_phone <- gsub("[^0-9]", "", new_driller_phone)

        # Check if it's a valid phone number (10 digits, or 11 digits starting with 1)
        if (
          nchar(clean_phone) == 10 ||
            (nchar(clean_phone) == 11 && substr(clean_phone, 1, 1) == "1")
        ) {
          # Format the phone number for display: (XXX) XXX-XXXX
          if (nchar(clean_phone) == 11) {
            clean_phone <- substr(clean_phone, 2, 11) # Remove the leading 1
          }
          new_driller_phone <- paste0(
            "(",
            substr(clean_phone, 1, 3),
            ") ",
            substr(clean_phone, 4, 6),
            "-",
            substr(clean_phone, 7, 10)
          )
        } else {
          showNotification(
            "Invalid phone number format. Please use a 10-digit number.",
            type = "error",
            duration = 5
          )
          return() # Exit the function early
        }
      }

      # Validate email format if provided
      if (!is.null(new_driller_email) && trimws(new_driller_email) != "") {
        # Basic email format validation
        email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
        if (!grepl(email_pattern, new_driller_email)) {
          showNotification(
            "Invalid email format. Please enter a valid email address.",
            type = "error",
            duration = 5
          )
          return() # Exit the function early
        }
      }

      new_driller_id <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "INSERT INTO boreholes.drillers (name,address,phone,email)
   VALUES ($1,$2,$3,$4) RETURNING driller_id",
        params = list(
          new_driller_name,
          if (nzchar(trimws(new_driller_address))) {
            new_driller_address
          } else {
            NULL
          },
          if (nzchar(trimws(new_driller_phone))) new_driller_phone else NULL,
          if (nzchar(trimws(new_driller_email))) new_driller_email else NULL
        )
      )[1, 1]

      moduleData$drillers <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT driller_id, name FROM boreholes.drillers"
      )
      updateSelectizeInput(
        session,
        "drilled_by",
        choices = stats::setNames(
          moduleData$drillers$driller_id,
          moduleData$drillers$name
        ),
        selected = new_driller_id
      )
      removeModal()
    })

    # Observer for new purpose creation
    observeEvent(input$purpose_of_borehole, {
      req(input$purpose_of_borehole)

      # If not in existing IDs or names, it's a new driller
      if (
        !(input$purpose_of_borehole %in%
          moduleData$purposes$borehole_well_purpose_id) &&
          !(input$purpose_of_borehole %in% moduleData$purposes$purpose_name)
      ) {
        # Create modal dialog
        showModal(modalDialog(
          title = "New Borehole/Well purpose",
          textInput(
            ns("new_purpose_name"),
            "Purpose name",
            value = input$purpose_of_borehole
          ),
          textInput(ns("new_purpose_description"), "Description"),

          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_new_purpose"), "Save", class = "btn-primary")
          )
        ))
      }
    })

    # Observer for new purpose creation
    observeEvent(input$purpose_of_well, {
      req(input$purpose_of_well)

      # If not in existing IDs or names, it's a new driller
      if (
        !(input$purpose_of_well %in%
          moduleData$purposes$borehole_well_purpose_id) &&
          !(input$purpose_of_well %in% moduleData$purposes$purpose_name)
      ) {
        # Create modal dialog
        showModal(modalDialog(
          title = "New Borehole/Well purpose",
          textInput(
            ns("new_purpose_name"),
            "Purpose name",
            value = input$purpose_of_well
          ),
          textInput(ns("new_purpose_description"), "Description"),

          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_new_purpose"), "Save", class = "btn-primary")
          )
        ))
      }
    })

    # Handle the save button for new drillers in the modal
    observeEvent(input$save_new_purpose, {
      # Ensure that name and description but have at least 3 characters
      if (nchar(trimws(input$new_purpose_name)) < 3) {
        showNotification(
          "Purpose name must be at least 3 characters long.",
          type = "error",
          duration = 5
        )
        return() # Exit the function early
      }
      if (nchar(trimws(input$new_purpose_description)) < 3) {
        showNotification(
          "Purpose description must be at least 3 characters long.",
          type = "error",
          duration = 5
        )
        return() # Exit the function early
      }

      new_purpose_id <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "INSERT INTO boreholes.borehole_well_purposes (purpose_name, description)
   VALUES ($1,$2) RETURNING borehole_well_purpose_id",
        params = list(input$new_purpose_name, input$new_purpose_description)
      )[1, 1]

      moduleData$purposes <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT borehole_well_purpose_id, purpose_name FROM boreholes.borehole_well_purposes"
      )

      updateSelectizeInput(
        session,
        "purpose_of_borehole",
        choices = stats::setNames(
          moduleData$purposes$borehole_well_purpose_id,
          moduleData$purposes$purpose_name
        ),
        selected = new_purpose_id
      )
      removeModal()
    })

    # Split PDFs into single-page files on upload
    observeEvent(input$pdf_file, {
      uploaded_files <- input$pdf_file
      # Rename uploaded files to their original names with robust path handling
      for (i in seq_len(nrow(uploaded_files))) {
        orig_name <- uploaded_files$name[i]
        from_path <- normalizePath(
          uploaded_files$datapath[i],
          winslash = "/",
          mustWork = FALSE
        )
        orig_path <- file.path(dirname(from_path), orig_name)
        rename_success <- file.rename(from_path, orig_path)
        if (!rename_success) {
          file.copy(from_path, orig_path, overwrite = TRUE)
          unlink(from_path)
        }
        uploaded_files$datapath[i] <- orig_path
      }

      # Show initial loading notification
      total_files <- nrow(uploaded_files)

      for (i in seq_len(nrow(uploaded_files))) {
        pdf_path <- uploaded_files$datapath[i][1]
        orig_name <- uploaded_files$name[i]

        # Show progress for current file
        showNotification(
          paste("Converting", orig_name, "- File", i, "of", total_files),
          type = "message",
          duration = 4
        )

        # Convert PDF to PNG files (one per page) and save to tempdir
        n_pages <- pdftools::pdf_info(pdf_path)$pages
        base <- tools::file_path_sans_ext(basename(pdf_path))
        png_tpl <- file.path(tempdir(), sprintf("%s_page_%%d.%%s", base)) # note %%d and %%s

        png_files <- pdftools::pdf_convert(
          pdf_path,
          dpi = 300,
          pages = seq_len(n_pages),
          format = "png",
          filenames = png_tpl
        )

        file_info <- file.info(png_files)
        split_df <- data.frame(
          Name = rep(orig_name, length(png_files)),
          Size_KB = round(file_info$size / 1024, 2),
          Date = as.character(file.info(pdf_path)$mtime),
          OrigFile = rep(orig_name, length(png_files)),
          Page = seq_along(png_files),
          Path = png_files,
          stringsAsFactors = FALSE
        )
        split_df$NewFilename <- file.path(basename(split_df$Path))
        split_df$tag <- paste0(split_df$Name, "-", split_df$Page)
        split_df$borehole_id <- NA

        if (i == 1) {
          all_split_files <- split_df
        } else {
          all_split_files <- rbind(all_split_files, split_df)
        }

        # Show completion for current file
        showNotification(
          paste(
            "Completed converting",
            orig_name,
            "- Generated",
            length(png_files),
            "page(s)"
          ),
          type = "message",
          duration = 5
        )
      }

      if (is.null(rv$files_df)) {
        rv$files_df <- all_split_files
      } else {
        rv$files_df <- rbind(rv$files_df, all_split_files)
      }
      rv$files_df$borehole_id <- as.character(rv$files_df$borehole_id)
      sort_files_df()

      rv$pdf_index <- 1

      if (length(rv$ocr_text) == 0) {
        rv$ocr_text <- vector("list", nrow(rv$files_df))
      } else {
        rv$ocr_text <- c(rv$ocr_text, vector("list", nrow(all_split_files)))
      }
      rv$ocr_display_mode <- "none"

      # Reset button states on upload
      brush_enabled(FALSE)
      updateSelectizeInput(session, "ocr_display_mode", selected = "none")

      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('btn-active');",
        ns('brush_select')
      ))

      # Show final completion notification
      total_pages <- nrow(rv$files_df)
      showNotification(
        paste(
          "PDF conversion completed! Generated",
          total_pages,
          "page(s) total."
        ),
        type = "message",
        duration = 4
      )
    })

    # Observe table row selection and update pdf_index
    observeEvent(
      input$pdf_table_rows_selected,
      {
        sel <- input$pdf_table_rows_selected
        if (!is.null(sel) && !identical(sel, rv$pdf_index)) {
          rv$pdf_index <- sel
        }
      },
      ignoreInit = TRUE
    )

    # Observe forward/back buttons and update table selection
    observeEvent(
      input$next_pdf,
      {
        req(rv$files_df)
        if (rv$pdf_index < nrow(rv$files_df)) {
          rv$pdf_index <- rv$pdf_index + 1
          # Ensure table selection follows
          DT::dataTableProxy("pdf_table", session = session) |>
            DT::selectRows(rv$pdf_index)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$prev_pdf,
      {
        req(rv$files_df)
        if (rv$pdf_index >= 2) {
          rv$pdf_index <- rv$pdf_index - 1
          # Ensure table selection follows
          DT::dataTableProxy("pdf_table", session = session) |>
            DT::selectRows(rv$pdf_index)
        } else {
          rv$pdf_index <- 1
        }
      },
      ignoreInit = TRUE
    )

    # Observe remove button and delete selected page, updating pdf_index and table selection as needed
    observeEvent(
      input$remove_pdf,
      {
        req(rv$files_df)
        if (nrow(rv$files_df) > 0) {
          selected_row <- rv$pdf_index

          fname <- rv$files_df$NewFilename[selected_row]

          # Remove from files_df and OCR text
          rv$files_df <- rv$files_df[-selected_row, ]
          rv$ocr_text <- rv$ocr_text[-selected_row]

          # Update well_data structure by removing the filename
          for (well_id in names(rv$borehole_data)) {
            rv$borehole_data[[well_id]]$files <- setdiff(
              rv$borehole_data[[well_id]]$files,
              fname
            )
          }

          if (nrow(rv$files_df) == 0) {
            rv$pdf_index <- 1
          } else if (rv$pdf_index > nrow(rv$files_df)) {
            rv$pdf_index <- nrow(rv$files_df)
            # Ensure table selection follows
            DT::dataTableProxy("pdf_table", session = session) |>
              DT::selectRows(rv$pdf_index)
          }
          sort_files_df()
        }
      },
      ignoreInit = TRUE
    )

    # Observe changes to files_df and set up observers for each select input, which are created dynamically on table render
    observeEvent(
      rv$files_df,
      {
        if (is.null(rv$files_df) || nrow(rv$files_df) == 0) {
          # No files, destroy all observers and exit
          lapply(
            rv$assign_observers,
            function(obs) {
              if (!is.null(obs)) {
                obs$destroy()
              }
            }
          )
          rv$assign_observers <- list()
          return()
        }
        # Destroy existing observers first
        lapply(
          rv$assign_observers,
          function(obs) {
            if (!is.null(obs)) {
              obs$destroy()
            }
          }
        )
        rv$assign_observers <- vector("list", length = nrow(rv$files_df))
        # Set up new observers for each row
        for (i in seq_len(nrow(rv$files_df))) {
          rv$assign_observers[[i]] <- local({
            row_index <- i
            observeEvent(
              input[[paste0("bh_select_", row_index)]],
              {
                if (row_index > nrow(rv$files_df)) {
                  return()
                }
                new_id <- input[[paste0("bh_select_", row_index)]]
                if (is.null(new_id)) {
                  new_id <- ""
                } else {
                  new_id <- as.character(new_id)
                }
                prev_id <- rv$files_df$borehole_id[row_index]
                prev_id_normalized <- ifelse(is.na(prev_id), "", prev_id)
                if (identical(prev_id_normalized, new_id)) {
                  return()
                }
                fname <- rv$files_df$NewFilename[row_index]
                if (
                  !is.na(prev_id) &&
                    nzchar(prev_id) &&
                    prev_id %in% names(rv$borehole_data)
                ) {
                  rv$borehole_data[[prev_id]]$files <- setdiff(
                    rv$borehole_data[[prev_id]]$files,
                    fname
                  )
                }
                if (nzchar(new_id) && new_id %in% names(rv$borehole_data)) {
                  rv$borehole_data[[new_id]]$files <- unique(c(
                    rv$borehole_data[[new_id]]$files,
                    fname
                  ))
                }
                rv$files_df$borehole_id[row_index] <- if (nzchar(new_id)) {
                  new_id
                } else {
                  NA_character_
                }
              },
              ignoreNULL = TRUE,
              ignoreInit = TRUE
            )
          })
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Render the data table of files
    output$pdf_table <- DT::renderDT({
      req(rv$files_df)
      validate(need(nrow(rv$files_df) > 0, "No files uploaded yet"))

      borehole_choices <- names(rv$borehole_data) # Fetch current borehole IDs
      # Generate selectInput for each row to assign pages to boreholes
      select_inputs <- vapply(
        seq_len(nrow(rv$files_df)),
        function(i) {
          selected_value <- rv$files_df$borehole_id[i]
          if (length(selected_value) == 0 || is.na(selected_value)) {
            selected_value <- ""
          }
          labelled_choices <- if (length(borehole_choices) > 0) {
            stats::setNames(
              borehole_choices,
              paste("Borehole", borehole_choices)
            )
          } else {
            NULL
          }
          as.character(selectizeInput(
            ns(paste0("bh_select_", i)),
            NULL,
            choices = c("Unassigned" = "", labelled_choices),
            selected = selected_value,
            width = "120px"
          ))
        },
        character(1)
      )

      dat <- data.frame(
        tag = rv$files_df$tag,
        borehole = select_inputs,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        dat,
        selection = list(mode = "single", selected = rv$pdf_index),
        escape = FALSE,
        options = list(
          pageLength = 10,
          layout = list(
            bottomStart = 'info',
            bottomEnd = 'paging'
          ),
          ordering = FALSE,
          scrollY = "300px",
          scrollCollapse = TRUE,
          preDrawCallback = DT::JS(
            'function() { Shiny.unbindAll(this.api().table().node()); }'
          ),
          drawCallback = DT::JS(
            'function() { Shiny.bindAll(this.api().table().node()); } '
          )
        )
      )
    })

    # Modified observer for OCR display mode: process OCR for all images when mode is highlight/text
    observeEvent(
      list(input$psm_mode, input$pre_processing_method, input$ocr_display_mode),
      {
        req(rv$files_df)
        rv$ocr_display_mode <- input$ocr_display_mode

        if (rv$ocr_display_mode %in% c("highlight", "text")) {
          # Set processing flag
          rv$ocr_text <- process_ocr_batch(
            rv$files_df,
            rv$ocr_text,
            as.integer(input$psm_mode),
            input$pre_processing_method
          )
        }
      }
    )

    # Render the plot, making sure not to re-render the same plot twice
    rendered_plot <- reactiveVal(NULL)
    output$plot <- renderPlot(
      expr = {
        req(rv$files_df)
        req(rv$pdf_index)
        req(nrow(rv$files_df) >= rv$pdf_index)
        zoom <- input$zoom_level
        # Load and prepare the image
        img_path <- rv$files_df$Path[rv$pdf_index]
        req(file.exists(img_path))

        img <- magick::image_read(img_path) |>
          magick::image_enhance()
        info <- magick::image_info(img)
        img_width <- info$width
        img_height <- info$height
        img_raster <- as.raster(img)

        # Set up the plot area
        par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
        plot(
          0,
          0,
          type = "n",
          xlim = c(0, img_width),
          ylim = c(0, img_height),
          xlab = "",
          ylab = "",
          axes = FALSE,
          asp = 1
        )

        # Draw the image
        rasterImage(img_raster, 0, 0, img_width, img_height)

        # Draw OCR overlay if in OCR mode and OCR data exists
        if (
          input$ocr_display_mode != "none" &&
            !is.null(rv$ocr_text[[rv$pdf_index]])
        ) {
          ocr_df <- rv$ocr_text[[rv$pdf_index]]

          # Filter by confidence threshold
          if (nrow(ocr_df) > 0) {
            ocr_df <- ocr_df[
              ocr_df$confidence >= input$confidence_threshold,
              ,
              drop = FALSE
            ]
          }

          # Draw OCR boxes or text
          if (nrow(ocr_df) > 0) {
            for (i in seq_len(nrow(ocr_df))) {
              tryCatch(
                {
                  # Parse bbox coordinates and convert to plot coordinates
                  bbox <- strsplit(ocr_df$bbox[i], ",")[[1]]
                  if (length(bbox) == 4) {
                    coords <- as.numeric(bbox)

                    # Handle coordinate conversion correctly
                    # Tesseract coordinates: (left, top, right, bottom) with origin at top-left
                    # Plot coordinates: (left, bottom, right, top) with origin at bottom-left
                    x1 <- coords[1] # left
                    y1 <- img_height - coords[4] # bottom (inverted)
                    x2 <- coords[3] # right
                    y2 <- img_height - coords[2] # top (inverted)

                    # Draw rectangle and/or text based on display mode
                    if (input$ocr_display_mode == "text") {
                      # Draw background for text
                      rect(
                        x1,
                        y1,
                        x2,
                        y2,
                        col = rgb(1, 1, 1, 0.7), # semi-transparent white
                        border = "darkgray",
                        lwd = 1
                      )

                      # Draw word on top
                      text_x <- (x1 + x2) / 2
                      text_y <- (y1 + y2) / 2
                      text(
                        text_x,
                        text_y,
                        ocr_df$word[i],
                        cex = 0.9,
                        col = "black",
                        font = 2
                      )
                    } else if (input$ocr_display_mode == "highlight") {
                      # Draw highlight rectangle
                      rect(
                        x1,
                        y1,
                        x2,
                        y2,
                        col = rgb(0, 0.48, 1, 0.3), # Semi-transparent blue
                        border = rgb(0, 0.48, 1, 0.8), # Solid blue border
                        lwd = 1
                      )
                    }
                  }
                },
                error = function(e) {
                  # Silently ignore errors in drawing individual words
                }
              )
            }
          } else {
            # Show message if no text meets confidence threshold
            text_width <- strwidth("No OCR text meets confidence threshold") *
              1.2
            rect(
              img_width / 2.2 - text_width / 2.2,
              img_height / 2.2 - 15,
              img_width / 2.2 + text_width / 2.2,
              img_height / 2.2 + 15,
              col = "white",
              border = "black"
            )

            text(
              img_width / 2.2,
              img_height / 2.2,
              paste(
                "No OCR text meets confidence threshold (",
                input$confidence_threshold,
                "%)"
              ),
              cex = 1,
              col = "red"
            )
          }
        }

        # Draw user-defined redaction rectangles
        if (
          !is.null(rv$rectangles[[img_path]]) &&
            length(rv$rectangles[[img_path]]) > 0
        ) {
          for (rect_data in rv$rectangles[[img_path]]) {
            rect(
              rect_data$xmin,
              rect_data$ymin,
              rect_data$xmax,
              rect_data$ymax,
              col = adjustcolor(rect_data$color, alpha.f = 0.3),
              border = rect_data$color,
              lwd = 2
            )
          }
        }
      },
      width = function() {
        req(rv$files_df)
        req(rv$pdf_index)
        img_path <- rv$files_df$Path[rv$pdf_index]
        if (is.null(img_path) || is.na(img_path) || !file.exists(img_path)) {
          return(400)
        }
        info <- magick::image_info(magick::image_read(img_path))
        info$width * input$zoom_level / 2.2
      },
      height = function() {
        req(rv$files_df)
        req(rv$pdf_index)
        img_path <- rv$files_df$Path[rv$pdf_index]
        if (is.null(img_path) || is.na(img_path) || !file.exists(img_path)) {
          return(400)
        }
        info <- magick::image_info(magick::image_read(img_path))
        info$height * input$zoom_level / 2.2
      },
      res = 96
    ) # Increased resolution for better text rendering

    # Observer for brush selection to extract text
    observeEvent(input$pdf_brush, {
      req(input$pdf_brush)
      req(rv$files_df)
      req(rv$pdf_index)
      req(brush_enabled())

      # Get current OCR data
      ocr_df <- rv$ocr_text[[rv$pdf_index]]
      if (is.null(ocr_df) || nrow(ocr_df) == 0) {
        rv$selected_text <- NULL
        return()
      }

      # Filter by confidence threshold to match what's displayed
      # This is the key fix - apply the same confidence filter used for display
      if (nrow(ocr_df) > 0) {
        ocr_df <- ocr_df[
          ocr_df$confidence >= input$confidence_threshold,
          ,
          drop = FALSE
        ]
      }

      if (nrow(ocr_df) == 0) {
        showNotification(
          "No OCR text meets confidence threshold",
          type = "warning",
          duration = 2
        )
        rv$selected_text <- NULL
        return()
      }

      # Get brush coordinates
      brush <- input$pdf_brush

      # Get image dimensions for coordinate conversion
      img_path <- rv$files_df$Path[rv$pdf_index]
      img <- magick::image_read(img_path)
      info <- magick::image_info(img)
      img_width <- info$width
      img_height <- info$height

      # Convert brush coordinates to image coordinates
      # Note: brush coordinates are in plot space, need to convert to image space
      brush_xmin <- brush$xmin
      brush_xmax <- brush$xmax
      brush_ymin <- img_height - brush$ymax # Flip Y coordinates
      brush_ymax <- img_height - brush$ymin # Flip Y coordinates

      # Find OCR words within brush selection
      selected_words <- character(0)

      for (i in seq_len(nrow(ocr_df))) {
        # Parse bbox coordinates
        coords <- as.numeric(strsplit(ocr_df$bbox[i], ",")[[1]])
        word_x1 <- coords[1]
        word_y1 <- coords[2]
        word_x2 <- coords[3]
        word_y2 <- coords[4]

        # Check if word overlaps with brush selection
        if (
          word_x2 >= brush_xmin &&
            word_x1 <= brush_xmax &&
            word_y2 >= brush_ymin &&
            word_y1 <= brush_ymax
        ) {
          selected_words <- c(selected_words, ocr_df$word[i])
        }
      }

      # Update selected text
      if (length(selected_words) > 0) {
        rv$selected_text <- selected_words

        # Create notification with the actual text (limit to reasonable length)
        selected_text <- paste(selected_words, collapse = " ")
        if (nchar(selected_text) > 100) {
          selected_text <- paste0(substr(selected_text, 1, 97), "...")
        }
        showNotification(
          paste("Selected:", selected_text),
          type = "message",
          duration = 6
        )
      } else {
        rv$selected_text <- NULL
        showNotification(
          "No text found in selection",
          type = "warning",
          duration = 2
        )
      }
    })

    # --- Rectangle logic: modified to store by file path instead of borehole ID ---
    observeEvent(input$draw_rectangle, {
      # Make sure we have a brush selection
      if (is.null(input$pdf_brush)) {
        showNotification(
          "Please make a selection first",
          type = "warning",
          duration = 2
        )
        return()
      }
      req(rv$files_df)
      req(rv$pdf_index)

      # Get file path as unique identifier
      file_path <- rv$files_df$Path[rv$pdf_index]

      # Get brush coordinates (already in plot coordinates)
      brush <- input$pdf_brush

      # Store rectangle data for this file path
      if (is.null(rv$rectangles[[file_path]])) {
        rv$rectangles[[file_path]] <- list()
      }

      new_rect <- list(
        xmin = brush$xmin,
        xmax = brush$xmax,
        ymin = brush$ymin,
        ymax = brush$ymax,
        color = "red"
      )
      rv$rectangles[[file_path]] <- append(
        rv$rectangles[[file_path]],
        list(new_rect)
      )
      showNotification("Selection redacted", type = "message", duration = 2)
    })

    observeEvent(input$clear_rectangles, {
      req(rv$files_df)
      req(rv$pdf_index)

      # Get file path as unique identifier
      file_path <- rv$files_df$Path[rv$pdf_index]

      # Clear rectangles for this file path only
      rv$rectangles[[file_path]] <- NULL
      showNotification("Rectangles cleared", type = "message", duration = 2)
    })

    # --- Modified save image handler to use the generalized function ---
    output$save_image <- downloadHandler(
      filename = function() {
        req(rv$files_df, rv$pdf_index)
        borehole_id <- rv$files_df$borehole_id[rv$pdf_index]
        create_pdf_with_redactions(borehole_id, return_path = FALSE)
      },
      content = function(file) {
        req(rv$files_df, rv$pdf_index)
        borehole_id <- rv$files_df$borehole_id[rv$pdf_index]
        showNotification(
          "Creating PDF with redactions...",
          type = "message",
          duration = 2
        )

        temp_file_path <- create_pdf_with_redactions(
          borehole_id,
          return_path = TRUE
        )
        if (!is.null(temp_file_path) && file.exists(temp_file_path)) {
          file.copy(temp_file_path, file)

          # Count OCR words for notification
          same_bh_rows <- which(rv$files_df$borehole_id == borehole_id)
          ocr_word_count <- 0
          for (i in same_bh_rows) {
            if (!is.null(rv$ocr_text[[i]]) && nrow(rv$ocr_text[[i]]) > 0) {
              ocr_word_count <- ocr_word_count + nrow(rv$ocr_text[[i]])
            }
          }

          if (ocr_word_count > 0) {
            showNotification(
              paste(
                "PDF created with redactions. OCR text contains",
                ocr_word_count,
                "words."
              ),
              type = "message",
              duration = 3
            )
          } else {
            showNotification(
              "PDF created with redactions (no OCR text available)",
              type = "message",
              duration = 3
            )
          }
        }
      }
    )

    # Observer to update input fields with selected OCR text when clicked
    observe({
      # First check if we have any selected text
      if (is.null(rv$selected_text) || length(rv$selected_text) == 0) {
        return() # Exit early if no text is selected
      }

      # Combine selected text into a single string
      selected_text <- paste(rv$selected_text, collapse = " ")

      # Get all inputs with "_clicked" suffix - force to character to prevent NA
      all_inputs <- as.character(names(reactiveValuesToList(input)))
      clicked_inputs <- all_inputs[grepl("_clicked$", all_inputs)]

      if (length(clicked_inputs) == 0) {
        return() # Exit if no click events are registered
      }

      # Safely get the values for clicked inputs
      clicked_values <- sapply(
        clicked_inputs,
        function(name) {
          val <- input[[name]]
          if (
            !is.null(val) && length(val) == 1 && !is.na(val) && is.numeric(val)
          ) {
            val
          } else {
            0
          }
        },
        USE.NAMES = TRUE
      )

      # Find max value - only proceed if it's greater than 0
      max_value <- max(clicked_values, na.rm = TRUE)
      if (!is.na(max_value) && max_value > 0) {
        # Find which input was most recently clicked (has the max value)
        max_index <- which(clicked_values == max_value)
        if (length(max_index) > 0) {
          # Take first if multiple
          most_recent <- clicked_inputs[max_index[1]]

          # Extract field name from clicked input name
          field_name <- sub("_clicked$", "", most_recent)

          # Function to blur the input field after updating
          blur_field <- function(field_id) {
            shinyjs::runjs(sprintf(
              "document.getElementById('%s').blur();",
              ns(field_id)
            ))
          }

          # Update different field types appropriately
          if (field_name %in% c("name", "notes_borehole", "notes_well")) {
            updateTextInput(session, field_name, value = selected_text)
            shinyjs::runjs(sprintf(
              "var el=$('#%s'); if(el.length){el.addClass('flash-update'); setTimeout(function(){el.removeClass('flash-update');},1400);}",
              ns(field_name)
            ))

            # Blur the field
            blur_field(field_name)
          } else if (
            field_name %in%
              c(
                "easting",
                "northing",
                "latitude",
                "longitude",
                "depth_to_bedrock",
                "permafrost_top",
                "permafrost_bot",
                "casing_od",
                "drill_depth",
                "top_of_screen",
                "bottom_of_screen",
                "well_head_stick_up",
                "static_water_level",
                "estimated_yield"
              )
          ) {
            # Numeric inputs - extract numbers
            tryCatch(
              {
                # Try to extract a number from the text
                num_pattern <- regexpr("\\d+\\.?\\d*", selected_text)
                if (!is.na(num_pattern) && num_pattern > 0) {
                  num_text <- regmatches(selected_text, num_pattern)
                  if (length(num_text) > 0) {
                    num_value <- as.numeric(num_text[1])
                    if (!is.na(num_value)) {
                      updateNumericInput(session, field_name, value = num_value)
                      shinyjs::runjs(sprintf(
                        "var el=$('#%s'); if(el.length){el.addClass('flash-update'); setTimeout(function(){el.removeClass('flash-update');},1400);}",
                        ns(field_name)
                      ))
                      showNotification(
                        paste("Updated", field_name, "with value", num_value),
                        type = "message",
                        duration = 2
                      )
                      # Blur the field
                      blur_field(field_name)
                    }
                  }
                } else {
                  showNotification(
                    "No numeric value found in selected text",
                    type = "warning",
                    duration = 2
                  )
                }
              },
              error = function(e) {
                showNotification(
                  paste0("Error extracting numeric value: ", e$message),
                  type = "error",
                  duration = 4
                )
              }
            )
          } else if (field_name == "date_drilled") {
            # Try to extract and parse date
            tryCatch(
              {
                # Try multiple date patterns
                date_patterns <- c(
                  "\\d{1,4}[-/]\\d{1,2}[-/]\\d{1,4}", # yyyy-mm-dd format
                  "\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}", # dd Month yyyy
                  "[A-Za-z]+\\s+\\d{1,2},?\\s+\\d{4}" # Month dd, yyyy
                )

                for (pattern in date_patterns) {
                  date_match <- regexpr(pattern, selected_text)
                  if (!is.na(date_match) && date_match > 0) {
                    date_str <- regmatches(selected_text, date_match)
                    if (length(date_str) > 0) {
                      parsed_date <- as.Date(date_str[1], format = "%Y-%m-%d")
                      if (is.na(parsed_date)) {
                        # Try other common formats
                        parsed_date <- as.Date(date_str[1], format = "%m/%d/%Y")
                      }
                      if (is.na(parsed_date)) {
                        parsed_date <- as.Date(date_str[1], format = "%d %B %Y")
                      }
                      if (is.na(parsed_date)) {
                        parsed_date <- as.Date(
                          date_str[1],
                          format = "%B %d, %Y"
                        )
                      }

                      if (!is.na(parsed_date)) {
                        updateDateInput(
                          session,
                          "date_drilled",
                          value = parsed_date
                        )
                        shinyjs::runjs(sprintf(
                          "var el=$('#%s'); if(el.length){el.addClass('flash-update'); setTimeout(function(){el.removeClass('flash-update');},1400);}",
                          ns('date_drilled')
                        ))
                        showNotification(
                          paste(
                            "Updated date to",
                            format(parsed_date, "%Y-%m-%d")
                          ),
                          type = "message",
                          duration = 2
                        )
                        # Date fields have complex structure, blur the input part
                        shinyjs::runjs(sprintf(
                          "document.querySelector('#%s input').blur();",
                          ns('date_drilled')
                        ))
                        break # Exit the loop once we've found a valid date
                      }
                    }
                  }
                }
              },
              error = function(e) {
                showNotification(
                  paste0("Error parsing date ", e$message),
                  type = "error",
                  duration = 5
                )
              }
            )
          } else {
            # If we reach here, we didn't handle the field type
            cat("Unhandled field type:", field_name, "\n")
          }

          # Clear selected text after using it
          rv$selected_text <- NULL

          # For selectize inputs which need special handling (if any exist)
          if (
            field_name %in%
              c(
                "drilled_by",
                "utm_zone",
                "purpose_of_borehole",
                "purpose_of_well"
              )
          ) {
            shinyjs::runjs(sprintf(
              "$('#%s-selectized').blur();",
              ns(field_name)
            ))
          }

          # Clear any brush selection
          if (brush_enabled()) {
            # This will remove the visual brush selection
            shinyjs::runjs(sprintf(
              "Shiny.setInputValue('%s', Math.random());",
              ns('pdf_brush-clear')
            ))
          }
        }
      }
    })

    # Comprehensive observer to store all input values in metadata for the current well
    observe({
      # Don't update metadata when we're loading
      if (loading_metadata()) {
        return()
      }

      well_id <- current_borehole_id()

      if (is.null(well_id) || !well_id %in% names(rv$borehole_data)) {
        return()
      }
      # Update metadata with current input values for the correct well
      rv$borehole_data[[well_id]]$metadata <- list(
        borehole_id = well_id,
        name = input$name,
        notes_borehole = input$notes_borehole,
        notes_well = input$notes_well,
        coordinate_system = input$coordinate_system,
        easting = input$easting,
        northing = input$northing,
        utm_zone = input$utm_zone,
        latitude = input$latitude,
        longitude = input$longitude,
        location_source = input$location_source,
        purpose_of_borehole = input$purpose_of_borehole,
        purpose_borehole_inferred = input$purpose_borehole_inferred,
        depth_to_bedrock = input$depth_to_bedrock,
        depth_to_bedrock_unit = input$depth_to_bedrock_unit,
        permafrost_present = input$permafrost_present,
        permafrost_top = input$permafrost_top,
        permafrost_top_unit = input$permafrost_top_unit,
        permafrost_bot = input$permafrost_bot,
        permafrost_bot_unit = input$permafrost_bot_unit,
        date_drilled = input$date_drilled,
        casing_od = input$casing_od,
        casing_od_unit = input$casing_od_unit,
        drill_depth = input$drill_depth,
        drill_depth_unit = input$drill_depth_unit,
        top_of_screen = input$top_of_screen,
        top_of_screen_unit = input$top_of_screen_unit,
        bottom_of_screen = input$bottom_of_screen,
        bottom_of_screen_unit = input$bottom_of_screen_unit,
        well_head_stick_up = input$well_head_stick_up,
        well_head_stick_up_unit = input$well_head_stick_up_unit,
        static_water_level = input$static_water_level,
        static_water_level_unit = input$static_water_level_unit,
        estimated_yield = input$estimated_yield,
        estimated_yield_unit = input$estimated_yield_unit,
        surveyed_ground_elev = input$surveyed_ground_elev,
        surveyed_ground_elev_unit = input$surveyed_ground_elev_unit,
        is_well = input$is_well,
        purpose_of_well = input$purpose_of_well,
        purpose_well_inferred = input$purpose_well_inferred,
        drilled_by = input$drilled_by,
        share_with_borehole = input$share_with_borehole,
        share_with_well = input$share_with_well
      )
    })

    # Metadata loader. Update input fields when a new borehole is selected
    observeEvent(
      input$borehole_details_selector,
      {
        well_id <- current_borehole_id()

        if (!is.null(well_id) && well_id %in% names(rv$borehole_data)) {
          loading_metadata(TRUE)
          metadata <- rv$borehole_data[[well_id]]$metadata

          # Update text inputs - make sure notes is included
          updateTextInput(
            session,
            "name",
            value = get_meta_value("name", metadata = metadata)
          )
          updateTextInput(
            session,
            "notes_borehole",
            value = get_meta_value("notes_borehole", metadata = metadata)
          )
          updateTextInput(
            session,
            "notes_well",
            value = get_meta_value("notes_well", metadata = metadata)
          )
          updateSelectizeInput(
            session,
            "location_source",
            selected = get_meta_value("location_source", metadata = metadata)
          )

          # Update selectize inputs
          updateSelectizeInput(
            session,
            "utm_zone",
            selected = get_meta_value(
              "utm_zone",
              metadata = metadata,
              default = "8N"
            )
          )
          updateSelectizeInput(
            session,
            "purpose_of_borehole",
            selected = get_meta_value(
              "purpose_of_borehole",
              metadata = metadata
            )
          )
          updateSelectizeInput(
            session,
            "purpose_of_well",
            selected = get_meta_value("purpose_of_well", metadata = metadata)
          )
          updateSelectizeInput(
            session,
            "drilled_by",
            selected = get_meta_value("drilled_by", metadata = metadata)
          )
          updateSelectizeInput(
            session,
            "share_with_borehole",
            selected = get_meta_value_multiple(
              "share_with_borehole",
              metadata = metadata
            )
          )
          updateSelectizeInput(
            session,
            "share_with_well",
            selected = get_meta_value_multiple(
              "share_with_well",
              metadata = metadata
            )
          )

          # Update radio buttons
          updateRadioButtons(
            session,
            "coordinate_system",
            selected = get_meta_value(
              "coordinate_system",
              metadata = metadata,
              default = "utm"
            )
          )
          updateRadioButtons(
            session,
            "depth_to_bedrock_unit",
            selected = get_meta_value(
              "depth_to_bedrock_unit",
              metadata = metadata,
              default = "ft"
            )
          )
          updateRadioButtons(
            session,
            "casing_od_unit",
            selected = get_meta_value(
              "casing_od_unit",
              metadata = metadata,
              default = "inch"
            )
          )
          updateRadioButtons(
            session,
            "drill_depth_unit",
            selected = get_meta_value(
              "drill_depth_unit",
              metadata = metadata,
              default = "ft"
            )
          )
          updateRadioButtons(
            session,
            "top_of_screen_unit",
            selected = get_meta_value(
              "top_of_screen_unit",
              metadata = metadata,
              default = "ft"
            )
          )
          updateRadioButtons(
            session,
            "bottom_of_screen_unit",
            selected = get_meta_value(
              "bottom_of_screen_unit",
              metadata = metadata,
              default = "ft"
            )
          )
          updateRadioButtons(
            session,
            "well_head_stick_up_unit",
            selected = get_meta_value(
              "well_head_stick_up_unit",
              metadata = metadata,
              default = "ft"
            )
          )
          updateRadioButtons(
            session,
            "static_water_level_unit",
            selected = get_meta_value(
              "static_water_level_unit",
              metadata = metadata,
              default = "ft"
            )
          )
          updateRadioButtons(
            session,
            "estimated_yield_unit",
            selected = get_meta_value(
              "estimated_yield_unit",
              metadata = metadata,
              default = "G/min"
            )
          )
          updateRadioButtons(
            session,
            "surveyed_ground_elev_unit",
            selected = get_meta_value(
              "surveyed_ground_elev_unit",
              metadata = metadata,
              default = "ft"
            )
          )
          updateRadioButtons(
            session,
            "permafrost_top_unit",
            selected = get_meta_value(
              "permafrost_top_unit",
              metadata = metadata,
              default = "ft"
            )
          )
          updateRadioButtons(
            session,
            "permafrost_bot_unit",
            selected = get_meta_value(
              "permafrost_bot_unit",
              metadata = metadata,
              default = "ft"
            )
          )
          updateRadioButtons(
            session,
            "purpose_borehole_inferred",
            selected = get_meta_boolean(
              "purpose_borehole_inferred",
              metadata = metadata,
              default = TRUE
            )
          )
          updateRadioButtons(
            session,
            "purpose_well_inferred",
            selected = get_meta_boolean(
              "purpose_well_inferred",
              metadata = metadata,
              default = TRUE
            )
          )

          # Update numeric inputs
          updateNumericInput(
            session,
            "easting",
            value = get_meta_numeric("easting", metadata = metadata)
          )
          updateNumericInput(
            session,
            "northing",
            value = get_meta_numeric("northing", metadata = metadata)
          )
          updateNumericInput(
            session,
            "latitude",
            value = get_meta_numeric("latitude", metadata = metadata)
          )
          updateNumericInput(
            session,
            "longitude",
            value = get_meta_numeric("longitude", metadata = metadata)
          )
          updateNumericInput(
            session,
            "depth_to_bedrock",
            value = get_meta_numeric("depth_to_bedrock", metadata = metadata)
          )
          updateNumericInput(
            session,
            "permafrost_top",
            value = get_meta_numeric("permafrost_top", metadata = metadata)
          )
          updateNumericInput(
            session,
            "permafrost_bot",
            value = get_meta_numeric("permafrost_bot", metadata = metadata)
          )
          updateNumericInput(
            session,
            "casing_od",
            value = get_meta_numeric("casing_od", metadata = metadata)
          )
          updateNumericInput(
            session,
            "drill_depth",
            value = get_meta_numeric("drill_depth", metadata = metadata)
          )
          updateNumericInput(
            session,
            "surveyed_ground_elev",
            value = get_meta_numeric(
              "surveyed_ground_elev",
              metadata = metadata
            )
          )
          updateNumericInput(
            session,
            "top_of_screen",
            value = get_meta_numeric("top_of_screen", metadata = metadata)
          )
          updateNumericInput(
            session,
            "bottom_of_screen",
            value = get_meta_numeric("bottom_of_screen", metadata = metadata)
          )
          updateNumericInput(
            session,
            "well_head_stick_up",
            value = get_meta_numeric("well_head_stick_up", metadata = metadata)
          )
          updateNumericInput(
            session,
            "static_water_level",
            value = get_meta_numeric("static_water_level", metadata = metadata)
          )
          updateNumericInput(
            session,
            "estimated_yield",
            value = get_meta_numeric("estimated_yield", metadata = metadata)
          )

          # Update checkbox inputs
          updateCheckboxInput(
            session,
            "permafrost_present",
            value = get_meta_boolean("permafrost_present", metadata = metadata)
          )
          updateCheckboxInput(
            session,
            "is_well",
            value = get_meta_boolean(
              "is_well",
              metadata = metadata,
              default = FALSE
            )
          )

          # Update date input
          updateDateInput(
            session,
            "date_drilled",
            value = get_meta_date("date_drilled", metadata = metadata)
          )

          # Re-enable metadata saving after all updates are complete
          loading_metadata(FALSE)
        } else {
          # If no metadata exists, clear all fields including notes
          loading_metadata(TRUE)

          updateTextInput(session, "name", value = "")
          updateTextInput(session, "notes_borehole", value = "")
          updateTextInput(session, "notes_well", value = "")
          updateSelectizeInput(session, "location_source", selected = "GPS")
          updateSelectizeInput(session, "utm_zone", selected = "8N")
          updateSelectizeInput(session, "purpose_of_borehole", selected = NULL)
          updateRadioButtons(
            session,
            "purpose_borehole_inferred",
            selected = TRUE
          )
          updateSelectizeInput(session, "purpose_of_well", selected = NULL)
          updateRadioButtons(session, "purpose_well_inferred", selected = TRUE)
          updateSelectizeInput(session, "drilled_by", selected = NULL)
          updateSelectizeInput(
            session,
            "share_with_borehole",
            selected = "public_reader"
          )
          updateSelectizeInput(
            session,
            "share_with_well",
            selected = "public_reader"
          )
          updateRadioButtons(session, "coordinate_system", selected = "utm")
          updateRadioButtons(session, "depth_to_bedrock_unit", selected = "ft")
          updateRadioButtons(session, "casing_od_unit", selected = "inch")
          updateRadioButtons(session, "drill_depth_unit", selected = "ft")
          updateRadioButtons(
            session,
            "estimated_yield_unit",
            selected = "G/min"
          )
          updateRadioButtons(
            session,
            "surveyed_ground_elev_unit",
            selected = "ft"
          )

          # Clear all numeric inputs
          for (field in c(
            "easting",
            "northing",
            "latitude",
            "longitude",
            "depth_to_bedrock",
            "permafrost_top",
            "permafrost_bot",
            "casing_od",
            "drill_depth",
            "surveyed_ground_elev",
            "top_of_screen",
            "bottom_of_screen",
            "well_head_stick_up",
            "static_water_level",
            "estimated_yield"
          )) {
            updateNumericInput(session, field, value = NULL)
          }

          # Clear checkboxes and date
          updateCheckboxInput(session, "permafrost_present", value = FALSE)
          updateCheckboxInput(session, "is_well", value = FALSE)
          updateDateInput(session, "date_drilled", value = NULL)

          loading_metadata(FALSE)
        }
      },
      ignoreNULL = FALSE
    )

    # Upload handlers
    observeEvent(input$upload_selected, {
      req(rv$files_df)
      req(rv$pdf_index)

      if (nrow(rv$files_df) == 0) {
        showNotification(
          "No PDF pages available to upload.",
          type = "error",
          duration = 5
        )
        return()
      }

      if (!all_pages_assigned()) {
        showNotification(
          "Please assign every document page to a borehole before uploading.",
          type = "error",
          duration = 5
        )
        return()
      }

      # Get the current well ID
      current_borehole_id <- current_borehole_id()

      if (is.null(current_borehole_id)) {
        showNotification(
          "Assign a borehole to upload before proceeding",
          type = "error",
          duration = 5
        )
        return()
      }

      if (current_borehole_id %in% names(rv$borehole_data)) {
        metadata <- rv$borehole_data[[current_borehole_id]]$metadata
        if (is.null(metadata)) {
          metadata <- empty_well_entry()$metadata
        }

        metadata <- sanitize_metadata_for_insert(metadata)
        if (!validate_metadata_for_upload(metadata)) {
          return()
        }

        out <<- metadata

        # Show processing notification
        showNotification(
          "Uploading selected borehole...",
          type = "message",
          duration = 3
        )

        tryCatch(
          {
            AquaCache::dbTransBegin(session$userData$AquaCache)

            # Create PDF with redactions for this borehole
            pdf_file_path <- create_pdf_with_redactions(
              current_borehole_id,
              return_path = TRUE
            )

            # Call AquaCache function with the metadata
            result <- AquaCache::insertACBorehole(
              con = session$userData$AquaCache,
              path = pdf_file_path,
              well_name = metadata[["name"]],
              latitude = metadata[["latitude"]],
              longitude = metadata[["longitude"]],
              location_source = metadata[["location_source"]],
              surveyed_ground_elev = metadata[[
                "surveyed_ground_elev"
              ]],
              purpose_of_borehole = metadata[["purpose_of_borehole"]],
              purpose_borehole_inferred = metadata[[
                "purpose_borehole_inferred"
              ]],
              depth_to_bedrock = metadata[["depth_to_bedrock"]],
              permafrost_present = metadata[["permafrost_present"]],
              permafrost_top = metadata[["permafrost_top"]],
              permafrost_bot = metadata[["permafrost_bot"]],
              date_drilled = metadata[["date_drilled"]],
              casing_od = metadata[["casing_od"]],
              is_well = metadata[["is_well"]],
              well_depth = metadata[["drill_depth"]],
              top_of_screen = metadata[["top_of_screen"]],
              bottom_of_screen = metadata[["bottom_of_screen"]],
              well_head_stick_up = metadata[["well_head_stick_up"]],
              static_water_level = metadata[["static_water_level"]],
              estimated_yield = metadata[["estimated_yield"]],
              notes_borehole = metadata[["notes_borehole"]],
              notes_well = metadata[["notes_well"]],
              share_with_borehole = metadata[["share_with_borehole"]],
              drilled_by = metadata[["drilled_by"]],
              drill_method = NULL,
              purpose_of_well = metadata[["purpose_of_well"]],
              purpose_well_inferred = metadata[["purpose_well_inferred"]],
              share_with_well = metadata[["share_with_well"]]
            )

            showNotification(
              paste("Successfully uploaded borehole", current_borehole_id),
              type = "message",
              duration = 5
            )
            DBI::dbExecute(session$userData$AquaCache, "COMMIT")

            # Remove uploaded borehole from local data to prevent re-upload
            rv$borehole_data[[current_borehole_id]] <- NULL
            # Update selector choices
            updateSelectizeInput(
              session,
              "borehole_details_selector",
              choices = names(rv$borehole_data),
              selected = if (length(names(rv$borehole_data)) > 0) {
                names(rv$borehole_data)[1]
              } else {
                ""
              }
            )
          },
          error = function(e) {
            DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
            showNotification(
              paste("Error uploading borehole:", e$message),
              type = "error",
              duration = 10
            )
          }
        )
      } else {
        showNotification(
          "No valid borehole data to upload",
          type = "warning",
          duration = 7
        )
      }
    })

    observeEvent(input$upload_all, {
      req(rv$borehole_data)
      req(rv$files_df)

      if (!all_pages_assigned()) {
        showNotification(
          "Please assign every document page to a borehole before uploading.",
          type = "error",
          duration = 7
        )
        return()
      }

      assigned_ids <- rv$files_df$borehole_id
      assigned_ids <- assigned_ids[!is.na(assigned_ids) & nzchar(assigned_ids)]
      unique_borehole_ids <- unique(assigned_ids)
      total_boreholes <- length(unique_borehole_ids)

      if (total_boreholes == 0) {
        showNotification(
          "No boreholes to upload",
          type = "warning",
          duration = 7
        )
        return()
      }

      # Show processing notification
      showNotification(
        paste("Starting upload of", total_boreholes, "boreholes..."),
        type = "message",
        duration = 5
      )

      # Track success and errors
      success_count <- 0
      error_count <- 0

      # Loop through each unique borehole ID
      for (well_id in unique_borehole_ids) {
        if (!(well_id %in% names(rv$borehole_data))) {
          next
        }
        metadata <- rv$borehole_data[[well_id]]$metadata
        if (is.null(metadata)) {
          metadata <- empty_well_entry()$metadata
        }

        metadata <- sanitize_metadata_for_insert(metadata)
        if (!validate_metadata_for_upload(metadata)) {
          error_count <- error_count + 1
          next
        }

        tryCatch(
          {
            AquaCache::dbTransBegin(session$userData$AquaCache)
            # Create PDF with redactions for this borehole
            pdf_file_path <- create_pdf_with_redactions(
              well_id,
              return_path = TRUE
            )

            # Call AquaCache function with the metadata
            result <- AquaCache::insertACBorehole(
              con = session$userData$AquaCache,
              path = pdf_file_path,
              well_name = metadata[["name"]],
              latitude = metadata[["latitude"]],
              longitude = metadata[["longitude"]],
              location_source = metadata[["location_source"]],
              surveyed_ground_elev = metadata[[
                "surveyed_ground_elev"
              ]],
              purpose_of_borehole = metadata[["purpose_of_borehole"]],
              purpose_borehole_inferred = metadata[[
                "purpose_borehole_inferred"
              ]],
              depth_to_bedrock = metadata[["depth_to_bedrock"]],
              permafrost_present = metadata[["permafrost_present"]],
              permafrost_top = metadata[["permafrost_top"]],
              permafrost_bot = metadata[["permafrost_bot"]],
              date_drilled = metadata[["date_drilled"]],
              casing_od = metadata[["casing_od"]],
              is_well = metadata[["is_well"]],
              well_depth = metadata[["drill_depth"]],
              top_of_screen = metadata[["top_of_screen"]],
              bottom_of_screen = metadata[["bottom_of_screen"]],
              well_head_stick_up = metadata[["well_head_stick_up"]],
              static_water_level = metadata[["static_water_level"]],
              estimated_yield = metadata[["estimated_yield"]],
              notes_borehole = metadata[["notes_borehole"]],
              notes_well = metadata[["notes_well"]],
              share_with_borehole = metadata[["share_with_borehole"]],
              drilled_by = metadata[["drilled_by"]],
              drill_method = NULL,
              purpose_of_well = metadata[["purpose_of_well"]],
              purpose_well_inferred = metadata[["purpose_well_inferred"]],
              share_with_well = metadata[["share_with_well"]]
            )

            success_count <- success_count + 1

            # Show progress notification
            showNotification(
              paste(
                "Uploaded",
                success_count,
                "of",
                total_boreholes,
                "boreholes"
              ),
              type = "message",
              duration = 7
            )
            DBI::dbExecute(session$userData$AquaCache, "COMMIT")
            # Remove uploaded borehole from local data to prevent re-upload
            rv$borehole_data[[well_id]] <- NULL
          },
          error = function(e) {
            DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
            error_count <<- error_count + 1
            showNotification(
              paste0(
                "Error uploading borehole ",
                well_id,
                ": ",
                e$message,
                "\n"
              ),
              type = "error",
              duration = 10
            )
          }
        )
      }

      # Show final summary
      if (error_count == 0) {
        showNotification(
          paste("Successfully uploaded all", success_count, "boreholes!"),
          type = "message",
          duration = 5
        )
      } else {
        showNotification(
          paste(
            "Uploaded",
            success_count,
            "boreholes with",
            error_count,
            "errors"
          ),
          type = "warning",
          duration = 10
        )
      }
      # Update selector choices
      updateSelectizeInput(
        session,
        "borehole_details_selector",
        choices = names(rv$borehole_data),
        selected = if (length(names(rv$borehole_data)) > 0) {
          names(rv$borehole_data)[1]
        } else {
          ""
        }
      )
    })

    # Add observer for OCR extracted text display
    output$ocr_text_display <- renderText({
      req(rv$files_df)
      req(rv$pdf_index)

      if (length(rv$ocr_text) == 0) {
        return()
      }
      # Show selected text if available
      if (!is.null(rv$selected_text) && length(rv$selected_text) > 0) {
        return(paste(rv$selected_text, collapse = " "))
      }
      # Only show text if OCR mode is not "none"
      if (is.null(input$ocr_display_mode) || input$ocr_display_mode == "none") {
        return("")
      }
      ocr_df <- rv$ocr_text[[rv$pdf_index]]
      if (is.null(ocr_df) || nrow(ocr_df) == 0) {
        return("")
      }
      # Filter by confidence threshold
      conf <- if (is.null(input$confidence_threshold)) {
        0
      } else {
        input$confidence_threshold
      }
      ocr_df <- ocr_df[ocr_df$confidence >= conf, , drop = FALSE]
      if (nrow(ocr_df) == 0) {
        return("(no OCR text above threshold)")
      }
      # Group words into lines
      lines <- tryCatch(concat_ocr_words_by_row(ocr_df), error = function(e) {
        return(ocr_df$word)
      })
      if (length(lines) == 0) {
        return("")
      }
      txt <- paste(lines, collapse = "\n")
      # Truncate long output
      if (nchar(txt) > 4000) {
        txt <- paste0(substr(txt, 1, 4000), "... (truncated)")
      }
      txt
    })
  }) # End of moduleServer
} # End of server
