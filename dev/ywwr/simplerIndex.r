library(shiny)
library(bslib)

config <- list(dbName = "aquacache",
               dbHost = Sys.getenv("aquacacheHost"),
               dbPort = Sys.getenv("aquacachePort"),
               dbUser = Sys.getenv("aquacacheAdminUser"),
               dbPass = Sys.getenv("aquacacheAdminPass")
)

ui <- bslib::page_fluid(
  
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
            .btn-active {
                background-color: #007bff !important;
                border-color: #007bff !important;
                color: white !important;
            }
            .sidebar-layout {
                display: flex;
                position: relative;
            }
            .sidebar-panel {
                min-width: 200px;
                max-width: 600px;
                width: 300px;
                background: #f8f9fa;
                border-right: 1px solid #dee2e6;
                position: relative;
            }
            .right-panel {
                min-width: 200px;
                max-width: none;
                width: 400px;
                background: #f8f9fa;
                border-left: 1px solid #dee2e6;
                position: relative;
                padding: 0;
                display: flex;
                flex-direction: column;
                height: calc(100vh - 80px);
                overflow: hidden;
                z-index: 1000 !important; /* Ensure right panel is above other elements */
            }
            .borehole-header {
                background: #f8f9fa;
                border-bottom: 1px solid #dee2e6;
                padding: 15px;
                position: sticky;
                top: 0;
                z-index: 100;
            }
            .scrollable-content {
                flex: 1;
                overflow-y: auto; /* Enable vertical scrolling */
                padding: 15px;
                max-height: calc(100vh - 180px); /* Maximum height for scroll area */
                scrollbar-width: thin; /* For Firefox */
                scrollbar-color: #007bff #f0f0f0; /* For Firefox */
            }
            /* Styling for WebKit scrollbars (Chrome, Safari, newer Edge) */
            .scrollable-content::-webkit-scrollbar {
                width: 8px;
            }
            .scrollable-content::-webkit-scrollbar-track {
                background: #f0f0f0;
                border-radius: 4px;
            }
            .scrollable-content::-webkit-scrollbar-thumb {
                background-color: #007bff;
                border-radius: 4px;
                border: 2px solid #f0f0f0;
            }
            .main-panel {
                flex: 1;
                padding-left: 15px;
                min-width: 0; /* Allow panel to shrink below content width */
                overflow: auto; /* Allow scrolling if needed */
                background: #fafafa;
                z-index: 1 !important;
                position: relative;
            }
            #pdf-container {
                box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                border-radius: 4px;
            }
            #pdf-container .shiny-plot-output {
                box-shadow: 0 1px 3px rgba(0,0,0,0.2);
                border-radius: 2px;
            }
            .nav-btn {
                width: 40px;
                padding: 8px 0;
                margin: 0 2px;
            }
            .zoom-btn {
                width: 35px;
                padding: 6px 0;
                margin: 0 1px;
                font-size: 12px;
            }
            .reset-btn {
                padding: 6px 12px;
                margin: 0 5px;
                font-size: 12px;
            }
            .control-row {
                display: flex;
                align-items: center;
                gap: 10px;
                margin-bottom: 10px;
                flex-wrap: wrap;
            }
            .control-group {
                display: flex;
                align-items: center;
                gap: 5px;
            }
            .selectize-control {
                display: inline-block;
                vertical-align: middle;
                min-width: 180px;
            }
            .selectize-input {
                height: 34px;
                padding: 6px 12px;
                font-size: 14px;
                border-radius: 4px;
            }
            .right-panel .form-group {
                width: 100%;
            }
            .right-panel .form-control {
                width: 100% !important;
            }
            .right-panel .selectize-control {
                width: 100% !important;
                min-width: unset;
            }
            .right-panel .shiny-input-container {
                width: 100%;
            }
            .right-panel .shiny-date-input {
                width: 100%;
            }
            .right-panel .radio {
                margin-top: 5px;
                margin-bottom: 5px;
            }
            .borehole-id-display {
                background: #f8f9fa;
                border: 1px solid #ced4da;
                border-radius: 4px;
                padding: 8px 12px;
                font-family: monospace;
                font-size: 14px;
                color: #495057;
                margin-bottom: 10px;
            }
            
            /* New styles for OCR accordion */
            .accordion-button {
                background-color: #f8f9fa;
                color: #212529;
                padding: 8px 15px;
                font-weight: 500;
            }
            .accordion-button:not(.collapsed) {
                background-color: #e9ecef;
                color: #0d6efd;
            }
            .accordion-body {
                padding: 10px 15px;
                background-color: #ffffff;
                overflow: visible !important; /* Ensure dropdowns are visible */
            }
            .accordion {
                margin-bottom: 15px;
                border-radius: 6px;
                overflow: visible !important; /* Allow dropdowns to extend outside */
                box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            }
            
            /* Fix for selectize dropdowns */
            .selectize-dropdown {
                z-index: 10000 !important; /* Ensure dropdown appears above everything */
                position: absolute !important;
            }
            
            /* Ensure containers don't clip the dropdowns */
            .control-row, .control-group {
                overflow: visible !important;
            }
            
            /* Fix for the main panel to not hide dropdowns */
            .main-panel {
                overflow: visible !important; /* Allow dropdowns to extend outside */
            }
            
            /* Only enable scrolling on the PDF container */
            #pdf-container {
                overflow: auto !important; 
            }
            
            /* Fix z-index issues with panels and sliders */
            .right-panel {
                min-width: 200px;
                max-width: none;
                width: 400px;
                background: #f8f9fa;
                border-left: 1px solid #dee2e6;
                position: relative;
                padding: 0;
                display: flex;
                flex-direction: column;
                height: calc(100vh - 80px);
                overflow: hidden;
                z-index: 1000 !important; /* Ensure right panel is above other elements */
            }
            
            /* Ensure sliders are properly contained */
            .control-row .slider-container {
                position: relative;
                z-index: 1;
                overflow: visible;
            }
            
            /* Ensure slider input is contained */
            .main-panel .form-group {
                position: relative;
                z-index: 1;
                max-width: 180px;
            }
            
            /* Fix for the slider handles */
            .irs {
                position: relative;
                z-index: 1;
                width: 100%;
                max-width: 180px;
            }
            
            /* Ensure the main panel has a lower z-index than the right panel */
            .main-panel {
                z-index: 1 !important;
                position: relative;
            }
            
            /* Make sure sliders are clipped to their containers */
            .slider-animate-container {
                max-width: 180px !important;
                overflow: hidden !important;
            }
            
            /* === Added: resize handle styling === */
            .resize-handle, .resize-handle-right {
                position: absolute;
                top: 0;
                width: 6px;
                height: 100%;
                cursor: col-resize;
                background: rgba(0,0,0,0);
                transition: background 0.15s;
            }
            .resize-handle { right: 0; }
            .resize-handle-right { left: 0; }
            .resize-handle:hover, .resize-handle-right:hover {
                background: rgba(0,123,255,0.15);
            }
            .sidebar-panel, .right-panel {
                position: relative; /* ensure handles align properly */
            }
            /* While resizing give visual cue */
            body.resizing-col { cursor: col-resize !important; user-select: none !important; }
            
            /* === Resize overrides (appended) === */
            .sidebar-layout { align-items: stretch; }
            .sidebar-panel, .right-panel {
              flex: 0 0 auto !important;
              max-width: none !important;
              box-sizing: border-box;
            }
            .main-panel { flex: 1 1 auto !important; }
            .resize-handle, .resize-handle-right {
              width: 10px !important;
              z-index: 1001;
            }
            
            /* === Added: flash feedback for auto-filled fields === */
            @keyframes flashField {
              0% { box-shadow: 0 0 0 0 rgba(0,123,255,0.7); background:#fffbe6; }
              40% { box-shadow: 0 0 10px 4px rgba(0,123,255,0.45); background:#e7f3ff; }
              100% { box-shadow: 0 0 0 0 rgba(0,123,255,0); background:white; }
            }
            .flash-update {
              animation: flashField 1.2s ease-out;
              transition: background .4s;
            }
        "))
  ),
  
  div(style = "display: flex; align-items: center; gap: 10px;",
      div(id = "logo-container",
          # Try to load the logo image with error handling
          tags$img(src = "imgs/simplerIndex.png", 
                   style = "height: 40px; width: 60px; object-fit: contain; border-radius: 6px; background: #fff;",
                   srcset = "logo@2x.png 2x, logo@3x.png 3x",
                   onerror = "this.onerror=null; this.style.display='none'; document.getElementById('text-logo').style.display='flex';"),
          # Fallback text logo that appears if image fails to load
          div(id = "text-logo",
              style = "width: 60px; height: 40px; background: linear-gradient(135deg, #007bff, #0056b3); border-radius: 6px; display: none; align-items: center; justify-content: center; color: white; font-weight: bold; font-size: 14px;",
              "YWRR"
          )
      ),
      "Simpler Index",
      hr()
  ), 
  
  div(class = "sidebar-layout",
      div(class = "sidebar-panel", id = "sidebar",
          div(class = "resize-handle", id = "resize-handle"),
          fileInput(
            "pdf_file",
            "Upload PDF(s)",
            accept = ".pdf",
            multiple = TRUE
          ),
          # Navigation buttons
          fluidRow(
            column(12,
                   actionButton("prev_pdf", icon("arrow-left"), class = "nav-btn", title = "Previous"),
                   actionButton("next_pdf", icon("arrow-right"), class = "nav-btn", title = "Next"),
                   actionButton("remove_pdf", icon("trash"), title = "Remove Selected", class = "nav-btn")
            )
          ),
          br(),
          DT::DTOutput("pdf_table")
      ),
      div(class = "main-panel",
          # First row: select, redact, clear, save, zoom
          div(class = "control-row",
              div(class = "control-group",
                  actionButton("brush_select", "Select", icon("mouse-pointer"), class = "btn-toggle") %>% 
                    tooltip("Enable the selection tool for OCR and content redaction."),
                  actionButton("draw_rectangle", "Redact", icon("rectangle-xmark"), class = "btn-toggle") %>%
                    tooltip("Redact the selected area. Boxes are transparent for usability but can be made opaque on upload."),
                  actionButton("clear_rectangles", "Clear", icon("eraser"), class = "btn btn-outline-secondary", title = "Clear Rectangles"),
                  downloadButton("save_image", "Export PDF", class = "btn btn-outline-primary", title = "Export PDF with redactions and OCR text"),
                  # Zoom control - wrap in a container div
                  div(class = "slider-container",
                      sliderInput("zoom_level", "Zoom:",
                                  min = 0.5, max = 4.0, value = 1.0, step = 0.1,
                                  width = "150px"
                      )
                  )
              )
          ),
          
          # Replace the Second row with simplified OCR controls
          bslib::accordion(
            id = "ocr-controls-accordion",
            open = FALSE,
            bslib::accordion_panel(
              title = "OCR Controls",
              div(class = "control-row", style = "margin-top: 10px;",
                  div(class = "control-group",
                      selectInput("ocr_display_mode", "OCR Display Mode:",
                                  choices = list(
                                    "None" = "none",
                                    "Highlight Boxes" = "highlight", 
                                    "Text Overlay" = "text"
                                  ),
                                  selected = "none"
                      ),
                      div(class = "slider-container",
                          sliderInput("confidence_threshold", "OCR Confidence:",
                                      min = 40, max = 100, value = 70, step = 10,
                                      width = "150px"
                          )
                      ),
                      
                      # OCR Text Display
                      div(
                        style = "margin-left: 20px; width: 300px;",
                        h6("Extracted Text:", style = "margin-bottom: 5px; color: #495057;"),
                        div(
                          style = "max-height: 120px; overflow-y: auto; border: 1px solid #ccc; padding: 8px; background: white; font-family: monospace; font-size: 11px; font-weight: bold; color: #007bff;",
                          verbatimTextOutput("ocr_text_display")
                        )
                      )
                  ),
              )
            )
          ),
          
          div(
            id = "pdf-container",
            style = "width:100%; max-width:100%; height:calc(100vh - 300px); min-height:500px; border:1px solid #ccc; margin:10px auto; overflow:auto; background:white; position:relative; display:block; padding:0;",
            uiOutput("pdf_viewer")
          )
      ),
      div(class = "right-panel", id = "right-sidebar",
          div(class = "resize-handle-right", id = "resize-handle-right"),
          # Navigation buttons in right panel
          div(style = "padding: 15px; border-bottom: 1px solid #dee2e6;",
              fluidRow(
                column(12,
                       actionButton("prev_pdf_right", icon("arrow-left"), class = "nav-btn", title = "Previous"),
                       actionButton("next_pdf_right", icon("arrow-right"), class = "nav-btn", title = "Next"),
                       actionButton("remove_pdf_right", icon("trash"), title = "Remove Selected", class = "nav-btn")
                )
              )
          ),
          # Fixed borehole ID header - only the ID display
          div(class = "borehole-header",
              tags$label("Borehole ID:", style = "font-weight: bold; display: block; margin-bottom: 5px;"),
              div(
                class = "borehole-id-display",
                textOutput("borehole_id_display")
              )
          ),
          # Scrollable content area
          div(class = "scrollable-content",
              # Borehole linking controls in scrollable area
              fluidRow(
                column(12,
                       selectizeInput("borehole_id_selector", "Link img to BH:",
                                      choices = NULL,
                                      selected = NULL,
                                      options = list(
                                        placeholder = "Select borehole",
                                        maxItems = 1
                                      )
                       ) %>% tooltip("Select a borehole ID to link this file/page to an existing borehole record.")
                )
              ),
              fluidRow(
                column(12,
                       div(
                         class = "borehole-id-display",
                         style = "background: #e8f4fd; border-color: #007bff; color: #0056b3; margin-bottom: 15px;",
                         textOutput("file_count_display")
                       )
                )
              ),
              br(),
              
              # Well identification
              textInput("name", "Name:", placeholder = "Enter name"),
              textInput("notes", "Notes:", placeholder = "Enter any notes"),
              # Add 'drilled by' selectize input
              selectizeInput("drilled_by", "Drilled By:",
                             choices = NULL,
                             selected = NULL,
                             options = list(
                               create = TRUE,
                               placeholder = "Select or enter driller",
                               maxItems = 1
                             )
              ),
              
              # Location information section - remove surveyed_location_top_casing field
              radioButtons("coordinate_system", "Coordinate System:",
                           choices = list("UTM" = "utm", "Lat/Lon" = "latlon"),
                           selected = "utm",
                           inline = TRUE
              ),
              conditionalPanel(
                condition = "input.coordinate_system == 'utm'",
                numericInput("easting", "Easting:", value = NULL, min = 0),
                numericInput("northing", "Northing:", value = NULL, min = 0),
                selectizeInput("utm_zone", "UTM Zone:",
                               choices = list(
                                 "7N" = "7N", "8N" = "8N", "9N" = "9N", "10N" = "10N",
                                 "11N" = "11N", "12N" = "12N", "13N" = "13N"
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
                numericInput("latitude", "Latitude:", value = NULL, min = 40, max = 85, step = 0.000001),
                numericInput("longitude", "Longitude:", value = NULL, min = -141, max = -60, step = 0.000001)
              ),
              textInput("location_source", "Location Source:", placeholder = "GPS, Survey, etc."),
              # Removed surveyed_location_top_casing field
              
              selectizeInput("purpose_of_well", "Purpose of Well:",
                             choices = list(
                               "Domestic" = "domestic",
                               "Municipal" = "municipal",
                               "Industrial" = "industrial",
                               "Agricultural" = "agricultural",
                               "Monitoring" = "monitoring",
                               "Test Well" = "test_well",
                               "Other" = "other"
                             ),
                             selected = NULL,
                             options = list(
                               placeholder = "Select purpose",
                               maxItems = 1
                             )
              ),
              
              # Well construction details
              fluidRow(
                column(8, numericInput("depth_to_bedrock", "Depth to Bedrock:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("depth_to_bedrock_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
              ),
              
              # Add permafrost checkbox and conditional inputs
              checkboxInput("permafrost_present", "Permafrost Present", value = FALSE),
              
              conditionalPanel(
                condition = "input.permafrost_present == true",
                fluidRow(
                  column(8, numericInput("permafrost_top_depth", "Depth to Top of Permafrost:", value = NULL, min = 0, step = 0.1)),
                  column(4, radioButtons("permafrost_top_depth_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
                ),
                fluidRow(
                  column(8, numericInput("permafrost_bottom_depth", "Depth to Bottom of Permafrost:", value = NULL, min = 0, step = 0.1)),
                  column(4, radioButtons("permafrost_bottom_depth_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
                )
              ),
              
              dateInput("date_drilled", "Date Drilled:", value = NULL),
              
              # Drill Depth and unit
              fluidRow(
                column(8, numericInput("drill_depth", "Drill Depth:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("drill_depth_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
              ),
              fluidRow(
                column(8, numericInput("surveyed_ground_level_elevation", "Surveyed Ground Level Elevation:", value = NULL, step = 0.01)),
                column(4, radioButtons("surveyed_ground_level_elevation_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
              ),
              checkboxInput("is_well", "Well Constructed", value = FALSE),
              
              # Show well construction fields only if 'is_well' is checked
              conditionalPanel(
                condition = "input.is_well == true",
                # Casing Outside Diameter
                fluidRow(
                  column(8, numericInput("casing_outside_diameter", "Casing Outside Diameter:", value = NULL, min = 0, step = 1)),
                  column(4, radioButtons("casing_outside_diameter_unit", "", choices = list("mm" = "mm", "inch" = "inch"), selected = "inch", inline = TRUE))
                ),
                # Top of Screen
                fluidRow(
                  column(8, numericInput("top_of_screen", "Top of Screen:", value = NULL, min = 0, step = 0.1)),
                  column(4, radioButtons("top_of_screen_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
                ),
                # Bottom of Screen
                fluidRow(
                  column(8, numericInput("bottom_of_screen", "Bottom of Screen:", value = NULL, min = 0, step = 0.1)),
                  column(4, radioButtons("bottom_of_screen_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
                ),
                # Well Head Stick Up
                fluidRow(
                  column(8, numericInput("well_head_stick_up", "Well Head Stick Up:", value = NULL, step = 0.01)),
                  column(4, radioButtons("well_head_stick_up_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
                ),
                # Static Water Level
                fluidRow(
                  column(8, numericInput("static_water_level", "Static Water Level:", value = NULL, step = 0.01)),
                  column(4, radioButtons("static_water_level_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
                ),
                # Estimated Yield
                fluidRow(
                  column(8, numericInput("estimated_yield", "Estimated Yield:", value = NULL, min = 0, step = 0.1)),
                  column(4, radioButtons("estimated_yield_unit", "", choices = list("L/s" = "L/s", "G/min" = "G/min"), selected = "G/min", inline = TRUE))
                )
              ),
              
              # Add upload buttons at the bottom of the scrollable content
              div(
                style = "margin-top: 30px; padding-top: 15px; border-top: 1px solid #dee2e6;",
                fluidRow(
                  column(6,
                         actionButton("upload_selected", "Upload Selected", 
                                      class = "btn btn-primary btn-block",
                                      icon = icon("upload"))
                  ),
                  column(6,
                         actionButton("upload_all", "Upload All", 
                                      class = "btn btn-success btn-block",
                                      icon = icon("cloud-upload-alt"))
                  )
                ),
                
              )
          )
      )
  ),
  
  # Replace previous resize script block with this one
  tags$script(HTML("
    $(function() {
      const MIN_LEFT = 140;
      const MIN_RIGHT = 160;
      const MIN_MAIN = 300; // keep central workspace usable
      let resizing = '';
      let startX = 0;
      let startWidth = 0;
      const $body = $('body');
      const $left = $('#sidebar');
      const $right = $('#right-sidebar');

      function viewportW(){ return $(window).width(); }
      function maxLeft(){ return Math.max(MIN_LEFT, viewportW() - $right.outerWidth() - MIN_MAIN); }
      function maxRight(){ return Math.max(MIN_RIGHT, viewportW() - $left.outerWidth() - MIN_MAIN); }

      function startResize(side, e){
        resizing = side;
        startX = e.clientX;
        startWidth = (side === 'left' ? $left.outerWidth() : $right.outerWidth());
        $body.addClass('resizing-col');
        e.preventDefault();
      }

      $('#resize-handle').off('.rs').on('mousedown.rs', e => startResize('left', e));
      $('#resize-handle-right').off('.rs').on('mousedown.rs', e => startResize('right', e));

      // Double-click reset
      $('#resize-handle').off('dblclick.rs').on('dblclick.rs', () => $left.css('width', '300px'));
      $('#resize-handle-right').off('dblclick.rs').on('dblclick.rs', () => $right.css('width', '400px'));

      $(document).off('.rsMove').on('mousemove.rsMove', function(e){
        if(!resizing) return;
        if(resizing === 'left'){
          let w = startWidth + (e.clientX - startX);
          w = Math.min(maxLeft(), Math.max(MIN_LEFT, w));
          $left.css('width', w + 'px');
        } else if(resizing === 'right'){
          let w = startWidth - (e.clientX - startX);
          w = Math.min(maxRight(), Math.max(MIN_RIGHT, w));
          $right.css('width', w + 'px');
        }
      });

      $(document).off('.rsUp').on('mouseup.rsUp', function(){
        if(resizing){
          resizing = '';
          $body.removeClass('resizing-col');
        }
      });

      $(window).off('.rsWin').on('resize.rsWin', function(){
        if($left.outerWidth() > maxLeft()) $left.css('width', maxLeft() + 'px');
        if($right.outerWidth() > maxRight()) $right.css('width', maxRight() + 'px');
      });

      /* Reattach existing focus/click handlers */
      const ids = [
        'name','notes','easting','northing','latitude','longitude',
        'location_source','depth_to_bedrock','permafrost_top_depth',
        'permafrost_bottom_depth','date_drilled','casing_outside_diameter',
        'drill_depth','surveyed_ground_level_elevation','top_of_screen',
        'bottom_of_screen','well_head_stick_up','static_water_level','estimated_yield'
      ];
      ids.forEach(id => {
        $(document).off('focus.rs click.rs', '#' + id)
                   .on('focus.rs click.rs', '#' + id, function(){
                     Shiny.setInputValue(id + '_clicked', Math.random());
                   });
      });
    });
  "))
)





server <- function(input, output, session) {
  
  
  session$userData$AquaCache <- AquaCache::AquaConnect(name = config$dbName, 
                                                       host = config$dbHost, 
                                                       port = config$dbPort, 
                                                       username = config$dbUser, 
                                                       password = config$dbPass)
  
  concat_ocr_words_by_row <- function(ocr_df) {
    if (is.null(ocr_df) || nrow(ocr_df) == 0) return(character(0))
    # Parse bbox coordinates
    coords <- do.call(rbind, lapply(ocr_df$bbox, function(b) as.numeric(strsplit(b, ",")[[1]])))
    ymin <- coords[,2]
    ymax <- coords[,4]
    words <- ocr_df$word
    
    result <- character(0)
    current_line <- ""
    prev_ymin <- ymin[1]
    prev_ymax <- ymax[1]
    
    for (i in seq_along(words)) {
      if (i == 1) {
        current_line <- words[i]
      } else {
        # If current ymax < previous ymin, start new line
        if (ymax[i] < prev_ymin) {
          result <- c(result, current_line)
          current_line <- words[i]
        } else {
          current_line <- paste(current_line, words[i], sep = " ")
        }
        prev_ymin <- ymin[i]
        prev_ymax <- ymax[i]
      }
    }
    result <- c(result, current_line)
    return(result)
  }
  
  filter_ocr_noise <- function(ocr_df) {
    if (is.null(ocr_df) || nrow(ocr_df) == 0) return(ocr_df)
    
    # Define common OCR error patterns and noise words
    noise_patterns <- c(
      # Empty strings and whitespace
      "^$",            # Empty string
      "^\\s*$",        # Only whitespace (spaces, tabs, newlines)
      "^\\s+$",        # One or more whitespace characters
      
      # Single characters that are often OCR errors
      "^-+$",          # Only dashes
      "^=+$",          # Only equals signs
      "^\\|+$",        # Only vertical bars/pipes
      "^_+$",          # Only underscores
      "^\\++$",        # Only plus signs
      "^\\*+$",        # Only asterisks
      "^#+$",          # Only hash symbols
      "^~+$",          # Only tildes
      "^`+$",          # Only backticks
      "^'+$",          # Only single quotes
      "^\"+$",         # Only double quotes
      "^\\^+$",        # Only carets
      "^&+$",          # Only ampersands
      "^%+$",          # Only percent signs
      "^@+$",          # Only at symbols
      "^\\$+$",        # Only dollar signs
      
      # Common OCR misreads
      "^[\\|Il1]{1,3}$",  # Vertical bars, I, l, 1 confusion (1-3 chars)
      "^[oO0]{1,2}$",     # o, O, 0 confusion (1-2 chars)
      "^[cC]{1}$",        # Single c or C
      "^[rR]{1}$",        # Single r or R
      "^[nN]{1}$",        # Single n or N
      "^[mM]{1}$",        # Single m or M
      "^[uU]{1}$",        # Single u or U
      "^[vV]{1}$",        # Single v or V
      "^[wW]{1}$",        # Single w or W
      "^[iI]{1,2}$",      # Single i or ii (common OCR noise)
      
      # Repeated characters (likely errors)
      "^([a-zA-Z])\\1{3,}$",  # Same letter repeated 4+ times
      
      # Pure punctuation strings
      "^[[:punct:]]+$",   # Only punctuation marks
      
      # Very short meaningless combinations
      "^[a-zA-Z]{1}[0-9]{1}$",  # Single letter + single digit
      "^[0-9]{1}[a-zA-Z]{1}$",  # Single digit + single letter
      
      # Common OCR artifacts
      "^\\.[a-zA-Z]{1,2}$",     # Dot followed by 1-2 letters
      "^[a-zA-Z]{1,2}\\.$",     # 1-2 letters followed by dot
      "^[\\(\\)\\[\\]\\{\\}]+$" # Only brackets/parentheses
    )
    
    # Additional filter: words that are too short and likely meaningless
    # Keep single letters that could be meaningful (like "A", "I", etc.)
    meaningful_single_chars <- c("A", "a", "O", "o")
    
    # Create filter condition - first remove empty/whitespace strings
    keep_word <- rep(TRUE, nrow(ocr_df))
    
    # Remove empty strings and whitespace-only strings
    keep_word <- keep_word & !is.na(ocr_df$word) & 
      ocr_df$word != "" & 
      trimws(ocr_df$word) != ""
    
    for (pattern in noise_patterns) {
      keep_word <- keep_word & !grepl(pattern, ocr_df$word, perl = TRUE)
    }
    
    # Additional filters
    # Remove very short words that aren't meaningful single characters
    short_and_meaningless <- nchar(trimws(ocr_df$word)) == 1 & 
      !trimws(ocr_df$word) %in% meaningful_single_chars &
      !grepl("^[0-9]$", trimws(ocr_df$word))  # Keep single digits
    
    keep_word <- keep_word & !short_and_meaningless
    
    # Remove words with very low confidence that are also short
    low_conf_short <- ocr_df$confidence < 30 & nchar(trimws(ocr_df$word)) <= 2
    keep_word <- keep_word & !low_conf_short
    
    # Filter the dataframe
    filtered_df <- ocr_df[keep_word, ]
    
    return(filtered_df)
  }
  
  # Helper function to apply different preprocessing methods
  preprocess_image <- function(img, method = "default") {
    switch(method,
           "default" = {
             img %>%
               magick::image_convert(colorspace = "gray") %>%
               magick::image_contrast(sharpen = 1) %>%
               magick::image_modulate(brightness = 110, saturation = 100, hue = 100) %>%
               magick::image_threshold(type = "black", threshold = "60%")
           },
           "enhance_dark" = {
             img %>%
               magick::image_convert(colorspace = "gray") %>%
               magick::image_contrast(sharpen = 2) %>%
               magick::image_modulate(brightness = 130, saturation = 0) %>%
               magick::image_threshold(type = "black", threshold = "45%")
           },
           "enhance_light" = {
             img %>%
               magick::image_convert(colorspace = "gray") %>%
               magick::image_contrast(sharpen = 1.5) %>%
               magick::image_modulate(brightness = 90, saturation = 0) %>%
               magick::image_threshold(type = "black", threshold = "70%")
           },
           "high_contrast" = {
             img %>%
               magick::image_convert(colorspace = "gray") %>%
               magick::image_contrast(sharpen = 3) %>%
               magick::image_normalize() %>%
               magick::image_threshold(type = "black", threshold = "50%")
           },
           "denoise" = {
             img %>%
               magick::image_convert(colorspace = "gray") %>%
               magick::image_enhance() %>%
               magick::image_threshold(type = "black", threshold = "60%")
           },
           "deskew" = {
             # Attempt to deskew (straighten) the image
             img %>%
               magick::image_convert(colorspace = "gray") %>%
               magick::image_deskew(threshold = 40) %>%
               magick::image_contrast(sharpen = 1) %>%
               magick::image_threshold(type = "black", threshold = "60%")
           },
           # Default fallback if method is not recognized
           {
             img %>%
               magick::image_convert(colorspace = "gray") %>%
               magick::image_contrast(sharpen = 1) %>%
               magick::image_modulate(brightness = 110, saturation = 100, hue = 100) %>%
               magick::image_threshold(type = "black", threshold = "60%")
           }
    )
  }
  
  process_ocr_batch <- function(files_df, ocr_text_list, current_index = NULL) {
    # Check if any OCR processing is needed
    needs_processing <- any(sapply(ocr_text_list, function(x) is.null(x)))
    
    if (!needs_processing) {
      # All OCR already processed
      showNotification("OCR data loaded from cache", type = "message", duration = 2)
      return(ocr_text_list)
    }
    
    # Check if we have some cached OCR results
    has_some_cached <- any(sapply(ocr_text_list, function(x) !is.null(x)))
    
    if (has_some_cached) {
      showNotification("Using cached OCR results where available", type = "message", duration = 2)
    } else {
      showNotification("Starting OCR processing...", type = "message", duration = 2)
    }
    
    # Show progress notification for batch processing
    total_pages <- nrow(files_df)
    pages_to_process <- sum(sapply(ocr_text_list, function(x) is.null(x)))
    
    if (pages_to_process > 0) {
      showNotification(paste("Processing OCR for", pages_to_process, "pages using PSM mode", psm_mode, ". This may take a moment..."), 
                       type = "warning", duration = 5)
      
      # Loop through all images in files_df and run OCR
      for (i in seq_len(nrow(files_df))) {
        if (is.null(ocr_text_list[[i]])) {
          # Show progress for current page
          showNotification(paste("Processing page", i, "of", total_pages), 
                           type = "message", duration = 1)
          
          img_path <- files_df$Path[i]
          # Add error handling for image loading
          tryCatch({
            img <- magick::image_read(img_path)
            
            # Apply selected preprocessing method
            img <- preprocess_image(img, preprocessing_method)
            
            # Create OCR engine options
            tessoptions <- list(
              tessedit_create_hocr = 1,
              tessedit_pageseg_mode = psm_mode
            )
            
            # Add whitelist if provided
            if (!is.null(whitelist) && whitelist != "") {
              tessoptions$tessedit_char_whitelist <- whitelist
            }
            
            # Perform OCR on the preprocessed image
            ocr_result <- tesseract::ocr_data(img, engine = tesseract::tesseract(
              options = tessoptions
            ))
            
            # Filter out common OCR noise and error words
            ocr_result <- filter_ocr_noise(ocr_result)
            ocr_text_list[[i]] <- ocr_result
          }, error = function(e) {
            message("Error processing OCR for page ", i, ": ", e$message)
            # Create an empty dataframe with the right structure instead of NULL
            ocr_text_list[[i]] <- data.frame(
              word = character(0),
              confidence = numeric(0),
              bbox = character(0),
              stringsAsFactors = FALSE
            )
          })
        }
      }
      
      # Show completion notification
      showNotification("OCR processing completed!", type = "message", duration = 3)
    }
    
    return(ocr_text_list)
  }
  
  split_pdf_to_pages <- function(pdf_path, output_dir = tempdir()) {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("The 'pdftools' package is required.")
    }
    n_pages <- pdftools::pdf_info(pdf_path)$pages
    out_files <- character(n_pages)
    for (i in seq_len(n_pages)) {
      out_file <- file.path(output_dir, sprintf("%s_page_%d.pdf", tools::file_path_sans_ext(basename(pdf_path)), i))
      pdftools::pdf_subset(pdf_path, pages = i, output = out_file)
      out_files[i] <- normalizePath(out_file)
    }
    out_files
  }
  
  # Store split PDF info
  rv <- reactiveValues(
    files_df = NULL,
    well_data = list(),  # Named list organized by borehole ID
    pdf_index = 1,
    ocr_text = list(),
    ocr_display_mode = "none",
    selected_text = NULL,
    rectangles = list(),
    selected_driller = NULL
  )
  
  moduleData <- reactiveValues(
    drillers = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM boreholes.drillers")
  )
  
  # Reactive value to control brush mode
  brush_enabled <- reactiveVal(FALSE)
  
  # Flag to prevent circular updates when loading metadata
  loading_metadata <- reactiveVal(FALSE)
  
  # Add observer for brush_select button
  observeEvent(input$brush_select, {
    # Toggle brush_enabled value
    brush_enabled(!brush_enabled())
    
    # Update button appearance based on new state
    if (brush_enabled()) {
      shinyjs::runjs("$('#brush_select').addClass('btn-active');")
    } else {
      shinyjs::runjs("$('#brush_select').removeClass('btn-active');")
    }
  })
  
  # Reactive expression to get the current well ID based on pdf_index
  current_well_id <- reactive({
    req(rv$files_df)
    req(rv$pdf_index)
    req(nrow(rv$files_df) >= rv$pdf_index)
    
    # Since each row has its own unique well ID, just return it directly
    return(rv$files_df$borehole_id[rv$pdf_index])
  })
  
  
  
  # Update the 'drillers' list based on the data loaded from Aquacache
  
  observe({
    updateSelectizeInput(
      session, 
      "drilled_by", 
      choices = stats::setNames(moduleData$drillers$driller_id, moduleData$drillers$name),
      selected = rv$selected_driller,
      options = list(
        placeholder = "Select or enter driller",
        create = TRUE,
        maxItems = 1
      )
    ) 
  })
  
  
  # Observer for new driller creation
  observeEvent(input$drilled_by, {
    req(input$drilled_by)
    
    # Get current value
    current_value <- input$drilled_by
    
    # Check if this is a new driller
    existing_driller_ids <- moduleData$drillers$driller_id
    existing_driller_names <- moduleData$drillers$name
    
    # If not in existing IDs or names, it's a new driller
    if (!(current_value %in% existing_driller_ids) && !(current_value %in% existing_driller_names)) {
      # Create modal dialog
      showModal(modalDialog(
        title = "New Driller Information",
        
        textInput("new_driller_name", "Name", value = current_value),
        textInput("new_driller_address", "Address"),
        textInput("new_driller_phone", "Phone"),
        textInput("new_driller_email", "Email"),
        
        footer = tagList(
          actionButton("cancel_new_driller", "Cancel"),
          actionButton("save_new_driller", "Save", class = "btn-primary")
        )
      ))
    }
  })
  
  # Handle the save button in the modal
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
      if (nchar(clean_phone) == 10 || (nchar(clean_phone) == 11 && substr(clean_phone, 1, 1) == "1")) {
        # Format the phone number for display: (XXX) XXX-XXXX
        if (nchar(clean_phone) == 11) {
          clean_phone <- substr(clean_phone, 2, 11)  # Remove the leading 1
        }
        new_driller_phone <- paste0("(", substr(clean_phone, 1, 3), ") ", 
                                    substr(clean_phone, 4, 6), "-", 
                                    substr(clean_phone, 7, 10))
      } else {
        showNotification("Invalid phone number format. Please use a 10-digit number.", type = "error", duration = 5)
        #removeModal()
        return()  # Exit the function early
      }
    }
    
    # Validate email format if provided
    if (!is.null(new_driller_email) && trimws(new_driller_email) != "") {
      # Basic email format validation
      email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
      if (!grepl(email_pattern, new_driller_email)) {
        showNotification("Invalid email format. Please enter a valid email address.", 
                         type = "error", duration = 5)
        #removeModal()
        return()  # Exit the function early
      }
    }
    
    new_driller_id <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "INSERT INTO boreholes.drillers (name,address,phone,email)
+    VALUES ($1,$2,$3,$4) RETURNING driller_id",
      params = list(new_driller_name, 
                    ifelse(trimws(new_driller_address) == "", "NULL", paste0("'", new_driller_address, "'")), ", ", 
                    ifelse(trimws(new_driller_phone) == "", "NULL", paste0("'", new_driller_phone, "'")), ", ", 
                    ifelse(trimws(new_driller_email) == "", "NULL", paste0("'", new_driller_email, "'")), ") "
      )
    )[1,1]
    
    moduleData$drillers <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM boreholes.drillers")
    rv$selected_driller <- new_driller_id
    removeModal()
  })
  
  # Handle modal cancel
  observeEvent(input$cancel_new_driller, {
    # If user cancels, revert to no selection
    updateSelectizeInput(session, "drilled_by", selected = "")
    removeModal()
  })
  
  # Split PDFs into single-page files on upload
  observeEvent(input$pdf_file, {
    
    uploaded_files <- input$pdf_file
    # Rename uploaded files to their original names
    for (i in seq_len(nrow(uploaded_files))) {
      orig_name <- uploaded_files$name[i]
      orig_path <- file.path(dirname(uploaded_files$datapath[i]), orig_name)
      file.rename(uploaded_files$datapath[i], orig_path)
      uploaded_files$datapath[i] <- orig_path
    }
    
    req(uploaded_files)
    
    rv$files_df <- uploaded_files
    
    # Show initial loading notification
    total_files <- nrow(uploaded_files)
    showNotification(paste("Starting conversion of", total_files, "PDF file(s) to images..."), 
                     type = "default", duration = 3)
    
    for (i in seq_len(nrow(uploaded_files))) {
      pdf_path <- uploaded_files$datapath[i][1]
      orig_name <- uploaded_files$name[i]
      
      # Show progress for current file
      showNotification(paste("Converting", orig_name, "- File", i, "of", total_files), 
                       type = "message", duration = 2)
      
      # Convert PDF to PNG files (one per page) and save to tempdir
      png_files <- pdftools::pdf_convert(pdf_path, dpi = 150, filenames = file.path(tempdir(), sprintf("%s_page_%d.png", tools::file_path_sans_ext(basename(pdf_path)), seq_len(pdftools::pdf_info(pdf_path)$pages))))
      
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
      
      if (i == 1) {
        all_split_files <- split_df
      } else {
        all_split_files <- rbind(all_split_files, split_df)
      }
      
      # Show completion for current file
      showNotification(paste("Completed converting", orig_name, "- Generated", length(png_files), "page(s)"), 
                       type = "message", duration = 1.5)
      
    }
    
    rv$files_df <- all_split_files
    # Initialize well information as named list organized by borehole ID
    well_fields <- c(
      "name", 
      "notes",  # Make sure notes is included
      "coordinate_system",
      "easting", "northing", "utm_zone", "latitude", "longitude",
      "location_source", "purpose_of_well",
      "depth_to_bedrock", "depth_to_bedrock_unit", "date_drilled",
      "casing_outside_diameter", "casing_outside_diameter_unit",
      "drill_depth", "drill_depth_unit", "top_of_screen", "top_of_screen_unit",
      "bottom_of_screen", "bottom_of_screen_unit", "well_head_stick_up",
      "well_head_stick_up_unit", "static_water_level", "static_water_level_unit",
      "estimated_yield", "estimated_yield_unit", "surveyed_ground_level_elevation",
      "surveyed_ground_level_elevation_unit",
      "permafrost_present", "permafrost_top_depth", "permafrost_top_depth_unit",
      "permafrost_bottom_depth", "permafrost_bottom_depth_unit",
      "is_well", "drilled_by"
    )
    
    # Initialize well_data as empty named list
    rv$well_data <- list()
    
    # Assign unique borehole IDs to each row (page) in files_df
    for (i in seq_len(nrow(rv$files_df))) {
      # Generate unique ID for each page
      borehole_id <- paste0("BH", sprintf("%04d", i))
      rv$files_df$borehole_id[i] <- borehole_id
      
      # Create metadata list with NA values for all fields
      metadata <- list()
      for (field in well_fields) {
        metadata[[field]] <- NA
      }
      
      # Create well_data entry for this individual page/well
      rv$well_data[[borehole_id]] <- list(
        files = rv$files_df$NewFilename[i],  # Add filename for this well
        metadata = metadata  # Named list of well metadata
      )
    }
    
    rv$pdf_index <- 1
    DT::dataTableProxy("pdf_table") %>% DT::selectRows(1)
    
    rv$ocr_text <- vector("list", nrow(rv$files_df))
    rv$ocr_display_mode <- "none"
    
    # Reset button states on upload
    brush_enabled(FALSE)
    updateSelectizeInput(session, "ocr_display_mode", selected = "none")
    
    shinyjs::runjs("$('#brush_select').removeClass('btn-active');")
    
    # Show final completion notification
    total_pages <- nrow(rv$files_df)
    showNotification(paste("PDF conversion completed! Generated", total_pages, "page(s) total."), 
                     type = "message", duration = 4)
  })
  
  observeEvent(input$pdf_table_rows_selected, {
    req(input$pdf_table_rows_selected)
    rv$pdf_index <- input$pdf_table_rows_selected
  })
  
  observe({
    req(rv$files_df)
    req(rv$pdf_index)
    # Use isolate to prevent reactive feedback loop
    current_selection <- isolate(input$pdf_table_rows_selected)
    if (is.null(current_selection) || length(current_selection) == 0 || current_selection != rv$pdf_index) {
      DT::dataTableProxy("pdf_table") %>% DT::selectRows(rv$pdf_index)
    }
  })
  
  observeEvent(input$next_pdf, {
    req(rv$files_df)
    if (rv$pdf_index < nrow(rv$files_df)) {
      rv$pdf_index <- rv$pdf_index + 1
      # Ensure table selection follows
      DT::dataTableProxy("pdf_table") %>% DT::selectRows(rv$pdf_index)
    }
  })
  
  observeEvent(input$prev_pdf, {
    req(rv$files_df)
    if (rv$pdf_index > 1) {
      rv$pdf_index <- rv$pdf_index - 1
      # Ensure table selection follows
      DT::dataTableProxy("pdf_table") %>% DT::selectRows(rv$pdf_index)
    }
  })
  
  observeEvent(input$remove_pdf, {
    req(rv$files_df)
    if (nrow(rv$files_df) > 0) {
      selected_row <- rv$pdf_index
      
      # Find which well this page belongs to
      well_id_to_remove <- rv$files_df$borehole_id[selected_row]
      if (!is.null(well_id_to_remove) && well_id_to_remove %in% names(rv$well_data)) {
        rv$well_data[[well_id_to_remove]] <- NULL
      }
      
      # Remove from files_df
      rv$files_df <- rv$files_df[-selected_row, ]
      rv$ocr_text <- rv$ocr_text[-selected_row]
      
      # Update well_data structure - adjust file indices
      for (well_id in names(rv$well_data)) {
        file_index <- rv$well_data[[well_id]]$files
        # Adjust the file index if it's greater than the removed row
        if (file_index > selected_row) {
          rv$well_data[[well_id]]$files <- file_index - 1
        }
        
        
        # store filenames only; remove if this page's filename is present
        fname <- rv$files_df$NewFilename[selected_row]
        rv$well_data[[well_id]]$files <- setdiff(rv$well_data[[well_id]]$files, fname)
        
        
      }
      
      if (nrow(rv$files_df) == 0) {
        rv$pdf_index <- 1
      } else if (rv$pdf_index > nrow(rv$files_df)) {
        rv$pdf_index <- nrow(rv$files_df)
      }
    }
  })
  
  
  observeEvent(input$next_pdf_right, {
    req(rv$files_df)
    if (rv$pdf_index < nrow(rv$files_df)) {
      rv$pdf_index <- rv$pdf_index + 1
      # Ensure table selection follows
      DT::dataTableProxy("pdf_table") %>% DT::selectRows(rv$pdf_index)
    }
  })
  
  observeEvent(input$prev_pdf_right, {
    req(rv$files_df)
    if (rv$pdf_index > 1) {
      rv$pdf_index <- rv$pdf_index - 1
      # Ensure table selection follows
      DT::dataTableProxy("pdf_table") %>% DT::selectRows(rv$pdf_index)
    }
  })
  
  observeEvent(input$remove_pdf_right, {
    req(rv$files_df)
    if (nrow(rv$files_df) > 0) {
      selected_row <- rv$pdf_index
      
      # Find which well this page belongs to
      well_id_to_remove <- rv$files_df$borehole_id[selected_row]
      if (!is.null(well_id_to_remove) && well_id_to_remove %in% names(rv$well_data)) {
        rv$well_data[[well_id_to_remove]] <- NULL
      }
      
      # Remove from files_df
      rv$files_df <- rv$files_df[-selected_row, ]
      rv$ocr_text <- rv$ocr_text[-selected_row]
      
      # Update well_data structure - adjust file indices
      for (well_id in names(rv$well_data)) {
        file_index <- rv$well_data[[well_id]]$files
        # Adjust the file index if it's greater than the removed row
        if (file_index > selected_row) {
          rv$well_data[[well_id]]$files <- file_index - 1
        }
      }
      
      if (nrow(rv$files_df) == 0) {
        rv$pdf_index <- 1
      } else if (rv$pdf_index > nrow(rv$files_df)) {
        rv$pdf_index <- nrow(rv$files_df)
      }
    }
  })
  
  
  # Fix the PDF table rendering
  output$pdf_table <- DT::renderDT({
    req(rv$files_df)
    validate(need(nrow(rv$files_df) > 0, "No files uploaded yet"))
    
    dat <- rv$files_df[, c("tag", "borehole_id")]
    
    DT::datatable(
      dat,
      selection = "single",
      options = list(
        pageLength = 10, 
        dom = 'tip',  # table, information, pagination (no search)
        ordering = FALSE,
        scrollY = "300px",
        scrollCollapse = TRUE
      )
    )
  })
  
  # Ensure the table selection works properly
  observe({
    req(rv$files_df)
    req(rv$pdf_index)
    if (nrow(rv$files_df) > 0 && rv$pdf_index <= nrow(rv$files_df)) {
      # Isolate to prevent circular reactivity
      isolate({
        current_selection <- input$pdf_table_rows_selected
        if (is.null(current_selection) || length(current_selection) == 0 || current_selection != rv$pdf_index) {
          DT::dataTableProxy("pdf_table") %>% DT::selectRows(rv$pdf_index)
        }
      })
    }
  })
  
  # Add a reactive value for OCR processing status
  ocr_processing <- reactiveVal(FALSE)
  
  # Modified observer for OCR display mode: process OCR for all images when mode is highlight/text
  observeEvent(input$ocr_display_mode, {
    req(rv$files_df)
    rv$ocr_display_mode <- input$ocr_display_mode
    
    if (rv$ocr_display_mode %in% c("highlight", "text")) {
      # Set processing flag
      ocr_processing(TRUE)
      # Process OCR for all images/pages
      for (i in seq_len(nrow(rv$files_df))) {
        if (is.null(rv$ocr_text[[i]]) || nrow(rv$ocr_text[[i]]) == 0) {
          img_path <- rv$files_df$Path[i]
          tryCatch({
            img <- magick::image_read(img_path)
            img <- img %>% magick::image_convert(colorspace = "gray") %>% magick::image_contrast(sharpen = 1) %>% magick::image_modulate(brightness = 110, saturation = 100, hue = 100) %>% magick::image_threshold(type = "black", threshold = "60%")
            tessoptions <- list(
              tessedit_create_hocr = 1,
              tessedit_pageseg_mode = 3
            )
            ocr_result <- tesseract::ocr_data(img, engine = tesseract::tesseract(
              options = tessoptions
            ))
            ocr_result <- filter_ocr_noise(ocr_result)
            rv$ocr_text[[i]] <- ocr_result
          }, error = function(e) {
            rv$ocr_text[[i]] <- data.frame(
              word = character(0),
              confidence = numeric(0),
              bbox = character(0),
              stringsAsFactors = FALSE
            )
          })
        }
      }
      ocr_processing(FALSE)
    }
  })
  
  # Force plot re-render when pdf_index changes (to show OCR overlays)
  observeEvent(rv$pdf_index, {
    req(rv$files_df)
    req(rv$pdf_index)
    # Reset OCR mode selectize to "none" when switching pages
    #updateSelectizeInput(session, "ocr_display_mode", selected = "none")
    
    plot_id <- paste0("pdf_plot_", rv$pdf_index)
    output[[plot_id]] <- renderPlot({
      # Load and prepare the image
      img_path <- rv$files_df$Path[rv$pdf_index]
      img <- magick::image_read(img_path)
      img <- img %>% magick::image_enhance()
      info <- magick::image_info(img)
      img_width <- info$width
      img_height <- info$height
      img_raster <- as.raster(img)
      
      # Set up the plot area
      par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
      plot(0, 0, type = "n", xlim = c(0, img_width), ylim = c(0, img_height),
           xlab = "", ylab = "", axes = FALSE, asp = 1)
      
      # Draw the image
      rasterImage(img_raster, 0, 0, img_width, img_height)
      
      # Show processing indicator if OCR is running
      if (ocr_processing()) {
        rect(10, 10, 300, 50, col = "black", border = NA)
        text(150, 30, "OCR Processing...", col = "white", cex = 1.5)
      }
      
      # Draw OCR overlay if in OCR mode and OCR data exists
      if (input$ocr_display_mode != "none" && !is.null(rv$ocr_text[[rv$pdf_index]])) {
        ocr_df <- rv$ocr_text[[rv$pdf_index]]
        
        # Filter by confidence threshold
        if (nrow(ocr_df) > 0) {
          ocr_df <- ocr_df[ocr_df$confidence >= input$confidence_threshold, , drop = FALSE]
        }
        
        # Draw OCR boxes or text
        if (nrow(ocr_df) > 0) {
          for (i in seq_len(nrow(ocr_df))) {
            tryCatch({
              # Parse bbox coordinates and convert to plot coordinates
              bbox <- strsplit(ocr_df$bbox[i], ",")[[1]]
              if (length(bbox) == 4) {
                coords <- as.numeric(bbox)
                
                # Handle coordinate conversion correctly
                # Tesseract coordinates: (left, top, right, bottom) with origin at top-left
                # Plot coordinates: (left, bottom, right, top) with origin at bottom-left
                x1 <- coords[1]  # left
                y1 <- img_height - coords[4]  # bottom (inverted)
                x2 <- coords[3]  # right
                y2 <- img_height - coords[2]  # top (inverted)
                
                
                # Draw rectangle and/or text based on display mode
                if (input$ocr_display_mode == "text") {
                  # Draw background for text
                  rect(x1, y1, x2, y2, 
                       col = rgb(1, 1, 1, 0.7), # semi-transparent white
                       border = "darkgray", 
                       lwd = 1)
                  
                  # Draw word on top
                  text_x <- (x1 + x2) / 2
                  text_y <- (y1 + y2) / 2
                  text(text_x, text_y, 
                       ocr_df$word[i],
                       cex = 0.9, 
                       col = "black", 
                       font = 2)
                } else if (input$ocr_display_mode == "highlight") {
                  # Draw highlight rectangle
                  rect(x1, y1, x2, y2, 
                       col = rgb(0, 0.48, 1, 0.3),  # Semi-transparent blue
                       border = rgb(0, 0.48, 1, 0.8),  # Solid blue border
                       lwd = 1)
                }
              }
            }, error = function(e) {
              # Silently ignore errors in drawing individual words
            })
          }
        } else {
          # Show message if no text meets confidence threshold
          text_width <- strwidth("No OCR text meets confidence threshold") * 1.2
          rect(img_width/2 - text_width/2, img_height/2 - 15, 
               img_width/2 + text_width/2, img_height/2 + 15,
               col = "white", border = "black")
          
          text(img_width/2, img_height/2,
               paste("No OCR text meets confidence threshold (", input$confidence_threshold, "%)"),
               cex = 1, col = "red")
        }
      }
      
      # Draw user-defined redaction rectangles
      if (!is.null(rv$rectangles[[img_path]]) && length(rv$rectangles[[img_path]]) > 0) {
        for (rect_data in rv$rectangles[[img_path]]) {
          rect(rect_data$xmin, rect_data$ymin, rect_data$xmax, rect_data$ymax,
               col = adjustcolor(rect_data$color, alpha.f = 0.3),
               border = rect_data$color,
               lwd = 2)
        }
      }
    }, res = 96)  # Increased resolution for better text rendering
  })
  
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
      ocr_df <- ocr_df[ocr_df$confidence >= input$confidence_threshold, , drop = FALSE]
    }
    
    if (nrow(ocr_df) == 0) {
      showNotification("No OCR text meets confidence threshold", type = "warning", duration = 2)
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
    brush_ymin <- img_height - brush$ymax  # Flip Y coordinates
    brush_ymax <- img_height - brush$ymin  # Flip Y coordinates
    
    # Find OCR words within brush selection
    selected_words <- character(0)
    
    for (i in seq_len(nrow(ocr_df))) {
      # Parse bbox coordinates
      coords <- as.numeric(strsplit(ocr_df$bbox[i], ",")[[1]])
      word_x1 <- coords[1]
      word_y1 <- coords[2]
      word_x2 <- coords[3]
      word_y2 <- coords[4];
      
      # Check if word overlaps with brush selection
      if (word_x2 >= brush_xmin && word_x1 <= brush_xmax &&
          word_y2 >= brush_ymin && word_y1 <= brush_ymax) {
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
      showNotification(paste("Selected:", selected_text), 
                       type = "message", duration = 6)
    } else {
      rv$selected_text <- NULL
      showNotification("No text found in selection", type = "warning", duration = 2)
    }
  })
  
  # --- Rectangle logic: modified to store by file path instead of borehole ID ---
  observeEvent(input$draw_rectangle, {
    # Make sure we have a brush selection
    if (is.null(input$pdf_brush)) {
      showNotification("Please make a selection first", type = "warning", duration = 2)
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
    rv$rectangles[[file_path]] <- append(rv$rectangles[[file_path]], list(new_rect))
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
  
  # Generalized function to create PDF with redactions
  create_pdf_with_redactions <- function(borehole_id, return_path = FALSE) {
    req(rv$files_df)
    
    # Find all rows for this borehole_id
    same_bh_rows <- which(rv$files_df$borehole_id == borehole_id)
    if (length(same_bh_rows) == 0) return(NULL)
    
    img_paths <- rv$files_df$Path[same_bh_rows]
    
    # Create temp directory and file path
    temp_dir <- file.path(tempdir(), paste0("pdf_export_", borehole_id))
    if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
    
    # Generate filename
    base_name <- tools::file_path_sans_ext(basename(rv$files_df$Name[same_bh_rows[1]]))
    if (length(same_bh_rows) > 1) {
      filename <- paste0(base_name, "_", borehole_id, "_multi_with_redactions.pdf")
    } else {
      page_num <- rv$files_df$Page[same_bh_rows[1]]
      filename <- paste0(base_name, "_page_", page_num, "_with_redactions.pdf")
    }
    
    temp_file_path <- file.path(temp_dir, filename)
    
    # Process images
    if (length(img_paths) == 1) {
      # Single page
      img <- magick::image_read(img_paths[1])
      info <- magick::image_info(img)
      img_width <- info$width
      img_height <- info$height
      
      # Get rectangles for this specific file path
      file_path <- img_paths[1]
      rectangles <- rv$rectangles[[file_path]]
      
      # Apply rectangles if there are any for this file
      if (!is.null(rectangles) && length(rectangles) > 0) {
        img <- magick::image_draw(img)
        for (rect in rectangles) {
          y_min_img <- img_height - rect$ymax
          y_max_img <- img_height - rect$ymin
          rect(
            rect$xmin, y_min_img, rect$xmax, y_max_img,
            border = "black",
            col = "black",
            lwd = 2
          )
        }
        dev.off()
      }
      magick::image_write(img, path = temp_file_path, format = "pdf")
    } else {
      # Multi-page PDF logic with file-specific redactions
      img_list <- list()
      for (i in seq_along(img_paths)) {
        file_path <- img_paths[i]
        img <- magick::image_read(file_path)
        info <- magick::image_info(img)
        img_width <- info$width
        img_height <- info$height
        
        # Get rectangles for this specific file path
        
        rectangles <- rv$rectangles[[file_path]]
        
        # Apply rectangles if there are any for this file
        if (!is.null(rectangles) && length(rectangles) > 0) {
          img <- magick::image_draw(img)
          for (rect in rectangles) {
            y_min_img <- img_height - rect$ymax
            y_max_img <- img_height - rect$ymin
            rect(
              rect$xmin, y_min_img, rect$xmax, y_max_img,
              border = "black",
              col = "black",
              lwd = 2
            )
          }
          dev.off()
        }
        img_list[[i]] <- img
      }
      # Combine all images into a single PDF
      img_joined <- do.call(magick::image_join, img_list)
      magick::image_write(img_joined, path = temp_file_path, format = "pdf")
    }
    
    # Create OCR text file if available
    ocr_words <- character(0)
    for (i in same_bh_rows) {
      if (!is.null(rv$ocr_text[[i]]) && nrow(rv$ocr_text[[i]]) > 0) {
        ocr_data <- rv$ocr_text[[i]]
        ocr_words <- c(ocr_words, ocr_data$word)
      }
    }
    if (length(ocr_words) > 0) {
      text_file <- file.path(temp_dir, paste0(tools::file_path_sans_ext(filename), "_ocr_text.txt"))
      writeLines(paste(ocr_words, collapse = " "), text_file)
    }
    
    if (return_path) {
      return(temp_file_path)
    } else {
      return(filename)
    }
  }
  
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
      showNotification("Creating PDF with redactions...", type = "message", duration = 2)
      
      temp_file_path <- create_pdf_with_redactions(borehole_id, return_path = TRUE)
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
            paste("PDF created with redactions. OCR text contains", ocr_word_count, "words."),
            type = "message", duration = 3
          )
        } else {
          showNotification("PDF created with redactions (no OCR text available)", 
                           type = "message", duration = 3)
        }
      }
    }
  )
  
  
  # Also update the rendering function to show file-specific rectangles
  observe({
    req(rv$files_df)
    req(rv$pdf_index)
    
    # Get required values
    current_ocr_mode <- input$ocr_display_mode
    current_confidence <- input$confidence_threshold
    file_path <- rv$files_df$Path[rv$pdf_index]
    # Use rectangles for this file path
    current_rectangles <- rv$rectangles[[file_path]]
    is_processing <- ocr_processing()
    plot_id <- paste0("pdf_plot_", rv$pdf_index)
    
    # Ensure that brush state is correctly reflected in UI when pdf_index changes
    isolate({
      if (brush_enabled()) {
        shinyjs::runjs("$('#brush_select').addClass('btn-active');")
      } else {
        shinyjs::runjs("$('#brush_select').removeClass('btn-active');")
      }
    })
    
    output$pdf_viewer <- renderUI({
      img_path <- isolate(rv$files_df$Path[rv$pdf_index])
      img <- magick::image_read(img_path)
      info <- magick::image_info(img)
      img_width <- info$width
      img_height <- info$height
      zoom <- input$zoom_level
      display_width <- img_width * zoom
      display_height <- img_height * zoom
      tags$div(
        style = "width: 100%; overflow: auto;",
        plotOutput(
          outputId = plot_id,
          width = paste0(display_width, "px"),
          height = paste0(display_height, "px"),
          brush = if (brush_enabled()) {
            brushOpts(
              id = "pdf_brush",
              resetOnNew = TRUE,
              direction = "xy",
              opacity = 0.3,
              fill = "#007bff"
            )
          } else {
            NULL
          }
        )
      )
    })
    
    output[[plot_id]] <- renderPlot({
      img_path <- rv$files_df$Path[rv$pdf_index]
      img <- magick::image_read(img_path)
      img <- img %>% magick::image_enhance()
      info <- magick::image_info(img)
      img_width <- info$width
      img_height <- info$height
      img_raster <- as.raster(img)
      par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
      plot(0, 0, type = "n", xlim = c(0, img_width), ylim = c(0, img_height),
           xlab = "", ylab = "", axes = FALSE, asp = 1)
      rasterImage(img_raster, 0, 0, img_width, img_height)
      if (is_processing) {
        rect(10, 10, 300, 50, col = "black", border = NA)
        text(150, 30, "OCR Processing...", col = "white", cex = 1.5)
      }
      if (current_ocr_mode != "none" && !is.null(rv$ocr_text[[rv$pdf_index]])) {
        ocr_df <- rv$ocr_text[[rv$pdf_index]]
        if (nrow(ocr_df) > 0) {
          ocr_df <- ocr_df[ocr_df$confidence >= current_confidence, , drop = FALSE]
        }
        if (nrow(ocr_df) > 0) {
          for (i in seq_len(nrow(ocr_df))) {
            tryCatch({
              coords <- as.numeric(strsplit(ocr_df$bbox[i], ",")[[1]])
              if (length(coords) == 4) {
                x1 <- coords[1]
                y1 <- img_height - coords[4]
                x2 <- coords[3]
                y2 <- img_height - coords[2]
                if (current_ocr_mode == "text") {
                  rect(x1, y1, x2, y2, col = "white", border = "black", lwd = 1)
                  text((x1 + x2) / 2, (y1 + y2) / 2, ocr_df$word[i],
                       cex = 1.2, col = "black", font = 2)
                } else {
                  rect(x1, y1, x2, y2, col = rgb(0, 0.48, 1, 0.3),
                       border = rgb(0, 0.48, 1, 0.8), lwd = 1)
                }
              }
            }, error = function(e) {})
          }
        } else if (current_ocr_mode != "none") {
          rect(img_width/2 - 200, img_height/2 - 25, img_width/2 + 200, img_height/2 + 25,
               col = "white", border = "black")
          text(img_width/2, img_height/2,
               paste("No OCR text meets confidence threshold (", current_confidence, "%)"),
               cex = 1.2, col = "red")
        }
      }
      # Draw user-defined rectangles for this specific file
      if (!is.null(current_rectangles) && length(current_rectangles) > 0) {
        for (rect in current_rectangles) {
          rect(rect$xmin, rect$ymin, rect$xmax, rect$ymax,
               col = adjustcolor(rect$color, alpha.f = 0.3),
               border = rect$color,
               lwd = 2)
        }
      }
    }, res = 50)
  })
  
  
  
  # Observer to track clicks in the right panel inputs
  observe({
    # Get all inputs with "_clicked" suffix
    all_inputs <- names(reactiveValuesToList(input))
    clicked_inputs <- all_inputs[grepl("_clicked$", all_inputs)]
  })
  
  # Observer to update input fields with selected OCR text when clicked
  observe({
    # First check if we have any selected text
    if (is.null(rv$selected_text) || length(rv$selected_text) == 0) {
      return()  # Exit early if no text is selected
    }
    
    # Combine selected text into a single string
    selected_text <- paste(rv$selected_text, collapse = " ")
    
    # Get all inputs with "_clicked" suffix - force to character to prevent NA
    all_inputs <- as.character(names(reactiveValuesToList(input)))
    clicked_inputs <- all_inputs[grepl("_clicked$", all_inputs)]
    
    if (length(clicked_inputs) == 0) {
      return()  # Exit if no click events are registered
    }
    
    # Safely get the values for clicked inputs
    clicked_values <- numeric(length(clicked_inputs))
    names(clicked_values) <- clicked_inputs
    
    for (i in seq_along(clicked_inputs)) {
      name <- clicked_inputs[i]
      val <- input[[name]]
      if (!is.null(val) && length(val) == 1 && !is.na(val) && 
          is.numeric(val)) {
        clicked_values[i] <- val
      } else {
        clicked_values[i] <- 0
      }
    }
    
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
          shinyjs::runjs(sprintf("document.getElementById('%s').blur();", 
                                 field_id))
        }
        
        # Update different field types appropriately
        if (field_name %in% c("name", "notes", "location_source")) {
          updateTextInput(session, field_name, value = selected_text)
          shinyjs::runjs(sprintf("var el=$('#%s'); if(el.length){el.addClass('flash-update'); setTimeout(function(){el.removeClass('flash-update');},1400);}", field_name))
          showNotification(paste("Updated", field_name, "with selected text"),
                           type = "message", duration = 5)
          # Blur the field
          blur_field(field_name)
        } else if (field_name %in% c("easting","northing","latitude",
                                     "longitude","depth_to_bedrock",
                                     "permafrost_top_depth","permafrost_bottom_depth",
                                     "casing_outside_diameter","drill_depth","top_of_screen",
                                     "bottom_of_screen","well_head_stick_up","static_water_level","estimated_yield")) {
          # Numeric inputs - extract numbers
          tryCatch({
            # Try to extract a number from the text
            num_pattern <- regexpr("\\d+\\.?\\d*", selected_text)
            if (!is.na(num_pattern) && num_pattern > 0) {
              num_text <- regmatches(selected_text, num_pattern)
              if (length(num_text) > 0) {
                num_value <- as.numeric(num_text[1])
                if (!is.na(num_value)) {
                  updateNumericInput(session, field_name, value = num_value)
                  shinyjs::runjs(sprintf("var el=$('#%s'); if(el.length){el.addClass('flash-update'); setTimeout(function(){el.removeClass('flash-update');},1400);}", field_name))
                  showNotification(paste("Updated", field_name, 
                                         "with value", num_value),
                                   type = "message", duration = 2)
                  # Blur the field
                  blur_field(field_name)
                }
              }
            } else {
              showNotification("No numeric value found in selected text",
                               type = "warning", duration = 2)
            }
          }, error = function(e) {
            showNotification(paste0("Error extracting numeric value: ", e$message), 
                             type = "error", duration = 4)
          })
        } else if (field_name == "date_drilled") {
          # Try to extract and parse date
          tryCatch({
            # Try multiple date patterns
            date_patterns <- c(
              "\\d{1,4}[-/]\\d{1,2}[-/]\\d{1,4}",  # yyyy-mm-dd format
              "\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}",   # dd Month yyyy
              "[A-Za-z]+\\s+\\d{1,2},?\\s+\\d{4}"  # Month dd, yyyy
            )
            
            for (pattern in date_patterns) {
              date_match <- regexpr(pattern, selected_text)
              if (!is.na(date_match) && date_match > 0) {
                date_str <- regmatches(selected_text, date_match)
                if (length(date_str) > 0) {
                  parsed_date <- as.Date(date_str[1], 
                                         format = "%Y-%m-%d")
                  if (is.na(parsed_date)) {
                    # Try other common formats
                    parsed_date <- as.Date(date_str[1], 
                                           format = "%m/%d/%Y")
                  }
                  if (is.na(parsed_date)) {
                    parsed_date <- as.Date(date_str[1], 
                                           format = "%d %B %Y")
                  }
                  if (is.na(parsed_date)) {
                    parsed_date <- as.Date(date_str[1], 
                                           format = "%B %d, %Y")
                  }
                  
                  if (!is.na(parsed_date)) {
                    updateDateInput(session, "date_drilled", value = parsed_date)
                    shinyjs::runjs("var el=$('#date_drilled'); if(el.length){el.addClass('flash-update'); setTimeout(function(){el.removeClass('flash-update');},1400);}")
                    showNotification(paste("Updated date to", 
                                           format(parsed_date, "%Y-%m-%d")),
                                     type = "message", duration = 2)
                    # Date fields have complex structure, blur the input part
                    shinyjs::runjs(
                      "document.querySelector('#date_drilled input').blur();"
                    )
                    break  # Exit the loop once we've found a valid date
                  }
                }
              }
            }
          }, error = function(e) {
            showNotification(paste0("Error parsing date ", e$message), type = "error", 
                             duration = 5)
          })
        } else {
          # If we reach here, we didn't handle the field type
          cat("Unhandled field type:", field_name, "\n")
        }
        
        # Clear selected text after using it
        rv$selected_text <- NULL
        
        # For selectize inputs which need special handling (if any exist)
        if (field_name %in% c("drilled_by", "utm_zone", "purpose_of_well")) {
          shinyjs::runjs(sprintf("$('#%s-selectized').blur();", field_name))
        }
        
        # Clear any brush selection
        if (brush_enabled()) {
          # This will remove the visual brush selection
          shinyjs::runjs("Shiny.setInputValue('pdf_brush-clear', Math.random());")
        }
      }
    }
  })
  
  # Comprehensive observer to store all input values in metadata for the current well
  observe({
    # Don't update metadata when we're loading
    if (loading_metadata()) return()
    
    req(rv$files_df)
    req(rv$pdf_index)
    req(nrow(rv$files_df) >= rv$pdf_index)
    
    well_id <- current_well_id()
    
    if (!is.null(well_id) && well_id %in% names(rv$well_data)) {
      # Update metadata with current input values for the correct well
      rv$well_data[[well_id]]$metadata <- list(
        borehole_id = well_id,
        name = input$name,
        notes = input$notes,  # <-- add notes field
        coordinate_system = input$coordinate_system,
        easting = input$easting,
        northing = input$northing,
        utm_zone = input$utm_zone,
        latitude = input$latitude,
        longitude = input$longitude,
        location_source = input$location_source,
        purpose_of_well = input$purpose_of_well,
        depth_to_bedrock = input$depth_to_bedrock,
        depth_to_bedrock_unit = input$depth_to_bedrock_unit,
        permafrost_present = input$permafrost_present,
        permafrost_top_depth = input$permafrost_top_depth,
        permafrost_top_depth_unit = input$permafrost_top_depth_unit,
        permafrost_bottom_depth = input$permafrost_bottom_depth,
        permafrost_bottom_depth_unit = input$permafrost_bottom_depth_unit,
        date_drilled = input$date_drilled,
        casing_outside_diameter = input$casing_outside_diameter,
        casing_outside_diameter_unit = input$casing_outside_diameter_unit,
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
        surveyed_ground_level_elevation = input$surveyed_ground_level_elevation,
        surveyed_ground_level_elevation_unit = input$surveyed_ground_level_elevation_unit,
        is_well = input$is_well,
        drilled_by = input$drilled_by # <-- add drilled_by field
      )
    }
  })
  
  # Metadata loader - fix the loading to properly update all inputs including notes
  observeEvent(rv$pdf_index, {
    req(rv$files_df)
    req(rv$pdf_index)
    req(nrow(rv$files_df) >= rv$pdf_index)
    
    well_id <- current_well_id()
    
    if (!is.null(well_id) && well_id %in% names(rv$well_data)) {
      loading_metadata(TRUE)
      metadata <- rv$well_data[[well_id]]$metadata
      
      # Helper function to safely get metadata values
      get_meta_value <- function(field, default = "") {
        val <- metadata[[field]]
        if (is.null(val) || identical(val, NA) || is.na(val)) return(default)
        return(val)
      }
      
      get_meta_numeric <- function(field) {
        val <- metadata[[field]]
        if (is.null(val) || identical(val, NA) || is.na(val)) return(NULL)
        return(val)
      }
      
      get_meta_date <- function(field) {
        val <- metadata[[field]]
        if (is.null(val) || identical(val, NA) || is.na(val)) return(NULL)
        return(val)
      }
      
      get_meta_boolean <- function(field, default = FALSE) {
        val <- metadata[[field]]
        if (is.null(val) || identical(val, NA) || is.na(val)) return(default)
        return(as.logical(val))
      }
      
      # Update text inputs - make sure notes is included
      updateTextInput(session, "name", value = get_meta_value("name"))
      updateTextInput(session, "notes", value = get_meta_value("notes"))
      updateTextInput(session, "location_source", value = get_meta_value("location_source"))
      
      # Update selectize inputs
      updateSelectizeInput(session, "utm_zone", selected = get_meta_value("utm_zone", "8N"))
      updateSelectizeInput(session, "purpose_of_well", selected = get_meta_value("purpose_of_well"))
      updateSelectizeInput(session, "drilled_by", selected = get_meta_value("drilled_by"))
      
      # Update radio buttons
      updateRadioButtons(session, "coordinate_system", selected = get_meta_value("coordinate_system", "utm"))
      updateRadioButtons(session, "depth_to_bedrock_unit", selected = get_meta_value("depth_to_bedrock_unit", "ft"))
      updateRadioButtons(session, "casing_outside_diameter_unit", selected = get_meta_value("casing_outside_diameter_unit", "inch"))
      updateRadioButtons(session, "drill_depth_unit", selected = get_meta_value("drill_depth_unit", "ft"))
      updateRadioButtons(session, "top_of_screen_unit", selected = get_meta_value("top_of_screen_unit", "ft"))
      updateRadioButtons(session, "bottom_of_screen_unit", selected = get_meta_value("bottom_of_screen_unit", "ft"))
      updateRadioButtons(session, "well_head_stick_up_unit", selected = get_meta_value("well_head_stick_up_unit", "ft"))
      updateRadioButtons(session, "static_water_level_unit", selected = get_meta_value("static_water_level_unit", "ft"))
      updateRadioButtons(session, "estimated_yield_unit", selected = get_meta_value("estimated_yield_unit", "G/min"))
      updateRadioButtons(session, "surveyed_ground_level_elevation_unit", selected = get_meta_value("surveyed_ground_level_elevation_unit", "ft"))
      updateRadioButtons(session, "permafrost_top_depth_unit", selected = get_meta_value("permafrost_top_depth_unit", "ft"))
      updateRadioButtons(session, "permafrost_bottom_depth_unit", selected = get_meta_value("permafrost_bottom_depth_unit", "ft"))
      
      # Update numeric inputs
      updateNumericInput(session, "easting", value = get_meta_numeric("easting"))
      updateNumericInput(session, "northing", value = get_meta_numeric("northing"))
      updateNumericInput(session, "latitude", value = get_meta_numeric("latitude"))
      updateNumericInput(session, "longitude", value = get_meta_numeric("longitude"))
      updateNumericInput(session, "depth_to_bedrock", value = get_meta_numeric("depth_to_bedrock"))
      updateNumericInput(session, "permafrost_top_depth", value = get_meta_numeric("permafrost_top_depth"))
      updateNumericInput(session, "permafrost_bottom_depth", value = get_meta_numeric("permafrost_bottom_depth"))
      updateNumericInput(session, "casing_outside_diameter", value = get_meta_numeric("casing_outside_diameter"))
      updateNumericInput(session, "drill_depth", value = get_meta_numeric("drill_depth"))
      updateNumericInput(session, "surveyed_ground_level_elevation", value = get_meta_numeric("surveyed_ground_level_elevation"))
      updateNumericInput(session, "top_of_screen", value = get_meta_numeric("top_of_screen"))
      updateNumericInput(session, "bottom_of_screen", value = get_meta_numeric("bottom_of_screen"))
      updateNumericInput(session, "well_head_stick_up", value = get_meta_numeric("well_head_stick_up"))
      updateNumericInput(session, "static_water_level", value = get_meta_numeric("static_water_level"))
      updateNumericInput(session, "estimated_yield", value = get_meta_numeric("estimated_yield"))
      
      # Update checkbox inputs
      updateCheckboxInput(session, "permafrost_present", value = get_meta_boolean("permafrost_present"))
      updateCheckboxInput(session, "is_well", value = get_meta_boolean("is_well"))
      
      # Update date input
      updateDateInput(session, "date_drilled", value = get_meta_date("date_drilled"))
      
      # Re-enable metadata saving after all updates are complete
      loading_metadata(FALSE)
    } else {
      # If no metadata exists, clear all fields including notes
      loading_metadata(TRUE)
      
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "notes", value = "")
      updateTextInput(session, "location_source", value = "")
      updateSelectizeInput(session, "utm_zone", selected = "8N")
      updateSelectizeInput(session, "purpose_of_well", selected = NULL)
      updateSelectizeInput(session, "drilled_by", selected = NULL)
      updateRadioButtons(session, "coordinate_system", selected = "utm")
      updateRadioButtons(session, "depth_to_bedrock_unit", selected = "ft")
      updateRadioButtons(session, "casing_outside_diameter_unit", selected = "inch")
      updateRadioButtons(session, "drill_depth_unit", selected = "ft")
      updateRadioButtons(session, "estimated_yield_unit", selected = "G/min")
      updateRadioButtons(session, "surveyed_ground_level_elevation_unit", selected = "ft")
      
      # Clear all numeric inputs
      for (field in c("easting", "northing", "latitude", "longitude", "depth_to_bedrock",
                      "permafrost_top_depth", "permafrost_bottom_depth", "casing_outside_diameter",
                      "drill_depth", "surveyed_ground_level_elevation", "top_of_screen",
                      "bottom_of_screen", "well_head_stick_up", "static_water_level", "estimated_yield")) {
        updateNumericInput(session, field, value = NULL)
      }
      
      # Clear checkboxes and date
      updateCheckboxInput(session, "permafrost_present", value = FALSE)
      updateCheckboxInput(session, "is_well", value = FALSE)
      updateDateInput(session, "date_drilled", value = NULL)
      
      loading_metadata(FALSE)
    }
  })
  
  # Add click handler for permafrost inputs
  observeEvent(input$permafrost_present, {
    if (input$permafrost_present) {
      showNotification("Permafrost fields enabled", type = "message", duration = 2)
    }
  })
  
  # Add notification for 'is_well' input checked
  observeEvent(input$is_well, {
    if (isTRUE(input$is_well)) {
      showNotification("Well construction fields enabled", type = "message", duration = 2)
    }
  })
  
  # Add renderText outputs for borehole ID and file count displays
  output$borehole_id_display <- renderText({
    req(rv$files_df)
    req(rv$pdf_index)
    
    # Return the current borehole ID
    return(rv$files_df$borehole_id[rv$pdf_index])
  })
  
  output$file_count_display <- renderText({
    req(rv$files_df)
    req(rv$pdf_index)
    
    # Get current file and borehole information
    current_borehole_id <- rv$files_df$borehole_id[rv$pdf_index]
    current_file_name <- rv$files_df$Name[rv$pdf_index]
    current_page <- rv$files_df$Page[rv$pdf_index]
    
    # Count how many total pages this file has
    same_file_rows <- which(rv$files_df$Name == current_file_name)
    total_pages_in_file <- length(same_file_rows)
    
    # Build informative display text
    if (total_pages_in_file > 1) {
      return(sprintf("Page %d of %d from file: %s", 
                     current_page, total_pages_in_file, current_file_name))
    } else {
      # For single page files, show simpler message
      return(sprintf("Single page file: %s", current_file_name))
    }
  })
  
  # Update the borehole_id_selector choices whenever files_df changes or pdf_index changes
  observe({
    req(rv$files_df)
    req(rv$pdf_index)
    
    # Get current borehole ID
    current_id <- rv$files_df$borehole_id[rv$pdf_index]
    
    # Get all available borehole IDs except the current one
    all_borehole_ids <- rv$files_df$borehole_id
    borehole_choices <- setdiff(all_borehole_ids, current_id)
    
    # Create named list for dropdown with descriptive labels
    names(borehole_choices) <- sapply(borehole_choices, function(id) {
      idx <- which(rv$files_df$borehole_id == id)
      if (length(idx) > 0) {
        idx <- idx[1]  # Use first instance if multiple matches
        file_name <- rv$files_df$Name[idx]
        page_num <- rv$files_df$Page[idx]
        return(paste0(id, " - ", file_name, " (Page ", page_num, ")"))
      } else {
        return(id)
      }
    })
    
    # Update the dropdown with all other borehole IDs
    updateSelectizeInput(session, "borehole_id_selector", 
                         choices = as.list(borehole_choices),
                         selected = "",
                         options = list(
                           placeholder = "Select borehole to link to",
                           maxItems = 1
                         ))
  })
  
  # Handle linking to another borehole when selection is made
  observeEvent(input$borehole_id_selector, {
    req(input$borehole_id_selector != "")
    req(rv$files_df)
    req(rv$pdf_index)
    
    # Get the selected borehole ID
    target_borehole_id <- input$borehole_id_selector
    
    # Get the current borehole ID
    current_borehole_id <- rv$files_df$borehole_id[rv$pdf_index]
    
    # Make sure we're not linking to the same ID
    if (target_borehole_id != current_borehole_id) {
      # Show confirmation message
      showNotification(
        paste("Linking current page to", target_borehole_id), 
        type = "message", 
        duration = 3
      )
      
      # Keep track of current metadata before changing borehole ID
      current_metadata <- NULL
      if (current_borehole_id %in% names(rv$well_data)) {
        current_metadata <- rv$well_data[[current_borehole_id]]$metadata
      }
      
      # Update the borehole_id in files_df
      rv$files_df$borehole_id[rv$pdf_index] <- target_borehole_id
      
      # If target borehole doesn't exist in well_data, create it
      if (!target_borehole_id %in% names(rv$well_data)) {
        # Initialize with metadata from current borehole (if available)
        rv$well_data[[target_borehole_id]] <- list(
          files = list(),
          metadata = current_metadata
        )
      }
      
      # Add current file to target borehole's files
      current_file <- rv$files_df$NewFilename[rv$pdf_index]
      if (!is.null(rv$well_data[[target_borehole_id]]$files)) {
        if (is.list(rv$well_data[[target_borehole_id]]$files)) {
          rv$well_data[[target_borehole_id]]$files <- c(rv$well_data[[target_borehole_id]]$files, current_file)
        } else {
          rv$well_data[[target_borehole_id]]$files <- c(rv$well_data[[target_borehole_id]]$files, current_file)
        }
      } else {
        rv$well_data[[target_borehole_id]]$files <- current_file
      }
      
      # Refresh the data table to show the updated borehole ID
      DT::dataTableProxy("pdf_table") %>% 
        DT::replaceData(rv$files_df[, c("tag", "borehole_id")])
      
      # Clear the dropdown selection
      updateSelectizeInput(session, "borehole_id_selector", selected = "")
      
      # Show success message
      showNotification(
        paste("Successfully linked to", target_borehole_id), 
        type = "message", 
        duration = 3
      )
      
      # Force metadata update for the new borehole ID
      loading_metadata(FALSE)
    }
  })
  
  
  # Upload handlers
  observeEvent(input$upload_selected, {
    req(rv$files_df)
    req(rv$pdf_index)
    
    # Get the current well ID
    current_well_id <- current_well_id()
    
    if (!is.null(current_well_id) && current_well_id %in% names(rv$well_data)) {
      metadata <- rv$well_data[[current_well_id]]$metadata
      
      # Show processing notification
      showNotification("Uploading selected borehole...", type = "message", duration = 3)
      
      tryCatch({
        # Create PDF with redactions for this borehole
        pdf_file_path <- create_pdf_with_redactions(current_well_id, return_path = TRUE)
        
        # Call AquaCache function with the metadata
        result <- AquaCache::insertACBorehole(
          con = session$userData$AquaCache,
          path = pdf_file_path,
          well_name = metadata$name,
          latitude = metadata$latitude,
          longitude = metadata$longitude,
          location_source = metadata$location_source,
          surveyed_ground_level_elevation = metadata$surveyed_ground_level_elevation,
          purpose_of_well = metadata$purpose_of_well,
          depth_to_bedrock = metadata$depth_to_bedrock,
          permafrost_present = metadata$permafrost_present,
          permafrost_top_depth = metadata$permafrost_top_depth,
          permafrost_bottom_depth = metadata$permafrost_bottom_depth,
          date_drilled = metadata$date_drilled,
          casing_outside_diameter = metadata$casing_outside_diameter,
          is_well = metadata$is_well,
          well_depth = metadata$drill_depth,
          top_of_screen = metadata$top_of_screen,
          bottom_of_screen = metadata$bottom_of_screen,
          well_head_stick_up = metadata$well_head_stick_up,
          static_water_level = metadata$static_water_level,
          estimated_yield = metadata$estimated_yield,
          ground_elev_m = metadata$surveyed_ground_level_elevation,
          notes = metadata$notes,
          share_with = "yg_reader",
          drilled_by = metadata$drilled_by,
          drill_method = NULL,
          pdf_file_path = pdf_file_path
        )
        
        showNotification(paste("Successfully uploaded borehole:", current_well_id), 
                         type = "message", duration = 5)
        
      }, error = function(e) {
        showNotification(paste("Error uploading borehole:", e$message), 
                         type = "error", duration = 5)
      })
    } else {
      showNotification("No valid borehole data to upload", type = "warning", duration = 3)
    }
  })
  
  observeEvent(input$upload_all, {
    req(rv$files_df)
    req(rv$well_data)
    
    # Count total boreholes to upload
    total_boreholes <- length(unique(rv$files_df$borehole_id))
    
    if (total_boreholes == 0) {
      showNotification("No boreholes to upload", type = "warning", duration = 3)
      return()
    }
    
    # Show processing notification
    showNotification(paste("Starting upload of", total_boreholes, "boreholes..."), 
                     type = "message", duration = 3)
    
    # Track success and errors
    success_count <- 0
    error_count <- 0
    
    # Loop through each unique borehole ID
    unique_borehole_ids <- unique(rv$files_df$borehole_id)
    
    for (well_id in unique_borehole_ids) {
      if (well_id %in% names(rv$well_data)) {
        metadata <- rv$well_data[[well_id]]$metadata
        
        tryCatch({
          # Create PDF with redactions for this borehole
          pdf_file_path <- create_pdf_with_redactions(well_id, return_path = TRUE)
          
          # Call AquaCache function with the metadata
          result <- AquaCache::insertACBorehole(
            well_name = metadata$name,
            latitude = metadata$latitude,
            longitude = metadata$longitude,
            location_source = metadata$location_source,
            surveyed_ground_level_elevation = metadata$surveyed_ground_level_elevation,
            purpose_of_well = metadata$purpose_of_well,
            depth_to_bedrock = metadata$depth_to_bedrock,
            permafrost_present = metadata$permafrost_present,
            permafrost_top_depth = metadata$permafrost_top_depth,
            permafrost_bottom_depth = metadata$permafrost_bottom_depth,
            date_drilled = metadata$date_drilled,
            casing_outside_diameter = metadata$casing_outside_diameter,
            is_well = metadata$is_well,
            well_depth = metadata$drill_depth,
            top_of_screen = metadata$top_of_screen,
            bottom_of_screen = metadata$bottom_of_screen,
            well_head_stick_up = metadata$well_head_stick_up,
            static_water_level = metadata$static_water_level,
            estimated_yield = metadata$estimated_yield,
            ground_elev_m = metadata$surveyed_ground_level_elevation,
            notes = metadata$notes,
            con = session$userData$AquaCache,
            share_with = "yg_reader",
            drilled_by = metadata$drilled_by,
            drill_method = NULL,
            pdf_file_path = pdf_file_path
          )
          
          success_count <- success_count + 1
          
          # Show progress notification
          showNotification(paste("Uploaded", success_count, "of", total_boreholes, "boreholes"), 
                           type = "message", duration = 1)
          
        }, error = function(e) {
          error_count <- error_count + 1
          showNotification(paste0("Error uploading borehole", well_id, ":", e$message, "\n", type = "error", duration = 5))
        })
      }
    }
    
    # Show final summary
    if (error_count == 0) {
      showNotification(paste("Successfully uploaded all", success_count, "boreholes!"), 
                       type = "message", duration = 5)
    } else {
      showNotification(paste("Uploaded", success_count, "boreholes with", error_count, "errors"), 
                       type = "warning", duration = 5)
    }
  })
  
  # Add observer for OCR extracted text display
  output$ocr_text_display <- renderText({
    req(rv$files_df)
    req(rv$pdf_index)
    # Show selected text if available
    if (!is.null(rv$selected_text) && length(rv$selected_text) > 0) {
      return(paste(rv$selected_text, collapse = " "))
    }
    # Only show text if OCR mode is not "none"
    if (is.null(input$ocr_display_mode) || input$ocr_display_mode == "none") return("")
    ocr_df <- rv$ocr_text[[rv$pdf_index]]
    if (is.null(ocr_df) || nrow(ocr_df) == 0) return("")
    # Filter by confidence threshold
    conf <- input$confidence_threshold %||% 0
    ocr_df <- ocr_df[ocr_df$confidence >= conf, , drop = FALSE]
    if (nrow(ocr_df) == 0) return("(no OCR text above threshold)")
    # Group words into lines
    lines <- tryCatch(concat_ocr_words_by_row(ocr_df), error = function(e) ocr_df$word)
    if (length(lines) == 0) return("")
    txt <- paste(lines, collapse = "\n")
    # Truncate long output
    if (nchar(txt) > 4000) txt <- paste0(substr(txt, 1, 4000), "... (truncated)")
    txt
  })
}

shinyApp(ui, server)
