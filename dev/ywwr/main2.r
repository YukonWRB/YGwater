library(shiny)
library(DT)
library(pdftools)
library(leaflet)
library(plotly)
library(magick)
library(shinyjs)
library(tesseract)
library(ggplot2)




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
    
    # Create filter condition
    keep_word <- rep(TRUE, nrow(ocr_df))
    
    for (pattern in noise_patterns) {
        keep_word <- keep_word & !grepl(pattern, ocr_df$word, perl = TRUE)
    }
    
    # Additional filters
    # Remove very short words that aren't meaningful single characters
    short_and_meaningless <- nchar(ocr_df$word) == 1 & 
                             !ocr_df$word %in% meaningful_single_chars &
                             !grepl("^[0-9]$", ocr_df$word)  # Keep single digits
    
    keep_word <- keep_word & !short_and_meaningless
    
    # Remove words with very low confidence that are also short
    low_conf_short <- ocr_df$confidence < 30 & nchar(ocr_df$word) <= 2
    keep_word <- keep_word & !low_conf_short
    
    # Filter the dataframe
    filtered_df <- ocr_df[keep_word, ]
    
    # Optional: Print summary of filtering
    if (nrow(ocr_df) > 0) {
        removed_count <- nrow(ocr_df) - nrow(filtered_df)
        if (removed_count > 0) {
            cat("OCR Filter: Removed", removed_count, "noise words out of", nrow(ocr_df), "total words\n")
        }
    }
    
    return(filtered_df)
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
        showNotification(paste("Processing OCR for", pages_to_process, "pages. This may take a moment..."), 
                       type = "warning", duration = 5)
        
        # Loop through all images in files_df and run OCR
        for (i in seq_len(nrow(files_df))) {
            if (is.null(ocr_text_list[[i]])) {
                # Show progress for current page
                showNotification(paste("Processing page", i, "of", total_pages), 
                               type = "message", duration = 1)
                
                img_path <- files_df$Path[i]
                img <- magick::image_read(img_path)

                # Perform OCR on the image
                ocr_result <- tesseract::ocr_data(img, engine = tesseract::tesseract(options = list(tessedit_create_hocr = 1)))

                # Filter out words with confidence less than 50%
                #ocr_result <- ocr_result[which(ocr_result$confidence >= 50), ]
                
                # Filter out common OCR noise and error words
                ocr_result <- filter_ocr_noise(ocr_result)

                # Store the OCR text in the reactive value
                ocr_text_list[[i]] <- ocr_result
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

ui <- fluidPage(
    useShinyjs(),
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
                padding: 15px;
            }
            .resize-handle {
                width: 5px;
                background: #dee2e6;
                cursor: col-resize;
                position: absolute;
                right: 0;
                top: 0;
                bottom: 0;
                z-index: 10;
            }
            .resize-handle-right {
                width: 5px;
                background: #dee2e6;
                cursor: col-resize;
                position: absolute;
                left: 0;
                top: 0;
                bottom: 0;
                z-index: 10;
            }
            .resize-handle:hover, .resize-handle-right:hover {
                background: #007bff;
            }
            .main-panel {
                flex: 1;
                padding-left: 15px;
                min-width: 0; /* Allow panel to shrink below content width */
                overflow: auto; /* Allow scrolling if needed */
                background: #fafafa;
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
        "))
    ),
    titlePanel("Simpler Index", windowTitle = "Simpler Index"),
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
            dataTableOutput("pdf_table")
        ),
        div(class = "main-panel",
            div(class = "control-row",
                div(class = "control-group",
                    actionButton("brush_select", "Select text", class = "btn-toggle"),
                    selectizeInput("ocr_display_mode", "OCR Display Mode:",
                        choices = list(
                            "None" = "none",
                            "Highlight Boxes" = "highlight", 
                            "Text Overlay" = "text"
                        ),
                        selected = "none",
                        options = list(
                            placeholder = "Select display mode",
                            maxItems = 1
                        )
                    ),
                    sliderInput("confidence_threshold", "OCR Confidence:",
                        min = 40, max = 100, value = 70, step = 10,
                        width = "150px"
                    ),
                    # OCR Text Display - now permanently visible
                    div(
                        style = "margin-left: 20px; width: 300px;",
                        h6("Extracted Text:", style = "margin-bottom: 5px; color: #495057;"),
                        div(
                            style = "max-height: 120px; overflow-y: auto; border: 1px solid #ccc; padding: 8px; background: white; font-family: monospace; font-size: 11px; font-weight: bold; color: #007bff;",
                            verbatimTextOutput("ocr_text_display")
                        )
                    )
                ),
                div(class = "control-group", style = "margin-left: auto;",
                    actionButton("zoom_out", "-", class = "zoom-btn btn-secondary", title = "Zoom Out"),
                    actionButton("zoom_reset", "⌂", class = "reset-btn btn-secondary", title = "Reset View"),
                    actionButton("zoom_in", "+", class = "zoom-btn btn-secondary", title = "Zoom In"),
                    actionButton("zoom_fit", "⬌", class = "zoom-btn btn-secondary", title = "Fit to Width")
                )
            ),
            div(
                id = "pdf-container",
                style = "width:100%; max-width:100%; height:calc(100vh - 300px); min-height:500px; border:1px solid #ccc; margin:10px auto; overflow:auto; background:white; position:relative; display:flex; justify-content:center; align-items:flex-start; padding:20px;",
                uiOutput("pdf_viewer")
            )
        ),
        div(class = "right-panel", id = "right-sidebar",
            div(class = "resize-handle-right", id = "resize-handle-right"),
            h4("Well Information"),
            br(),
            
            # Well identification
            textInput("borehole_id", "Borehole ID:", placeholder = "Enter borehole ID"),
            textInput("well_name", "Well Name:", placeholder = "Enter well name"),
            selectizeInput("community", "Community:",
                choices = list(
                    "Whitehorse" = "whitehorse",
                    "Dawson City" = "dawson_city",
                    "Watson Lake" = "watson_lake",
                    "Haines Junction" = "haines_junction",
                    "Mayo" = "mayo",
                    "Carmacks" = "carmacks",
                    "Faro" = "faro",
                    "Teslin" = "teslin",
                    "Other" = "other"
                ),
                selected = NULL,
                options = list(
                    placeholder = "Select community",
                    maxItems = 1
                )
            ),
            
            # Location information
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
            textInput("surveyed_location_top_casing", "Surveyed Location of Top of Casing:", placeholder = "Enter surveyed location"),
            
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
                column(4, radioButtons("depth_to_bedrock_unit", "Unit:", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            dateInput("date_drilled", "Date Drilled:", value = NULL),
            fluidRow(
                column(8, numericInput("casing_outside_diameter", "Casing Outside Diameter:", value = NULL, min = 0, step = 1)),
                column(4, radioButtons("casing_outside_diameter_unit", "Unit:", choices = list("mm" = "mm", "inch" = "inch"), selected = "mm", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("well_depth", "Well Depth:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("well_depth_unit", "Unit:", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("top_of_screen", "Top of Screen:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("top_of_screen_unit", "Unit:", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("bottom_of_screen", "Bottom of Screen:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("bottom_of_screen_unit", "Unit:", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("well_head_stick_up", "Well Head Stick Up:", value = NULL, step = 0.01)),
                column(4, radioButtons("well_head_stick_up_unit", "Unit:", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("static_water_level", "Static Water Level:", value = NULL, step = 0.01)),
                column(4, radioButtons("static_water_level_unit", "Unit:", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("estimated_yield", "Estimated Yield:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("estimated_yield_unit", "Unit:", choices = list("L/min" = "L/min", "G/min" = "G/min"), selected = "L/min", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("surveyed_ground_level_elevation", "Surveyed Ground Level Elevation:", value = NULL, step = 0.01)),
                column(4, radioButtons("surveyed_ground_level_elevation_unit", "Unit:", choices = list("m" = "m", "ft" = "m"), selected = "m", inline = TRUE))
            ),
            br(),
            
            # Action buttons
            fluidRow(
                column(6, actionButton("save_metadata", "Save Info", class = "btn-primary btn-block")),
                column(6, actionButton("export_data", "Export", class = "btn-success btn-block"))
            )
        )
    ),
    
    tags$script(HTML("
        $(document).ready(function() {
            let isResizing = false;
            let isResizingRight = false;
            let startX = 0;
            let startWidth = 0;
            let zoomLevel = 1;
            let panX = 0;
            let panY = 0;
            
            // Left panel resize
            $('#resize-handle').mousedown(function(e) {
                isResizing = true;
                startX = e.clientX;
                startWidth = $('#sidebar').width();
                $('body').css('user-select', 'none');
                e.preventDefault();
            });
            
            // Right panel resize
            $('#resize-handle-right').mousedown(function(e) {
                isResizingRight = true;
                startX = e.clientX;
                startWidth = $('#right-sidebar').width();
                $('body').css('user-select', 'none');
                e.preventDefault();
            });
            
            $(document).mousemove(function(e) {
                if (isResizing) {
                    let newWidth = startWidth + (e.clientX - startX);
                    newWidth = Math.max(250, Math.min(600, newWidth));
                    $('#sidebar').css('width', newWidth + 'px');
                } else if (isResizingRight) {
                    let newWidth = startWidth - (e.clientX - startX);
                    newWidth = Math.max(200, Math.min(800, newWidth));
                    $('#right-sidebar').css('width', newWidth + 'px');
                }
            });
            
            $(document).mouseup(function() {
                if (isResizing || isResizingRight) {
                    isResizing = false;
                    isResizingRight = false;
                    $('body').css('user-select', '');
                }
            });
            
            // Mouse wheel zoom functionality
            function setupZoom() {
                // Remove any existing event handlers first
                $('#zoom_out, #zoom_in, #zoom_reset, #zoom_fit').off('click');
                $('#pdf-container').off('wheel mousedown');
                $(document).off('mousemove.pdfpan mouseup.pdfpan');
                
                // Zoom button functionality
                $('#zoom_out').on('click', function() {
                    const plot = $('#pdf-container').find('div[id*=\"pdf_plot_\"]');
                    if (plot.length === 0) return;
                    
                    const newZoom = Math.max(0.1, zoomLevel * 0.8);
                    if (newZoom !== zoomLevel) {
                        zoomLevel = newZoom;
                        plot.css({
                            'transform': 'scale(' + zoomLevel + ') translate(' + panX + 'px, ' + panY + 'px)',
                            'transform-origin': '0 0'
                        });
                    }
                });
                
                $('#zoom_in').on('click', function() {
                    const plot = $('#pdf-container').find('div[id*=\"pdf_plot_\"]');
                    if (plot.length === 0) return;
                    
                    const newZoom = Math.min(5, zoomLevel * 1.25);
                    if (newZoom !== zoomLevel) {
                        zoomLevel = newZoom;
                        plot.css({
                            'transform': 'scale(' + zoomLevel + ') translate(' + panX + 'px, ' + panY + 'px)',
                            'transform-origin': '0 0'
                        });
                    }
                });
                
                $('#zoom_reset').on('click', function() {
                    const plot = $('#pdf-container').find('div[id*=\"pdf_plot_\"]');
                    if (plot.length === 0) return;
                    
                    zoomLevel = 1;
                    panX = 0;
                    panY = 0;
                    plot.css({
                        'transform': 'scale(1) translate(0px, 0px)',
                        'transform-origin': '0 0'
                    });
                });
                
                $('#zoom_fit').on('click', function() {
                    const plot = $('#pdf-container').find('div[id*=\"pdf_plot_\"]');
                    const container = $('#pdf-container')[0];
                    if (plot.length === 0 || !container) return;
                    
                    const containerWidth = container.clientWidth;
                    const fitZoom = Math.min(1.0, (containerWidth - 20) / 595);
                    
                    zoomLevel = fitZoom;
                    panX = 0;
                    panY = 0;
                    plot.css({
                        'transform': 'scale(' + zoomLevel + ') translate(0px, 0px)',
                        'transform-origin': '0 0'
                    });
                });
                
                // Mouse wheel zoom
                $('#pdf-container').on('wheel', function(e) {
                    e.preventDefault();
                    
                    const plot = $(this).find('div[id*=\"pdf_plot_\"]');
                    if (plot.length === 0) return;
                    
                    const delta = e.originalEvent.deltaY;
                    const zoomFactor = delta > 0 ? 0.9 : 1.1;
                    
                    const rect = this.getBoundingClientRect();
                    const mouseX = e.originalEvent.clientX - rect.left;
                    const mouseY = e.originalEvent.clientY - rect.top;
                    
                    const newZoom = Math.max(0.1, Math.min(5, zoomLevel * zoomFactor));
                    
                    if (newZoom !== zoomLevel) {
                        const plotMouseX = (mouseX - panX) / zoomLevel;
                        const plotMouseY = (mouseY - panY) / zoomLevel;
                        
                        zoomLevel = newZoom;
                        
                        panX = mouseX - plotMouseX * zoomLevel;
                        panY = mouseY - plotMouseY * zoomLevel;
                        
                        plot.css({
                            'transform': 'scale(' + zoomLevel + ') translate(' + panX + 'px, ' + panY + 'px)',
                            'transform-origin': '0 0'
                        });
                    }
                });
                
                // Middle mouse button pan functionality
                let isPanning = false;
                let lastPanX = 0;
                let lastPanY = 0;
                
                $('#pdf-container').on('mousedown', function(e) {
                    // Check if middle mouse button (button 1) is pressed
                    if (e.originalEvent.button === 1) {
                        e.preventDefault();
                        isPanning = true;
                        lastPanX = e.originalEvent.clientX;
                        lastPanY = e.originalEvent.clientY;
                        $(this).css('cursor', 'grabbing');
                    }
                });
                
                $(document).on('mousemove.pdfpan', function(e) {
                    if (isPanning) {
                        e.preventDefault();
                        
                        const plot = $('#pdf-container').find('div[id*=\"pdf_plot_\"]');
                        if (plot.length === 0) return;
                        
                        const deltaX = e.originalEvent.clientX - lastPanX;
                        const deltaY = e.originalEvent.clientY - lastPanY;
                        
                        panX += deltaX;
                        panY += deltaY;
                        
                        plot.css({
                            'transform': 'scale(' + zoomLevel + ') translate(' + panX + 'px, ' + panY + 'px)',
                            'transform-origin': '0 0'
                        });
                        
                        lastPanX = e.originalEvent.clientX;
                        lastPanY = e.originalEvent.clientY;
                    }
                });
                
                $(document).on('mouseup.pdfpan', function(e) {
                    if (isPanning && e.originalEvent.button === 1) {
                        isPanning = false;
                        $('#pdf-container').css('cursor', 'default');
                    }
                });
            }
            
            // Setup zoom when page loads
            setupZoom();
            
            // Add click handler for borehole ID input
            $('#borehole_id').on('focus click', function() {
                Shiny.setInputValue('borehole_id_clicked', Math.random());
            });
            
            // Re-setup zoom when new PDF is loaded
            $(document).on('shiny:value', function(event) {
                if (event.name === 'pdf_viewer') {
                    setTimeout(function() {
                        setupZoom();
                        // Reset zoom when new PDF loads
                        zoomLevel = 1;
                        panX = 0;
                        panY = 0;
                    }, 100);
                }
            });
            
            // Also setup zoom when plots are re-rendered
            $(document).on('shiny:outputinvalidated', function(event) {
                if (event.name && event.name.indexOf('pdf_plot_') === 0) {
                    setTimeout(function() {
                        setupZoom();
                    }, 50);
                }
            });
            
            // Additional fallback for when plot content changes
            $(document).on('DOMNodeInserted', '#pdf-container', function() {
                setTimeout(function() {
                    if ($('#pdf-container').find('div[id*=\"pdf_plot_\"]').length > 0) {
                        setupZoom();
                    }
                }, 50);
            });
        });
    "))
)


server <- function(input, output, session) {
    # Store split PDF info
    rv <- reactiveValues(
        files_df = NULL,
        well_df = NULL,  # Separate dataframe for well information
        pdf_index = 1,
        ocr_text = list(),
        ocr_display_mode = "none",
        selected_text = NULL
    )
    
    # Reactive value to control brush mode
    brush_enabled <- reactiveVal(FALSE)




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
            png_files <- pdftools::pdf_convert(pdf_path, dpi = 300, filenames = file.path(tempdir(), sprintf("%s_page_%d.png", tools::file_path_sans_ext(basename(pdf_path)), seq_len(pdftools::pdf_info(pdf_path)$pages))))

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
            split_df$NewFilename <- file.path("/pdfs", basename(split_df$Path))
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

        # Initialize well information dataframe with same number of rows
        well_fields <- c(
            "borehole_id", "well_name", "community", "coordinate_system",
            "easting", "northing", "utm_zone", "latitude", "longitude",
            "location_source", "surveyed_location_top_casing", "purpose_of_well",
            "depth_to_bedrock", "depth_to_bedrock_unit", "date_drilled",
            "casing_outside_diameter", "casing_outside_diameter_unit",
            "well_depth", "well_depth_unit", "top_of_screen", "top_of_screen_unit",
            "bottom_of_screen", "bottom_of_screen_unit", "well_head_stick_up",
            "well_head_stick_up_unit", "static_water_level", "static_water_level_unit",
            "estimated_yield", "estimated_yield_unit", "surveyed_ground_level_elevation",
            "surveyed_ground_level_elevation_unit"
        )
        
        # Create well dataframe with same number of rows as files_df
        rv$well_df <- data.frame(
            page_id = seq_len(nrow(rv$files_df)),
            stringsAsFactors = FALSE
        )
        
        # Initialize all well fields with NA
        for (field in well_fields) {
            rv$well_df[[field]] <- NA
        }

        rv$pdf_index <- 1
        dataTableProxy("pdf_table") %>% selectRows(1)

        rv$ocr_text <- vector("list", nrow(rv$files_df))
        rv$ocr_display_mode <- "none"
        
        # Reset button states on upload
        brush_enabled(FALSE)
        updateSelectizeInput(session, "ocr_display_mode", selected = "none")

        runjs("$('#brush_select').removeClass('btn-active');")
        
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
            dataTableProxy("pdf_table") %>% selectRows(rv$pdf_index)
        }
    })

    observeEvent(input$next_pdf, {
        req(rv$files_df)
        if (rv$pdf_index < nrow(rv$files_df)) {
            rv$pdf_index <- rv$pdf_index + 1
            # Ensure table selection follows
            dataTableProxy("pdf_table") %>% selectRows(rv$pdf_index)
        }
    })

    observeEvent(input$prev_pdf, {
        req(rv$files_df)
        if (rv$pdf_index > 1) {
            rv$pdf_index <- rv$pdf_index - 1
            # Ensure table selection follows
            dataTableProxy("pdf_table") %>% selectRows(rv$pdf_index)
        }
    })

    observeEvent(input$remove_pdf, {
        req(rv$files_df)
        if (nrow(rv$files_df) > 0) {
            selected_row <- rv$pdf_index
            rv$files_df <- rv$files_df[-selected_row, ]
            rv$well_df <- rv$well_df[-selected_row, ]  # Also remove from well_df
            rv$ocr_text <- rv$ocr_text[-selected_row]
            if (nrow(rv$files_df) == 0) {
                rv$pdf_index <- 1
            } else if (rv$pdf_index > nrow(rv$files_df)) {
                rv$pdf_index <- nrow(rv$files_df)
            }
        }
    })


    observeEvent(input$ocr_display_mode, {
        req(rv$files_df)
        
        # Update the display mode
        rv$ocr_display_mode <- input$ocr_display_mode
        
        # If switching to highlight or text mode, ensure OCR data is available
        if (rv$ocr_display_mode %in% c("highlight", "text")) {
            # Process OCR for all pages (function handles caching and notifications)
            rv$ocr_text <- process_ocr_batch(rv$files_df, rv$ocr_text, rv$pdf_index)
        }
    })

    output$pdf_table <- renderDataTable({
        req(rv$files_df)

        dat <- rv$files_df[, 
            c("tag", "Date")
        ]

        # Format Date as dd-mm-yy hh:mm
        dat$Date <- format(as.POSIXct(dat$Date), "%d-%m-%y %H:%M")

        datatable(
            dat,
            selection = "single",
            options = list(pageLength = 5, dom = 'tip') # removes search box
        )
    })

    observe({
        req(rv$files_df)
        req(rv$pdf_index)
        
        # Always render normal version - OCR overlay is handled in the plot itself
        output$pdf_viewer <- renderUI({
            img_path <- rv$files_df$Path[rv$pdf_index]
            
            # Get image dimensions to calculate aspect ratio
            img <- magick::image_read(img_path)
            info <- magick::image_info(img)
            img_width <- info$width
            img_height <- info$height
            aspect_ratio <- img_height / img_width
            
            # Calculate optimal display size
            container_max_width <- 1200  # Maximum reasonable width
            display_width <- min(container_max_width, img_width)
            display_height <- display_width * aspect_ratio
            
            # Use CSS to make the plot responsive while maintaining aspect ratio
            tags$div(
                style = "width: 100%; display: flex; justify-content: center; align-items: flex-start;",
                plotOutput(
                    outputId = paste0("pdf_plot_", rv$pdf_index),
                    width = paste0(display_width, "px"),
                    height = paste0(display_height, "px"),
                    brush = if (brush_enabled()) {
                        brushOpts(id = "pdf_brush", resetOnNew = TRUE)
                    } else {
                        NULL
                    }
                )
            )
        })
        
        output[[paste0("pdf_plot_", rv$pdf_index)]] <- renderPlot({
            img_path <- rv$files_df$Path[rv$pdf_index]
            img <- magick::image_read(img_path)
            
            # Enhance image quality for better display
            img <- img %>%
                magick::image_enhance()
            
            img_gg <- as.raster(img)
            
            # Get image dimensions
            info <- magick::image_info(img)
            img_width <- info$width
            img_height <- info$height
            
            # Start with base plot with improved styling
            p <- ggplot2::ggplot() +
                ggplot2::annotation_raster(img_gg, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
                ggplot2::theme_void() +
                ggplot2::theme(
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    plot.margin = ggplot2::margin(5, 5, 5, 5, "pt"),
                    aspect.ratio = img_height / img_width,
                    panel.border = ggplot2::element_rect(color = "#ddd", fill = NA, size = 1)
                ) +
                ggplot2::coord_fixed(ratio = 1) +
                ggplot2::scale_x_continuous(expand = c(0, 0)) +
                ggplot2::scale_y_continuous(expand = c(0, 0))
            
            # Add OCR rectangles if in OCR mode and OCR data exists
            if (rv$ocr_display_mode != "none" && !is.null(rv$ocr_text[[rv$pdf_index]])) {
                ocr_df <- rv$ocr_text[[rv$pdf_index]]
                
                # Filter by confidence threshold
                if (nrow(ocr_df) > 0) {
                    ocr_df <- ocr_df[ocr_df$confidence >= input$confidence_threshold, , drop = FALSE]
                }

                if (nrow(ocr_df) > 0) {
                    # bbox is "x1,y1,x2,y2"
                    rects <- do.call(rbind, lapply(seq_len(nrow(ocr_df)), function(i) {
                        coords <- as.numeric(strsplit(ocr_df$bbox[i], ",")[[1]])
                        data.frame(
                            xmin = coords[1] / img_width,
                            xmax = coords[3] / img_width,
                            ymin = 1 - coords[4] / img_height,
                            ymax = 1 - coords[2] / img_height,
                            word = ocr_df$word[i],
                            stringsAsFactors = FALSE
                        )
                    }))
                    
                    if (rv$ocr_display_mode == "text") {
                        # Show text with solid background instead of OCR boxes
                        p <- p + ggplot2::geom_rect(
                            data = rects,
                            ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                            fill = "white", color = "black", size = 0.3, alpha = 0.9
                        )
                        
                        # Add text labels
                        text_data <- rects
                        text_data$x <- (text_data$xmin + text_data$xmax) / 2
                        text_data$y <- (text_data$ymin + text_data$ymax) / 2
                        
                        p <- p + ggplot2::geom_text(
                            data = text_data,
                            ggplot2::aes(x = x, y = y, label = word),
                            size = 3, color = "black", fontface = "bold",
                            check_overlap = TRUE
                        )
                    } else {
                        # Default to highlighting boxes
                        p <- p + ggplot2::geom_rect(
                            data = rects,
                            ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                            color = "#007bff", size = 0.5, alpha = 0.3
                        )
                    }
                }
            }
            
            p
        }, res = 150)  # Higher resolution for better image quality
    })

    observeEvent(input$brush_select, {
        brush_enabled(!brush_enabled())
        
        # If enabling brush, reset zoom/pan to avoid coordinate issues
        if (brush_enabled()) {
            runjs("
                const plot = $('#pdf-container').find('div[id*=\"pdf_plot_\"]');
                if (plot.length > 0) {
                    zoomLevel = 1;
                    panX = 0;
                    panY = 0;
                    plot.css({
                        'transform': 'scale(1) translate(0px, 0px)',
                        'transform-origin': '0 0'
                    });
                }
            ")
        }
        
        # If enabling brush and no OCR data exists, process OCR
        if (brush_enabled() && !is.null(rv$files_df)) {
            # Check if we need OCR processing for brush functionality
            needs_ocr <- any(sapply(rv$ocr_text, function(x) is.null(x)))
            
            if (needs_ocr) {
                # Process OCR for brush text extraction
                rv$ocr_text <- process_ocr_batch(rv$files_df, rv$ocr_text, rv$pdf_index)
                
                # Enable highlight mode for visualization if not already enabled
                if (rv$ocr_display_mode == "none") {
                    updateSelectizeInput(session, "ocr_display_mode", selected = "highlight")
                }
            }
        }
        
        # Toggle button styling based on brush state
        if (brush_enabled()) {
            runjs("$('#brush_select').addClass('btn-active');")
        } else {
            runjs("$('#brush_select').removeClass('btn-active');")
        }
    })

    observeEvent(input$pdf_brush, {
        brush <- input$pdf_brush
        if (!is.null(brush)) {
            # Find intersection between brush bbox and OCR bounding boxes
            if (rv$ocr_display_mode != "none" && !is.null(rv$ocr_text[[rv$pdf_index]])) {
                ocr_df <- rv$ocr_text[[rv$pdf_index]]
                # Filter by confidence threshold
                if (nrow(ocr_df) > 0) {
                    ocr_df <- ocr_df[ocr_df$confidence >= input$confidence_threshold, , drop = FALSE]
                }
                info <- magick::image_info(magick::image_read(rv$files_df$Path[rv$pdf_index]))
                img_width <- info$width
                img_height <- info$height

                # Convert brush coordinates (normalized) to image pixel coordinates
                brush_xmin <- brush$xmin * img_width
                brush_xmax <- brush$xmax * img_width
                # Y is reversed in ggplot2 raster
                brush_ymin <- (1 - brush$ymax) * img_height
                brush_ymax <- (1 - brush$ymin) * img_height

                # Function to check intersection between two rectangles
                rect_intersect <- function(ax1, ay1, ax2, ay2, bx1, by1, bx2, by2) {
                    !(ax2 < bx1 || ax1 > bx2 || ay2 < by1 || ay1 > by2)
                }

                # Find OCR words whose bbox intersects with brush
                intersect_idx <- sapply(ocr_df$bbox, function(b) {
                    coords <- as.numeric(strsplit(b, ",")[[1]])
                    # OCR bbox: x1, y1, x2, y2
                    rect_intersect(
                        brush_xmin, brush_ymin, brush_xmax, brush_ymax,
                        coords[1], coords[2], coords[3], coords[4]
                    )
                })

                selected_words <- ocr_df$word[intersect_idx]

                # Save selected text to reactiveValues for later use
                rv$selected_text <- concat_ocr_words_by_row(ocr_df[intersect_idx, ])
                
                
                if (length(rv$selected_text) > 0) {
                    cat(paste(rv$selected_text, collapse = "\n"), "\n")
                } else {
                    cat("No text found in brush region.\n")
                }
            }
        }
    })
    
    # Display OCR text in right panel
    output$ocr_text_display <- renderText({
        if (!is.null(rv$selected_text) && length(rv$selected_text) > 0) {
            # Show selected text when available
            paste(rv$selected_text, collapse = "\n")
        } else if (rv$ocr_display_mode != "none" && !is.null(rv$ocr_text[[rv$pdf_index]])) {
            paste("OCR text for page", rv$pdf_index, "is available. Use brush to select text.")
        } else {
            ""
        }
    })
    
    # Observer for borehole ID input - add selected text when field is clicked
    observeEvent(input$borehole_id_clicked, {
        # Check if there's selected text to add
        if (!is.null(rv$selected_text) && length(rv$selected_text) > 0) {
            # Combine selected text into a single string
            selected_text_combined <- paste(rv$selected_text, collapse = " ")
            
            # Get current value in the borehole ID field
            current_borehole_id <- input$borehole_id
            
            # Add selected text to the current borehole ID (replace if empty, otherwise add)
            if (is.null(selected_text_combined) || selected_text_combined == "") {
                new_borehole_id <- current_borehole_id
            } else {
                new_borehole_id <- selected_text_combined
            }
            
            # Update the text input with the new borehole ID
            updateTextInput(session, "borehole_id", value = new_borehole_id)
            
            # Clear the selected text after using it
            rv$selected_text <- NULL
            
            # Show notification
            showNotification("Selected text added to borehole ID", type = "message", duration = 2)
        }
    })


    # Save metadata
    observeEvent(input$save_metadata, {
        req(rv$well_df)
        req(rv$pdf_index)
        
        # Show current well data dataframe for debugging
        current_df <- well_data_df()
        if (nrow(current_df) > 0) {
            cat("Current well data saved:\n")
            print(current_df)
        }
        
        showNotification("Well information saved!", type = "message", duration = 2)
    })
    
    # Export data
    observeEvent(input$export_data, {
        req(rv$files_df)
        
        # Export both OCR and well data
        export_data <- list(
            well_data = rv$well_df,
            ocr_data = rv$ocr_text,
            files_info = rv$files_df[, c("Name", "OrigFile", "Page", "Path", "tag")]
        )
        
        # You could save this to CSV, Excel, or JSON here
        cat("Export data structure:\n")
        cat("Well data records:", nrow(rv$well_df), "\n")
        cat("OCR data pages:", length(rv$ocr_text), "\n")
        cat("Total files:", nrow(rv$files_df), "\n")
        
        showNotification("Export data prepared! Check console for details.", type = "message", duration = 3)
    })

    # Observe all input fields and update well_df at pdf_index
    observeEvent(list(
        input$borehole_id, input$well_name, input$community, input$coordinate_system,
        input$easting, input$northing, input$utm_zone, input$latitude, input$longitude,
        input$location_source, input$surveyed_location_top_casing, input$purpose_of_well,
        input$depth_to_bedrock, input$depth_to_bedrock_unit, input$date_drilled,
        input$casing_outside_diameter, input$casing_outside_diameter_unit,
        input$well_depth, input$well_depth_unit, input$top_of_screen, input$top_of_screen_unit,
        input$bottom_of_screen, input$bottom_of_screen_unit, input$well_head_stick_up,
        input$well_head_stick_up_unit, input$static_water_level, input$static_water_level_unit,
        input$estimated_yield, input$estimated_yield_unit, input$surveyed_ground_level_elevation,
        input$surveyed_ground_level_elevation_unit
    ), {
        req(rv$well_df)
        req(rv$pdf_index <= nrow(rv$well_df))
        req(rv$pdf_index > 0)

        # Update well dataframe with current input values (compact version)
        fields <- c(
            "borehole_id", "well_name", "community", "coordinate_system",
            "easting", "northing", "utm_zone", "latitude", "longitude",
            "location_source", "surveyed_location_top_casing", "purpose_of_well",
            "depth_to_bedrock", "depth_to_bedrock_unit", "date_drilled",
            "casing_outside_diameter", "casing_outside_diameter_unit",
            "well_depth", "well_depth_unit", "top_of_screen", "top_of_screen_unit",
            "bottom_of_screen", "bottom_of_screen_unit", "well_head_stick_up",
            "well_head_stick_up_unit", "static_water_level", "static_water_level_unit",
            "estimated_yield", "estimated_yield_unit", "surveyed_ground_level_elevation",
            "surveyed_ground_level_elevation_unit"
        )
        for (field in fields) {
            value <- input[[field]]
            if (!is.null(value)) {
            # Convert date to character for consistency
            if (field == "date_drilled") value <- as.character(value)
            rv$well_df[[field]][rv$pdf_index] <- value
            }
        }
    }, ignoreInit = TRUE)

    # Update input fields when pdf_index changes
    observeEvent(rv$pdf_index, {
        req(rv$well_df)
        req(rv$pdf_index <= nrow(rv$well_df))
        
        current_row <- rv$well_df[rv$pdf_index, ]
        print(paste("Updating inputs for PDF index:", rv$pdf_index))
        
        # Update all input fields from well_df
        updateTextInput(session, "borehole_id", value = ifelse(is.null(current_row$borehole_id) || is.na(current_row$borehole_id), "", current_row$borehole_id))
        #updateTextInput(session, "well_name", value = ifelse(is.na(current_row$well_name), "", current_row$well_name))
        #updateTextInput(session, "location_source", value = ifelse(is.na(current_row$location_source), "", current_row$location_source))
        #updateTextInput(session, "surveyed_location_top_casing", value = ifelse(is.na(current_row$surveyed_location_top_casing), "", current_row$surveyed_location_top_casing))
        #
        ## Update selectize inputs
        #updateSelectizeInput(session, "community", selected = ifelse(is.na(current_row$community), NULL, current_row$community))
        #updateSelectizeInput(session, "purpose_of_well", selected = ifelse(is.na(current_row$purpose_of_well), NULL, current_row$purpose_of_well))
        #updateSelectizeInput(session, "utm_zone", selected = ifelse(is.na(current_row$utm_zone), "8N", current_row$utm_zone))
        #
        ## Update radio buttons
        #updateRadioButtons(session, "coordinate_system", selected = ifelse(is.na(current_row$coordinate_system), "utm", current_row$coordinate_system))
        #updateRadioButtons(session, "depth_to_bedrock_unit", selected = ifelse(is.na(current_row$depth_to_bedrock_unit), "ft", current_row$depth_to_bedrock_unit))
        #updateRadioButtons(session, "casing_outside_diameter_unit", selected = ifelse(is.na(current_row$casing_outside_diameter_unit), "mm", current_row$casing_outside_diameter_unit))
        #updateRadioButtons(session, "well_depth_unit", selected = ifelse(is.na(current_row$well_depth_unit), "ft", current_row$well_depth_unit))
        #updateRadioButtons(session, "top_of_screen_unit", selected = ifelse(is.na(current_row$top_of_screen_unit), "ft", current_row$top_of_screen_unit))
        #updateRadioButtons(session, "bottom_of_screen_unit", selected = ifelse(is.na(current_row$bottom_of_screen_unit), "ft", current_row$bottom_of_screen_unit))
        #updateRadioButtons(session, "well_head_stick_up_unit", selected = ifelse(is.na(current_row$well_head_stick_up_unit), "ft", current_row$well_head_stick_up_unit))
        #updateRadioButtons(session, "static_water_level_unit", selected = ifelse(is.na(current_row$static_water_level_unit), "ft", current_row$static_water_level_unit))
        #updateRadioButtons(session, "estimated_yield_unit", selected = ifelse(is.na(current_row$estimated_yield_unit), "L/min", current_row$estimated_yield_unit))
        #updateRadioButtons(session, "surveyed_ground_level_elevation_unit", selected = ifelse(is.na(current_row$surveyed_ground_level_elevation_unit), "m", current_row$surveyed_ground_level_elevation_unit))
        #
        ## Update numeric inputs
        #updateNumericInput(session, "easting", value = ifelse(is.na(current_row$easting), NULL, current_row$easting))
        #updateNumericInput(session, "northing", value = ifelse(is.na(current_row$northing), NULL, current_row$northing))
        #updateNumericInput(session, "latitude", value = ifelse(is.na(current_row$latitude), NULL, current_row$latitude))
        #updateNumericInput(session, "longitude", value = ifelse(is.na(current_row$longitude), NULL, current_row$longitude))
        #updateNumericInput(session, "depth_to_bedrock", value = ifelse(is.na(current_row$depth_to_bedrock), NULL, current_row$depth_to_bedrock))
        #updateNumericInput(session, "casing_outside_diameter", value = ifelse(is.na(current_row$casing_outside_diameter), NULL, current_row$casing_outside_diameter))
        #updateNumericInput(session, "well_depth", value = ifelse(is.na(current_row$well_depth), NULL, current_row$well_depth))
        #updateNumericInput(session, "top_of_screen", value = ifelse(is.na(current_row$top_of_screen), NULL, current_row$top_of_screen))
        #updateNumericInput(session, "bottom_of_screen", value = ifelse(is.na(current_row$bottom_of_screen), NULL, current_row$bottom_of_screen))
        #updateNumericInput(session, "well_head_stick_up", value = ifelse(is.na(current_row$well_head_stick_up), NULL, current_row$well_head_stick_up))
        #updateNumericInput(session, "static_water_level", value = ifelse(is.na(current_row$static_water_level), NULL, current_row$static_water_level))
        #updateNumericInput(session, "estimated_yield", value = ifelse(is.na(current_row$estimated_yield), NULL, current_row$estimated_yield))
        #updateNumericInput(session, "surveyed_ground_level_elevation", value = ifelse(is.na(current_row$surveyed_ground_level_elevation), NULL, current_row$surveyed_ground_level_elevation))
        
        # Update date input
        #updateDateInput(session, "date_drilled", value = ifelse(is.na(current_row$date_drilled), NULL, as.Date(current_row$date_drilled)))
    })

    # ...existing code...

    observeEvent(input$remove_pdf, {
        req(rv$files_df)
        if (nrow(rv$files_df) > 0) {
            selected_row <- rv$pdf_index
            rv$files_df <- rv$files_df[-selected_row, ]
            rv$well_df <- rv$well_df[-selected_row, ]  # Also remove from well_df
            rv$ocr_text <- rv$ocr_text[-selected_row]
            if (nrow(rv$files_df) == 0) {
                rv$pdf_index <- 1
            } else if (rv$pdf_index > nrow(rv$files_df)) {
                rv$pdf_index <- nrow(rv$files_df)
            }
        }
    })

    # Save metadata
    observeEvent(input$save_metadata, {
        req(rv$well_df)
        req(rv$pdf_index)
        
        # Show current well data for the current PDF
        current_row <- rv$well_df[rv$pdf_index, ]
        cat("Well information saved for page", rv$pdf_index, ":\n")
        
        # Print relevant fields
        fields_to_show <- c("borehole_id", "well_name", "community", "easting", "northing", 
                           "well_depth", "date_drilled", "purpose_of_well")
        for (field in fields_to_show) {
            if (!is.na(current_row[[field]]) && current_row[[field]] != "") {
                cat(paste0(field, ": ", current_row[[field]]), "\n")
            }
        }
        
        showNotification("Well information saved!", type = "message", duration = 2)
    })
    
    # Export data
    observeEvent(input$export_data, {
        req(rv$files_df)
        req(rv$well_df)
        
        # Export both OCR and well data
        export_data <- list(
            well_data = rv$well_df,
            ocr_data = rv$ocr_text,
            files_info = rv$files_df[, c("Name", "OrigFile", "Page", "Path", "tag")]
        )
        
        # You could save this to CSV, Excel, or JSON here
        cat("Export data structure:\n")
        cat("Well data records:", nrow(rv$well_df), "\n")
        cat("OCR data pages:", length(rv$ocr_text), "\n")
        cat("Total files:", nrow(rv$files_df), "\n")
        
        showNotification("Export data prepared! Check console for details.", type = "message", duration = 3)
    })

}

shinyApp(ui, server)
# Example: OCR and highlight bounding boxes for specific text in a PDF
# 
# # Convert PDF to PNG
# pngfile <- pdftools::pdf_convert('C:\\Users\\esniede\\Documents\\github\\YGwater\\dev\\ywwr\\.data\\204110248.pdf', dpi = 600)
# 
# # Preprocess image and extract text using OCR
# text <- pngfile %>%
#     image_resize("2000x") %>%
#     image_convert(type = 'Grayscale') %>%
#     image_trim(fuzz = 40) %>%
#     image_write(format = 'png', density = '300x300') %>%
#     tesseract::ocr() 
# 
# cat(text)
# 
# Ensure required packages are available
# if (!requireNamespace("magick", quietly = TRUE)) stop("The 'magick' package is required.")
# if (!requireNamespace("tesseract", quietly = TRUE)) stop("The 'tesseract' package is required.")

# Read the PNG file
# img <- magick::image_read(pngfile)

# Run OCR with HOCR output to get bounding boxes
# ocr_data <- tesseract::ocr_data(img, engine = tesseract::tesseract(options = list(tessedit_create_hocr = 1)))

# Find the row(s) containing the specific text, e.g., "Date"
# search_text <- "Date"

# matches <- ocr_data[
#     grepl(search_text, ocr_data$word, ignore.case = TRUE) & ocr_data$conf > 0.8,
# ]

# Print the bounding box locations for the matched text
# print(matches[, "bbox"])

# Draw rectangles on the image for each bounding box
# for (bbox in matches[,"bbox"]) {
#     coords <- as.numeric(unlist(strsplit(bbox, ",")))
#     if (length(coords) == 4) {
#         img <- magick::image_draw(img)
#         rect(coords[1], coords[2], coords[3], coords[4], border = "#00ffaa", lwd = 5, col = "#d400ff")
#         dev.off()
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
#     coords <- as.numeric(unlist(strsplit(bbox, ",")))
#     if (length(coords) == 4) {
#         img <- magick::image_draw(img)
#         rect(coords[1], coords[2], coords[3], coords[4], border = "#00ffaa", lwd = 5, col = "#d400ff")
#         dev.off()
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
#     if (length(coords) == 4) {
#         img <- magick::image_draw(img)
#         rect(coords[1], coords[2], coords[3], coords[4], border = "#00ffaa", lwd = 5, col = "#d400ff")
#         dev.off()
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
#     if (length(coords) == 4) {
#         img <- magick::image_draw(img)
#         rect(coords[1], coords[2], coords[3], coords[4], border = "#00ffaa", lwd = 5, col = "#d400ff")
#         dev.off()
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
#     if (length(coords) == 4) {
#         img <- magick::image_draw(img)
#         rect(coords[1], coords[2], coords[3], coords[4], border = "#00ffaa", lwd = 5, col = "#d400ff")
#         dev.off()
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
#     }
# }

# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")
# Save the annotated image
# magick::image_write(img, path = "annotated.png", format = "png")

