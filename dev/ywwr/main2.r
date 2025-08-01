library(shiny)
library(DT)
library(pdftools)
library(leaflet)
library(plotly)
library(magick)
library(shinyjs)
library(tesseract)




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

                # --- Preprocessing for better OCR accuracy ---
                img <- img %>%
                    magick::image_convert(colorspace = "gray") %>%   # Convert to grayscale
                    magick::image_contrast(sharpen = 1) %>%          # Increase contrast
                    magick::image_modulate(brightness = 110, saturation = 100, hue = 100) %>% # Enhance brightness
                    magick::image_threshold(type = "black", threshold = "60%") # Binarize

                # Perform OCR on the preprocessed image
                ocr_result <- tesseract::ocr_data(img, engine = tesseract::tesseract(options = list(tessedit_create_hocr = 1)))

                # Filter out words with confidence less than 50%
                #ocr_result <- ocr_result[which(ocr_result$confidence >= 50), ]
                
                # Filter out common OCR noise and error words
                ocr_result <- filter_ocr_noise(ocr_result)
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
        "))
    ),
    titlePanel(
        div(style = "display: flex; align-items: center; gap: 10px;",
            div(
                style = "width: 60px; height: 40px; background: linear-gradient(135deg, #007bff, #0056b3); border-radius: 6px; display: flex; align-items: center; justify-content: center; color: white; font-weight: bold; font-size: 14px;",
                "YWRR"
            ),
            "Simpler Index"
        ), 
        windowTitle = "Simpler Index"
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
                div(class = "control-group",
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
            div(
                style = "margin-bottom: 15px;",
                tags$label("Borehole ID:", style = "font-weight: bold; display: block; margin-bottom: 5px;"),
                div(
                    class = "borehole-id-display",
                    textOutput("borehole_id_display")
                )
            ),
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
                column(4, radioButtons("depth_to_bedrock_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            dateInput("date_drilled", "Date Drilled:", value = NULL),
            fluidRow(
                column(8, numericInput("casing_outside_diameter", "Casing Outside Diameter:", value = NULL, min = 0, step = 1)),
                column(4, radioButtons("casing_outside_diameter_unit", "", choices = list("mm" = "mm", "inch" = "inch"), selected = "inch", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("well_depth", "Well Depth:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("well_depth_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("top_of_screen", "Top of Screen:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("top_of_screen_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("bottom_of_screen", "Bottom of Screen:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("bottom_of_screen_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("well_head_stick_up", "Well Head Stick Up:", value = NULL, step = 0.01)),
                column(4, radioButtons("well_head_stick_up_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("static_water_level", "Static Water Level:", value = NULL, step = 0.01)),
                column(4, radioButtons("static_water_level_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("estimated_yield", "Estimated Yield:", value = NULL, min = 0, step = 0.1)),
                column(4, radioButtons("estimated_yield_unit", "", choices = list("L/s" = "L/s", "G/min" = "G/min"), selected = "G/min", inline = TRUE))
            ),
            fluidRow(
                column(8, numericInput("surveyed_ground_level_elevation", "Surveyed Ground Level Elevation:", value = NULL, step = 0.01)),
                column(4, radioButtons("surveyed_ground_level_elevation_unit", "", choices = list("m" = "m", "ft" = "ft"), selected = "ft", inline = TRUE))
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
            
            // Add click handlers for all input fields
            $('#well_name').on('focus click', function() {
                Shiny.setInputValue('well_name_clicked', Math.random());
            });
            
            $('#easting').on('focus click', function() {
                Shiny.setInputValue('easting_clicked', Math.random());
            });
            
            $('#northing').on('focus click', function() {
                Shiny.setInputValue('northing_clicked', Math.random());
            });
            
            $('#latitude').on('focus click', function() {
                Shiny.setInputValue('latitude_clicked', Math.random());
            });
            
            $('#longitude').on('focus click', function() {
                Shiny.setInputValue('longitude_clicked', Math.random());
            });
            
            $('#location_source').on('focus click', function() {
                Shiny.setInputValue('location_source_clicked', Math.random());
            });
            
            $('#surveyed_location_top_casing').on('focus click', function() {
                Shiny.setInputValue('surveyed_location_top_casing_clicked', Math.random());
            });
            
            $('#depth_to_bedrock').on('focus click', function() {
                Shiny.setInputValue('depth_to_bedrock_clicked', Math.random());
            });
            
            $('#date_drilled').on('focus click', function() {
                Shiny.setInputValue('date_drilled_clicked', Math.random());
            });
            
            $('#casing_outside_diameter').on('focus click', function() {
                Shiny.setInputValue('casing_outside_diameter_clicked', Math.random());
            });
            
            $('#well_depth').on('focus click', function() {
                Shiny.setInputValue('well_depth_clicked', Math.random());
            });
            
            $('#top_of_screen').on('focus click', function() {
                Shiny.setInputValue('top_of_screen_clicked', Math.random());
            });
            
            $('#bottom_of_screen').on('focus click', function() {
                Shiny.setInputValue('bottom_of_screen_clicked', Math.random());
            });
            
            $('#well_head_stick_up').on('focus click', function() {
                Shiny.setInputValue('well_head_stick_up_clicked', Math.random());
            });
            
            $('#static_water_level').on('focus click', function() {
                Shiny.setInputValue('static_water_level_clicked', Math.random());
            });
            
            $('#estimated_yield').on('focus click', function() {
                Shiny.setInputValue('estimated_yield_clicked', Math.random());
            });
            
            $('#surveyed_ground_level_elevation').on('focus click', function() {
                Shiny.setInputValue('surveyed_ground_level_elevation_clicked', Math.random());
            });
        });
    "))
)


server <- function(input, output, session) {
    # Store split PDF info
    rv <- reactiveValues(
        files_df = NULL,
        well_data = list(),  # Named list organized by borehole ID
        pdf_index = 1,
        ocr_text = list(),
        ocr_display_mode = "none",
        selected_text = NULL
    )
    
    # Reactive value to control brush mode
    brush_enabled <- reactiveVal(FALSE)
    
    # Flag to prevent circular updates when loading metadata
    loading_metadata <- reactiveVal(FALSE)
    
    # Reactive expression to get the current well ID based on pdf_index
    current_well_id <- reactive({
        req(rv$files_df)
        req(rv$pdf_index)
        req(nrow(rv$files_df) >= rv$pdf_index)
        
        # Since each row has its own unique well ID, just return it directly
        return(rv$files_df$unique_id[rv$pdf_index])
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

        # Initialize well information as named list organized by borehole ID
        well_fields <- c(
            "well_name", "community", "coordinate_system",
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
        
        # Initialize well_data as empty named list
        rv$well_data <- list()
        
        # Assign unique borehole IDs to each row (page) in files_df
        for (i in seq_len(nrow(rv$files_df))) {
            # Generate unique ID for each page
            unique_id <- paste0("BH", sprintf("%04d", i))
            rv$files_df$unique_id[i] <- unique_id

            # Create metadata list with NA values for all fields
            metadata <- list()
            for (field in well_fields) {
                metadata[[field]] <- NA
            }
            
            # Create well_data entry for this individual page/well
            rv$well_data[[unique_id]] <- list(
                files = i,  # Single page index for this well
                metadata = metadata  # Named list of well metadata
            )
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
            
            # Find which well this page belongs to
            well_id_to_remove <- rv$files_df$unique_id[selected_row]
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
    
    # Consolidated observer for PDF rendering - triggers on PDF index changes AND OCR settings
    observe({
        req(rv$files_df)
        req(rv$pdf_index)
        
        # Include brush_enabled() as a reactive dependency so UI re-renders when brush state changes
        current_brush_enabled <- brush_enabled()
        
        # Only render UI when PDF index changes OR brush state changes
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

            tags$div(
                style = "width: 100%; display: flex; justify-content: center; align-items: flex-start;",
                plotOutput(
                    outputId = paste0("pdf_plot_", rv$pdf_index),
                    width = paste0(display_width, "px"),
                    height = paste0(display_height, "px"),
                    # Conditionally include brush based on brush_enabled state
                    brush = if (current_brush_enabled) {
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
    })

    # Single observer for plot rendering that responds to both PDF changes AND OCR settings
    observe({
        req(rv$files_df)
        req(rv$pdf_index)
        
        # Include OCR display mode and confidence as reactive dependencies
        current_ocr_mode <- input$ocr_display_mode
        current_confidence <- input$confidence_threshold
        
        # Create the plot output only once per PDF index
        plot_id <- paste0("pdf_plot_", rv$pdf_index)
        
        # Use isolate to prevent unnecessary re-rendering
        output[[plot_id]] <- renderPlot({
            img_path <- isolate(rv$files_df$Path[rv$pdf_index])
            img <- magick::image_read(img_path)
            
            # Enhance image quality for better display
            img <- img %>%
                magick::image_enhance()
            
            # Get image dimensions
            info <- magick::image_info(img)
            img_width <- info$width
            img_height <- info$height
            
            # Convert magick image to raster format for base R
            img_raster <- as.raster(img)
            
            # Set up the plot area with correct orientation
            par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
            plot(0, 0, type = "n", xlim = c(0, img_width), ylim = c(0, img_height), 
                 xlab = "", ylab = "", axes = FALSE, asp = 1)
            
            # Display the image
            rasterImage(img_raster, 0, 0, img_width, img_height)
            
            # Add OCR rectangles if in OCR mode and OCR data exists
            if (current_ocr_mode != "none" && !is.null(rv$ocr_text[[rv$pdf_index]])) {
                ocr_df <- rv$ocr_text[[rv$pdf_index]]
                
                # Filter by confidence threshold
                if (nrow(ocr_df) > 0) {
                    ocr_df <- ocr_df[ocr_df$confidence >= current_confidence, , drop = FALSE]
                }

                if (nrow(ocr_df) > 0) {
                    # Process each OCR word with flipped Y coordinates
                    for (i in seq_len(nrow(ocr_df))) {
                        coords <- as.numeric(strsplit(ocr_df$bbox[i], ",")[[1]])
                        x1 <- coords[1]
                        y1 <- img_height - coords[4]  # Flip Y coordinates to match plot
                        x2 <- coords[3]
                        y2 <- img_height - coords[2]  # Flip Y coordinates to match plot
                        
                        if (current_ocr_mode == "text") {
                            # Draw white background rectangle
                            rect(x1, y1, x2, y2, col = "white", border = "black", lwd = 1)
                            # Add text
                            text((x1 + x2) / 2, (y1 + y2) / 2, ocr_df$word[i], 
                                 cex = 0.7, col = "black", font = 2)
                        } else {
                            # Draw highlight rectangle
                            rect(x1, y1, x2, y2, col = rgb(0, 0.48, 1, 0.3), 
                                 border = rgb(0, 0.48, 1, 0.8), lwd = 1)
                        }
                    }
                }
            }
        }, res = 150)  # Higher resolution for better image quality
    })

    observeEvent(input$brush_select, {
        # Toggle brush state
        new_state <- !brush_enabled()
        brush_enabled(new_state)
        
        # If disabling brush, reset zoom/pan and re-enable controls
        if (!new_state) {
            runjs("
                // Re-enable zoom and pan controls
                $('#zoom_out, #zoom_in, #zoom_reset, #zoom_fit').prop('disabled', false);
                $('#zoom_out, #zoom_in, #zoom_reset, #zoom_fit').removeClass('disabled');
                
                // Re-setup zoom functionality
                setTimeout(function() {
                    setupZoom();
                }, 100);
            ")
        } else {
            # If enabling brush, disable zoom/pan controls
            runjs("
                // Reset zoom/pan to avoid coordinate issues
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
                
                // Disable zoom and pan controls
                $('#pdf-container').off('wheel mousedown');
                $(document).off('mousemove.pdfpan mouseup.pdfpan');
                $('#zoom_out, #zoom_in, #zoom_reset, #zoom_fit').prop('disabled', true);
                $('#zoom_out, #zoom_in, #zoom_reset, #zoom_fit').addClass('disabled');
            ")
        }
        
        # If enabling brush and no OCR data exists, process OCR
        if (new_state && !is.null(rv$files_df)) {
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
        if (new_state) {
            runjs("$('#brush_select').addClass('btn-active');")
            showNotification("Brush tool enabled. Zoom/pan disabled for accurate selection.", type = "message", duration = 3)
        } else {
            runjs("$('#brush_select').removeClass('btn-active');")
            # Clear any selected text
            rv$selected_text <- NULL
            showNotification("Brush tool disabled. Zoom/pan re-enabled.", type = "message", duration = 2)
        }
    })

    observeEvent(input$pdf_brush, {
        # Only process brush if brush is enabled
        if (!brush_enabled()) return()
        
        brush <- input$pdf_brush
        if (!is.null(brush) && !is.null(rv$files_df) && rv$pdf_index <= length(rv$ocr_text)) {
            # Ensure OCR data exists for current page
            if (is.null(rv$ocr_text[[rv$pdf_index]])) {
                showNotification("No OCR data available. Processing...", type = "warning", duration = 2)
                rv$ocr_text <- process_ocr_batch(rv$files_df, rv$ocr_text, rv$pdf_index)
            }
            
            # Find intersection between brush bbox and OCR bounding boxes
            ocr_df <- rv$ocr_text[[rv$pdf_index]]
            if (!is.null(ocr_df) && nrow(ocr_df) > 0) {
                # Filter by confidence threshold
                ocr_df <- ocr_df[ocr_df$confidence >= input$confidence_threshold, , drop = FALSE]
                
                if (nrow(ocr_df) > 0) {
                    info <- magick::image_info(magick::image_read(rv$files_df$Path[rv$pdf_index]))
                    img_width <- info$width
                    img_height <- info$height

                    # The brush coordinates are in plot coordinates, not normalized 0-1
                    # They should already be in pixel coordinates matching the plot
                    brush_xmin <- brush$xmin
                    brush_xmax <- brush$xmax
                    brush_ymin <- brush$ymin
                    brush_ymax <- brush$ymax
                    
                    # Debug: Print brush coordinates
                    cat("Brush coordinates: x(", brush_xmin, "-", brush_xmax, "), y(", brush_ymin, "-", brush_ymax, ")\n")
                    cat("Image dimensions:", img_width, "x", img_height, "\n")
                    cat("OCR words to check:", nrow(ocr_df), "\n")

                    # Function to check intersection between two rectangles
                    rect_intersect <- function(ax1, ay1, ax2, ay2, bx1, by1, bx2, by2) {
                        !(ax2 < bx1 || ax1 > bx2 || ay2 < by1 || ay1 > by2)
                    }

                    # Find OCR words whose bbox intersects with brush
                    intersect_idx <- sapply(ocr_df$bbox, function(b) {
                        coords <- as.numeric(strsplit(b, ",")[[1]]);
                        # Convert OCR coordinates to match the plot coordinate system
                        ocr_x1 <- coords[1]
                        ocr_y1 <- img_height - coords[4]  # Flip Y to match plot rendering
                        ocr_x2 <- coords[3]
                        ocr_y2 <- img_height - coords[2]  # Flip Y to match plot rendering
                        
                        # Calculate centroid of OCR rectangle
                        ocr_centroid_x <- (ocr_x1 + ocr_x2) / 2
                        ocr_centroid_y <- (ocr_y1 + ocr_y2) / 2
                        
                        # Debug: Print first few OCR coordinates and centroids
                        if (which(ocr_df$bbox == b) <= 3) {
                            cat("OCR word '", ocr_df$word[which(ocr_df$bbox == b)], "' at: x(", ocr_x1, "-", ocr_x2, "), y(", ocr_y1, "-", ocr_y2, "), centroid(", ocr_centroid_x, ",", ocr_centroid_y, ")\n")
                        }
                        
                        # Check if brush contains the centroid
                        centroid_in_brush <- (ocr_centroid_x >= brush_xmin && ocr_centroid_x <= brush_xmax &&
                                            ocr_centroid_y >= brush_ymin && ocr_centroid_y <= brush_ymax)
                        
                        return(centroid_in_brush)
                    })
                    
                    cat("Number of intersections found:", sum(intersect_idx), "\n")

                    if (any(intersect_idx)) {
                        # Save selected text to reactiveValues for later use
                        rv$selected_text <- concat_ocr_words_by_row(ocr_df[intersect_idx, ])
                        
                        if (length(rv$selected_text) > 0) {
                            cat("Selected text:\n", paste(rv$selected_text, collapse = "\n"), "\n")
                            showNotification(paste("Selected:", rv$selected_text), 
                                           type = "message", duration = 5)
                        }
                    } else {
                        rv$selected_text <- NULL
                        showNotification("No text found in selected area", type = "warning", duration = 2)
                    }
                } else {
                    showNotification("No OCR words above confidence threshold", type = "warning", duration = 2)
                }
            } else {
                showNotification("No OCR data available for this page", type = "warning", duration = 2)
            }
        }
    })

    # Display the borehole ID
    output$borehole_id_display <- renderText({
        req(rv$files_df)
        req(rv$pdf_index)
        
        well_id <- current_well_id()
        
        if (!is.null(well_id) && well_id %in% names(rv$well_data)) {
            # Generate display ID based on filename and page
            file_base <- tools::file_path_sans_ext(rv$files_df$Name[rv$pdf_index])
            page_num <- rv$files_df$Page[rv$pdf_index]
            return(paste0(well_id))
        }
        
        return("No ID available")
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

    # Observer for input field clicks - add selected text when any field is clicked
    input_fields <- c(
        "well_name", "easting", "northing",
        "latitude", "longitude", "location_source", "surveyed_location_top_casing",
        "depth_to_bedrock", "date_drilled",
        "casing_outside_diameter", "well_depth",
        "top_of_screen", "bottom_of_screen",
        "well_head_stick_up", "static_water_level", "estimated_yield",
        "surveyed_ground_level_elevation"
    )

    lapply(input_fields, function(field) {
        observeEvent(input[[paste0(field, "_clicked")]], {
            if (!is.null(rv$selected_text) && length(rv$selected_text) > 0) {
                selected_text_combined <- paste(rv$selected_text, collapse = " ")
                current_value <- input[[field]]
                new_value <- if (is.null(selected_text_combined) || selected_text_combined == "") current_value else selected_text_combined;

                # Use appropriate update function based on input type
                if (grepl("date", field)) {
                    # Try to parse as date
                    parsed_date <- suppressWarnings(as.Date(new_value))
                    if (is.na(parsed_date)) {
                        showNotification(paste("Warning: Selected text is not a valid date for", field), type = "warning", duration = 3)
                    } else {
                        updateDateInput(session, field, value = parsed_date)
                    }
                } else if (grepl("depth|diameter|easting|northing|latitude|longitude|top|bottom|stick_up|static_water_level|estimated_yield|surveyed_ground_level_elevation", field)) {
                    parsed_num <- suppressWarnings(as.numeric(new_value))
                    if (is.na(parsed_num)) {
                        showNotification(paste("Warning: Selected text is not numeric for", field), type = "warning", duration = 3)
                    } else {
                        updateNumericInput(session, field, value = parsed_num)
                    }
                } else {
                    updateTextInput(session, field, value = new_value)
                }
                rv$selected_text <- NULL
                showNotification(paste("Selected text added to", field), type = "message", duration = 2)
            }
        })
    })


    # Save metadata
    observeEvent(input$save_metadata, {
        req(rv$well_data)
        req(rv$files_df)
        req(rv$pdf_index)
        
        well_id <- current_well_id()
        
        if (!is.null(well_id) && well_id %in% names(rv$well_data)) {
            current_metadata <- rv$well_data[[well_id]]$metadata
            cat("Well information saved for borehole", well_id, ":\n")
            
            # Print relevant fields
            fields_to_show <- c("well_name", "community", "easting", "northing", 
                               "well_depth", "date_drilled", "purpose_of_well")
            for (field in fields_to_show) {
                field_value <- current_metadata[[field]]
                if (!is.null(field_value) && !is.na(field_value) && field_value != "") {
                    cat(paste0(field, ": ", field_value), "\n")
                }
            }
            
            showNotification("Well information saved!", type = "message", duration = 2)
        }
    })
    
    # Export data
    observeEvent(input$export_data, {
        req(rv$files_df)
        req(rv$well_data)
        
        # Export both OCR and well data
        export_data <- list(
            well_data = rv$well_data,
            ocr_data = rv$ocr_text,
            files_info = rv$files_df[, c("Name", "OrigFile", "Page", "Path", "tag")]
        )
        
        # You could save this to CSV, Excel, or JSON here
        cat("Export data structure:\n")
        cat("Number of boreholes:", length(rv$well_data), "\n")
        cat("OCR data pages:", length(rv$ocr_text), "\n")
        cat("Total files:", nrow(rv$files_df), "\n")
        
        showNotification("Export data prepared! Check console for details.", type = "message", duration = 3)
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
                well_name = input$well_name,
                community = input$community,
                coordinate_system = input$coordinate_system,
                easting = input$easting,
                northing = input$northing,
                utm_zone = input$utm_zone,
                latitude = input$latitude,
                longitude = input$longitude,
                location_source = input$location_source,
                surveyed_location_top_casing = input$surveyed_location_top_casing,
                purpose_of_well = input$purpose_of_well,
                depth_to_bedrock = input$depth_to_bedrock,
                depth_to_bedrock_unit = input$depth_to_bedrock_unit,
                date_drilled = input$date_drilled,
                casing_outside_diameter = input$casing_outside_diameter,
                casing_outside_diameter_unit = input$casing_outside_diameter_unit,
                well_depth = input$well_depth,
                well_depth_unit = input$well_depth_unit,
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
                surveyed_ground_level_elevation_unit = input$surveyed_ground_level_elevation_unit
            )
        }
    })

    # Observer to load metadata into input fields when switching pages
    observeEvent(rv$pdf_index, {
        req(rv$files_df)
        req(rv$pdf_index)
        req(nrow(rv$files_df) >= rv$pdf_index)
        
        well_id <- current_well_id()
        
        print(paste("Loading metadata for well ID:", well_id, "at index", rv$pdf_index))
        print(rv$well_data[[well_id]]$metadata$easting);

        if (!is.null(well_id) && well_id %in% names(rv$well_data)) {
            # Set flag to prevent metadata updates while loading
            loading_metadata(TRUE)
            
            metadata <- rv$well_data[[well_id]]$metadata
            
            # Helper function to safely get metadata values
            get_meta_value <- function(field, default = "") {
                val <- metadata[[field]]
                if (is.null(val) || is.na(val) || identical(val, NA)) {
                    return(default)
                }
                return(val)
            }
            
            get_meta_numeric <- function(field) {
                val <- metadata[[field]]
                if (is.null(val) || is.na(val) || identical(val, NA)) {
                    return(NA)
                }
                return(val)
            }
            
            get_meta_date <- function(field) {
                val <- metadata[[field]]
                if (is.null(val) || is.na(val) || identical(val, NA)) {
                    return(NULL)
                }
                return(val)
            }
            
            #updateTextInput(session, "borehole_id", value = well_id)
            updateTextInput(session, "well_name", value = get_meta_value("well_name"))
            updateTextInput(session, "location_source", value = get_meta_value("location_source"))
            updateTextInput(session, "surveyed_location_top_casing", value = get_meta_value("surveyed_location_top_casing"))
            
            # Update selectize inputs
            updateSelectizeInput(session, "community", selected = get_meta_value("community", NULL))
            updateSelectizeInput(session, "utm_zone", selected = get_meta_value("utm_zone", "8N"))
            updateSelectizeInput(session, "purpose_of_well", selected = get_meta_value("purpose_of_well", NULL))
            
            # Update radio buttons
            updateRadioButtons(session, "coordinate_system", selected = get_meta_value("coordinate_system", "utm"))
            updateRadioButtons(session, "depth_to_bedrock_unit", selected = get_meta_value("depth_to_bedrock_unit", "ft"))
            updateRadioButtons(session, "casing_outside_diameter_unit", selected = get_meta_value("casing_outside_diameter_unit", "mm"))
            updateRadioButtons(session, "well_depth_unit", selected = get_meta_value("well_depth_unit", "ft"))
            updateRadioButtons(session, "top_of_screen_unit", selected = get_meta_value("top_of_screen_unit", "ft"))
            updateRadioButtons(session, "bottom_of_screen_unit", selected = get_meta_value("bottom_of_screen_unit", "ft"))
            updateRadioButtons(session, "well_head_stick_up_unit", selected = get_meta_value("well_head_stick_up_unit", "ft"))
            updateRadioButtons(session, "static_water_level_unit", selected = get_meta_value("static_water_level_unit", "ft"))
            updateRadioButtons(session, "estimated_yield_unit", selected = get_meta_value("estimated_yield_unit", "L/min"))
            updateRadioButtons(session, "surveyed_ground_level_elevation_unit", selected = get_meta_value("surveyed_ground_level_elevation_unit", "m"))
            
            # Update numeric inputs
            updateNumericInput(session, "easting", value = get_meta_numeric("easting"))
            updateNumericInput(session, "northing", value = get_meta_numeric("northing"))
            updateNumericInput(session, "latitude", value = get_meta_numeric("latitude"))
            updateNumericInput(session, "longitude", value = get_meta_numeric("longitude"))
            updateNumericInput(session, "depth_to_bedrock", value = get_meta_numeric("depth_to_bedrock"))
            updateNumericInput(session, "casing_outside_diameter", value = get_meta_numeric("casing_outside_diameter"))
            updateNumericInput(session, "well_depth", value = get_meta_numeric("well_depth"))
            updateNumericInput(session, "top_of_screen", value = get_meta_numeric("top_of_screen"))
            updateNumericInput(session, "bottom_of_screen", value = get_meta_numeric("bottom_of_screen"))
            updateNumericInput(session, "well_head_stick_up", value = get_meta_numeric("well_head_stick_up"))
            updateNumericInput(session, "static_water_level", value = get_meta_numeric("static_water_level"))
            updateNumericInput(session, "estimated_yield", value = get_meta_numeric("estimated_yield"))
            updateNumericInput(session, "surveyed_ground_level_elevation", value = get_meta_numeric("surveyed_ground_level_elevation"))
            
            # Update date input
            updateDateInput(session, "date_drilled", value = get_meta_date("date_drilled"))
            
            # Re-enable metadata saving after all updates are complete
            loading_metadata(FALSE)

        }
    })

}

shinyApp(ui, server)
