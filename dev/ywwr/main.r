library(shiny)
library(DT)
library(pdftools)
library(leaflet)
library(plotly)
library(magick)
library(shinyjs)
library(tesseract)
library(bslib)



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

ui <- bslib::page_fluid(
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
                padding: 0;
                display: flex;
                flex-direction: column;
                height: calc(100vh - 80px); /* Set fixed height based on viewport */
                overflow: hidden; /* Hide overflow at the panel level */
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

        div(style = "display: flex; align-items: center; gap: 10px;",
            div(id = "logo-container",
                # Try to load the logo image with error handling
                tags$img(src = "logo.png", 
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
            dataTableOutput("pdf_table")
        ),
        div(class = "main-panel",
            div(class = "control-row",
                div(class = "control-group",
                    actionButton("brush_select", "Select text", class = "btn-toggle") %>% 
                        tooltip("Enable the selection tool for OCR and content redaction."),
                    actionButton("draw_rectangle", "Draw Rectangle", class = "btn-toggle") %>%
                        tooltip("Redact the selected area. Boxes are transparent for usability but can be made opaque on upload."),
                    actionButton("clear_rectangles", "Clear", class = "btn btn-outline-secondary", title = "Clear Rectangles"),
                    downloadButton("save_image", "Save Image", class = "btn btn-outline-primary", title = "Save image with rectangles"),
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
                    # Add simple zoom control
                    sliderInput("zoom_level", "Zoom:",
                        min = 0.5, max = 2.0, value = 1.0, step = 0.1,
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
                    # Removed zoom/pan buttons
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
                textInput("well_name", "Well Name:", placeholder = "Enter well name"),
                selectizeInput("community", "Community:",
                    choices = list(
                        "Beaver Creek" = "beaver_creek",
                        "Burwash Landing" = "burwash_landing",
                        "Carmacks" = "carmacks",
                        "Champagne" = "champagne",
                        "Dawson City" = "dawson_city",
                        "Destruction Bay" = "destruction_bay",
                        "Eagle Plains" = "eagle_plains",
                        "Elsa" = "elsa",
                        "Faro" = "faro",
                        "Haines Junction" = "haines_junction",
                        "Keno City" = "keno_city",
                        "Mayo" = "mayo",
                        "Old Crow" = "old_crow",
                        "Pelly Crossing" = "pelly_crossing",
                        "Ross River" = "ross_river",
                        "Stewart Crossing" = "stewart_crossing",
                        "Tagish" = "tagish",
                        "Teslin" = "teslin",
                        "Watson Lake" = "watson_lake",
                        "Whitehorse" = "whitehorse",
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
                    column(12, downloadButton("save_export_data", "Save & Export to CSV", class = "btn-primary btn-block"))
                )
            )
        )
    ),
    
    tags$script(HTML("
        $(document).ready(function() {
            let isResizing = false;
            let isResizingRight = false;
            let startX = 0;
            let startWidth = 0;
            
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
        selected_text = NULL,
        rectangles = list()  # List to store rectangles for each page
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
        return(rv$files_df$borehole_id[rv$pdf_index])
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
        print(all_split_files)
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


    observeEvent(input$next_pdf_right, {
        req(rv$files_df)
        if (rv$pdf_index < nrow(rv$files_df)) {
            rv$pdf_index <- rv$pdf_index + 1
            # Ensure table selection follows
            dataTableProxy("pdf_table") %>% selectRows(rv$pdf_index)
        }
    })

    observeEvent(input$prev_pdf_right, {
        req(rv$files_df)
        if (rv$pdf_index > 1) {
            rv$pdf_index <- rv$pdf_index - 1
            # Ensure table selection follows
            dataTableProxy("pdf_table") %>% selectRows(rv$pdf_index)
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
            c("tag", "borehole_id")
        ]

        # Format Date as dd-mm-yy hh:mm
        #dat$Date <- format(as.POSIXct(dat$Date), "%d-%m-%y %H:%M")

        datatable(
            dat,
            selection = "single",
            options = list(pageLength = 5, dom = 'tip') # removes search box
        )
    })
    
    # Separate UI rendering observer - only react to brush changes
    observe({
        req(rv$files_df)
        
        # Only depend on brush state for UI changes
        current_brush_enabled <- brush_enabled()
        
        # Use isolate to prevent reactive dependency on pdf_index
        pdf_index <- isolate(rv$pdf_index)
        req(pdf_index)
        
        # Render UI
        output$pdf_viewer <- renderUI({
            img_path <- isolate(rv$files_df$Path[pdf_index])
            
            # Get image dimensions to calculate aspect ratio
            img <- magick::image_read(img_path)
            info <- magick::image_info(img)
            img_width <- info$width
            img_height <- info$height
            
            # Apply zoom factor to dimensions
            zoom <- input$zoom_level
            display_width <- img_width * zoom
            display_height <- img_height * zoom
            
            # Use zoomed dimensions but maintain scrollbars for navigation
            tags$div(
                style = "width: 100%; overflow: auto;",
                plotOutput(
                    outputId = paste0("pdf_plot_", pdf_index),
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
    
    # Plot rendering observer - react to pdf_index and OCR settings
    observe({
        req(rv$files_df)
        req(rv$pdf_index)
        
        # Include OCR settings and rectangles as reactive dependencies
        current_ocr_mode <- input$ocr_display_mode
        current_confidence <- input$confidence_threshold
        page_key <- paste0("page_", rv$pdf_index)
        current_rectangles <- rv$rectangles[[page_key]]
        
        # Create the plot output only when dependencies change
        plot_id <- paste0("pdf_plot_", rv$pdf_index)
        
        output[[plot_id]] <- renderPlot({
            img_path <- rv$files_df$Path[rv$pdf_index]
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
                            # Add text with larger font size
                            text((x1 + x2) / 2, (y1 + y2) / 2, ocr_df$word[i], 
                                 cex = 1.2, col = "black", font = 2)
                        } else {
                            # Draw highlight rectangle
                            rect(x1, y1, x2, y2, col = rgb(0, 0.48, 1, 0.3), 
                                 border = rgb(0, 0.48, 1, 0.8), lwd = 1)
                        }
                    }
                }
            }
            
            # Draw user-defined rectangles on top
            if (!is.null(current_rectangles) && length(current_rectangles) > 0) {
                for (rect in current_rectangles) {
                    rect(rect$xmin, rect$ymin, rect$xmax, rect$ymax, 
                         col = adjustcolor(rect$color, alpha.f = 0.3), 
                         border = rect$color, 
                         lwd = 2)
                }
            }
        }, res = 50)  # Higher resolution for better image quality
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
            word_y2 <- coords[4]
            
            # Check if word overlaps with brush selection
            if (word_x2 >= brush_xmin && word_x1 <= brush_xmax &&
                word_y2 >= brush_ymin && word_y1 <= brush_ymax) {
                selected_words <- c(selected_words, ocr_df$word[i])
            }
        }
        
        # Update selected text
        if (length(selected_words) > 0) {
            rv$selected_text <- selected_words
            showNotification(paste("Selected:", length(selected_words), "words"), 
                           type = "message", duration = 2)
        } else {
            rv$selected_text <- NULL
            showNotification("No text found in selection", type = "warning", duration = 2)
        }
    })

    # Observer for drawing rectangles
    observeEvent(input$draw_rectangle, {
        # Make sure we have a brush selection
        if (is.null(input$pdf_brush)) {
            showNotification("Please make a selection first", type = "warning", duration = 2)
            return()
        }
        
        req(rv$files_df)
        req(rv$pdf_index)
        
        # Get brush coordinates (already in plot coordinates)
        brush <- input$pdf_brush
        
        # Store rectangle data directly in plot coordinates
        page_key <- paste0("page_", rv$pdf_index)
        if (is.null(rv$rectangles[[page_key]])) {
            rv$rectangles[[page_key]] <- list()
        }

        # Create a single rectangle object and add it to the list
        new_rect <- list(
            xmin = brush$xmin,
            xmax = brush$xmax,
            ymin = brush$ymin,  
            ymax = brush$ymax,  
            color = "red"
        )

        # Add the rectangle to the list for this page with proper list nesting
        rv$rectangles[[page_key]] <- append(rv$rectangles[[page_key]], list(new_rect))

        showNotification("Rectangle added", type = "message", duration = 2)
    })
    
    # Observer for clearing rectangles
    observeEvent(input$clear_rectangles, {
        req(rv$files_df)
        req(rv$pdf_index)
        
        page_key <- paste0("page_", rv$pdf_index)
        rv$rectangles[[page_key]] <- NULL
        
        showNotification("Rectangles cleared", type = "message", duration = 2)
    })
    
    observeEvent(input$brush_select, {
        # Toggle brush state
        new_state <- !brush_enabled()
        brush_enabled(new_state)
        
        # If disabling brush, no special handling needed for scroll navigation
        if (!new_state) {
            # No special handling needed
        } else {
            # No special handling needed for scroll navigation
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
            showNotification("Brush tool enabled. Use scroll bars to navigate.", type = "message", duration = 3)
        } else {
            runjs("$('#brush_select').removeClass('btn-active');")
            # Clear any selected text
            rv$selected_text <- NULL
            showNotification("Brush tool disabled.", type = "message", duration = 2)
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

    # Display the file count for current borehole
    output$file_count_display <- renderText({
        req(rv$files_df)
        req(rv$pdf_index)
        
        well_id <- current_well_id()
        
        if (!is.null(well_id) && well_id %in% names(rv$well_data)) {
            files <- rv$well_data[[well_id]]$files
            if (is.null(files)) {
                file_count <- 0
            } else if (is.character(files)) {
                # Count unique filenames
                file_count <- length(unique(files))
            } else {
                file_count <- 1
            }
            
            if (file_count == 1) {
                return(paste("Files associated:", file_count, "file"))
            } else {
                return(paste("Files associated:", file_count, "files"))
            }
        }
        
        return("Files associated: 0 files")
    })



    # Update borehole ID selector choices when files change
    observe({
        if (!is.null(rv$files_df) && nrow(rv$files_df) > 0) {
            # Create choices list with borehole IDs as labels and indices as values
            # Exclude the currently selected borehole from choices
            current_id <- rv$files_df$borehole_id[rv$pdf_index]
            choices <- setNames(
                which(rv$files_df$borehole_id != current_id),
                rv$files_df$borehole_id[rv$files_df$borehole_id != current_id]
            )
            updateSelectizeInput(session, "borehole_id_selector", 
                                choices = choices,
                                selected = character(0),
                                options = list(
                                    placeholder = "Select a borehole ID"
                                ))
        } else {
            # Clear choices when no files
            updateSelectizeInput(session, "borehole_id_selector", 
                                choices = NULL,
                                selected = NULL)
        }
    })


    observeEvent(input$borehole_id_selector, {
        req(rv$files_df)
        req(input$borehole_id_selector)
        selected_index <- as.integer(input$borehole_id_selector)
        selected_id <- rv$files_df$borehole_id[selected_index]
        current_filename <- rv$files_df$Name[rv$pdf_index]
        print(selected_id)
        if (!is.null(selected_id) && selected_id %in% names(rv$well_data)) {
            # Add current filename to the list of filenames for the selected borehole
            filenames <- rv$well_data[[selected_id]]$files
            if (is.null(filenames)) {
                rv$well_data[[selected_id]]$files <- current_filename
            } else if (!(current_filename %in% filenames)) {
                rv$well_data[[selected_id]]$files <- c(filenames, current_filename)
            }
            showNotification(paste("Added file", current_filename, "to borehole", selected_id), type = "message", duration = 2)
            print(rv$well_data[[selected_id]]$files)




            # Update the borehole_id for the current file/page to the selected borehole ID
            rv$files_df$borehole_id[rv$pdf_index] <- selected_id
            print(selected_id)
            #rv$well_data[[selected_id]] <- NULL  # Commented out: do not remove metadata

            # Move to the selected borehole/page in the table
            rv$pdf_index <- selected_index
            # Optionally, remove the old well_data entry for the selected borehole (if needed)
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


    # Combined Save and Export functionality
    output$save_export_data <- downloadHandler(
        filename = function() {
            paste("well_metadata_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            req(rv$well_data)
            
            # Prepare a data frame with one row per well and columns for all metadata fields
            well_ids <- names(rv$well_data)
            if (length(well_ids) == 0) {
                # Create empty data frame if no wells
                write.csv(data.frame(), file, row.names = FALSE)
                return()
            }
            
            metadata_fields <- names(rv$well_data[[well_ids[1]]]$metadata)
            well_metadata_df <- data.frame(matrix(nrow = length(well_ids), ncol = length(metadata_fields)))
            colnames(well_metadata_df) <- metadata_fields
            rownames(well_metadata_df) <- well_ids

            # Helper function to convert feet to meters
            ft_to_m <- function(x) {
                if (is.null(x) || is.na(x)) return(NA)
                as.numeric(x) * 0.3048
            }

            # Loop through each well and populate the data frame
            for (i in seq_along(well_ids)) {
                well_id <- well_ids[i]
                meta <- rv$well_data[[well_id]]$metadata

                if (!is.null(meta$surveyed_ground_level_elevation_unit) && !is.na(meta$surveyed_ground_level_elevation_unit) && meta$surveyed_ground_level_elevation_unit == "ft") {
                    well_metadata_df[i, "surveyed_ground_level_elevation"] <- ft_to_m(meta$surveyed_ground_level_elevation)
                    well_metadata_df[i, "surveyed_ground_level_elevation_unit"] <- "m"
                } else {
                    well_metadata_df[i, "surveyed_ground_level_elevation"] <- meta$surveyed_ground_level_elevation
                    well_metadata_df[i, "surveyed_ground_level_elevation_unit"] <- meta$surveyed_ground_level_elevation_unit
                }

                # Estimated yield: convert G/min to L/s if needed
                if (!is.null(meta$estimated_yield_unit) && !is.na(meta$estimated_yield_unit) && meta$estimated_yield_unit == "G/min") {
                    # 1 US gallon = 3.78541 liters, so G/min to L/s: multiply by 3.78541/60
                    gmin_to_ls <- function(x) { if (is.null(x) || is.na(x)) return(NA); as.numeric(x) * 3.78541 / 60 }
                    well_metadata_df[i, "estimated_yield"] <- gmin_to_ls(meta$estimated_yield)
                    well_metadata_df[i, "estimated_yield_unit"] <- "L/s"
                } else {
                    well_metadata_df[i, "estimated_yield"] <- meta$estimated_yield
                    well_metadata_df[i, "estimated_yield_unit"] <- meta$estimated_yield_unit
                }

                # If easting, northing, and utm_zone are provided, convert to lat/lon
                if (!is.null(meta$coordinate_system) && !is.na(meta$coordinate_system) && meta$coordinate_system == "utm" &&
                    !is.null(meta$easting) && !is.na(meta$easting) &&
                    !is.null(meta$northing) && !is.na(meta$northing) &&
                    !is.null(meta$utm_zone) && !is.na(meta$utm_zone)) {
                    # Extract zone number and hemisphere
                    zone_str <- as.character(meta$utm_zone)
                    zone_num <- as.numeric(gsub("[^0-9]", "", zone_str))
                    hemisphere <- ifelse(grepl("N", zone_str, ignore.case = TRUE), "north", "south")
                    # Use sf for conversion
                    if (requireNamespace("sf", quietly = TRUE)) {
                        coords_sf <- sf::st_sfc(
                            sf::st_point(c(as.numeric(meta$easting), as.numeric(meta$northing))),
                            crs = paste0("EPSG:", 32600 + zone_num)
                        )
                        coords_ll <- sf::st_transform(coords_sf, crs = 4326)
                        latlon <- sf::st_coordinates(coords_ll);
                        well_metadata_df[i, "latitude"] <- latlon[2]
                        well_metadata_df[i, "longitude"] <- latlon[1]
                    }
                } else {
                    well_metadata_df[i, "latitude"] <- meta$latitude
                    well_metadata_df[i, "longitude"] <- meta$longitude
                }

                # Copy all other fields
                well_metadata_df[i, "borehole_id"] <- meta$borehole_id
                well_metadata_df[i, "well_name"] <- meta$well_name
                well_metadata_df[i, "community"] <- meta$community
                well_metadata_df[i, "coordinate_system"] <- meta$coordinate_system
                well_metadata_df[i, "easting"] <- meta$easting
                well_metadata_df[i, "northing"] <- meta$northing
                well_metadata_df[i, "utm_zone"] <- meta$utm_zone
                well_metadata_df[i, "location_source"] <- meta$location_source
                well_metadata_df[i, "surveyed_location_top_casing"] <- meta$surveyed_location_top_casing
                well_metadata_df[i, "purpose_of_well"] <- meta$purpose_of_well
                well_metadata_df[i, "date_drilled"] <- meta$date_drilled
            }
            
            # Write the CSV file
            write.csv(well_metadata_df, file, row.names = FALSE)
            
            showNotification("Well metadata exported successfully!", type = "message", duration = 3)
        }
    )

    # Handler for saving image with rectangles
    output$save_image <- downloadHandler(
        filename = function() {
            # Generate a filename based on the current PDF name and page
            req(rv$files_df, rv$pdf_index)
            base_name <- tools::file_path_sans_ext(basename(rv$files_df$Name[rv$pdf_index]))
            page_num <- rv$files_df$Page[rv$pdf_index]
            paste0(base_name, "_page_", page_num, "_with_rectangles.png")
        },
        content = function(file) {
            req(rv$files_df, rv$pdf_index)
            
            # Get the current image path
            img_path <- rv$files_df$Path[rv$pdf_index]
            img <- magick::image_read(img_path)
            
            # Get image dimensions for coordinate conversion
            info <- magick::image_info(img)
            img_width <- info$width
            img_height <- info$height
            
            # Get rectangles for this page, if any
            page_key <- paste0("page_", rv$pdf_index)
            rectangles <- rv$rectangles[[page_key]]
            
            if (!is.null(rectangles) && length(rectangles) > 0) {
                # Create a drawing context
                img <- magick::image_draw(img)
                
                # Draw each rectangle on the image
                for (rect in rectangles) {
                    # Convert from plot coordinates to image coordinates by flipping Y
                    # Plot coordinates: (0,0) at bottom-left
                    # Image coordinates: (0,0) at top-left
                    y_min_img <- img_height - rect$ymax
                    y_max_img <- img_height - rect$ymin
                    
                    # Draw solid black rectangle for redaction
                    rect(
                        rect$xmin, y_min_img, rect$xmax, y_max_img,
                        border = rect$color,
                        col = adjustcolor("black", alpha.f = 1),
                        lwd = 2
                    )
                }
                
                # Close the drawing context
                dev.off()
            }
            
            # Save the image
            magick::image_write(img, path = file, format = "png")
            
            showNotification("Image saved with rectangles", type = "message", duration = 3)
        }
    )

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
