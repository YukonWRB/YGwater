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
                    
                    # Debug output
                    cat("Running OCR with mode:", psm_mode, "method:", preprocessing_method, "\n")
                    
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
            # First row: select, redact, clear, save, zoom
            div(class = "control-row",
                div(class = "control-group",
                    actionButton("brush_select", "Select", icon("mouse-pointer"), class = "btn-toggle") %>% 
                        tooltip("Enable the selection tool for OCR and content redaction."),
                    actionButton("draw_rectangle", "Redact", icon("rectangle-xmark"), class = "btn-toggle") %>%
                        tooltip("Redact the selected area. Boxes are transparent for usability but can be made opaque on upload."),
                    actionButton("clear_rectangles", "Clear", icon("eraser"), class = "btn btn-outline-secondary", title = "Clear Rectangles"),
                    downloadButton("save_image", "Save Image", class = "btn btn-outline-primary", title = "Save image with rectangles"),
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
                        )
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
                
                # Action buttons - split into two buttons
                fluidRow(
                    column(6, actionButton("upload_current_record", "Upload Record", class = "btn-primary btn-block")),
                    column(6, actionButton("upload_all_records", "Upload All Records", class = "btn-primary btn-block"))
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
        rectangles = list()  # List to store rectangles for each PDF (by borehole_id)
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
            runjs("$('#brush_select').addClass('btn-active');")
        } else {
            runjs("$('#brush_select').removeClass('btn-active');")
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


    # Fix the PDF table rendering
    output$pdf_table <- renderDataTable({
        req(rv$files_df)
        validate(need(nrow(rv$files_df) > 0, "No files uploaded yet"))

        dat <- rv$files_df[, c("tag", "borehole_id")]
        
        datatable(
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
                    dataTableProxy("pdf_table") %>% selectRows(rv$pdf_index)
                }
            })
        }
    })
    
    # Add a reactive value for OCR processing status
    ocr_processing <- reactiveVal(FALSE)
    
    # Modified observer for OCR display mode
    observeEvent(input$ocr_display_mode, {
        req(rv$files_df)
        
        # Update the display mode
        rv$ocr_display_mode <- input$ocr_display_mode
        
        # If switching to highlight or text mode, ensure OCR data is available
        if (rv$ocr_display_mode %in% c("highlight", "text")) {
            # Set processing flag
            ocr_processing(TRUE)
            
            # Process OCR only if not already processed
            if (is.null(rv$ocr_text[[rv$pdf_index]]) || nrow(rv$ocr_text[[rv$pdf_index]]) == 0) {
                showNotification("Processing OCR for current page...", type = "message", duration = 3)
                
                img_path <- rv$files_df$Path[rv$pdf_index]
                tryCatch({
                    img <- magick::image_read(img_path)
                    
                    # Apply default preprocessing
                    img <- img %>%
                        magick::image_convert(colorspace = "gray") %>%
                        magick::image_contrast(sharpen = 1) %>%
                        magick::image_modulate(brightness = 110, saturation = 100, hue = 100) %>%
                        magick::image_threshold(type = "black", threshold = "60%")
                    
                    # Create OCR engine options
                    tessoptions <- list(
                        tessedit_create_hocr = 1,
                        tessedit_pageseg_mode = 3  # Default auto page segmentation
                    )
                    
                    # Perform OCR on the preprocessed image
                    ocr_result <- tesseract::ocr_data(img, engine = tesseract::tesseract(
                        options = tessoptions
                    ))
                    
                    # Filter out common OCR noise and error words
                    ocr_result <- filter_ocr_noise(ocr_result)
                    rv$ocr_text[[rv$pdf_index]] <- ocr_result
                }, error = function(e) {
                    message("Error processing OCR: ", e$message)
                    rv$ocr_text[[rv$pdf_index]] <- data.frame(
                        word = character(0),
                        confidence = numeric(0),
                        bbox = character(0),
                        stringsAsFactors = FALSE
                    )
                })
            }
            
            # Clear processing flag
            ocr_processing(FALSE)
            
            # Verify OCR data for current page exists
            if (is.null(rv$ocr_text[[rv$pdf_index]]) || nrow(rv$ocr_text[[rv$pdf_index]]) == 0) {
                showNotification("No OCR text found for this page.", 
                               type = "warning", duration = 4)
            }
        }
    })
    
    # Simplify and fix the plot rendering function for OCR overlays
    observe({
        req(rv$files_df)
        req(rv$pdf_index)
        
        # Get required values
        current_ocr_mode <- input$ocr_display_mode
        current_confidence <- input$confidence_threshold
        borehole_id <- rv$files_df$borehole_id[rv$pdf_index]
        current_rectangles <- if (!is.null(borehole_id)) rv$rectangles[[borehole_id]] else NULL
        is_processing <- ocr_processing()
        plot_id <- paste0("pdf_plot_", rv$pdf_index)
        
        # Render the plot
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
            if (is_processing) {
                rect(10, 10, 300, 50, col = "black", border = NA)
                text(150, 30, "OCR Processing...", col = "white", cex = 1.5)
            }
            
            # Draw OCR overlay if in OCR mode and OCR data exists
            if (current_ocr_mode != "none" && !is.null(rv$ocr_text[[rv$pdf_index]])) {
                ocr_df <- rv$ocr_text[[rv$pdf_index]]
                
                # Filter by confidence threshold
                if (nrow(ocr_df) > 0) {
                    ocr_df <- ocr_df[ocr_df$confidence >= current_confidence, , drop = FALSE]
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
                                if (current_ocr_mode == "text") {
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
                                } else if (current_ocr_mode == "highlight") {
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
                         paste("No OCR text meets confidence threshold (", current_confidence, "%)"),
                         cex = 1, col = "red")
                }
            }
            
            # Draw user-defined redaction rectangles
            if (!is.null(current_rectangles) && length(current_rectangles) > 0) {
                for (rect_data in current_rectangles) {
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
            
            # Create notification with the actual text (limit to reasonable length)
            selected_text <- paste(selected_words, collapse = " ")
            if (nchar(selected_text) > 100) {
                selected_text <- paste0(substr(selected_text, 1, 97), "...")
            }
            showNotification(paste("Selected:", selected_text), 
                           type = "message", duration = 3)
        } else {
            rv$selected_text <- NULL
            showNotification("No text found in selection", type = "warning", duration = 2)
        }
    })

    # --- Rectangle logic: store by borehole_id, not by page index ---
    observeEvent(input$draw_rectangle, {
        # Make sure we have a brush selection
        if (is.null(input$pdf_brush)) {
            showNotification("Please make a selection first", type = "warning", duration = 2)
            return()
        }
        req(rv$files_df)
        req(rv$pdf_index)
        borehole_id <- rv$files_df$borehole_id[rv$pdf_index]
        if (is.null(borehole_id)) return()

        # Get brush coordinates (already in plot coordinates)
        brush <- input$pdf_brush

        # Store rectangle data for this borehole_id
        if (is.null(rv$rectangles[[borehole_id]])) {
            rv$rectangles[[borehole_id]] <- list()
        }
        new_rect <- list(
            xmin = brush$xmin,
            xmax = brush$xmax,
            ymin = brush$ymin,
            ymax = brush$ymax,
            color = "red"
        )
        rv$rectangles[[borehole_id]] <- append(rv$rectangles[[borehole_id]], list(new_rect))
        showNotification("Selection redacted", type = "message", duration = 2)
    })

    observeEvent(input$clear_rectangles, {
        req(rv$files_df)
        req(rv$pdf_index)
        borehole_id <- rv$files_df$borehole_id[rv$pdf_index]
        if (is.null(borehole_id)) return()
        rv$rectangles[[borehole_id]] <- NULL
        showNotification("Rectangles cleared", type = "message", duration = 2)
    })

    # --- Render rectangles for the current PDF (by borehole_id) ---
    observe({
        req(rv$files_df)
        req(rv$pdf_index)
        current_ocr_mode <- input$ocr_display_mode
        current_confidence <- input$confidence_threshold
        borehole_id <- rv$files_df$borehole_id[rv$pdf_index]
        # Use rectangles for this borehole_id
        current_rectangles <- if (!is.null(borehole_id)) rv$rectangles[[borehole_id]] else NULL
        is_processing <- ocr_processing()
        plot_id <- paste0("pdf_plot_", rv$pdf_index)
        
        # Ensure that brush state is correctly reflected in UI when pdf_index changes
        isolate({
            if (brush_enabled()) {
                runjs("$('#brush_select').addClass('btn-active');")
            } else {
                runjs("$('#brush_select').removeClass('btn-active');")
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
            # Draw user-defined rectangles for this PDF
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
    
    # --- Save image handler: use rectangles for current borehole_id ---
    output$save_image <- downloadHandler(
        filename = function() {
            req(rv$files_df, rv$pdf_index)
            base_name <- tools::file_path_sans_ext(basename(rv$files_df$Name[rv$pdf_index]))
            page_num <- rv$files_df$Page[rv$pdf_index]
            paste0(base_name, "_page_", page_num, "_with_rectangles.png")
        },
        content = function(file) {
            req(rv$files_df, rv$pdf_index)
            img_path <- rv$files_df$Path[rv$pdf_index]
            img <- magick::image_read(img_path)
            info <- magick::image_info(img)
            img_width <- info$width
            img_height <- info$height
            borehole_id <- rv$files_df$borehole_id[rv$pdf_index]
            rectangles <- if (!is.null(borehole_id)) rv$rectangles[[borehole_id]] else NULL
            if (!is.null(rectangles) && length(rectangles) > 0) {
                img <- magick::image_draw(img)
                for (rect in rectangles) {
                    y_min_img <- img_height - rect$ymax
                    y_max_img <- img_height - rect$ymin
                    rect(
                        rect$xmin, y_min_img, rect$xmax, y_max_img,
                        border = rect$color,
                        col = adjustcolor("black", alpha.f = 1),
                        lwd = 2
                    )
                }
                dev.off()
            }
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
                permafrost_present = input$permafrost_present,
                permafrost_top_depth = input$permafrost_top_depth,
                permafrost_top_depth_unit = input$permafrost_top_depth_unit,
                permafrost_bottom_depth = input$permafrost_bottom_depth,
                permafrost_bottom_depth_unit = input$permafrost_bottom_depth_unit,
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
    
    # Add click handler for permafrost inputs
    observeEvent(input$permafrost_present, {
        if (input$permafrost_present) {
            showNotification("Permafrost fields enabled", type = "message", duration = 2)
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
            if(length(idx) > 0) {
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
            dataTableProxy("pdf_table") %>% 
                replaceData(rv$files_df[, c("tag", "borehole_id")])
            
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
}
shinyApp(ui, server)

