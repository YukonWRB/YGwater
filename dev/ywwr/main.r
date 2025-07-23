library(shiny)
library(DT)

ui <- fluidPage(
    titlePanel("PDF Viewer"),
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "pdf_file",
                "Upload PDF(s)",
                accept = ".pdf",
                multiple = TRUE
            ),
            dataTableOutput("pdf_table")
        ),
        mainPanel(
            fluidRow(
                column(12,
                    actionButton("prev_pdf", "Previous"),
                    actionButton("next_pdf", "Next"),
                    actionButton("remove_pdf", "Remove Selected")
                )
            ),
            uiOutput("pdf_viewer")
        )
    )
)

workingDir <- getwd()
cat("Current working directory:", workingDir, "\n")
server <- function(input, output, session) {
    # Store split PDF info
    rv <- reactiveValues(
        files_df = NULL,
        pdf_index = 1
    )

    # Split PDFs into single-page files on upload
    observeEvent(input$pdf_file, {

        print(input$pdf_file)
        print(class(input$pdf_file))

        req(input$pdf_file)

        rv$file_df <- input$pdf_file

        for (i in seq_len(nrow(input$pdf_file))) {
            pdf_path <- input$pdf_file$datapath[i][1]
            orig_name <- input$pdf_file$name[i]
            split_files <- split_pdf_to_pages(pdf_path)
            file_info <- file.info(split_files)
            split_df <- data.frame(
                Name = rep(orig_name, length(split_files)),
                Size_KB = round(file_info$size / 1024, 2),
                Date = as.character(file_info$mtime),
                OrigFile = rep(orig_name, length(split_files)),
                Page = seq_along(split_files),
                Path = split_files,
                stringsAsFactors = FALSE
            )
            split_df$NewFilename <- file.path("/pdfs", basename(split_df$Path))

            if (i == 1) {
                all_split_files <- split_df
            } else {
                all_split_files <- rbind(all_split_files, split_df)
            }
        }

        rv$files_df <- all_split_files

        rv$pdf_index <- 1
    })

    observeEvent(input$pdf_table_rows_selected, {
        req(input$pdf_table_rows_selected)
        rv$pdf_index <- input$pdf_table_rows_selected
    })

    observe({
        dataTableProxy("pdf_table") %>% selectRows(rv$pdf_index)
    })

    observeEvent(input$next_pdf, {
        req(rv$files_df)
        if (rv$pdf_index < length(rv$files_df)) {
            rv$pdf_index <- rv$pdf_index + 1
        }
    })

    observeEvent(input$prev_pdf, {
        req(rv$files_df)
        if (rv$pdf_index > 1) {
            rv$pdf_index <- rv$pdf_index - 1
        }
    })

    observeEvent(input$remove_pdf, {
        req(rv$files_df)
        if (nrow(rv$files_df) > 0) {
            selected_row <- rv$pdf_index
            rv$files_df <- rv$files_df[-selected_row, ]
            if (nrow(rv$files_df) == 0) {
                rv$pdf_index <- 1
            } else if (rv$pdf_index > nrow(rv$files_df)) {
                rv$pdf_index <- nrow(rv$files_df)
            }
        }
    })


    output$pdf_table <- renderDataTable({
        req(rv$files_df)

        dat <- rv$files_df[, 
            c("Name", "Page", "Size_KB", "Date")
        ]

        # Format Date as dd-mm-yy hh:mm
        dat$Date <- format(as.POSIXct(dat$Date), "%d-%m-%y %H:%M")

        datatable(
            dat,
            selection = "single",
            options = list(pageLength = 5)
        )
    })

    output$pdf_viewer <- renderUI({

        req(rv$files_df)
        idx <- rv$pdf_index
        addResourcePath("pdfs", dirname(rv$files_df$Path[[idx]]))

        tags$iframe(
            style = "height:600px; width:100%;",
            src = file.path("/pdfs", basename(rv$files_df$Path[[idx]]))
        )
    })
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

shinyApp(ui, server)

# pngfile <- pdftools::pdf_convert('C:\\Users\\esniede\\Documents\\github\\YGwater\\dev\\ywwr\\.data\\204110248.pdf', dpi = 600)
# text <- pngfile %>%
#   image_resize("2000x") %>%
#   image_convert(type = 'Grayscale') %>%
#   image_trim(fuzz = 40) %>%
#   image_write(format = 'png', density = '300x300') %>%
#   tesseract::ocr() 
# 
# cat(text)
# 
# if (!requireNamespace("magick", quietly = TRUE)) stop("The 'magick' package is required.")
# if (!requireNamespace("tesseract", quietly = TRUE)) stop("The 'tesseract' package is required.")
# 
# # Read the PNG file
# img <- magick::image_read(pngfile)
# 
# # Run OCR with HOCR output to get bounding boxes
# ocr_data <- tesseract::ocr_data(img, engine = tesseract::tesseract(options = list(tessedit_create_hocr = 1)))
# 
# # Find the row(s) containing the specific text, e.g., "Project"
# search_text <- "Date"
# 
# matches <- ocr_data[
#     grepl(search_text, ocr_data$word, ignore.case = TRUE) & ocr_data$conf > 0.8,
# ]
# 
# # Print the bounding box locations for the matched text
# print(matches[, "bbox"])
# 
# 
# # Draw black rectangles on the image for each bounding box
# for (bbox in matches[,"bbox"]) {
#     coords <- as.numeric(unlist(strsplit(bbox, ",")))
#     if (length(coords) == 4) {
#         img <- magick::image_draw(img)
#         rect(coords[1], coords[2], coords[3], coords[4], border = "#00ffaa", lwd = 5, col = "#d400ff")
#         dev.off()
#     }
# }
# 
# # Save or display the modified image
# magick::image_write(img, path = "annotated.png", format = "png")
