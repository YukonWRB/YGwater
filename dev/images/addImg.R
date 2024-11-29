addImgUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$script(HTML("
      Shiny.addCustomMessageHandler('bindDeleteButtons', function(message) {
        setTimeout(function() {
          var tbl = $('#image_upload-imgTbl').DataTable();
          tbl.rows().every(function() {
            var row = this.node();
            $(row).find('.delete_btn').off('click').on('click', function() {
              var id = $(this).data('id');
              Shiny.setInputValue('image_upload-delete_row', id, {priority: 'event'});
            });
          });
        }, 500);
      });
    "))
    ),
    tagList(
      fileInput(
        ns("imgs"),
        "Choose Images",
        accept = c('image/png', 'image/jpeg'),
        multiple = TRUE,
        placeholder = "Drag and drop images here or click to select"
      ),
      DT::dataTableOutput(ns("imgTbl")),
      actionButton(ns("upload"), "Upload Images")
    )
  )
}


addImg <- function(id, AquaCache) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    imgs_info <- reactiveVal(data.frame())
    
    # Observe when images are uploaded
    observeEvent(input$imgs, {
      req(input$imgs)
      files <- input$imgs
      
      # Get existing data
      existing_data <- imgs_info()
      
      # Avoid duplicates by checking if files are already in the table
      new_files <- files$datapath
      new_names <- files$name
      existing_names <- existing_data$FileName
      
      # Identify new files that are not already in the table
      is_new_file <- !(new_names %in% existing_names)
      if (!any(is_new_file)) {
        showNotification("No new images to add.", type = "warning")
        return()
      }
      
      new_file_paths <- new_files[is_new_file]
      new_file_names <- new_names[is_new_file]
      
      # Read EXIF data for new files
      exif_data <- exifr::read_exif(new_file_paths)
      exif_data <- exifr::read_exif("G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS/YOWN-0101 Wolf Creek/Site Photos - Wolf Creek/220921/20220921_113358.jpg")
      exif_data <- exiftoolr::exif_read("G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS/YOWN-0101 Wolf Creek/Site Photos - Wolf Creek/220921/20220921_113358.jpg")
      
      
      
      # Prepare data frame with metadata for new files
      new_data <- data.frame(
        FileID = seq_len(length(new_file_names)) + nrow(existing_data),
        FileName = new_file_names,
        FilePath = new_file_paths,
        Longitude = exif_data$GPSLongitude,
        Latitude = exif_data$GPSLatitude,
        Azimuth = exif_data$GPSImgDirection,
        Elevation = exif_data$GPSAltitude,
        stringsAsFactors = FALSE
      )
      
      # Generate thumbnails for new files
      thumbnails <- sapply(new_file_paths, function(path) {
        img <- magick::image_read(path)
        img_thumb <- magick::image_scale(img, "100") # Scale to 100 pixels width
        img_data <- magick::image_write(img_thumb, format = "png")
        base64enc::dataURI(file = img_data, mime = "image/png")
      })
      new_data$Thumbnail <- paste0('<img src="', thumbnails, '" height="50"/>')
      
      # Add Delete button
      new_data$Delete <- sprintf(
        '<button class="btn btn-danger btn-sm delete_btn" data-id="%d">
          <span class="glyphicon glyphicon-trash"></span> Delete
        </button>',
        new_data$FileID
      )
      
      # Reorder columns to place Thumbnail and Delete first
      new_data <- new_data[, c("Thumbnail", "Delete", setdiff(names(new_data), c("Thumbnail", "Delete")))]
      
      # Combine with existing data
      combined_data <- rbind(existing_data, new_data)
      imgs_info(combined_data)
    })
    
    # Render data table with editable cells and delete buttons
    output$imgTbl <- DT::renderDataTable({
      req(imgs_info())
      DT::datatable(
        imgs_info(),
        escape = FALSE, # Allow HTML rendering for thumbnails and buttons
        editable = list(target = 'cell', disable = list(columns = c(0,1))), # Disable editing on Thumbnail and Delete columns
        selection = 'none',
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(targets = c(0,1), orderable = FALSE), # Disable ordering on Thumbnail and Delete columns
            list(targets = c(2), visible = FALSE), # Hide FileID column
            list(targets = c(3), visible = FALSE)  # Hide FilePath column
          ),
          pageLength = 5
        )
      )
    }, server = FALSE)
    
    # Update reactive value on edits
    observeEvent(input$imgTbl_cell_edit, {
      info <- input$imgTbl_cell_edit
      row <- info$row
      col <- info$col
      value <- info$value
      
      # Update the data frame
      data <- imgs_info()
      colnames <- names(data)
      # Adjust column index because of hidden columns
      actual_col <- col + 1 # +1 due to row names being false
      data[row, actual_col] <- DT::coerceValue(value, data[row, actual_col])
      imgs_info(data)
    })
    
    # Handle delete button clicks
    observe({
      req(input$imgTbl_rows_all)
      # Add a JavaScript handler for delete buttons
      session$sendCustomMessage('bindDeleteButtons', list())
    })
    
    # Receive delete action from JavaScript
    observeEvent(input$delete_row, {
      req(input$delete_row)
      file_id <- as.numeric(input$delete_row)
      data <- imgs_info()
      data <- data[data$FileID != file_id, ]
      imgs_info(data)
    })
    
    # Handle upload button click
    observeEvent(input$upload, {
      req(imgs_info())
      data <- imgs_info()
      
      # # Loop over images and upload
      # for (i in seq_len(nrow(data))) {
      #   metadata <- list(
      #     FileName = data$FileName[i],
      #     Longitude = as.numeric(data$Longitude[i]),
      #     Latitude = as.numeric(data$Latitude[i]),
      #     Azimuth = as.numeric(data$Azimuth[i]),
      #     Elevation = as.numeric(data$Elevation[i])
      #   )
      #   file_path <- data$FilePath[i]
      #   
      #   # Call the user's upload function
      # }
      
      # Clear the images_info after upload
      imgs_info(data.frame())
      showNotification("Images uploaded successfully!", type = "message")
    })
  })
}
