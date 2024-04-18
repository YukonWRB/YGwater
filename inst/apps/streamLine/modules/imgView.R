imgUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      selectizeInput(ns("type"), "Image Type", choices = c("auto (series)" = "auto", "manual (one-off)" = "man")),
      uiOutput(ns("dates")), # This is rendered in the server to allow updating language
      uiOutput(ns("loc")), # This is rendered in the server to enable resetting from URL because the default value set here would conflict with the one passed via URL
      DT::dataTableOutput(ns("tbl")) # Table of images. Clicking on a row will display the image in the main panel.
    ),
    mainPanel(
      imageOutput(ns("img"), fill = TRUE)
    )
  )
}


img <- function(id, con, language, restoring) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    setBookmarkExclude(c("tbl_columns_selected", "tbl_cells_selected", "tbl_rows_current", "tbl_rows_all", "tbl_state", "tbl_search", "tbl_cell_clicked", "tbl_row_last_clicked"))
    
    # Load info about last two weeks of images for speed
    imgs <- reactiveValues(imgs = DBI::dbGetQuery(con, paste0("SELECT i.image_id, i.img_meta_id, i.datetime, l.name, l.name_fr, l.location_id FROM images AS i JOIN images_index AS ii ON i.img_meta_id = ii.img_meta_id JOIN locations AS l ON ii.location_id = l.location_id WHERE i.datetime >= '", Sys.Date() - 14, "' ORDER BY datetime DESC;")),  # Note that this is added to later on if the user's date range is beyond 2 weeks ago; propagate any edits done to this query!
                           img_min = Sys.Date() - 14,
                           img_meta = DBI::dbGetQuery(con, "SELECT a.img_meta_id, a.img_type, a.first_img, a.last_img, a.location_id, l.name, l.name_fr FROM images_index AS a JOIN locations AS l ON a.location_id = l.location_id;"))
    tables <- reactiveValues()
    
    # Initialize reactive value for managing selected row state
    selected_row <- reactiveVal()
    
    # Update selected row only when necessary
    observe({
      if (!identical(selected_row(), input$tbl_rows_selected)) {
        selected_row(input$tbl_rows_selected)
      }
    })
    
    # Update text based on language ###########################################
    observeEvent(language(), {
      lang <- language()
      abbrev <- translations[translations$id == "titleCase", ..lang][[1]]
      
      auto <- titleCase(translations[translations$id == "img_type_auto", ..lang][[1]], abbrev)
      man <- titleCase(translations[translations$id == "img_type_man", ..lang][[1]], abbrev)
      choices <- c("auto", "man")
      choices <- stats::setNames(choices, c(auto, man))
      
      updateSelectizeInput(session, "type", label = titleCase(translations[translations$id == "img_type_lab", ..lang][[1]], abbrev), choices = choices, selected = input$type)
      output$dates <- renderUI({
        dateRangeInput(ns("dates"), label = translations[translations$id == "date_range_lab", ..lang][[1]], start = if (restoring()) input$dates[1] else Sys.Date() - 2, end = if (restoring()) input$dates[2] else Sys.Date(), language = abbrev, separator = translations[translations$id == "date_sep", ..lang][[1]])
      })
      output$loc <- renderUI({
        selectizeInput(ns("loc"), label = titleCase(translations[translations$id == "loc", ..lang][[1]], abbrev), choices = stats::setNames(c("All", imgs$img_meta$location_id), c(translations[translations$id == "all", ..lang][[1]], titleCase(imgs$img_meta[[translations[translations$id == "generic_name_col", ..lang][[1]]]], abbrev))), selected = input$loc)
      })
      
    })
    
    # Get images further back in time if the date range is changed to something beyond 2 weeks ago ############################
    observeEvent(input$dates, {
      if (input$dates[1] < imgs$img_min) {
        extra <- DBI::dbGetQuery(con, paste0("SELECT i.image_id, i.img_meta_id, i.datetime, l.name, l.name_fr, l.location_id FROM images AS i JOIN images_index AS ii ON i.img_meta_id = ii.img_meta_id JOIN locations AS l ON ii.location_id = l.location_id WHERE i.datetime >= '", input$dates[1], "' AND i.datetime < '", min(imgs$imgs$datetime), "';"))
        imgs$img_min <- min(extra$datetime)
        imgs$imgs <- rbind(imgs$imgs, extra)
        imgs$imgs <- imgs$imgs[order(imgs$imgs$datetime, decreasing = TRUE), ]
      }
    }, ignoreInit = TRUE)
    
    
    # Give user a data table of images matching inputs #########################
    observe({
      if (is.null(input$type) || is.null(input$dates) || is.null(input$loc)) {
        return()
      }
      lang <- language()
      
      img_ids <- imgs$img_meta[imgs$img_meta$img_type == input$type, "img_meta_id"]
      
      if (input$loc == "All") {
        tbl <- imgs$imgs[imgs$imgs$datetime >= input$dates[1] & imgs$imgs$datetime <= as.POSIXct(paste0(input$dates[2], " 23:59")) & imgs$imgs$img_meta_id %in% img_ids, c("datetime", translations[translations$id == "generic_name_col", ..lang][[1]], "image_id")]
      } else {
        tbl <- imgs$imgs[imgs$imgs$datetime >= input$dates[1] & imgs$imgs$datetime <= as.POSIXct(paste0(input$dates[2], " 23:59")) & imgs$imgs$location_id == input$loc & imgs$imgs$img_meta_id %in% img_ids, c("datetime", translations[translations$id == "generic_name_col", ..lang][[1]], "image_id")]
      }
      
      names(tbl) <- c(translations[translations$id == "datetime", ..lang][[1]], translations[translations$id == "loc", ..lang][[1]], "image_id")
      
      tables$tbl <- tbl
      
      out_tbl <- DT::datatable(tbl, rownames = FALSE, selection = list(mode = "single", selected = selected_row()),
                               filter = "none",
                               options = list(initComplete = htmlwidgets::JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#079', 'color': '#fff'});",
                                 "}"),
                                 columnDefs = list(
                                   list(visible = FALSE, targets = 2), #Hides the image_id column
                                   list(
                                     targets = 1,
                                     render = htmlwidgets::JS( # Truncate long strings in the table
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data.length > 30 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                       "}")
                                   )
                                 ),
                                 language = list(
                                   info = translations[translations$id == "tbl_info", ..lang][[1]],
                                   infoEmpty = translations[translations$id == "tbl_info_empty", ..lang][[1]],
                                   paginate = list(previous = "", `next` = ""),
                                   search = translations[translations$id == "tbl_search", ..lang][[1]],
                                   lengthMenu = translations[translations$id == "tbl_length", ..lang][[1]],
                                   infoFiltered = translations[translations$id == "tbl_filtered", ..lang][[1]],
                                   zeroRecords = translations[translations$id == "tbl_zero", ..lang][[1]]
                                 ),
                                 layout = list(topEnd = "paging")
                               )
      ) %>% 
        DT::formatDate(1, method = "toLocaleString", params = list('fr-FR'))
      output$tbl <- DT::renderDataTable(out_tbl)
    })
    
    observeEvent(selected_row(), {
      if (selected_row() == 0) {
        return()
      }
      lang <- language()
      img_id <- tables$tbl[selected_row(), "image_id"]
      
      output$img <- renderImage({
        image <- DBI::dbGetQuery(con, paste0("SELECT format, file FROM images WHERE image_id = ", img_id))
        # Check if image retrieval was successful and if there is a file to display
        if (nrow(image) == 1 && !is.null(image$file)) {
          outfile <- tempfile(fileext = paste0(".", image$format))
          writeBin(image$file[[1]], outfile)
          list(src = outfile, alt = "User selected image", width = "100%", height = "auto")
        } else {
          list(src = NULL)  # Return NULL if no image or an error occurs
        }
      }, deleteFile = TRUE)
    }, ignoreInit = TRUE)
  })
}
