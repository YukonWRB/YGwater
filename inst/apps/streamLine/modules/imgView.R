# js code to select a row in a datatable with the arrow keys
js_select_dt <-   "var dt = table.table().node();
  var tblID = $(dt).closest('.datatables').attr('id');
  var inputName = tblID + '_rows_selected'
  var incrementName = tblID + '_rows_selected2_increment'
  table.on('key-focus', function(e, datatable, cell, originalEvent){
    if (originalEvent.type === 'keydown'){
      table.rows().deselect();
      table.row(cell[0][0].row).select();
      row = table.rows({selected: true})
      Shiny.setInputValue(inputName, [parseInt(row[0]) + 1]);
}
  });"

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
    
    # Load info about last two weeks of images. More is loaded later if the user goes back further.
    imgs <- reactiveValues(imgs = dbGetQueryDT(con, paste0("SELECT i.image_id, i.img_meta_id, i.datetime, l.name, l.name_fr, l.location_id FROM images AS i JOIN images_index AS ii ON i.img_meta_id = ii.img_meta_id JOIN locations AS l ON ii.location_id = l.location_id WHERE i.datetime >= '", Sys.Date() - 14, "' ORDER BY datetime DESC;")),  # Note that this is added to later on if the user's date range is beyond 2 weeks ago; propagate any edits done to this query!
                           img_min = Sys.Date() - 14,
                           img_meta = dbGetQueryDT(con, "SELECT a.img_meta_id, a.img_type, a.first_img, a.last_img, a.location_id, l.name, l.name_fr FROM images_index AS a JOIN locations AS l ON a.location_id = l.location_id;"))
    
    tables <- reactiveValues()
    
    # Update text based on language ###########################################
    observeEvent(language$language, {

      auto <- titleCase(translations[id == "img_type_auto", get(language$language)][[1]], language$abbrev)
      man <- titleCase(translations[id == "img_type_man", get(language$language)][[1]], language$abbrev)
      choices <- c("auto", "man")
      choices <- stats::setNames(choices, c(auto, man))
      
      updateSelectizeInput(session, "type", label = titleCase(translations[id == "img_type_lab", get(language$language)][[1]], language$abbrev), choices = choices, selected = input$type)
      output$dates <- renderUI({
        dateRangeInput(ns("dates"), label = translations[id == "date_range_lab", get(language$language)][[1]], start = if (restoring()) input$dates[1] else Sys.Date() - 2, end = if (restoring()) input$dates[2] else Sys.Date(), language = language$abbrev, separator = translations[id == "date_sep", get(language$language)][[1]])
      })
      
      loc_choices <- stats::setNames(c("All", imgs$img_meta$location_id), c(translations[id == "all", get(language$language)][[1]], titleCase(imgs$img_meta[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)))
      loc_choices <- c(loc_choices[1], loc_choices[-1][order(names(loc_choices)[-1])]) # Order but keep "All" at the top
      output$loc <- renderUI({
        selectizeInput(ns("loc"), label = titleCase(translations[id == "loc", get(language$language)][[1]], language$abbrev), choices = loc_choices, selected = input$loc)
      })
    })
    
    # Get images further back in time if the date range is changed to something beyond 2 weeks ago ############################
    observeEvent(input$dates, {
      if (input$dates[1] < imgs$img_min) {
        extra <- dbGetQueryDT(con, paste0("SELECT i.image_id, i.img_meta_id, i.datetime, l.name, l.name_fr, l.location_id FROM images AS i JOIN images_index AS ii ON i.img_meta_id = ii.img_meta_id JOIN locations AS l ON ii.location_id = l.location_id WHERE i.datetime >= '", input$dates[1], "' AND i.datetime < '", min(imgs$imgs$datetime), "';"))
        imgs$img_min <- min(extra$datetime)
        imgs$imgs <- rbind(imgs$imgs, extra)
        data.table::setorder(imgs$imgs, -datetime)
      }
    }, ignoreInit = TRUE)
    
    
    # Create a data table of images matching filter inputs #########################
    table_data <- reactive({
      req(input$type, input$dates, input$loc)  # Ensure all inputs are available

      img_ids <- imgs$img_meta[img_type == input$type, "img_meta_id"]

      generic_name_col <- translations[id == "generic_name_col", get(language$language)][[1]]
      
      if (input$loc == "All") {
        tbl <- imgs$imgs[datetime >= input$dates[1] & datetime <= as.POSIXct(paste0(input$dates[2], " 23:59")) & img_meta_id %in% img_ids$img_meta_id, 
                         .(datetime, get(generic_name_col), image_id)]
      } else {
        tbl <- imgs$imgs[datetime >= input$dates[1] & datetime <= as.POSIXct(paste0(input$dates[2], " 23:59")) & location_id == input$loc & img_meta_id %in% img_ids$img_meta_id, 
                         .(datetime, get(generic_name_col), image_id)]
      }
      # create sorting column for datetimes
      tbl[, sort.dt := datetime]
      # Nicely format datetimes (makes them a character object)
      attr(tbl$datetime, "tzone") <- "MST"
      tbl[, datetime := format(datetime, format = "%Y-%m-%d %H:%M")]
      
      # Rename columns
      data.table::setnames(tbl, c(translations[id == "datetime_utc_offset", get(language$language)][[1]], translations[id == "loc", get(language$language)][[1]], "image_id", "sort.dt"))
      
      return(tbl)
    }) # End of reactive creating table
    
    # Initialize reactive value for managing selected row state. This prevents an endless loop triggering the image rendering when the user clicks on a row.
    selected_row <- reactiveVal()
    
    # Update selected row only when necessary
    observe({
      if (!identical(selected_row(), input$tbl_rows_selected)) {
        selected_row(input$tbl_rows_selected)
      }
    })
    
    # Render the data table ########################################################
    observe({
      tbl <- table_data()
      
      tables$tbl <- tbl # Used for image rendering later
      
      out_tbl <- DT::datatable(tbl, 
                               rownames = FALSE, 
                               selection = list(
                                 mode = "single", 
                                 selected = isolate(selected_row())),
                               filter = "none",
                               options = list(
                                 initComplete = htmlwidgets::JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({",
                                   "  'background-color': '#079',",
                                   "  'color': '#fff',",
                                   "  'font-size': '100%',",
                                   # "  'font-family': 'montserrat'", # Unfortunately this isn't as readable as the default font. Left just in case it's needed later.
                                   "});",
                                   "$(this.api().table().body()).css({",
                                   "  'font-size': '90%',",
                                   # "  'font-family': 'nunito-sans'", # Unfortunately this isn't as readable as the default font. Left just in case it's needed later.
                                   "});",
                                   "}"
                                   ),
                                 columnDefs = list(
                                   list(targets = c(2,3),
                                        visible = FALSE), #Hides the image_id and sort.dt columns
                                   list(targets = 1,
                                        orderData = 4),
                                   list(
                                     targets = 1,
                                     render = htmlwidgets::JS( # Truncate long strings in the table
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data !== null && data.length > 30 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                       "}")
                                   )
                                 ),
                                 language = list(
                                   info = translations[id == "tbl_info", get(language$language)][[1]],
                                   infoEmpty = translations[id == "tbl_info_empty", get(language$language)][[1]],
                                   paginate = list(previous = "", `next` = ""),
                                   search = translations[id == "tbl_search", get(language$language)][[1]],
                                   lengthMenu = translations[id == "tbl_length", get(language$language)][[1]],
                                   infoFiltered = translations[id == "tbl_filtered", get(language$language)][[1]],
                                   zeroRecords = translations[id == "tbl_zero", get(language$language)][[1]]
                                 ),
                                 keys = list(keys = c(38,40)) # specific keys used to navigate the table with up/down arrows
                               ),
                               extensions = c("KeyTable", "Select"),
                               callback = htmlwidgets::JS(js_select_dt)
      )
      output$tbl <- DT::renderDataTable(out_tbl, server = FALSE)
    })
    
    # Render the image based on selected_row() ############################################################
    observeEvent(selected_row(), {
      if (selected_row() == 0) {
        return()
      }
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
