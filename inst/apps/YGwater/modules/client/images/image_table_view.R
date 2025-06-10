# js code to select a row in a datatable with the arrow keys
js_select_dt <- "var dt = table.table().node();
  var tblID = $(dt).closest('.datatables').attr('id');
  var inputName = tblID + '_rows_selected';

  // Handle keyboard arrow key navigation
  table.on('key-focus', function(e, datatable, cell, originalEvent){
    if (originalEvent.type === 'keydown'){
      table.rows().deselect();
      table.row(cell[0][0].row).select();
      var row = table.rows({selected: true});
      Shiny.setInputValue(inputName, [parseInt(row[0]) + 1]);
    }
  });

  // Add mouse click event handler for row selection
  $(dt).on('click', 'tr', function(){
      table.rows().deselect();
      table.row(this).select();
      var row = table.rows({selected: true});
      Shiny.setInputValue(inputName, [parseInt(row[0]) + 1]);
  });"


imgTableViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
    .dataTables_wrapper table.dataTable td.focus, 
    .dataTables_wrapper table.dataTable th.focus {
      outline: none !important;
      box-shadow: none !important;
    }
  "))
  )
  layout_sidebar(
    sidebar = sidebar(
      width = "30%",
      bg = config$sidebar_bg,
      position = "left",
      open = TRUE,
      fillable = TRUE,
      fillable_mobile = TRUE,
      selectizeInput(ns("type"), "Image Type", choices = c("placeholder")),
      uiOutput(ns("dates")), # This is rendered in the server to allow updating language
      uiOutput(ns("loc")), # This is rendered in the server to enable resetting from URL because the default value set here would conflict with the one passed via URL
      DT::dataTableOutput(ns("tbl")) # Table of images. Clicking on a row will display the image in the main panel.
    ),
    imageOutput(ns("img"), fill = TRUE),
    height = "1000px"
  )
}

imgTableView <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load info about last two weeks of images. More is loaded later if the user goes back further.
    imgs <- reactiveValues(imgs = dbGetQueryDT(session$userData$AquaCache, paste0("SELECT i.image_id, i.img_meta_id, i.datetime, l.name, l.name_fr, l.location_id, i.image_type FROM images AS i JOIN image_series AS ii ON i.img_meta_id = ii.img_meta_id JOIN locations AS l ON ii.location_id = l.location_id WHERE i.datetime >= '", Sys.Date() - 14, "' ORDER BY datetime DESC;")),  # Note that this is added to later on if the user's date range is beyond 2 weeks ago; propagate any edits done to this query!
                           img_min = Sys.Date() - 14,
                           img_meta = dbGetQueryDT(session$userData$AquaCache, "SELECT a.img_meta_id, a.first_img, a.last_img, a.location_id, l.name, l.name_fr FROM image_series AS a JOIN locations AS l ON a.location_id = l.location_id;"),
                           imgs_types = dbGetQueryDT(session$userData$AquaCache, "SELECT image_type_id, image_type, description FROM image_types;"))
    
    tables <- reactiveValues()
    
    # Update text based on language ###########################################
    observeEvent(language$language, {
      
      auto <- tr("img_type_auto", language$language)
      man <- tr("img_type_man", language$language)
      choices <- c("auto", "man")
      choices <- stats::setNames(choices, c(auto, man))
      
      updateSelectizeInput(session, "type", label = tr("img_type_lab", language$language), choices = stats::setNames(c("all", imgs$imgs_types$image_type_id), c("All", imgs$imgs_types$image_type)), selected = input$type)
      
      output$dates <- renderUI({
        dateRangeInput(ns("dates"), label = tr("date_range_lab", language$language), start = Sys.Date() - 2, end = Sys.Date(), language = language$abbrev, separator = tr("date_sep", language$language))
      })
      
      loc_choices <- stats::setNames(c("All", imgs$img_meta$location_id), c(tr("all", language$language), imgs$img_meta[[tr("generic_name_col", language$language)]]))
      loc_choices <- c(loc_choices[1], loc_choices[-1][order(names(loc_choices)[-1])]) # Order but keep "All" at the top
      output$loc <- renderUI({
        selectizeInput(ns("loc"), label = tr("loc", language$language), choices = loc_choices, selected = input$loc, multiple = FALSE)
      })
    })
    
    # Get images further back in time if the date range is changed to something beyond 2 weeks ago ############################
    observeEvent(input$dates, {
      if (input$dates[1] < imgs$img_min) {
        if (nrow(imgs$imgs) == 0) {
          imgs$imgs <- dbGetQueryDT(session$userData$AquaCache, paste0("SELECT i.image_id, i.img_meta_id, i.datetime, l.name, l.name_fr, l.location_id, i.image_type FROM images AS i JOIN image_series AS ii ON i.img_meta_id = ii.img_meta_id JOIN locations AS l ON ii.location_id = l.location_id WHERE i.datetime >= '", input$dates[1], "';"))
          imgs$img_min <- min(imgs$imgs$datetime)
        } else {
          extra <- dbGetQueryDT(session$userData$AquaCache, paste0("SELECT i.image_id, i.img_meta_id, i.datetime, l.name, l.name_fr, l.location_id, i.image_type FROM images AS i JOIN image_series AS ii ON i.img_meta_id = ii.img_meta_id JOIN locations AS l ON ii.location_id = l.location_id WHERE i.datetime >= '", input$dates[1], "' AND i.datetime < '", min(imgs$imgs$datetime), "';"))
          imgs$img_min <- min(extra$datetime)
          imgs$imgs <- rbind(imgs$imgs, extra)
        }
        data.table::setorder(imgs$imgs, -datetime)
      }
    }, ignoreInit = TRUE)
    
    
    # Create a data table of images matching filter inputs #########################
    table_data <- reactive({
      req(input$type, input$dates, input$loc, imgs$imgs)  # Ensure all inputs are available

      # Select only the necessary name column based on the language
      generic_name_col <- tr("generic_name_col", language$language)
      
      if (input$type != "all") {
        tbl <- imgs$imgs[imgs$image_type == input$type]
      } else {
        tbl <- imgs$imgs
      }
      
      if (input$loc == "All") {
        tbl <- tbl[datetime >= input$dates[1] & datetime <= as.POSIXct(paste0(input$dates[2], " 23:59")), 
                         .(datetime, get(generic_name_col), image_id)]
      } else {
        tbl <- tbl[datetime >= input$dates[1] & datetime <= as.POSIXct(paste0(input$dates[2], " 23:59")) & location_id == input$loc, 
                         .(datetime, get(generic_name_col), image_id)]
      }

      # create sorting column for datetimes
      tbl[, sort.dt := datetime]
      # Nicely format datetimes (makes them a character object)
      attr(tbl$datetime, "tzone") <- "MST"
      tbl[, datetime := format(datetime, format = "%Y-%m-%d %H:%M")]
      
      # Rename columns
      data.table::setnames(tbl, c(tr("datetime_utc_offset", language$language), tr("loc", language$language), "image_id", "sort.dt"))
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
      req(table_data)
      tbl <- table_data()
      
      tables$tbl <- tbl # Used for image rendering later
      
      out_tbl <- DT::datatable(tbl, 
                               rownames = FALSE, 
                               # selection = list(
                               #   mode = "single", 
                               #   selected = isolate(selected_row())),
                               selection = "none",
                               filter = "none",
                               options = list(
                                 scrollX = TRUE,
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
                                   info = tr("tbl_info", language$language),
                                   infoEmpty = tr("tbl_info_empty", language$language),
                                   paginate = list(previous = "", `next` = ""),
                                   search = tr("tbl_search", language$language),
                                   lengthMenu = tr("tbl_length", language$language),
                                   infoFiltered = tr("tbl_filtered", language$language),
                                   zeroRecords = tr("tbl_zero", language$language)
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
        image <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT format, file FROM images WHERE image_id = ", img_id))
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
  }) # End of moduleServer
} # End of img server function
