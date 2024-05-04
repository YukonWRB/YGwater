
dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      # Load the little "i" button for the tooltip
      tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")
    ),
    tags$script(
      HTML(" // Handles tooltip updates outside of the datatable, binds tooltip properties to elements
       
            Shiny.addCustomMessageHandler('update-tooltip', function(message) {
                var selector = '#' + message.id;
                $(selector).attr('title', message.title)
                .tooltip('fixTitle').tooltip('hide');
            });
    "),
      HTML(" // Handles tootip creation and update for the datatable headers
    $(document).ready(function() {
      // Initialize tooltips
      $('body').tooltip({
        selector: '[data-toggle=\"tooltip\"]',
        container: 'body'
      });
      
      // Reinitialize tooltips on table redraw
      $('#tbl').on('draw.dt', function() {
        $('.tooltip').remove();
        $('body').tooltip({
          selector: '[data-toggle=\"tooltip\"]',
          container: 'body'
        });
      });
    });
  ")
    ),
    sidebarPanel(
      span(
        id = ns("infoIcon"),
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        `data-trigger` = "click hover",
        title = "Placeholder",
        icon("info-circle", style = "font-size: 150%;")),
      selectizeInput(ns("type"), "Data Type", choices = c("discrete" = "Discrete"), multiple = FALSE), # choices and labels are updated in the server module
      selectizeInput(ns("pType"), "Parameter Type", choices = c("All" = "All"), multiple = TRUE),
      selectizeInput(ns("pGrp"), "Parameter Group", choices = c("All" = "All"), multiple = TRUE),
      selectizeInput(ns("param"), "Parameter", choices = c("All" = "All"), multiple = TRUE),
      selectizeInput(ns("proj"), "Project", choices = c("All" = "All"), multiple = TRUE),
      selectizeInput(ns("net"), "Network", choices = c("All" = "All"), multiple = TRUE),
      sliderInput(ns("yrs"), "With data between...", sep = "", min = 1897, max = lubridate::year(Sys.Date()), value = c(1897, lubridate::year(Sys.Date())), step = 1),
      selectizeInput(ns("loc"), "Or choose one or more locations", choices = c("All" = "All"), multiple = TRUE),
      actionButton(ns("reset"), "Reset Filters")
    ),
    mainPanel(
      htmlOutput(ns("instructions")),
      tags$div(style = "height: 10px;"),
      DT::dataTableOutput(ns("tbl")), # Table with timeseries, filtered by the sidebar inputs
      actionButton(ns("view_data"), "View Data"),  # Button will be hidden until a row is selected
      uiOutput(ns("modal_subset"))  # Placeholder for the modal DataTable
    )
    
  ) # end tagList
} # end dataUI


data <- function(id, con, language, restoring, data, inputs) {
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c("reset", "view_data"))
    ns <- session$ns
    
    outputs <- reactiveValues()  # This allows the module to pass values back to the main server
    
    # Adjust multiple selection based on if 'All' is selected ################
    observeFilterInput <- function(inputId) {
      observeEvent(input[[inputId]], {
        # Check if 'All' is selected and adjust accordingly
        if (length(input[[inputId]]) > 1) {
          if ("All" %in% input[[inputId]]) {
            updateSelectizeInput(session, inputId, selected = "All")
          }
        }
      })
    }
    observeFilterInput("type")
    observeFilterInput("pType")
    observeFilterInput("pGrp")
    observeFilterInput("param")
    observeFilterInput("proj")
    observeFilterInput("net")
    observeFilterInput("loc")
    
    
    # Update text (including map popup) based on language ###########################################
    
    observe({
      # Update the tooltip's text
      tooltipText <- translations[id == "tooltip_reset", get(language$language)][[1]]
      session$sendCustomMessage(type = 'update-tooltip', message = list(id = ns("infoIcon"), title = tooltipText))
      
      # Update selectizeInputs
      updateSelectizeInput(session, 
                           "type",
                           label = translations[id == "data_type", get(language$language)][[1]],
                           choices = stats::setNames(c("discrete", "continuous"),
                                                     c(titleCase(c(translations[id == "discrete", get(language$language)][[1]], translations[id == "continuous", get(language$language)][[1]]), language$abbrev)
                                                     )
                           ),
                           selected = "discrete"
      )
      updateSelectizeInput(session, 
                           "pType",
                           label = translations[id == "param_type", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$param_types$param_type_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_types[[translations[id == "param_type_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pGrp",
                           label = translations[id == "param_group", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$param_groups$group),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_groups[[translations[id == "param_group_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "param",
                           label = translations[id == "parameter", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$parameters$param_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "proj",
                           label = translations[id == "project", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$projects$project_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$projects[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "net",
                           label = translations[id == "network", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$networks$network_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$networks[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSliderInput(session,
                        "yrs",
                        label = translations[id == "year_filter", get(language$language)][[1]],
                        min = lubridate::year(min(data$timeseries$start_datetime)),
                        max = lubridate::year(max(data$timeseries$end_datetime)),
                        value = lubridate::year(c(min(data$timeseries$start_datetime), max(data$timeseries$end_datetime)))
      )
      updateSelectizeInput(session,
                           "loc",
                           label = translations[id == "choose_locs", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$locations$location_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$locations[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           ),
                           selected = if (!is.null(inputs)) inputs else NULL
      )
      updateActionButton(session,
                         "reset",
                         label = translations[id == "reset", get(language$language)][[1]]
      )
      output$instructions <- renderUI(translations[id == "view_data_instructions", get(language$language)][[1]])
      updateActionButton(session,
                         "view_data",
                         label = translations[id == "view_data", get(language$language)])
    }) # End of text updates based on language selection
    
    
    # Reset all filters when button pressed ##################################
    observeEvent(input$reset, {
      updateSelectizeInput(session, 
                           "type",
                           choices = stats::setNames(c("discrete", "continuous"),
                                                     c(titleCase(c(translations[id == "discrete", get(language$language)][[1]], translations[id == "continuous", get(language$language)][[1]]), language$abbrev)
                                                     )
                           ),
                           selected = "discrete"
      )
      updateSelectizeInput(session, 
                           "pType",
                           choices = stats::setNames(c("All", data$param_types$param_type_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_types[[translations[id == "param_type_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pGrp",
                           choices = stats::setNames(c("All", data$param_groups$group),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_groups[[translations[id == "param_group_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "param",
                           choices = stats::setNames(c("All", data$parameters$param_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "proj",
                           choices = stats::setNames(c("All", data$projects$project_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$projects[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev))
                           )
      )
      updateSelectizeInput(session,
                           "net",
                           choices = stats::setNames(c("All", data$networks$network_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$networks[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev))
                           )
      )
      updateSliderInput(session,
                        "yrs",
                        min = lubridate::year(min(data$timeseries$start_datetime)),
                        max = lubridate::year(max(data$timeseries$end_datetime)),
                        value = lubridate::year(c(min(data$timeseries$start_datetime), max(data$timeseries$end_datetime)))
      )
      updateSelectizeInput(session, "loc", choices = stats::setNames(c("All", data$locations$location_id),
                                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$locations[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                                     )
      ))
    }) # End of observeEvent for reset filters button
    
    # Create the table and render it ###################################
    table_data <- reactive({ # Create the table
      # apply the location filter (locations are already filtered by other filters or directly selected)
      if (!is.null(input$loc) & !("All" %in% input$loc)) {
        tbl <- data.table::copy(data$timeseries)[location_id %in% input$loc]
      } else {
        tbl <- data.table::copy(data$timeseries)
      }
      
      # TODO: period_type and record_rate needs to be part of the table!!!
      # Could be combined: sum (monthly), sum (daily), mean (daily), (min + max) / 2 (daily), min (monthly)
      
      
        # Drop period_type
        tbl[, period_type := NULL]
        tbl[, record_rate := NULL]
        
        
        # Attach location
        tbl[data$locations, on = c(location_id = "location_id"), translations[id == "loc", get(language$language)] := .(get(translations[id == "generic_name_col", get(language$language)]))]
        tbl[, location_id := NULL]
        # Attach parameter type
        tbl[data$param_types, on = c(param_type = "param_type_code"), 
            translations[id == "type", get(language$language)] := get(translations[id == "param_type_col", get(language$language)])]
        tbl[, param_type := NULL]
        # Attach parameter descriptions
        tbl[data$parameters, on = c(parameter = "param_code"), 
            c(translations[id == "group", get(language$language)], translations[id == "parameter", get(language$language)], translations[id == "units", get(language$language)]) 
            := 
              .(get(translations[id == "param_group_col", get(language$language)]), get(translations[id == "param_name_col", get(language$language)]), unit)] # data in column names "unit" is unchanging based on language
        tbl[, parameter := NULL]
        
        # Create sort.start, sort.end for sorting only
        tbl[, sort.start := start_datetime]
        tbl[, sort.end := end_datetime]
        
        # Nicely format datetimes (makes them a character object)
        attr(tbl$start_datetime, "tzone") <- "MST"
        attr(tbl$end_datetime, "tzone") <- "MST"
        tbl[, start_datetime := format(start_datetime, format = "%Y-%m-%d %H:%M")]
        tbl[, end_datetime := format(end_datetime, format = "%Y-%m-%d %H:%M")]
        
        # Modify Category depending on language
        tbl[translations, on = .(category = id), category := get(language$language)]
        
        # Rename start_datetime, end_datetime, category
        data.table::setnames(tbl, old = c("start_datetime", "end_datetime", "category"), new = c(translations[id == "start", get(language$language)], translations[id == "end", get(language$language)], translations[id == "category", get(language$language)]))

        # titleCase column names
        data.table::setnames(tbl, old = names(tbl)[-c(1,3,4)], new = titleCase(names(tbl)[-c(1,3,4)], language$abbrev))
        
        # titleCase columns
        for (j in c(2L,5L:8L)) data.table::set(tbl, j = j, value = titleCase(tbl[[j]], language$abbrev))
        
        # Order by location name, parameter name
        data.table::setorderv(tbl, c(translations[id == "loc", get(language$language)], translations[id == "parameter", get(language$language)]))
        
        return(tbl)
    }) # End of reactive creating table
    
    observe({ # Render the table
      tbl <- table_data()
      
      out_tbl <- DT::datatable(tbl, 
                               rownames = FALSE,
                               selection = "multiple",
                               filter  = "none",
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
                                 # The code below interferes with language updates in the entire module!!!!!
                             #     headerCallback = htmlwidgets::JS(  # This creates the tooltips!
                             #       paste0("function(thead, data, start, end, display) {
                             #   var tooltips = ['", translations[id == "tooltip1", get(language$language)],"', '", translations[id == "tooltip2", get(language$language)], "', '", translations[id == "tooltip2", get(language$language)], "', 'fourth column tooltip', 'fifth column tooltip']; // Define tooltips for each column
                             #   $(thead).find('th').each(function(i) {
                             #     var title = $(this).text();
                             #     var tooltip = tooltips[i] ? tooltips[i] : title; // Use custom tooltip if available
                             #     $(this).html('<span title=\"' + tooltip + '\" data-toggle=\"tooltip\" data-placement=\"top\">' + title + ' <i class=\"fa fa-info-circle\"></i></span>');
                             #   });
                             # }")
                             #     ),
                                 columnDefs = list(
                                   list(targets = 3, orderData = 10), # Order the character datetime column using the hidden true datetime. Column numbers are true.
                                   list(targets = 4, orderData = 11), # Order the character datetime column using the hidden true datetime. Column numbers are true.
                                   list(targets = c(0,9,10), visible = FALSE), #Hides the timeseries_id and datetime sorting columns. Column index numbers start at 0 here!!!
                                   list(
                                     targets = c(4:7), # Column index numbers start at 0 here again!!!
                                     render = htmlwidgets::JS( # Truncate long strings in the table
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data.length > 20 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
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
                                 pageLength = 25
                               ),
      )
      output$tbl <- DT::renderDataTable(out_tbl)
    }) # End of table render
    # End of creating and rendering table section 
    
    
    # Show/hide the view button based on if a row is selected ################
    observe({
      if (is.null(input$tbl_rows_selected)) {
        shinyjs::hide("view_data")
      } else {
        shinyjs::show("view_data")
      }
    })
    
    
    # Show a modal with the data when the view button is clicked ################
    
    observeEvent(input$view_data, {
      # Get the timeseries_id of the selected rows
      selected_ids <- table_data()[input$tbl_rows_selected, timeseries_id]
      
      # Query will be for discrete of continuous data, depending on input$type
      # Show a modal with a subset (first 3 rows per timeseries_id) of the data. Below this, show a date range picker (with min/max preset based on the selected data), the number of rows that would be returned, and download and close buttons. The download button will give the user the entire dataset within the date range selected
      
      # Get the data
      if (input$type == "discrete") {
        subset <- DBI::dbGetQuery(con, paste0("SELECT * FROM measurements_discrete WHERE timeseries_id ", if (length(selected_ids) == 1) paste0("= ", selected_ids) else paste0("IN (", paste(selected_ids, collapse = ", "), ")"), " LIMIT ", length(selected_ids) * 3, ";"))
      } else if (input$type == "continuous") {
        subset <- DBI::dbGetQuery(con, paste0("SELECT * FROM measurements_continuous WHERE timeseries_id ", if (length(selected_ids) == 1) paste0("= ", selected_ids) else paste0("IN (", paste(selected_ids, collapse = ", "), ")"), " LIMIT ", length(selected_ids) * 3, ";"))
      }
      
      output$modal_subset <- DT::renderDataTable({
        DT::datatable(subset, 
                      rownames = FALSE,
                      selection = "none",
                      filter  = "none",
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
                        language = list(
                          info = translations[id == "tbl_info", get(language$language)][[1]],
                          infoEmpty = translations[id == "tbl_info_empty", get(language$language)][[1]],
                          paginate = list(previous = "", `next` = ""),
                          search = translations[id == "tbl_search", get(language$language)][[1]],
                          lengthMenu = translations[id == "tbl_length", get(language$language)][[1]],
                          infoFiltered = translations[id == "tbl_filtered", get(language$language)][[1]],
                          zeroRecords = translations[id == "tbl_zero", get(language$language)][[1]]
                        )
                      )
        )
      })
      # Get the data range
      min_date <- DBI::dbGetQuery(con, paste0("SELECT MIN(start_datetime) FROM timeseries WHERE timeseries_id ", if (length(selected_ids) == 1) paste0("= ", selected_ids) else paste0("IN (", paste(selected_ids, collapse = ", "), ")"), ";"))[[1]]
      max_date <- DBI::dbGetQuery(con, paste0("SELECT MAX(end_datetime) FROM timeseries WHERE timeseries_id ", if (length(selected_ids) == 1) paste0("= ", selected_ids) else paste0("IN (", paste(selected_ids, collapse = ", "), ")"), ";"))[[1]]
      
      # Create the modal
      showModal(modalDialog(
        h3("Subset of data (3 rows per timeseries)"),
        DT::dataTableOutput(ns("modal_subset")),
        selectizeInput(ns("frequency"), label = "Frequency:", choices = c("Daily", "Hourly", "Max resolution"), selected = "Daily"),
        dateRangeInput(ns("modal_date_range"), "Select a date range", start = min_date, end = max_date, min = min_date, max = max_date, format = "yyyy-mm-dd", language = language$abbrev),
        textOutput(ns("num_rows")),
        h4("Download the entire dataset within the date range selected:"),
        downloadButton(ns("download"), "Download"),
        size = "l"
      ))
    })
    
    observe( {
      if (!is.null(input$type)) {
        if (exists("input$frequency")) {
          if (input$type == "discrete") {
            shinyjs::hide("frequency")
          } else {
            shinyjs::show("frequency")
          }
        }
      }
    }
    )
    
    observe({
      req(input$modal_date_range, input$frequency, input$type)
      if (input$frequency == "Daily") {
        output$num_rows <- renderText({paste0("Number of rows that will be returned: " , DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", if (input$type == "discrete") "measurements_discrete" else "calculated_daily", " WHERE timeseries_id ", if (length(selected_ids) == 1) paste0("= ", selected_ids) else paste0("IN (", paste(selected_ids, collapse = ", "), ")"), " AND date > '", input$modal_date_range[1], "' AND ", if (input$type == "discrete") "datetime" else "date", " < '", input$modal_date_range[2], "';"))[[1]])})
      } else if (input$frequency == "Hourly") {
        output$num_rows <- renderText({paste0("Number of rows that will be returned: " , 
        DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM measurements_hourly WHERE timeseries_id ", if (length(selected_ids) == 1) paste0("= ", selected_ids) else paste0("IN (", paste(selected_ids, collapse = ", "), ")"), " AND datetime > '", input$modal_date_range[1], "' AND datetime < '", input$modal_date_range[2], "';"))[[1]])})
      } else {
        output$num_rows <- renderText({paste0("Number of rows that will be returned: " , 
        DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM measurements_continuous WHERE timeseries_id ", if (length(selected_ids) == 1) paste0("= ", selected_ids) else paste0("IN (", paste(selected_ids, collapse = ", "), ")"), " AND datetime > '", input$modal_date_range[1], "' AND datetime < '", input$modal_date_range[2], "';"))[[1]])})
      }
    })
    
  })
}
