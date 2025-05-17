
dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(
        "// Make the ...preparing download... notification stand out more
        .shiny-notification {
          font-size: 24px;
          font-weight: bold;
          background-color: #f9f9f9;  /* Light grey background */
          color: #333;  /* Dark grey text */
          padding: 15px;  /* Larger padding for more space */
          border-left: 5px solid #007BFF;  /* Blue left border */
          border-right: 5px solid #007BFF;  /* Blue right border */
          border-top: 5px solid #007BFF;  /* Blue top border */
          border-bottom: 5px solid #007BFF;  /* Blue bottom border */
          border-radius: 10px;  /* Rounded corners */
        }"
      )
    ),
    tags$script(
      HTML("
      // Handles tooltip updates outside of the datatable, binds tooltip properties to elements
      Shiny.addCustomMessageHandler('update-tooltip', function(message) {
        var selector = '#' + message.id;
        $(selector).attr('title', message.title)
        .tooltip('fixTitle').tooltip('hide');
      });
      
      // Handles tootip creation and update for the datatable headers
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
    });"
      )
    ),
    sidebarPanel(
      span(
        id = ns("infoIcon"),
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        `data-trigger` = "click hover",
        title = "Placeholder",
        icon("info-circle", style = "font-size: 150%;")),
      selectizeInput(ns("type"), "Data Type", choices = c("Discrete" = "discrete"), multiple = FALSE), # choices and labels are updated in the server module
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
      # The modal UI elements are created in the server module
    )
    
  ) # end tagList
} # end dataUI


data <- function(id, con, language, restoring, data, inputs) {
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c(
      "reset", 
      "view_data", 
      "tbl_rows_selected", 
      "tbl_columns_selected",
      "tbl_cells_selected",
      "tbl_rows_current",
      "tbl_rows_all",
      "tbl_state",
      "tbl_search",
      "tbl_cell_clicked",
      "tbl_row_last_clicked",
      
      "modal_location_metadata_rows_selected",
      "modal_location_metadata_columns_selected",
      "modal_location_metadata_cells_selected",
      "modal_location_metadata_rows_current",
      "modal_location_metadata_rows_all",
      "modal_location_metadata_state",
      "modal_location_metadata_search",
      "modal_location_metadata_cell_clicked",
      
      "modal_subset_rows_selected",
      "modal_subset_columns_selected",
      "modal_subset_cells_selected",
      "modal_subset_rows_current",
      "modal_subset_rows_all",
      "modal_subset_state",
      "modal_subset_search",
      "modal_subset_cell_clicked",
      
      "modal_date_range",
      "modal_frequency",
      "modal_format"
    ))
    ns <- session$ns
    
    # Function to create modals with a custom ID ################################
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
    
    
    # Update text based on language ###########################################
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
                           label = translations[id == "media_id", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$media_types$media_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$media_types[[translations[id == "media_type_col", get(language$language)][[1]]]], language$abbrev)
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
                           choices = stats::setNames(c("All", data$parameters$parameter_id),
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
                           choices = stats::setNames(c("All", data$media_types$media_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$media_types[[translations[id == "media_type_col", get(language$language)][[1]]]], language$abbrev)
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
                           choices = stats::setNames(c("All", data$parameters$parameter_id),
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
      
      # apply the location filter first, since it's the most restrictive and since the map page click through uses it as the only filter
      if (!is.null(input$loc) & !("All" %in% input$loc)) {
        tbl <- data$timeseries[location_id %in% input$loc]
      } else {
        tbl <- data$timeseries
      }
      
      # Apply filters for type, pType, pGrp, param, proj, net, yrs in succession
      if (!is.null(input$type)) {
        if (length(input$type) > 1) {
          tbl <- tbl[tbl$category %in% input$type, ]
        } else {
          if (input$type != "All") {
            tbl <- tbl[tbl$category == input$type, ]
          }
        }
      }
      
      if (!is.null(input$pType)) {
        if (length(input$pType) > 1) {
          tbl <- tbl[tbl$media_id %in% input$pType, ]
        } else {
          if (input$pType != "All") {
            tbl <- tbl[tbl$media_id == input$pType, ]
          }
        }
      }
      
      if (!is.null(input$pGrp)) {
        if (length(input$pGrp) > 1) {
          select.params <- data$parameters[data$parameters$group %in% input$pGrp, "parameter_id"]$parameter_id
          tbl <- tbl[parameter %in% select.params, ]
        } else {
          if (input$pGrp != "All") {
            select.params <- data$parameters[data$parameters$group == input$pGrp, "parameter_id"]$parameter_id
            if (length(select.params) > 1) {
               tbl <- tbl[parameter %in% select.params, ]
            } else {
              tbl <- tbl[parameter == select.params, ]
            }
          }
        }
      }
      
      if (!is.null(input$param)) {
        if (length(input$param) > 1) {
          tbl <- tbl[parameter %in% input$param, ]
        } else {
          if (input$param != "All") {
            tbl <- tbl[parameter == input$param, ]
          }
        }
      }
      
      if (!is.null(input$proj)) {
        if (length(input$proj) > 1) {
          ids <- data$locations_projects[data$locations_projects$project_id %in% input$proj, "location_id"]$location_id
          if (length(ids) > 1) {
            tbl <- tbl[location_id %in% ids, ]
          } else {
            tbl <- tbl[location_id == ids, ]
          }
        } else {
          if (input$proj != "All") {
            ids <- data$locations_projects[data$locations_projects$project_id == input$proj, "location_id"]$location_id
            if (length(ids) > 1) {
              tbl <- tbl[location_id %in% ids, ]
            } else {
              tbl <- tbl[location_id == ids, ]
            }
          }
        }
      }
      
      if (!is.null(input$net)) {
        if (length(input$net) > 1) {
          ids <- data$locations_networks[data$locations_networks$network_id %in% input$net, "location_id"]$location_id
          if (length(ids) > 1) {
            tbl <- tbl[location_id %in% ids, ]
          } else {
            tbl <- tbl[location_id == ids, ]
          }
        } else {
          if (input$net != "All") {
            ids <- data$locations_networks[data$locations_networks$network_id == input$net, "location_id"]$location_id
            if (length(ids) > 1) {
              tbl <- tbl[location_id %in% ids, ]
            } else {
              tbl <- tbl[location_id == ids, ]
            }
          }
        }
      }
      
      tbl <- tbl[tbl$start_datetime <= as.POSIXct(paste0(input$yrs[2], "-12-31 23:59:59"), tz = "UTC") & tbl$end_datetime >= as.POSIXct(paste0(input$yrs[1], "-01-01 00:00"), tz = "UTC"), ]
      
      
      
      # Combine period type and record rate for ease of viewing
      tbl[, measurement_type := paste0(aggregation_type, " (", record_rate, ")")]
      
        # Drop aggregation_type
        tbl[, aggregation_type := NULL]
        tbl[, record_rate := NULL]
        
        
        # Attach location
        tbl[data$locations, on = c(location_id = "location_id"), translations[id == "loc", get(language$language)] := .(get(translations[id == "generic_name_col", get(language$language)]))]
        # Attach parameter type
        tbl[data$media_types, on = c(media_id = "media_id"), 
            translations[id == "type", get(language$language)] := get(translations[id == "media_type_col", get(language$language)])]
        tbl[, media_id := NULL]
        
        # Attach parameter descriptions
        tbl[data$parameters, on = c(parameter = "parameter_id"), 
            c(translations[id == "group", get(language$language)], translations[id == "parameter", get(language$language)], translations[id == "units", get(language$language)]) 
            := 
              .(get(translations[id == "param_group_col", get(language$language)]), get(translations[id == "param_name_col", get(language$language)]), unit)] # data in column named "unit" is unchanging based on language
        
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
        data.table::setnames(tbl, old = c("start_datetime", "end_datetime", "category", "measurement_type"), new = c(translations[id == "start", get(language$language)], translations[id == "end", get(language$language)], translations[id == "category", get(language$language)], translations[id == "measurement_type", get(language$language)]))

        # titleCase column names
        data.table::setnames(tbl, old = names(tbl)[-c(1,2,12,13)], new = titleCase(names(tbl)[-c(1,2,12,13)], language$abbrev))
        
        # titleCase columns
        for (j in c(7L)) data.table::set(tbl, j = j, value = titleCase(tbl[[j]], language$abbrev))
        
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
                                   list(targets = 4, orderData = 12), # Order the character datetime column using the hidden true datetime. Column numbers are true.
                                   list(targets = 5, orderData = 13), # Order the character datetime column using the hidden true datetime. Column numbers are true.
                                   list(targets = c(0,1,11,12), visible = FALSE), #Hides the timeseries_id and datetime sorting columns. Column index numbers start at 0 here!!!
                                   list(
                                     targets = c(6), # Column index numbers start at 0 here again!!!
                                     render = htmlwidgets::JS( # Truncate long strings in the table
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data !== null && data.length > 20 ?",
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
                             pageLength = 10
                               )
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
      # Get the timeseries_ids of the selected rows
      selected_tsids <- table_data()[input$tbl_rows_selected, timeseries_id]
      selected_loc_ids <- table_data()[input$tbl_rows_selected, location_id]
      
      # Query will be for discrete or continuous data, depending on input$type
      # Show a modal with a subset (first 3 rows per timeseries_id) of the data. Below this, show a date range picker (with min/max preset based on the selected data), the number of rows that would be returned, and download and close buttons. The download button will give the user the entire dataset within the date range selected
      
      # Get the timeseries and location data
      if (input$type == "discrete") {
        subset_list <- vector("list", length(selected_tsids)) 
        for (i in seq_along(selected_tsids)) {
          query <- sprintf("SELECT timeseries_id, target_datetime, datetime, value, sample_class, note FROM measurements_discrete WHERE timeseries_id = %d ORDER BY datetime LIMIT 3;", selected_tsids[i])
          subset_list[[i]] <- dbGetQueryDT(con, query)
        }
        subset <- data.table::rbindlist(subset_list)
        subset[, value := round(value, 2)]  # Round the 'value' column
        subset[, datetime := substr(as.character(datetime), 1, 16)]  # Truncate 'datetime'
        subset[, target_datetime := substr(as.character(target_datetime), 1, 16)]  # Truncate 'target_datetime'
        
      } else if (input$type == "continuous") {
        subset_list <- vector("list", length(selected_tsids))
        for (i in seq_along(selected_tsids)) {
          query <- sprintf("SELECT timeseries_id, date, value, grade, approval, imputed, percent_historic_range, max, min, q90, q75, q50, q25, q10, mean, doy_count FROM measurements_calculated_daily_corrected WHERE timeseries_id = %d ORDER BY date LIMIT 3;", selected_tsids[i])
          subset_list[[i]] <- dbGetQueryDT(con, query)
        }
        subset <- data.table::rbindlist(subset_list)
        subset[, c(3, 7:15) := lapply(.SD, round, 2), .SDcols = c(3, 7:15)]
      }
      
      output$modal_subset <- DT::renderDataTable({  # Create datatable for the measurements
        DT::datatable(subset,
                      rownames = FALSE,
                      selection = "none",
                      filter  = "none",
                      options = list(
                        scrollX = TRUE,
                        initComplete = htmlwidgets::JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({",
                          "  'background-color': '#079',",
                          "  'color': '#fff',",
                          "  'font-size': '100%',",
                          "});",
                          "$(this.api().table().body()).css({",
                          "  'font-size': '90%',",
                          "});",
                          "}"
                        ),
                        language = list(
                          info = "",
                          infoEmpty = translations[id == "tbl_info_empty", get(language$language)][[1]],
                          paginate = list(previous = "", `next` = ""),
                          search = translations[id == "tbl_search", get(language$language)][[1]],
                          infoFiltered = "",
                          zeroRecords = translations[id == "tbl_zero", get(language$language)][[1]]
                        ),
                        dom = 'rtip'
                      )
        )
      }) # End of function creating data subset datatable
      
      location <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", if (language$abbrev == "en") "location_metadata_en" else "location_metadata_fr", " WHERE location_id ", if (length(selected_loc_ids) == 1) paste0("= ", selected_loc_ids) else paste0("IN (", paste(selected_loc_ids, collapse = ", "), ")"), " LIMIT 3;")) # Get the location metadata
      location[, c(4:6)] <- round(location[, c(4:6)], 2)
      
      output$modal_location_metadata <- DT::renderDataTable({  # Create datatable for the locations
        DT::datatable(location,
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
                          "});",
                          "$(this.api().table().body()).css({",
                          "  'font-size': '90%',",
                          "});",
                          "}"
                        ),
                        columnDefs = list(
                          list(
                            targets = c(2, 6:9), # Column index numbers start at 0 here again!!!
                            render = htmlwidgets::JS( # Truncate long strings in the table
                              "function(data, type, row, meta) {",
                              "return type === 'display' && data !== null && data.length > 20 ?",
                              "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                              "}")
                          )
                        ),
                        language = list(
                          info = "",
                          infoEmpty = translations[id == "tbl_info_empty", get(language$language)][[1]],
                          paginate = list(previous = "", `next` = ""),
                          search = "",
                          infoFiltered = "",
                          zeroRecords = translations[id == "tbl_zero", get(language$language)][[1]]
                        ),
                        dom = 'rt',
                        scrollX = TRUE
                      )
        )
      }) # End of function creating location metatadata datatable
      
      # Get the data temporal range
      min_date <- DBI::dbGetQuery(con, paste0("SELECT MIN(start_datetime) FROM timeseries WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), ";"))[[1]]
      max_date <- DBI::dbGetQuery(con, paste0("SELECT MAX(end_datetime) FROM timeseries WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), ";"))[[1]]

      # Create the modal
      showModal(modalDialog(
        h4(translations[id == "data_subset_msg", get(language$language)][[1]]),
        DT::dataTableOutput(ns("modal_subset")),
        h4(translations[id == "loc_meta_msg", get(language$language)][[1]]),
        DT::dataTableOutput(ns("modal_location_metadata")),
        h4(translations[id == "extra_tbl_msg", get(language$language)][[1]]),
        selectizeInput(ns("modal_frequency"), label = translations[id == "frequency", get(language$language)][[1]], choices = stats::setNames(c("daily", "hourly", "max"), c(translations[id == "daily", get(language$language)][[1]], translations[id == "hourly", get(language$language)][[1]], translations[id == "max", get(language$language)][[1]])), selected = "daily"),
        dateRangeInput(ns("modal_date_range"), label = translations[id == "date_range_select", get(language$language)][[1]], start = min_date, end = max_date, min = min_date, max = max_date, format = "yyyy-mm-dd", language = language$abbrev),
        textOutput(ns("num_rows")),
        selectizeInput(ns("modal_format"), label = translations[id == "dl_format", get(language$language)][[1]], choices = stats::setNames(c("xlsx", "csv", "sqlite"), c(translations[id == "dl_format_xlsx", get(language$language)][[1]], translations[id == "dl_format_csv", get(language$language)][[1]], translations[id == "dl_format_sqlite", get(language$language)][[1]])), selected = "xlsx"),
        downloadButton(ns("download"), translations[id == "dl_data", get(language$language)][[1]]),
        size = "l"
      ))
    })
    
    observe( {
      if (!is.null(input$type)) {
        if (exists("input$modal_frequency")) {
          if (input$type == "discrete") {
            shinyjs::hide("modal_frequency")
          } else {
            shinyjs::show("modal_frequency")
          }
        }
      }
    }
    )
    
    # Updates to modal ########################################################
    # Get the number of rows that will be returned based on the date range selected and update the subset table if necessary
    observe({
      req(input$type, input$modal_date_range, input$tbl_rows_selected)
      selected_tsids <- table_data()[input$tbl_rows_selected, timeseries_id]
      if (input$type == "continuous") {
        req(input$modal_frequency)
        if (input$modal_frequency == "daily") {
          rows <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM measurements_calculated_daily_corrected", " WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), " AND date > '", input$modal_date_range[1], "' AND date", " < '", input$modal_date_range[2], "';"))[[1]]
          
          subset_list <- vector("list", length(selected_tsids))
          for (i in seq_along(selected_tsids)) {
            query <- sprintf("SELECT timeseries_id, date, value, grade, approval, imputed, percent_historic_range, max, min, q90, q75, q50, q25, q10, mean, doy_count FROM measurements_calculated_daily_corrected WHERE timeseries_id = %d ORDER BY date LIMIT 3;", selected_tsids[i])
            subset_list[[i]] <- dbGetQueryDT(con, query)
          }
          subset <- data.table::rbindlist(subset_list)
          subset[, c(3, 7:15) := lapply(.SD, round, 2), .SDcols = c(3, 7:15)]
        } else if (input$modal_frequency == "hourly") {
          rows <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM measurements_hourly_corrected WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), " AND datetime > '", input$modal_date_range[1], "' AND datetime < '", input$modal_date_range[2], "';"))[[1]]
          
          subset_list <- vector("list", length(selected_tsids))
          for (i in seq_along(selected_tsids)) {
            subset_list[[i]] <- dbGetQueryDT(con, paste0("SELECT timeseries_id, datetime, value_corrected AS value, grade, approval, imputed FROM measurements_continuous_corrected WHERE timeseries_id = ", selected_tsids[i], " ORDER BY datetime LIMIT 3;"))
          }
          subset <- data.table::rbindlist(subset_list)
          subset[, datetime := substr(as.character(datetime), 1, 16)]
          subset[, value := round(value, 2)]
        } else {
          rows <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM measurements_continuous_corrected WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), " AND datetime > '", input$modal_date_range[1], "' AND datetime < '", input$modal_date_range[2], "';"))[[1]]
          
          subset_list <- vector("list", length(selected_tsids))
          for (i in seq_along(selected_tsids)) {
            subset_list[[i]] <- dbGetQueryDT(con, paste0("SELECT timeseries_id, datetime, value_corrected AS value, grade, approval, imputed, period FROM measurements_continuous_corrected WHERE timeseries_id = ", selected_tsids[i], " ORDER BY datetime LIMIT 3;"))
          }
          subset <- data.table::rbindlist(subset_list)
          subset[, datetime := substr(as.character(datetime), 1, 16)]
          subset[, value := round(value, 2)]
        } 
        output$modal_subset <- DT::renderDataTable({ # Create datatable for the measurements
          DT::datatable(subset,
                        rownames = FALSE,
                        selection = "none",
                        filter  = "none",
                        options = list(
                          scrollX = TRUE,
                          initComplete = htmlwidgets::JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({",
                            "  'background-color': '#079',",
                            "  'color': '#fff',",
                            "  'font-size': '100%',",
                            "});",
                            "$(this.api().table().body()).css({",
                            "  'font-size': '90%',",
                            "});",
                            "}"
                          ),
                          language = list(
                            info = "",
                            infoEmpty = translations[id == "tbl_info_empty", get(language$language)][[1]],
                            paginate = list(previous = "", `next` = ""),
                            search = translations[id == "tbl_search", get(language$language)][[1]],
                            infoFiltered = "",
                            zeroRecords = translations[id == "tbl_zero", get(language$language)][[1]]
                          ),
                          dom = 'rtip'
                        )
          )
        }) # End of function re-creating data subset datatable
      } else { # type is discrete
        rows <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM measurements_discrete WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), " AND datetime > '", input$modal_date_range[1], "' AND datetime < '", input$modal_date_range[2], "';"))[[1]]
      }
      
      # ouput message about number of rows
      output$num_rows <- renderText({ paste0(translations[id == "dl_num_rows", get(language$language)][[1]], " ", rows) })
      
      # Update selectizeInput based on number of rows
      if (rows > 1000000) {
        updateSelectizeInput(session, "modal_format", label = translations[id == "dl_format_no_xlsx", get(language$language)][[1]],  choices = stats::setNames(c("csv", "sqlite"), c(translations[id == "dl_format_csv", get(language$language)][[1]], translations[id == "dl_format_sqlite", get(language$language)][[1]])), selected = "csv")
      } else {
        updateSelectizeInput(session, "modal_format", label = translations[id == "dl_format", get(language$language)][[1]], choices = stats::setNames(c("xlsx", "csv", "sqlite"), c(translations[id == "dl_format_xlsx", get(language$language)][[1]], translations[id == "dl_format_csv", get(language$language)][[1]], translations[id == "dl_format_sqlite", get(language$language)][[1]])), selected = "xlsx")  
      }
    }) # End of observe for number of rows
      
    # Download handling #######################################################
    output$download <- downloadHandler(
      filename = function() {
        paste0("data_", format(Sys.time(), "%Y%m%d_%H%M%S%Z"), ".", if (input$modal_format == "csv") "zip" else input$modal_format)
      },
      content = function(file) {
        
        showNotification(translations[id == "dl_prep", get(language$language)][[1]], id = "download_notification", duration = NULL, type = "message")
        
        # Get the data together
        selected_tsids <- table_data()[input$tbl_rows_selected, timeseries_id]
        selected_loc_ids <- table_data()[input$tbl_rows_selected, location_id]
        
        data <- list()
        data$location_metadata <- dbGetQueryDT(con, paste0("SELECT * FROM ", if (language$language == "Français") "location_metadata_fr" else "location_metadata_en", " WHERE location_id ", if (length(selected_loc_ids) == 1) paste0("= ", selected_loc_ids) else paste0("IN (", paste(selected_loc_ids, collapse = ", "), ")"), ";"))
        data$timeseries_metadata <- dbGetQueryDT(con, paste0("SELECT * FROM ", if (language$language == "Français") "timeseries_metadata_fr" else "timeseries_metadata_en", " WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), ";"))
        data$grades <- dbGetQueryDT(con, "SELECT * FROM grades;")
        data$approvals <- dbGetQueryDT(con, "SELECT * FROM approvals;")
        
        if (input$type == "discrete") {
          data$measurements <- dbGetQueryDT(con, paste0("SELECT timeseries_id, target_datetime, datetime, value, sample_class, note FROM measurements_discrete WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), " AND datetime > '", input$modal_date_range[1], "' AND datetime < '", input$modal_date_range[2], "';"))
          
        } else if (input$type == "continuous") {
          data$measurements_daily <- dbGetQueryDT(con, paste0("SELECT timeseries_id, date, value, grade, approval, imputed, percent_historic_range, max, min, q90, q75, q50, q25, q10, mean, doy_count FROM measurements_calculated_daily_corrected WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), " AND date > '", input$modal_date_range[1], "' AND date < '", input$modal_date_range[2], "';"))
          
          # Now add hourly or max resolution data if selected
          if (input$modal_frequency == "hourly") {
            data$measurements_hourly_corrected <- dbGetQueryDT(con, paste0("SELECT * FROM measurements_hourly_corrected WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), " AND datetime > '", input$modal_date_range[1], "' AND datetime < '", input$modal_date_range[2], "';"))
          } else if (input$modal_frequency == "max") {
            data$measurements_max <- dbGetQueryDT(con, paste0("SELECT timeseries_id, datetime, value_corrected AS value, grade, approval, period, imputed FROM measurements_continuous_corrected WHERE timeseries_id ", if (length(selected_tsids) == 1) paste0("= ", selected_tsids) else paste0("IN (", paste(selected_tsids, collapse = ", "), ")"), " AND datetime > '", input$modal_date_range[1], "' AND datetime < '", input$modal_date_range[2], "';"))
          }
        }
        
        if (input$modal_format == "xlsx") {
          openxlsx::write.xlsx(data, file)
        } else if (input$modal_format == "csv") {
          # Temporary directory to store CSV files
          temp_dir <- tempdir()
          csv_files <- lapply(names(data), function(name) {
            file_name <- file.path(temp_dir, paste0(name, ".csv"))
            data.table::fwrite(data[[name]], file_name)
            return(file_name)
          })
          # Use zip to compress the files
          utils::zip(file, unlist(csv_files))
        } else if (input$modal_format == "sqlite") {
          # Create an sqlite database and write the data tables to it
          db <- DBI::dbConnect(RSQLite::SQLite(), dbname = file)
          lapply(names(data), function(name) {
            DBI::dbWriteTable(conn = db, name = name, value = data[[name]], overwrite = TRUE)
          })
          DBI::dbDisconnect(db)
        }
        removeNotification("download_notification")
        # session$sendCustomMessage('close-modal', list(modalId = 'downloadModal'))
      }) # End of downloadHandler
    
  }) # End of moduleServer
} # End of data server module
