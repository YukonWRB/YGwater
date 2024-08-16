checksUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    textOutput(ns("text_output")),
    sidebarLayout(
      sidebarPanel(
        dateRangeInput(ns("dateRange"), "Date range", start = Sys.Date() - 10, end = Sys.Date() - 7, format = "yyyy-mm-dd"),
        selectizeInput(ns("stnGrp"), "Location Group(s)", choices = "Placeholder", multiple = TRUE, options = list(placeholder = "Optional")),
        selectizeInput(ns("stns"), "Locations(s)", choices = "Placeholder", multiple = TRUE, options = list(placeholder = "Optional")),
        selectizeInput(ns("sampleIds"), "Sample ID(s)", choices = "Placeholder", selected = "All", multiple = TRUE, options = list(placeholder = "Optional")),
        actionButton(ns("reset_filters"), "Reset filters"),
        checkboxInput(ns("toggleChecks"), "Show/hide check selection", value = FALSE),
        conditionalPanel(
          condition = "input.toggleChecks",
          checkboxGroupInput(ns("selected_checks"), "Checks to run:",
          c("Variance Analysis", "Field vs lab", "Replicate check", "Blank check", "Molecule/element ratios", "Alkalinity check", "Ion balance", "RDL check", "CN checks", "D <= T"),
          selected = c("Variance Analysis", "Field vs lab", "Replicate check", "Blank check", "Molecule/element ratios", "Alkalinity check", "Ion balance", "RDL check", "CN checks", "D <= T")),
          ns = NS(id),
        ),
        checkboxInput(ns("failedOnly"), "Show only failed checks", value = TRUE),
        actionButton(ns("runChecks"), "Run checks")
      ),
    mainPanel(
      DT::dataTableOutput(ns("failedChecks"))
    )
  )
  )
}

checks <- function(id, con) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # Text above sidebar/main panels
    output$text_output <- renderText({
      "Specify a date range first, which will update the other fields with valid options. You can then optionally filter by location group, location, and sample ID. Click 'Run checks' to see if any matching sampleIDs fail the checks."
    })
    
    
    # Get the data ########################################################################################
    # Load initial data with no filtering applied. This only runs once. If the date range changes the data will be updated in the observeEvent below.
    data_all <- reactiveValues(
      sampleIds = DBI::dbGetQuery(con, paste0("SELECT sampleId, StnId FROM eqsampls WHERE DateValue(CollectDateTime) > '", input$dateRange[1], "' AND DateValue(CollectDateTime) < '", input$dateRange[2], "';")),
      all_groups = DBI::dbGetQuery(con, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqstns'"),
    )
    
    # Update the sampleIds when the date range changes, which will trigger the other data to update
    observeEvent(input$dateRange, {
      data_all$sampleIds <- DBI::dbGetQuery(con, paste0("SELECT sampleId, StnId FROM eqsampls WHERE DateValue(CollectDateTime) > '", input$dateRange[1], "' AND DateValue(CollectDateTime) < '", input$dateRange[2], "';"))
      
      if (!is.null(data_filtered$sampleIds)) {
        # If the date range changes but the reset button wasn't pressed, update the filtered data to include more sampleIds while applying filtering for the stations and groups that are already selected
        # TODO: fill this in
        
        
        
      }
      
    }, ignoreInit = TRUE)
    
    
    
    # This needs to run after the initial data_all creation (and re-creation) so as to use the values of sampleIds$sampleId
    run_filt_data <- reactiveVal(TRUE)
    data_filtered <- reactiveValues()
    
    observe({  # Should really only be depending on data_all$sampleIds, which in turn depends on input$dateRange
      if (is.null(data_all)) {
        return()
      }
      data_all$stns <- DBI::dbGetQuery(con, paste0("SELECT StnCode, StnDesc FROM eqstns WHERE StnId IN (", paste0(data_all$sampleIds$StnId, collapse = ", "), ");"))

      groups <- data_all$all_groups %>% dplyr::mutate(groupitems_list = strsplit(groupitems, ","))
      matching_rows <- sapply(groups$groupitems_list, function(items) {
        any(items %in% data_all$sampleIds$StnId)
      })
      data_all$stnGrps <- groups[matching_rows, c("groupname", "groupdesc", "groupitems_list")]
      
      
      if (run_filt_data()) {
        data_filtered$sampleIds <- data_all$sampleIds
        data_filtered$stnGrps <- data_all$stnGrps
        data_filtered$stns <- data_all$stns
        
        run_filt_data <- FALSE
      }
    })
      
    
    
    # Deal with filtered data and updates to UI elements ############################################
    # Assign the data that will get filtered. This is reset when the 'Reset' button is pushed. Exception is if the date range changes, in which case more sampleIds can be added but the stnGrps and stns will remain the same. This is with the observeEvent on input$date_range above!
    
    observeEvent(input$reset_filters, {
      data_filtered$sampleIds <- data_all$sampleIds
      data_filtered$stnGrps <- data_all$stnGrps
      data_filtered$stns <- data_all$stns
      print(input$selected_checks)
    })
    
    # Observer to update selectizeInputs
    observe({
      updateSelectizeInput(session, "stnGrp", choices = data_filtered$stnGrps$groupname)
      updateSelectizeInput(session, "stns", choices = data_filtered$stns$StnCode)
      updateSelectizeInput(session, "sampleIds", choices = data_filtered$sampleIds$sampleId)
    })
    
    
    # The observers below need to be worked out properly and uncommented when ready.
    # observeEvent(input$stnGrp, {
    #   if (is.null(data_filtered$stnGrps)) {
    #     return()
    #   }
    #   # Get the stations to retain as a char vector
    #   stations <- data_filtered$stnGrps$groupitems_list[data_filtered$stnGrps$groupname %in% input$stnGrp]
    #   data_filtered$stns <- data_filtered$stns[data_filtered$stns$StnId %in% stations, ]
    #   data_filtered$sampleIds <- data_filtered$sampleIds[data_filtered$sampleIds$StnId %in% stations, ]
    # }, ignoreInit = TRUE)
    # 
    # observeEvent(input$stns, {
    #   if (is.null(data_filtered$stns)) {
    #     return()
    #   }
    #   data_filtered$stnGrps <- data_filtered$stnGrps[data_filtered$stnGrps$groupitems_list %in% input$stns, ]
    #   data_filtered$sampleIds <- data_filtered$sampleIds[data_filtered$sampleIds$StnId %in% input$stns, ]
    # }, ignoreInit = TRUE)
    # observeEvent(input$sampleIds, {
    #   if (is.null(data_filtered$sampleIds)) {
    #     return()
    #   }
    #   data_filtered$stnGrps <- data_filtered$stnGrps[data_filtered$stnGrps$groupitems_list %in% input$sampleIds, ]
    #   data_filtered$stns <- data_filtered$stns[data_filtered$stns$StnId %in% input$sampleIds, ]
    # }, ignoreInit = TRUE)
    
    
    # Run the checks ##############################################################
    observeEvent(input$runChecks, {
      if (input$sampleIds == "") { #User is not using this filter but may be using others, so use data_filtered$sampleIds
        sampleIds <- data_filtered$sampleIds$sampleId
      } else { #User has selected one or more sampleId
        sampleIds <- input$sampleIds
      }
      
      # Run checks on each sampleId
      checks <- list()
      for (i in sampleIds) {
        # Run checks for each test selected by the user in the checkboxGroupInput. These come out as as a character vector in input$selected_checks of length = whatever is selected     
        for (j in input$selected_checks) {
          # checks[[i]] <- ??
        }
        # checks[[i]] <- ??
      }
      
    })
    
  })
}
