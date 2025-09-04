# UI and server code for water quality guidelines management module

addGuidelinesUI <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      title = NULL,
      position = "right",
      width = "50%",
      bg = config$sidebar_bg,
      open = list(mobile = "always-above"),
      
      
      textInput(ns("guideline_name"), 
                "Guideline Name",
                width = "100%"),
      textInput(ns("publisher"), 
                "Publisher",
                placeholder = "CA-CCME, US-EPA, BC-MOE, etc.",
                width = "100%"),
      textInput(ns("reference"),
                "Reference",
                placeholder = "URL, DOI, etc.",
                width = "100%"),
      textAreaInput(ns("note"), 
                    "Note", 
                    placeholder = "Optional", 
                    width = "100%", 
                    height = "100px"),
      selectizeInput(ns("parameter_id"), 
                     "Parameter", 
                     choices = NULL, 
                     width = "100%", 
                     multiple = TRUE,
                     options = list(maxItems = 1)),
      selectizeInput(ns("sample_fraction"), 
                     "Sample Fraction", 
                     choices = NULL, 
                     width = "100%",
                     multiple = TRUE,
                     options = list(maxItems = 1)),
      selectizeInput(ns("result_speciation"),
                     "Result Speciation", 
                     choices = NULL, 
                     width = "100%",
                     multiple = TRUE,
                     options = list(maxItems = 1)),
      actionLink(ns("open_guideline_help"), 
                 label = "Guideline SQL help", 
                 icon = icon("book")),
      textAreaInput(ns("guideline_sql"), 
                    "Guideline SQL", 
                    width = "100%", 
                    height = "300px"),
      actionButton(ns("save_guideline"), 
                   "Update/save guideline", 
                   width = "100%", 
                   style = "display: none")
    ),
    div( # Rendered within a div so that the buttons are close to the table
      DT::DTOutput(ns("guidelines_table")
      ),
      # Small space between table and button
      br(),
      splitLayout(
        cellWidths = c("50%", "50%"),
        actionButton(ns("add_guideline"), 
                     "Add new guideline",
                     style = "font-size: 14px;", 
                     class = "btn btn-primary"),
        actionButton(ns("delete_guideline"),
                     "Delete selected guideline",
                     style = "font-size: 14px;", 
                     class = "btn btn-primary")
      )
    )
  )
}

addGuidelines <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    moduleData <- reactiveValues(
      guidelines = DBI::dbGetQuery(session$userData$AquaCache, "SELECT g.guideline_id, g.guideline_name, g.publisher, g.note, g.reference, p.param_name AS parameter, p.unit_default AS units, sf.sample_fraction AS fraction, rs.result_speciation AS speciation, g.parameter_id, g.sample_fraction_id, g.result_speciation_id, g.guideline_sql FROM discrete.guidelines as g LEFT JOIN sample_fractions sf ON sf.sample_fraction_id = g.sample_fraction_id JOIN public.parameters as p ON p.parameter_id = g.parameter_id LEFT JOIN result_speciations rs ON rs.result_speciation_id = g.result_speciation_id"),
      parameters = DBI::dbGetQuery(session$userData$AquaCache, "SELECT parameter_id, param_name, unit_default FROM public.parameters ORDER BY param_name"),
      sample_fractions = DBI::dbGetQuery(session$userData$AquaCache, "SELECT sample_fraction_id, sample_fraction FROM discrete.sample_fractions ORDER BY sample_fraction"),
      result_speciations = DBI::dbGetQuery(session$userData$AquaCache, "SELECT result_speciation_id, result_speciation FROM discrete.result_speciations ORDER BY result_speciation")
    )
    
    # Create a temporary copy of the guidelines data to track changes before saving
    moduleData$guidelines_temp <- moduleData$guidelines
    
    output$guidelines_table <- DT::renderDT({
      DT::datatable(moduleData$guidelines, 
                    selection = "single", 
                    rownames = FALSE,
                    options = list(pageLength = 10, 
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
                                   layout = list(
                                     bottomStart = 'info',
                                     bottomEnd   = 'paging'
                                   ),
                                   columnDefs = list(
                                     list(
                                       targets = c(0,9:12),  # guideline_id and SQL columns; index starts at 0
                                       visible = FALSE
                                     )
                                   )
                    )
      )
    }, server = TRUE)
    
    # Update parameter and sample fraction choices when moduleData changes
    observe({
      updateSelectizeInput(session, "parameter_id", 
                           choices = setNames(moduleData$parameters$parameter_id, 
                                              paste0(moduleData$parameters$param_name,
                                                     " (", 
                                                     moduleData$parameters$unit_default, ")")),
                           selected = NULL)
      updateSelectizeInput(session, "sample_fraction", 
                           choices = setNames(moduleData$sample_fractions$sample_fraction_id, 
                                              moduleData$sample_fractions$sample_fraction),
                           selected = NULL)
      updateSelectizeInput(session, "result_speciation", 
                           choices = setNames(moduleData$result_speciations$result_speciation_id, 
                                              moduleData$result_speciations$result_speciation),
                           selected = NULL)
    })
    
    # Show the user help file as a new html document:
    observeEvent(input$open_guideline_help, {
      # opens in a new tab
      shinyjs::runjs("window.open('html/guidelines_help.html', '_blank');")
    })
    
    # Clicking 'Add Guideline' creates a new row in the data.frame, which in turn updates the datatable
    observeEvent(input$add_guideline, {
      new <- data.frame(
        guideline_id = -1,  # Identifies the newly added row
        guideline_name = "",
        publisher = "",
        reference = "",
        note = "",
        parameter = "",
        units = "",
        fraction = "",
        speciation = "",
        parameter_id = NA,
        sample_fraction_id = NA,
        result_speciation_id = NA,
        guideline_sql = ""
      )
      moduleData$guidelines <- rbind(moduleData$guidelines, new)   # Appended here so that the data.table updates
      moduleData$guidelines_temp <- rbind(moduleData$guidelines_temp, new)
      
      # Select the new row in the data.table
      DT::dataTableProxy("guidelines_table") %>% DT::selectRows(nrow(moduleData$guidelines))
      
      # Show the user a modal telling them to edit their new guideline to the right
      showModal(modalDialog(
        "Edit your new guideline in the form to the right, then click 'Save Guideline' to save it to the database.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      ))
    })
    
    # Respond to row selections in the datatable and update the left sidebar inputs accordingly
    observeEvent(input$guidelines_table_rows_selected, {
      req(input$guidelines_table_rows_selected)
      selected_row <- input$guidelines_table_rows_selected
      guideline <- moduleData$guidelines[selected_row, ]
      
      updateTextInput(session, "guideline_name", value = guideline$guideline_name)
      updateTextInput(session, "publisher", value = guideline$publisher)
      updateTextInput(session, "reference", value = guideline$reference)
      updateTextAreaInput(session, "note", value = guideline$note)
      updateSelectizeInput(session, "parameter_id", selected = guideline$parameter_id)
      updateSelectizeInput(session, "sample_fraction", selected = guideline$sample_fraction_id)
      updateSelectizeInput(session, "result_speciation", selected = guideline$result_speciation_id)
      updateTextAreaInput(session, "guideline_sql", value = guideline$guideline_sql)
      
      shinyjs::show("save_guideline")
    })
    
    # Observe changes in the input fields and update the reactiveValues data.frame accordingly
    observe({
      req(input$guidelines_table_rows_selected)
      selected_row <- input$guidelines_table_rows_selected
      
      moduleData$guidelines_temp[selected_row, "guideline_name"] <- if (nchar(input$guideline_name) == 0) NA else input$guideline_name
      moduleData$guidelines_temp[selected_row, "publisher"] <- if (nchar(input$publisher) == 0) NA else input$publisher
      moduleData$guidelines_temp[selected_row, "reference"] <- if (nchar(input$reference) == 0) NA else input$reference
      moduleData$guidelines_temp[selected_row, "note"] <- if (nchar(input$note) == 0) NA else input$note
      moduleData$guidelines_temp[selected_row, "parameter_id"] <- if (is.null(input$parameter_id)) NA else input$parameter_id
      moduleData$guidelines_temp[selected_row, "sample_fraction_id"] <- if (is.null(input$sample_fraction)) NA else input$sample_fraction
      moduleData$guidelines_temp[selected_row, "result_speciation_id"] <- if (is.null(input$result_speciation)) NA else input$result_speciation
      moduleData$guidelines_temp[selected_row, "guideline_sql"] <- if (nchar(input$guideline_sql) == 0) NA else input$guideline_sql
    })
    
    # Save changes to the database when 'Save Guideline' is clicked
    observeEvent(input$save_guideline, {
      req(input$guidelines_table_rows_selected)
      selected_row <- input$guidelines_table_rows_selected
      guideline <- moduleData$guidelines_temp[selected_row, ]
      
      # Basic validation
      if (is.na(guideline$guideline_name) || is.na(guideline$parameter_id) || is.na(guideline$publisher) || is.na(guideline$guideline_sql)) {
        showModal(modalDialog(
          title = "Error",
          "Please fill in all required fields: Guideline Name, Publisher, Parameter, and Guideline SQL.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
        return()
      }
      
      tryCatch({
        
        sf <- if (is.null(guideline$sample_fraction_id) || is.na(guideline$sample_fraction_id) || guideline$sample_fraction_id == "")
          NA_integer_ else as.integer(guideline$sample_fraction_id)
        rs <- if (is.null(guideline$result_speciation_id) || is.na(guideline$result_speciation_id) || guideline$result_speciation_id == "")
          NA_integer_ else as.integer(guideline$result_speciation_id)
        ref <- if (is.null(guideline$reference) || is.na(guideline$reference) || guideline$reference == "")
          NA_character_ else guideline$reference
        
        if (guideline$guideline_id == -1) {
          # New guideline - perform INSERT
          DBI::dbExecute(session$userData$AquaCache, 
                         "INSERT INTO discrete.guidelines (guideline_name, publisher, reference, note, parameter_id, sample_fraction_id, result_speciation_id, guideline_sql) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
                         params = list(guideline$guideline_name, guideline$publisher, guideline$reference, guideline$note, guideline$parameter_id, sf, rs, guideline$guideline_sql))
        } else {
          # Existing guideline - perform UPDATE
          DBI::dbExecute(session$userData$AquaCache, 
                         "UPDATE discrete.guidelines SET guideline_name = $1, publisher = $2, reference = $3, note = $4, parameter_id = $5, sample_fraction_id = $6, result_speciation_id = $7, guideline_sql = $8 WHERE guideline_id = $9",
                         params = list(guideline$guideline_name, guideline$publisher, guideline$reference, guideline$note, guideline$parameter_id, sf, rs, guideline$guideline_sql, guideline$guideline_id))
        }
        
        # Refresh the guidelines data from the database
        moduleData$guidelines <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT g.guideline_id, g.guideline_name, g.publisher, g.note, g.reference, p.param_name AS parameter, p.unit_default AS units, sf.sample_fraction AS fraction, rs.result_speciation AS speciation, g.parameter_id, g.sample_fraction_id, g.result_speciation_id, g.guideline_sql FROM discrete.guidelines as g LEFT JOIN sample_fractions sf ON sf.sample_fraction_id = g.sample_fraction_id JOIN public.parameters as p ON p.parameter_id = g.parameter_id LEFT JOIN result_speciations rs ON rs.result_speciation_id = g.result_speciation_id")
        moduleData$guidelines_temp <- moduleData$guidelines
        
        # Clear selection and hide save button
        DT::dataTableProxy("guidelines_table") %>% DT::selectRows(NULL)
        shinyjs::hide("save_guideline")
        showModal(modalDialog(
          title = "Success",
          "Guideline saved successfully.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred while saving the guideline: ", e$message),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
      })
    })
    
    # Observe a request to delete a guideline
    observeEvent(input$delete_guideline, {
      req(input$guidelines_table_rows_selected)
      
      selected_row <- input$guidelines_table_rows_selected
      guideline <- moduleData$guidelines[selected_row, ]

      showModal(modalDialog(
        title = "Confirm Deletion",
        paste("Are you sure you want to delete the guideline '", guideline$guideline_name, "'? This action cannot be undone.", sep = ""),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete"), "Delete", class = "btn btn-danger")
        )
      ))
    })
      
      observeEvent(input$confirm_delete, {
        removeModal()
        
        tryCatch({
          if (guideline$guideline_id != -1) {  # Only attempt to delete if it's not a new unsaved guideline
            DBI::dbExecute(session$userData$AquaCache, "DELETE FROM discrete.guidelines WHERE guideline_id = $1", params = list(guideline$guideline_id))
          }
          
          # Refresh the data, which will re-render the datatable without the deleted row
          moduleData$guidelines <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT g.guideline_id, g.guideline_name, g.publisher, g.note, g.reference, p.param_name AS parameter, p.unit_default AS units, sf.sample_fraction AS fraction, rs.result_speciation AS speciation, g.parameter_id, g.sample_fraction_id, g.result_speciation_id, g.guideline_sql FROM discrete.guidelines as g LEFT JOIN sample_fractions sf ON sf.sample_fraction_id = g.sample_fraction_id JOIN public.parameters as p ON p.parameter_id = g.parameter_id LEFT JOIN result_speciations rs ON rs.result_speciation_id = g.result_speciation_id")
          moduleData$guidelines_temp <- moduleData$guidelines
          
          # Clear table selection and hide save button
          DT::dataTableProxy("guidelines_table") %>% DT::selectRows(NULL)
          shinyjs::hide("save_guideline")
          showModal(modalDialog(
            title = "Success",
            "Guideline deleted successfully.",
            footer = tagList(
              modalButton("Close")
            )
          ))
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred while deleting the guideline:", e$message),
            footer = tagList(
              modalButton("Close")
            )
          ))
        }) # End of tryCatch
      }) # End of observeEvent for confirm_delete
    
    
  }) # End of moduleServer
}
