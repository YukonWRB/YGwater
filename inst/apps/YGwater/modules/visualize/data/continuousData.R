contDataUI <- function(id) {
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
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("sidebar")) # UI is rendered in the server function below so that it can use database information as well as language selections.
      ),
      mainPanel(
        uiOutput(ns("main"))
      )
    )
  )
}

contData <- function(id, language) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements within the server code
    
    # Functions ################
    # Adjust multiple selection based on if 'All' is selected
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
    
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    moduleData <- reactiveValues(
      locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT loc.location_id, loc.name, loc.name_fr FROM locations AS loc INNER JOIN samples ON loc.location_id = samples.location_id ORDER BY loc.name ASC"),
      params = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id ORDER BY p.param_name ASC;"),
      media_types = DBI::dbGetQuery(session$userData$AquaCache,
                                    "SELECT * FROM media_types;"),
      param_groups = DBI::dbGetQuery(session$userData$AquaCache,
                                     "SELECT * FROM parameter_groups;"),
      param_sub_groups = DBI::dbGetQuery(session$userData$AquaCache,
                                         "SELECT * FROM parameter_sub_groups;"),
      parameter_relationships = DBI::dbGetQuery(session$userData$AquaCache,
                                                "SELECT * FROM parameter_relationships;")
    )
    
    output$sidebar <- renderUI({
      tagList(
        # start and end datetime
        dateRangeInput(ns("date_range"),
                       "Select date range",
                       start = Sys.Date() - 30,
                       end = Sys.Date(),
                       max = Sys.Date() + 1,
                       format = "yyyy-mm-dd"),
        
        # Toggle button for locations or location groups
        # Selectize input for locations, populated once connection is established
        selectizeInput(ns("locations"),
                       label = tr("loc(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$locs$location_id),
                                         c(tr("all", language$language), moduleData$locs[, tr("generic_name_col", language$language)])),
                       multiple = TRUE
        ),
        # Selectize input for media type
        selectizeInput(ns("mType"),
                       label = tr("media_type(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$media_types$media_id),
                                         c(tr("all", language$language), moduleData$media_types[, tr("media_type_col", language$language)])),
                       multiple = TRUE
        ),
        # Selectize input for parameter groups
        selectizeInput(ns("pGrps"),
                       label = tr("param_group(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$param_groups$group_id),
                                         c(tr("all", language$language), moduleData$param_groups[, tr("param_group_col", language$language)])),
                       multiple = TRUE
        ),
        # parameter sub-group
        selectizeInput(ns("pSubGrps"),
                       label = tr("param_sub_group(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$param_sub_groups$sub_group_id),
                                         c(tr("all", language$language), moduleData$param_sub_groups[, tr("param_sub_group_col", language$language)])),
                       multiple = TRUE
        ),
        # Selectize input for parameters
        selectizeInput(ns("params"),
                       label = tr("parameter(s)", language$language),
                       choices = 
                         stats::setNames(c("all", moduleData$params$parameter_id),
                                         c(tr("all", language$language), moduleData$params[, tr("param_name_col", language$language)])),
                       multiple = TRUE
        ),
        # Selectize input for projects and networks
        
        
        actionButton(ns("reset"),
                     label = tr("reset", language$language)
        )
        
        
      ) # End of tagList
    })  %>% # End of renderUI for sidebar
      bindEvent(language$language)  #TODO: bindEvent should also be on moduleData, but moduleData is not being used in the creation of lists yet
    
    
    
    output$main <- renderUI({
      tagList(
        # Add DT data table here
      ) # End of tagList
    }) %>% # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or moduleData changes
    
  }) # End moduleServer
} # End contPlot function
