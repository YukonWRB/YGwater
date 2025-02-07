# UI and server code for add new location module

locsNewTSUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      # On larger screens, a 6-column width centered with offset
      # On smaller screens, it naturally adjusts to full width
      column(width = 6,
             textOutput(ns("hydat_note")
             ),
             textInput(ns("loc_code"), 
                       "Location code (must not exist already)",
                       width = "100%"
                       
             ),
             actionButton(ns("hydat_fill"), "Auto-fill from HYDAT",
                          width = "100%"
             ),
             textInput(ns("loc_name"), 
                       "Location name (must not exist already)",
                       width = "100%"
             ),
             textInput(ns("loc_name_fr"), 
                       "French location name (must not exist already)",
                       width = "100%"
             ),
             selectizeInput(ns("loc_type"), 
                            "Location type",
                            choices = stats::setNames(moduleData$loc_types$type_id, moduleData$loc_types$type),
                            multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                            options = list(maxItems = 1),
                            width = "100%"
             ),
             numericInput(ns("lat"), 
                          "Latitude (decimal degrees)", 
                          value = NULL,
                          width = "100%"
             ),
             uiOutput(ns("lat_warning")),
             numericInput(ns("lon"), 
                          "Longitude (decimal degrees)", 
                          value = NULL,
                          width = "100%"
             ),
             uiOutput(ns("lon_warning")),
             selectizeInput(ns("viz"), 
                            "Public visibility", 
                            choices = stats::setNames(c("exact", "region", "jitter"), 
                                                      c("Exact", "Within general region", "At random within a 5 km radius of true location")),
                            width = "100%"
             ),
             selectizeInput(ns("share_with"), 
                            "Share with groups (1 or more, type your own if not in list)", 
                            choices = stats::setNames(moduleData$user_groups$group_id,
                                                      paste0(moduleData$user_groups$group_name, " (", moduleData$user_groups$group_description, ")")),
                            multiple = TRUE,
                            selected = 1,
                            options = list(create = TRUE),
                            width = "100%"
             ),
             selectizeInput(ns("loc_owner"), 
                            "Owner (type your own if not in list)", 
                            choices = stats::setNames(moduleData$organizations$organization_id, 
                                                      moduleData$organizations$name),
                            multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                            options = list(maxItems = 1,
                                           create = TRUE),
                            width = "100%"
             ),
             textInput(ns("loc_contact"),
                       "Contact details if different than owner default (optional)",
                       width = "100%"
             ),
             selectizeInput(ns("data_sharing_agreement"), 
                            "Data sharing agreement", 
                            choices = stats::setNames(moduleData$agreements$document_id, 
                                                      moduleData$agreements$name),
                            options = list(placeholder = "Optional - add the document first if needed"),
                            width = "100%"
             ),
             selectizeInput(ns("datum_id_from"), 
                            "Datum ID from (Assumed datum is station 0)", 
                            choices = stats::setNames(moduleData$datums$datum_id, 
                                                      titleCase(moduleData$datums$datum_name_en, "en")), 
                            selected = 10,
                            width = "100%"
             ),
             selectizeInput(ns("datum_id_to"), 
                            "Datum ID to (Use assumed datum if no conversion to apply)",
                            multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                            choices = stats::setNames(moduleData$datums$datum_id, 
                                                      titleCase(moduleData$datums$datum_name_en, "en")),
                            options = list(maxItems = 1), # Overrides multiple selection
                            width = "100%"
             ), 
             numericInput(ns("elev"), 
                          "Elevation conversion (meters, use 0 if not converting)", 
                          value = NULL,
                          width = "100%"
             ),
             selectizeInput(ns("network"), 
                            "Network (type your own if not in list)", 
                            choices = stats::setNames(moduleData$networks$network_id, moduleData$networks$name),
                            multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                            options = list(create = TRUE, 
                                           placeholder = "Optional but recommended",
                                           maxItems = 1),  # With a choice to allow users to add a network
                            width = "100%"
             ),
             selectizeInput(ns("project"), 
                            "Project (type your own if not in list)", 
                            choices = stats::setNames(moduleData$projects$project_id, moduleData$projects$name), 
                            options = list(create = TRUE, 
                                           placeholder = "Optional"),  # With a choice to allow users to add a project
                            width = "100%"
             ),
             textInput(ns("loc_note"), 
                       "Location note",
                       width = "100%"
             ),
             actionButton(ns("add_loc"), 
                          "Add location",
                          width = "100%"
             )
      ) 
    )
  )
}

locsNewTSServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    

  }) # End of moduleServer
}
