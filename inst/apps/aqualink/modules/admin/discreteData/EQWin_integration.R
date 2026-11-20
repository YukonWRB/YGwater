# This module will be used to facilitate integration of EQWin databases into AquaCache. It will:
# 1. Show which locations are not being reflected in AquaCache and allow them to add those locations to AquaCache if they have adequate location metadata, and alert them if loc metadata is missing.
# 2. Show which locations are missing auto import pipelines and help them create those pipelines with function 'downloadEQWin'.
# 3. Show which parameters are not mapped for import and allow users to create new mappings.

# UI and server code for managing discrete samples

editSamplesUI <- function(id) {
    ns <- NS(id)

    tagList(
        tags$style(
            HTML(sprintf(
                "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ",
                ns("accordion1")
            )),
            HTML(sprintf(
                "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #E5F4F6;
        /* collapsed header */
        --bs-accordion-btn-bg:      #0097A9;
        /* expanded header */
        --bs-accordion-active-bg:   #0097A9;
      }
    ",
                ns("accordion2")
            )),
            HTML(sprintf(
                "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #F2F7EC;
        /* collapsed header */
        --bs-accordion-btn-bg:      #83A95C;
        /* expanded header */
        --bs-accordion-active-bg:   #83A95C;
      }
    ",
                ns("accordion3")
            ))
        ),
        page_fluid(
            uiOutput(ns("banner")),
            uiOutput(ns("ui"))
        )
    )
}

editSamples <- function(id, language) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Standard banner UI for this module, if any exists
        output$banner <- renderUI({
            req(language$language)
            application_notifications_ui(
                ns = ns,
                lang = language$language,
                con = session$userData$AquaCache,
                module_id = "editSamples"
            )
        })
    })

    moduleData <- reactiveValues() # populated via a function later so that it can be refreshed when the user clicks a refresh button

    getModuleData <- function() {
        con <- session$userData$AquaCache
        # populate moduleData
    }

    output$ui <- renderUI({
        # Main UI will render here
    })
}
