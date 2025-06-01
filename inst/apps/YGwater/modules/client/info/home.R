
homeUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    htmlOutput(ns("betaTitle")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("betaText")),
    tags$div(style = "height: 60px;"),
    htmlOutput(ns("title")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("text")),
    tags$div(style = "height: 20px;"),
    htmlOutput(ns("buttonsTitle")),
    tags$div(style = "height: 10px;"),
    fluidRow(
      column(4, actionButton(ns("plot_disc"), "Plot discrete data",
                             class = "btn btn-primary w-100")),
      column(4, actionButton(ns("plot_cont"), "Plot continuous data",
                             class = "btn btn-primary w-100")),
      column(4, actionButton(ns("dl_disc"), "Download discrete data",
                             class = "btn btn-primary w-100"))
    ),
    tags$div(style = "height: 10px;"),
    fluidRow(
      column(4, actionButton(ns("dl_cont"), "Download continuous data",
                             class = "btn btn-primary w-100")),
      column(4, actionButton(ns("map_locs"), "Monitoring locations map",
                             class = "btn btn-primary w-100")),
      column(4, actionButton(ns("map_params"), "Parameter values map",
                             class = "btn btn-primary w-100"))
    ),
    tags$div(style = "height: 40px;"),
    htmlOutput(ns("discTitle")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("discBody"))
  )
}

home <- function(id, language) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    outputs <- reactiveValues()

    observe({
      req(language$language)
      output$betaTitle <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 24px; font-weight: 600; font-style: normal">',
                    tr("betaTitle", language$language),
                    '</div>'
        ))
      })
      output$betaText <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
                    tr("betaText", language$language),
                    '</div>'
        ))
      })
      output$title <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 24px; font-weight: 600; font-style: normal">',
                    tr("homeTitle", language$language),
                    '</div>'
        ))
      })
      output$text <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
                    tr("homeText", language$language),
                    '</div>'
        ))
      })
      output$buttonsTitle <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500;">',
                    'Select an option below to explore the data:',
                    '</div>'
        ))
      })
      output$discTitle <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    tr("disclaimer_title", language$language),
                    '</div>'
        ))
      })
      output$discBody <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 14px; font-weight: 500; font-style: normal">',
                    tr("disclaimer_body", language$language),
                    '</div>'
        ))
      })
    })

    observeEvent(input$plot_disc, {
      outputs$change_tab <- "discrete"
    })
    observeEvent(input$plot_cont, {
      outputs$change_tab <- "continuous"
    })
    observeEvent(input$dl_disc, {
      outputs$change_tab <- "discData"
    })
    observeEvent(input$dl_cont, {
      outputs$change_tab <- "contData"
    })
    observeEvent(input$map_locs, {
      outputs$change_tab <- "monitoringLocations"
    })
    observeEvent(input$map_params, {
      outputs$change_tab <- "parameterValues"
    })

    return(outputs)
  })
}
