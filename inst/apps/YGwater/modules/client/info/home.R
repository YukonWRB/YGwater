
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
    htmlOutput(ns("mapButtonsTitle")),
    tags$div(style = "height: 10px;"),
    uiOutput(ns("map_buttons")),
    tags$div(style = "height: 20px;"),
    htmlOutput(ns("plotButtonsTitle")),
    tags$div(style = "height: 10px;"),
    uiOutput(ns("plot_buttons")),
    tags$div(style = "height: 20px;"),
    htmlOutput(ns("dataButtonsTitle")),
    tags$div(style = "height: 10px;"),
    uiOutput(ns("data_buttons")),
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
      
      output$mapButtonsTitle <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500;">',
                    "You can explore the data in map form. This is the best option if you're not sure of what you want to do with the data, or if you just want to see where the monitoring locations are and which parameters are available.",
                    '</div>'
        ))
      })
      output$map_buttons <- renderUI({
        fluidRow(
          column(6, actionButton(ns("map_locs"), "See a map of monitoring locations",
                                 class = "btn btn-primary w-100")),
          column(6, actionButton(ns("map_params"), "See a map of parameter values now or at a past date",
                                 class = "btn btn-primary w-100"))
        )
      })
      output$plotButtonsTitle <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500;">',
                    "If you know what you want to see you can plot the data directly. You can choose to plot continuous or discrete data:",
                    '</div>'
        ))
      })
      output$plot_buttons <- renderUI({
        fluidRow(
          column(6, actionButton(ns("plot_cont"), "Plot continuous data",
                                 class = "btn btn-primary w-100")),
          column(6, actionButton(ns("plot_disc"), "Plot discrete data",
                                 class = "btn btn-primary w-100"))
        )
      })
      output$dataButtonsTitle <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500;">',
                    "If you're interested in the data itself, you can download both types:",
                    '</div>'
        ))
      })
      output$data_buttons <- renderUI({
        fluidRow(
          column(6, actionButton(ns("dl_cont"), "Download continuous data",
                                 class = "btn btn-primary w-100")),
          column(6, actionButton(ns("dl_disc"), "Download discrete data",
                                 class = "btn btn-primary w-100"))
        )
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
