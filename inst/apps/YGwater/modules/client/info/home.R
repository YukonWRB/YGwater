
homeUI <- function(id) {
  ns <- NS(id)
  tagList()
  verticalLayout(
    htmlOutput(ns("betaTitle")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("betaText")),
    tags$div(style = "height: 60px;"),
    htmlOutput(ns("title")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("main_text")),
    hr(),
    tags$div(style = "height: 30px;"),
    htmlOutput(ns("discrete_continuous_title")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("discrete_continuous")),
    tags$div(style = "height: 20px;"),
    tags$div(style = "height: 10px;"),
    uiOutput(ns("map_buttons")),
    tags$div(style = "height: 20px;"),
    tags$div(style = "height: 10px;"),
    uiOutput(ns("plot_buttons")),
    tags$div(style = "height: 20px;"),
    tags$div(style = "height: 10px;"),
    uiOutput(ns("data_buttons")),
    tags$div(style = "height: 40px;"),
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
      output$main_text <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
                    tr("homeText", language$language),
                    '</div>'
        ))
      })
      
      
      output$discrete_continuous_title <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    tr("home_discrete_continuous_title", language$language),
                    '</div>'
        ))
      })
      output$discrete_continuous <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 16px; font-weight: 500; font-style: normal">',
                    tr("home_discrete_continuous", language$language),
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
          column(6, actionButton(ns("map_locs"), tr("home_map_locs_btn", language$language),
                                 class = "btn-bar w-100 btn-bar-locs",
                                 icon = icon("map-location"))),
          column(6, actionButton(ns("map_params"), tr("home_map_params_btn", language$language),
                                 class = "btn-bar w-100 btn-bar-params",
                                 icon = icon("map-location")))
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
          column(6, actionButton(ns("plot_cont"), tr("view_plots_continuous", language$language),
                                 class = "btn-bar w-100 btn-bar-plot-c",
                                 icon = icon("chart-simple"))),
          column(6, actionButton(ns("plot_disc"), tr("view_plots_discrete", language$language),
                                 class = "btn-bar w-100 btn-bar-plot-d",
                                 icon = icon("chart-simple")))
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
          column(6, actionButton(ns("dl_cont"), tr("dl_data_continuous", language$language),
                                 class = "btn-bar w-100 btn-bar-dl-c",
                                 icon = icon("table"))),
          column(6, actionButton(ns("dl_disc"), tr("dl_data_discrete", language$language),
                                 class = "btn-bar w-100 btn-bar-dl-d",
                                 icon = icon("table")))
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
