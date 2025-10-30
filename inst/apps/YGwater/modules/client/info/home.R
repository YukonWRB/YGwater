homeUI <- function(id) {
  ns <- NS(id)
  tagList()
  verticalLayout(
    htmlOutput(ns("betaTitle")), # TO BE REMOVED once testing is complete
    tags$div(style = "height: 10px;"), # TO BE REMOVED once testing is complete
    htmlOutput(ns("betaText")), # TO BE REMOVED once testing is complete
    hr(), # TO BE REMOVED once testing is complete

    tags$div(style = "height: 30px;"), # ADJUST once testing is complete
    htmlOutput(ns("title")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("main_text")),
    tags$div(style = "height: 30px;"),
    uiOutput(ns("map_buttons")),
    tags$div(style = "height: 20px;"),
    uiOutput(ns("plot_buttons")),
    tags$div(style = "height: 20px;"),
    uiOutput(ns("data_buttons")),
    tags$div(style = "height: 20px;"),
    htmlOutput(ns("discrete_continuous_title")), # Title for discrete vs continuous data
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("discrete_continuous")), # Explanatory text about discrete vs continuous data
    tags$div(style = "height: 10px;"),
  )
}

home <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    outputs <- reactiveValues()

    observe({
      req(language$language)
      output$betaTitle <- renderUI({
        HTML(paste0(
          '<div class="montserrat" style="font-size: 24px; font-weight: 600; font-style: normal">',
          tr("betaTitle", language$language),
          '</div>'
        ))
      })
      output$betaText <- renderUI({
        HTML(paste0(
          '<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
          tr("betaText", language$language),
          '</div>'
        ))
      })
      output$title <- renderUI({
        HTML(paste0(
          '<div class="montserrat" style="font-size: 24px; font-weight: 600; font-style: normal">',
          tr("homeTitle", language$language),
          '</div>'
        ))
      })
      output$main_text <- renderUI({
        HTML(paste0(
          '<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
          tr("homeText", language$language),
          '</div>'
        ))
      })

      output$map_buttons <- renderUI({
        fluidRow(
          column(
            6,
            actionButton(
              ns("map_locs"),
              tr("home_map_locs_btn", language$language),
              class = "btn-bar w-100 btn-bar-locs",
              style = "padding: 14px",
              icon = icon("map-location")
            )
          ),
          column(
            6,
            actionButton(
              ns("map_params"),
              tr("home_map_params_btn", language$language),
              class = "btn-bar w-100 btn-bar-params",
              style = "padding: 14px",
              icon = icon("map-location")
            )
          )
        )
      })
      output$plot_buttons <- renderUI({
        fluidRow(
          column(
            6,
            actionButton(
              ns("plot_cont"),
              tr("view_plots_continuous", language$language),
              class = "btn-bar w-100 btn-bar-plot-c",
              style = "padding: 14px",
              icon = icon("chart-simple")
            )
          ),
          column(
            6,
            actionButton(
              ns("plot_disc"),
              tr("view_plots_discrete", language$language),
              class = "btn-bar w-100 btn-bar-plot-d",
              style = "padding: 14px",
              icon = icon("chart-simple")
            )
          )
        )
      })
      output$data_buttons <- renderUI({
        fluidRow(
          column(
            6,
            actionButton(
              ns("dl_cont"),
              tr("dl_data_continuous", language$language),
              class = "btn-bar w-100 btn-bar-dl-c",
              style = "padding: 14px",
              icon = icon("table")
            )
          ),
          column(
            6,
            actionButton(
              ns("dl_disc"),
              tr("dl_data_discrete", language$language),
              class = "btn-bar w-100 btn-bar-dl-d",
              style = "padding: 14px",
              icon = icon("table")
            )
          )
        )
      })
    })

    output$discrete_continuous_title <- renderUI({
      HTML(paste0(
        '<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
        tr("home_discrete_continuous_title", language$language),
        '</div>'
      ))
    })
    output$discrete_continuous <- renderUI({
      HTML(paste0(
        '<div class="montserrat" style="font-size: 16px; font-weight: 500; font-style: normal">',
        tr("home_discrete_continuous", language$language),
        '</div>'
      ))
    })

    observeEvent(input$plot_disc, {
      outputs$change_tab <- "discPlot"
    })
    observeEvent(input$plot_cont, {
      outputs$change_tab <- "contPlot"
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
