homeUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("content"))
}

home <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    outputs <- reactiveValues()

    output$content <- renderUI({
      req(language$language)

      verticalLayout(
        application_notifications_ui(
          ns = ns,
          lang = language$language,
          con = session$userData$AquaCache,
          module_id = "home"
        ),
        tags$div(style = "height: 20px;"),
        HTML(paste0(
          '<div class="montserrat" style="font-size: 24px; font-weight: 600; font-style: normal">',
          tr("homeTitle", language$language),
          '</div>'
        )),
        tags$div(style = "height: 10px;"),
        HTML(paste0(
          '<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
          tr("homeText", language$language),
          '</div>'
        )),
        tags$div(style = "height: 30px;"),
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
        ),
        tags$div(style = "height: 20px;"),
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
        ),
        tags$div(style = "height: 20px;"),
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
        ),
        tags$div(style = "height: 20px;"),
        HTML(paste0(
          '<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
          tr("home_discrete_continuous_title", language$language),
          '</div>'
        )),
        tags$div(style = "height: 10px;"),
        HTML(paste0(
          '<div class="montserrat" style="font-size: 16px; font-weight: 500; font-style: normal">',
          tr("home_discrete_continuous", language$language),
          '</div>'
        )),
        tags$div(style = "height: 10px;")
      )
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
      outputs$change_tab <- "monitoringLocationsMap"
    })
    observeEvent(input$map_params, {
      outputs$change_tab <- "parameterValuesMap"
    })

    return(outputs)
  })
}
