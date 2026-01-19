aboutUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    uiOutput(ns("banner")),
    htmlOutput(ns("title_licence")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("licence_body")),
    tags$div(style = "height: 30px;"),
    htmlOutput(ns("title_fun_facts")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("content_fun_facts")),
    tags$div(style = "height: 30px;"),
    htmlOutput(ns("title_web_page")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("content_web_page")),
    tags$div(style = "height: 20px;"),
    htmlOutput(ns("version")),
  )
}

about <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(language)

      output$banner <- renderUI({
        application_notifications_ui(
          ns = ns,
          lang = language$language,
          con = session$userData$AquaCache,
          module_id = "about"
        )
      })
      output$title_licence <- renderUI({
        HTML(paste0(
          '<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal;">',
          tr("licence_title", language$language),
          '</div>'
        ))
      })
      output$licence_body <- renderUI({
        HTML(paste0(
          '<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
          tr("licence1", language$language),
          "<a href='",
          tr("licence_url", language$language),
          "' target='_blank'>",
          tr("licence2", language$language),
          "</a>.<br><br>",
          tr("licence3", language$language),
          '</div>'
        ))
      })

      output$title_fun_facts <- renderUI({
        HTML(paste0(
          '<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal;">',
          tr("about_fun_facts_title", language$language),
          '</div>'
        ))
      })

      output$content_fun_facts <- renderUI({
        format_count <- function(value) {
          if (is.na(value)) {
            return("0")
          }
          format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
        }

        timeseries_count <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT COUNT(*) AS count FROM timeseries;"
        )$count[1]
        measurements_estimate <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT reltuples::bigint AS estimate FROM pg_class WHERE oid = 'measurements_continuous'::regclass;"
        )$estimate[1]
        samples_count <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT COUNT(*) AS count FROM samples;"
        )$count[1]
        results_count <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT reltuples::bigint AS estimate FROM pg_class WHERE oid = 'results'::regclass;"
        )$estimate[1]
        wells_count <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT COUNT(*) AS count FROM wells;"
        )$count[1]

        HTML(paste0(
          '<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
          "<ul>",
          "<li>",
          sprintf(
            tr("about_fun_fact_timeseries", language$language),
            format_count(timeseries_count),
            format_count(measurements_estimate)
          ),
          "</li>",
          "<li>",
          sprintf(
            tr("about_fun_fact_samples", language$language),
            format_count(samples_count),
            format_count(results_count)
          ),
          "</li>",
          "<li>",
          sprintf(
            tr("about_fun_fact_wells", language$language),
            format_count(wells_count)
          ),
          "</li>",
          "</ul>",
          '</div>'
        ))
      })

      output$title_web_page <- renderUI({
        HTML(paste0(
          '<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
          tr("about_title2", language$language),
          '</div>'
        ))
      })
      output$content_web_page <- renderUI({
        HTML(paste0(
          '<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal">',
          tr("about_content2", language$language),
          '<a href="',
          tr("about_url_ygwater", language$language),
          '" target="_blank">',
          tr("about_url_ygwater", language$language),
          '</a>.',
          '<br><br>',
          tr("about_content3", language$language),
          '<a href="',
          tr("about_url_aquacache", language$language),
          '" target="_blank">',
          tr("about_url_aquacache", language$language),
          '</a>.',
          tr("about_content4", language$language),
          '<br><br>',
          tr("about_content5", language$language),
          '<br><br>',
          '</div>'
        ))
      })

      output$version <- renderUI({
        # Get the AquaCache revision number
        revision <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT version FROM information.version_info WHERE item = 'Last patch number'"
        )[1, 1]

        HTML(paste0(
          '<div class="nunito-sans" style="font-size: 18px; font-weight: 600; font-style: normal;">',
          tr("current_software", language$language),
          ' <br>',
          '</div>',
          '<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
          tr("app_version1", language$language),
          " ",
          utils::packageVersion("YGwater"),
          ", ",
          tr("app_version2", language$language),
          " ",
          revision,
          ".",
          '</div>'
        ))
      })
    })
  })
}
