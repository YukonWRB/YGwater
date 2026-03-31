contPlotAdaptiveUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("banner")),
    uiOutput(ns("main"))
  )
}


contPlotAdaptive <- function(id, language, windowDims, inputs = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    txt <- function(en, fr = en) {
      if (identical(language$language, "Français")) {
        fr
      } else {
        en
      }
    }

    fmt_count <- function(x) {
      prettyNum(as.integer(round(x)), big.mark = ",")
    }

    build_demo_data <- function(n_rows = 1000000L, tz = "UTC") {
      n_rows <- max(1000L, as.integer(n_rows))
      idx <- seq_len(n_rows)

      datetime <- as.POSIXct("2023-01-01 00:00:00", tz = tz) +
        idx * 60

      slow_wave <- 2.8 * sin(idx / 18000)
      mid_wave <- 0.9 * sin(idx / 2600)
      fast_wave <- 0.18 * sin(idx / 35) + 0.09 * cos(idx / 117)
      trend <- idx / n_rows * 1.6

      pulse_up <- 2.2 * exp(-((idx - n_rows * 0.23)^2) / (2 * (n_rows / 85)^2))
      pulse_down <- 1.7 * exp(-((idx - n_rows * 0.61)^2) / (2 * (n_rows / 70)^2))
      centre <- slow_wave + mid_wave + fast_wave + trend + pulse_up - pulse_down

      typical_half <- 0.38 + 0.10 * sin(idx / 4200)^2
      historic_half <- typical_half + 0.48 + 0.14 * cos(idx / 9000)^2

      spikes <- ifelse(idx %% 9973L == 0L, 1.25, 0) -
        ifelse(idx %% 14591L == 0L, 1.15, 0)

      dt <- data.table::data.table(
        datetime = datetime,
        value = centre + 0.05 * sin(idx / 19) + 0.04 * cos(idx / 73) + spikes,
        typical_lo = centre - typical_half,
        typical_hi = centre + typical_half,
        historic_lo = centre - historic_half,
        historic_hi = centre + historic_half
      )

      keep <- rep(TRUE, nrow(dt))
      gap_starts <- as.integer(round(n_rows * c(0.18, 0.49, 0.79)))
      gap_lengths <- as.integer(round(n_rows * c(0.015, 0.020, 0.010)))

      for (i in seq_along(gap_starts)) {
        start_i <- gap_starts[[i]]
        end_i <- min(n_rows, start_i + gap_lengths[[i]])
        keep[start_i:end_i] <- FALSE
      }

      dt[keep]
    }

    plot_width_px <- reactive({
      if (!is.null(input$plot_container_dims$width)) {
        return(input$plot_container_dims$width)
      }

      if (!is.null(windowDims())) {
        return(max(450, windowDims()$width - 420))
      }

      900
    })

    density_multiplier <- reactive({
      if (is.null(input$pixel_density)) {
        return(1)
      }
      as.numeric(input$pixel_density)
    })

    target_bins <- reactive({
      viewport_ribbon_target_bins(
        width_px = plot_width_px(),
        density = density_multiplier()
      )
    })

    current_n_points <- reactive({
      if (is.null(input$demo_points)) {
        return(1000000L)
      }
      as.integer(input$demo_points)
    })

    demo_data <- reactiveVal(NULL)
    state <- reactiveValues(
      current_xlim = NULL,
      render_key = NULL,
      plot_ready = FALSE,
      last_trace_count = 0L,
      last_summary = NULL,
      last_bundle = NULL
    )

    output$banner <- renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "contPlotAdaptive"
      )
    })

    output$sidebar <- renderUI({
      tagList(
        tags$h4(txt(
          "Adaptive Ribbon Demo",
          "Démo de ruban adaptatif"
        )),
        tags$p(txt(
          "This module keeps the full time series on the server, then redraws only a viewport-sized summary of the line and ribbons after each zoom or pan.",
          "Ce module conserve la série complète sur le serveur, puis redessine seulement un résumé adapté à la fenêtre visible après chaque zoom ou déplacement."
        )),
        tags$ul(
          tags$li(txt(
            "The line is reduced with a bin-wise first/min/max/last sampler so spikes survive heavy down-sampling.",
            "La ligne est réduite avec un échantillonnage premier/min/max/dernier par bin afin de conserver les pointes lors d'une forte réduction."
          )),
          tags$li(txt(
            "Each ribbon is summarized conservatively with the minimum lower bound and maximum upper bound inside each bin.",
            "Chaque ruban est résumé de façon conservatrice avec la borne inférieure minimale et la borne supérieure maximale dans chaque bin."
          )),
          tags$li(txt(
            "Only the summarized traces are sent to plotly, so browser payload scales with plot width instead of raw row count.",
            "Seules les traces résumées sont envoyées à plotly; la charge du navigateur dépend donc de la largeur du graphique plutôt que du nombre brut de lignes."
          ))
        ),
        tags$hr(),
        selectInput(
          ns("demo_points"),
          label = txt("Demo rows", "Lignes de démonstration"),
          choices = c(
            "250,000" = "250000",
            "1,000,000" = "1000000",
            "2,000,000" = "2000000"
          ),
          selected = "1000000"
        ),
        selectInput(
          ns("pixel_density"),
          label = txt("Summary density", "Densité du résumé"),
          choices = c(
            "0.75 x plot width" = "0.75",
            "1.00 x plot width" = "1.00",
            "1.50 x plot width" = "1.50"
          ),
          selected = "1.00"
        ),
        actionButton(
          ns("regen"),
          label = txt("Build / rebuild demo", "Construire / reconstruire la démo"),
          class = "btn btn-primary"
        ),
        tags$div(style = "height: 12px;"),
        tags$p(
          class = "small text-muted",
          txt(
            "Use the mode bar to zoom. Double-click the plot to reset to the full extent.",
            "Utilisez la barre d'outils pour zoomer. Double-cliquez sur le graphique pour revenir à l'étendue complète."
          )
        )
      )
    })

    output$main <- renderUI({
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          title = NULL,
          width = 360,
          bg = config$sidebar_bg,
          open = list(mobile = "always-above"),
          uiOutput(ns("sidebar"))
        ),
        div(
          id = ns("plot_container"),
          plotly::plotlyOutput(ns("plot"), height = "760px")
        ),
        tags$div(style = "height: 12px;"),
        uiOutput(ns("status"))
      )
    })

    session$onFlushed(function() {
      shinyjs::runjs(
        sprintf(
          "trackContainerSize('%s', '%s');",
          ns("plot_container"),
          ns("plot_container_dims")
        )
      )
    }, once = TRUE)

    render_demo_plot <- function(xlim = NULL, full_render = FALSE, force = FALSE) {
      req(!is.null(demo_data()))

      lang_key <- if (is.null(language$language)) {
        "English"
      } else {
        language$language
      }

      key_xlim <- if (is.null(xlim)) {
        "full"
      } else {
        paste(format(xlim, tz = "UTC", usetz = TRUE), collapse = "|")
      }
      render_key <- paste(key_xlim, target_bins(), lang_key, sep = "::")

      if (!force && identical(state$render_key, render_key)) {
        return(invisible(NULL))
      }

      band_names <- c(
        txt("Historic envelope", "Enveloppe historique"),
        txt("Typical envelope", "Enveloppe typique")
      )

      summary <- viewport_ribbon_resample(
        data = demo_data(),
        x_col = "datetime",
        line_col = "value",
        bands = stats::setNames(
          list(
            c("historic_lo", "historic_hi"),
            c("typical_lo", "typical_hi")
          ),
          band_names
        ),
        xlim = xlim,
        n_bins = target_bins()
      )

      built <- viewport_ribbon_plot(
        summary = summary,
        title = list(
          text = txt(
            "Viewport-aware ribbon resampling",
            "Rééchantillonnage de rubans selon la fenêtre visible"
          ),
          x = 0.02,
          xanchor = "left"
        ),
        source = ns("plot"),
        line_name = txt("Observed signal", "Signal observé"),
        band_styles = stats::setNames(
          list(
            list(
              fillcolor = "rgba(212, 236, 239, 0.85)",
              line = list(color = "rgba(212, 236, 239, 1)", width = 0.3)
            ),
            list(
              fillcolor = "rgba(95, 157, 166, 0.45)",
              line = list(color = "rgba(95, 157, 166, 0.85)", width = 0.3)
            )
          ),
          band_names
        ),
        yaxis_title = txt("Synthetic value", "Valeur synthétique")
      )

      old_trace_count <- state$last_trace_count
      state$current_xlim <- xlim
      state$render_key <- render_key
      state$last_summary <- summary
      state$last_bundle <- built$trace_bundle
      state$last_trace_count <- built$trace_bundle$trace_count

      if (full_render || !isTRUE(state$plot_ready)) {
        output$plot <- plotly::renderPlotly({
          built$plot
        })
        state$plot_ready <- TRUE
        return(invisible(NULL))
      }

      proxy <- plotly::plotlyProxy("plot", session)
      if (old_trace_count > 0) {
        proxy <- plotly::plotlyProxyInvoke(
          proxy,
          "deleteTraces",
          seq_len(old_trace_count) - 1L
        )
      }

      plotly::plotlyProxyInvoke(
        proxy,
        "addTraces",
        built$trace_bundle$traces
      )
    }

    output$status <- renderUI({
      req(state$last_summary, state$last_bundle)

      meta <- state$last_summary$meta
      reduction <- if (state$last_bundle$client_points > 0) {
        meta$window_rows / state$last_bundle$client_points
      } else {
        NA_real_
      }

      current_window <- if (is.null(state$current_xlim)) {
        txt("Full extent", "Étendue complète")
      } else {
        paste(
          format(state$current_xlim[[1]], "%Y-%m-%d %H:%M"),
          txt("to", "au"),
          format(state$current_xlim[[2]], "%Y-%m-%d %H:%M")
        )
      }

      bslib::card(
        bslib::card_header(txt(
          "Current summary",
          "Résumé courant"
        )),
        tags$p(
          tags$strong(txt("Rows in full demo:", "Lignes dans la démo complète :")),
          " ",
          fmt_count(meta$total_rows)
        ),
        tags$p(
          tags$strong(txt("Rows in current window:", "Lignes dans la fenêtre courante :")),
          " ",
          fmt_count(meta$window_rows)
        ),
        tags$p(
          tags$strong(txt("Rows used after padding:", "Lignes utilisées après marge :")),
          " ",
          fmt_count(meta$visible_rows)
        ),
        tags$p(
          tags$strong(txt("Approximate bins:", "Bins approximatifs :")),
          " ",
          fmt_count(meta$n_bins)
        ),
        tags$p(
          tags$strong(txt("Points sent to plotly:", "Points envoyés à plotly :")),
          " ",
          fmt_count(state$last_bundle$client_points)
        ),
        tags$p(
          tags$strong(txt("Reduction factor:", "Facteur de réduction :")),
          " ",
          if (is.na(reduction)) {
            "-"
          } else {
            paste0(format(round(reduction, 1), nsmall = 1), "x")
          }
        ),
        tags$p(
          tags$strong(txt("Displayed window:", "Fenêtre affichée :")),
          " ",
          current_window
        )
      )
    })

    observeEvent(input$regen, {
      demo_data(build_demo_data(current_n_points()))
      state$current_xlim <- NULL
      state$render_key <- NULL
      state$plot_ready <- FALSE
      state$last_trace_count <- 0L
      render_demo_plot(full_render = TRUE, force = TRUE)
    }, ignoreInit = FALSE)

    relayout_event <- shiny::debounce(
      reactive(plotly::event_data("plotly_relayout", source = ns("plot"))),
      millis = 150
    )

    observeEvent(relayout_event(), {
      req(!is.null(demo_data()))
      xlim <- viewport_ribbon_relayout_xlim(relayout_event(), tz = "UTC")
      render_demo_plot(xlim = xlim)
    }, ignoreInit = TRUE)

    observeEvent(input$plot_container_dims, {
      req(!is.null(demo_data()))
      render_demo_plot(xlim = state$current_xlim)
    }, ignoreInit = TRUE)
  })
}
