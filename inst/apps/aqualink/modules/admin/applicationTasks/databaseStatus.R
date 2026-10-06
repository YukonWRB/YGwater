#' Database status dashboard
#'
#' Read-only dashboard of the freshness of configured automated continuous
#' data streams, image series, and raster series. The source-adapter assignment
#' tables are the authority for which series are automated; data-age timestamps
#' are maintained by AquaCache when new records are added.
#'
#' @param id Shiny module ID.
#' @param language Reactive language selection from the parent app.
#'
#' @return A Shiny UI tag list.
#' @keywords internal
#' @noRd
databaseStatusUI <- function(id, language = "English") {
  ns <- shiny::NS(id)
  french <- identical(language, "Français")

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 9,
        shiny::h2(if (french) "État de la base de données" else "Database status"),
        shiny::p(
          if (french) {
            "Fraîcheur des séries automatisées. Les flux continus sont orange après 1 heure et rouges après 3 heures; les images et les séries raster sont orange après 1 jour et rouges après 2 jours."
          } else {
            "Freshness of automated series. Continuous streams turn orange after 1 hour and red after 3 hours; images and raster series turn orange after 1 day and red after 2 days."
          }
        )
      ),
      shiny::column(
        width = 3,
        shiny::div(
          style = "text-align: right; padding-top: 18px;",
          shiny::actionButton(
            ns("refresh"),
            label = if (french) "Actualiser" else "Refresh",
            icon = shiny::icon("refresh"),
            class = "btn-primary"
          )
        )
      )
    ),
    shiny::uiOutput(ns("summary")),
    bslib::card(
      bslib::card_header(
        if (french) "Flux de données continus automatisés" else "Automated continuous data streams"
      ),
      DT::DTOutput(ns("streams_table"))
    ),
    bslib::card(
      bslib::card_header(
        if (french) "Séries d’images à récupération automatique" else "Auto-fetch image series"
      ),
      DT::DTOutput(ns("images_table"))
    ),
    bslib::card(
      bslib::card_header(
        if (french) "Séries raster automatisées" else "Automated raster series"
      ),
      DT::DTOutput(ns("rasters_table"))
    )
  )
}

#' Serve the read-only database status dashboard
#'
#' @param id Shiny module ID.
#' @param language Reactive language selection from the parent app.
#'
#' @return A Shiny module server function.
#' @keywords internal
#' @noRd
databaseStatus <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {
    status_data <- shiny::reactiveVal(NULL)
    stream_watch_seconds <- 60 * 60
    stream_stale_seconds <- 3 * 60 * 60
    image_watch_seconds <- 24 * 60 * 60
    image_stale_seconds <- 2 * 24 * 60 * 60
    raster_watch_seconds <- 24 * 60 * 60
    raster_stale_seconds <- 2 * 24 * 60 * 60

    label <- function(english, french) {
      if (identical(language$language, "Français")) french else english
    }

    refresh_status <- function() {
      con <- session$userData$AquaCache
      status <- tryCatch(
        {
          if (is.null(con) || !DBI::dbIsValid(con)) {
            stop("The AquaCache connection is not available.", call. = FALSE)
          }

          database <- DBI::dbGetQuery(
            con,
            "SELECT current_database() AS database_name,
                    current_user AS database_role,
                    CURRENT_TIMESTAMP AS checked_at"
          )

          streams <- DBI::dbGetQuery(
            con,
            "WITH automated_sources AS (
               SELECT tsa.timeseries_id,
                      string_agg(
                        tsa.source_fx || ' [fetch ' ||
                          COALESCE(tsa.fetch_priority::text, '—') ||
                          ', sync ' ||
                          COALESCE(tsa.synchronize_priority::text, '—') || ']',
                        '; ' ORDER BY
                          COALESCE(tsa.fetch_priority, tsa.synchronize_priority),
                          tsa.timeseries_source_adapter_id
                      ) AS source_function
                 FROM continuous.timeseries_source_adapters AS tsa
                WHERE tsa.active
                  AND (
                    tsa.fetch_priority IS NOT NULL
                    OR tsa.synchronize_priority IS NOT NULL
                  )
                GROUP BY tsa.timeseries_id
             )
             SELECT md.timeseries_id,
                    md.location_name,
                    md.alias_name,
                    md.parameter_name,
                    md.units,
                    md.media_type,
                    md.aggregation_type,
                    md.timeseries_type,
                    md.recording_rate,
                    md.start_datetime,
                    md.end_datetime,
                    src.source_function,
                    md.last_new_data,
                    EXTRACT(
                      EPOCH FROM (CURRENT_TIMESTAMP - md.last_new_data)
                    )::double precision AS age_seconds
               FROM automated_sources AS src
               JOIN continuous.timeseries_metadata_en AS md
                 ON md.timeseries_id = src.timeseries_id
              ORDER BY md.last_new_data ASC NULLS FIRST, md.timeseries_id"
          )

          images <- DBI::dbGetQuery(
            con,
            "WITH automated_sources AS (
               SELECT isa.img_series_id,
                      string_agg(
                        isa.source_fx || ' [priority ' ||
                          isa.fetch_priority::text || ']',
                        '; ' ORDER BY isa.fetch_priority,
                                     isa.image_series_source_adapter_id
                      ) AS source_function
                 FROM files.image_series_source_adapters AS isa
                WHERE isa.active
                GROUP BY isa.img_series_id
             )
             SELECT s.img_series_id,
                    loc.name AS location_name,
                    loc.name_fr AS location_name_fr,
                    s.active AS series_active,
                    src.source_function,
                    s.last_img,
                    EXTRACT(
                      EPOCH FROM (CURRENT_TIMESTAMP - s.last_img)
                    )::double precision AS age_seconds
               FROM automated_sources AS src
               JOIN files.image_series AS s
                 ON s.img_series_id = src.img_series_id
               LEFT JOIN public.locations AS loc
                 ON loc.location_id = s.location_id
              ORDER BY s.active DESC, s.last_img ASC NULLS FIRST,
                       s.img_series_id"
          )

          rasters <- DBI::dbGetQuery(
            con,
            "WITH automated_sources AS (
               SELECT rsa.raster_series_id,
                      string_agg(
                        rsa.source_fx || ' [priority ' ||
                          rsa.fetch_priority::text || ']',
                        '; ' ORDER BY rsa.fetch_priority,
                                     rsa.raster_series_source_adapter_id
                      ) AS source_function
                 FROM spatial.raster_series_source_adapters AS rsa
                WHERE rsa.active
                GROUP BY rsa.raster_series_id
             )
             SELECT rsi.raster_series_id,
                    rt.raster_type_name AS raster_type,
                    COALESCE(
                      p.param_name,
                      rsi.parameter,
                      rsi.parameter_id::text
                    ) AS parameter_name,
                    rsi.active AS series_active,
                    src.source_function,
                    rsi.start_datetime,
                    rsi.end_datetime,
                    rsi.last_issue,
                    rsi.last_new_raster,
                    COALESCE(
                      rsi.last_new_raster,
                      rsi.last_issue,
                      rsi.end_datetime
                    ) AS latest_raster_activity,
                    EXTRACT(
                      EPOCH FROM (
                        CURRENT_TIMESTAMP - COALESCE(
                          rsi.last_new_raster,
                          rsi.last_issue,
                          rsi.end_datetime
                        )
                      )
                    )::double precision AS age_seconds
               FROM automated_sources AS src
               JOIN spatial.raster_series_index AS rsi
                 ON rsi.raster_series_id = src.raster_series_id
               LEFT JOIN spatial.raster_types AS rt
                 ON rt.raster_type_id = rsi.raster_type_id
               LEFT JOIN public.parameters AS p
                 ON p.parameter_id = rsi.parameter_id
              ORDER BY rsi.active DESC,
                       COALESCE(rsi.last_new_raster, rsi.last_issue, rsi.end_datetime)
                         ASC NULLS FIRST,
                       rsi.raster_series_id"
          )

          list(
            database = database,
            streams = data.table::as.data.table(streams),
            images = data.table::as.data.table(images),
            rasters = data.table::as.data.table(rasters),
            checked_at = database$checked_at[[1]],
            error = NULL
          )
        },
        error = function(e) {
          list(
            database = NULL,
            streams = NULL,
            images = NULL,
            checked_at = Sys.time(),
            error = conditionMessage(e)
          )
        }
      )
      status_data(status)
    }

    format_elapsed <- function(seconds) {
      if (is.na(seconds)) {
        return(label("No data yet", "Aucune donnée"))
      }
      seconds <- max(0, as.numeric(seconds))
      days <- floor(seconds / 86400)
      hours <- floor((seconds %% 86400) / 3600)
      minutes <- floor((seconds %% 3600) / 60)

      if (days > 0) {
        return(if (identical(language$language, "Français")) {
          paste(days, "j", hours, "h")
        } else {
          paste(days, "d", hours, "h")
        })
      }
      if (hours > 0) return(paste(hours, "h", minutes, "min"))
      if (minutes > 0) return(paste(minutes, "min"))
      label("< 1 min", "< 1 min")
    }

    format_timestamp <- function(value, missing = label("Never", "Jamais")) {
      if (is.null(value) || !length(value) || is.na(value)) {
        return(missing)
      }
      format(value, tz = "UTC", format = "%Y-%m-%d %H:%M:%S UTC")
    }

    freshness_status <- function(age_seconds, kind, enabled = rep(TRUE, length(age_seconds))) {
      enabled[is.na(enabled)] <- FALSE
      status <- rep(label("OK", "À jour"), length(age_seconds))
      if (kind == "stream") {
        status[is.na(age_seconds)] <- label("No data", "Aucune donnée")
        status[!is.na(age_seconds) & age_seconds >= stream_watch_seconds] <- label("Watch", "À surveiller")
        status[!is.na(age_seconds) & age_seconds >= stream_stale_seconds] <- label("Stale", "En retard")
      } else if (kind == "image") {
        status[!enabled] <- label("Paused", "En pause")
        status[enabled & is.na(age_seconds)] <- label("No images", "Aucune image")
        status[enabled & !is.na(age_seconds) & age_seconds >= image_watch_seconds] <- label("Watch", "À surveiller")
        status[enabled & !is.na(age_seconds) & age_seconds >= image_stale_seconds] <- label("Stale", "En retard")
      } else {
        status[!enabled] <- label("Paused", "En pause")
        status[enabled & is.na(age_seconds)] <- label("No rasters", "Aucun raster")
        status[enabled & !is.na(age_seconds) & age_seconds >= raster_watch_seconds] <- label("Watch", "À surveiller")
        status[enabled & !is.na(age_seconds) & age_seconds >= raster_stale_seconds] <- label("Stale", "En retard")
      }
      status
    }

    freshness_row_callback <- function() {
      states <- c(
        "OK", "À jour",
        "Watch", "À surveiller",
        "Stale", "En retard",
        "No data", "Aucune donnée",
        "No images", "Aucune image",
        "No rasters", "Aucun raster",
        "Paused", "En pause"
      )
      colors <- stats::setNames(
        c(
          "#d1e7dd", "#d1e7dd",
          "#ffe5b4", "#ffe5b4",
          "#f8d7da", "#f8d7da",
          "#f8d7da", "#f8d7da",
          "#f8d7da", "#f8d7da",
          "#f8d7da", "#f8d7da",
          "#eeeeee", "#eeeeee"
        ),
        states
      )
      color_map <- paste(
        sprintf("'%s':'%s'", names(colors), unname(colors)),
        collapse = ","
      )
      htmlwidgets::JS(
        paste0(
          "function(row) {",
          "var colors = {", color_map, "};",
          "var state = $('td:last', row).text().trim();",
          "var color = colors[state];",
          "if (color) {",
          "$('td', row).each(function() {",
          "this.style.setProperty('background', color, 'important');",
          "this.style.setProperty('background-image', 'none', 'important');",
          "this.style.setProperty('box-shadow', 'none', 'important');",
          "});",
          "}",
          "}"
        )
      )
    }

    table_options <- list(
      scrollX = TRUE,
      pageLength = 25,
      lengthMenu = c(10, 25, 50, 100),
      order = list(),
      autoWidth = TRUE,
      rowCallback = freshness_row_callback()
    )

    output$summary <- shiny::renderUI({
      status <- status_data()
      shiny::req(status)

      if (!is.null(status$error)) {
        return(shiny::tags$div(
          class = "alert alert-danger",
          role = "alert",
          shiny::tags$strong(label("Status check failed", "Échec de la vérification")),
          shiny::tags$div(status$error)
        ))
      }

      stream_age <- status$streams$age_seconds
      image_age <- status$images$age_seconds
      image_enabled <- as.logical(status$images$series_active)
      raster_age <- status$rasters$age_seconds
      raster_enabled <- as.logical(status$rasters$series_active)
      stream_attention <- sum(
        is.na(stream_age) | stream_age >= stream_watch_seconds
      )
      image_attention <- sum(
        image_enabled & (
          is.na(image_age) | image_age >= image_watch_seconds
        ),
        na.rm = TRUE
      )
      raster_attention <- sum(
        raster_enabled & (
          is.na(raster_age) | raster_age >= raster_watch_seconds
        ),
        na.rm = TRUE
      )
      checked <- format_timestamp(status$checked_at)

      shiny::fluidRow(
        shiny::column(
          3,
          shiny::tags$div(
            class = "alert alert-success",
            shiny::tags$strong(label("Database connection", "Connexion à la base")),
            shiny::tags$div(status$database$database_name[[1]]),
            shiny::tags$small(paste(label("Role", "Rôle"), status$database$database_role[[1]]))
          )
        ),
        shiny::column(
          3,
          shiny::tags$div(
            class = if (stream_attention > 0 || nrow(status$streams) == 0) {
              "alert alert-warning"
            } else {
              "alert alert-success"
            },
            shiny::tags$strong(label("Continuous streams", "Flux continus")),
            shiny::tags$div(paste(
              nrow(status$streams), label("automated", "automatisés"), "·",
              stream_attention, label("need attention", "à surveiller")
            ))
          )
        ),
        shiny::column(
          3,
          shiny::tags$div(
            class = if (image_attention > 0 || !any(image_enabled, na.rm = TRUE)) {
              "alert alert-warning"
            } else {
              "alert alert-success"
            },
            shiny::tags$strong(label("Auto-fetch images", "Images à récupération automatique")),
            shiny::tags$div(paste(
              sum(image_enabled, na.rm = TRUE), label("enabled series", "séries actives"), "·",
              image_attention, label("need attention", "à surveiller")
            ))
          )
        ),
        shiny::column(
          3,
          shiny::tags$div(
            class = if (raster_attention > 0 || !any(raster_enabled, na.rm = TRUE)) {
              "alert alert-warning"
            } else {
              "alert alert-success"
            },
            shiny::tags$strong(label("Automated raster series", "Séries raster automatisées")),
            shiny::tags$div(paste(
              sum(raster_enabled, na.rm = TRUE), label("enabled series", "séries actives"), "·",
              raster_attention, label("need attention", "à surveiller")
            ))
          )
        ),
        shiny::column(
          12,
          shiny::tags$div(
            style = "font-size: 1.2em; font-weight: 700; color: #dc3545;",
            paste(label("Last checked", "Dernière vérification"), checked)
          )
        )
      )
    })

    output$streams_table <- DT::renderDT({
      status <- status_data()
      shiny::req(status)
      if (!is.null(status$error)) {
        return(DT::datatable(data.frame(Note = status$error), rownames = FALSE))
      }

      french <- identical(language$language, "Français")
      data <- data.table::copy(status$streams)
      age <- data$age_seconds
      data[, status := freshness_status(age, "stream")]
      data[, elapsed := vapply(age, format_elapsed, character(1))]
      data[, last_added := vapply(last_new_data, format_timestamp, character(1))]
      data[, location := location_name]
      data[, parameter := parameter_name]
      data[, units_display := units]
      data[, media := media_type]
      data[, aggregation := aggregation_type]
      data[, series_type := timeseries_type]
      data[, rate := recording_rate]
      data[, start_time := vapply(start_datetime, format_timestamp, character(1))]
      data[, end_time := vapply(
        end_datetime,
        function(value) format_timestamp(value, label("Open", "En cours")),
        character(1)
      )]

      for (column in c(
        "location", "alias_name", "parameter", "units_display", "media",
        "aggregation", "series_type", "rate", "source_function", "status"
      )) {
        data.table::set(data, j = column, value = factor(data[[column]]))
      }

      display <- data[, .(
        timeseries_id,
        location,
        alias_name,
        parameter,
        units_display,
        media,
        aggregation,
        series_type,
        rate,
        start_time,
        end_time,
        source_function,
        last_added,
        elapsed,
        status
      )]
      data.table::setnames(
        display,
        c(
          if (french) "Série temporelle" else "Timeseries ID",
          if (french) "Emplacement" else "Location",
          if (french) "Alias" else "Alias",
          if (french) "Paramètre" else "Parameter",
          if (french) "Unités" else "Units",
          if (french) "Type de milieu" else "Media type",
          if (french) "Agrégation" else "Aggregation",
          if (french) "Type de série" else "Timeseries type",
          if (french) "Fréquence" else "Recording rate",
          if (french) "Date de début (UTC)" else "Start datetime (UTC)",
          if (french) "Date de fin (UTC)" else "End datetime (UTC)",
          if (french) "Fonction source" else "Source function",
          if (french) "Dernières données ajoutées (UTC)" else "Last data added (UTC)",
          if (french) "Écoulé" else "Elapsed",
          if (french) "État" else "Status"
        )
      )

      table <- DT::datatable(
        as.data.frame(display),
        rownames = FALSE,
        escape = TRUE,
        filter = "top",
        options = table_options
      )
      table
    })

    output$images_table <- DT::renderDT({
      status <- status_data()
      shiny::req(status)
      if (!is.null(status$error)) {
        return(DT::datatable(data.frame(Note = status$error), rownames = FALSE))
      }

      french <- identical(language$language, "Français")
      data <- data.table::copy(status$images)
      enabled <- as.logical(data$series_active)
      age <- data$age_seconds
      data[, status := freshness_status(age, "image", enabled)]
      data[, elapsed := vapply(age, format_elapsed, character(1))]
      data[, last_image := vapply(last_img, format_timestamp, character(1))]
      data[, location := if (french) {
        data.table::fifelse(is.na(location_name_fr), location_name, location_name_fr)
      } else {
        location_name
      }]
      data[, enabled_display := if (french) {
        data.table::fifelse(enabled, "Oui", "Non")
      } else {
        data.table::fifelse(enabled, "Yes", "No")
      }]
      for (column in c("location", "source_function", "enabled_display", "status")) {
        data.table::set(data, j = column, value = factor(data[[column]]))
      }

      display <- data[, .(
        img_series_id,
        location,
        source_function,
        enabled_display,
        last_image,
        elapsed,
        status
      )]
      data.table::setnames(
        display,
        c(
          if (french) "Série d’images" else "Image series ID",
          if (french) "Emplacement" else "Location",
          if (french) "Fonction source" else "Source function",
          if (french) "Série active" else "Series enabled",
          if (french) "Dernière image (UTC)" else "Last image (UTC)",
          if (french) "Écoulé" else "Elapsed",
          if (french) "État" else "Status"
        )
      )

      table <- DT::datatable(
        as.data.frame(display),
        rownames = FALSE,
        escape = TRUE,
        filter = "top",
        options = table_options
      )
      table
    })

    output$rasters_table <- DT::renderDT({
      status <- status_data()
      shiny::req(status)
      if (!is.null(status$error)) {
        return(DT::datatable(data.frame(Note = status$error), rownames = FALSE))
      }

      french <- identical(language$language, "Français")
      data <- data.table::copy(status$rasters)
      enabled <- as.logical(data$series_active)
      age <- data$age_seconds
      data[, status := freshness_status(age, "raster", enabled)]
      data[, elapsed := vapply(age, format_elapsed, character(1))]
      data[, last_added := vapply(
        last_new_raster,
        function(value) format_timestamp(value, label("Not recorded", "Non enregistré")),
        character(1)
      )]
      data[, latest_activity := vapply(
        latest_raster_activity,
        format_timestamp,
        character(1)
      )]
      data[, start_time := vapply(start_datetime, format_timestamp, character(1))]
      data[, end_time := vapply(
        end_datetime,
        function(value) format_timestamp(value, label("Open", "En cours")),
        character(1)
      )]
      data[, last_issue_time := vapply(
        last_issue,
        function(value) format_timestamp(value, label("Not applicable", "Sans objet")),
        character(1)
      )]
      data[, enabled_display := if (french) {
        data.table::fifelse(enabled, "Oui", "Non")
      } else {
        data.table::fifelse(enabled, "Yes", "No")
      }]
      for (column in c(
        "raster_type", "parameter_name", "enabled_display", "source_function", "status"
      )) {
        data.table::set(data, j = column, value = factor(data[[column]]))
      }

      display <- data[, .(
        raster_series_id,
        raster_type,
        parameter_name,
        enabled_display,
        start_time,
        end_time,
        last_issue_time,
        last_added,
        latest_activity,
        source_function,
        elapsed,
        status
      )]
      data.table::setnames(
        display,
        c(
          if (french) "Série raster" else "Raster series ID",
          if (french) "Type raster" else "Raster type",
          if (french) "Paramètre" else "Parameter",
          if (french) "Série active" else "Series enabled",
          if (french) "Date de début (UTC)" else "Start datetime (UTC)",
          if (french) "Date de fin (UTC)" else "End datetime (UTC)",
          if (french) "Dernière émission (UTC)" else "Last issue (UTC)",
          if (french) "Dernière insertion (UTC)" else "Last added to database (UTC)",
          if (french) "Activité raster la plus récente (UTC)" else "Latest raster activity (UTC)",
          if (french) "Fonction source" else "Source function",
          if (french) "Écoulé" else "Elapsed",
          if (french) "État" else "Status"
        )
      )

      DT::datatable(
        as.data.frame(display),
        rownames = FALSE,
        escape = TRUE,
        filter = "top",
        options = table_options
      )
    })

    shiny::observeEvent(input$refresh, refresh_status(), ignoreInit = TRUE)
    refresh_status()
  })
}
