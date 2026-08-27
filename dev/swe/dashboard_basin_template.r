# Standalone basin dashboard template.
# Source this file and call run_basin_template_app().

#' Open AquaCache connection for dashboard queries
#'
#' @return DBI connection object or NULL if connection fails.
#' @noRd
basin_db_connect <- function() {
    tryCatch(
        {
            YGwater::AquaConnect(
                name = "aquacache",
                host = Sys.getenv("aquacacheHostProd"),
                port = Sys.getenv("aquacachePortProd"),
                username = Sys.getenv("aquacacheAdminUser"),
                password = Sys.getenv("aquacacheAdminPass"),
                silent = TRUE
            )
        },
        error = function(e) {
            message("AquaCache connection failed: ", e$message)
            NULL
        }
    )
}

#' Query latest discrete SWE values for a month/year by location
#'
#' @param con DBI connection.
#' @param year Integer year.
#' @param month Integer month (3, 4, 5).
#' @param param_name Parameter name in public.parameters.
#'
#' @return data.frame with location metadata and SWE values.
#' @noRd
get_discrete_swe_month <- function(
    con,
    year,
    month,
    param_name = "snow water equivalent"
) {
    if (is.null(con)) {
        return(data.frame(
            location_id = integer(),
            location_code = character(),
            name = character(),
            latitude = numeric(),
            longitude = numeric(),
            obs_datetime = as.POSIXct(character()),
            target_month_datetime = as.POSIXct(character()),
            most_recent_datetime = as.POSIXct(character()),
            historical_median_swe = numeric(),
            swe_mm = numeric(),
            stringsAsFactors = FALSE
        ))
    }

    sql <- paste(
        "WITH swe_locations AS (",
        "  SELECT DISTINCT",
        "    l.location_id,",
        "    l.location_code,",
        "    l.name,",
        "    l.latitude,",
        "    l.longitude",
        "  FROM public.locations l",
        "  JOIN discrete.samples s ON s.location_id = l.location_id",
        "  JOIN discrete.results r ON r.sample_id = s.sample_id",
        "  WHERE r.parameter_id = (",
        "    SELECT parameter_id",
        "    FROM public.parameters",
        "    WHERE param_name = $1",
        "  )",
        "),",
        "latest AS (",
        "  SELECT",
        "    s.location_id,",
        "    COALESCE(s.target_datetime, s.datetime) AS most_recent_datetime,",
        "    ROW_NUMBER() OVER (",
        "      PARTITION BY s.location_id",
        "      ORDER BY COALESCE(s.target_datetime, s.datetime) DESC",
        "    ) AS rn",
        "  FROM discrete.samples s",
        "  JOIN discrete.results r ON r.sample_id = s.sample_id",
        "  WHERE r.parameter_id = (",
        "    SELECT parameter_id",
        "    FROM public.parameters",
        "    WHERE param_name = $1",
        "  )",
        "    AND r.result IS NOT NULL",
        "),",
        "historical AS (",
        "  SELECT",
        "    s.location_id,",
        "    percentile_cont(0.5) WITHIN GROUP (ORDER BY r.result) AS historical_median_swe",
        "  FROM discrete.samples s",
        "  JOIN discrete.results r ON r.sample_id = s.sample_id",
        "  WHERE r.parameter_id = (",
        "    SELECT parameter_id",
        "    FROM public.parameters",
        "    WHERE param_name = $1",
        "  )",
        "    AND r.result IS NOT NULL",
        "    AND s.target_datetime IS NOT NULL",
        "    AND EXTRACT(YEAR FROM s.target_datetime) BETWEEN 1990 AND 2020",
        "    AND EXTRACT(MONTH FROM s.target_datetime) = $3",
        "  GROUP BY s.location_id",
        "),",
        "monthly AS (",
        "  SELECT",
        "    s.location_id,",
        "    s.datetime AS obs_datetime,",
        "    s.target_datetime AS target_month_datetime,",
        "    r.result AS swe_mm,",
        "    ROW_NUMBER() OVER (",
        "      PARTITION BY s.location_id",
        "      ORDER BY s.target_datetime DESC, s.datetime DESC",
        "    ) AS rn",
        "  FROM discrete.samples s",
        "  JOIN discrete.results r ON r.sample_id = s.sample_id",
        "  WHERE r.parameter_id = (",
        "    SELECT parameter_id",
        "    FROM public.parameters",
        "    WHERE param_name = $1",
        "  )",
        "    AND r.result IS NOT NULL",
        "    AND s.target_datetime IS NOT NULL",
        "    AND EXTRACT(YEAR FROM s.target_datetime) = $2",
        "    AND EXTRACT(MONTH FROM s.target_datetime) = $3",
        ")",
        "SELECT",
        "  sl.location_id,",
        "  sl.location_code,",
        "  sl.name,",
        "  sl.latitude,",
        "  sl.longitude,",
        "  m.obs_datetime,",
        "  m.target_month_datetime,",
        "  lt.most_recent_datetime,",
        "  hs.historical_median_swe,",
        "  m.swe_mm",
        "FROM swe_locations sl",
        "LEFT JOIN monthly m",
        "  ON m.location_id = sl.location_id",
        " AND m.rn = 1",
        "LEFT JOIN latest lt",
        "  ON lt.location_id = sl.location_id",
        " AND lt.rn = 1",
        "LEFT JOIN historical hs",
        "  ON hs.location_id = sl.location_id",
        "ORDER BY sl.name"
    )

    out <- DBI::dbGetQuery(
        con,
        sql,
        params = list(param_name, as.integer(year), as.integer(month))
    )

    if (nrow(out) == 0) {
        return(out)
    }

    out$obs_datetime <- as.POSIXct(out$obs_datetime, tz = "UTC")
    out$target_month_datetime <- as.POSIXct(
        out$target_month_datetime,
        tz = "UTC"
    )
    out$most_recent_datetime <- as.POSIXct(out$most_recent_datetime, tz = "UTC")
    out$historical_median_swe <- as.numeric(out$historical_median_swe)
    out$swe_mm <- as.numeric(out$swe_mm)
    out$swe_mm[is.na(out$swe_mm)] <- NaN
    out
}

#' Build Basin Dashboard UI
#'
#' @return A Shiny UI definition.
#' @noRd
basin_app_ui <- function() {
    current_year <- as.integer(format(Sys.Date(), "%Y"))

    shiny::fluidPage(
        shiny::titlePanel("Basin SWE Dashboard"),
        shiny::fluidRow(
            shiny::column(
                width = 2,
                shiny::selectInput(
                    inputId = "month",
                    label = "Month",
                    choices = c("March" = 3L, "April" = 4L, "May" = 5L),
                    selected = 3L
                )
            ),
            shiny::column(
                width = 2,
                shiny::selectInput(
                    inputId = "year",
                    label = "Year",
                    choices = as.character(current_year:2000),
                    selected = as.character(current_year)
                )
            ),
            shiny::column(
                width = 2,
                shiny::selectInput(
                    inputId = "parameter",
                    label = "Parameter",
                    choices = c("SWE" = "snow water equivalent"),
                    selected = "snow water equivalent"
                )
            ),
            shiny::column(
                width = 2,
                shiny::selectizeInput(
                    inputId = "map_display_mode",
                    label = "Map display",
                    choices = c("Status", "SWE value", "Relative"),
                    selected = "Status",
                    options = list(create = FALSE)
                )
            ),
            shiny::column(
                width = 2,
                shiny::checkboxInput(
                    inputId = "inactive_station_mode",
                    label = "Hide inactive stations",
                    value = FALSE
                )
            ),
            shiny::column(
                width = 2,
                shiny::checkboxInput(
                    inputId = "hide_completed_mode",
                    label = "Hide completed stations",
                    value = FALSE
                )
            )
        ),
        shiny::br(),
        shiny::fluidRow(
            shiny::column(
                width = 6,
                shiny::h4("Discrete SWE Locations"),
                DT::dataTableOutput("locations_table", width = "100%")
            ),
            shiny::column(
                width = 6,
                leaflet::leafletOutput("locations_map", height = "75vh")
            )
        )
    )
}

#' Build Basin Dashboard server
#'
#' @param input,output,session Standard Shiny server parameters.
#' @param db_con Optional external DBI connection.
#' @noRd
basin_app_server <- function(input, output, session, db_con = NULL) {
    owns_connection <- is.null(db_con)
    con <- if (is.null(db_con)) basin_db_connect() else db_con

    if (owns_connection) {
        session$onSessionEnded(function() {
            if (!is.null(con) && DBI::dbIsValid(con)) {
                try(DBI::dbDisconnect(con), silent = TRUE)
            }
        })
    }

    monthly_swe <- shiny::reactive({
        shiny::req(input$month, input$year, input$parameter)

        tryCatch(
            {
                get_discrete_swe_month(
                    con = con,
                    year = as.integer(input$year),
                    month = as.integer(input$month),
                    param_name = input$parameter
                )
            },
            error = function(e) {
                shiny::showNotification(
                    paste("Database query failed:", e$message),
                    type = "error"
                )
                data.frame(
                    location_id = integer(),
                    location_code = character(),
                    name = character(),
                    latitude = numeric(),
                    longitude = numeric(),
                    obs_datetime = as.POSIXct(character()),
                    target_month_datetime = as.POSIXct(character()),
                    most_recent_datetime = as.POSIXct(character()),
                    historical_median_swe = numeric(),
                    swe_mm = numeric(),
                    stringsAsFactors = FALSE
                )
            }
        )
    })

    status_annotated_swe <- shiny::reactive({
        dat <- monthly_swe()
        most_recent_date <- as.Date(dat$most_recent_datetime)
        active_cutoff <- Sys.Date() - 365
        dat$is_active <- !is.na(most_recent_date) &
            most_recent_date >= active_cutoff
        dat$active_status <- ifelse(dat$is_active, "Active", "Inactive")

        dat$relative_percent <- NaN
        valid_relative <- !is.nan(dat$swe_mm) &
            !is.na(dat$historical_median_swe) &
            dat$historical_median_swe > 0
        dat$relative_percent[valid_relative] <-
            dat$swe_mm[valid_relative] *
            100 /
            dat$historical_median_swe[valid_relative]

        dat
    })

    table_swe <- shiny::reactive({
        dat <- status_annotated_swe()

        if (isTRUE(input$inactive_station_mode)) {
            dat <- dat[dat$is_active, , drop = FALSE]
        }

        if (isTRUE(input$hide_completed_mode)) {
            dat <- dat[is.nan(dat$swe_mm), , drop = FALSE]
        }

        dat
    })

    selected_location_ids <- shiny::reactive({
        dat <- table_swe()
        selected_rows <- input$locations_table_rows_selected

        if (length(selected_rows) == 0) {
            return(integer())
        }

        selected_rows <- selected_rows[
            selected_rows >= 1 & selected_rows <= nrow(dat)
        ]
        if (length(selected_rows) == 0) {
            return(integer())
        }

        dat$location_id[selected_rows]
    })

    output$locations_table <- DT::renderDataTable({
        dat <- table_swe()

        if (isTRUE(input$inactive_station_mode)) {
            shiny::validate(shiny::need(
                nrow(dat) > 0,
                "No active SWE locations found."
            ))
            dat <- dat[,
                c(
                    "location_code",
                    "name",
                    "obs_datetime",
                    "most_recent_datetime",
                    "historical_median_swe",
                    "swe_mm"
                ),
                drop = FALSE
            ]
            names(dat) <- c(
                "Station ID",
                "Location",
                "Survey date",
                "Most recent sample date",
                "Historical median",
                "SWE (mm)"
            )
        } else {
            shiny::validate(shiny::need(
                nrow(dat) > 0,
                "No SWE locations found."
            ))
            dat <- dat[,
                c(
                    "location_code",
                    "name",
                    "obs_datetime",
                    "most_recent_datetime",
                    "active_status",
                    "historical_median_swe",
                    "swe_mm"
                ),
                drop = FALSE
            ]
            names(dat) <- c(
                "Station ID",
                "Location",
                "Survey date",
                "Most recent sample date",
                "Status",
                "Historical median",
                "SWE (mm)"
            )
        }
        dat[["Historical median"]] <- round(dat[["Historical median"]], 1)
        dat[["SWE (mm)"]] <- round(dat[["SWE (mm)"]], 1)
        dat[["Survey date"]] <- ifelse(
            is.na(dat[["Survey date"]]),
            "No data",
            format(dat[["Survey date"]], "%b %d, %Y")
        )
        dat[["Most recent sample date"]] <- ifelse(
            is.na(dat[["Most recent sample date"]]),
            "No data",
            format(dat[["Most recent sample date"]], "%b %Y")
        )
        dat[["Historical median"]] <- ifelse(
            is.na(dat[["Historical median"]]),
            "NaN",
            sprintf("%.1f", dat[["Historical median"]])
        )
        dat[["SWE (mm)"]] <- ifelse(
            is.nan(dat[["SWE (mm)"]]),
            "NaN",
            sprintf("%.1f", dat[["SWE (mm)"]])
        )

        DT::datatable(
            dat,
            rownames = FALSE,
            filter = "none",
            selection = list(mode = "multiple", target = "row"),
            width = "100%",
            options = list(
                dom = "tip",
                pageLength = 15,
                scrollX = FALSE,
                autoWidth = FALSE,
                order = list(list(1, "asc"))
            )
        )
    })

    output$locations_map <- leaflet::renderLeaflet({
        dat <- status_annotated_swe()
        shiny::req(input$map_display_mode)

        m <- leaflet::leaflet() |>
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

        if (nrow(dat) == 0) {
            return(
                m |>
                    leaflet::setView(lng = -135, lat = 64.5, zoom = 5)
            )
        }

        roads <- tryCatch(
            {
                YGwater::getVector(
                    layer_name = "Roads",
                    con = con,
                    silent = TRUE
                )
            },
            error = function(e) {
                message("Roads layer not found: ", e$message)
                NULL
            }
        )
        if (!is.null(roads)) {
            roads_sf <- sf::st_transform(sf::st_as_sf(roads), 4326)
            m <- m |>
                leaflet::addPolylines(
                    data = roads_sf,
                    color = "#888888",
                    weight = 2,
                    opacity = 0.6,
                    dashArray = "5, 5",
                    popup = ~feature_name
                )
        }

        communities <- tryCatch(
            {
                YGwater::getVector(
                    layer_name = "Communities",
                    con = con,
                    silent = TRUE
                )
            },
            error = function(e) {
                message("Communities layer not found: ", e$message)
                NULL
            }
        )
        if (!is.null(communities)) {
            communities_sf <- sf::st_transform(sf::st_as_sf(communities), 4326)
            geom_type <- sf::st_geometry_type(communities_sf)[1]

            if (geom_type %in% c("POINT", "MULTIPOINT")) {
                m <- m |>
                    leaflet::addCircleMarkers(
                        data = communities_sf,
                        radius = 4,
                        color = "#000000",
                        fillColor = "#000000",
                        fillOpacity = 1,
                        weight = 1,
                        popup = ~feature_name
                    )
            } else if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
                m <- m |>
                    leaflet::addPolygons(
                        data = communities_sf,
                        color = "#000000",
                        weight = 2,
                        fillColor = "#000000",
                        fillOpacity = 0.3,
                        popup = ~feature_name
                    )
            }
        }

        dat <- dat[
            stats::complete.cases(dat[, c("latitude", "longitude")]),
            ,
            drop = FALSE
        ]
        has_data <- !is.nan(dat$swe_mm)
        has_relative <- !is.nan(dat$relative_percent)
        is_active <- dat$is_active

        if (identical(input$map_display_mode, "SWE value")) {
            gradient_values <- dat$swe_mm[is_active & has_data]
            pal <- leaflet::colorNumeric(
                palette = "YlGnBu",
                domain = if (length(gradient_values) > 0) {
                    gradient_values
                } else {
                    c(0, 1)
                },
                na.color = "#EF6C00"
            )

            marker_fill <- ifelse(
                !is_active,
                "#9E9E9E",
                ifelse(has_data, pal(dat$swe_mm), "#EF6C00")
            )
            marker_edge <- ifelse(!is_active, "#616161", "#1F2A44")
        } else if (identical(input$map_display_mode, "Relative")) {
            pal <- leaflet::colorNumeric(
                palette = c("#2E7D32", "#FFFFFF", "#1565C0"),
                domain = c(0, 200),
                na.color = "#EF6C00"
            )

            marker_fill <- ifelse(
                !is_active,
                "#9E9E9E",
                ifelse(
                    has_relative,
                    pal(pmax(0, pmin(200, dat$relative_percent))),
                    "#EF6C00"
                )
            )
            marker_edge <- ifelse(!is_active, "#616161", "#1F2A44")
        } else {
            marker_fill <- ifelse(
                !is_active,
                "#9E9E9E",
                ifelse(has_data, "#1565C0", "#EF6C00")
            )
            marker_edge <- ifelse(
                !is_active,
                "#616161",
                ifelse(has_data, "#0D47A1", "#E65100")
            )
        }

        popup_date <- ifelse(
            is.na(dat$obs_datetime),
            "No data",
            format(dat$obs_datetime, "%b %d, %Y")
        )
        popup_target_month_date <- ifelse(
            is.na(dat$target_month_datetime),
            "No data",
            format(dat$target_month_datetime, "%b %d, %Y")
        )
        popup_most_recent_date <- ifelse(
            is.na(dat$most_recent_datetime),
            "No data",
            format(dat$most_recent_datetime, "%b %Y")
        )
        popup_historical_swe <- ifelse(
            is.na(dat$historical_median_swe),
            "NaN",
            sprintf("%.1f", dat$historical_median_swe)
        )
        popup_swe <- ifelse(
            is.nan(dat$swe_mm),
            "NaN",
            sprintf("%.1f", dat$swe_mm)
        )
        popup_relative <- ifelse(
            is.nan(dat$relative_percent),
            "NaN",
            sprintf("%.1f", dat$relative_percent)
        )

        m <- m |>
            leaflet::addCircleMarkers(
                lng = dat$longitude,
                lat = dat$latitude,
                radius = 6,
                stroke = TRUE,
                weight = 2,
                color = marker_edge,
                fillColor = marker_fill,
                fillOpacity = 0.9,
                popup = sprintf(
                    "<b>%s</b><br/>Code: %s<br/>Target month date: %s<br/>Survey date: %s<br/>Most recent date: %s<br/>Historical SWE: %s mm<br/>SWE: %s mm<br/>Relative: %s%%",
                    dat$name,
                    dat$location_code,
                    popup_target_month_date,
                    popup_date,
                    popup_most_recent_date,
                    popup_historical_swe,
                    popup_swe,
                    popup_relative
                )
            )

        if (identical(input$map_display_mode, "SWE value")) {
            if (any(is_active & has_data)) {
                m <- m |>
                    leaflet::addLegend(
                        position = "bottomright",
                        pal = pal,
                        values = dat$swe_mm[is_active & has_data],
                        title = "SWE (mm)",
                        opacity = 1
                    )
            }

            m |>
                leaflet::addLegend(
                    position = "bottomleft",
                    colors = c("#EF6C00", "#9E9E9E"),
                    labels = c("No data", "Inactive"),
                    title = "Station status",
                    opacity = 1
                )
        } else if (identical(input$map_display_mode, "Relative")) {
            if (any(is_active & has_relative)) {
                m <- m |>
                    leaflet::addLegend(
                        position = "bottomright",
                        pal = pal,
                        values = c(0, 200),
                        title = "Relative (% of median)",
                        opacity = 1
                    )
            }

            m |>
                leaflet::addLegend(
                    position = "bottomleft",
                    colors = c("#EF6C00", "#9E9E9E"),
                    labels = c("No data", "Inactive"),
                    title = "Station status",
                    opacity = 1
                )
        } else {
            m |>
                leaflet::addLegend(
                    position = "bottomright",
                    colors = c("#1565C0", "#EF6C00", "#9E9E9E"),
                    labels = c("Completed", "No data", "Inactive"),
                    title = "Station status",
                    opacity = 1
                )
        }
    })

    shiny::observe({
        dat <- status_annotated_swe()
        selected_ids <- selected_location_ids()

        dat <- dat[
            stats::complete.cases(dat[, c("latitude", "longitude")]),
            ,
            drop = FALSE
        ]
        selected_dat <- dat[dat$location_id %in% selected_ids, , drop = FALSE]

        leaflet::leafletProxy("locations_map") |>
            leaflet::clearGroup("selected_highlight")

        if (nrow(selected_dat) > 0) {
            leaflet::leafletProxy("locations_map") |>
                leaflet::addCircleMarkers(
                    lng = selected_dat$longitude,
                    lat = selected_dat$latitude,
                    radius = 10,
                    stroke = TRUE,
                    weight = 3,
                    color = "#000000",
                    fill = FALSE,
                    opacity = 1,
                    group = "selected_highlight"
                )
        }
    })
}

#' Run Basin SWE Dashboard
#'
#' @param db_con Optional DBI connection. If NULL, app opens its own connection.
#'
#' @return A running Shiny app object.
#' @export
run_basin_template_app <- function(db_con = NULL) {
    shiny::shinyApp(
        ui = basin_app_ui(),
        server = function(input, output, session) {
            basin_app_server(input, output, session, db_con = db_con)
        }
    )
}

if (interactive()) {
    run_basin_template_app()
}
