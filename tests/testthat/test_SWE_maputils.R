test_that("snowbull_months returns correct month names", {
    expect_equal(snowbull_months(3), "March")
    expect_equal(snowbull_months(3, short = TRUE), "mar")
    expect_length(snowbull_months(), 12)
})

test_that("get_static_style_elements returns a list", {
    style <- get_static_style_elements()
    expect_type(style, "list")
    expect_true("basins" %in% names(style))
})

test_that("get_dynamic_style_elements returns a list with bins/colors/labels", {
    style <- get_dynamic_style_elements("relative_to_med")
    expect_type(style, "list")
    expect_true(all(c("bins", "colors", "labels") %in% names(style)))
})

test_that("get_state_style_elements returns correct color mapping", {
    style <- get_dynamic_style_elements("relative_to_med")
    vals <- c(NA, 0, 50, 100, 200)
    cols <- get_state_style_elements(vals, style)
    expect_length(cols, length(vals))
})

test_that("get_display_data does not require polygon labels for point layers", {
    state <- data.frame(
        name = c("Station A", "Station B"),
        location = c("A", "B"),
        latest_date = as.POSIXct(c("2026-05-01", "2026-05-01"), tz = "UTC"),
        value = c(120, 140),
        relative_to_med = c(80, 110),
        historic_median = c(150, 127),
        percentile = c(25, 60),
        anomalies = c(-30, 13)
    )

    result <- testthat::with_mocked_bindings(
        get_display_data(
            dataset = list(
                timeseries = list(data = data.frame()),
                metadata = data.frame(),
                geom = "point",
                continuity = "discrete",
                param_name = "snow water equivalent"
            ),
            year = 2026,
            month = 5,
            statistic = "relative_to_med",
            october_start = FALSE
        ),
        get_state_as_shp = function(...) state,
        .package = "YGwater"
    )

    expect_equal(nrow(result), nrow(state))
    expect_false(any(c("annotation_en", "annotation_fr") %in% names(result)))
})

test_that("get_display_data requires translated labels for polygon layers", {
    state <- data.frame(
        name = "Basin A",
        value = 120,
        relative_to_med = 80,
        historic_median = 150,
        percentile = 25,
        anomalies = -30
    )

    expect_error(
        testthat::with_mocked_bindings(
            get_display_data(
                dataset = list(
                    timeseries = list(data = data.frame()),
                    metadata = data.frame(),
                    geom = "poly",
                    continuity = "discrete",
                    param_name = "snow water equivalent"
                ),
                year = 2026,
                month = 5,
                statistic = "relative_to_med",
                october_start = FALSE
            ),
            get_state_as_shp = function(...) state,
            .package = "YGwater"
        ),
        "Polygon metadata is missing required annotation column"
    )
})

test_that("get_display_data preserves generated polygon line breaks", {
    state <- data.frame(
        name = "Lower_Yukon",
        value = 120,
        relative_to_med = 80,
        historic_median = 150,
        percentile = 25,
        anomalies = -30,
        annotation_en = "Lower<br>Yukon",
        annotation_fr = "Bas<br>Yukon <script>"
    )

    result <- testthat::with_mocked_bindings(
        get_display_data(
            dataset = list(
                timeseries = list(data = data.frame()),
                metadata = data.frame(),
                geom = "poly",
                continuity = "discrete",
                param_name = "snow water equivalent"
            ),
            year = 2026,
            month = 5,
            statistic = "relative_to_med",
            october_start = FALSE
        ),
        get_state_as_shp = function(...) state,
        .package = "YGwater"
    )

    expect_identical(result$annotation_en, "Lower<br>Yukon<br>(80 %)")
    expect_identical(
        result$annotation_fr,
        "Bas<br>Yukon &lt;script&gt;<br>(80 %)"
    )
})

test_that("standardize_swe_param_name returns valid param", {
    expect_equal(
        standardize_swe_param_name("snow water equivalent"),
        "snow water equivalent"
    )
    expect_error(standardize_swe_param_name("not_a_param"))
})

test_that("get_period_dates returns correct start/end", {
    pd <- get_period_dates(2025, 3, october_start = TRUE)
    expect_true(all(c("start_date", "end_date") %in% names(pd)))
})

test_that("get_bulletin_value returns named vector", {
    ts <- data.frame(datetime = as.Date("2025-03-01") + 0:2, A = 1:3, B = 4:6)
    vals <- get_bulletin_value(3, 2025, ts, "snow water equivalent")
    expect_named(vals)
})

test_that("get_norms returns station_norms and historical_distr", {
    ts <- data.frame(datetime = as.Date("2020-03-01") + 0:2, A = 1:3, B = 4:6)
    n <- get_norms(
        ts,
        "snow water equivalent",
        start_year_historical = 2020,
        end_year_historical = 2020,
        end_months_historical = 3
    )
    expect_true(all(c("station_norms", "historical_distr") %in% names(n)))
})

test_that("make_snowbull_map runs and returns expected type", {
    skip_on_cran()
    skip_on_ci()
    skip_if_not_installed("YGwater")
    skip_if_not_installed("sf")
    skip_if_not_installed("leaflet")
    con <- tryCatch(AquaConnect(silent = TRUE), error = function(e) {
        NULL
    })
    # Look for the 'spatial' schema to confirm connection is valid. Might be missing on CI
    if (!is.null(con)) {
        schemas <- DBI::dbGetQuery(
            con,
            "SELECT schema_name FROM information_schema.schemata"
        )
        if (!"spatial" %in% schemas$schema_name) {
            skip("DB connection does not have 'spatial' schema, skipping test")
        }
    } else {
        skip("Unable to connect to DB, skipping test")
    }

    # Test leaflet output and also save as HTML for checking
    result <- make_snowbull_map(
        year = 2025,
        month = 3,
        param_name = "snow water equivalent",
        statistic = "relative_to_med",
        format = "leaflet",
        con = con
    )
    expect_true(inherits(result, "leaflet"))
    # Optionally, save to a temp HTML file and check existence
    tmp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(result, tmp_html)
    expect_true(file.exists(tmp_html))
})
