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

test_that("standardize_param_name returns valid param", {
    expect_equal(
        standardize_param_name("snow water equivalent"),
        "snow water equivalent"
    )
    expect_error(standardize_param_name("not_a_param"))
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
    skip_if_not_installed("YGwater")
    skip_if_not_installed("sf")
    skip_if_not_installed("leaflet")
    con <- tryCatch(YGwater::AquaConnect(silent = TRUE), error = function(e) {
        NULL
    })
    skip_if(is.null(con), "No DB connection")
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
