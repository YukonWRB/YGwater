flood_image_test_server <- function() {
  path <- testthat::test_path(
    "..", "..", "inst", "apps", "aqualink", "modules", "client",
    "reports", "floodDashboard.R"
  )
  lines <- readLines(path, warn = FALSE)
  start <- grep("selected_img_series_id <-", lines, fixed = TRUE)
  end <- grep("output$station_image <-", lines, fixed = TRUE)
  code <- parse(text = lines[start:(end - 1L)])
  mocked <- c(
    "community_image_series", "selected_image_series",
    "available_image_dates", "available_image_timestamps"
  )
  code <- code[!vapply(code, function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.symbol(expr[[2]]) &&
      as.character(expr[[2]]) %in% mocked
  }, logical(1))]

  function(input, output, session) {
    language <- shiny::reactiveValues(language = "en")
    tr <- function(key, lang) key
    community_image_series <- shiny::reactive(data.frame(
      img_series_id = 1L, name = "Test camera"
    ))
    available_image_dates <- shiny::reactive(as.Date(c(
      "2026-10-10", "2026-10-12", "2026-10-13"
    )))
    available_image_timestamps <- shiny::reactive(data.frame(
      image_id = 1:3,
      datetime = as.POSIXct(selected_image_date(), tz = "Etc/GMT+7") +
        c(8, 12, 16) * 3600
    ))

    # Exercise the actual image controls without a spatial database connection.
    eval(code, envir = environment())
    session$userData$image_date <- selected_image_date
    session$userData$image_index <- selected_image_ts_index
  }
}

test_that("image navigation keeps a single date picker and stable controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(flood_image_test_server(), {
    session$flushReact()
    session$setInputs(image_date_select = as.Date("2026-10-12"))
    header <- output$image_series_header
    date_id <- paste0('id="', session$ns("image_date_select"), '"')
    expect_length(
      gregexpr(date_id, header$html, fixed = TRUE)[[1]],
      1L
    )
    expect_match(header$html, date_id, fixed = TRUE)

    session$setInputs(previous_image_series = 1L)
    expect_equal(session$userData$image_date(), "2026-10-10")
    expect_identical(output$image_series_header, header)
    expect_match(output$image_series_navigation$html, "10-Oct-26")

    session$setInputs(image_date_select = as.Date("2026-10-10"))
    session$setInputs(next_image_series = 1L)
    expect_equal(session$userData$image_date(), "2026-10-12")
    expect_identical(output$image_series_header, header)

    session$setInputs(image_date_select = as.Date("2026-10-12"))
    session$setInputs(next_image_series = 2L)
    expect_equal(session$userData$image_date(), "2026-10-13")
    session$setInputs(image_date_select = as.Date("2026-10-13"))
    session$setInputs(next_image_series = 3L)
    expect_equal(session$userData$image_date(), "2026-10-13")
    expect_identical(output$image_series_header, header)

    session$setInputs(previous_image_series = 2L)
    expect_equal(session$userData$image_date(), "2026-10-12")
    session$setInputs(image_date_select = as.Date("2026-10-10"))
    session$setInputs(previous_image_series = 3L)
    expect_equal(session$userData$image_date(), "2026-10-10")
    expect_identical(output$image_series_header, header)
  })
})

test_that("date input synchronization preserves the reference-time image", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(flood_image_test_server(), {
    session$flushReact()
    session$setInputs(image_date_select = as.Date("2026-10-10"))
    header <- output$image_series_header
    session$setInputs(time0 = "2026-10-12T13:00")
    expect_equal(session$userData$image_date(), "2026-10-12")
    expect_equal(session$userData$image_index(), 2L)

    session$setInputs(image_date_select = as.Date("2026-10-12"))
    expect_equal(session$userData$image_index(), 2L)
    expect_identical(output$image_series_header, header)
    expect_match(output$image_series_navigation$html, "12-Oct-26 12:00")

    session$setInputs(image_date_select = as.Date("2026-10-13"))
    expect_equal(session$userData$image_date(), "2026-10-13")
    expect_true(is.na(session$userData$image_index()))
    expect_match(output$image_series_navigation$html, "13-Oct-26 16:00")
    expect_identical(output$image_series_header, header)
  })
})
