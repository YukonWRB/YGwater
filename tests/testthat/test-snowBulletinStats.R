# Skip on CI and CRAN because DB connection is mandatory
skip_on_ci()
skip_on_cran()

con <- AquaConnect(silent = TRUE)
on.exit(DBI::dbDisconnect(con), add = TRUE)

# Ensure the snow DB can be accessed before running tests
snowCon <- NULL
tryCatch(
  {
    dets <- DBI::dbGetQuery(
      aquaCon,
      "SELECT inet_server_addr() AS ip, inet_server_port() AS port"
    )
    snowCon <- snowConnect(
      host = as.character(dets$ip),
      port = as.numeric(dets$port),
      silent = TRUE
    )
    on.exit(DBI::dbDisconnect(snowCon), add = TRUE)
  },
  error = function(e) {
    snowCon <<- NULL
    message(
      "Unable to connect to the snow database. Skipping tests that require snow connection."
    )
  }
)

test_that("output to console is consistent with expectations", {
  skip_if(is.null(snowCon), "Skipping tests that require snow connection.")
  res <- snowBulletinStats(2024, 3)
  expect_type(res, "list")
  expect_equal(length(res), 9)
  expect_named(
    res,
    c(
      "pillow_stats",
      "station_stats",
      "basin_stats",
      "precip_stats",
      "cddf_stats",
      "flow_stats",
      "swe_basin_summary",
      "swe_compiled",
      "symbols"
    ),
    ignore.order = TRUE
  )
  expect_snapshot_value(res$pillow_stats, style = "deparse")
  expect_snapshot_value(res$station_stats, style = "deparse")
  expect_snapshot_value(res$basin_stats, style = "deparse")
  expect_snapshot_value(res$precip_stats, style = "deparse")
  expect_snapshot_value(res$cddf_stats, style = "deparse")
  expect_snapshot_value(res$flow_stats, style = "deparse")
  expect_snapshot_value(res$swe_basin_summary, style = "deparse")
  expect_snapshot_value(res$swe_compiled, style = "deparse")
  expect_snapshot_value(res$symbols, style = "deparse")
})

test_that("In non-interactive mode, excel_output requires a valid save_path", {
  skip_if(interactive(), "This test runs only in non-interactive mode")
  expect_error(
    snowBulletinStats(
      year = 2020,
      month = 3,
      excel_output = TRUE,
      save_path = "choose"
    ),
    "You must specify a save path when running in non-interactive mode."
  )
})

test_that("Invalid basins trigger error", {
  expect_error(
    snowBulletinStats(year = 2020, month = 3, basins = "Invalid Basin"),
    "Basins must be one or more of"
  )
})

test_that("Invalid source triggers error", {
  expect_error(
    snowBulletinStats(year = 2020, month = 3, source = "invalid"),
    "Source must be 'aquacache' or 'snow'."
  )
})

test_that("Invalid month triggers error", {
  expect_error(
    snowBulletinStats(year = 2020, month = 2),
    "Month must be 3, 4 or 5."
  )
})

test_that("Excel file are written to disk", {
  skip_if(is.null(snowCon), "Skipping tests that require snow connection.")
  dir <- paste0(tempdir(), "/snowBulletinStats")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  suppressMessages(snowBulletinStats(
    year = 2024,
    month = 3,
    excel_output = TRUE,
    save_path = dir
  ))
  files <- list.files(dir)

  expect_true(length(files) == 2)
})
