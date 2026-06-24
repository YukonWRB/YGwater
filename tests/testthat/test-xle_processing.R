# Test that all testing dependencies are as expected
test_that("YOWN master is as expected", {
  # Check YOWN_MASTER.xlsx
  master <- openxlsx::read.xlsx(
    test_path("fixtures/logger_reads/YOWN_MASTER.xlsx"),
    sheet = 1
  )
  expect_snapshot(master)
})

test_that("YOWN logger tracking is as expected", {
  # Check YOWN_Logger_Tracking.xlsx
  tracking <- openxlsx::read.xlsx(
    test_path("fixtures/logger_reads/YOWN_Logger_Tracking.xlsx"),
    sheet = 1
  )
  expect_snapshot(tracking)
})

test_that("xle file is as expected", {
  # Check .xle file
  xle <- xml2::read_xml(test_path(
    "fixtures/logger_reads/1071079_YOWN-0000_TEST_2020_05_07.xle"
  ))
  expect_snapshot(xle)
})


test_that("xle file can be read", {
  # Create a temporary directory to store the output
  unlink(paste0(tempdir(), "/xle_processing_test"), recursive = TRUE)
  dir <- paste0(tempdir(), "/xle_processing_test")
  dir.create(dir)

  # Clean up on exit
  on.exit(unlink(dir, recursive = TRUE))

  res <- xle_processing(
    file = test_path(
      "fixtures/logger_reads/1071079_YOWN-0000_TEST_2020_05_07.xle"
    ),
    aq_upload = FALSE, # Will force function to output a data.frame instead of aquarius upload
    master_file = test_path("fixtures/logger_reads/YOWN_MASTER.xlsx"),
    logger_tracking = test_path(
      "fixtures/logger_reads/YOWN_Logger_Tracking.xlsx"
    ),
    dropbox = dir,
    repo = NULL
  )

  # Check that the returned tibble is as expected
  expect_snapshot(res)

  # check that the logger tracking file has a new row
  logger_tracking <- openxlsx::read.xlsx(
    test_path("fixtures/logger_reads/YOWN_Logger_Tracking.xlsx"),
    sheet = 1
  )
  expect_equal(nrow(logger_tracking), 1)

  # Delete the new row in the logger tracking file
  openxlsx::write.xlsx(
    logger_tracking[-1, ],
    test_path("fixtures/logger_reads/YOWN_Logger_Tracking.xlsx"),
    overwrite = TRUE
  )
})

test_that("logger file reader converts xle to uploadable data", {
  res <- read_logger_file_data(
    test_path("fixtures/logger_reads/1071079_YOWN-0000_TEST_2020_05_07.xle")
  )

  expect_s3_class(res, "data.frame")
  expect_true("datetime" %in% names(res))
  expect_true("Level (m)" %in% names(res))
  expect_true("Temperature (\u00B0C)" %in% names(res))
  expect_s3_class(res$datetime, "POSIXct")
  expect_identical(attr(res$datetime, "tzone"), "UTC")
  expect_false(any(is.na(res$datetime)))
  expect_match(
    attr(res, "logger_timezone_note"),
    "no offset shift was applied",
    fixed = TRUE
  )
})

test_that("logger file reader accepts default_tz names and fixed offsets", {
  file <- test_path("fixtures/logger_reads/1071079_YOWN-0000_TEST_2020_05_07.xle")
  utc <- read_logger_file_data(file, default_tz = "UTC")
  numeric_offset <- read_logger_file_data(file, default_tz = -7)
  string_offset <- read_logger_file_data(file, default_tz = "UTC-07:00")
  olson <- read_logger_file_data(file, default_tz = "America/Whitehorse")

  expect_equal(
    as.numeric(difftime(numeric_offset$datetime[1], utc$datetime[1], units = "hours")),
    7
  )
  expect_identical(numeric_offset$datetime, string_offset$datetime)
  expect_s3_class(olson$datetime, "POSIXct")
  expect_false(any(is.na(olson$datetime)))
})

test_that("logger file reader converts VuSitu html to uploadable data", {
  html_file <- tempfile(fileext = ".html")
  writeLines(
    c(
      '<html><body><table>',
      '<tr class="sectionMember"><td isi-group-member="ReportProperties" isi-property="TimeOffset"><span isi-label="">Time Offset</span> = <span isi-value="">-07:00:00</span></td></tr>',
      '<tr class="dataHeader" isi-data-table="">',
      '<td isi-data-column-header="DateTime">Date Time</td>',
      '<td isi-data-column-header="Parameter" isi-unit-type="19">Pressure (kPa) (123)</td>',
      '<td isi-data-column-header="Parameter" isi-unit-type="1">Depth (m) (123)</td>',
      '<td isi-data-column-header="Parameter" isi-unit-type="1">Temperature (\u00B0C) (123)</td>',
      '</tr>',
      '<tr class="data" isi-data-row=""><td class="dateTime">2025-08-25 18:00:00.057</td><td>100.985</td><td>1.4584961</td><td>10.307915</td></tr>',
      '</table></body></html>'
    ),
    html_file,
    useBytes = TRUE
  )

  res <- read_logger_file_data(html_file, file_type = "html")

  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 1L)
  expect_true(all(c(
    "datetime",
    "Pressure (m)",
    "Depth (m)",
    "Temperature (\u00B0C)"
  ) %in% names(res)))
  expect_identical(
    format(res$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2025-08-26 01:00:00"
  )
  expect_equal(res$`Pressure (m)`, 100.985 * 0.1019716213 * 1.001)
  expect_match(
    attr(res, "logger_timezone_note"),
    "UTC-07:00",
    fixed = TRUE
  )
})
