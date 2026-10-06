if (!methods::isClass("AquaCacheReportMockConnection")) {
  methods::setClass(
    "AquaCacheReportMockConnection",
    contains = "DBIConnection",
    slots = c(fixture = "list")
  )
}

methods::setMethod(
  DBI::dbIsValid,
  signature(dbObj = "AquaCacheReportMockConnection"),
  function(dbObj, ...) TRUE
)


methods::setMethod(
  DBI::dbGetQuery,
  signature(
    conn = "AquaCacheReportMockConnection",
    statement = "character"
  ),
  function(conn, statement, params = NULL, ...) {
    fixture <- conn@fixture
    if (grepl("SELECT l.location_id", statement, fixed = TRUE)) {
      ids <- as.integer(jsonlite::fromJSON(params[[1]]))
      return(fixture$locations[
        fixture$locations$location_id %in% ids,
        ,
        drop = FALSE
      ])
    }
    if (grepl("SELECT parameter_id, param_name", statement, fixed = TRUE)) {
      ids <- as.integer(jsonlite::fromJSON(params[[1]]))
      return(fixture$parameters[
        fixture$parameters$parameter_id %in% ids,
        ,
        drop = FALSE
      ])
    }
    if (grepl("to_regprocedure", statement, fixed = TRUE)) {
      return(data.frame(available = TRUE))
    }
    if (grepl("FROM criteria.guidelines g", statement, fixed = TRUE)) {
      return(data.frame(
        guideline_id = 30L,
        guideline_code = "TST",
        guideline_name = "Test limit",
        stringsAsFactors = FALSE
      ))
    }
    if (grepl("WITH requested AS", statement, fixed = TRUE)) {
      requests <- jsonlite::fromJSON(params[[1]])
      ids <- unique(as.integer(requests$result_id))
      rows <- fixture$results[
        match(ids, fixture$results$result_id),
        ,
        drop = FALSE
      ]
      return(data.frame(
        result_id = rows$result_id,
        guideline_id = 30L,
        comparison_status = ifelse(
          rows$result > ifelse(rows$location_id == 1L, 1.2, 2.2),
          "exceeds",
          "meets"
        ),
        output_status = "value",
        guideline_value = ifelse(rows$location_id == 1L, 1.2, 2.2),
        bound_code = "upper",
        comparison_operator_code = "lte",
        stringsAsFactors = FALSE
      ))
    }
    if (grepl("WITH date_requests AS", statement, fixed = TRUE)) {
      fixture$state$sql <- statement
      requests <- jsonlite::fromJSON(params[[3]])
      fixture$state$requests <- requests
      locations <- as.integer(jsonlite::fromJSON(params[[1]]))
      parameters <- as.integer(jsonlite::fromJSON(params[[2]]))
      selected <- list()
      output_index <- 0L
      for (i in seq_len(nrow(requests))) {
        target_date <- as.Date(requests$requested_date[[i]])
        tolerance <- requests$date_approx[[i]]
        for (location_id in locations) {
          candidates <- fixture$samples[
            fixture$samples$location_id == location_id &
              abs(as.integer(fixture$samples$sample_date - target_date)) <=
                tolerance,
            ,
            drop = FALSE
          ]
          if (!nrow(candidates)) {
            next
          }
          candidates$distance <- abs(as.integer(
            candidates$sample_date - target_date
          ))
          candidates <- candidates[
            order(
              candidates$distance,
              candidates$sample_date < target_date,
              candidates$sample_date
            ),
            ,
            drop = FALSE
          ]
          chosen_date <- candidates$sample_date[[1]]
          chosen_samples <- fixture$samples[
            fixture$samples$location_id == location_id &
              fixture$samples$sample_date == chosen_date,
            ,
            drop = FALSE
          ]
          for (sample_id in chosen_samples$sample_id) {
            rows <- fixture$results[
              fixture$results$sample_id == sample_id &
                fixture$results$parameter_id %in% parameters,
              ,
              drop = FALSE
            ]
            if (!nrow(rows)) {
              next
            }
            rows$requested_date <- target_date
            rows$date_approx <- tolerance
            output_index <- output_index + 1L
            selected[[output_index]] <- rows
          }
        }
      }
      if (!length(selected)) {
        return(fixture$results[FALSE, , drop = FALSE])
      }
      return(do.call(rbind, selected))
    }
    stop("Unexpected AquaCacheReport mock query: ", statement, call. = FALSE)
  }
)

make_report_fixture <- function() {
  locations <- data.frame(
    location_id = c(1L, 2L, 3L),
    location_code = c("LOC-A", "LOC-B", "LOC-C"),
    stringsAsFactors = FALSE
  )
  parameters <- data.frame(
    parameter_id = c(10L, 20L),
    param_name = c("Nitrate", "pH"),
    param_name_fr = c("Nitrate fr", "pH fr"),
    stringsAsFactors = FALSE
  )
  samples <- data.frame(
    sample_id = c(101L, 102L, 201L, 202L, 301L),
    location_id = c(1L, 1L, 2L, 2L, 3L),
    sample_date = as.Date(c(
      "2024-01-01",
      "2024-01-03",
      "2024-01-01",
      "2024-01-03",
      "2024-01-01"
    )),
    stringsAsFactors = FALSE
  )
  samples$location <- locations$location_code[match(
    samples$location_id,
    locations$location_id
  )]
  samples$location_name <- paste("Station", samples$location)
  samples$datetime <- as.POSIXct(
    paste(samples$sample_date, "12:00:00"),
    tz = "UTC"
  )

  rows <- list()
  row_index <- 0L
  for (i in seq_len(nrow(samples))) {
    for (parameter_id in parameters$parameter_id) {
      row_index <- row_index + 1L
      value <- switch(
        as.character(parameter_id),
        `10` = samples$location_id[[i]] +
          as.integer(format(samples$sample_date[[i]], "%d")) / 10,
        `20` = 10 *
          samples$location_id[[i]] +
          as.integer(format(samples$sample_date[[i]], "%d")),
        NA_real_
      )
      rows[[row_index]] <- data.frame(
        result_id = 1000L + row_index,
        sample_id = samples$sample_id[[i]],
        location_id = samples$location_id[[i]],
        location = samples$location[[i]],
        alias = NA_character_,
        location_name = samples$location_name[[i]],
        location_name_fr = NA_character_,
        latitude = 60 + samples$location_id[[i]],
        longitude = -130 - samples$location_id[[i]],
        sub_location_id = NA_integer_,
        sub_location_name = NA_character_,
        sub_location_name_fr = NA_character_,
        sample_date = samples$sample_date[[i]],
        datetime = samples$datetime[[i]],
        target_datetime = as.POSIXct(NA, tz = "UTC"),
        media_id = 1L,
        media_type = "Water",
        media_type_fr = "Eau",
        sample_type_id = 1L,
        sample_type = "Routine",
        collection_method_id = 1L,
        collection_method = "Grab",
        parameter_id = parameter_id,
        param_name = parameters$param_name[match(
          parameter_id,
          parameters$parameter_id
        )],
        param_name_fr = NA_character_,
        matrix_state_id = 1L,
        matrix_state = "Liquid",
        sample_fraction_id = 1L,
        sample_fraction = "Total",
        result_speciation_id = NA_integer_,
        result_speciation = NA_character_,
        result_type_id = 1L,
        result_type = "Discrete",
        result_value_type_id = 1L,
        result_value_type = "Numeric",
        result = as.numeric(value),
        result_condition = NA_integer_,
        result_condition_label = "",
        result_condition_value = NA_real_,
        units = if (parameter_id == 10L) "mg/L" else "pH units",
        result_grade_id = NA_integer_,
        result_grade_code = NA_character_,
        result_grade = NA_character_,
        result_approval_id = NA_integer_,
        result_approval_code = NA_character_,
        result_approval = NA_character_,
        lab_report_no = NA_character_,
        lab_sample_no = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  methods::new(
    "AquaCacheReportMockConnection",
    fixture = list(
      locations = locations,
      parameters = parameters,
      samples = samples,
      results = do.call(rbind, rows),
      state = new.env(parent = emptyenv())
    )
  )
}

run_mock_aquacache_report <- function(con, ..., format = "by_date") {
  testthat::with_mocked_bindings(
    AquaCacheReport(
      ...,
      output_path = tempfile(fileext = ".xlsx"),
      con = con,
      format = format
    ),
    ac_parameter_unit_select_sql = function(...) "NULL::text AS units",
    .package = "YGwater"
  )
}

test_that("AquaCacheReport preserves the original single-date matrix and writes date tabs", {
  con <- make_report_fixture()
  one_date <- as.Date("2024-01-01")
  one_result <- run_mock_aquacache_report(
    con,
    date = one_date,
    location_ids = c(1L, 2L),
    parameter_ids = c(10L, 20L)
  )
  on.exit(unlink(one_result$xlsx_path), add = TRUE)
  expect_equal(
    openxlsx::getSheetNames(one_result$xlsx_path),
    c("Report", "Result details", "Guideline details")
  )
  one_sheet <- openxlsx::read.xlsx(
    one_result$xlsx_path,
    "Report",
    colNames = FALSE
  )
  expect_equal(as.character(one_sheet[5, 1]), "Parameter")
  expect_match(as.character(one_sheet[5, 7]), "LOC-A")
  expect_equal(as.character(one_sheet[6, 7]), "1.1")

  dates <- as.Date(c("2024-01-01", "2024-01-03"))
  date_tabs <- run_mock_aquacache_report(
    con,
    date = format(dates, "%Y-%m-%d"),
    date_approx = 0L,
    location_ids = c(1L, 2L),
    parameter_ids = c(10L, 20L)
  )
  on.exit(unlink(date_tabs$xlsx_path), add = TRUE)
  expect_equal(
    openxlsx::getSheetNames(date_tabs$xlsx_path),
    c("2024-01-01", "2024-01-03", "Result details", "Guideline details")
  )
  expect_equal(nrow(con@fixture$state$requests), 2L)
  expect_match(con@fixture$state$sql, "DISTINCT ON")

  date_with_no_data <- run_mock_aquacache_report(
    con,
    date = as.Date(c("2024-01-01", "2024-01-02")),
    location_ids = 1L,
    parameter_ids = 10L
  )
  on.exit(unlink(date_with_no_data$xlsx_path), add = TRUE)
  expect_equal(
    openxlsx::getSheetNames(date_with_no_data$xlsx_path)[1:2],
    c("2024-01-01", "2024-01-02")
  )
  empty_date_sheet <- openxlsx::read.xlsx(
    date_with_no_data$xlsx_path,
    "2024-01-02",
    colNames = FALSE
  )
  expect_equal(as.character(empty_date_sheet[5, 7]), "No eligible samples")
})

test_that("AquaCacheReport writes location and parameter date matrices", {
  con <- make_report_fixture()
  dates <- as.Date(c("2024-01-01", "2024-01-03"))
  by_location <- run_mock_aquacache_report(
    con,
    date = dates,
    location_ids = c(1L, 2L),
    parameter_ids = c(10L, 20L),
    format = "by_location"
  )
  on.exit(unlink(by_location$xlsx_path), add = TRUE)
  expect_equal(
    openxlsx::getSheetNames(by_location$xlsx_path),
    c("LOC-A", "LOC-B", "Result details", "Guideline details")
  )
  location_sheet <- openxlsx::read.xlsx(
    by_location$xlsx_path,
    "LOC-A",
    colNames = FALSE
  )
  expect_equal(as.character(location_sheet[5, 1]), "Parameter")
  expect_equal(
    as.character(location_sheet[5, 7:8]),
    c("2024-01-01", "2024-01-03")
  )
  expect_equal(as.character(location_sheet[6, 7:8]), c("1.1", "1.3"))

  by_parameter <- run_mock_aquacache_report(
    con,
    date = dates,
    location_ids = c(1L, 2L),
    parameter_ids = c(10L, 20L),
    date_approx = c(0L, 0L),
    format = "by_parameter"
  )
  on.exit(unlink(by_parameter$xlsx_path), add = TRUE)
  expect_equal(
    openxlsx::getSheetNames(by_parameter$xlsx_path),
    c("Nitrate", "pH", "Result details", "Guideline details")
  )
  parameter_sheet <- openxlsx::read.xlsx(
    by_parameter$xlsx_path,
    "Nitrate",
    colNames = FALSE
  )
  expect_equal(as.character(parameter_sheet[5, 1]), "Location")
  expect_equal(
    as.character(parameter_sheet[5, 8:9]),
    c("2024-01-01", "2024-01-03")
  )
  expect_equal(as.character(parameter_sheet[6, 8:9]), c("1.1", "1.3"))

  one_location <- run_mock_aquacache_report(
    con,
    date = dates,
    location_ids = 1L,
    parameter_ids = c(10L, 20L),
    format = "by_location"
  )
  on.exit(unlink(one_location$xlsx_path), add = TRUE)
  expect_equal(
    openxlsx::getSheetNames(one_location$xlsx_path),
    c("Report", "Result details", "Guideline details")
  )
  one_parameter <- run_mock_aquacache_report(
    con,
    date = dates,
    location_ids = c(1L, 2L),
    parameter_ids = 10L,
    format = "by_parameter"
  )
  on.exit(unlink(one_parameter$xlsx_path), add = TRUE)
  expect_equal(
    openxlsx::getSheetNames(one_parameter$xlsx_path),
    c("Report", "Result details", "Guideline details")
  )
  one_parameter_sheet <- openxlsx::read.xlsx(
    one_parameter$xlsx_path,
    "Report",
    colNames = FALSE
  )
  expect_equal(as.character(one_parameter_sheet[6:7, 1]), c("LOC-A", "LOC-B"))
})

test_that("AquaCacheReport warns when approximation reuses a sample across dates", {
  con <- make_report_fixture()
  report_box <- new.env(parent = emptyenv())
  testthat::expect_warning(
    report_box$report <- run_mock_aquacache_report(
      con,
      date = as.Date(c("2024-01-01", "2024-01-02")),
      date_approx = c(0L, 1L),
      location_ids = 3L,
      parameter_ids = 10L
    ),
    "same sample for multiple requested dates"
  )
  report <- report_box$report
  on.exit(unlink(report$xlsx_path), add = TRUE)
  details <- openxlsx::read.xlsx(report$xlsx_path, "Result details")
  reused <- details[details[[2]] == 301L, ]
  expect_equal(length(unique(reused[[7]])), 2L)
  expect_equal(report$result_count, 2L)
})

test_that("AquaCacheReport retains per-result guideline limits in date-column layouts", {
  con <- make_report_fixture()
  report <- run_mock_aquacache_report(
    con,
    date = as.Date(c("2024-01-01", "2024-01-03")),
    location_ids = 1L,
    parameter_ids = 10L,
    guideline_ids = 30L,
    format = "by_location"
  )
  on.exit(unlink(report$xlsx_path), add = TRUE)
  sheet <- openxlsx::read.xlsx(report$xlsx_path, "Report", colNames = FALSE)
  expect_match(as.character(sheet[5, 2]), "TST - Test limit")
  expect_equal(as.character(sheet[6, 2]), "<= 1.2")
  expect_equal(as.character(sheet[6, 8:9]), c("1.1", "1.3"))

  by_parameter <- run_mock_aquacache_report(
    con,
    date = as.Date(c("2024-01-01", "2024-01-03")),
    location_ids = c(1L, 2L),
    parameter_ids = 10L,
    guideline_ids = 30L,
    format = "by_parameter"
  )
  on.exit(unlink(by_parameter$xlsx_path), add = TRUE)
  parameter_sheet <- openxlsx::read.xlsx(
    by_parameter$xlsx_path,
    "Report",
    colNames = FALSE
  )
  expect_equal(as.character(parameter_sheet[6:7, 8]), c("<= 1.2", "<= 2.2"))
})

test_that("AquaCacheReport validates date and approximation vectors before connecting", {
  expect_error(
    AquaCacheReport(
      date = as.Date(c("2024-01-01", "2024-01-02")),
      date_approx = c(0L, 1L, 2L),
      location_ids = 1L,
      parameter_ids = 10L,
      output_path = tempfile(fileext = ".xlsx")
    ),
    "one per date"
  )
  expect_error(
    AquaCacheReport(
      date = as.Date(c("2024-01-01", "2024-01-01")),
      location_ids = 1L,
      parameter_ids = 10L,
      output_path = tempfile(fileext = ".xlsx")
    ),
    "duplicate dates"
  )
})

test_that("AquaCacheReport supports every format and selection cardinality", {
  formats <- c("by_date", "by_location", "by_parameter")
  dates <- as.Date(c("2024-01-01", "2024-01-03"))
  locations <- c(1L, 2L)
  parameters <- c(10L, 20L)
  output_paths <- character()
  on.exit(unlink(output_paths), add = TRUE)

  for (report_format in formats) {
    for (location_count in 1:2) {
      for (parameter_count in 1:2) {
        for (date_count in 1:2) {
          selected_dates <- dates[seq_len(date_count)]
          selected_locations <- locations[seq_len(location_count)]
          selected_parameters <- parameters[seq_len(parameter_count)]
          tolerance <- if (date_count == 1L || parameter_count == 1L) {
            0L
          } else {
            rep(0L, date_count)
          }
          report <- run_mock_aquacache_report(
            make_report_fixture(),
            date = selected_dates,
            date_approx = tolerance,
            location_ids = selected_locations,
            parameter_ids = selected_parameters,
            format = report_format
          )
          output_paths <- c(output_paths, report$xlsx_path)

          tab_count <- switch(
            report_format,
            by_date = date_count,
            by_location = location_count,
            by_parameter = parameter_count
          )
          report_names <- openxlsx::getSheetNames(report$xlsx_path)[seq_len(
            tab_count
          )]
          expected_names <- switch(
            report_format,
            by_date = if (date_count == 1L) {
              "Report"
            } else {
              format(selected_dates, "%Y-%m-%d")
            },
            by_location = if (location_count == 1L) {
              "Report"
            } else {
              c("LOC-A", "LOC-B")[seq_len(location_count)]
            },
            by_parameter = if (parameter_count == 1L) {
              "Report"
            } else {
              c("Nitrate", "pH")[seq_len(parameter_count)]
            }
          )
          expect_equal(report_names, expected_names)
          expect_equal(
            report$result_count,
            location_count * parameter_count * date_count
          )

          sheet <- openxlsx::read.xlsx(
            report$xlsx_path,
            report_names[[1]],
            colNames = FALSE
          )
          if (report_format == "by_date") {
            expect_equal(as.character(sheet[5, 1]), "Parameter")
            expect_equal(
              sum(!is.na(sheet[5, 7:ncol(sheet)])),
              location_count
            )
          } else if (report_format == "by_location") {
            expect_equal(as.character(sheet[5, 1]), "Parameter")
            expect_equal(
              as.character(sheet[5, 7:(6 + date_count)]),
              format(selected_dates, "%Y-%m-%d")
            )
          } else {
            expect_equal(as.character(sheet[5, 1]), "Location")
            expect_equal(
              as.character(sheet[5, 8:(7 + date_count)]),
              format(selected_dates, "%Y-%m-%d")
            )
          }
        }
      }
    }
  }
})

test_that("AquaCacheReport supports vector tolerances in all three layouts", {
  formats <- c("by_date", "by_location", "by_parameter")
  output_paths <- character()
  on.exit(unlink(output_paths), add = TRUE)

  for (report_format in formats) {
    con <- make_report_fixture()
    report_box <- new.env(parent = emptyenv())
    expect_warning(
      report_box$report <- run_mock_aquacache_report(
        con,
        date = as.Date(c("2024-01-01", "2024-01-02")),
        date_approx = c(0L, 1L),
        location_ids = 3L,
        parameter_ids = 10L,
        format = report_format
      ),
      "same sample for multiple requested dates"
    )
    report <- report_box$report
    output_paths <- c(output_paths, report$xlsx_path)
    expect_equal(report$reused_sample_count, 1L)
    expect_equal(report$result_count, 2L)

    details <- openxlsx::read.xlsx(report$xlsx_path, "Result details")
    expect_equal(
      as.Date(as.numeric(details[[7]]), origin = "1899-12-30"),
      as.Date(c("2024-01-01", "2024-01-02"))
    )
    expect_equal(details[[8]], c(0, 1))
    expect_equal(
      as.Date(as.numeric(details[[9]]), origin = "1899-12-30"),
      as.Date(c("2024-01-01", "2024-01-01"))
    )

    if (report_format == "by_date") {
      expect_equal(
        openxlsx::getSheetNames(report$xlsx_path)[1:2],
        c("2024-01-01", "2024-01-02")
      )
    } else {
      sheet <- openxlsx::read.xlsx(report$xlsx_path, "Report", colNames = FALSE)
      date_columns <- if (report_format == "by_location") 7:8 else 8:9
      expect_equal(
        as.character(sheet[5, date_columns]),
        c("2024-01-01", "2024-01-02")
      )
      value_row <- if (report_format == "by_location") 6L else 6L
      expect_equal(
        as.character(sheet[value_row, date_columns]),
        c("3.1", "3.1")
      )
    }
  }
})

test_that("AquaCacheReport makes case-insensitive unique worksheet names", {
  con <- make_report_fixture()
  con@fixture$locations$location_code[2] <- "loc-a"
  report <- run_mock_aquacache_report(
    con,
    date = as.Date("2024-01-01"),
    location_ids = c(1L, 2L),
    parameter_ids = 10L,
    format = "by_location"
  )
  on.exit(unlink(report$xlsx_path), add = TRUE)
  expect_equal(
    openxlsx::getSheetNames(report$xlsx_path)[1:2],
    c("LOC-A", "loc-a (2)")
  )
})

test_that("AquaCacheReport keeps empty target dates and resolves ties consistently", {
  dates <- as.Date(c("2024-01-01", "2024-01-02"))
  output_paths <- character()
  on.exit(unlink(output_paths), add = TRUE)
  for (report_format in c("by_location", "by_parameter")) {
    report <- run_mock_aquacache_report(
      make_report_fixture(),
      date = dates,
      date_approx = c(0L, 0L),
      location_ids = 1L,
      parameter_ids = 10L,
      format = report_format
    )
    output_paths <- c(output_paths, report$xlsx_path)
    sheet <- openxlsx::read.xlsx(report$xlsx_path, "Report", colNames = FALSE)
    date_columns <- if (report_format == "by_location") 7:8 else 8:9
    expect_equal(
      as.character(sheet[5, date_columns]),
      format(dates, "%Y-%m-%d")
    )
    expect_true(
      is.na(sheet[6, date_columns[[2]]]) ||
        !nzchar(as.character(sheet[6, date_columns[[2]]]))
    )
  }

  tie_report <- run_mock_aquacache_report(
    make_report_fixture(),
    date = as.Date("2024-01-02"),
    date_approx = 1L,
    location_ids = 1L,
    parameter_ids = 10L
  )
  output_paths <- c(output_paths, tie_report$xlsx_path)
  details <- openxlsx::read.xlsx(tie_report$xlsx_path, "Result details")
  expect_equal(details[[2]], 102L)
})
