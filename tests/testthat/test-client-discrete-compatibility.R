# Synthetic result rows shared by the report and plot regression checks. No
# live connection, credentials, or instance-specific catalogue IDs are needed.
client_discrete_fixture <- function() {
  day <- as.Date("2026-10-01")
  time <- as.POSIXct("2026-10-01 12:00:00", tz = "UTC")
  data.frame(
    result_id = c(101L, 102L),
    sample_id = 10L,
    location_id = 1L,
    location = "TEST",
    alias = "Test",
    location_name = "Test location",
    location_name_fr = "Lieu test",
    latitude = 60,
    longitude = -135,
    sub_location_id = NA_integer_,
    sub_location_name = NA_character_,
    sub_location_name_fr = NA_character_,
    requested_date = day,
    date_approx = 0L,
    sample_date = day,
    datetime = time,
    target_datetime = time,
    media_id = 1L,
    media_type = "water",
    media_type_fr = "eau",
    sample_type_id = 1L,
    sample_type = "regular",
    collection_method_id = 1L,
    collection_method = "grab",
    parameter_id = 10L,
    param_name = "aluminum",
    param_name_fr = "aluminium",
    matrix_state_id = 2L,
    matrix_state = "Solid",
    sample_fraction_id = 1L,
    sample_fraction = "total",
    reporting_basis_id = NA_integer_,
    reporting_basis = NA_character_,
    result_speciation_id = NA_integer_,
    result_speciation = NA_character_,
    result_target_id = NA_integer_,
    result_target = NA_character_,
    endpoint_mode = NA_character_,
    endpoint_ph = NA_real_,
    analysis_conditions = "{}",
    analysis_conditions_key = "empty",
    result_type_id = 1L,
    result_type = "lab",
    result_value_type_id = 1:2,
    result_value_type = c("Actual", "Calculated"),
    result = c(10, 100),
    result_condition = NA_integer_,
    result_condition_label = NA_character_,
    result_condition_value = NA_real_,
    units = "mg/kg",
    result_grade_id = NA_integer_,
    result_grade_code = NA_character_,
    result_grade = NA_character_,
    result_approval_id = NA_integer_,
    result_approval_code = NA_character_,
    result_approval = NA_character_,
    lab_report_no = "TEST-REPORT",
    lab_sample_no = "TEST-SAMPLE"
  )
}

test_that("metadata downloads use bilingual identity columns and parameter scope", {
  module <- new.env(parent = asNamespace("YGwater"))
  sys.source(
    system.file(
      "apps/aqualink/modules/client/plot/discretePlot.R",
      package = "YGwater"
    ),
    module
  )
  rows <- data.frame(result_id = 1:2, sample_id = 10L, parameter_id = 10:11)
  queries <- character()
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      queries <<- c(queries, statement)
      expect_match(
        statement,
        "ORDER BY datetime, sample_id, parameter_id, result_id",
        fixed = TRUE
      )
      expect_false(grepl(
        "parameter_name|sample_source_adapter_function|sample_external_sample_id",
        statement
      ))
      if (grepl("AND FALSE", statement, fixed = TRUE)) {
        return(rows[0, ])
      }
      if (grepl("AND parameter_id IN (10)", statement, fixed = TRUE)) {
        return(rows[1, ])
      }
      rows
    },
    .package = "DBI"
  )

  for (lang in c("en", "fr")) {
    expect_equal(nrow(module$disc_result_metadata(NULL, 10L, lang)), 2L)
    selected <- module$disc_result_metadata(
      NULL,
      10L,
      lang,
      parameter_ids = 10L
    )
    expect_identical(selected$parameter_id, 10L)
    expect_equal(
      nrow(module$disc_result_metadata(NULL, 10L, lang, integer())),
      0L
    )
    expect_equal(
      nrow(module$disc_result_metadata(NULL, 10L, lang, c(NA, "invalid"))),
      0L
    )
  }
  expect_true(any(grepl("results_metadata_fr", queries, fixed = TRUE)))
  expect_equal(nrow(module$disc_result_metadata(NULL, integer(), "fr")), 0L)

  # Both the preview and download must pass the UI's parameter selection.
  code <- paste(
    readLines(
      system.file(
        "apps/aqualink/modules/client/data/discreteData.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    code,
    'sample_ids_str,\n        ") AND r.parameter_id IN (",',
    fixed = TRUE
  )
  expect_match(
    code,
    "parameter_ids = filteredData$params$parameter_id",
    fixed = TRUE
  )
})

test_that("report matrices and SD calculations distinguish result value types", {
  rows <- client_discrete_fixture()
  history <- rows[rep(1:2, each = 2L), ]
  history$result <- c(0, 2, 90, 110)
  local_mocked_bindings(
    ac_parameter_unit_select_sql = function(...) "resolved_unit AS units",
    .package = "YGwater"
  )
  local_mocked_bindings(
    dbIsValid = function(dbObj, ...) TRUE,
    dbGetQuery = function(conn, statement, ...) {
      if (
        grepl("SELECT l.location_id, l.location_code", statement, fixed = TRUE)
      ) {
        return(data.frame(location_id = 1L, location_code = "TEST"))
      }
      if (
        grepl(
          "SELECT parameter_id, param_name, param_name_fr",
          statement,
          fixed = TRUE
        )
      ) {
        return(unique(rows[c("parameter_id", "param_name", "param_name_fr")]))
      }
      if (
        grepl(
          "SELECT r.result_id, s.sample_id, s.location_id",
          statement,
          fixed = TRUE
        )
      ) {
        return(rows)
      }
      if (
        grepl("SELECT s.location_id, r.parameter_id", statement, fixed = TRUE)
      ) {
        expect_match(
          statement,
          "r.result_value_type AS result_value_type_id",
          fixed = TRUE
        )
        return(history)
      }
      stop("Unexpected report query: ", statement)
    },
    .package = "DBI"
  )
  for (format in c("by_date", "by_location", "by_parameter")) {
    for (range in c(FALSE, TRUE)) {
      path <- tempfile(fileext = ".xlsx")
      on.exit(unlink(path), add = TRUE)
      AquaCacheReport(
        date = rows$sample_date[[1]],
        date_range = if (range) rep(rows$sample_date[[1]], 2L) else NULL,
        location_ids = 1L,
        parameter_ids = 10L,
        output_path = path,
        con = structure(list(), class = "test_connection"),
        format = format,
        sd_multiplier = 2
      )
      matrix <- openxlsx::read.xlsx(
        path,
        "Report",
        startRow = 5,
        sep.names = " "
      )
      details <- openxlsx::read.xlsx(path, "Result details", sep.names = " ")
      expect_equal(nrow(matrix), 2L, info = paste(format, range))
      expect_identical(matrix$`Result value type`, c("Actual", "Calculated"))
      expect_equal(as.numeric(matrix[[ncol(matrix)]]), c(10, 100))
      expect_equal(details$`SD sample count`, c(2, 2))
      expect_equal(details$`SD mean`, c(1, 100))
      expect_equal(details$`SD`, c(sqrt(2), sqrt(200)))
      expect_identical(details$`SD exceedance`, c(TRUE, FALSE))
      # The first result column must follow the added value-type context column.
      value_col <- match("Result value type", names(matrix))
      expect_equal(value_col, if (format == "by_parameter") 8L else 7L)
      expect_equal(ncol(matrix), value_col + 1L)
    }
  }
})

test_that("plots resolve matrix units and show a guideline for one result", {
  rows <- client_discrete_fixture()[1, ]
  rows$name <- rows$location_name
  rows$name_fr <- rows$location_name_fr
  rows$replicate_group_id <- NA_integer_
  rows$replicate_group_count <- 0L
  for (field in c(
    "grade_type_description",
    "approval_type_description",
    "qualifier_type_description",
    "result_grade_description",
    "result_approval_description",
    "grade_type_description_fr",
    "approval_type_description_fr",
    "qualifier_type_description_fr",
    "result_grade_description_fr",
    "result_approval_description_fr"
  )) {
    rows[[field]] <- NA_character_
  }
  for (field in c(
    "aggregation_type",
    "calculation_version",
    "calculation_arguments",
    "expected_count"
  )) {
    rows[[field]] <- NA
  }
  local_mocked_bindings(
    ac_parameter_unit_select_sql = function(
      con,
      parameter_alias,
      output_alias,
      ...,
      matrix_state_alias = NULL
    ) {
      if (is.null(matrix_state_alias)) {
        return("fallback_units AS units")
      }
      expect_identical(matrix_state_alias, "r")
      "public.get_parameter_unit_name(p.parameter_id, r.matrix_state_id) AS units"
    },
    .package = "YGwater"
  )
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      if (grepl("SELECT DISTINCT l.location_id", statement, fixed = TRUE)) {
        return(data.frame(
          location_id = 1L,
          location = "TEST",
          alias = "Test",
          name = "Test location",
          name_fr = "Lieu test"
        ))
      }
      if (grepl("SELECT DISTINCT p.parameter_id", statement, fixed = TRUE)) {
        return(unique(rows[c(
          "parameter_id",
          "param_name",
          "param_name_fr",
          "units"
        )]))
      }
      if (grepl("r.result_type AS result_type_id", statement, fixed = TRUE)) {
        expect_match(
          statement,
          "public.get_parameter_unit_name(p.parameter_id, r.matrix_state_id) AS units",
          fixed = TRUE
        )
        return(rows)
      }
      if (grepl("to_regprocedure", statement, fixed = TRUE)) {
        return(data.frame(exists = TRUE))
      }
      if (
        grepl(
          "criteria.applicable_guidelines_for_result",
          statement,
          fixed = TRUE
        )
      ) {
        return(data.frame(
          result_id = rows$result_id,
          guideline_id = 1L,
          guideline_code = "TEST-LIMIT",
          guideline_name = "Test limit",
          guideline_parameter_id = 10L,
          guideline_units = "mg/kg",
          std_min = if (bound == "upper") NA_real_ else 5,
          std_max = if (bound == "lower") NA_real_ else 15
        ))
      }
      stop("Unexpected plot query: ", statement)
    },
    .package = "DBI"
  )
  for (lang in c("en", "fr")) {
    for (bound in c("upper", "lower", "range")) {
      result <- plotDiscrete(
        start = "2026-10-01",
        end = "2026-10-02",
        sample_ids = 10L,
        guidelines = 1L,
        dbSource = "AC",
        dbCon = "test_connection",
        target_datetime = FALSE,
        data = TRUE,
        lang = lang
      )
      expect_identical(result$data$units, "mg/kg")
      expected_limits <- switch(bound, upper = 15, lower = 5, range = c(5, 15))
      traces <- plotly::plotly_build(result$plot)$x$data
      limits <- Filter(
        function(trace) identical(trace$marker$symbol, "line-ew"),
        traces
      )
      expect_length(limits, length(expected_limits))
      expect_equal(
        sort(unlist(lapply(limits, function(trace) as.numeric(trace$y)))),
        expected_limits
      )
    }
  }
})
