import_setup_environment <- function() {
  env <- new.env(parent=asNamespace("YGwater"))
  for (file in c("addDiscData.R","importSetup.R")) sys.source(system.file(
    "apps/aqualink/modules/admin/discreteData",file,package="YGwater"),env)
  env
}

test_that("source criteria preserve package field names, case and blank units", {
  env <- import_setup_environment()
  eqwin <- env$importSetup_match_values('{"ParamCode":"Al-D","input_unit":"mg/L"}')
  expect_identical(eqwin,list(ParamCode="Al-D",input_unit="mg/L"))
  expect_identical(env$importSetup_match_values('{"input_param":"ALKALINITY","input_unit":""}'),
    list(input_param="ALKALINITY",input_unit=""))
  expect_identical(env$importSetup_match_values('{"parameter_code":"Al-D","unit":"mg/L"}'),
    list(parameter_code="Al-D",unit="mg/L"))
  expect_identical(names(env$importSetup_match_values('{"Source parameter":"value"}')),"Source parameter")
})

test_that("malformed and empty criteria cannot become catch-all mappings", {
  env <- import_setup_environment()
  for (text in c("{", "{}", "[]", '{"code":null}', '{"code":["A","B"]}',
    '{"code":12}', '{"code":"A","code":"B"}', '{"code":" ","unit":""}'))
    expect_error(env$importSetup_match_values(text),"criterion|criteria|JSON")
  expect_error(env$importSetup_match_values('{"parameter_id":"12"}'),"reserved")
})

test_that("profile text result mappings validate labels and numbers", {
  env <- import_setup_environment()
  expect_identical(
    env$importSetup_parse_result_text_values("Absent = 0\nPresent = 1"),
    list(Absent = 0, Present = 1)
  )
  expect_identical(env$importSetup_parse_result_text_values(" \n"), list())
  expect_error(
    env$importSetup_parse_result_text_values("Present = 1\npresent = 0"),
    "unique labels"
  )
  expect_error(
    env$importSetup_parse_result_text_values("Present = yes"),
    "unique labels and finite numeric values"
  )
})


test_that("an unspecified laboratory stays unassigned in workbook defaults", {
  env <- import_setup_environment()
  profile <- data.frame(profile_code="no_lab")
  profile$defaults <- I(list(list(owner=1L,media_id=1L)))
  expect_true(is.na(env$addDiscData_defaults(profile)$laboratory))
  profile$defaults[[1]]$laboratory <- 7L
  expect_identical(env$addDiscData_defaults(profile)$laboratory,7L)
})


test_that("review context does not label a complete mapping as needing attention", {
  env <- import_setup_environment()
  statements <- as.list(body(env$importSetup)[[2]][[3]][[3]])[-1]
  assignment <- Filter(function(x) is.call(x) && identical(x[[1]],quote(`<-`)) &&
    identical(x[[2]],quote(output$editing_mapping)),statements)[[1]]
  render <- function() NULL
  body(render) <- assignment[[3]][[2]]
  state <- new.env(parent=env)
  state$tags <- htmltools::tags
  state$input <- list(manage_mapping_type="parameter")
  state$row <- data.frame(import_mapping_id=4L, source_code="ALS", profile_code="xlr", active=TRUE)
  state$context <- data.frame(source_code="ALS",profile_code="xlr",profile_name="Detailed Report",
    mapping_type="parameter",issue=NA_character_,status="published",version=3L,
    import_mapping_id=4L, source_match='{"source.Analyte":"Conductivity"}')
  state$review_rows <- function() state$context
  state$manage_mapping_selected <- function() state$row
  state$opened_review_mapping <- function() state$context
  environment(render) <- state
  complete <- as.character(render())
  expect_false(grepl("Needs attention",complete,fixed=TRUE))
  expect_match(complete,"alert-info",fixed=TRUE)
  expect_match(complete,"Saving updates this mapping immediately",fixed=TRUE)
  state$context$issue <- "Unit missing"
  incomplete <- as.character(render())
  expect_match(incomplete,"needs attention",fixed=TRUE)
  expect_match(incomplete,"Unit missing",fixed=TRUE)
  expect_match(incomplete,"alert-warning",fixed=TRUE)
})

test_that("source-wide mapping summaries count real rows and name the source", {
  env <- import_setup_environment()
  statements <- as.list(body(env$importSetup)[[2]][[3]][[3]])[-1]
  assignment <- Filter(function(x) is.call(x) && identical(x[[1]], quote(`<-`)) &&
    identical(x[[2]], quote(output$mapping_status)), statements)[[1]]
  render <- function() NULL
  body(render) <- assignment[[3]][[2]]
  state <- new.env(parent = env)
  state$tags <- htmltools::tags
  state$selected_manage_profile <- function() data.frame(source_code = "LAB",
    profile_code = NA_character_, profile_name = NA_character_)
  state$review_rows <- function() data.frame(source_code = "LAB",
    profile_code = c(NA_character_, NA_character_, "workbook"), active = TRUE, issue = NA_character_)
  environment(render) <- state
  html <- as.character(render())
  expect_match(html, "2 current mapping(s) for LAB (source-wide).", fixed = TRUE)
  expect_false(grepl("for NA", html, fixed = TRUE))
})
