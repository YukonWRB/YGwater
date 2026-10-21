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

test_that("publication review includes deactivation, chemistry and all mapping types", {
  env <- import_setup_environment()
  rows <- data.frame(status=rep(c("published","draft"),each=3),
    mapping_type=rep(c("parameter","location","result_flag"),2),
    source_match=rep(c('{"ParamCode":"AL","input_unit":"mg/L"}',
      '{"source_location_code":"SITE"}','{"source_flag_value":"Q"}'),2),
    definition=c('{"parameter_id":1,"active":true,"analysis_conditions":{}}',
      '{"location_id":1,"active":true}','{"result_action":"keep_result","active":true}',
      '{"parameter_id":2,"active":false,"analysis_conditions":{"mass_basis":"dry"}}',
      '{"location_id":2,"active":true}','{"result_action":"skip_result","active":true}'))
  changes <- env$importSetup_changes(rows,labels=list(parameter_id=c("1"="Same label","2"="Same label")))
  expect_setequal(changes$Type,c("parameter","location","result_flag"))
  expect_true(all(c("Parameter","Active","Analysis conditions","Location","Result handling") %in% changes$Field))
  expect_identical(changes$Before[changes$Field=="Parameter"],"Same label [1]")
  expect_identical(changes$After[changes$Field=="Parameter"],"Same label [2]")
  expect_identical(changes$After[changes$Field=="Active"],"FALSE")
  expect_match(changes$After[changes$Field=="Analysis conditions"],"dry",fixed=TRUE)
})

test_that("unchanged definitions and new mappings have distinct review results", {
  env <- import_setup_environment()
  rows <- data.frame(status=c("published","draft"),mapping_type="parameter",
    source_match='{"code":"A"}',definition='{"parameter_id":1,"active":true}')
  expect_equal(nrow(env$importSetup_changes(rows)),0)
  rows$status <- "draft"
  rows <- rows[1,,drop=FALSE]
  changes <- env$importSetup_changes(rows)
  expect_true(all(changes$Change=="New mapping"))
  expect_true(all(changes$Before==""))
})

test_that("an unspecified laboratory stays unassigned in workbook defaults", {
  env <- import_setup_environment()
  profile <- data.frame(profile_code="no_lab")
  profile$defaults <- I(list(list(owner=1L,media_id=1L)))
  expect_true(is.na(env$addDiscData_defaults(profile)$laboratory))
  profile$defaults[[1]]$laboratory <- 7L
  expect_identical(env$addDiscData_defaults(profile)$laboratory,7L)
})

test_that("review labels show source conditions and hide derived storage fields", {
  env <- import_setup_environment()
  expect_identical(env$importSetup_criteria_label('{"source.Speciation":"as N","unit":""}'),
    "Speciation = as N; Unit = (empty)")
  rows <- data.frame(status="draft", mapping_type="parameter", source_match='{"source.Analyte":"pH"}',
    definition='{"source_match":{"source.Analyte":"pH"},"analysis_conditions_key":"hash","parameter_id":1}')
  changes <- env$importSetup_changes(rows)
  expect_identical(changes$Field,"Parameter")
  expect_identical(changes$Criteria,"Analyte = pH")
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
  state$row <- data.frame(import_mapping_id=4L)
  state$context <- data.frame(source_code="ALS",profile_code="xlr",profile_name="Detailed Report",
    mapping_type="parameter",issue=NA_character_,status="published",version=3L,
    source_match='{"source.Analyte":"Conductivity"}')
  state$manage_mapping_selected <- function() state$row
  state$opened_review_mapping <- function() state$context
  environment(render) <- state
  complete <- as.character(render())
  expect_false(grepl("Needs attention",complete,fixed=TRUE))
  expect_match(complete,"alert-info",fixed=TRUE)
  expect_match(complete,"published revision 3",fixed=TRUE)
  state$context$issue <- "Unit missing"
  incomplete <- as.character(render())
  expect_match(incomplete,"Needs attention",fixed=TRUE)
  expect_match(incomplete,"Unit missing",fixed=TRUE)
  expect_match(incomplete,"alert-warning",fixed=TRUE)
})
