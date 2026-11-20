test_that("affected module UIs render their namespaced readiness input", {
  withr::local_envvar(c(R_USER_CACHE_DIR = tempdir()))
  env <- new.env(parent = asNamespace("YGwater"))
  # App globals attach these packages before sourcing the modules. Reproduce
  # their UI lookup environment without starting an app or connecting to SQL.
  for (package in c("shiny", "bslib")) {
    exports <- getNamespaceExports(package)
    list2env(stats::setNames(lapply(exports, function(name) {
      getExportedValue(package, name)
    }), exports), env)
  }
  env$config <- list(sidebar_bg = "#f8f8f8")
  # Time-zone helpers normally come from YGwater_globals(), whose full startup
  # sources unrelated applications. These fixtures keep this a UI-only check.
  env$default_input_timezone <- function() "UTC"
  env$input_timezone_choices <- function() c("UTC" = "UTC")
  env$air_datetime_widget_timezone <- function(...) "UTC"
  env$format_utc_offset <- function(offset) sprintf("UTC%+d", offset)
  cases <- list(
    c("admin/instruments/manageInstruments.R", "manageInstrumentsUI"),
    c("admin/instruments/manageSensors.R", "manageSensorsUI"),
    c("admin/instruments/instrumentMaintenance.R", "instrumentMaintenanceUI"),
    c("admin/instruments/calibrate.R", "calibrateUI"),
    c("admin/applicationTasks/manageNewsContent.R", "manageNewsContentUI"),
    c("admin/users/manageUsers.R", "manageUsersUI"),
    c("admin/imgupload/addImgs.R", "addImgsUI"),
    c("admin/boreholes_wells/editBoreholesWells.R", "editBoreholesWellsUI"),
    c("admin/boreholes_wells/manageBoreholeDocuments.R", "manageBoreholeDocumentsUI"),
    c("admin/boreholes_wells/simplerIndex.R", "simplerIndexUI"),
    c("admin/continuousData/addContData.R", "addContDataUI"),
    c("admin/continuousData/continuousDataReview.R", "continuousDataReviewUI"),
    c("admin/discreteData/importSetup.R", "importSetupUI"),
    c("admin/discreteData/addDiscData.R", "addDiscDataUI"),
    c("admin/discreteData/addGuidelines.R", "addGuidelinesUI"),
    c("admin/documents/viewForecasterComments.R", "viewForecasterCommentsUI"),
    c("admin/metadata/manageReferenceTables.R", "manageParametersUI"),
    c("client/reports/basinDashboard.R", "basinDashboardUIMod")
  )
  for (case in cases) {
    sys.source(system.file("apps", "aqualink", "modules", case[[1L]], package = "YGwater"), env)
    html <- withCallingHandlers(
      htmltools::renderTags(env[[case[[2L]]]]("fixture"))$html,
      warning = function(w) {
        # Existing optional date fields use NA rather than a date string.
        if (grepl("Couldn't coerce the `value` argument to a date string", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
    marker <- xml2::xml_find_all(xml2::read_html(html), "//input[@id='fixture-ui_ready']")
    expect_length(marker, 1L)
    expect_true(nzchar(xml2::xml_attr(marker, "value")), info = case[[2L]])
  }
  html <- htmltools::renderTags(env$referenceTableManagerUI(
    "fixture", env$reference_table_configs()[["networks"]]
  ))$html
  expect_length(xml2::xml_find_all(xml2::read_html(html), "//input[@id='fixture-ui_ready']"), 1L)
})
