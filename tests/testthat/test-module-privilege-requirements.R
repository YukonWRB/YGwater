module_privilege_environment <- function() {
  module_env <- new.env(parent = baseenv())
  sys.source(
    system.file(
      "apps/YGwater/modules/modulePrivilegeRequirements.R",
      package = "YGwater"
    ),
    envir = module_env
  )
  module_env
}

test_that("the privilege catalogue covers server admin privilege keys", {
  module_env <- module_privilege_environment()
  requirements <- module_env$ygwater_module_privilege_requirements()
  server <- readLines(
    system.file("apps/YGwater/server.R", package = "YGwater"),
    warn = FALSE
  )
  matches <- regmatches(
    server,
    gregexpr("admin_privs\\$[A-Za-z0-9_]+", server)
  )
  server_keys <- unique(sub("admin_privs\\$", "", unlist(matches)))

  expect_setequal(
    setdiff(server_keys, "lookup_tables"),
    intersect(server_keys, names(requirements))
  )
})

test_that("navigation visibility uses the registered table privileges", {
  module_env <- module_privilege_environment()
  requirements <- module_env$ygwater_module_privilege_requirements()
  table_privs <- data.frame(
    qual_name = "application.notifications",
    extra_privileges = "INSERT, SELECT, UPDATE",
    stringsAsFactors = FALSE
  )

  access <- module_env$ygwater_admin_privileges(table_privs, requirements)

  expect_true(access$manageNotifications)
  expect_false(access$addLocation)
})

test_that("schema USAGE distinguishes visibility from full functionality", {
  module_env <- module_privilege_environment()
  requirements <- module_env$ygwater_module_privilege_requirements()
  table_status <- data.frame(
    table = "application.notifications",
    privilege = c("INSERT", "SELECT", "UPDATE"),
    object_exists = TRUE,
    granted = TRUE,
    stringsAsFactors = FALSE
  )
  schema_status <- data.frame(
    schema_name = "application",
    object_exists = TRUE,
    granted = FALSE,
    stringsAsFactors = FALSE
  )

  access <- module_env$module_access_summary(
    "manageNotifications",
    requirements,
    table_status,
    schema_status
  )

  expect_true(access$visible)
  expect_false(access[["full functionality"]])
  expect_match(access[["missing privileges"]], "USAGE ON SCHEMA application")
})

test_that("any-table visibility still reports all missing functionality", {
  module_env <- module_privilege_environment()
  requirements <- module_env$ygwater_module_privilege_requirements()
  tables <- requirements$continuousDataReview$tables
  privileges <- requirements$continuousDataReview$privileges
  table_status <- do.call(
    rbind,
    lapply(seq_along(tables), function(i) {
      data.frame(
        table = tables[[i]],
        privilege = privileges[[i]],
        object_exists = TRUE,
        granted = i == 1,
        stringsAsFactors = FALSE
      )
    })
  )
  schema_status <- data.frame(
    schema_name = "continuous",
    object_exists = TRUE,
    granted = TRUE,
    stringsAsFactors = FALSE
  )

  access <- module_env$module_access_summary(
    "continuousDataReview",
    requirements,
    table_status,
    schema_status
  )

  expect_true(access$visible)
  expect_false(access[["full functionality"]])
  expect_match(access[["missing privileges"]], "continuous.approvals")
})

test_that("unregistered public modules are not reported as missing privileges", {
  module_env <- module_privilege_environment()
  access <- module_env$module_access_summary(
    "home",
    module_env$ygwater_module_privilege_requirements(),
    data.frame(),
    data.frame()
  )

  expect_true(access$visible)
  expect_true(access[["full functionality"]])
  expect_identical(access[["missing privileges"]], "")
})

test_that("manageUsers reports the non-inheritable CREATEROLE attribute", {
  module_env <- module_privilege_environment()
  requirements <- module_env$ygwater_module_privilege_requirements()

  denied <- module_env$module_access_summary(
    "manageUsers",
    requirements,
    data.frame(),
    data.frame(),
    role_attributes = c(CREATEROLE = FALSE)
  )
  allowed <- module_env$module_access_summary(
    "manageUsers",
    requirements,
    data.frame(),
    data.frame(),
    role_attributes = c(CREATEROLE = TRUE)
  )

  expect_false(denied$visible)
  expect_match(denied[["missing privileges"]], "CREATEROLE")
  expect_true(allowed$visible)
  expect_true(allowed[["full functionality"]])
})

test_that("viewFeedback requires all feedback table privileges", {
  module_env <- module_privilege_environment()
  requirements <- module_env$ygwater_module_privilege_requirements()
  requirement <- requirements$viewFeedback

  expect_setequal(
    requirement$privileges[[1]],
    c("SELECT", "INSERT", "UPDATE", "DELETE")
  )

  partial_privileges <- data.frame(
    qual_name = "application.feedback",
    extra_privileges = "SELECT, INSERT, UPDATE"
  )
  full_privileges <- data.frame(
    qual_name = "application.feedback",
    extra_privileges = "SELECT, INSERT, UPDATE, DELETE"
  )

  expect_false(
    module_env$ygwater_admin_privileges(
      partial_privileges,
      requirements
    )$viewFeedback
  )
  expect_true(
    module_env$ygwater_admin_privileges(
      full_privileges,
      requirements
    )$viewFeedback
  )
})

test_that("addTimeseries includes privileges used by replace-all helpers", {
  module_env <- module_privilege_environment()
  requirements <- module_env$ygwater_module_privilege_requirements()
  requirement <- requirements$addTimeseries

  required_privileges <- stats::setNames(
    requirement$privileges,
    requirement$tables
  )

  expect_setequal(
    required_privileges[["continuous.timeseries_source_adapters"]],
    c("DELETE", "INSERT", "UPDATE")
  )
  expect_setequal(
    required_privileges[["continuous.transmission_timeseries_mappings"]],
    c("DELETE", "INSERT", "UPDATE")
  )
  expect_setequal(
    required_privileges[[
      "public.locations_metadata_instrument_timeseries"
    ]],
    c("DELETE", "INSERT")
  )
  expect_identical(
    required_privileges[["continuous.corrections"]],
    "INSERT"
  )
})

test_that("addTimeseries is hidden when a replace-all DELETE is missing", {
  module_env <- module_privilege_environment()
  requirements <- module_env$ygwater_module_privilege_requirements()
  requirement <- requirements$addTimeseries
  table_privs <- data.frame(
    qual_name = requirement$tables,
    extra_privileges = vapply(
      requirement$privileges,
      paste,
      collapse = ", ",
      FUN.VALUE = character(1)
    ),
    stringsAsFactors = FALSE
  )

  mapping_row <- table_privs$qual_name ==
    "continuous.transmission_timeseries_mappings"
  table_privs$extra_privileges[mapping_row] <- "INSERT, UPDATE"

  access <- module_env$ygwater_admin_privileges(table_privs, requirements)

  expect_false(access$addTimeseries)
})

test_that("discrete administration covers Patch 60 child-table writes", {
  module_env <- module_privilege_environment()
  requirements <- module_env$ygwater_module_privilege_requirements()

  edit_privileges <- stats::setNames(
    requirements$editSamples$privileges,
    requirements$editSamples$tables
  )
  expect_setequal(
    edit_privileges[["discrete.sample_qualifiers"]],
    c("DELETE", "INSERT")
  )
  expect_setequal(
    edit_privileges[["discrete.sample_observers"]],
    c("DELETE", "INSERT")
  )

  expect_true(all(c(
    "discrete.sample_qualifiers",
    "discrete.sample_observers",
    "discrete.result_aggregations",
    "discrete.result_components"
  ) %in% requirements$syncDisc$tables))
})
