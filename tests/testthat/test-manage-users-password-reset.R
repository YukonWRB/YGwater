test_that("Manage users exposes a guarded password-reset workflow", {
  module_path <- system.file(
    "apps/YGwater/modules/admin/users/manageUsers.R",
    package = "YGwater"
  )
  module <- paste(readLines(module_path, warn = FALSE), collapse = "\n")

  expect_match(module, '"Reset user password"', fixed = TRUE)
  expect_match(module, 'ns("password_reset_user")', fixed = TRUE)
  expect_match(module, 'ns("new_user_password")', fixed = TRUE)
  expect_match(module, 'ns("confirm_user_password")', fixed = TRUE)
  expect_match(module, 'title = "Confirm password reset"', fixed = TRUE)
  expect_match(
    module,
    "password_reset_users(setdiff(users, current_user))",
    fixed = TRUE
  )
  expect_match(
    module,
    "!user %in% password_reset_users()",
    fixed = TRUE
  )
  expect_match(
    module,
    '"ALTER ROLE %s WITH PASSWORD %s;"',
    fixed = TRUE
  )
  expect_match(module, "DBI::dbQuoteIdentifier(", fixed = TRUE)
  expect_match(module, "DBI::dbQuoteString(", fixed = TRUE)
  expect_match(
    module,
    'updateTextInput(session, "new_user_password", value = "")',
    fixed = TRUE
  )
  expect_match(
    module,
    'updateTextInput(session, "confirm_user_password", value = "")',
    fixed = TRUE
  )
})

test_that("Manage users help documents password resets", {
  help_path <- system.file(
    "apps/YGwater/www/html/admin_help/pages/manageUsers.html",
    package = "YGwater"
  )
  help <- paste(readLines(help_path, warn = FALSE), collapse = "\n")

  expect_match(help, "Resetting a password", fixed = TRUE)
  expect_match(help, "old password stops working immediately", fixed = TRUE)
  expect_match(help, "Your own account is not listed", fixed = TRUE)
})

test_that("Manage users safely resets another user's password", {
  module_path <- system.file(
    "apps/YGwater/modules/admin/users/manageUsers.R",
    package = "YGwater"
  )
  env <- new.env(parent = asNamespace("shiny"))
  env$application_notifications_ui <- function(...) NULL
  sys.source(module_path, envir = env)

  executed_sql <- character(0)
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      statement <- as.character(statement)
      if (grepl("information_schema.schemata", statement, fixed = TRUE)) {
        return(data.frame(schema_name = "public"))
      }
      if (grepl("FROM pg_catalog.pg_roles", statement, fixed = TRUE)) {
        return(data.frame(
          rolname = c("manager", 'target"user', "editors_group"),
          rolcanlogin = c(TRUE, TRUE, FALSE)
        ))
      }
      if (grepl("SELECT current_user", statement, fixed = TRUE)) {
        return(data.frame(current_user = "manager"))
      }
      stop("Unexpected query in password-reset test: ", statement)
    },
    dbExecute = function(conn, statement, ...) {
      executed_sql <<- c(executed_sql, as.character(statement))
      1L
    },
    .package = "DBI"
  )

  mock_session <- shiny::MockShinySession$new()
  mock_session$userData$AquaCache <- DBI::ANSI()
  mock_session$userData$config <- list(dbName = "aquacache")
  mock_session$userData$user_logged_in <- TRUE
  mock_session$userData$can_create_role <- TRUE

  shiny::testServer(
    env$manageUsers,
    args = list(
      language = shiny::reactiveValues(language = "en"),
      modules = character(0),
      module_requirements = list()
    ),
    session = mock_session,
    {
      session$flushReact()
      session$setInputs(
        password_reset_user = "manager",
        new_user_password = "SecurePass1",
        confirm_user_password = "SecurePass1",
        request_user_password_reset = 1
      )
      session$flushReact()

      expect_length(executed_sql, 0)
      expect_match(output$status, "Change password page", fixed = TRUE)

      session$setInputs(
        password_reset_user = 'target"user',
        new_user_password = "Secure'Pass1",
        confirm_user_password = "Secure'Pass1",
        request_user_password_reset = 2
      )
      session$flushReact()
      session$setInputs(confirm_user_password_reset = 1)
      session$flushReact()

      expect_equal(
        executed_sql,
        "ALTER ROLE \"target\"\"user\" WITH PASSWORD 'Secure''Pass1';"
      )
      expect_equal(output$status, "Reset password for user 'target\"user'.")
    }
  )
})
