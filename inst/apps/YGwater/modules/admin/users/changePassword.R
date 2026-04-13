changePasswordUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    uiOutput(ns("pwd_ui"))
  )
}

changePassword <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    trl <- function(key) tr(key, language$language) # tiny helper

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "changePwd"
      )
    })

    output$pwd_ui <- renderUI({
      req(language$language)
      tagList(
        passwordInput(ns("current_pwd"), trl("current_pw")),
        passwordInput(ns("new_pwd"), trl("new_pw")),
        helpText(
          "Password must be at least 8 characters and include uppercase, lowercase, and a number."
        ),
        actionButton(ns("generate_pwd"), "Generate password"),
        passwordInput(ns("confirm_pwd"), trl("confirm_pw")),
        actionButton(ns("submit"), trl("pw_change_btn"), class = "btn-primary")
      )
    })

    password_requirements <- "Password must be at least 8 characters and include uppercase, lowercase, and a number."
    validate_password <- function(password) {
      nchar(password) >= 8 &&
        grepl("[A-Z]", password) &&
        grepl("[a-z]", password) &&
        grepl("[0-9]", password) &&
        !grepl("\\s", password)
    }
    generate_password <- function(length = 12) {
      length <- max(length, 8)
      upper <- sample(LETTERS, 1)
      lower <- sample(letters, 1)
      digit <- sample(0:9, 1)
      all_chars <- c(LETTERS, letters, 0:9)
      remaining <- sample(all_chars, length - 3, replace = TRUE)
      paste0(sample(c(upper, lower, digit, remaining), length), collapse = "")
    }

    observeEvent(input$generate_pwd, {
      generated_password <- generate_password()
      updateTextInput(session, "new_pwd", value = generated_password)
      updateTextInput(session, "confirm_pwd", value = generated_password)
      showModal(modalDialog(
        title = "Generated password",
        tagList(
          "A new password has been generated and filled in.",
          tags$br(),
          tags$strong("Password:"),
          tags$code(generated_password)
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$submit, ignoreInit = TRUE, {
      req(input$current_pwd, input$new_pwd, input$confirm_pwd)

      req(
        nchar(input$current_pwd) > 0,
        nchar(input$new_pwd) > 0,
        nchar(input$confirm_pwd) > 0
      )

      if (!identical(input$new_pwd, input$confirm_pwd)) {
        showModal(modalDialog(
          title = trl("pw_change_title"),
          trl("pw_change_fail_match"),
          easyClose = TRUE,
          footer = actionButton(ns("close"), trl("close"))
        ))
        return(invisible(NULL))
      }

      if (!validate_password(input$new_pwd)) {
        showModal(modalDialog(
          title = trl("pw_change_title"),
          password_requirements,
          easyClose = TRUE,
          footer = actionButton(ns("close"), trl("close"))
        ))
        return(invisible(NULL))
      }

      # 1) Verify current password
      verified <- FALSE
      test_conn <- NULL
      try(
        {
          test_conn <- AquaConnect(
            name = session$userData$config$dbName,
            host = session$userData$config$dbHost,
            port = session$userData$config$dbPort,
            username = session$userData$config$dbUser,
            password = input$current_pwd,
            silent = TRUE
          )
          DBI::dbGetQuery(test_conn, "SELECT 1;")
          verified <- TRUE
        },
        silent = TRUE
      )
      if (!is.null(test_conn)) {
        DBI::dbDisconnect(test_conn)
      }

      if (!isTRUE(verified)) {
        showModal(modalDialog(
          title = trl("pw_change_title"),
          trl("pw_change_fail_current"),
          easyClose = TRUE,
          footer = actionButton(ns("close"), trl("close"))
        ))
        return(invisible(NULL))
      }

      # 2) Attempt password change (quote both user & password)
      sql <- DBI::SQL(sprintf(
        "ALTER ROLE %s WITH PASSWORD %s",
        DBI::dbQuoteIdentifier(
          session$userData$AquaCache,
          session$userData$config$dbUser
        ),
        DBI::dbQuoteString(session$userData$AquaCache, input$new_pwd)
      ))

      res <- try(DBI::dbExecute(session$userData$AquaCache, sql), silent = TRUE)
      if (inherits(res, "try-error")) {
        showModal(modalDialog(
          title = trl("error"),
          paste0(conditionMessage(attr(res, "condition"))),
          easyClose = TRUE,
          footer = actionButton(ns("close"), trl("close"))
        ))
        return(invisible(NULL))
      }

      # 3) Update in-memory config + clear inputs
      session$userData$config$dbPass <- input$new_pwd
      updateTextInput(session, "current_pwd", value = "")
      updateTextInput(session, "new_pwd", value = "")
      updateTextInput(session, "confirm_pwd", value = "")

      showModal(modalDialog(
        title = trl("pw_change_title"),
        trl("pw_change_success"),
        easyClose = TRUE,
        footer = actionButton(ns("close"), trl("close"))
      ))
    })

    # Observe close button in modal
    observeEvent(input$close, {
      removeModal()
    })
  })
}
