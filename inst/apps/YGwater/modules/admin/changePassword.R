changePasswordUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("pwd_ui"))
  )
}

changePassword <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$pwd_ui <- renderUI({
      req(language$language)
      tagList(
        passwordInput(ns("current_pwd"), tr("current_pw", language$language)),
        passwordInput(ns("new_pwd"), tr("new_pw", language$language)),
        passwordInput(ns("confirm_pwd"), tr("confirm_pw", language$language)),
        actionButton(ns("submit"), tr("pw_change_btn", language$language), class = "btn-primary")
      )
    })

    observeEvent(input$submit, {
      req(input$current_pwd, input$new_pwd, input$confirm_pwd)

      if (input$new_pwd != input$confirm_pwd) {
        showModal(modalDialog(
          title = tr("pw_change_title", language$language),
          tr("pw_change_fail_match", language$language),
          easyClose = TRUE,
          footer = modalButton(tr("close", language$language))
        ))
        return()
      }

      # Verify current password
      tryCatch({
        test_conn <- AquaConnect(
          name = session$userData$config$dbName,
          host = session$userData$config$dbHost,
          port = session$userData$config$dbPort,
          username = session$userData$config$dbUser,
          password = input$current_pwd,
          silent = TRUE
        )
        on.exit(DBI::dbDisconnect(test_conn), add = TRUE)
        DBI::dbGetQuery(test_conn, "SELECT 1;")
      }, error = function(e) {
        showModal(modalDialog(
          title = tr("pw_change_title", language$language),
          tr("pw_change_fail_current", language$language),
          easyClose = TRUE,
          footer = modalButton(tr("close", language$language))
        ))
        return()
      })

      # Attempt password change
      tryCatch({
        DBI::dbExecute(
          session$userData$AquaCache,
          sprintf(
            "ALTER USER %s WITH PASSWORD %s",
            DBI::dbQuoteIdentifier(session$userData$AquaCache, session$userData$config$dbUser),
            DBI::dbQuoteString(session$userData$AquaCache, input$new_pwd)
          )
        )
        session$userData$config$dbPass <- input$new_pwd
        showModal(modalDialog(
          title = tr("pw_change_title", language$language),
          tr("pw_change_success", language$language),
          easyClose = TRUE,
          footer = modalButton(tr("close", language$language))
        ))
      }, error = function(e) {
        showModal(modalDialog(
          title = tr("error", language$language),
          e$message,
          easyClose = TRUE,
          footer = modalButton(tr("close", language$language))
        ))
      })
    })
  })
}

