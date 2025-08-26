changePasswordUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("pwd_ui"))
  )
}

changePassword <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    trl <- function(key) tr(key, language$language)  # tiny helper
    
    output$pwd_ui <- renderUI({
      req(language$language)
      tagList(
        passwordInput(ns("current_pwd"), trl("current_pw")),
        passwordInput(ns("new_pwd"),     trl("new_pw")),
        passwordInput(ns("confirm_pwd"), trl("confirm_pw")),
        actionButton(ns("submit"), trl("pw_change_btn"), class = "btn-primary")
      )
    })
    
    observeEvent(input$submit, ignoreInit = TRUE, {
      # prevent double-clicks while processing (if shinyjs available)
      if (requireNamespace("shinyjs", quietly = TRUE)) {
        shinyjs::disable(ns("submit"))
        on.exit(shinyjs::enable(ns("submit")), add = TRUE)
      }
      
      req(input$current_pwd, input$new_pwd, input$confirm_pwd)
      
      if (!identical(input$new_pwd, input$confirm_pwd)) {
        showModal(modalDialog(
          title = trl("pw_change_title"),
          trl("pw_change_fail_match"),
          easyClose = TRUE,
          footer = modalButton(trl("close"))
        ))
        return(invisible(NULL))
      }
      
      # 1) Verify current password
      verified <- FALSE
      test_conn <- NULL
      try({
        test_conn <- AquaConnect(
          name     = session$userData$config$dbName,
          host     = session$userData$config$dbHost,
          port     = session$userData$config$dbPort,
          username = session$userData$config$dbUser,
          password = input$current_pwd,
          silent   = TRUE
        )
        DBI::dbGetQuery(test_conn, "SELECT 1;")
        verified <- TRUE
      }, silent = TRUE)
      if (!is.null(test_conn)) DBI::dbDisconnect(test_conn)
      
      if (!isTRUE(verified)) {
        showModal(modalDialog(
          title = trl("pw_change_title"),
          trl("pw_change_fail_current"),
          easyClose = TRUE,
          footer = modalButton(trl("close"))
        ))
        return(invisible(NULL))
      }
      
      # 2) Attempt password change (quote both user & password)
      sql <- DBI::SQL(sprintf(
        "ALTER ROLE %s WITH PASSWORD %s",
        DBI::dbQuoteIdentifier(session$userData$AquaCache, session$userData$config$dbUser),
        DBI::dbQuoteString(    session$userData$AquaCache, input$new_pwd)
      ))
      
      res <- try(DBI::dbExecute(session$userData$AquaCache, sql), silent = TRUE)
      if (inherits(res, "try-error")) {
        showModal(modalDialog(
          title = trl("error"),
          paste0(conditionMessage(attr(res, "condition"))),
          easyClose = TRUE,
          footer = modalButton(trl("close"))
        ))
        return(invisible(NULL))
      }
      
      # 3) Update in-memory config + clear inputs
      session$userData$config$dbPass <- input$new_pwd
      updateTextInput(session, "current_pwd", value = "")
      updateTextInput(session, "new_pwd",     value = "")
      updateTextInput(session, "confirm_pwd", value = "")
      
      showModal(modalDialog(
        title = trl("pw_change_title"),
        trl("pw_change_success"),
        easyClose = TRUE,
        footer = modalButton(trl("close"))
      ))
    })
  })
}
