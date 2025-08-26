manageUsersUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    tagList(
      h3("Create group"),
      textInput(ns("group_name"), "Group name"),
      actionButton(ns("create_group"), "Create group"),
      hr(),
      h3("Create user"),
      textInput(ns("user_name"), "Username"),
      passwordInput(ns("user_password"), "Password"),
      actionButton(ns("create_user"), "Create user"),
      hr(),
      h3("Add user to group"),
      selectInput(ns("existing_user"), "User", choices = NULL),
      selectInput(ns("existing_group"), "Group", choices = NULL),
      actionButton(ns("add_user_group"), "Add to group"),
      hr(),
      verbatimTextOutput(ns("status"))
    )
  )
}

manageUsers <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conn <- session$userData$AquaCache

    # Reload user and group lists
    load_roles <- function() {
      roles <- DBI::dbGetQuery(conn,
                               "SELECT rolname, rolcanlogin FROM pg_roles WHERE rolname !~ '^pg_' ORDER BY rolname")
      users <- roles$rolname[roles$rolcanlogin]
      groups <- roles$rolname[!roles$rolcanlogin]
      updateSelectInput(session, "existing_user", choices = users)
      updateSelectInput(session, "existing_group", choices = groups)
    }
    load_roles()

    observeEvent(input$create_group, {
      req(input$group_name)
      tryCatch({
        sql <- sprintf("CREATE ROLE %s;", DBI::dbQuoteIdentifier(conn, input$group_name))
        DBI::dbExecute(conn, sql)
        load_roles()
        output$status <- renderText(sprintf("Created group '%s'", input$group_name))
      }, error = function(e) {
        output$status <- renderText(e$message)
      })
    })

    observeEvent(input$create_user, {
      req(input$user_name, input$user_password)
      tryCatch({
        sql <- sprintf(
          "CREATE ROLE %s WITH LOGIN PASSWORD %s;",
          DBI::dbQuoteIdentifier(conn, input$user_name),
          DBI::dbQuoteString(conn, input$user_password)
        )
        DBI::dbExecute(conn, sql)
        load_roles()
        output$status <- renderText(sprintf("Created user '%s'", input$user_name))
      }, error = function(e) {
        output$status <- renderText(e$message)
      })
    })

    observeEvent(input$add_user_group, {
      req(input$existing_user, input$existing_group)
      tryCatch({
        sql <- sprintf(
          "GRANT %s TO %s;",
          DBI::dbQuoteIdentifier(conn, input$existing_group),
          DBI::dbQuoteIdentifier(conn, input$existing_user)
        )
        DBI::dbExecute(conn, sql)
        output$status <- renderText(sprintf("Added '%s' to '%s'",
                                            input$existing_user, input$existing_group))
      }, error = function(e) {
        output$status <- renderText(e$message)
      })
    })
  })
}
