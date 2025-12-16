manageUsersUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    tagList(
      selectizeInput(
        ns("group_user"),
        NULL,
        choices = stats::setNames(
          c("groups", "users", "users_to_groups"),
          c("Create new group", "Create new user", "Assign users to groups")
        )
      ),
      conditionalPanel(
        condition = "input.group_user == 'groups'",
        ns = ns,
        h4("Create group"),
        textInput(ns("group_name"), "Group name"),
        uiOutput(ns("group_options_ui")),
        actionButton(ns("create_group"), "Create group"),
      ),
      conditionalPanel(
        condition = "input.group_user == 'users'",
        ns = ns,
        h4("Create user"),
        textInput(ns("user_name"), "Username"),
        passwordInput(ns("user_password"), "Password"),
        actionButton(ns("create_user"), "Create user"),
      ),
      conditionalPanel(
        condition = "input.group_user == 'users_to_groups'",
        ns = ns,
        h4(
          "Add user to a group. They'll inherit that the group's default permissions on schemas and tables as well as see records restricted to that group. You can add users to multiple groups as needed."
        ),
        selectInput(ns("existing_user"), "User", choices = NULL),
        selectInput(ns("existing_group"), "Group", choices = NULL),
        actionButton(ns("add_user_group"), "Add to group"),
      ),
      verbatimTextOutput(ns("status"))
    )
  )
}

manageUsers <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Fetch the schema names; this is used to grant usage to groups on relevant schemas
    schemas <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "
SELECT schema_name
FROM information_schema.schemata 
WHERE schema_name NOT LIKE 'pg_%' 
  AND schema_name <> 'information_schema'
  AND schema_name <> 'application'
  AND schema_name <> 'information';"
    )

    # Reload user and group lists
    load_roles <- function() {
      roles <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT rolname, rolcanlogin FROM pg_roles WHERE rolname !~ '^pg_' ORDER BY rolname"
      )
      users <- roles$rolname[roles$rolcanlogin]
      groups <- roles$rolname[!roles$rolcanlogin]
      updateSelectInput(session, "existing_user", choices = users)
      updateSelectInput(session, "existing_group", choices = groups)
    }
    load_roles()

    # Observe entry to group_name to show/hide additional options like schema and table permissions
    group_options_rendered <- reactiveVal(FALSE)
    observeEvent(input$group_name, {
      if (nchar(input$group_name) > 0) {
        if (!group_options_rendered()) {
          output$group_options_ui <- renderUI({
            tagList(
              selectizeInput(
                ns("schema_permissions"),
                "Grant usage on schemas",
                choices = schemas$schema_name,
                multiple = TRUE
              ),
              # Selectize inputs below are populated dynamically based on the selected schemas
              selectizeInput(
                ns("table_permission_select"),
                "Grant SELECT on tables",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                ns("table_permission_insert"),
                "Grant INSERT on tables",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                ns("table_permission_delete"),
                "Grant DELETE on tables",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                ns("table_permission_update"),
                "Grant UPDATE on tables",
                choices = NULL,
                multiple = TRUE
              ),
            )
          })
          group_options_rendered(TRUE)
        } else {
          shinyjs::show("group_options_ui")
        }
      } else {
        if (group_options_rendered()) {
          shinyjs::hide("group_options_ui")
        }
      }
    })

    observeEvent(input$schema_permissions, {
      req(input$schema_permissions)
      tables <- DBI::dbGetQuery(
        session$userData$AquaCache,
        sprintf(
          "SELECT table_schema, table_name 
         FROM information_schema.tables 
         WHERE table_schema IN (%s)
         AND table_type = 'BASE TABLE'
        ORDER BY table_schema, table_name;",
          paste(
            DBI::dbQuoteString(
              session$userData$AquaCache,
              input$schema_permissions
            ),
            collapse = ", "
          )
        )
      )
      table_choices <- paste0(tables$table_schema, ".", tables$table_name)
      updateSelectizeInput(
        session,
        "table_permission_select",
        choices = c("All tables", table_choices),
        selected = NULL
      )
      updateSelectizeInput(
        session,
        "table_permission_insert",
        choices = c("All tables", table_choices),
        selected = NULL
      )
      updateSelectizeInput(
        session,
        "table_permission_delete",
        choices = c("All tables", table_choices),
        selected = NULL
      )
      updateSelectizeInput(
        session,
        "table_permission_update",
        choices = c("All tables", table_choices),
        selected = NULL
      )
    })

    observeEvent(input$create_group, {
      req(input$group_name)

      # Disable the create_group button while operation is taking place
      shinyjs::disable("create_group")
      on.exit(shinyjs::enable("create_group"), add = TRUE)

      # Ensure that group_name ends with _group
      if (!grepl("_group$", input$group_name)) {
        output$status <- renderText("Group names must end with '_group'")
        return(NULL)
      }

      if (length(input$schema_permissions) == 0) {
        output$status <- renderText(
          "Please select at least one schema for the group to have usage on."
        )
        return(NULL)
      }

      if (
        length(input$table_permission_select) == 0 &&
          length(input$table_permission_insert) == 0 &&
          length(input$table_permission_update) == 0 &&
          length(input$table_permission_delete) == 0
      ) {
        output$status <- renderText(
          "Please select at least one table permission for the group."
        )
        return(NULL)
      }

      # Ensure the group doesn't already exist
      if (
        input$group_name %in%
          (DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT rolname FROM pg_roles"
          )$rolname)
      ) {
        output$status <- renderText(sprintf(
          "Group '%s' already exists",
          input$group_name
        ))
        return(NULL)
      }

      tryCatch(
        {
          DBI::dbExecute(con, "BEGIN;")
          sql <- paste0(
            "CREATE ROLE ",
            input$group_name,
            " NOLOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;"
          )
          DBI::dbExecute(session$userData$AquaCache, sql)

          # Give the user group usage on relevant schemas
          if (
            !is.null(input$schema_permissions) &&
              length(input$schema_permissions) > 0
          ) {
            for (schema in input$schema_permissions) {
              sql <- paste0(sprintf(
                "GRANT USAGE ON SCHEMA %s TO %s;",
                DBI::dbQuoteIdentifier(session$userData$AquaCache, schema),
                DBI::dbQuoteIdentifier(
                  session$userData$AquaCache,
                  input$group_name
                )
              ))
              DBI::dbExecute(session$userData$AquaCache, sql)
            }
          }

          # Grant table permissions
          grant_table_permissions <- function(tables, permission) {
            if (!is.null(tables) && length(tables) > 0) {
              for (table in tables) {
                if (table == "All tables") {
                  for (schema in input$schema_permissions) {
                    sql <- sprintf(
                      "GRANT %s ON ALL TABLES IN SCHEMA %s TO %s;",
                      permission,
                      DBI::dbQuoteIdentifier(
                        session$userData$AquaCache,
                        schema
                      ),
                      DBI::dbQuoteIdentifier(
                        session$userData$AquaCache,
                        input$group_name
                      )
                    )
                    DBI::dbExecute(session$userData$AquaCache, sql)
                  }
                } else {
                  sql <- sprintf(
                    "GRANT %s ON TABLE %s TO %s;",
                    permission,
                    DBI::dbQuoteIdentifier(session$userData$AquaCache, table),
                    DBI::dbQuoteIdentifier(
                      session$userData$AquaCache,
                      input$group_name
                    )
                  )
                  DBI::dbExecute(session$userData$AquaCache, sql)
                }
              }
            }
          }

          if (
            !is.null(input$table_permission_select) &&
              length(input$table_permission_select) > 0
          ) {
            grant_table_permissions(input$table_permission_select, "SELECT")
          }
          if (
            !is.null(input$table_permission_insert) &&
              length(input$table_permission_insert) > 0
          ) {
            grant_table_permissions(input$table_permission_insert, "INSERT")
          }
          if (
            !is.null(input$table_permission_update) &&
              length(input$table_permission_update) > 0
          ) {
            grant_table_permissions(input$table_permission_update, "UPDATE")
          }
          if (
            !is.null(input$table_permission_delete) &&
              length(input$table_permission_delete) > 0
          ) {
            grant_table_permissions(input$table_permission_delete, "DELETE")
          }

          DBI::dbExecute(con, "COMMIT;")

          load_roles()
          output$status <- renderText(sprintf(
            "Created group '%s'",
            input$group_name
          ))
        },
        error = function(e) {
          output$status <- renderText(e$message)
          DBI::dbExecute(con, "ROLLBACK;")
        },
        warning = function(w) {
          output$status <- renderText(w$message)
          DBI::dbExecute(con, "ROLLBACK;")
        }
      )
    })

    observeEvent(input$create_user, {
      req(input$user_name, input$user_password)
      tryCatch(
        {
          sql <- sprintf(
            "CREATE ROLE %s WITH LOGIN PASSWORD %s;",
            DBI::dbQuoteIdentifier(session$userData$AquaCache, input$user_name),
            DBI::dbQuoteString(session$userData$AquaCache, input$user_password)
          )
          DBI::dbExecute(session$userData$AquaCache, sql)
          load_roles()
          output$status <- renderText(sprintf(
            "Created user '%s'. Remember to add them to relevant user groups!",
            input$user_name
          ))
        },
        error = function(e) {
          output$status <- renderText(e$message)
        }
      )
    })

    observeEvent(input$add_user_group, {
      req(input$existing_user, input$existing_group)
      tryCatch(
        {
          sql <- sprintf(
            "GRANT %s TO %s;",
            DBI::dbQuoteIdentifier(
              session$userData$AquaCache,
              input$existing_group
            ),
            DBI::dbQuoteIdentifier(
              session$userData$AquaCache,
              input$existing_user
            )
          )
          DBI::dbExecute(session$userData$AquaCache, sql)
          output$status <- renderText(sprintf(
            "Added '%s' to '%s'",
            input$existing_user,
            input$existing_group
          ))
        },
        error = function(e) {
          output$status <- renderText(e$message)
        }
      )
    })
  })
}
