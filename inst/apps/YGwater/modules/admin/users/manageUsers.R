manageUsersUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    tagList(
      uiOutput(ns("banner")),
      selectizeInput(
        ns("group_user"),
        NULL,
        choices = stats::setNames(
          c(
            "groups",
            "users",
            "users_to_groups",
            "group_privileges",
            "user_privileges",
            "delete_roles"
          ),
          c(
            "Create new group",
            "Create new user",
            "Assign users to groups",
            "Modify group privileges",
            "Modify user privileges",
            "Delete users/groups"
          )
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
        helpText(
          "Password must be at least 8 characters and include uppercase, lowercase, and a number."
        ),
        actionButton(ns("generate_password"), "Generate password"),
        actionButton(ns("create_user"), "Create user"),
      ),
      conditionalPanel(
        condition = "input.group_user == 'users_to_groups'",
        ns = ns,
        h5(
          "Add user to a group. They'll inherit the group's default permissions on schemas and tables as well as see records restricted to that group. You can add users to multiple groups as needed."
        ),
        selectInput(ns("existing_user"), "User", choices = NULL),
        selectInput(ns("existing_group"), "Group", choices = NULL),
        actionButton(ns("add_user_group"), "Add to group"),
      ),
      conditionalPanel(
        condition = "input.group_user == 'group_privileges'",
        ns = ns,
        h4("Modify group privileges"),
        selectInput(ns("privilege_group"), "Group", choices = NULL),
        selectizeInput(
          ns("group_schema_usage"),
          "Direct USAGE on schemas",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns("group_table_schemas"),
          "Schemas to show table privileges for",
          choices = NULL,
          multiple = TRUE
        ),
        helpText("Table privilege controls apply to tables in the selected schemas."),
        selectizeInput(
          ns("group_table_permission_select"),
          "Direct SELECT on tables",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns("group_table_permission_insert"),
          "Direct INSERT on tables",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns("group_table_permission_delete"),
          "Direct DELETE on tables",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns("group_table_permission_update"),
          "Direct UPDATE on tables",
          choices = NULL,
          multiple = TRUE
        ),
        actionButton(ns("save_group_privileges"), "Save group privileges"),
        DT::DTOutput(ns("group_privileges_table"))
      ),
      conditionalPanel(
        condition = "input.group_user == 'user_privileges'",
        ns = ns,
        h4("Modify user privileges"),
        selectInput(ns("privilege_user"), "User", choices = NULL),
        selectizeInput(
          ns("user_schema_usage"),
          "Direct USAGE on schemas",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns("user_table_schemas"),
          "Schemas to show table privileges for",
          choices = NULL,
          multiple = TRUE
        ),
        helpText("Inherited privileges are shown below but only direct user-level grants are modified here."),
        selectizeInput(
          ns("user_table_permission_select"),
          "Direct SELECT on tables",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns("user_table_permission_insert"),
          "Direct INSERT on tables",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns("user_table_permission_delete"),
          "Direct DELETE on tables",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns("user_table_permission_update"),
          "Direct UPDATE on tables",
          choices = NULL,
          multiple = TRUE
        ),
        actionButton(ns("save_user_privileges"), "Save user privileges"),
        DT::DTOutput(ns("user_privileges_table"))
      ),
      conditionalPanel(
        condition = "input.group_user == 'delete_roles'",
        ns = ns,
        h4("Delete users/groups"),
        selectInput(ns("delete_role"), "Role to delete", choices = NULL),
        selectInput(
          ns("delete_replacement_role"),
          "Replacement share_with role",
          choices = NULL
        ),
        checkboxInput(
          ns("delete_drop_owned"),
          "Reassign owned objects and drop privileges before deleting",
          value = FALSE
        ),
        selectInput(
          ns("delete_reassign_owned_to"),
          "Reassign owned objects to",
          choices = NULL
        ),
        actionButton(ns("preview_role_delete"), "Preview references"),
        actionButton(ns("cleanup_role_references"), "Clean share_with references"),
        actionButton(ns("drop_role"), "Drop role"),
        DT::DTOutput(ns("role_delete_preview"))
      ),
      verbatimTextOutput(ns("status"))
    )
  )
}

manageUsers <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    password_requirements <- "Password must be at least 8 characters and include uppercase, lowercase, and a number."
    status <- reactiveVal("")
    set_status <- function(message) {
      status(as.character(message))
    }

    output$status <- renderText(status())

    validate_password <- function(password) {
      nchar(password) >= 8 &&
        grepl("[A-Z]", password) &&
        grepl("[a-z]", password) &&
        grepl("[0-9]", password) &&
        !grepl("\\s", password)
    }

    is_valid_group_name <- function(group_name) {
      is.character(group_name) &&
        length(group_name) == 1 &&
        !is.na(group_name) &&
        grepl("^[A-Za-z0-9_]+_group$", group_name)
    }

    quote_identifier_sql <- function(value) {
      as.character(DBI::dbQuoteIdentifier(session$userData$AquaCache, value))
    }

    quote_qualified_table_sql <- function(value) {
      parts <- strsplit(as.character(value), ".", fixed = TRUE)[[1]]
      if (length(parts) != 2 || any(!nzchar(parts))) {
        stop("Invalid table name selected.")
      }
      as.character(DBI::dbQuoteIdentifier(
        session$userData$AquaCache,
        DBI::Id(schema = parts[[1]], table = parts[[2]])
      ))
    }

    table_privileges <- c("SELECT", "INSERT", "UPDATE", "DELETE")

    input_or_empty <- function(value) {
      if (is.null(value)) {
        character(0)
      } else {
        unique(as.character(value))
      }
    }

    sql_string_list <- function(value) {
      paste(
        DBI::dbQuoteString(session$userData$AquaCache, value),
        collapse = ", "
      )
    }

    db_function_exists <- function(signature) {
      DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT to_regprocedure($1) IS NOT NULL AS exists",
        params = list(signature)
      )$exists[[1]]
    }

    role_deletion_functions <- function() {
      c(
        role_share_with_references = db_function_exists(
          "public.role_share_with_references(text)"
        ),
        cleanup_share_with_role = db_function_exists(
          "public.cleanup_share_with_role(text,text,boolean)"
        ),
        drop_role_if_unused = db_function_exists(
          "public.drop_role_if_unused(text,text,boolean)"
        )
      )
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

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "manageUsers"
      )
    })

    if (
      !isTRUE(session$userData$user_logged_in) ||
        !isTRUE(session$userData$can_create_role)
    ) {
      showModal(modalDialog(
        title = "Insufficient Privileges",
        "You do not have the necessary privileges to manage users.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    observeEvent(input$generate_password, {
      generated_password <- generate_password()
      updateTextInput(session, "user_password", value = generated_password)
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
    available_table_choices <- reactiveVal(character(0))
    existing_users <- reactiveVal(character(0))
    existing_groups <- reactiveVal(character(0))
    existing_roles <- reactiveVal(character(0))
    can_reassign_owned_to_users <- reactiveVal(FALSE)
    role_delete_preview <- reactiveVal(data.frame())

    update_delete_reassign_choices <- function() {
      delete_role <- isolate(input$delete_role)
      choices <- if (isTRUE(can_reassign_owned_to_users())) {
        existing_roles()
      } else {
        existing_groups()
      }
      choices <- setdiff(choices, delete_role)
      selected <- isolate(input$delete_reassign_owned_to)
      if (is.null(selected) || !(selected %in% choices)) {
        selected <- if ("postgres" %in% choices) {
          "postgres"
        } else if (length(choices) > 0) {
          choices[[1]]
        } else {
          character(0)
        }
      }

      updateSelectInput(
        session,
        "delete_reassign_owned_to",
        choices = choices,
        selected = selected
      )
    }

    load_roles <- function() {
      current_delete_role <- isolate(input$delete_role)
      current_replacement_role <- isolate(input$delete_replacement_role)

      roles <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT rolname, rolcanlogin
FROM pg_roles
WHERE rolname !~ '^pg_'
  AND rolname <> 'public'
ORDER BY rolname"
      )
      current_user <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT current_user"
      )$current_user[[1]]

      users <- roles$rolname[
        roles$rolcanlogin & !roles$rolname %in% c("admin", "postgres")
      ]
      groups <- roles$rolname[
        !roles$rolcanlogin & !roles$rolname %in% c("admin", "postgres")
      ]
      role_names <- roles$rolname
      existing_users(users)
      existing_groups(groups)
      existing_roles(role_names)
      can_reassign_owned_to_users(current_user %in% c("admin", "postgres"))
      updateSelectInput(session, "existing_user", choices = users)
      updateSelectInput(session, "existing_group", choices = groups)
      updateSelectInput(session, "privilege_user", choices = users)
      updateSelectInput(session, "privilege_group", choices = groups)

      delete_choices <- setdiff(c(users, groups), "public_reader")
      updateSelectInput(
        session,
        "delete_role",
        choices = delete_choices,
        selected = if (
          !is.null(current_delete_role) &&
            current_delete_role %in% delete_choices
        ) {
          current_delete_role
        } else {
          character(0)
        }
      )

      replacement_choices <- c(
        "Remove without replacement" = "",
        stats::setNames(
          c("public_reader", groups),
          c("public_reader", groups)
        )
      )
      updateSelectInput(
        session,
        "delete_replacement_role",
        choices = replacement_choices,
        selected = if (
          !is.null(current_replacement_role) &&
            current_replacement_role %in% replacement_choices
        ) {
          current_replacement_role
        } else {
          ""
        }
      )
      update_delete_reassign_choices()
    }
    load_roles()

    get_tables <- function(schema_names) {
      schema_names <- input_or_empty(schema_names)
      if (length(schema_names) == 0) {
        return(data.frame(
          table_schema = character(0),
          table_name = character(0),
          table_id = character(0)
        ))
      }

      tables <- DBI::dbGetQuery(
        session$userData$AquaCache,
        sprintf(
          "SELECT table_schema, table_name
FROM information_schema.tables
WHERE table_schema IN (%s)
  AND table_type = 'BASE TABLE'
ORDER BY table_schema, table_name;",
          sql_string_list(schema_names)
        )
      )
      tables$table_id <- paste0(tables$table_schema, ".", tables$table_name)
      tables
    }

    member_groups <- function(role) {
      DBI::dbGetQuery(
        session$userData$AquaCache,
        sprintf(
          "SELECT parent.rolname
FROM pg_roles parent
WHERE parent.rolname <> %s
  AND NOT parent.rolcanlogin
  AND pg_has_role(%s, parent.oid, 'member')
ORDER BY parent.rolname;",
          DBI::dbQuoteString(session$userData$AquaCache, role),
          DBI::dbQuoteString(session$userData$AquaCache, role)
        )
      )$rolname
    }

    schema_privileges <- function(role, inherited_roles = character(0)) {
      role_sql <- DBI::dbQuoteString(session$userData$AquaCache, role)
      inherited_sql <- if (length(inherited_roles) == 0) {
        "NULL"
      } else {
        sql_string_list(inherited_roles)
      }

      DBI::dbGetQuery(
        session$userData$AquaCache,
        sprintf(
          "WITH selected_schemas AS (
  SELECT schema_name
  FROM information_schema.schemata
  WHERE schema_name IN (%s)
),
role_direct AS (
  SELECT n.nspname AS schema_name
  FROM pg_namespace n
  CROSS JOIN LATERAL aclexplode(n.nspacl) acl
  JOIN pg_roles grantee ON grantee.oid = acl.grantee
  WHERE grantee.rolname = %s
    AND acl.privilege_type = 'USAGE'
),
inherited AS (
  SELECT n.nspname AS schema_name,
         string_agg(DISTINCT grantee.rolname, ', ' ORDER BY grantee.rolname) AS inherited_from
  FROM pg_namespace n
  CROSS JOIN LATERAL aclexplode(n.nspacl) acl
  JOIN pg_roles grantee ON grantee.oid = acl.grantee
  WHERE grantee.rolname IN (%s)
    AND acl.privilege_type = 'USAGE'
  GROUP BY n.nspname
)
SELECT ss.schema_name,
       rd.schema_name IS NOT NULL AS direct,
       COALESCE(i.inherited_from, '') AS inherited_from,
       has_schema_privilege(%s, ss.schema_name, 'USAGE') AS effective
FROM selected_schemas ss
LEFT JOIN role_direct rd ON rd.schema_name = ss.schema_name
LEFT JOIN inherited i ON i.schema_name = ss.schema_name
ORDER BY ss.schema_name;",
          sql_string_list(schemas$schema_name),
          role_sql,
          inherited_sql,
          role_sql
        )
      )
    }

    table_privilege_summary <- function(
      role,
      schema_names,
      inherited_roles = character(0)
    ) {
      tables <- get_tables(schema_names)
      if (nrow(tables) == 0) {
        return(data.frame(
          table = character(0),
          privilege = character(0),
          direct = logical(0),
          inherited_from = character(0),
          effective = logical(0)
        ))
      }

      role_sql <- DBI::dbQuoteString(session$userData$AquaCache, role)
      inherited_sql <- if (length(inherited_roles) == 0) {
        "NULL"
      } else {
        sql_string_list(inherited_roles)
      }

      DBI::dbGetQuery(
        session$userData$AquaCache,
        sprintf(
          "WITH selected_tables AS (
  SELECT table_schema, table_name
  FROM information_schema.tables
  WHERE table_schema IN (%s)
    AND table_type = 'BASE TABLE'
),
permissions(privilege) AS (
  VALUES ('SELECT'), ('INSERT'), ('UPDATE'), ('DELETE')
),
role_direct AS (
  SELECT n.nspname AS table_schema,
         c.relname AS table_name,
         acl.privilege_type AS privilege
  FROM pg_class c
  JOIN pg_namespace n ON n.oid = c.relnamespace
  CROSS JOIN LATERAL aclexplode(c.relacl) acl
  JOIN pg_roles grantee ON grantee.oid = acl.grantee
  WHERE c.relkind IN ('r', 'p')
    AND grantee.rolname = %s
),
inherited AS (
  SELECT n.nspname AS table_schema,
         c.relname AS table_name,
         acl.privilege_type AS privilege,
         string_agg(DISTINCT grantee.rolname, ', ' ORDER BY grantee.rolname) AS inherited_from
  FROM pg_class c
  JOIN pg_namespace n ON n.oid = c.relnamespace
  CROSS JOIN LATERAL aclexplode(c.relacl) acl
  JOIN pg_roles grantee ON grantee.oid = acl.grantee
  WHERE c.relkind IN ('r', 'p')
    AND grantee.rolname IN (%s)
  GROUP BY n.nspname, c.relname, acl.privilege_type
)
SELECT st.table_schema || '.' || st.table_name AS table,
       p.privilege,
       rd.privilege IS NOT NULL AS direct,
       COALESCE(i.inherited_from, '') AS inherited_from,
       has_table_privilege(
         %s,
         format('%%I.%%I', st.table_schema, st.table_name),
         p.privilege
       ) AS effective
FROM selected_tables st
CROSS JOIN permissions p
LEFT JOIN role_direct rd
  ON rd.table_schema = st.table_schema
 AND rd.table_name = st.table_name
 AND rd.privilege = p.privilege
LEFT JOIN inherited i
  ON i.table_schema = st.table_schema
 AND i.table_name = st.table_name
 AND i.privilege = p.privilege
ORDER BY st.table_schema, st.table_name, p.privilege;",
          sql_string_list(schema_names),
          role_sql,
          inherited_sql,
          role_sql
        )
      )
    }

    update_role_privilege_inputs <- function(prefix, role, inherited_roles) {
      schema_privs <- schema_privileges(role, inherited_roles)
      selected_schemas <- schema_privs$schema_name[schema_privs$direct]
      table_schemas <- schema_privs$schema_name[
        schema_privs$direct | schema_privs$effective
      ]

      updateSelectizeInput(
        session,
        paste0(prefix, "_schema_usage"),
        choices = schemas$schema_name,
        selected = selected_schemas
      )
      updateSelectizeInput(
        session,
        paste0(prefix, "_table_schemas"),
        choices = schemas$schema_name,
        selected = table_schemas
      )

      table_privs <- table_privilege_summary(
        role = role,
        schema_names = table_schemas,
        inherited_roles = inherited_roles
      )
      table_choices <- unique(table_privs$table)

      for (privilege in table_privileges) {
        input_id <- paste0(prefix, "_table_permission_", tolower(privilege))
        selected <- table_privs$table[
          table_privs$privilege == privilege & table_privs$direct
        ]
        updateSelectizeInput(
          session,
          input_id,
          choices = table_choices,
          selected = selected
        )
      }
    }

    privilege_table_output <- function(role, schema_names, inherited_roles) {
      schema_privs <- schema_privileges(role, inherited_roles)
      schema_display <- data.frame(
        object = schema_privs$schema_name,
        object_type = "schema",
        privilege = "USAGE",
        direct = schema_privs$direct,
        inherited_from = schema_privs$inherited_from,
        effective = schema_privs$effective,
        stringsAsFactors = FALSE
      )

      table_display <- table_privilege_summary(
        role = role,
        schema_names = schema_names,
        inherited_roles = inherited_roles
      )
      if (nrow(table_display) > 0) {
        names(table_display)[names(table_display) == "table"] <- "object"
        table_display$object_type <- "table"
        table_display <- table_display[
          ,
          c(
            "object",
            "object_type",
            "privilege",
            "direct",
            "inherited_from",
            "effective"
          )
        ]
      }

      display <- rbind(schema_display, table_display)
      display$direct <- ifelse(display$direct, "yes", "")
      display$effective <- ifelse(display$effective, "yes", "")
      display
    }

    save_direct_privileges <- function(
      role,
      selected_schemas,
      table_schema_names,
      table_grants
    ) {
      if (
        !all(selected_schemas %in% schemas$schema_name) ||
          !all(table_schema_names %in% schemas$schema_name)
      ) {
        stop("Please select schemas from the available list.")
      }

      tables <- get_tables(table_schema_names)
      valid_table_choices <- tables$table_id
      selected_tables <- unique(unlist(table_grants, use.names = FALSE))
      if (
        length(selected_tables) > 0 &&
          !all(selected_tables %in% valid_table_choices)
      ) {
        stop("Please select table permissions from the available list.")
      }

      role_ident <- quote_identifier_sql(role)

      for (schema in schemas$schema_name) {
        sql <- sprintf(
          "%s USAGE ON SCHEMA %s %s %s",
          if (schema %in% selected_schemas) "GRANT" else "REVOKE",
          quote_identifier_sql(schema),
          if (schema %in% selected_schemas) "TO" else "FROM",
          role_ident
        )
        DBI::dbExecute(session$userData$AquaCache, sql)
      }

      if (nrow(tables) == 0) {
        return(invisible(NULL))
      }

      for (privilege in table_privileges) {
        selected <- input_or_empty(table_grants[[privilege]])
        for (table in tables$table_id) {
          sql <- sprintf(
            "%s %s ON TABLE %s %s %s",
            if (table %in% selected) "GRANT" else "REVOKE",
            privilege,
            quote_qualified_table_sql(table),
            if (table %in% selected) "TO" else "FROM",
            role_ident
          )
          DBI::dbExecute(session$userData$AquaCache, sql)
        }
      }

      invisible(NULL)
    }

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
      available_table_choices(table_choices)
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

      if (!is_valid_group_name(input$group_name)) {
        set_status(
          "Group names may only contain letters, numbers, and underscores, and must end with '_group'."
        )
        return(NULL)
      }

      selected_schemas <- unique(as.character(input$schema_permissions))
      if (length(selected_schemas) == 0) {
        set_status(
          "Please select at least one schema for the group to have usage on."
        )
        return(NULL)
      }

      if (!all(selected_schemas %in% schemas$schema_name)) {
        set_status(
          "Please select schema permissions from the available list."
        )
        return(NULL)
      }

      selected_tables <- unique(c(
        input$table_permission_select,
        input$table_permission_insert,
        input$table_permission_update,
        input$table_permission_delete
      ))
      if (
        length(input$table_permission_select) == 0 &&
          length(input$table_permission_insert) == 0 &&
          length(input$table_permission_update) == 0 &&
          length(input$table_permission_delete) == 0
      ) {
        set_status(
          "Please select at least one table permission for the group."
        )
        return(NULL)
      }

      valid_table_choices <- c("All tables", isolate(available_table_choices()))
      if (!all(selected_tables %in% valid_table_choices)) {
        set_status(
          "Please select table permissions from the available list."
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
        set_status(sprintf(
          "Group '%s' already exists",
          input$group_name
        ))
        return(NULL)
      }

      tryCatch(
        {
          DBI::dbExecute(session$userData$AquaCache, "BEGIN;")
          role_ident <- quote_identifier_sql(input$group_name)
          sql <- sprintf(
            "CREATE ROLE %s NOLOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;",
            role_ident
          )
          DBI::dbExecute(session$userData$AquaCache, sql)

          # Give the user group usage on relevant schemas
          if (
            !is.null(input$schema_permissions) &&
              length(input$schema_permissions) > 0
          ) {
            for (schema in selected_schemas) {
              sql <- sprintf(
                "GRANT USAGE ON SCHEMA %s TO %s",
                quote_identifier_sql(schema),
                role_ident
              )
              DBI::dbExecute(
                session$userData$AquaCache,
                sql
              )
            }
          }

          # Grant table permissions
          grant_table_permissions <- function(tables, permission) {
            if (!is.null(tables) && length(tables) > 0) {
              for (table in tables) {
                if (table == "All tables") {
                  for (schema in selected_schemas) {
                    sql <- sprintf(
                      "GRANT %s ON ALL TABLES IN SCHEMA %s TO %s",
                      permission,
                      quote_identifier_sql(schema),
                      role_ident
                    )
                    DBI::dbExecute(
                      session$userData$AquaCache,
                      sql
                    )
                  }
                } else {
                  sql <- sprintf(
                    "GRANT %s ON TABLE %s TO %s",
                    permission,
                    quote_qualified_table_sql(table),
                    role_ident
                  )
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    sql
                  )
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

          DBI::dbExecute(session$userData$AquaCache, "COMMIT;")

          load_roles()
          set_status(sprintf(
            "Created group '%s'",
            input$group_name
          ))
        },
        error = function(e) {
          set_status(e$message)
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK;")
        },
        warning = function(w) {
          set_status(w$message)
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK;")
        }
      )
    })

    observeEvent(
      input$create_user,
      {
        req(input$user_name, input$user_password)
        if (!validate_password(input$user_password)) {
          set_status(password_requirements)
          return(NULL)
        }
        tryCatch(
          {
            sql <- sprintf(
              "CREATE ROLE %s WITH LOGIN PASSWORD %s;",
              DBI::dbQuoteIdentifier(
                session$userData$AquaCache,
                input$user_name
              ),
              DBI::dbQuoteString(
                session$userData$AquaCache,
                input$user_password
              )
            )
            DBI::dbExecute(session$userData$AquaCache, sql)
            load_roles()
            set_status(sprintf(
              "Created user '%s'. Remember to add them to relevant user groups!",
              input$user_name
            ))
          },
          error = function(e) {
            set_status(e$message)
          }
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_user_group,
      {
        user <- input$existing_user
        group <- input$existing_group
        req(user, group)

        shinyjs::disable("add_user_group")
        on.exit(shinyjs::enable("add_user_group"), add = TRUE)

        tryCatch(
          {
            sql <- sprintf(
              "GRANT %s TO %s",
              DBI::dbQuoteIdentifier(session$userData$AquaCache, group),
              DBI::dbQuoteIdentifier(session$userData$AquaCache, user)
            )

            DBI::dbExecute(session$userData$AquaCache, sql)

            set_status(sprintf(
              "Added '%s' to '%s'",
              user,
              group
            ))
          },
          error = function(e) {
            set_status(e$message)
          }
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$privilege_group,
      {
        req(input$privilege_group)
        update_role_privilege_inputs(
          prefix = "group",
          role = input$privilege_group,
          inherited_roles = character(0)
        )
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$group_table_schemas,
      {
        req(input$privilege_group)
        table_privs <- table_privilege_summary(
          role = input$privilege_group,
          schema_names = input_or_empty(input$group_table_schemas),
          inherited_roles = character(0)
        )
        table_choices <- unique(table_privs$table)

        for (privilege in table_privileges) {
          input_id <- paste0(
            "group_table_permission_",
            tolower(privilege)
          )
          selected <- table_privs$table[
            table_privs$privilege == privilege & table_privs$direct
          ]
          updateSelectizeInput(
            session,
            input_id,
            choices = table_choices,
            selected = selected
          )
        }
      },
      ignoreInit = FALSE,
      ignoreNULL = FALSE
    )

    output$group_privileges_table <- DT::renderDT({
      req(input$privilege_group)
      DT::datatable(
        privilege_table_output(
          role = input$privilege_group,
          schema_names = input_or_empty(input$group_table_schemas),
          inherited_roles = character(0)
        ),
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE)
      )
    })

    observeEvent(
      input$save_group_privileges,
      {
        role <- input$privilege_group
        req(role)

        if (!role %in% existing_groups()) {
          set_status("Please select an existing group.")
          return(NULL)
        }

        shinyjs::disable("save_group_privileges")
        on.exit(shinyjs::enable("save_group_privileges"), add = TRUE)

        selected_schemas <- input_or_empty(input$group_schema_usage)
        table_schema_names <- input_or_empty(input$group_table_schemas)
        table_grants <- list(
          SELECT = input_or_empty(input$group_table_permission_select),
          INSERT = input_or_empty(input$group_table_permission_insert),
          UPDATE = input_or_empty(input$group_table_permission_update),
          DELETE = input_or_empty(input$group_table_permission_delete)
        )

        tryCatch(
          {
            DBI::dbExecute(session$userData$AquaCache, "BEGIN;")
            save_direct_privileges(
              role,
              selected_schemas,
              table_schema_names,
              table_grants
            )
            DBI::dbExecute(session$userData$AquaCache, "COMMIT;")
            update_role_privilege_inputs(
              prefix = "group",
              role = role,
              inherited_roles = character(0)
            )
            set_status(sprintf("Updated privileges for group '%s'.", role))
          },
          error = function(e) {
            DBI::dbExecute(session$userData$AquaCache, "ROLLBACK;")
            set_status(e$message)
          }
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$privilege_user,
      {
        req(input$privilege_user)
        update_role_privilege_inputs(
          prefix = "user",
          role = input$privilege_user,
          inherited_roles = member_groups(input$privilege_user)
        )
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$user_table_schemas,
      {
        req(input$privilege_user)
        groups <- member_groups(input$privilege_user)
        table_privs <- table_privilege_summary(
          role = input$privilege_user,
          schema_names = input_or_empty(input$user_table_schemas),
          inherited_roles = groups
        )
        table_choices <- unique(table_privs$table)

        for (privilege in table_privileges) {
          input_id <- paste0(
            "user_table_permission_",
            tolower(privilege)
          )
          selected <- table_privs$table[
            table_privs$privilege == privilege & table_privs$direct
          ]
          updateSelectizeInput(
            session,
            input_id,
            choices = table_choices,
            selected = selected
          )
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    output$user_privileges_table <- DT::renderDT({
      req(input$privilege_user)
      DT::datatable(
        privilege_table_output(
          role = input$privilege_user,
          schema_names = input_or_empty(input$user_table_schemas),
          inherited_roles = member_groups(input$privilege_user)
        ),
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE)
      )
    })

    observeEvent(
      input$save_user_privileges,
      {
        role <- input$privilege_user
        req(role)

        if (!role %in% existing_users()) {
          set_status("Please select an existing user.")
          return(NULL)
        }

        shinyjs::disable("save_user_privileges")
        on.exit(shinyjs::enable("save_user_privileges"), add = TRUE)

        selected_schemas <- input_or_empty(input$user_schema_usage)
        table_schema_names <- input_or_empty(input$user_table_schemas)
        table_grants <- list(
          SELECT = input_or_empty(input$user_table_permission_select),
          INSERT = input_or_empty(input$user_table_permission_insert),
          UPDATE = input_or_empty(input$user_table_permission_update),
          DELETE = input_or_empty(input$user_table_permission_delete)
        )

        tryCatch(
          {
            DBI::dbExecute(session$userData$AquaCache, "BEGIN;")
            save_direct_privileges(
              role,
              selected_schemas,
              table_schema_names,
              table_grants
            )
            DBI::dbExecute(session$userData$AquaCache, "COMMIT;")
            update_role_privilege_inputs(
              prefix = "user",
              role = role,
              inherited_roles = member_groups(role)
            )
            set_status(sprintf("Updated direct privileges for user '%s'.", role))
          },
          error = function(e) {
            DBI::dbExecute(session$userData$AquaCache, "ROLLBACK;")
            set_status(e$message)
          }
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    output$role_delete_preview <- DT::renderDT({
      dat <- role_delete_preview()
      if (nrow(dat) == 0) {
        dat <- data.frame(
          message = "Select a role and preview references.",
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(
        dat,
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE)
      )
    })

    observeEvent(
      input$delete_role,
      {
        update_delete_reassign_choices()
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    observeEvent(
      input$preview_role_delete,
      {
        role <- input$delete_role
        req(role)

        available <- role_deletion_functions()
        if (!isTRUE(available[["role_share_with_references"]])) {
          set_status(
            "Role deletion helpers are not installed. Apply AquaCache patch 45 before using this tool."
          )
          return(NULL)
        }

        dat <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT *
FROM public.role_share_with_references($1)
WHERE matched_rows > 0
ORDER BY table_schema, table_name",
          params = list(role)
        )
        if (nrow(dat) == 0) {
          dat <- data.frame(
            table_schema = character(0),
            table_name = character(0),
            matched_rows = integer(0),
            only_role_rows = integer(0),
            stringsAsFactors = FALSE
          )
        }
        role_delete_preview(dat)
        set_status(sprintf("Previewed share_with references for role '%s'.", role))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$cleanup_role_references,
      {
        role <- input$delete_role
        req(role)

        available <- role_deletion_functions()
        if (
          !isTRUE(available[["role_share_with_references"]]) ||
            !isTRUE(available[["cleanup_share_with_role"]])
        ) {
          set_status(
            "Role cleanup helper is not installed. Apply AquaCache patch 45 before using this tool."
          )
          return(NULL)
        }

        replacement <- input$delete_replacement_role
        if (is.null(replacement)) {
          replacement <- ""
        }

        shinyjs::disable("cleanup_role_references")
        on.exit(shinyjs::enable("cleanup_role_references"), add = TRUE)

        tryCatch(
          {
            DBI::dbExecute(session$userData$AquaCache, "BEGIN;")
            dat <- DBI::dbGetQuery(
              session$userData$AquaCache,
              "SELECT *
FROM public.cleanup_share_with_role($1, NULLIF($2, ''), false)
WHERE matched_rows > 0
ORDER BY table_schema, table_name",
              params = list(role, replacement)
            )
            DBI::dbExecute(session$userData$AquaCache, "COMMIT;")
            role_delete_preview(dat)
            set_status(sprintf(
              "Cleaned share_with references for role '%s'.",
              role
            ))
          },
          error = function(e) {
            DBI::dbExecute(session$userData$AquaCache, "ROLLBACK;")
            set_status(e$message)
          }
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$drop_role,
      {
        role <- input$delete_role
        req(role)

        if (role == "public_reader") {
          set_status("The public_reader role cannot be deleted from this tool.")
          return(NULL)
        }

        available <- role_deletion_functions()
        if (!isTRUE(available[["drop_role_if_unused"]])) {
          set_status(
            "Role drop helper is not installed. Apply AquaCache patch 45 before using this tool."
          )
          return(NULL)
        }

        reassign_to <- if (isTRUE(input$delete_drop_owned)) {
          input$delete_reassign_owned_to
        } else {
          ""
        }
        if (
          isTRUE(input$delete_drop_owned) &&
            (is.null(reassign_to) || !nzchar(reassign_to))
        ) {
          set_status("Please select a role to receive reassigned objects.")
          return(NULL)
        }

        shinyjs::disable("drop_role")
        on.exit(shinyjs::enable("drop_role"), add = TRUE)

        tryCatch(
          {
            result <- DBI::dbGetQuery(
              session$userData$AquaCache,
              "SELECT *
FROM public.drop_role_if_unused($1, NULLIF($2, ''), $3)",
              params = list(role, reassign_to, isTRUE(input$delete_drop_owned))
            )
            if (isTRUE(result$dropped[[1]])) {
              load_roles()
              role_delete_preview(data.frame())
            } else {
              updateSelectInput(session, "delete_role", selected = role)
            }
            set_status(result$message[[1]])
          },
          error = function(e) {
            set_status(e$message)
          }
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
  })
}
