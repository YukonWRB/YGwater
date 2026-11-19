# The Selectize create callback requests a catalogue entry without selecting
# free text. Only an existing or newly committed protocol ID goes into a form.
discrete_protocol_input <- function(id, protocols, selected = NULL) {
  shiny::selectizeInput(
    id,
    "Protocol/method",
    choices = stats::setNames(
      protocols$protocol_method_id,
      protocols$protocol_name
    ),
    selected = selected,
    multiple = TRUE,
    options = list(
      maxItems = 1,
      placeholder = "Search or type a new method",
      create = I(
        "function(name) {
          name = name.trim();
          this.setTextboxValue('');
          if (name) Shiny.setInputValue(
            this.$input.attr('id') + '_create', name, {priority: 'event'}
          );
          return false;
        }"
      )
    ),
    width = "100%"
  )
}

# Shared by the mapping and result editors. Each context identifies the
# current form so a modal cannot assign a method to a different selected row.
discrete_protocol_server <- function(input, session, con, contexts) {
  read_protocols <- function() {
    DBI::dbGetQuery(
      con,
      "SELECT protocol_method_id, protocol_name FROM discrete.protocols_methods
       ORDER BY protocol_name, protocol_method_id"
    )
  }
  protocols <- shiny::reactiveVal(read_protocols())
  pending <- shiny::reactiveVal(NULL)
  ns <- session$ns
  for (id in names(contexts)) {
    local({
      field <- id
      shiny::observeEvent(
        input[[paste0(field, "_create")]],
        {
          context <- contexts[[field]]()
          name <- trimws(input[[paste0(field, "_create")]])
          shiny::req(!is.null(context), nzchar(name))
          catalogue <- read_protocols()
          existing <- match(
            tolower(name),
            tolower(trimws(catalogue$protocol_name))
          )
          if (!is.na(existing)) {
            update_protocols(
              catalogue,
              field,
              catalogue$protocol_method_id[[existing]]
            )
            return()
          }
          pending(list(field = field, context = context))
          shiny::showModal(
            shiny::modalDialog(
              title = "Add protocol/method",
              shiny::textInput(ns("new_protocol_name"), "Name", value = name),
              shiny::textAreaInput(
                ns("new_protocol_description"),
                "Description (optional)"
              ),
              shiny::textInput(
                ns("new_protocol_url"),
                "Reference URL (optional)"
              ),
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton(
                  ns("save_new_protocol"),
                  "Add protocol/method",
                  class = "btn-primary"
                )
              )
            ),
            session = session
          )
        },
        ignoreInit = TRUE
      )
    })
  }
  update_protocols <- function(catalogue, field, selected) {
    protocols(catalogue)
    choices <- stats::setNames(
      catalogue$protocol_method_id,
      catalogue$protocol_name
    )
    for (id in names(contexts)) {
      shiny::updateSelectizeInput(
        session,
        id,
        choices = choices,
        selected = if (identical(id, field)) {
          as.character(selected)
        } else {
          as.character(input[[id]])
        }
      )
    }
  }
  shiny::observeEvent(
    input$save_new_protocol,
    {
      request <- pending()
      shiny::req(!is.null(request))
      if (!identical(request$context, contexts[[request$field]]())) {
        pending(NULL)
        shiny::removeModal(session)
        shiny::showNotification(
          "The selected row changed. Open the protocol picker again.",
          type = "warning",
          session = session
        )
        return()
      }
      name <- trimws(input$new_protocol_name)
      if (!nzchar(name)) {
        shiny::showNotification(
          "Enter a protocol/method name.",
          type = "warning",
          session = session
        )
        return()
      }
      description <- trimws(input$new_protocol_description)
      url <- trimws(input$new_protocol_url)
      saved <- tryCatch(
        DBI::dbWithTransaction(con, {
          # Serialize mixed-case requests for the same catalogue name.
          DBI::dbGetQuery(
            con,
            "SELECT pg_advisory_xact_lock(hashtextextended(
             'discrete.protocols_methods:' || lower($1), 0))",
            params = list(name)
          )
          existing <- DBI::dbGetQuery(
            con,
            "SELECT protocol_method_id FROM discrete.protocols_methods
           WHERE lower(btrim(protocol_name)) = lower($1)
           ORDER BY (protocol_name = $1) DESC, protocol_method_id LIMIT 1",
            params = list(name)
          )
          if (!nrow(existing)) {
            existing <- DBI::dbGetQuery(
              con,
              "INSERT INTO discrete.protocols_methods (protocol_name, protocol_description, url)
             VALUES ($1, NULLIF($2, ''), NULLIF($3, ''))
             ON CONFLICT (protocol_name) DO NOTHING RETURNING protocol_method_id",
              params = list(name, description, url)
            )
            if (!nrow(existing)) {
              existing <- DBI::dbGetQuery(
                con,
                "SELECT protocol_method_id FROM discrete.protocols_methods WHERE protocol_name = $1",
                params = list(name)
              )
            }
          }
          list(
            id = existing$protocol_method_id[[1]],
            catalogue = read_protocols()
          )
        }),
        error = identity
      )
      if (inherits(saved, "error")) {
        shiny::showNotification(
          paste("Could not add protocol/method.", conditionMessage(saved)),
          type = "error",
          session = session
        )
        return()
      }
      update_protocols(saved$catalogue, request$field, saved$id)
      pending(NULL)
      shiny::removeModal(session)
    },
    ignoreInit = TRUE
  )
  protocols
}
