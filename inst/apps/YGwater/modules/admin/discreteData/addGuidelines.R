# UI and server code for water quality guidelines management module

addGuidelinesUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$script(HTML(
        "
Shiny.addCustomMessageHandler('insertAtCursor', function(msg) {
      const ta = document.getElementById(msg.target);
      if (!ta) return;
      ta.focus();

      const start = ta.selectionStart ?? 0;
      const end   = ta.selectionEnd   ?? 0;

      // 1) Insert main text at caret
      const before = ta.value.slice(0, start);
      const after  = ta.value.slice(end);
      const insert = msg.text || '';
      ta.value = before + insert + after;

      // Caret after the inserted text (we keep it here)
      const caret = start + insert.length;

      // 2) Append/merge end-of-line comment
      if (msg.eolComment && msg.eolComment.trim().length > 0) {
        let val = ta.value;

        // Identify line containing the caret (post-insert)
        const lineStart = val.lastIndexOf('\\n', caret - 1) + 1;
        const lineEndIdx = val.indexOf('\\n', caret);
        const lineEnd = (lineEndIdx === -1) ? val.length : lineEndIdx;

        const lineText = val.slice(lineStart, lineEnd);
        const commentIdx = lineText.indexOf('--');

        // Normalized helper
        const norm = s => s.replace(/\\s+/g, ' ').trim().toLowerCase();

        if (commentIdx === -1) {
          // No existing comment -> add one
          const left  = val.slice(0, lineEnd).replace(/[ \\t]+$/, ''); // trim right
          const right = val.slice(lineEnd);
          const comment = '  -- ' + msg.eolComment.trim();
          ta.value = left + comment + right;
        } else {
          // Existing comment -> append if not present
          const codePart    = lineText.slice(0, commentIdx).replace(/[ \\t]+$/, '');
          const commentPart = lineText.slice(commentIdx + 2); // after '--'

          // Split existing comment into tokens using ';' as separator (robust to spaces)
          const sep = ' ; ';
          const tokens = commentPart.split(';').map(t => t.trim()).filter(t => t.length > 0);

          // Only append if not already included (case-insensitive, whitespace-normalized)
          const hasAlready = tokens.some(t => norm(t) === norm(msg.eolComment));
          const newTokens = hasAlready ? tokens : tokens.concat([msg.eolComment.trim()]);

          const newLine = codePart + '  -- ' + newTokens.join(sep);

          // Rebuild textarea value
          ta.value = val.slice(0, lineStart) + newLine + val.slice(lineEnd);
        }
      }

      // Keep caret after inserted SQL snippet
      ta.setSelectionRange(caret, caret);
      ta.dispatchEvent(new Event('input', { bubbles: true }));
    });
  "
      ))
    ),

    uiOutput(ns("banner")),
    page_sidebar(
      sidebar = sidebar(
        title = NULL,
        position = "right",
        width = "50%",
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),

        selectizeInput(
          ns("publisher"),
          "Publisher",
          choices = NULL,
          width = "100%",
          multiple = TRUE,
          options = list(
            maxItems = 1,
            create = TRUE,
            placeholder = "Select publisher or type to add new"
          )
        ) |>
          tooltip(
            "Select the publisher of the guideline, i.e. CCME or EPA. You can create new publishers by typing a name and pressing enter."
          ),
        selectizeInput(
          ns("series"),
          "Guideline series",
          choices = NULL,
          width = "100%",
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = "Optional - type to add new",
            create = TRUE
          )
        ) |>
          tooltip(
            "If the guideline is part of a series of related guidelines (i.e. CCME Water Quality Guidelines for the Protection of Aquatic Life), select the series here. You can create new series by typing a name and pressing enter."
          ),
        textInput(ns("guideline_name"), "Guideline Name", width = "100%"),
        textInput(
          ns("reference"),
          "Reference",
          placeholder = "URL, DOI, etc.",
          width = "100%"
        ),
        textAreaInput(
          ns("note"),
          "Note",
          placeholder = "Optional",
          width = "100%",
          height = "100px"
        ),
        selectizeInput(
          ns("parameter_id"),
          "Parameter",
          choices = NULL,
          width = "100%",
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("sample_fraction"),
          "Guideline Fractions",
          choices = NULL,
          width = "100%",
          multiple = TRUE,
          options = list(
            placeholder = "Optional - select one or more fractions"
          )
        ) |>
          tooltip(
            "Select one or more fractions that this guideline applies to (e.g., dissolved, total)."
          ),
        selectizeInput(
          ns("media_type"),
          "Guideline Media Types",
          choices = NULL,
          width = "100%",
          multiple = TRUE,
          options = list(
            placeholder = "Optional - select one or more media types"
          )
        ) |>
          tooltip(
            "Select one or more media types that this guideline applies to (e.g., freshwater, saltwater)."
          ),
        selectizeInput(
          ns("result_speciation"),
          "Result Speciation",
          choices = NULL,
          width = "100%",
          multiple = TRUE,
          options = list(maxItems = 1)
        ) |>
          tooltip(
            "Only required for some parameters, e.g. hardness as CaCO3. If visible after selecting a parameter, it's required."
          ),
        actionLink(
          ns("open_guideline_help"),
          label = "Guideline SQL help",
          icon = icon("book")
        ),
        fluidRow(
          column(
            width = 9,
            textAreaInput(
              ns("guideline_sql"),
              "Guideline SQL",
              width = "100%",
              height = "400px"
            ) |>
              tagAppendAttributes(spellcheck = "false") |>
              tooltip(
                "The SQL code that defines how the guideline value is calculated. See the help file for details and examples."
              ),
          ),
          column(
            width = 3,
            div(
              style = "display: flex; flex-direction: column; gap: 10px; margin-top: 10px;",
              selectizeInput(
                ns("templates"),
                "Insert template",
                choices = stats::setNames(
                  c("fixed", "calc", "calc_hard"),
                  c(
                    "Fixed guideline",
                    "Calculated guideline no hardness",
                    "Calculated guideline with hardness"
                  )
                ),
                multiple = TRUE,
                options = list(maxItems = 1),
                width = "100%"
              ) |>
                tooltip("Insert a template for common guideline SQL patterns."),
              actionButton(
                ns("insert_parameter"),
                "Insert parameter",
                width = "100%"
              ) |>
                tooltip(
                  "Select a parameter_id interactively and insert it where the cursor is in the SQL box."
                ),
              actionButton(
                ns("insert_fraction"),
                "Insert sample fraction",
                width = "100%"
              ) |>
                tooltip(
                  "Select a sample_fraction_id interactively and insert it where the cursor is in the SQL box."
                ),
              actionButton(
                ns("insert_speciation"),
                "Insert result speciation",
                width = "100%"
              ) |>
                tooltip(
                  "Select a result_speciation_id interactively and insert it where the cursor is in the SQL box."
                )
            )
          )
        ),
        actionButton(
          ns("save_guideline"),
          "Update/save guideline",
          width = "100%",
          style = "font-size: 14px;",
          class = "btn btn-primary"
        ) |>
          tooltip(
            "Save changes to the selected guideline. This will update the database if successful, otherwise an error message will be shown to help you fix the issue."
          ),
        actionButton(
          ns("test_guideline"),
          "Test guideline",
          width = "100%",
          style = "font-size: 14px;",
          class = "btn btn-primary"
        ) |>
          tooltip(
            "Test the guideline SQL by providing temporary sample results."
          )
      ),

      div(
        # Rendered within a div so that the buttons are close to the table
        DT::DTOutput(ns("guidelines_table")),
        # Small space between table and button
        br(),
        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(
            ns("add_guideline"),
            "Add new guideline",
            style = "font-size: 14px;",
            class = "btn btn-primary"
          ),
          actionButton(
            ns("delete_guideline"),
            "Delete selected guideline",
            style = "font-size: 14px;",
            class = "btn btn-primary"
          )
        )
      )
    )
  )
}

addGuidelines <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ensure_character <- function(x) {
      if (is.null(x)) {
        character(0)
      } else {
        as.character(x)
      }
    }

    parse_id_csv <- function(x) {
      vals <- ensure_character(x)
      vals <- vals[nzchar(vals)]
      if (!length(vals)) {
        return(integer(0))
      }
      parts <- trimws(unlist(strsplit(vals[1], ",")))
      parts <- parts[nzchar(parts)]
      out <- suppressWarnings(as.integer(parts))
      unique(out[!is.na(out)])
    }

    load_guidelines <- function() {
      DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT g.guideline_id, g.guideline_name, gp.publisher_name AS publisher, gs.series_name AS series, g.note, g.reference, p.param_name AS parameter, p.unit_default AS units, fr.fractions, mt.media_types, rs.result_speciation AS speciation, g.parameter_id, g.result_speciation_id, g.guideline_sql, g.publisher AS publisher_id, g.series AS series_id, fr.fraction_ids, mt.media_ids
FROM discrete.guidelines AS g
LEFT JOIN discrete.guideline_publishers gp ON gp.publisher_id = g.publisher
LEFT JOIN discrete.guideline_series gs ON gs.series_id = g.series
JOIN public.parameters AS p ON p.parameter_id = g.parameter_id
LEFT JOIN discrete.result_speciations rs ON rs.result_speciation_id = g.result_speciation_id
LEFT JOIN (
  SELECT gf.guideline_id,
         string_agg(sf.sample_fraction, ', ' ORDER BY sf.sample_fraction) AS fractions,
         string_agg(gf.fraction_id::text, ',' ORDER BY gf.fraction_id::text) AS fraction_ids
  FROM discrete.guidelines_fractions gf
  JOIN discrete.sample_fractions sf ON sf.sample_fraction_id = gf.fraction_id
  GROUP BY gf.guideline_id
) fr ON fr.guideline_id = g.guideline_id
LEFT JOIN (
  SELECT gm.guideline_id,
         string_agg(mt.media_type, ', ' ORDER BY mt.media_type) AS media_types,
         string_agg(gm.media_id::text, ',' ORDER BY gm.media_id::text) AS media_ids
  FROM discrete.guidelines_media_types gm
  JOIN public.media_types mt ON mt.media_id = gm.media_id
  GROUP BY gm.guideline_id
) mt ON mt.guideline_id = g.guideline_id"
      )
    }

    load_publishers <- function() {
      DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT publisher_id, publisher_name, country, prov_terr_state FROM discrete.guideline_publishers ORDER BY publisher_name"
      )
    }

    load_series <- function() {
      DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT series_id, series_name, series_name_fr, publisher_id FROM discrete.guideline_series ORDER BY series_name"
      )
    }

    load_media_types <- function() {
      DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT media_id, media_type FROM public.media_types ORDER BY media_type"
      )
    }

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addGuidelines"
      )
    })

    moduleData <- reactiveValues(
      guidelines = load_guidelines(),
      publishers = load_publishers(),
      series = load_series(),
      media_types = load_media_types(),
      parameters = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT parameter_id, param_name, unit_default,result_speciation, sample_fraction FROM public.parameters ORDER BY param_name"
      ),
      sample_fractions = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sample_fraction_id, sample_fraction FROM discrete.sample_fractions ORDER BY sample_fraction"
      ),
      result_speciations = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT result_speciation_id, result_speciation FROM discrete.result_speciations ORDER BY result_speciation"
      ),
      test_pairs = NULL
    )

    # Create a temporary copy of the guidelines data to track changes before saving
    moduleData$guidelines_temp <- moduleData$guidelines

    output$guidelines_table <- DT::renderDT(
      {
        DT::datatable(
          moduleData$guidelines,
          selection = "single",
          rownames = FALSE,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            initComplete = htmlwidgets::JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({",
              "  'background-color': '#079',",
              "  'color': '#fff',",
              "  'font-size': '100%',",
              "});",
              "$(this.api().table().body()).css({",
              "  'font-size': '90%',",
              "});",
              "}"
            ),
            layout = list(
              bottomStart = 'info',
              bottomEnd = 'paging'
            ),
            columnDefs = list(
              list(
                targets = c(0, 11:17), # guideline_id and id/sql columns; index starts at 0
                visible = FALSE
              )
            ),
            filter = 'top'
          )
        )
      },
      server = TRUE
    )

    # Update parameter and sample fraction choices when moduleData changes
    observeEvent(
      list(
        moduleData$parameters,
        moduleData$publishers,
        moduleData$series,
        moduleData$media_types,
        moduleData$sample_fractions,
        moduleData$result_speciations
      ),
      {
        selected_parameter <- ensure_character(input$parameter_id)
        selected_publisher <- ensure_character(input$publisher)
        selected_series <- ensure_character(input$series)
        selected_media <- ensure_character(input$media_type)
        selected_fraction <- ensure_character(input$sample_fraction)
        selected_speciation <- ensure_character(input$result_speciation)

        updateSelectizeInput(
          session,
          "parameter_id",
          choices = stats::setNames(
            moduleData$parameters$parameter_id,
            paste0(
              moduleData$parameters$param_name,
              " (",
              moduleData$parameters$unit_default,
              ")"
            )
          ),
          selected = selected_parameter
        )
        updateSelectizeInput(
          session,
          "publisher",
          choices = stats::setNames(
            moduleData$publishers$publisher_id,
            moduleData$publishers$publisher_name
          ),
          selected = selected_publisher
        )
        updateSelectizeInput(
          session,
          "series",
          choices = stats::setNames(
            moduleData$series$series_id,
            moduleData$series$series_name
          ),
          selected = selected_series
        )
        updateSelectizeInput(
          session,
          "media_type",
          choices = stats::setNames(
            moduleData$media_types$media_id,
            moduleData$media_types$media_type
          ),
          selected = selected_media
        )
        updateSelectizeInput(
          session,
          "sample_fraction",
          choices = stats::setNames(
            moduleData$sample_fractions$sample_fraction_id,
            moduleData$sample_fractions$sample_fraction
          ),
          selected = selected_fraction
        )
        updateSelectizeInput(
          session,
          "result_speciation",
          choices = stats::setNames(
            moduleData$result_speciations$result_speciation_id,
            moduleData$result_speciations$result_speciation
          ),
          selected = selected_speciation
        )
      }
    )

    # Show/hide sample_fraction and result_speciation if they're required for the selected parameter
    observeEvent(list(input$parameter_id), {
      req(input$parameter_id)
      param <- moduleData$parameters[
        moduleData$parameters$parameter_id == input$parameter_id,
      ]
      if (param$result_speciation) {
        shinyjs::show("result_speciation")
      } else {
        shinyjs::hide("result_speciation")
        updateSelectizeInput(session, "result_speciation", selected = NULL)
      }
    })

    # Show the user help file as a new html document:
    observeEvent(input$open_guideline_help, {
      # opens in a new tab
      shinyjs::runjs("window.open('html/guidelines_help.html', '_blank');")
    })

    # Clicking 'Add Guideline' creates a new row in the data.frame, which in turn updates the datatable
    observeEvent(input$add_guideline, {
      new <- data.frame(
        guideline_id = -1, # Identifies the newly added row
        guideline_name = "<new guideline>",
        publisher = "<select or create publisher>",
        series = "<optional series>",
        fractions = "<optional fractions>",
        media_types = "<optional media>",
        reference = NA_character_,
        note = NA_character_,
        parameter = NA_character_,
        units = NA_character_,
        speciation = NA_character_,
        parameter_id = NA,
        result_speciation_id = NA,
        guideline_sql = NA_character_,
        publisher_id = NA,
        series_id = NA,
        fraction_ids = NA_character_,
        media_ids = NA_character_
      )
      moduleData$guidelines <- rbind(moduleData$guidelines, new) # Appended here so that the data.table updates
      moduleData$guidelines_temp <- rbind(moduleData$guidelines_temp, new)

      # Select the new row in the data.table
      new_row <- nrow(moduleData$guidelines)
      rows_per_page <- 10
      target_page <- ceiling(new_row / rows_per_page)
      proxy <- DT::dataTableProxy("guidelines_table")
      DT::selectPage(proxy, target_page)
      DT::selectRows(proxy, new_row)

      # Show the user a modal telling them to edit their new guideline to the right
      showModal(modalDialog(
        "Edit your new guideline in the form to the right, then click 'Save Guideline' to save it to the database.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      ))
    })

    # Respond to row selections in the datatable and update the left sidebar inputs accordingly
    observeEvent(input$guidelines_table_rows_selected, {
      selected_row <- input$guidelines_table_rows_selected
      if (is.null(selected_row) || !length(selected_row)) {
        updateTextInput(session, "guideline_name", value = "")
        updateSelectizeInput(session, "publisher", selected = NULL)
        updateSelectizeInput(session, "series", selected = NULL)
        updateSelectizeInput(session, "sample_fraction", selected = NULL)
        updateSelectizeInput(session, "media_type", selected = NULL)
        updateTextInput(session, "reference", value = "")
        updateTextAreaInput(session, "note", value = "")
        updateSelectizeInput(session, "parameter_id", selected = NULL)
        updateSelectizeInput(session, "result_speciation", selected = NULL)
        updateTextAreaInput(session, "guideline_sql", value = "")
        shinyjs::hide("save_guideline")
        return()
      }
      guideline <- moduleData$guidelines[selected_row, ]

      updateTextInput(
        session,
        "guideline_name",
        value = guideline$guideline_name
      )
      updateSelectizeInput(
        session,
        "publisher",
        selected = guideline$publisher_id
      )
      updateSelectizeInput(
        session,
        "series",
        selected = guideline$series_id
      )
      updateSelectizeInput(
        session,
        "sample_fraction",
        selected = parse_id_csv(guideline$fraction_ids)
      )
      updateSelectizeInput(
        session,
        "media_type",
        selected = parse_id_csv(guideline$media_ids)
      )
      updateTextInput(session, "reference", value = guideline$reference)
      updateTextAreaInput(session, "note", value = guideline$note)
      updateSelectizeInput(
        session,
        "parameter_id",
        selected = guideline$parameter_id
      )
      updateSelectizeInput(
        session,
        "result_speciation",
        selected = guideline$result_speciation_id
      )
      updateTextAreaInput(
        session,
        "guideline_sql",
        value = guideline$guideline_sql
      )

      shinyjs::show("save_guideline")
    }, ignoreNULL = FALSE)

    # Observe changes in the input fields and update the reactiveValues data.frame accordingly
    observe({
      req(input$guidelines_table_rows_selected)
      selected_row <- input$guidelines_table_rows_selected

      moduleData$guidelines_temp[selected_row, "guideline_name"] <- if (
        nchar(input$guideline_name) == 0
      ) {
        NA
      } else {
        input$guideline_name
      }
      moduleData$guidelines_temp[selected_row, "publisher_id"] <- if (
        is.null(input$publisher)
      ) {
        NA
      } else {
        as.integer(input$publisher)
      }
      moduleData$guidelines_temp[selected_row, "series_id"] <- if (
        is.null(input$series)
      ) {
        NA
      } else {
        as.integer(input$series)
      }
      moduleData$guidelines_temp[selected_row, "fraction_ids"] <- if (
        is.null(input$sample_fraction) || !length(input$sample_fraction)
      ) {
        NA_character_
      } else {
        paste(as.integer(input$sample_fraction), collapse = ",")
      }
      moduleData$guidelines_temp[selected_row, "media_ids"] <- if (
        is.null(input$media_type) || !length(input$media_type)
      ) {
        NA_character_
      } else {
        paste(as.integer(input$media_type), collapse = ",")
      }
      moduleData$guidelines_temp[selected_row, "reference"] <- if (
        nchar(input$reference) == 0
      ) {
        NA
      } else {
        input$reference
      }
      moduleData$guidelines_temp[selected_row, "note"] <- if (
        nchar(input$note) == 0
      ) {
        NA
      } else {
        input$note
      }
      moduleData$guidelines_temp[selected_row, "parameter_id"] <- if (is.null(input$parameter_id)) NA else as.integer(input$parameter_id)
      moduleData$guidelines_temp[selected_row, "result_speciation_id"] <- if (
        is.null(input$result_speciation)
      ) {
        NA
      } else {
        as.integer(input$result_speciation)
      }
      moduleData$guidelines_temp[selected_row, "guideline_sql"] <- if (
        nchar(input$guideline_sql) == 0
      ) {
        NA
      } else {
        input$guideline_sql
      }

      # Keep display column in sync for datatable
      moduleData$guidelines_temp[selected_row, "publisher"] <- if (
        is.null(moduleData$guidelines_temp[selected_row, "publisher_id"]) ||
          is.na(moduleData$guidelines_temp[selected_row, "publisher_id"])
      ) {
        NA_character_
      } else {
        pid <- moduleData$guidelines_temp[selected_row, "publisher_id"]
        moduleData$publishers$publisher_name[
          match(pid, moduleData$publishers$publisher_id)
        ]
      }
      moduleData$guidelines_temp[selected_row, "series"] <- if (
        is.null(moduleData$guidelines_temp[selected_row, "series_id"]) ||
          is.na(moduleData$guidelines_temp[selected_row, "series_id"])
      ) {
        NA_character_
      } else {
        sid <- moduleData$guidelines_temp[selected_row, "series_id"]
        moduleData$series$series_name[
          match(sid, moduleData$series$series_id)
        ]
      }
      moduleData$guidelines_temp[selected_row, "fractions"] <- if (
        is.null(moduleData$guidelines_temp[selected_row, "fraction_ids"]) ||
          is.na(moduleData$guidelines_temp[selected_row, "fraction_ids"]) ||
          moduleData$guidelines_temp[selected_row, "fraction_ids"] == ""
      ) {
        NA_character_
      } else {
        fids <- parse_id_csv(moduleData$guidelines_temp[selected_row, "fraction_ids"])
        paste(
          moduleData$sample_fractions$sample_fraction[
            match(fids, moduleData$sample_fractions$sample_fraction_id)
          ],
          collapse = ", "
        )
      }
      moduleData$guidelines_temp[selected_row, "media_types"] <- if (
        is.null(moduleData$guidelines_temp[selected_row, "media_ids"]) ||
          is.na(moduleData$guidelines_temp[selected_row, "media_ids"]) ||
          moduleData$guidelines_temp[selected_row, "media_ids"] == ""
      ) {
        NA_character_
      } else {
        mids <- parse_id_csv(moduleData$guidelines_temp[selected_row, "media_ids"])
        paste(
          moduleData$media_types$media_type[
            match(mids, moduleData$media_types$media_id)
          ],
          collapse = ", "
        )
      }
    })

    pending_publisher_selection <- reactiveVal(character(0))
    pending_publisher_new <- reactiveVal(NULL)

    # Allow creating new guideline publishers directly from selectize
    observeEvent(
      input$publisher,
      {
        vals <- ensure_character(input$publisher)
        pending_publisher_selection(vals)

        existing_ids <- ensure_character(moduleData$publishers$publisher_id)
        new_vals <- setdiff(vals, existing_ids)
        new_vals <- new_vals[nzchar(new_vals)]

        if (!length(new_vals)) {
          pending_publisher_new(NULL)
          return()
        }

        new_val <- trimws(new_vals[length(new_vals)])
        if (!nzchar(new_val)) {
          pending_publisher_new(NULL)
          return()
        }

        existing_match <- moduleData$publishers$publisher_id[
          tolower(moduleData$publishers$publisher_name) == tolower(new_val)
        ]
        if (length(existing_match)) {
          updateSelectizeInput(
            session,
            "publisher",
            selected = as.character(existing_match[1])
          )
          pending_publisher_selection(as.character(existing_match[1]))
          pending_publisher_new(NULL)
          return()
        }

        pending_publisher_new(new_val)

        showModal(modalDialog(
          title = "Add new publisher",
          textInput(
            ns("publisher_name"),
            "Publisher name",
            value = new_val
          ),
          textInput(
            ns("publisher_country"),
            "Country (optional)"
          ),
          textInput(
            ns("publisher_prov_terr_state"),
            "Province/Territory/State (optional)"
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("add_publisher"),
              "Add publisher",
              class = "btn-primary"
            )
          )
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_publisher,
      {
        if (!isTruthy(input$publisher_name)) {
          shinyjs::js$backgroundCol(ns("publisher_name"), "#fdd")
          return()
        }

        p_name <- trimws(input$publisher_name)
        p_country <- trimws(input$publisher_country)
        p_state <- trimws(input$publisher_prov_terr_state)

        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO discrete.guideline_publishers (publisher_name, country, prov_terr_state) VALUES ($1, $2, $3) ON CONFLICT (publisher_name) DO NOTHING",
          params = list(
            p_name,
            if (nzchar(p_country)) p_country else NA_character_,
            if (nzchar(p_state)) p_state else NA_character_
          )
        )

        moduleData$publishers <- load_publishers()
        new_id <- moduleData$publishers$publisher_id[
          moduleData$publishers$publisher_name == p_name
        ]
        if (!length(new_id)) {
          new_id <- moduleData$publishers$publisher_id[
            tolower(moduleData$publishers$publisher_name) == tolower(p_name)
          ]
        }
        selected_values <- ensure_character(new_id[1])

        updateSelectizeInput(
          session,
          "publisher",
          choices = stats::setNames(
            moduleData$publishers$publisher_id,
            moduleData$publishers$publisher_name
          ),
          selected = selected_values
        )

        pending_publisher_selection(selected_values)
        pending_publisher_new(NULL)
        removeModal()
        showModal(modalDialog(
          "New publisher added.",
          easyClose = TRUE,
          footer = tagList(modalButton("Close"))
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    pending_series_selection <- reactiveVal(character(0))
    pending_series_new <- reactiveVal(NULL)

    # Allow creating new guideline series directly from selectize
    observeEvent(
      input$series,
      {
        vals <- ensure_character(input$series)
        pending_series_selection(vals)

        existing_ids <- ensure_character(moduleData$series$series_id)
        new_vals <- setdiff(vals, existing_ids)
        new_vals <- new_vals[nzchar(new_vals)]

        if (!length(new_vals)) {
          pending_series_new(NULL)
          return()
        }

        new_val <- trimws(new_vals[length(new_vals)])
        if (!nzchar(new_val)) {
          pending_series_new(NULL)
          return()
        }

        existing_match <- moduleData$series$series_id[
          tolower(moduleData$series$series_name) == tolower(new_val)
        ]
        if (length(existing_match)) {
          updateSelectizeInput(
            session,
            "series",
            selected = as.character(existing_match[1])
          )
          pending_series_selection(as.character(existing_match[1]))
          pending_series_new(NULL)
          return()
        }

        preselected_publisher <- ensure_character(input$publisher)
        pending_series_new(new_val)

        showModal(modalDialog(
          title = "Add new guideline series",
          textInput(
            ns("series_name"),
            "Series name",
            value = new_val
          ),
          textInput(
            ns("series_name_fr"),
            "Series name (French, optional)"
          ),
          selectizeInput(
            ns("series_publisher_id"),
            "Publisher",
            choices = stats::setNames(
              moduleData$publishers$publisher_id,
              moduleData$publishers$publisher_name
            ),
            selected = if (length(preselected_publisher)) preselected_publisher else NULL,
            multiple = TRUE,
            options = list(maxItems = 1)
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_series"), "Add series", class = "btn-primary")
          )
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_series,
      {
        if (!isTruthy(input$series_name)) {
          shinyjs::js$backgroundCol(ns("series_name"), "#fdd")
          return()
        }
        if (!isTruthy(input$series_publisher_id)) {
          shinyjs::js$backgroundCol(ns("series_publisher_id"), "#fdd")
          return()
        }

        s_name <- trimws(input$series_name)
        s_name_fr <- trimws(input$series_name_fr)
        s_pub <- as.integer(input$series_publisher_id)

        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO discrete.guideline_series (series_name, series_name_fr, publisher_id) VALUES ($1, $2, $3) ON CONFLICT (series_name) DO NOTHING",
          params = list(
            s_name,
            if (nzchar(s_name_fr)) s_name_fr else NA_character_,
            s_pub
          )
        )

        moduleData$series <- load_series()
        new_id <- moduleData$series$series_id[
          moduleData$series$series_name == s_name
        ]
        if (!length(new_id)) {
          new_id <- moduleData$series$series_id[
            tolower(moduleData$series$series_name) == tolower(s_name)
          ]
        }
        selected_values <- ensure_character(new_id[1])

        updateSelectizeInput(
          session,
          "series",
          choices = stats::setNames(
            moduleData$series$series_id,
            moduleData$series$series_name
          ),
          selected = selected_values
        )

        pending_series_selection(selected_values)
        pending_series_new(NULL)
        removeModal()
        showModal(modalDialog(
          "New guideline series added.",
          easyClose = TRUE,
          footer = tagList(modalButton("Close"))
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # ObserveEvents for when the user inserts parameter_id = X or other via modals.
    # helper to show a modal with selectize and a confirm button
    showPickModal <- function(id_pick, title, choices_named, id_confirm) {
      showModal(modalDialog(
        size = "m",
        title = title,
        selectizeInput(
          ns(id_pick),
          NULL,
          choices = choices_named,
          options = list(placeholder = "Type to search...")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(id_confirm), "Insert", class = "btn-primary")
        )
      ))
    }

    # ----- Insert PARAMETER -----
    observeEvent(input$insert_parameter, ignoreInit = TRUE, {
      # choices: names = param_name (shown), values = parameter_id (used)
      ch <- stats::setNames(
        moduleData$parameters$parameter_id,
        paste0(
          moduleData$parameters$param_name,
          " (",
          moduleData$parameters$unit_default,
          ")"
        )
      )
      showPickModal("pick_param", "Choose a parameter", ch, "confirm_param")
    })

    # ----- Insert PARAMETER -----
    observeEvent(input$confirm_param, ignoreInit = TRUE, {
      req(input$pick_param)
      removeModal()

      # Look up name/units for the chosen parameter
      i <- match(input$pick_param, moduleData$parameters$parameter_id)
      p_name <- moduleData$parameters$param_name[i]
      p_units <- moduleData$parameters$unit_default[i]
      label <- if (!is.na(p_units) && nzchar(p_units)) {
        paste0("parameter: ", p_name, " [", p_units, "]")
      } else {
        paste0("parameter: ", p_name)
      }

      session$sendCustomMessage(
        "insertAtCursor",
        list(
          target = ns("guideline_sql"),
          text = paste0("parameter_id := ", input$pick_param),
          eolComment = label
        )
      )
    })

    # ----- Insert SAMPLE FRACTION -----
    observeEvent(input$insert_fraction, ignoreInit = TRUE, {
      ch <- stats::setNames(
        moduleData$sample_fractions$sample_fraction_id,
        moduleData$sample_fractions$sample_fraction
      )
      showPickModal("pick_frac", "Choose a sample fraction", ch, "confirm_frac")
    })

    observeEvent(input$confirm_frac, ignoreInit = TRUE, {
      req(input$pick_frac)
      removeModal()

      i <- match(
        input$pick_frac,
        moduleData$sample_fractions$sample_fraction_id
      )
      f_name <- moduleData$sample_fractions$sample_fraction[i]
      label <- paste0("fraction: ", f_name)

      session$sendCustomMessage(
        "insertAtCursor",
        list(
          target = ns("guideline_sql"),
          text = paste0("sample_fraction_id := ", input$pick_frac),
          eolComment = label
        )
      )
    })

    # ----- Insert RESULT SPECIATION -----
    observeEvent(input$insert_speciation, ignoreInit = TRUE, {
      ch <- stats::setNames(
        moduleData$result_speciations$result_speciation_id,
        moduleData$result_speciations$result_speciation
      )
      showPickModal(
        "pick_spec",
        "Choose a result speciation",
        ch,
        "confirm_spec"
      )
    })

    observeEvent(input$confirm_spec, ignoreInit = TRUE, {
      req(input$pick_spec)
      removeModal()

      i <- match(
        input$pick_spec,
        moduleData$result_speciations$result_speciation_id
      )
      s_name <- moduleData$result_speciations$result_speciation[i]
      label <- paste0("speciation: ", s_name)

      session$sendCustomMessage(
        "insertAtCursor",
        list(
          target = ns("guideline_sql"),
          text = paste0("result_speciation_id := ", input$pick_spec),
          eolComment = label
        )
      )
    })

    # ----- Insert TEMPLATE -----
    observeEvent(input$templates, ignoreInit = TRUE, {
      showModal(modalDialog(
        "Inserting a template will overwrite any existing text in the SQL box. You can then edit the template as needed.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_template"),
            "Insert template",
            class = "btn-primary"
          )
        )
      ))
    })

    observeEvent(input$confirm_template, {
      removeModal()
      req(input$templates)
      if (input$templates == "fixed") {
        updateTextAreaInput(
          session,
          "guideline_sql",
          value = "-- Replace ***!value!*** with the fixed guideline value in the database's units for your target parameter
SELECT ***!value!***::INT"
        )
        showModal(modalDialog(
          "Template inserted with placeholder denoted by ***!value!***. Replace this with the fixed guideline value in the database's units for your target parameter.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else if (input$templates == "calc") {
        updateTextAreaInput(
          session,
          "guideline_sql",
          value = "WITH vals AS (  -- create a CTE of relevant parameters
  SELECT 
  discrete.get_sample_val(
    sample_id := $1::INT, -- sample_id placeholder, leave
    ***!parameter_id := xxx::INT!***,
    ***!sample_fraction_id := xxx::INT!***, -- remove if not required
    ***!result_speciation_id := xxx::INT!*** --remove if not required
  ) AS v1 -- change 'v1' (optional)
      
  -- repeat get_sample_val for other parameters if needed
  -- delete block if not needed
  discrete.get_sample_val(
    sample_id := $1::INT, -- sample_id placeholder, leave
    ***!parameter_id := xxx::INT!***,
    ***!sample_fraction_id := xxx::INT!***, -- remove if not required
    ***!result_speciation_id := xxx::INT!*** --remove if not required
  ) AS v2 -- change 'v2'
)
-- Calculate guideline value
SELECT CASE
-- Replace with your logic
-- use value names from the vals CTE
    WHEN ***! enter condition here !*** THEN
      ***! value or equation !*** END
    WHEN ***! enter second condition here !*** THEN
      ***! value or equation !*** END
    ELSE
      ***! fallback value !***
END
FROM vals -- from the 'vals' CTE"
        )
        showModal(modalDialog(
          "Template inserted with placeholders denoted by ***!some_text!***. Use the buttons to the left to insert parameters_ids and other necessary text and replace these placeholders. Remember to return values in the database's units for your target parameter.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else if (input$templates == "calc_hard") {
        updateTextAreaInput(
          session,
          "guideline_sql",
          value = "WITH vals AS ( -- create a CTE of relevant parameters
  -- special function for calculating hardness
  SELECT 
  discrete.get_sample_hardness($1::INTEGER) -- sample_id placeholder, leave
  AS h  -- value name 'h'
  
  -- add other parameters as needed
  -- delete block if no other params needed
  discrete.get_sample_val(
    sample_id := $1, -- sample_id placeholder, leave
    ***!parameter_id := xxx!***,
    ***!sample_fraction_id := xxx!***, -- remove if not required
    ***!result_speciation_id := xxx!*** --remove if not required
  ) AS v1 -- change 'v1' (optional)
      
  -- repeat for other parameters
)
  -- Calculate guideline value
SELECT CASE
-- Replace with your logic
-- use value names from the vals CTE
  WHEN h IS NULL OR h < ***!value!*** THEN
    ***!value!***::numeric END
  WHEN h <= ***!value!** THEN
    ***!value or equation!***::numeric END
  ELSE
  ***!fallback value!***::numeric
END
FROM vals -- from the 'vals' CTE"
        )
        showModal(modalDialog(
          "Template inserted with placeholders denoted by ***!some_text!***. Use the buttons to the left to insert parameters_ids and other necessary text and replace these placeholders. Remember to return values in the database's units for your target parameter.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    })

    # Reusable functions to help extract parameters from guidelines for testing
    # Parse the SQL and return a single data.frame of required values
    parse_guideline_requirements <- function(sql) {
      s <- sql
      # strip comments
      s <- gsub("(?s)/\\*.*?\\*/", "", s, perl = TRUE)
      s <- gsub("(?m)--.*$", "", s, perl = TRUE)

      # detect hardness usage
      has_hard <- length(stringr::str_extract_all(
        s,
        "(?:\\bdiscrete\\.)?get_sample_hardness\\s*\\([^)]*\\)"
      )[[1]]) >
        0

      # extract get_sample_val() calls (schema optional)
      call_re <- stringr::regex(
        "(?:\\bdiscrete\\.)?get_sample_val\\s*\\([^)]*\\)",
        ignore_case = TRUE
      )
      calls <- stringr::str_extract_all(s, call_re)[[1]]
      calls <- gsub("[\r\n ]+", "", calls)

      # build rows from calls
      rows <- lapply(calls, function(v) {
        pid <- suppressWarnings(as.integer(stringr::str_match(
          v,
          "parameter_id\\s*[:=]{1,2}\\s*(\\d+)"
        )[, 2]))
        fid <- suppressWarnings(stringr::str_match(
          v,
          "sample_fraction_id\\s*[:=]{1,2}\\s*(\\d+|NULL)"
        )[, 2])
        sid <- suppressWarnings(stringr::str_match(
          v,
          "result_speciation_id\\s*[:=]{1,2}\\s*(\\d+|NULL)"
        )[, 2])
        data.frame(
          parameter_id = ifelse(is.na(pid), NA_integer_, pid),
          sample_fraction_id = ifelse(
            is.na(fid) | fid == "NULL",
            NA_integer_,
            as.integer(fid)
          ),
          result_speciation_id = ifelse(
            is.na(sid) | sid == "NULL",
            NA_integer_,
            as.integer(sid)
          ),
          stringsAsFactors = FALSE
        )
      })

      df_vals <- if (length(rows)) {
        do.call(rbind, rows)
      } else {
        data.frame(
          parameter_id = integer(0),
          sample_fraction_id = integer(0),
          result_speciation_id = integer(0)
        )
      }

      # prepend hardness triples if referenced
      if (has_hard) {
        df_hard <- data.frame(
          parameter_id = c(1061, 1103, 100, 1061, 1103, 100), # Ca, Mg, Hardness as CaCO3
          sample_fraction_id = c(5, 5, 5, 19, 19, 19), # dissolved=5, total=19? (swap if yours differ)
          result_speciation_id = c(NA, NA, 3, NA, NA, 3)
        )
        df_vals <- rbind(df_hard, df_vals)
      }

      # de-dup
      unique(df_vals)
    }

    # Build numericInputs from a requirements df (no re-parsing)
    make_test_inputs <- function(
      ns,
      req_df,
      parameters_df,
      fractions_df,
      speciations_df
    ) {
      if (!nrow(req_df)) {
        return(NULL)
      }

      pname <- function(pid) {
        parameters_df$param_name[match(pid, parameters_df$parameter_id)]
      }
      units <- function(pid) {
        parameters_df$unit_default[match(pid, parameters_df$parameter_id)]
      }
      fracname <- function(fid) {
        ifelse(
          is.na(fid),
          NA_character_,
          fractions_df$sample_fraction[match(
            fid,
            fractions_df$sample_fraction_id
          )]
        )
      }
      specname <- function(sid) {
        ifelse(
          is.na(sid),
          NA_character_,
          speciations_df$result_speciation[match(
            sid,
            speciations_df$result_speciation_id
          )]
        )
      }

      lapply(seq_len(nrow(req_df)), function(i) {
        pid <- req_df$parameter_id[i]
        fid <- req_df$sample_fraction_id[i]
        sid <- req_df$result_speciation_id[i]

        label <- paste0(
          pname(pid),
          if (!is.na(fid)) paste0(" (", fracname(fid), ")") else "",
          if (!is.na(sid)) paste0(" ", specname(sid)) else "",
          " [",
          units(pid),
          "]"
        )

        inputId <- paste0(
          "test_val_",
          pid,
          "_",
          ifelse(is.na(fid), 0L, fid),
          "_",
          ifelse(is.na(sid), 0L, sid)
        )
        numericInput(ns(inputId), label = label, value = NA_real_)
      })
    }

    # Test a guideline by inserting temporary sample/results and evaluating
    observeEvent(input$test_guideline, {
      req(nzchar(input$guideline_sql))

      if (is.null(input$guidelines_table_rows_selected)) {
        showModal(modalDialog(
          title = "Select or add a guideline first",
          "Please select an existing guideline from the table or add a new one (and make sure it's selected in the table) before testing.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
        return()
      }

      # Parse requirements once and store them
      req_df <- parse_guideline_requirements(input$guideline_sql)
      moduleData$requirements_df <- req_df

      # Build inputs
      inputs_ui <- make_test_inputs(
        ns,
        req_df,
        moduleData$parameters,
        moduleData$sample_fractions,
        moduleData$result_speciations
      )

      removeModal()
      output$test_guideline_result <- renderUI({
        ""
      })

      showModal(modalDialog(
        title = "Test guideline",
        if (nrow(req_df) > 0) {
          "Provide TEMPORARY values for the following parameters to test the guideline"
        } else {
          "This guideline does not reference any parameters."
        },
        if (nrow(req_df) > 0) do.call(tagList, inputs_ui) else NULL,
        actionButton(ns("run_test_guideline"), "Run"),
        uiOutput(ns("test_guideline_result")),
        footer = tagList(modalButton("Close")),
        size = "xl"
      ))
    })

    observeEvent(input$run_test_guideline, {
      result <- NULL
      err <- NULL
      sample_id <- NULL

      req_df <- isolate(moduleData$requirements_df)
      # collect supplied values
      req_df$values <- NA
      if (nrow(req_df) > 0) {
        for (i in 1:nrow(req_df)) {
          pid <- req_df$parameter_id[i]
          fid <- req_df$sample_fraction_id[i]
          sid <- req_df$result_speciation_id[i]
          inputId <- paste0(
            "test_val_",
            pid,
            "_",
            ifelse(is.na(fid), 0L, fid),
            "_",
            ifelse(is.na(sid), 0L, sid)
          )
          val <- input[[inputId]]
          if (is.null(val)) {
            req_df$values[i] <- NA
          } else {
            req_df$values[i] <- val
          }
        }
      }

      # if params required but none provided
      if (nrow(req_df) > 0 && all(is.na(req_df$values))) {
        output$test_guideline_result <- renderUI({
          div(
            style = "color:red;font-weight:bold;font-size:120%;margin-top:10px;",
            "No values provided. Please enter at least one value to test the guideline."
          )
        })
        return()
      }

      # run in a transaction and ROLLBACK
      tryCatch(
        {
          DBI::dbExecute(session$userData$AquaCache, "BEGIN;")
          uses_sample_id <- grepl("\\$1\\b", input$guideline_sql)

          # Only create a temporary sample when needed
          if (uses_sample_id || nrow(req_df) > 0) {
            sample_id <- DBI::dbGetQuery(
              session$userData$AquaCache,
              "
          INSERT INTO discrete.samples (location_id, media_id, datetime, collection_method, sample_type, owner)
          VALUES (100, 1, '1800-01-01 00:00', 1, 1, 2)
          RETURNING sample_id
        "
            )$sample_id
          }

          # Insert supplied results
          for (i in 1:nrow(req_df)) {
            if (is.na(req_df$values[i])) {
              next
            }
            DBI::dbExecute(
              session$userData$AquaCache,
              "INSERT INTO discrete.results
             (sample_id, result_type, parameter_id, sample_fraction_id, result_speciation_id, result)
             VALUES ($1,$2,$3,$4,$5,$6)",
              params = list(
                sample_id,
                2,
                req_df$parameter_id[i],
                req_df$sample_fraction_id[i],
                req_df$result_speciation_id[i],
                req_df$values[i]
              )
            )
          }

          # Evaluate current SQL from the editor directly so testing works before save
          test_sql <- paste0(
            "WITH q AS (",
            input$guideline_sql,
            ") SELECT (SELECT * FROM q)::numeric AS value"
          )
          if (uses_sample_id) {
            result <- DBI::dbGetQuery(
              session$userData$AquaCache,
              test_sql,
              params = list(sample_id)
            )[1, 1]
          } else {
            result <- DBI::dbGetQuery(
              session$userData$AquaCache,
              test_sql
            )[1, 1]
          }

          # rollback
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
        },
        error = function(e) {
          err <<- conditionMessage(e)
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
        },
        warning = function(w) {
          err <<- conditionMessage(w)
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
        }
      )

      # Show output
      if (!is.null(err)) {
        output$test_guideline_result <- renderUI({
          div(
            style = "color:red;font-weight:bold;font-size:120%;margin-top:10px;",
            paste("Error:", err)
          )
        })
      } else if (is.null(result) || is.na(result)) {
        output$test_guideline_result <- renderUI({
          div(
            style = "color:orange;font-weight:bold;font-size:120%;margin-top:10px;",
            "The guideline returned NULL or NA."
          )
        })
      } else {
        output$test_guideline_result <- renderUI({
          div(
            style = "color:green;font-weight:bold;font-size:120%;margin-top:10px;",
            paste("Returned guideline value:", result)
          )
        })
      }
    })

    # Save changes to the database when 'Save Guideline' is clicked
    observeEvent(input$save_guideline, {
      req(input$guidelines_table_rows_selected)
      selected_row <- input$guidelines_table_rows_selected
      guideline <- moduleData$guidelines_temp[selected_row, ]

      # Basic validation
      if (
        is.na(guideline$guideline_name) ||
          is.na(guideline$parameter_id) ||
          is.na(guideline$publisher_id) ||
          is.na(guideline$guideline_sql)
      ) {
        showModal(modalDialog(
          title = "Error",
          "Please fill in all required fields: Guideline Name, Publisher, Parameter, and Guideline SQL.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
        return()
      }

      # Match DB-side trigger rules with clearer UI feedback
      param_row <- moduleData$parameters[
        moduleData$parameters$parameter_id == guideline$parameter_id,
      ]
      if (nrow(param_row) == 1) {
        if (
          isTRUE(param_row$result_speciation) &&
            is.na(guideline$result_speciation_id)
        ) {
          showModal(modalDialog(
            title = "Error",
            "This parameter requires a Result Speciation. Please select one before saving.",
            easyClose = TRUE,
            footer = tagList(modalButton("Close"))
          ))
          return()
        }
      }

      tryCatch(
        {
          rs <- if (
            is.null(guideline$result_speciation_id) ||
              is.na(guideline$result_speciation_id) ||
              guideline$result_speciation_id == ""
          ) {
            NA_integer_
          } else {
            as.integer(guideline$result_speciation_id)
          }
          ref <- if (
            is.null(guideline$reference) ||
              is.na(guideline$reference) ||
              guideline$reference == ""
          ) {
            NA_character_
          } else {
            guideline$reference
          }
          fraction_ids <- parse_id_csv(guideline$fraction_ids)
          media_ids <- parse_id_csv(guideline$media_ids)
          guideline_id <- guideline$guideline_id

          if (guideline$guideline_id == -1) {
            # New guideline - perform INSERT
            guideline_id <- DBI::dbGetQuery(
              session$userData$AquaCache,
              "INSERT INTO discrete.guidelines (guideline_name, publisher, series, reference, note, parameter_id, result_speciation_id, guideline_sql) VALUES ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING guideline_id",
              params = list(
                guideline$guideline_name,
                guideline$publisher_id,
                guideline$series_id,
                ref,
                guideline$note,
                guideline$parameter_id,
                rs,
                guideline$guideline_sql
              )
            )$guideline_id[1]
          } else {
            # Existing guideline - perform UPDATE
            DBI::dbExecute(
              session$userData$AquaCache,
              "UPDATE discrete.guidelines SET guideline_name = $1, publisher = $2, series = $3, reference = $4, note = $5, parameter_id = $6, result_speciation_id = $7, guideline_sql = $8 WHERE guideline_id = $9",
              params = list(
                guideline$guideline_name,
                guideline$publisher_id,
                guideline$series_id,
                ref,
                guideline$note,
                guideline$parameter_id,
                rs,
                guideline$guideline_sql,
                guideline$guideline_id
              )
            )
          }

          # Update many-to-many links for guideline fractions
          DBI::dbExecute(
            session$userData$AquaCache,
            "DELETE FROM discrete.guidelines_fractions WHERE guideline_id = $1",
            params = list(guideline_id)
          )
          if (length(fraction_ids)) {
            for (i in seq_along(fraction_ids)) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "INSERT INTO discrete.guidelines_fractions (guideline_id, fraction_id) VALUES ($1, $2)",
                params = list(guideline_id, fraction_ids[i])
              )
            }
          }

          # Update many-to-many links for guideline media types
          DBI::dbExecute(
            session$userData$AquaCache,
            "DELETE FROM discrete.guidelines_media_types WHERE guideline_id = $1",
            params = list(guideline_id)
          )
          if (length(media_ids)) {
            for (i in seq_along(media_ids)) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "INSERT INTO discrete.guidelines_media_types (guideline_id, media_id) VALUES ($1, $2)",
                params = list(guideline_id, media_ids[i])
              )
            }
          }

          # Refresh the guidelines data from the database
          moduleData$guidelines <- load_guidelines()
          moduleData$guidelines_temp <- moduleData$guidelines

          showModal(modalDialog(
            title = "Success",
            "Guideline saved successfully.",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
          ))
        },
        error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred while saving the guideline: ", e$message),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
          ))
        }
      )
    })

    # Observe a request to delete a guideline
    observeEvent(input$delete_guideline, {
      req(input$guidelines_table_rows_selected)

      guideline <- moduleData$guidelines[input$guidelines_table_rows_selected, ]

      showModal(modalDialog(
        title = "Confirm Deletion",
        paste(
          "Are you sure you want to delete the guideline '",
          guideline$guideline_name,
          "'? This action cannot be undone.",
          sep = ""
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete"), "Delete", class = "btn btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_delete, {
      removeModal()

      guideline <- moduleData$guidelines[input$guidelines_table_rows_selected, ]

      tryCatch(
        {
          if (guideline$guideline_id != -1) {
            # Only attempt to delete if it's not a new unsaved guideline
            DBI::dbExecute(
              session$userData$AquaCache,
              "DELETE FROM discrete.guidelines WHERE guideline_id = $1",
              params = list(guideline$guideline_id)
            )
          }

          # Refresh the data, which will re-render the datatable without the deleted row
          moduleData$guidelines <- load_guidelines()
          moduleData$guidelines_temp <- moduleData$guidelines

          # Clear table selection and hide save button
          DT::dataTableProxy("guidelines_table") |>
            DT::selectRows(NULL)
          shinyjs::hide("save_guideline")

          # Reset inputs
          updateTextInput(session, "guideline_name", value = "")
          updateSelectizeInput(session, "publisher", selected = NULL)
          updateSelectizeInput(session, "series", selected = NULL)
          updateSelectizeInput(session, "media_type", selected = NULL)
          updateTextInput(session, "reference", value = "")
          updateTextAreaInput(session, "note", value = "")
          updateSelectizeInput(session, "parameter_id", selected = NULL)
          updateSelectizeInput(session, "sample_fraction", selected = NULL)
          updateSelectizeInput(session, "result_speciation", selected = NULL)
          updateTextAreaInput(session, "guideline_sql", value = "")

          showModal(modalDialog(
            title = "Success",
            "Guideline deleted successfully.",
            footer = tagList(
              modalButton("Close")
            )
          ))
        },
        error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred while deleting the guideline:", e$message),
            footer = tagList(
              modalButton("Close")
            )
          ))
        }
      ) # End of tryCatch
    }) # End of observeEvent for confirm_delete
  }) # End of moduleServer
}
