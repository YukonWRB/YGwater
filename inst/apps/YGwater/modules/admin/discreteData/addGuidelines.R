# UI and server code for water quality guidelines management module

addGuidelinesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$script(HTML("
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
  "))
    ),
    
    page_sidebar(
      sidebar = sidebar(
        title = NULL,
        position = "right",
        width = "50%",
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),
        
        
        textInput(ns("guideline_name"), 
                  "Guideline Name",
                  width = "100%"),
        textInput(ns("publisher"), 
                  "Publisher",
                  placeholder = "CA-CCME, US-EPA, BC-MOE, etc.",
                  width = "100%"),
        textInput(ns("reference"),
                  "Reference",
                  placeholder = "URL, DOI, etc.",
                  width = "100%"),
        textAreaInput(ns("note"), 
                      "Note", 
                      placeholder = "Optional", 
                      width = "100%", 
                      height = "100px"),
        selectizeInput(ns("parameter_id"), 
                       "Parameter", 
                       choices = NULL, 
                       width = "100%", 
                       multiple = TRUE,
                       options = list(maxItems = 1)),
        selectizeInput(ns("sample_fraction"), 
                       "Sample Fraction", 
                       choices = NULL, 
                       width = "100%",
                       multiple = TRUE,
                       options = list(maxItems = 1)) %>%
          tooltip("Required for most parameters."),
        selectizeInput(ns("result_speciation"),
                       "Result Speciation", 
                       choices = NULL, 
                       width = "100%",
                       multiple = TRUE,
                       options = list(maxItems = 1)) %>%
          tooltip("Only required for some parameters, e.g. hardness as CaCO3. Leave blank if not applicable."),
        actionLink(ns("open_guideline_help"), 
                   label = "Guideline SQL help", 
                   icon = icon("book")),
        fluidRow(
          column(
            width = 9,
            textAreaInput(ns("guideline_sql"),
                          "Guideline SQL",
                          width = "100%",
                          height = "300px") %>%
              tooltip("The SQL code that defines how the guideline value is calculated. See the help file for details and examples."),
          ),
          column(
            width = 3,
            div(style = "display: flex; flex-direction: column; gap: 10px; margin-top: 70px;",
                actionButton(ns("insert_parameter"), 
                             "Insert parameter", 
                             width = "100%") %>%
                  tooltip("Select a parameter_id interactively and insert it where the cursor is in the SQL box."),
                actionButton(ns("insert_fraction"), 
                             "Insert sample fraction", 
                             width = "100%") %>%
                  tooltip("Select a sample_fraction_id interactively and inserty it where the cursor is in the SQL box."),
                actionButton(ns("insert_speciation"), 
                             "Insert result speciation", 
                             width = "100%") %>%
                  tooltip("Select a result_speciation_id interactively and inserty it where the cursor is in the SQL box.")
            )
          )
        ),
        actionButton(ns("save_guideline"), 
                     "Update/save guideline", 
                     width = "100%", ) %>%
          tooltip("Save changes to the selected guideline. This will update the database if successful, otherwise an error message will be shown to help you fix the issue."),
        actionButton(ns("test_guideline"),
                     "Test guideline",
                     width = "100%") %>%
          tooltip("Test the guideline SQL by providing temporary sample results.")
      ),
      
      div( # Rendered within a div so that the buttons are close to the table
        DT::DTOutput(ns("guidelines_table")
        ),
        # Small space between table and button
        br(),
        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(ns("add_guideline"), 
                       "Add new guideline",
                       style = "font-size: 14px;", 
                       class = "btn btn-primary"),
          actionButton(ns("delete_guideline"),
                       "Delete selected guideline",
                       style = "font-size: 14px;", 
                       class = "btn btn-primary")
        )
      )
    )
  )
}

addGuidelines <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    moduleData <- reactiveValues(
      guidelines = DBI::dbGetQuery(session$userData$AquaCache, "SELECT g.guideline_id, g.guideline_name, g.publisher, g.note, g.reference, p.param_name AS parameter, p.unit_default AS units, sf.sample_fraction AS fraction, rs.result_speciation AS speciation, g.parameter_id, g.sample_fraction_id, g.result_speciation_id, g.guideline_sql FROM discrete.guidelines as g LEFT JOIN sample_fractions sf ON sf.sample_fraction_id = g.sample_fraction_id JOIN public.parameters as p ON p.parameter_id = g.parameter_id LEFT JOIN result_speciations rs ON rs.result_speciation_id = g.result_speciation_id"),
      parameters = DBI::dbGetQuery(session$userData$AquaCache, "SELECT parameter_id, param_name, unit_default FROM public.parameters ORDER BY param_name"),
      sample_fractions = DBI::dbGetQuery(session$userData$AquaCache, "SELECT sample_fraction_id, sample_fraction FROM discrete.sample_fractions ORDER BY sample_fraction"),
      result_speciations = DBI::dbGetQuery(session$userData$AquaCache, "SELECT result_speciation_id, result_speciation FROM discrete.result_speciations ORDER BY result_speciation"),
      test_pairs = NULL
    )
    
    # Create a temporary copy of the guidelines data to track changes before saving
    moduleData$guidelines_temp <- moduleData$guidelines
    
    output$guidelines_table <- DT::renderDT({
      DT::datatable(moduleData$guidelines, 
                    selection = "single", 
                    rownames = FALSE,
                    options = list(pageLength = 10, 
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
                                     bottomEnd   = 'paging'
                                   ),
                                   columnDefs = list(
                                     list(
                                       targets = c(0,9:12),  # guideline_id and SQL columns; index starts at 0
                                       visible = FALSE
                                     )
                                   )
                    )
      )
    }, server = TRUE)
    
    # Update parameter and sample fraction choices when moduleData changes
    observe({
      updateSelectizeInput(session, "parameter_id", 
                           choices = setNames(moduleData$parameters$parameter_id, 
                                              paste0(moduleData$parameters$param_name,
                                                     " (", 
                                                     moduleData$parameters$unit_default, ")")),
                           selected = NULL)
      updateSelectizeInput(session, "sample_fraction", 
                           choices = setNames(moduleData$sample_fractions$sample_fraction_id, 
                                              moduleData$sample_fractions$sample_fraction),
                           selected = NULL)
      updateSelectizeInput(session, "result_speciation", 
                           choices = setNames(moduleData$result_speciations$result_speciation_id, 
                                              moduleData$result_speciations$result_speciation),
                           selected = NULL)
    })
    
    # Show the user help file as a new html document:
    observeEvent(input$open_guideline_help, {
      # opens in a new tab
      shinyjs::runjs("window.open('html/guidelines_help.html', '_blank');")
    })
    
    # Clicking 'Add Guideline' creates a new row in the data.frame, which in turn updates the datatable
    observeEvent(input$add_guideline, {
      new <- data.frame(
        guideline_id = -1,  # Identifies the newly added row
        guideline_name = "",
        publisher = "",
        reference = "",
        note = "",
        parameter = "",
        units = "",
        fraction = "",
        speciation = "",
        parameter_id = NA,
        sample_fraction_id = NA,
        result_speciation_id = NA,
        guideline_sql = ""
      )
      moduleData$guidelines <- rbind(moduleData$guidelines, new)   # Appended here so that the data.table updates
      moduleData$guidelines_temp <- rbind(moduleData$guidelines_temp, new)
      
      # Select the new row in the data.table
      DT::dataTableProxy("guidelines_table") %>% DT::selectRows(nrow(moduleData$guidelines))
      
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
      req(input$guidelines_table_rows_selected)
      selected_row <- input$guidelines_table_rows_selected
      guideline <- moduleData$guidelines[selected_row, ]
      
      updateTextInput(session, "guideline_name", value = guideline$guideline_name)
      updateTextInput(session, "publisher", value = guideline$publisher)
      updateTextInput(session, "reference", value = guideline$reference)
      updateTextAreaInput(session, "note", value = guideline$note)
      updateSelectizeInput(session, "parameter_id", selected = guideline$parameter_id)
      updateSelectizeInput(session, "sample_fraction", selected = guideline$sample_fraction_id)
      updateSelectizeInput(session, "result_speciation", selected = guideline$result_speciation_id)
      updateTextAreaInput(session, "guideline_sql", value = guideline$guideline_sql)
      
      shinyjs::show("save_guideline")
    })
    
    # Observe changes in the input fields and update the reactiveValues data.frame accordingly
    observe({
      req(input$guidelines_table_rows_selected)
      selected_row <- input$guidelines_table_rows_selected
      
      moduleData$guidelines_temp[selected_row, "guideline_name"] <- if (nchar(input$guideline_name) == 0) NA else input$guideline_name
      moduleData$guidelines_temp[selected_row, "publisher"] <- if (nchar(input$publisher) == 0) NA else input$publisher
      moduleData$guidelines_temp[selected_row, "reference"] <- if (nchar(input$reference) == 0) NA else input$reference
      moduleData$guidelines_temp[selected_row, "note"] <- if (nchar(input$note) == 0) NA else input$note
      moduleData$guidelines_temp[selected_row, "parameter_id"] <- if (is.null(input$parameter_id)) NA else input$parameter_id
      moduleData$guidelines_temp[selected_row, "sample_fraction_id"] <- if (is.null(input$sample_fraction)) NA else input$sample_fraction
      moduleData$guidelines_temp[selected_row, "result_speciation_id"] <- if (is.null(input$result_speciation)) NA else input$result_speciation
      moduleData$guidelines_temp[selected_row, "guideline_sql"] <- if (nchar(input$guideline_sql) == 0) NA else input$guideline_sql
    })
    
    
    # ObserveEvents for when the user inserts parameter_id = X or other via modals.
    # helper to show a modal with selectize and a confirm button
    showPickModal <- function(id_pick, title, choices_named, id_confirm) {
      showModal(modalDialog(
        size = "m",
        title = title,
        selectizeInput(ns(id_pick), NULL, choices = choices_named, options = list(placeholder = "Type to search...")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(id_confirm), "Insert", class = "btn-primary")
        )
      ))
    }
    
    # ----- Insert PARAMETER -----
    observeEvent(input$insert_parameter, ignoreInit = TRUE, {
      # choices: names = param_name (shown), values = parameter_id (used)
      ch <- setNames(moduleData$parameters$parameter_id, paste0(moduleData$parameters$param_name,
                                                                " (", 
                                                                moduleData$parameters$unit_default, ")"))
      showPickModal("pick_param", "Choose a parameter", ch, "confirm_param")
    })
    
    # ----- Insert PARAMETER -----
    observeEvent(input$confirm_param, ignoreInit = TRUE, {
      req(input$pick_param)
      removeModal()
      
      # Look up name/units for the chosen parameter
      i <- match(input$pick_param, moduleData$parameters$parameter_id)
      p_name  <- moduleData$parameters$param_name[i]
      p_units <- moduleData$parameters$unit_default[i]
      label <- if (!is.na(p_units) && nzchar(p_units)) {
        paste0("parameter: ", p_name, " [", p_units, "]")
      } else {
        paste0("parameter: ", p_name)
      }
      
      session$sendCustomMessage("insertAtCursor", list(
        target     = ns("guideline_sql"),
        text       = paste0("parameter_id = ", input$pick_param),
        eolComment = label
      ))
    })
    
    # ----- Insert SAMPLE FRACTION -----
    observeEvent(input$insert_fraction, ignoreInit = TRUE, {
      ch <- setNames(moduleData$sample_fractions$sample_fraction_id,
                     moduleData$sample_fractions$sample_fraction)
      showPickModal("pick_frac", "Choose a sample fraction", ch, "confirm_frac")
    })
    
    observeEvent(input$confirm_frac, ignoreInit = TRUE, {
      req(input$pick_frac)
      removeModal()
      
      i <- match(input$pick_frac, moduleData$sample_fractions$sample_fraction_id)
      f_name <- moduleData$sample_fractions$sample_fraction[i]
      label  <- paste0("fraction: ", f_name)
      
      session$sendCustomMessage("insertAtCursor", list(
        target     = ns("guideline_sql"),
        text       = paste0("sample_fraction_id = ", input$pick_frac),
        eolComment = label
      ))
    })
    
    # ----- Insert RESULT SPECIATION -----
    observeEvent(input$insert_speciation, ignoreInit = TRUE, {
      ch <- setNames(moduleData$result_speciations$result_speciation_id,
                     moduleData$result_speciations$result_speciation)
      showPickModal("pick_spec", "Choose a result speciation", ch, "confirm_spec")
    })
    
    observeEvent(input$confirm_spec, ignoreInit = TRUE, {
      req(input$pick_spec)
      removeModal()
      
      i <- match(input$pick_spec, moduleData$result_speciations$result_speciation_id)
      s_name <- moduleData$result_speciations$result_speciation[i]
      label  <- paste0("speciation: ", s_name)
      
      session$sendCustomMessage("insertAtCursor", list(
        target     = ns("guideline_sql"),
        text       = paste0("result_speciation_id = ", input$pick_spec),
        eolComment = label
      ))
    })
    
    
    
    
    # Reusable functions to help extract parameters from guidelines for testing
    # 1) Extract unique (parameter_id, sample_fraction_id, result_speciation_id?) triples
    extract_guideline_combos <- function(sql) {
      
      # Strategy A: look inside each FILTER (WHERE ...) block (most precise)
      filter_blocks <- stringr::str_match_all(sql, "FILTER\\s*\\(\\s*WHERE\\s*([^\\)]*)\\)")[[1]]
      if (nrow(filter_blocks) == 0) {
        return(NULL)
      }
      combos_a <- lapply(seq_len(nrow(filter_blocks)), function(i) {
        block <- filter_blocks[i, 2]
        data.frame(
          parameter_id         = suppressWarnings(as.integer(stringr::str_match(block, "parameter_id\\s*=\\s*(\\d+)")[,2])),
          sample_fraction_id   = suppressWarnings(as.integer(stringr::str_match(block, "sample_fraction_id\\s*=\\s*(\\d+)")[,2])),
          result_speciation_id = suppressWarnings(as.integer(stringr::str_match(block, "result_speciation_id\\s*=\\s*(\\d+)")[,2]))
        )
      }) |> dplyr::bind_rows()
      
      # Strategy B (fallback): co-occurrence within a short window (either order)
      # param then fraction (optional speciation)
      pairs1 <- stringr::str_match_all(
        sql,
        "parameter_id\\s*=\\s*(\\d+)[^\\)]{0,160}?sample_fraction_id\\s*=\\s*(\\d+)(?:[^\\)]{0,160}?result_speciation_id\\s*=\\s*(\\d+))?"
      )[[1]]
      # fraction then param (optional speciation)
      pairs2 <- stringr::str_match_all(
        sql,
        "sample_fraction_id\\s*=\\s*(\\d+)[^\\)]{0,160}?parameter_id\\s*=\\s*(\\d+)(?:[^\\)]{0,160}?result_speciation_id\\s*=\\s*(\\d+))?"
      )[[1]]
      
      combos_b <- data.frame()
      if (nrow(pairs1) > 0) {
        combos_b <- dplyr::bind_rows(combos_b, data.frame(
          parameter_id         = as.integer(pairs1[,2]),
          sample_fraction_id   = as.integer(pairs1[,3]),
          result_speciation_id = suppressWarnings(as.integer(pairs1[,4]))
        ))
      }
      if (nrow(pairs2) > 0) {
        combos_b <- dplyr::bind_rows(combos_b, data.frame(
          parameter_id         = as.integer(pairs2[,3]),
          sample_fraction_id   = as.integer(pairs2[,2]),
          result_speciation_id = suppressWarnings(as.integer(pairs2[,4]))
        ))
      }
      
      # Combine, drop rows missing param or fraction, make unique & sorted
      dplyr::bind_rows(combos_a, combos_b) |>
        filter(!is.na(parameter_id), !is.na(sample_fraction_id)) |>
        dplyr::distinct() |>
        dplyr::arrange("parameter_id", "sample_fraction_id", "result_speciation_id")
    }
    
    # 2) Build numericInputs with descriptive labels
    make_test_inputs <- function(ns, sql, parameters_df, fractions_df, speciations_df) {
      combos <- extract_guideline_combos(sql)
      
      # map IDs -> names
      pname <- function(pid) {
        parameters_df$param_name[match(pid, parameters_df$parameter_id)]
      }
      units <- function(pid) {
        parameters_df$unit_default[match(pid, parameters_df$parameter_id)]
      }
      fracname <- function(fid) {
        fractions_df$sample_fraction[match(fid, fractions_df$sample_fraction_id)]
      }
      specname <- function(sid) {
        if (is.na(sid)) NA_character_ else speciations_df$result_speciation[match(sid, speciations_df$result_speciation_id)]
      }
      
      # stable inputId, readable label
      lapply(seq_len(nrow(combos)), function(i) {
        pid <- combos$parameter_id[i]
        fid <- combos$sample_fraction_id[i]
        sid <- combos$result_speciation_id[i]
        
        label <- paste0(
          pname(pid), " (", fracname(fid), ")",
          if (!is.na(sid)) specname(sid) else "",
          " [", units(pid), "]"
        )
        
        # include sid (or 0) in id to make it unique & deterministic
        inputId <- paste0("test_val_", pid, "_", fid, "_", ifelse(is.na(sid), 0L, sid))
        
        numericInput(
          ns(inputId),
          label = label,
          value = NA_real_
        )
      })
    }
    
    
    # Test a guideline by inserting temporary sample/results and evaluating
    observeEvent(input$test_guideline, {
      req(nzchar(input$guideline_sql))
      
      # Make sure the user has saved their guideline first if there's anything pending
      if (!is.null(input$guidelines_table_rows_selected)) {
        selected_row <- input$guidelines_table_rows_selected
        # Check to see if guideline_id is -1 (new guideline not yet saved) or if the selected row isn't an exact match between moduleData$guidelines and moduleData$guidelines_temp
        
        if (moduleData$guidelines$guideline_id[selected_row] == -1 ||
            !identical(moduleData$guidelines[selected_row, ], moduleData$guidelines_temp[selected_row, ])) {
          showModal(modalDialog(
            title = "Save guideline first",
            "Please save your guideline first before testing it. Click 'Save Guideline' in the sidebar after making any changes or entering a new guideline, then try testing again.",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
          ))
          return()
        }
      } else {
        showModal(modalDialog(
          title = "Select or add a guideline first",
          "Please select an existing guideline from the table or add a new one (and make sure it's selected in the table) before testing.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
      }
      
      # For SQL that requires parameter_id and sample_fraction_id, extract these pairs from the SQL
      sql <- input$guideline_sql
      
      # Example SQL:
      # sql <- "WITH vals AS (
      #   SELECT
      #   -- dissolved Ca/Mg (preferred)
      #   MAX(result) FILTER (WHERE parameter_id = 1061 AND sample_fraction_id = 19) AS ca_d,
      #   MAX(result) FILTER (WHERE parameter_id = 1103 AND sample_fraction_id = 19) AS mg_d,
      #   -- reported dissolved hardness (as CaCO3 - fallback 1)
      #   MAX(result) FILTER (WHERE parameter_id = 100 AND sample_fraction_id = 19 AND result_speciation_id = 3) AS hard_d,  -- as CaCO3
      #   -- total Ca/Mg (fallback 2)
      #   MAX(result) FILTER (WHERE parameter_id = 1061 AND sample_fraction_id = 5)  AS ca_t,
      #   MAX(result) FILTER (WHERE parameter_id = 1103 AND sample_fraction_id = 5)  AS mg_t,
      #   -- reported total hardness (fallback 3)
      #   MAX(result) FILTER (WHERE parameter_id = 100 AND sample_fraction_id = 5 AND result_speciation_id = 3)  AS hard_t  -- as CaCO3
      #   FROM discrete.results
      #   WHERE sample_id = $1::integer
      # ),
      # hardness AS (
      #   -- Compute hardness as CaCO3 (mg/L) where possible; otherwise use reported hardness
      #   SELECT CASE
      #   WHEN ca_d IS NOT NULL AND mg_d IS NOT NULL AND ca_d > 0 AND mg_d > 0
      #   THEN 2.497*ca_d + 4.118*mg_d
      #   WHEN hard_d IS NOT NULL AND hard_d > 0
      #   THEN hard_d
      #   WHEN ca_t IS NOT NULL AND mg_t IS NOT NULL AND ca_t > 0 AND mg_t > 0
      #   THEN 2.497*ca_t + 4.118*mg_t
      #   WHEN hard_t IS NOT NULL AND hard_t > 0
      #   THEN hard_t
      #   ELSE NULL
      #   END AS h
      #   FROM vals
      # )
      # -- Return Copper CWQG in mg/L (convert µg/L thresholds by /1000)
      # SELECT CASE
      # WHEN h IS NULL OR h < 82
      # THEN 0.002::numeric                          -- 2 µg/L, the minimum and fallback value
      # WHEN h <= 180
      # THEN (0.2 * exp(0.8545*ln(h) - 1.465) / 1000)::numeric  -- convert µg/L → mg/L
      # ELSE
      # 0.004::numeric                          -- 4 µg/L, the maximum value which is only ever valid if the calculations could be run
      # END
      # FROM hardness;"
      
      moduleData$test_pairs <- extract_guideline_combos(input$guideline_sql)  # Will return NULL if no parameters found
      
      if (!is.null(moduleData$test_pairs)) { # Parameters required, show inputs
        guideline_inputs <- make_test_inputs(
          ns,
          sql = input$guideline_sql,
          parameters_df = moduleData$parameters,
          fractions_df  = moduleData$sample_fractions,
          speciations_df = moduleData$result_speciations
        )
        showModal(modalDialog(
          title = "Test guideline",
          do.call(tagList, guideline_inputs),
          actionButton(ns("run_test_guideline"), "Run"),
          uiOutput(ns("test_guideline_result")),
          footer = tagList(modalButton("Cancel")),
          size = "xl"
        ))
      } else {  # No parameters required, just show the Run button
        showModal(modalDialog(
          title = "Test guideline",
          actionButton(ns("run_test_guideline"), "Run"),
          uiOutput(ns("test_guideline_result")),
          footer = tagList(
            modalButton("Cancel")
          ),
          size = "xl"
        ))
      }
      
    })
    
    observeEvent(input$run_test_guideline, {
      
      # Initialize result and error variables
      result <- NULL
      err <- NULL
      
      if (!is.null(moduleData$test_pairs)) {
        pairs <- moduleData$test_pairs
        values <- lapply(seq_len(nrow(pairs)), function(i) {
          pid <- as.integer(pairs$parameter_id[i])
          sfid <- as.integer(pairs$sample_fraction_id[i])
          val <- input[[paste0("test_val_", pid, "_", sfid)]]
          if (is.null(val) || is.na(val)) return(NULL)
          list(parameter_id = pid, sample_fraction = sfid, value = val)
        })
        values <- values[!vapply(values, is.null, logical(1))]
        
        
        # Tell the user they need to provide values if none were given
        if (length(values) == 0) {
          output$test_guideline_result <- renderUI({
            div(
              style = "color: red; font-weight: bold;",
              "No values provided. Please enter at least one value to test the guideline."
            )
          })
        }
        
        # Run the guideline
        tryCatch({
          DBI::dbExecute(session$userData$AquaCache, "BEGIN")
          sample_id <- DBI::dbGetQuery(session$userData$AquaCache,
                                       "INSERT INTO discrete.samples DEFAULT VALUES RETURNING sample_id")$sample_id
          for (v in values) {
            DBI::dbExecute(session$userData$AquaCache,
                           "INSERT INTO discrete.results (sample_id, result_type, parameter_id, sample_fraction_id, result_speciation_id, result) VALUES ($1,1,$2,$3,$4)",
                           params = list(sample_id, v$parameter_id, v$sample_fraction_id, v$result_speciation_id, v$value))
          }
          gid <- NA
          if (!is.null(input$guidelines_table_rows_selected)) {
            gid <- moduleData$guidelines$guideline_id[input$guidelines_table_rows_selected]
          }
          gval <- if (!is.na(gid) && gid != -1) gid else input$guideline_sql
          res <- DBI::dbGetQuery(session$userData$AquaCache,
                                 "SELECT get_guideline_value($1, $2) AS value",
                                 params = list(gval, sample_id))
          result <- res$value[1]
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
        }, error = function(e) {
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
          err <<- conditionMessage(e)
        }, warning = function(w) {
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
          err <<- conditionMessage(w)
        })
      } else {  # No test pairs provided, simple guideline calculation required.
        tryCatch({
          gid <- NA
          if (!is.null(input$guidelines_table_rows_selected)) {
            gid <- moduleData$guidelines$guideline_id[input$guidelines_table_rows_selected]
          }
          gval <- if (!is.na(gid) && gid != -1) gid else input$guideline_sql
          res <- DBI::dbGetQuery(session$userData$AquaCache,
                                 "SELECT get_guideline_value($1, NULL) AS value",
                                 params = list(gval))
          result <- res$value[1]
        }, error = function(e) {
          err <<- conditionMessage(e)
        }, warning = function(w) {
          err <<- conditionMessage(w)
        })
      }
      
      # Show the user diagnostic information
      if (!is.null(err)) {       # If there was an error, show it:
        output$test_guideline_result <- renderUI({
          div(
            style = "color: red; font-weight: bold;",
            paste("Error:", err)
          )
        })
      } else if (is.null(result) || is.na(result)) { # If the result is NULL or NA
        output$test_guideline_result <- renderUI({
          div(
            style = "color: orange; font-weight: bold;",
            "The guideline returned NULL or NA. This may indicate an issue with the SQL or that no applicable guideline was found."
          )
        })
      } else {  # Otherwise show the result
        output$test_guideline_result <- renderUI({
          div(
            style = "color: green; font-weight: bold;",
            paste("Guideline value:", result)
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
      if (is.na(guideline$guideline_name) || is.na(guideline$parameter_id) || is.na(guideline$publisher) || is.na(guideline$guideline_sql)) {
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
      
      tryCatch({
        
        sf <- if (is.null(guideline$sample_fraction_id) || is.na(guideline$sample_fraction_id) || guideline$sample_fraction_id == "")
          NA_integer_ else as.integer(guideline$sample_fraction_id)
        rs <- if (is.null(guideline$result_speciation_id) || is.na(guideline$result_speciation_id) || guideline$result_speciation_id == "")
          NA_integer_ else as.integer(guideline$result_speciation_id)
        ref <- if (is.null(guideline$reference) || is.na(guideline$reference) || guideline$reference == "")
          NA_character_ else guideline$reference
        
        if (guideline$guideline_id == -1) {
          # New guideline - perform INSERT
          DBI::dbExecute(session$userData$AquaCache, 
                         "INSERT INTO discrete.guidelines (guideline_name, publisher, reference, note, parameter_id, sample_fraction_id, result_speciation_id, guideline_sql) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
                         params = list(guideline$guideline_name, guideline$publisher, guideline$reference, guideline$note, guideline$parameter_id, sf, rs, guideline$guideline_sql))
        } else {
          # Existing guideline - perform UPDATE
          DBI::dbExecute(session$userData$AquaCache, 
                         "UPDATE discrete.guidelines SET guideline_name = $1, publisher = $2, reference = $3, note = $4, parameter_id = $5, sample_fraction_id = $6, result_speciation_id = $7, guideline_sql = $8 WHERE guideline_id = $9",
                         params = list(guideline$guideline_name, guideline$publisher, guideline$reference, guideline$note, guideline$parameter_id, sf, rs, guideline$guideline_sql, guideline$guideline_id))
        }
        
        # Refresh the guidelines data from the database
        moduleData$guidelines <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT g.guideline_id, g.guideline_name, g.publisher, g.note, g.reference, p.param_name AS parameter, p.unit_default AS units, sf.sample_fraction AS fraction, rs.result_speciation AS speciation, g.parameter_id, g.sample_fraction_id, g.result_speciation_id, g.guideline_sql FROM discrete.guidelines as g LEFT JOIN sample_fractions sf ON sf.sample_fraction_id = g.sample_fraction_id JOIN public.parameters as p ON p.parameter_id = g.parameter_id LEFT JOIN result_speciations rs ON rs.result_speciation_id = g.result_speciation_id")
        moduleData$guidelines_temp <- moduleData$guidelines
        
        # Clear selection and hide save button
        DT::dataTableProxy("guidelines_table") %>% DT::selectRows(NULL)
        shinyjs::hide("save_guideline")
        showModal(modalDialog(
          title = "Success",
          "Guideline saved successfully.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred while saving the guideline: ", e$message),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
      })
    })
    
    # Observe a request to delete a guideline
    observeEvent(input$delete_guideline, {
      req(input$guidelines_table_rows_selected)
      
      guideline <- moduleData$guidelines[input$guidelines_table_rows_selected, ]
      
      showModal(modalDialog(
        title = "Confirm Deletion",
        paste("Are you sure you want to delete the guideline '", guideline$guideline_name, "'? This action cannot be undone.", sep = ""),
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
      
      tryCatch({
        if (guideline$guideline_id != -1) {  # Only attempt to delete if it's not a new unsaved guideline
          DBI::dbExecute(session$userData$AquaCache, "DELETE FROM discrete.guidelines WHERE guideline_id = $1", params = list(guideline$guideline_id))
        }
        
        # Refresh the data, which will re-render the datatable without the deleted row
        moduleData$guidelines <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT g.guideline_id, g.guideline_name, g.publisher, g.note, g.reference, p.param_name AS parameter, p.unit_default AS units, sf.sample_fraction AS fraction, rs.result_speciation AS speciation, g.parameter_id, g.sample_fraction_id, g.result_speciation_id, g.guideline_sql FROM discrete.guidelines as g LEFT JOIN sample_fractions sf ON sf.sample_fraction_id = g.sample_fraction_id JOIN public.parameters as p ON p.parameter_id = g.parameter_id LEFT JOIN result_speciations rs ON rs.result_speciation_id = g.result_speciation_id")
        moduleData$guidelines_temp <- moduleData$guidelines
        
        # Clear table selection and hide save button
        DT::dataTableProxy("guidelines_table") %>% DT::selectRows(NULL)
        shinyjs::hide("save_guideline")
        showModal(modalDialog(
          title = "Success",
          "Guideline deleted successfully.",
          footer = tagList(
            modalButton("Close")
          )
        ))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred while deleting the guideline:", e$message),
          footer = tagList(
            modalButton("Close")
          )
        ))
      }) # End of tryCatch
    }) # End of observeEvent for confirm_delete
    
    
  }) # End of moduleServer
}

