test_that("document spatial choices stay bounded and preserve existing associations", {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/documents/addDocs.R",
      package = "YGwater"
    ),
    env
  )
  env$application_notifications_ui <- function(...) NULL
  queries <- list()
  selected_ids <- integer()
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params = NULL, ...) {
      queries[[length(queries) + 1L]] <<- list(sql = statement, params = params)
      if (grepl("WITH RECURSIVE", statement, fixed = TRUE)) {
        return(data.frame(layer_name = c("Communities", "Waterbodies")))
      }
      if (grepl("FROM spatial.vectors", statement, fixed = TRUE)) {
        if (grepl("ANY($1", statement, fixed = TRUE)) {
          ids <- as.integer(strsplit(gsub("[{}]", "", params[[1L]]), ",")[[1L]])
          selected_ids <<- ids
          return(data.frame(
            geom_id = ids,
            layer_name = rep("Waterbodies", length(ids)),
            feature_name = rep("Existing lake", length(ids))
          ))
        }
        return(data.frame(
          geom_id = 1:100,
          layer_name = params[[1L]],
          feature_name = paste0(params[[2L]], seq_len(100))
        ))
      }
      if (grepl("FROM files.document_types", statement, fixed = TRUE)) {
        return(data.frame(document_type_id = 1L, document_type_en = "Report"))
      }
      if (grepl("FROM files.documents_spatial", statement, fixed = TRUE)) {
        return(data.frame(document_id = 1L, geom_id = 999L))
      }
      if (grepl("FROM files.documents d", statement, fixed = TRUE)) {
        return(data.frame(
          document_id = 1L,
          name = "Report",
          description = "Description",
          publish_date = as.Date("2026-01-01"),
          url = NA_character_,
          authors = "Author",
          tags = "tag",
          share_with = "{public_reader}",
          type = "Report",
          document_type_en = "Report"
        ))
      }
      if (grepl("get_shareable_principals_for", statement, fixed = TRUE)) {
        return(data.frame(role_name = "public_reader"))
      }
      if (grepl("FROM public.organizations", statement, fixed = TRUE)) {
        return(data.frame(organization_id = integer(), name = character()))
      }
      if (
        grepl(
          "FROM public.organization_data_sharing_agreements",
          statement,
          fixed = TRUE
        )
      ) {
        return(data.frame(organization_id = integer(), document_id = integer()))
      }
      stop("Unexpected query: ", statement)
    },
    .package = "DBI"
  )

  messages <- list()
  registrations <- list()
  session <- shiny::MockShinySession$new()
  session$registerDataObj <- function(name, data, filterFunc) {
    if (grepl("doc_geoms$", name)) {
      registrations[[length(registrations) + 1L]] <<- data
    }
    "/mock/selectize"
  }
  session$sendInputMessage <- function(inputId, message) {
    messages[[length(messages) + 1L]] <<- list(id = inputId, message = message)
  }
  shiny::testServer(
    env$addDocs,
    args = list(language = list(language = "en")),
    session = session,
    {
      session$flushReact()
      html <- output$ui$html
      expect_match(html, 'id="proxy1-ui_ready"', fixed = TRUE)
      # Opening the form only enumerates layers, never all spatial features.
      expect_length(
        Filter(
          function(q) grepl("FROM spatial.vectors", q$sql, fixed = TRUE),
          queries
        ),
        1L
      )
      expect_length(messages, 0L)

      session$setInputs(
        ui_ready = "first",
        geom_layer = "Communities",
        geom_start = "White"
      )
      expect_length(messages, 1L)
      expect_identical(messages[[1L]]$id, "doc_geoms")
      expect_true(nzchar(messages[[1L]]$message$url))
      expect_equal(nrow(registrations[[1L]]), 100L)
      search <- tail(queries, 1L)[[1L]]
      expect_match(search$sql, "LIMIT 100", fixed = TRUE)
      expect_identical(search$params, list("Communities", "White"))

      # An association outside the current search must still have a label and
      # remain selected when editing an existing document.
      session$setInputs(documents_table_rows_selected = 1L)
      expect_identical(selected_ids, 999L)
      geom_messages <- Filter(function(m) m$id == "doc_geoms", messages)
      expect_identical(tail(geom_messages, 1L)[[1L]]$message$value, "999")
      expect_true(
        "Waterbodies - Existing lake" %in% tail(registrations, 1L)[[1L]]$label
      )

      session$setInputs(
        doc_geoms = c("999", "1"),
        geom_layer = "Waterbodies",
        geom_start = "Lake",
        find_geoms = 1L
      )
      expect_identical(selected_ids, c(999L, 1L))
      expect_identical(
        tail(queries, 1L)[[1L]]$params,
        list("Waterbodies", "Lake")
      )
      # A replacement form receives a fresh server-side data URL.
      session$setInputs(ui_ready = "reloaded", doc_geoms = character())
      expect_length(selected_ids, 0L)
      expect_true(all(vapply(
        Filter(
          function(q) {
            grepl("FROM spatial.vectors", q$sql, fixed = TRUE) &&
              !grepl("WITH RECURSIVE", q$sql, fixed = TRUE)
          },
          queries
        ),
        function(q) grepl("LIMIT 100|ANY\\(\\$1", q$sql),
        logical(1)
      )))
    }
  )
})
