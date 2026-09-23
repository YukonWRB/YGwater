test_that("borehole document uploads run in an ExtendedTask", {
  module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/manageBoreholeDocuments.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    module,
    'bslib::input_task_button(',
    fixed = TRUE
  )
  expect_match(
    module,
    "upload_task <- ExtendedTask$new(function(request)",
    fixed = TRUE
  )
  expect_match(
    module,
    "promises::future_promise(seed = NULL, expr = {",
    fixed = TRUE
  )
  expect_match(
    module,
    'bslib::bind_task_button("upload_and_associate")',
    fixed = TRUE
  )
  expect_match(module, "upload_task$invoke(request)", fixed = TRUE)
  expect_match(module, "file.copy(", fixed = TRUE)
  expect_match(module, "YGwater::AquaConnect(", fixed = TRUE)
  expect_match(
    module,
    "on.exit(unlink(request$path, force = TRUE), add = TRUE)",
    fixed = TRUE
  )
  expect_match(
    module,
    "result <- AquaCache::insertACDocument(",
    fixed = TRUE
  )
  expect_match(
    module,
    "result$new_document_id[[1]]",
    fixed = TRUE
  )
  expect_false(grepl("doc_bytes <- readBin(", module, fixed = TRUE))
  expect_false(grepl("SELECT md5($1::bytea)", module, fixed = TRUE))
  expect_false(grepl(
    "WHERE name = $1 AND type = $2",
    module,
    fixed = TRUE
  ))
})
