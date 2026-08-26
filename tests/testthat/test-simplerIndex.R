test_that("simplerIndex carries seal and screen construction fields to AquaCache", {
  module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/simplerIndex.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  for (input_id in c(
    "seal_material",
    "seal_diameter",
    "seal_depth_from",
    "seal_depth_to",
    "screen_material",
    "screen_type"
  )) {
    expect_match(module, paste0('ns("', input_id, '")'), fixed = TRUE)
  }
  expect_match(module, "for (field in well_specific_fields)", fixed = TRUE)
  field_arguments <- c(
    seal_material = "seal_material",
    seal_diameter_mm = "seal_diameter",
    seal_depth_from = "seal_depth_from",
    seal_depth_to = "seal_depth_to",
    screen_material = "screen_material",
    screen_type = "screen_type"
  )
  for (argument in names(field_arguments)) {
    expect_match(
      module,
      paste0(
        argument,
        " = well_vector(wells, \"",
        field_arguments[[argument]],
        "\""
      ),
      fixed = TRUE
    )
  }
})

test_that("simplerIndex stores arbitrary wells beneath one borehole", {
  module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/simplerIndex.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(module, "wells = list()", fixed = TRUE)
  expect_match(module, "bslib::accordion_panel", fixed = TRUE)
  expect_match(module, "input$add_nested_well", fixed = TRUE)
  expect_match(module, "insert_vectorized_borehole <- function(", fixed = TRUE)
  expect_match(
    module,
    "metadata$input_key <- new_well_input_key()",
    fixed = TRUE
  )
  expect_match(
    module,
    'value = paste0("well_", well_key)',
    fixed = TRUE
  )
  expect_match(module, "queue_auto_well_name_updates(wells)", fixed = TRUE)
  expect_match(
    module,
    "!selection %in% borehole_choices()",
    fixed = TRUE
  )
  expect_false(grepl(
    "!selection %in% names(rv$borehole_data)",
    module,
    fixed = TRUE
  ))
  expect_match(
    module,
    "value = well_display_value(well, field, NA_real_)",
    fixed = TRUE
  )
  expect_match(module, "min = NA_real_", fixed = TRUE)
  expect_match(
    module,
    'depth_to_bedrock = metadata[["depth_to_bedrock"]]',
    fixed = TRUE
  )
  expect_match(
    module,
    'permafrost_top = metadata[["permafrost_top"]]',
    fixed = TRUE
  )
  expect_match(
    module,
    'permafrost_bot = metadata[["permafrost_bot"]]',
    fixed = TRUE
  )
  expect_false(grepl("depth_to_bedrock = metadata$", module, fixed = TRUE))
  expect_false(grepl("parent_borehole_id", module, fixed = TRUE))
  expect_false(grepl("INSERT INTO boreholes.wells", module, fixed = TRUE))
})

test_that("simplerIndex supports named and document-free borehole uploads", {
  module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/simplerIndex.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(module, 'uiOutput(ns("document_names_ui"))', fixed = TRUE)
  expect_match(module, "validate_document_names_for_upload", fixed = TRUE)
  expect_match(module, "SELECT name FROM files.documents", fixed = TRUE)
  expect_match(module, "document_name = document_name", fixed = TRUE)
  expect_match(
    module,
    "unique_borehole_ids <- names(rv$borehole_data)",
    fixed = TRUE
  )
  expect_false(grepl("No PDF pages available to upload.", module, fixed = TRUE))
})

test_that("simplerIndex stages PDFs before background processing", {
  module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/simplerIndex.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(module, 'pattern = "simplerIndex_upload_"', fixed = TRUE)
  expect_match(module, "copy_success <- file.copy(", fixed = TRUE)
  expect_match(
    module,
    "process_pdf_uploads$invoke(uploaded_files, upload_job_dir)",
    fixed = TRUE
  )
  expect_match(
    module,
    "promises::future_promise(seed = NULL, expr = {",
    fixed = TRUE
  )
  expect_match(module, "output_dir = upload_job_dir", fixed = TRUE)
  expect_false(grepl("file.rename(from_path, orig_path)", module, fixed = TRUE))
})

test_that("simplerIndex caps PDF raster size and avoids graphics-device redactions", {
  skip_if_not_installed("magick")
  skip_if_not_installed("pdftools")

  helper_environment <- new.env(parent = globalenv())
  sys.source(
    system.file(
      "apps/YGwater/modules/admin/boreholes_wells/simplerIndex_helpers.R",
      package = "YGwater"
    ),
    envir = helper_environment
  )

  pdf_path <- tempfile(fileext = ".pdf")
  output_dir <- tempfile("simplerIndex_render_test_")
  dir.create(output_dir)
  on.exit(unlink(c(pdf_path, output_dir), recursive = TRUE), add = TRUE)

  grDevices::pdf(pdf_path, width = 42, height = 56, paper = "special")
  graphics::plot.new()
  graphics::text(0.5, 0.5, "oversized PDF page")
  grDevices::dev.off()

  rendered <- helper_environment$render_pdf_pages(
    pdf_path,
    output_dir = output_dir,
    filename_prefix = "test",
    max_pixels = 1e6
  )
  info <- magick::image_info(magick::image_read(rendered))

  expect_length(rendered, 1)
  expect_true(file.exists(rendered))
  expect_lte(info$width * info$height, 1e6)

  image <- magick::image_blank(20, 20, color = "white")
  redacted <- helper_environment$apply_image_redactions(
    image,
    list(list(xmin = 2, ymin = 3, xmax = 7, ymax = 8))
  )
  redacted_pixel <- magick::image_data(
    magick::image_crop(redacted, "1x1+3+13"),
    channels = "rgb"
  )
  untouched_pixel <- magick::image_data(
    magick::image_crop(redacted, "1x1+0+0"),
    channels = "rgb"
  )

  expect_true(all(redacted_pixel == as.raw(0)))
  expect_true(all(untouched_pixel == as.raw(255)))
  expect_error(
    helper_environment$apply_image_redactions(
      image,
      list(list(xmin = NA, ymin = 3, xmax = 7, ymax = 8))
    ),
    "invalid coordinates"
  )

  module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/simplerIndex.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  helpers <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/simplerIndex_helpers.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(module, "render_pdf_pages(", fixed = TRUE)
  expect_match(module, "apply_image_redactions(img, rectangles)", fixed = TRUE)
  expect_false(grepl("image_draw(", paste(module, helpers), fixed = TRUE))
})

test_that("WWR cache and popup expose well construction details", {
  cache_module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/cache_functions.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  registry_module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/WWR/registry_front_end.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(cache_module, "LEFT JOIN boreholes.seal_materials", fixed = TRUE)
  expect_match(
    cache_module,
    "LEFT JOIN boreholes.screen_materials",
    fixed = TRUE
  )
  expect_match(cache_module, "LEFT JOIN boreholes.screen_types", fixed = TRUE)
  expect_match(registry_module, "Seal material:", fixed = TRUE)
  expect_match(registry_module, "Screen material:", fixed = TRUE)
  expect_match(registry_module, "Screen type:", fixed = TRUE)
})

test_that("WWR preserves distinct well and borehole registry rows", {
  cache_module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/cache_functions.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  registry_module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/WWR/registry_front_end.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(cache_module, "w.well_id", fixed = TRUE)
  expect_match(cache_module, "w.well_name", fixed = TRUE)
  expect_match(cache_module, "w.stick_up_height_m", fixed = TRUE)
  expect_match(cache_module, "LEFT JOIN boreholes.wells", fixed = TRUE)
  expect_match(cache_module, "b.borehole_purpose_id", fixed = TRUE)
  expect_match(cache_module, 'key = "wwr_module_data_v2"', fixed = TRUE)
  expect_match(cache_module, "registry_id = data.table::fifelse", fixed = TRUE)
  expect_match(
    registry_module,
    'ns("borehole_well_scope")',
    fixed = TRUE
  )
  expect_match(registry_module, 'selected = "with_wells"', fixed = TRUE)
  expect_match(
    registry_module,
    "wells_sub <- wells_sub[has_well == FALSE]",
    fixed = TRUE
  )
  expect_match(
    registry_module,
    "tmp[, .(registry_id, popup_html)]",
    fixed = TRUE
  )
  expect_match(registry_module, "on = .(registry_id)", fixed = TRUE)
  expect_match(registry_module, "matches_name(well_name)", fixed = TRUE)
  expect_match(registry_module, "layerId = ~registry_id", fixed = TRUE)
  expect_match(
    registry_module,
    "display_purpose_id = data.table::fifelse",
    fixed = TRUE
  )
  expect_match(registry_module, "hollow = !has_well[idx]", fixed = TRUE)
  expect_match(
    registry_module,
    "stroke_width <- max(stroke_width, 4)",
    fixed = TRUE
  )
  expect_match(registry_module, '" loc-fill-"', fixed = TRUE)
  expect_match(
    registry_module,
    "group_titles = legend_group_titles",
    fixed = TRUE
  )
  expect_match(
    registry_module,
    '"download_document_",\n          registry_id,',
    fixed = TRUE
  )
  expect_match(registry_module, "WHERE document_id = $1", fixed = TRUE)
  expect_false(grepl("wwr_popup_data_", registry_module, fixed = TRUE))
  expect_false(grepl(
    "on = .(borehole_id),\n        popup_html",
    registry_module,
    fixed = TRUE
  ))
})

test_that("WWR borehole scope labels are bilingual", {
  expect_equal(
    tr("show_boreholes_with_wells", "English"),
    "Show boreholes with wells"
  )
  expect_equal(
    tr("show_boreholes_without_wells", "Français"),
    "Afficher les forage sans puits"
  )
  expect_equal(tr("show_all_boreholes", "Français"), "Tout afficher")
  expect_equal(tr("borehole_purpose", "English"), "Borehole purpose")
  expect_equal(tr("borehole_purpose", "Français"), "Usage du forage")
  expect_equal(
    tr("well_or_borehole_purpose", "English"),
    "Well or borehole purpose"
  )
})

test_that("simplerIndex construction selects default to empty", {
  simpler_index <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/simplerIndex.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(simpler_index, '"Select seal material" = ""', fixed = TRUE)
  expect_match(simpler_index, '"Select screen material" = ""', fixed = TRUE)
  expect_match(simpler_index, '"Select screen type" = ""', fixed = TRUE)
  expect_match(
    simpler_index,
    'well_display_value(well, "seal_material", "")',
    fixed = TRUE
  )
  expect_match(
    simpler_index,
    'well_display_value(well, "screen_material", "")',
    fixed = TRUE
  )
  expect_match(
    simpler_index,
    'well_display_value(well, "screen_type", "")',
    fixed = TRUE
  )
})
