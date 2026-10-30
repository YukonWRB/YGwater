test_that("HEIF conversion availability reflects the ImageMagick build", {
  expect_identical(
    YGwater:::heif_jpeg_conversion_available(),
    isTRUE(magick::magick_config()$heic) &&
      isTRUE(magick::magick_config()$jpeg)
  )
})

test_that("HEIF uploads are converted to temporary JPEGs before callbacks", {
  if (!YGwater:::heif_jpeg_conversion_available()) {
    skip("ImageMagick was built without HEIC support.")
  }

  heif_path <- tempfile(fileext = ".heic")
  upload_path <- tempfile()
  on.exit(unlink(heif_path, force = TRUE), add = TRUE)
  on.exit(unlink(upload_path, force = TRUE), add = TRUE)
  magick::image_write(
    magick::image_blank(width = 8, height = 6, color = "red"),
    path = heif_path,
    format = "heic"
  )
  expect_true(file.copy(heif_path, upload_path))

  converted_path <- NULL
  result <- YGwater:::with_heif_upload_as_jpeg(
    upload_path,
    function(path) {
      converted_path <<- path
      expect_true(file.exists(path))
      expect_identical(tolower(tools::file_ext(path)), "jpg")
      expect_identical(
        readBin(path, "raw", n = 2L),
        as.raw(c(0xff, 0xd8))
      )
      "uploaded"
    },
    extension = "HEIC"
  )

  expect_identical(result, "uploaded")
  expect_false(file.exists(converted_path))

  heif_path <- tempfile(fileext = ".heif")
  on.exit(unlink(heif_path, force = TRUE), add = TRUE)
  magick::image_write(
    magick::image_blank(width = 8, height = 6, color = "blue"),
    path = heif_path,
    format = "heic"
  )
  converted_heif <- YGwater:::convert_heif_to_jpeg(
    heif_path,
    extension = "heif"
  )
  on.exit(unlink(converted_heif, force = TRUE), add = TRUE)
  expect_identical(tolower(tools::file_ext(converted_heif)), "jpg")
  expect_identical(
    readBin(converted_heif, "raw", n = 2L),
    as.raw(c(0xff, 0xd8))
  )
})

test_that("HEIF uploads are rejected when JPEG conversion is unavailable", {
  helper_environment <- new.env(parent = globalenv())
  sys.source("R/heif_upload_helpers.R", envir = helper_environment)
  helper_environment$heif_jpeg_conversion_available <- function() FALSE
  callback_called <- FALSE
  expect_error(
    helper_environment$with_heif_upload_as_jpeg(
      "missing.heic",
      function(path) {
        callback_called <<- TRUE
        path
      }
    ),
    "cannot convert HEIC/HEIF images to JPEG"
  )
  expect_false(callback_called)
})

test_that("simplerIndex normalizes HEIC and HEIF images to JPEG pages", {
  if (!YGwater:::heif_jpeg_conversion_available()) {
    skip("ImageMagick was built without HEIC support.")
  }

  helper_environment <- new.env(parent = globalenv())
  source(
    "inst/apps/aqualink/modules/admin/boreholes_wells/simplerIndex_helpers.R",
    local = helper_environment
  )
  output_dir <- tempfile("simplerIndex_heif_test_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  for (extension in c("heic", "heif")) {
    input_path <- tempfile(fileext = paste0(".", extension))
    on.exit(unlink(input_path, force = TRUE), add = TRUE)
    magick::image_write(
      magick::image_blank(width = 8, height = 6, color = "green"),
      path = input_path,
      format = "heic"
    )
    rendered <- helper_environment$render_document_pages(
      document_path = input_path,
      document_type = extension,
      output_dir = output_dir,
      filename_prefix = extension
    )
    expect_length(rendered, 1L)
    expect_identical(tolower(tools::file_ext(rendered)), "jpg")
    expect_identical(
      readBin(rendered, "raw", n = 2L),
      as.raw(c(0xff, 0xd8))
    )
  }
})
