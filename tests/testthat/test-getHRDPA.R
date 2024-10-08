skip_if_offline()
skip_on_ci()

test_that("four or five (depending on time and availability on remote) rasters are downloaded and saved as tiffs when using default parameters", {
  suppressWarnings(file.remove(list.files(tempdir(), full.names = TRUE)))
  existing <- list.files(tempdir(), pattern = "\\.tiff$")
  getHRDPA(save_path = tempdir())
  new <- list.files(tempdir(), pattern = "\\.tiff$")
  expect_equal((length(new) - length(existing)), 4)
})


test_that("clipped file is smaller than full (clip worked)", {
  suppressWarnings(file.remove(list.files(tempdir(), full.names = TRUE)))
  existingClipped <- list.files(tempdir(), pattern = "^clipped*") #in case anything remains after file.remove
  existingFull <- list.files(tempdir(), pattern = "^HRDPA*") #in case anything remains after file.remove

  getHRDPA(start = Sys.time() - 60*60*6, clip = NULL, save_path = tempdir())
  getHRDPA(start = Sys.time() - 60*60*6, clip = "YT", save_path = tempdir())

  postClipped <- list.files(tempdir(), pattern = "^clipped*")
  postFull <- list.files(tempdir(), pattern = "^HRDPA*")
  clipSize <- file.size(paste0(tempdir(), "/", postClipped[!(postClipped %in% existingClipped)][1]))
  fullSize <- file.size(paste0(tempdir(), "/", postFull[!(postFull %in% existingClipped)][1]))
  diff <- fullSize - clipSize

  expect_equal((diff > 1000000), TRUE)
})

test_that("same number of clipped and not clipped files are created if both are called separately",{
  suppressWarnings(file.remove(list.files(tempdir(), full.names = TRUE)))

  existingClipped <- list.files(tempdir(), pattern = "^clipped*") #in case anything remains after file.remove
  existingFull <- list.files(tempdir(), pattern = "^HRDPA*") #in case anything remains after file.remove

  getHRDPA(clip = NULL, save_path = tempdir())
  getHRDPA(clip = "YT", save_path = tempdir())

  postClipped <- list.files(tempdir(), pattern = "^clipped*")
  postFull <- list.files(tempdir(), pattern = "^HRDPA*")
  newClipped <- length(postClipped[!(postClipped %in% existingClipped)])
  newFull <- length(postFull[!(postFull %in% existingClipped)])

  expect_equal(newClipped, newFull)
})
