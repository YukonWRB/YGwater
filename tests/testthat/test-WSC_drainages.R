test_that("drainages works with no limit_stns", {
  suppressWarnings(file.remove(list.files(tempdir(), full.names = TRUE)))
  suppressWarnings(dir.create(paste0(tempdir(), "/test")))
  expect_snapshot(WSC_drainages(inputs_folder = test_path("drainages_data"), save_path = paste0(tempdir(), "/test"), limit_stns = NULL), transform = function(x) {substr(x, 1,40)})
})

test_that("drainages works with limit_stns", {
  suppressWarnings(file.remove(list.files(tempdir(), full.names = TRUE)))
  suppressWarnings(dir.create(paste0(tempdir(), "/test")))
  expect_snapshot(WSC_drainages(inputs_folder = test_path("drainages_data"), save_path = paste0(tempdir(), "/test")), transform = function(x) {substr(x, 1,40)})
})
