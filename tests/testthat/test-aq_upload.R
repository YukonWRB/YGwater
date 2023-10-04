#For this test to work the testing machine must have Aquarius credentials loaded into the .Renviron file. Refer to function documentation for more info.
#These tests may never complete if Aquarius is being slow. Comment them all and run tests again if necessary to skip.
test_that("aquarius upload works without overwrite", {
  sequence <- seq.POSIXt(as.POSIXct("2022-01-01"), as.POSIXct("2022-01-15"), by = "hour")
  data <- data.frame(Time = sequence,
                    Value = runif(n = length(sequence), min = 10, max = 20))
  expect_snapshot(aq_upload("TEST", "Air Temp.Temperature", data, overwrite = FALSE))
})

test_that("aquarius upload works with overwrite", {
  sequence <- seq.POSIXt(as.POSIXct("2022-01-01"), as.POSIXct("2022-01-15"), by = "hour")
  data <- data.frame(Time = sequence,
                     Value = runif(n = length(sequence), min = 10, max = 20))
  expect_snapshot(aq_upload("TEST", "Air Temp.Temperature", data, overwrite = TRUE, start = min(sequence), end = max(sequence)))
})
