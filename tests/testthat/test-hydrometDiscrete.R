# Tests depend on snapshots so can't be run on CRAN
skip_on_cran()
skip_on_ci()

#NOTE: as of June 2023 hydrometDiscrete does not accept start/end dates other than 1 and 365. This explained in the help file.

# Tests below are skipped. Function used to work with direct connection to DB, now only works with provided data. Only use case is snow bulletin so not re-writing for now.
# test_that("violin plot is as expected for full year with numeric startDay and endDay", {
#   plot <- suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = 1, endDay = 365, years = "2022"))
#   vdiffr::expect_doppelganger("full year violin", plot)
# })
# 
# test_that("console plot output is as expected", {
#   plot <- suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = 1, endDay = 365, years = "2022"))
#   vdiffr::expect_doppelganger("full year violin", plot)
# })
# 
# test_that("violin plot is as expected for full year with Date startDay and endDay", {
#   plot <- suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir))
#   vdiffr::expect_doppelganger("full year violin with Date", plot)
# })
# 
# test_that("box plot is as expected for full year with numeric startDay and endDay", {
#   plot <- suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = 1, endDay = 365, years = "2022", save_path = dir,  plot_type = "boxplot"))
#   vdiffr::expect_doppelganger("full year boxplot", plot)
# })
# 
# test_that("box plot is as expected for full year with Date startDay and endDay", {
#   plot <- suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir, plot_type = "boxplot"))
#   vdiffr::expect_doppelganger("full year boxplot with Date", plot)
# })
# 
# test_that("plot scale factor and titles works", {
#   plot <- suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir, plot_type = "boxplot", plot_scale = 2, title = FALSE))
#   vdiffr::expect_doppelganger("full year boxplot with Date and scale", plot)
# })
# 
# test_that("depth plots work", {
#   plot <- suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir))
#   vdiffr::expect_doppelganger("full year depth plot", plot)
# })



# Tests below use provided (created) data for plots to ensure this function continues to work with snow bulletin data.

# # Run swe_basin to get discrete_data for the next two tests
# discrete_data <- suppressWarnings(SWE_basin(year = 2022,
#                                             month = c(3,4,5),
#                                             threshold = 6,
#                                             csv = FALSE,
#                                             summarise = FALSE))
# discrete_data$datetime <- paste0(discrete_data$year, "-0", discrete_data$month, "-01")
# discrete_data <- discrete_data %>%
#   dplyr::filter(location == "Upper_Yukon")
#   
#   # Save to a .rdata or .rds file
# save(discrete_data, file = test_path("fixtures", "discrete_data.rdata"))

# Load the saved data
load(test_path("fixtures", "discrete_data.rdata"))

test_that("violin plot is as expected when discrete data is given", {
  plot <- hydrometDiscrete(location = NULL, 
                           parameter = 'snow water equivalent',
                           years = c(2021, 2022), title = TRUE, 
                           plot_type = "boxplot", 
                           discrete_data = discrete_data)
  vdiffr::expect_doppelganger("discrete data boxplot", plot)
})


test_that("linedbox plot is as expected when discrete data is given", {
  plot <- hydrometDiscrete(location = "Upper Yukon", 
                   parameter = "snow water equivalent",
                   startDay = 1, 
                   tzone = "MST", 
                   years = 2022, 
                   title = TRUE,
                   custom_title = "Upper Yukon Basin Monthly Snow Course Data",
                   plot_type = "linedbox",
                   discrete_data = discrete_data,
                   plot_scale = 1)
  vdiffr::expect_doppelganger("discrete data linedbox", plot)
})

rm(discrete_data)

test_that("linedbox plot that starts and ends in different years is as expected when discrete data is given", {
  # Get precipitation data from measurements_continuous_corrected
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  discrete_data <- DBI::dbGetQuery(con, "SELECT datetime, value_corrected AS value FROM measurements_continuous_corrected WHERE timeseries_id = 663")
  attr(discrete_data$datetime, "tzone") <- "MST"
  discrete_data$month <- format(discrete_data$datetime, "%m")
  discrete_data$year <- format(discrete_data$datetime, "%Y")
  discrete_data$units <- "mm"
  
  plot <- hydrometDiscrete(location = NULL, parameter = "Total precipitation",
                   tzone = "MST",
                   years = 2022,
                   startDay = "2022-10-01", # 275
                   endDay = "2023-05-01", #120
                   title = TRUE, 
                   custom_title = "Whitehorse Monthly Precipitation",
                   plot_type = "linedbox", 
                   plot_scale = 1,#params$scale,
                   discrete_data = discrete_data, 
                   con = con)

  vdiffr::expect_doppelganger("discrete data linedbox different years", plot)
})
