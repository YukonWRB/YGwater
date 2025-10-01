test_that("round_any rounds using the supplied function", {
  expect_equal(YGwater:::round_any(c(1.1, 1.6), 1), c(1, 2))
  expect_equal(YGwater:::round_any(5.7, 2, floor), 4)
  expect_equal(YGwater:::round_any(5.1, 2, ceiling), 6)
})


test_that("iso_period converts numeric hours to ISO 8601 durations", {
  expect_equal(YGwater:::iso_period(0), "P0DT0H0M0S")
  expect_equal(YGwater:::iso_period(26.5), "P1DT2H30M0S")
  expect_equal(
    YGwater:::iso_period(c(1.5, 50.75)),
    c("P0DT1H30M0S", "P2DT2H45M0S")
  )
})
