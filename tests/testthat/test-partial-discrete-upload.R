partial_upload_environment <- function() {
  env <- new.env(parent = asNamespace("YGwater"))
  path <- Sys.getenv("YGWATER_PARTIAL_MODULE")
  if (!nzchar(path)) path <- system.file("apps/aqualink/modules/admin/discreteData/addDiscData.R", package="YGwater")
  sys.source(path, env)
  env
}

test_that("partial plans include only assigned results and preserve sample edits", {
  env <- partial_upload_environment()
  rows <- env$addDiscData_empty_table()[rep(NA_integer_, 380), ]
  rows$source_code <- "review"
  rows$sample_key <- rep(paste0("sample",1:4), each=95)
  rows$source_sample_id <- rows$sample_key
  rows$source_parameter_code <- rep(paste0("p",1:95),4)
  rows$source_parameter_name <- rows$source_parameter_code
  rows$source_unit <- "mg/L"
  rows$sample_group_id <- rep(41:44, each=95)
  rows$location_id <- rep(c(1L,2L,3L,NA_integer_), each=95)
  assigned <- rep(1:95 %in% c(8,34,57,81),4)
  rows$parameter_id[assigned] <- 36L
  original <- rows
  plan <- env$addDiscData_upload_plan(rows)
  expect_identical(rows, original)
  expect_equal(c(plan$total_results,plan$included_results,plan$omitted_results), c(380,16,364))
  expect_equal(c(plan$total_samples,plan$included_samples), c(4,4))
  expect_equal(sum(plan$omissions$results), 364)
  expect_equal(nrow(plan$omissions),91)
  expect_identical(plan$df$sample_group_id, rep(41:44,each=4))
  expect_true(all(is.na(plan$df$location_id[plan$df$sample_key=="sample4"])))
  expect_equal(nrow(plan$omitted_samples),0)
})

test_that("manual shells and result-flag skips have separate inclusion rules", {
  env <- partial_upload_environment()
  rows <- env$addDiscData_empty_table()[rep(NA_integer_,7), ]
  rows$source_code <- c("YGwater-manual","YGwater-manual",rep("review",5))
  rows$sample_key <- paste0("s",1:7)
  rows$source_sample_id <- rows$sample_key
  rows$source_parameter_code <- paste0("p",1:7)
  rows$parameter_id <- c(NA_integer_,36L,36L,NA_integer_,NA_integer_,36L,0L)
  rows$result_flag_action[c(3,4)] <- "skip_result"
  plan <- env$addDiscData_upload_plan(rows)
  expect_equal(plan$total_results,6)
  expect_equal(plan$included_results,3)
  expect_equal(plan$omitted_results,3)
  expect_equal(plan$flag_skipped_results,2)
  expect_equal(plan$unmapped_results,1)
  expect_identical(plan$df$sample_key,c("s2","s6","s7"))
  expect_identical(plan$df$parameter_id,c(36L,36L,0L))
  expect_setequal(plan$omitted_samples$sample_key,c("s3","s4","s5"))
})

test_that("mapped analytical errors are not silently omitted", {
  env <- partial_upload_environment()
  rows <- env$addDiscData_empty_table()[NA_integer_, ]
  rows$source_code <- "review"
  rows$sample_key <- "s1"
  rows$parameter_id <- 36L
  rows$endpoint_mode <- "fixed"
  rows$endpoint_ph <- NA_real_
  plan <- env$addDiscData_upload_plan(rows)
  expect_equal(plan$included_results,1)
  expect_equal(plan$omitted_results,0)
  expect_error(AquaCache::validateResultChemistry(NULL,plan$df,
    parameters=data.frame(parameter_id=36L)),"finite pH")
})

test_that("empty and entirely unmapped previews cannot produce uploadable rows", {
  env <- partial_upload_environment()
  plan <- env$addDiscData_upload_plan(env$addDiscData_empty_table())
  expect_equal(plan$total_results,0)
  expect_equal(nrow(plan$df),0)
  rows <- env$addDiscData_empty_table()[NA_integer_, ]
  rows$source_code <- "review"
  rows$sample_key <- "s1"
  rows$source_sample_id <- "s1"
  plan <- env$addDiscData_upload_plan(rows)
  expect_equal(plan$omitted_results,1)
  expect_equal(nrow(plan$df),0)
  expect_identical(plan$omitted_samples$sample_key,"s1")
})

test_that("the upload worker requires acknowledgement before connecting", {
  env <- partial_upload_environment()
  outcome <- env$addDiscData_run_upload(list(
    upload_coverage=list(omitted_result_rows=1L),partial_upload_acknowledged=FALSE))
  expect_false(outcome$ok)
  expect_match(outcome$message,"Acknowledge")
})

test_that("fully mapped previews retain every result without omissions", {
  env <- partial_upload_environment()
  rows <- env$addDiscData_empty_table()[rep(NA_integer_,3), ]
  rows$source_code <- "review"
  rows$sample_key <- c("s1","s1","s2")
  rows$source_sample_id <- rows$sample_key
  rows$parameter_id <- c(36L,37L,36L)
  rows$result <- c(0.1,0.2,0.3)
  plan <- env$addDiscData_upload_plan(rows)
  expect_equal(c(plan$total_results,plan$included_results,plan$omitted_results),c(3,3,0))
  expect_identical(plan$df$parameter_id,rows$parameter_id)
  expect_identical(plan$df$result,rows$result)
  expect_equal(nrow(plan$omissions),0)
  expect_equal(nrow(plan$omitted_samples),0)
})

test_that("selecting one sample records other samples as explicit omissions", {
  env <- partial_upload_environment()
  rows <- env$addDiscData_empty_table()[rep(NA_integer_, 4), ]
  rows$source_code <- "review"
  rows$sample_key <- rep(c("s1", "s2"), each = 2)
  rows$source_sample_id <- rows$sample_key
  rows$parameter_id <- c(36L, NA_integer_, 36L, 37L)
  original <- rows
  plan <- env$addDiscData_upload_plan(rows, "s1")
  expect_identical(rows, original)
  expect_identical(plan$df$sample_key, "s1")
  expect_equal(c(plan$total_results, plan$included_results, plan$omitted_results), c(4, 1, 3))
  expect_equal(c(plan$sample_excluded_results, plan$unmapped_results), c(2, 1))
  expect_equal(sum(plan$omissions$results), 3)
  expect_identical(plan$omitted_samples$sample_key, "s2")
  expect_equal(nrow(env$addDiscData_upload_plan(rows, character())$df), 0)
  expect_equal(nrow(env$addDiscData_upload_plan(rows, "missing")$df), 0)
})
