# Backward-compatible launcher for existing scheduled tasks.
# The complete schema setup and comment-scrape workflow now lives in
# create_comments_schema.R.

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grepl("^--file=", script_arg)])
if (!length(script_path)) script_path <- "upload_new_forecaster_comments.R"
source(file.path(dirname(normalizePath(script_path[[1L]])), "create_comments_schema.R"))

if (sys.nframe() == 0L) {
  nDays <- suppressWarnings(as.integer(Sys.getenv("FORECASTER_COMMENTS_N_DAYS", "-1")))
  if (is.na(nDays) || nDays < -1L) nDays <- -1L
  run_forecaster_commentary(nDays = nDays)
}
