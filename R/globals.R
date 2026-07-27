# globals for aq_download (aquarius timeseries QPI scripts)
utils::globalVariables(c("timeseries"))

# globals for Shiny applications that source a script
utils::globalVariables(c(
  "floodAtlas_ts_globals",
  "floodAtlas_over_globals",
  "YGwater_globals"
))

# globals related to non-standard evaluation in data.table and plotly formulas
utils::globalVariables(c(
  ".",
  "..expected_cols",
  ".band_run",
  ".bin",
  ".line_run",
  ".row_id",
  ".run",
  "bin_end",
  "bin_end_utc",
  "bin_label",
  "bin_order",
  "bin_seconds",
  "bin_start",
  "bin_start_utc",
  "channel_num",
  "completeness",
  "completeness_text",
  "coverage_seconds",
  "covered_seconds",
  "datetime",
  "density_key",
  "depth",
  "Depth (m)",
  "duration_seconds",
  "fallback_step",
  "hover_x",
  "i.bin_label",
  "i.coverage_seconds",
  "i.datetime",
  "i.duration_seconds",
  "i.value",
  "imputed",
  "inferred_step_seconds",
  "interval_seconds",
  "lead_seconds",
  "len",
  "location",
  "period",
  "period_seconds",
  "plot_year",
  "Pressure (m)",
  "result",
  "run",
  "sample_id",
  "season_end_utc",
  "season_start_utc",
  "step_seconds",
  "SWE",
  "target_datetime",
  "value",
  "x",
  "x.bin_end_utc",
  "x.bin_order",
  "x.bin_start_utc",
  "x.plot_year",
  "year_group"
))

# Function to find global variables in a function and print them in a format that can be copied and pasted into the R script as utils::globalVariables(c(...))
# globs <- function(fun,
#                   pkg = rprojroot::find_package_root_file(),
#                   ignore = c(":=", ".SD", ".BY", ".N", ".I", ".GRP"))
# {
#   pkg <- basename(pkg)
#   ns  <- c(
#     ls(pkgload::pkg_env("base")),
#     ls(pkgload::ns_env(pkg)),
#     ls(pkgload::imports_env(pkg)),
#     ignore
#   )
#
#   res <- sort(setdiff(
#     codetools::findGlobals(fun),
#     ns
#   ))
#   cat(strwrap(paste0("utils::globalVariables(c(", paste('"', res, '"', collapse = ", ", sep = ""), "))"), width = 80), sep = "\n")
#   return(res)
# }
#
#
# #example
# globs(YGwater::plotOverlap)
