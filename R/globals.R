# globals for aq_download (aquarius timeseries QPI scripts)
utils::globalVariables(c("timeseries"))

# globals for Shiny applications that source a script
utils::globalVariables(c(
  "floodAtlas_ts_globals",
  "floodAtlas_over_globals",
  "YGwater_globals"
))


#globals related to use of data.table (anywhere in the package)
utils::globalVariables(c("."))

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
