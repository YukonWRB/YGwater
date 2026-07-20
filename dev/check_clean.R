check_clean <- function(path = ".") {
  src <- normalizePath(path, winslash = "/", mustWork = TRUE)
  dst <- file.path(tempdir(), paste0("YGwater-check-", Sys.getpid()))

  unlink(dst, recursive = TRUE, force = TRUE)
  dir.create(dst, recursive = TRUE)

  top_level <- list.files(src, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  exclude <- basename(top_level) %in%
    c(
      ".git",
      ".codex",
      ".agents",
      ".Rproj.user",
      ".vscode",
      "..Rcheck",
      ".RData",
      ".Rhistory"
    )

  file.copy(top_level[!exclude], dst, recursive = TRUE, copy.date = TRUE)

  devtools::check(
    pkg = dst,
    document = FALSE,
    manual = FALSE,
    vignettes = FALSE,
    args = c(
      "--no-tests",
      "--no-examples",
      "--no-vignettes",
      "--no-build-vignettes"
    ),
    build_args = "--no-build-vignettes",
    error_on = "never"
  )

  unlink(dst, recursive = TRUE, force = TRUE)
}


check_clean()
