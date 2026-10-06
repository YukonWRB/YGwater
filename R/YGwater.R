#' Launch the AquaLink Shiny application (deprecated name)
#'
#' `YGwater()` remains available for compatibility and forwards all arguments
#' to [AquaLink()]. It warns on every call to encourage migration to the new
#' application name.
#'
#' @param ... Arguments passed on to [AquaLink()].
#' @return Opens a Shiny application.
#' @seealso [AquaLink()]
#' @export
YGwater <- function(...) {
  message(
    "WARNING: the `YGwater()` function is deprecated and will be removed in a future release.\n",
    "Please use `AquaLink()` instead."
  )

  .Deprecated(
    "AquaLink",
    package = "YGwater",
    msg = "The `YGwater()` function is deprecated; please use `AquaLink()` instead."
  )
  AquaLink(...)
}
