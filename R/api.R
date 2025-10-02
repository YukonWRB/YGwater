#' Run the YGwater API
#'
#' @description
#' Launches the YGwater API using the plumber package.
#'
#' @param host The host address to bind the server to. Default is '0.0.0.0'.
#' @param port The port number to listen on. Default is 8000.
#' @return Runs the API server.
#' @export
#' @examples
#' \dontrun{
#' run_api()
#' }

api <- function(host = "0.0.0.0", port = 8000) {
  rlang::check_installed("plumber", reason = "required to run the YGwater API")

  api_path <- system.file("api", "plumber.R", package = "YGwater")
  if (api_path == "") {
    # when running from source
    api_path <- "inst/api/plumber.R"
  }
  pr <- plumber::plumb(api_path)
  pr$run(host = host, port = port)
}
