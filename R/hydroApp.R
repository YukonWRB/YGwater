#' Hydrometric data, plot, + map viewing app
#' 
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' Easy app-type interface for viewing forecaster of the day comments, level/flow plots, precipitation maps, and more. Data can be exported as .csv or .png file formats with the click of a button. Requires access to the G drive for full functionality.
#'
#' @param ... Do not use. Arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
hydroApp <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
  
}
