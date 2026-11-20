#' Create a unique generation for a dynamically rendered Shiny UI.
#'
#' A process-local counter avoids repeated values when several renders happen
#' within the same clock tick. It does not change R's random-number state.
#' @return A character scalar identifying one UI render.
#' @noRd
shiny_ui_ready_token <- local({
  generation <- 0
  function() {
    generation <<- generation + 1
    sprintf("%.0f", generation)
  }
})

#' Acknowledge binding of a dynamically rendered Shiny UI.
#'
#' Put this marker in the same renderUI result (or modal) as the inputs being
#' updated. Observe its value on the server before sending initial choices.
#' Shiny submits the marker with the other inputs after binding the subtree;
#' server-side onFlushed() alone does not establish this browser-side ordering.
#'
#' Each render gets a distinct token, so recreation after a language change is
#' acknowledged even when the visible inputs have the same default values.
#' For one-time initialization registered before a replacement UI is rendered,
#' use observeEvent(..., once = TRUE, ignoreInit = TRUE) to ignore the previous
#' module instance's token until the new subtree reports its own generation.
#' @param input_id Namespaced input ID for the acknowledgement.
#' @param token Optional generation, for matching a particular modal instance.
#' @return A hidden Shiny text input.
#' @noRd
shiny_ui_ready <- function(input_id, token = shiny_ui_ready_token()) {
  shiny::tags$div(
    style = "display:none;",
    `aria-hidden` = "true",
    shiny::textInput(input_id, NULL, value = token)
  )
}
