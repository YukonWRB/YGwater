# Build a small, serializable environment for helper functions used by mirai
# worker expressions. Helpers are copied from the calling module environment
# so worker tasks do not inherit Shiny sessions or reactive state.

mirai_worker_environment <- function(
  helper_names,
  source_env = parent.frame(),
  parent = asNamespace("YGwater")
) {
  worker_env <- new.env(parent = parent)
  pending <- unique(as.character(helper_names))
  copied <- character()

  find_binding_environment <- function(name) {
    env <- source_env
    while (is.environment(env) && !identical(env, emptyenv())) {
      if (exists(name, envir = env, inherits = FALSE)) {
        return(env)
      }
      if (identical(env, baseenv())) {
        break
      }
      env <- parent.env(env)
    }
    NULL
  }

  while (length(pending) > 0L) {
    name <- pending[[1L]]
    pending <- pending[-1L]
    if (
      name %in% copied ||
        exists(name, envir = worker_env, inherits = FALSE)
    ) {
      next
    }
    binding_env <- find_binding_environment(name)
    if (is.null(binding_env)) {
      next
    }
    helper <- get(name, envir = binding_env, inherits = FALSE)
    if (!is.function(helper) || is.primitive(helper)) {
      next
    }

    worker_helper <- helper
    environment(worker_helper) <- worker_env
    assign(name, worker_helper, envir = worker_env)
    copied <- c(copied, name)

    dependencies <- tryCatch(
      codetools::findGlobals(helper, merge = FALSE)$functions,
      error = function(error) character()
    )
    pending <- unique(c(pending, setdiff(dependencies, copied)))
  }

  worker_env
}
