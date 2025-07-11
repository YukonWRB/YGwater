#' In-memory cache for Shiny app data
#'
#' @description
#' Provides global helper functions to store and retrieve cached data across user sessions. Each cached item includes a timestamp and is automatically refreshed when the specified time-to-live (TTL) has expired.
#'
#' @param key A unique character string identifying the cached data.
#' @param fetch_fun A function that returns the value to cache when the current cache entry is missing or expired.
#' @param ttl Time-to-live for the cached value in seconds. Defaults to 3600 seconds (1 hour).
#' @param env The environment in which to store the cache. Defaults to `.GlobalEnv`, but can be set to a different environment if needed, such as session$userData in a Shiny app so that the cache is specific to that session.
#'
#' @return The cached value for `key`.
#' @export

get_cached <- function(key, fetch_fun, ttl = 3600, env = .GlobalEnv) {
  if (!exists("app_cache", envir = env)) {
    assign("app_cache", new.env(parent = emptyenv()), envir = env)
  }
  cache_env <- get("app_cache", envir = env)

  # Check if the key exists and if the cached value is still valid. If it exists and is valid, return the cached value. If it exists but is expired, fetch a new value. If it does not exist, fetch a new value.
  if (exists(key, envir = cache_env, inherits = FALSE)) {
    entry <- get(key, envir = cache_env, inherits = FALSE)
    if (difftime(Sys.time(), entry$timestamp, units = "secs") < ttl) {
      return(entry$value)
    }
  }

  value <- fetch_fun()
  assign(key, list(value = value, timestamp = Sys.time()), envir = cache_env)
  value
}

#' Remove a value from the cache
#'
#' @param key The cached key to remove.
#' @param env The environment from which to remove the cached key. Defaults to `.GlobalEnv`.
#' @export

clear_cached <- function(key, env = .GlobalEnv) {
  if (exists("app_cache", envir = env)) {
    cache_env <- get("app_cache", envir = env)
    if (exists(key, envir = cache_env, inherits = FALSE)) {
      rm(list = key, envir = cache_env)
    }
  }
}
