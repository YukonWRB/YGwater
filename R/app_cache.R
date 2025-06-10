#' In-memory cache for Shiny app data
#'
#' @description
#' Provides global helper functions to store and retrieve cached data across user sessions. Each cached item includes a timestamp and is automatically refreshed when the specified time-to-live (TTL) has expired.
#'
#' @param key A unique character string identifying the cached data.
#' @param fetch_fun A function that returns the value to cache when the current cache entry is missing or expired.
#' @param ttl Time-to-live for the cached value in seconds. Defaults to 3600 seconds (1 hour).
#'
#' @return The cached value for `key`.
#' @export

get_cached <- function(key, fetch_fun, ttl = 3600) {
  if (!exists("app_cache", envir = .GlobalEnv)) {
    assign("app_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  cache_env <- get("app_cache", envir = .GlobalEnv)

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
#' @export

clear_cached <- function(key) {
  if (exists("app_cache", envir = .GlobalEnv)) {
    cache_env <- get("app_cache", envir = .GlobalEnv)
    if (exists(key, envir = cache_env, inherits = FALSE)) {
      rm(list = key, envir = cache_env)
    }
  }
}
