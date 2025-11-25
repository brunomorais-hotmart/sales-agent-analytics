library(httr)

#' Hotmart Astrobox Authentication
#' @param username Hotmart username
#' @param password Hotmart password  
#' @param client_id Application client ID (optional)
#' @param client_secret Application client secret (optional)
#' @param env Environment: "production", "staging", "production_internal"
#' @param save_token Whether to cache the token
hotmart_auth <- function(username = NULL, password = NULL, 
                         client_id = NULL, client_secret = NULL,
                         env = "production", save_token = TRUE) {
  
  # Check for cached token first
  if (is.null(username) && is.null(password) && is.null(client_id)) {
    cached_token <- .get_cached_token()
    if (!is.null(cached_token)) {
      message("🎯 Using cached token...")
      return(cached_token)
    }
  }
  
  # Environment configurations
  config <- list(
    production = list(
      security_uri = "https://api-sec-vlc.hotmart.com/security/oauth/token",
      auth_token = "MTJmYzVlZGUtNzRmYS00MWVjLTg4NmYtYzM4YzA4YjcxMGZmOjE4MjFhOGRlLTMxNDgtNGU3ZC05OGZmLWZkY2NiOGY0MzkzNw=="
    ),
    staging = list(
      security_uri = "https://api-security.buildstaging.com/security/oauth/token", 
      auth_token = "YjYwMjRkNDctZWNmYi00ZDBkLWEyYTUtZmIzMjM1NTdmZDZkOjhjNGM4OTc4LTBmZTYtNGMyZS1iYzBiLTQ3MjI1YTg2MGQwNQ=="
    ),
    production_internal = list(
      security_uri = "https://api-sec-vlc-internal.hotmart.com/security/oauth/token",
      auth_token = "MTJmYzVlZGUtNzRmYS00MWVjLTg4NmYtYzM4YzA4YjcxMGZmOjE4MjFhOGRlLTMxNDgtNGU3ZC05OGZmLWZkY2NiOGY0MzkzNw=="
    )
  )
  
  env_config <- config[[env]]
  if (is.null(env_config)) {
    stop("Invalid environment. Use: ", paste(names(config), collapse = ", "))
  }
  
  # Application authentication
  if (!is.null(client_id) && !is.null(client_secret)) {
    message("🔐 Using application authentication...")
    token <- .get_app_token(client_id, client_secret, env_config)
  }
  # User authentication with provided credentials
  else if (!is.null(username) && !is.null(password)) {
    message("👤 Using provided user credentials...")
    token <- .get_user_token(username, password, env_config)
  }
  # Try to get from stored credentials
  else {
    message("🔑 Attempting to use stored credentials...")
    creds <- .get_stored_credentials()
    token <- .get_user_token(creds$username, creds$password, env_config)
  }
  
  if (save_token) {
    .save_token(token)
  }
  
  message("✅ Authentication successful!")
  return(token)
}

# Helper functions
.get_user_token <- function(username, password, env_config) {
  body <- list(
    grant_type = "password", 
    username = username,
    password = password
  )
  
  response <- POST(
    env_config$security_uri,
    add_headers(
      "Authorization" = paste("Basic", env_config$auth_token),
      "cache-control" = "no-cache"
    ),
    body = body,
    encode = "form"
  )
  
  if (status_code(response) != 200) {
    error_content <- content(response, "parsed")
    error_msg <- error_content$error_description %||% 
      error_content$error %||% 
      "Unknown authentication error"
    stop("❌ Authentication failed: ", error_msg)
  }
  
  return(content(response)$access_token)
}

.get_app_token <- function(client_id, client_secret, env_config) {
  library(base64enc)
  credentials <- paste0(client_id, ":", client_secret)
  encoded_credentials <- base64encode(charToRaw(credentials))
  
  uri <- paste0(env_config$security_uri, "?grant_type=client_credentials")
  
  response <- POST(
    uri,
    add_headers("Authorization" = paste("Basic", encoded_credentials)),
    timeout(10)
  )
  
  if (status_code(response) != 200) {
    error_content <- content(response, "parsed")
    error_msg <- error_content$error_description %||% 
      error_content$error %||% 
      "Unknown authentication error"
    stop("❌ Application authentication failed: ", error_msg)
  }
  
  return(content(response)$access_token)
}

.save_token <- function(token, env = "production") {
  token_file <- file.path(tempdir(), paste0("hotmart_token_", env, ".rds"))
  saveRDS(list(token = token, timestamp = Sys.time(), env = env), token_file)
}

.get_cached_token <- function(env = "production") {
  token_file <- file.path(tempdir(), paste0("hotmart_token_", env, ".rds"))
  if (file.exists(token_file)) {
    token_data <- readRDS(token_file)
    # Check if token is less than 1 hour old and matches environment
    if (difftime(Sys.time(), token_data$timestamp, units = "hours") < 1 && 
        token_data$env == env) {
      return(token_data$token)
    }
  }
  return(NULL)
}

# Utility operator
`%||%` <- function(x, y) if (is.null(x)) y else x