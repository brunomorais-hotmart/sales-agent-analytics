#' Set up Hotmart credentials with fallback options
setup_hotmart_credentials <- function(use_keyring = TRUE) {
  cat("🔧 Setting up Hotmart credentials...\n")
  
  username <- readline("Enter your Hotmart username: ")
  password <- getPass::getPass("Enter your Hotmart password: ")
  
  # Try keyring first, fallback to environment variables
  if (use_keyring) {
    tryCatch({
      keyring::key_set("hotmart", username, password)
      Sys.setenv(HOTMART_USERNAME = username)
      cat("✅ Credentials stored in Windows Credential Manager!\n")
      return(invisible(TRUE))
    }, error = function(e) {
      cat("⚠️  Keyring failed, falling back to environment variables...\n")
      use_keyring <<- FALSE
    })
  }
  
  # Fallback: Use environment variables and .Renviron
  if (!use_keyring) {
    # Store in current session
    Sys.setenv(HOTMART_USERNAME = username)
    Sys.setenv(HOTMART_PASSWORD = password)
    
    # Also save to .Renviron file for persistence
    renviron_file <- file.path(Sys.getenv("HOME"), ".Renviron")
    
    # Read existing .Renviron
    if (file.exists(renviron_file)) {
      existing <- readLines(renviron_file)
      # Remove existing Hotmart entries
      existing <- existing[!grepl("^HOTMART_", existing)]
    } else {
      existing <- character(0)
    }
    
    # Add new entries
    new_entries <- c(
      paste0("HOTMART_USERNAME=", username),
      paste0("HOTMART_PASSWORD=", password)
    )
    
    # Write back
    writeLines(c(existing, new_entries), renviron_file)
    
    cat("✅ Credentials stored in environment variables!\n")
    cat("💡 Note: Restart R or run readRenviron('~/.Renviron') to load in new sessions.\n")
  }
  
  cat("You can now use hotmart_auth() without parameters.\n")
}

#' Get stored credentials with multiple fallbacks
.get_stored_credentials <- function() {
  username <- Sys.getenv("HOTMART_USERNAME")
  
  if (username == "") {
    username <- readline("Enter Hotmart username: ")
    Sys.setenv(HOTMART_USERNAME = username)
  }
  
  # Try to get password from multiple sources
  password <- .get_stored_password(username)
  
  return(list(username = username, password = password))
}

.get_stored_password <- function(username) {
  # Method 1: Environment variable
  password <- Sys.getenv("HOTMART_PASSWORD")
  if (password != "") {
    return(password)
  }
  
  # Method 2: Try keyring
  tryCatch({
    password <- keyring::key_get("hotmart", username)
    if (password != "") return(password)
  }, error = function(e) {
    # Keyring failed, continue to next method
  })
  
  # Method 3: Ask user
  password <- getPass::getPass(paste0("Enter password for ", username, ": "))
  
  # Try to store for next time (environment variable)
  Sys.setenv(HOTMART_PASSWORD = password)
  
  return(password)
}

#' Clear stored credentials (all methods)
clear_hotmart_credentials <- function() {
  username <- Sys.getenv("HOTMART_USERNAME")
  
  # Clear keyring
  if (username != "") {
    tryCatch({
      keyring::key_delete("hotmart", username)
    }, error = function(e) {
      # Keyring might not exist, that's OK
    })
  }
  
  # Clear environment variables
  Sys.unsetenv("HOTMART_USERNAME")
  Sys.unsetenv("HOTMART_PASSWORD")
  
  # Clear from .Renviron
  renviron_file <- file.path(Sys.getenv("HOME"), ".Renviron")
  if (file.exists(renviron_file)) {
    existing <- readLines(renviron_file)
    cleaned <- existing[!grepl("^HOTMART_", existing)]
    writeLines(cleaned, renviron_file)
  }
  
  # Clear cached token
  token_file <- file.path(tempdir(), "hotmart_token.rds")
  if (file.exists(token_file)) file.remove(token_file)
  
  message("🗑️ All Hotmart credentials cleared!")
}