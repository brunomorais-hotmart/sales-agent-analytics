#' Make requests to Hotmart Astrobox API (Environment-aware)
#' @param query_id The query ID to execute
#' @param parameters Named list of parameters to pass to the query
#' @param token Bearer token (if NULL, will try to authenticate)
#' @param client_name Client name for the request
#' @param method HTTP method
#' @param body Request body for POST requests
#' @param env Environment: "production", "staging", "production_internal"
#' @param return_df Whether to return a data frame (TRUE) or raw list (FALSE)
astrobox_request <- function(query_id, parameters = NULL, token = NULL, client_name = "R-Client",
                             method = "GET", body = NULL, env = "production", return_df = FALSE, ...) {
  
  # Environment-specific API endpoints
  api_endpoints <- list(
    production = "https://api-astrobox.hotmart.com/v1/executor/reactive/by-id",
    staging = "https://api-astrobox-staging.hotmart.com/v1/executor/reactive/by-id",  # hypothetical
    production_internal = "https://api-astrobox-internal.hotmart.com/v1/executor/reactive/by-id"  # hypothetical
  )
  
  # Get the correct API endpoint
  base_url <- api_endpoints[[env]]
  if (is.null(base_url)) {
    stop("❌ Invalid environment: ", env, ". Use: ", paste(names(api_endpoints), collapse = ", "))
  }
  
  if (is.null(token)) {
    message("🔄 No token provided, authenticating for ", env, "...")
    token <- .get_cached_token(env) %||% hotmart_auth(env = env, ...)
  }
  
  # Build the URL with parameters for GET requests
  url <- paste0(base_url, "/", query_id)
  if (!is.null(parameters) && length(parameters) > 0 && toupper(method) == "GET") {
    url <- .build_url_with_parameters(url, parameters)
  }
  
  headers <- add_headers(
    "Authorization" = paste("Bearer", token),  
    "Content-Type" = "application/x-ndjson",
    "X-Client-Name" = client_name
  )
  
  message("🚀 Making API request to ", env, " query: ", query_id)
  if (!is.null(parameters) && length(parameters) > 0) {
    message("📋 Parameters: ", paste(names(parameters), "=", parameters, collapse = ", "))
  }
  
  response <- switch(toupper(method),
                     "GET" = GET(url, headers),
                     "POST" = POST(url, headers, body = body),
                     stop("Unsupported method: ", method)
  )
  
  if (status_code(response) == 401) {
    message("🔄 Token expired, re-authenticating for ", env, "...")
    token <- hotmart_auth(env = env, ...)
    headers <- add_headers(
      "Authorization" = paste("Bearer", token),
      "Content-Type" = "application/x-ndjson", 
      "X-Client-Name" = client_name
    )
    response <- switch(toupper(method),
                       "GET" = GET(url, headers),
                       "POST" = POST(url, headers, body = body)
    )
  }
  
  stop_for_status(response)
  message("✅ Request successful!")
  
  # Get raw content
  result <- content(response, "parsed")
  
  # Convert to data frame if requested
  if (return_df) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("📦 dplyr package is required for data frame conversion. Install with: install.packages('dplyr')")
    }
    message("📊 Converting to data frame...")
    result <- .convert_to_dataframe_preserve_columns(result)
  }
  
  return(result)
}

#' Helper function to convert list to data frame while preserving all columns
.convert_to_dataframe_preserve_columns <- function(data_list) {
  if (length(data_list) == 0) {
    return(data.frame())
  }
  
  # Get all unique column names across all rows
  all_columns <- unique(unlist(lapply(data_list, names)))
  
  # Ensure each row has all columns (fill missing with NA) AND replace NULLs with NAs
  standardized_list <- lapply(data_list, function(row) {
    # Step 1: Add missing columns as NA
    missing_cols <- setdiff(all_columns, names(row))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        row[[col]] <- NA
      }
    }
    
    # Step 2: Replace NULL values in existing columns with NA
    row <- lapply(row, function(x) if(is.null(x)) NA else x)
    
    # Step 3: Ensure consistent column order
    row <- row[all_columns]
    
    return(row)
  })
  
  # Now bind_rows will preserve all columns
  result_df <- dplyr::bind_rows(standardized_list)
  
  return(result_df)
}

#' Helper function to build URL with parameters
.build_url_with_parameters <- function(base_url, parameters) {
  if (is.null(parameters) || length(parameters) == 0) {
    return(base_url)
  }
  
  # Handle different parameter types
  param_strings <- character(0)
  
  for (name in names(parameters)) {
    value <- parameters[[name]]
    
    # Handle arrays/vectors
    if (length(value) > 1) {
      # For arrays, repeat the parameter name for each value
      for (v in value) {
        param_strings <- c(param_strings, paste0(name, "=", URLencode(as.character(v), reserved = TRUE)))
      }
    } else {
      # Single value
      param_strings <- c(param_strings, paste0(name, "=", URLencode(as.character(value), reserved = TRUE)))
    }
  }
  
  if (length(param_strings) > 0) {
    query_string <- paste(param_strings, collapse = "&")
    return(paste0(base_url, "?", query_string))
  }
  
  return(base_url)
}