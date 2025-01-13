#' Generate Patterns for Census API
#'
#' @param initial_pattern Initial pattern string
#' @param total_patterns Total number of patterns needed
#' @param num_parts Number of parts to split patterns into
#' @param year Census year
#' @return List of pattern strings
#' @keywords internal
generate_patterns <- function(initial_pattern, total_patterns, num_parts, year) {
  patterns_per_part <- floor(total_patterns / num_parts)
  patterns <- vector("list", num_parts)
  counter <- 0

  # Generate patterns for each part
  for(i in 1:num_parts) {
    temp_patterns <- character(0)
    for(j in 1:patterns_per_part) {
      pattern <- extract_pattern(initial_pattern, counter, year)
      temp_patterns <- c(temp_patterns, pattern)
      counter <- counter + 1
    }
    patterns[[i]] <- paste(temp_patterns, collapse = ",")
  }

  # Handle remaining patterns if any
  remaining <- total_patterns - (patterns_per_part * num_parts)
  if(remaining > 0) {
    remaining_patterns <- sapply((counter):(total_patterns-1),
                                 function(x) extract_pattern(initial_pattern, x, year))
    patterns[[num_parts]] <- paste(c(unlist(strsplit(patterns[[num_parts]], ",")),
                                     remaining_patterns), collapse = ",")
  }

  return(patterns)
}

#' Convert and Process Census API Data
#'
#' @param endpoint API endpoint URL
#' @param query Query parameters
#' @param pattern_list List of patterns
#' @param filter_sub Filter substring
#' @param max_retries Number of retry attempts for failed requests
#' @return List of processed data frames
#' @import httr
#' @import jsonlite
#' @keywords internal
convert_process_df <- function(endpoint, query, pattern_list, filter_sub, max_retries = 3) {
  df_list <- list()

  for(pattern in pattern_list) {
    url <- paste0(endpoint, query, pattern, filter_sub)
    response <- NULL
    data <- NULL

    # Retry logic
    for(attempt in 1:max_retries) {
      tryCatch({
        response <- GET(url)
        if(response$status_code == 200) {
          data <- fromJSON(rawToChar(response$content))
          break
        }
        if(attempt == max_retries) {
          stop(sprintf("Failed to fetch data after %d attempts. Status: %d",
                       max_retries, response$status_code))
        }
        Sys.sleep(2^attempt) # Exponential backoff
      }, error = function(e) {
        if(attempt == max_retries) stop(e)
        message(sprintf("Attempt %d failed. Retrying...", attempt))
        Sys.sleep(2^attempt)
      })
    }

    # Process successful response
    if(!is.null(data)) {
      # Convert to dataframe
      df <- as.data.frame(data[-1,], stringsAsFactors = FALSE)
      names(df) <- data[1,]
      numeric_cols <- grep("PCT", names(df))
      df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)
      df_list[[length(df_list) + 1]] <- df
    }
  }

  return(df_list)
}
