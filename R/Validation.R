# validation.R contents:
#' Validate Census Data Structure
#'
#' @param data Data frame to validate
#' @param year Census year
#' @return Logical indicating if data is valid
#' @keywords internal
validate_census_data <- function(data, year) {
  required_cols <- c("state", "county", "county_subdivision",
                     "municipality_name", "county_name")

  # Check required columns
  missing_cols <- setdiff(required_cols, names(data))
  if(length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Validate data types
  numeric_cols <- grep("_years$|^Total", names(data), value = TRUE)
  for(col in numeric_cols) {
    if(!is.numeric(data[[col]])) {
      stop("Column ", col, " should be numeric")
    }
  }

  # Validate geographic identifiers
  if(any(is.na(data$county)) || any(is.na(data$county_subdivision))) {
    stop("Missing geographic identifiers")
  }

  TRUE
}

#' Validate API Response
#'
#' @param response HTTP response object
#' @param pattern API pattern used
#' @return Logical indicating if response is valid
#' @keywords internal
validate_api_response <- function(response, pattern) {
  if(is.null(response) || is.null(response$content)) {
    stop("Empty API response for pattern: ", pattern)
  }

  if(response$status_code != 200) {
    stop("API error: ", httr::http_status(response)$message,
         " for pattern: ", pattern)
  }

  # Validate content structure
  content <- tryCatch({
    jsonlite::fromJSON(rawToChar(response$content))
  }, error = function(e) {
    stop("Invalid JSON response for pattern: ", pattern)
  })

  if(length(content) < 2 || !is.matrix(content)) {
    stop("Unexpected response structure for pattern: ", pattern)
  }

  TRUE
}

#' Validate Geographic Reference Data
#'
#' @param geo_ref Geographic reference data frame
#' @return Logical indicating if data is valid
#' @keywords internal
validate_geo_reference <- function(geo_ref) {
  required_cols <- c("COUNTYFP", "municipality_name", "county_name")

  # Check required columns
  missing_cols <- setdiff(required_cols, names(geo_ref))
  if(length(missing_cols) > 0) {
    stop("Missing required columns in geographic reference data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check for duplicate geographic identifiers
  dupes <- duplicated(geo_ref[c("COUNTYFP", "municipality_name")])
  if(any(dupes)) {
    stop("Duplicate geographic identifiers found in reference data")
  }

  TRUE
}
