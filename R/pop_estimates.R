#' Process Population Estimates Data for New Jersey
#'
#' @description Downloads and processes population estimates data for New Jersey counties.
#'   Data is obtained from the Census Bureau's Population Estimates Program.
#'
#' @param years Numeric vector. Years to process data for (e.g., c(2021, 2022, 2023)).
#' @param file_types Character vector. Types of estimates files to process.
#'   Options include 'agesex' (age and sex), 'alldata' (all demographic data),
#'   and 'syasex' (single-year age and sex).
#' @param state_fips Character. State FIPS code. Default is "34" for New Jersey.
#' @param save_to_db Logical. Whether to save data to the census database. Default is TRUE.
#'
#' @return A list of data frames, one for each file type requested.
#'   If save_to_db is TRUE, also saves to the census database.
#'
#' @import dplyr readr
#' @importFrom purrr map map2 compact set_names
#' @export
#'
#' @examples
#' \dontrun{
#' # Process population estimates for 2021-2023, all file types
#' pop_data <- process_pop_estimates(c(2021, 2022, 2023))
#'
#' # Process only age-sex data for 2022
#' pop_data <- process_pop_estimates(2022, file_types = "agesex")
#' }
process_pop_estimates <- function(years = c(2021, 2022, 2023),
                                  file_types = c('agesex', 'alldata', 'syasex'),
                                  state_fips = "34",
                                  save_to_db = TRUE) {

  # Validate inputs
  if(!all(years >= 2020)) {
    stop("Years must be 2020 or later for this dataset.")
  }

  valid_file_types <- c('agesex', 'alldata', 'syasex')
  if(!all(file_types %in% valid_file_types)) {
    stop("Invalid file type. Must be one of: ", paste(valid_file_types, collapse = ", "))
  }

  # Handle special case for syasex (only available for 2023)
  if("syasex" %in% file_types && any(years < 2023)) {
    warning("Single-year age and sex (syasex) files are only available for 2023 and later.")
    # Filter years for syasex
    syasex_years <- years[years >= 2023]
    other_file_types <- setdiff(file_types, "syasex")

    # Process other file types with all years
    results_other <- list()
    if(length(other_file_types) > 0) {
      results_other <- process_pop_estimates(
        years = years,
        file_types = other_file_types,
        state_fips = state_fips,
        save_to_db = save_to_db
      )
    }

    # Process syasex with valid years only
    results_syasex <- list()
    if(length(syasex_years) > 0) {
      results_syasex <- process_pop_estimates(
        years = syasex_years,
        file_types = "syasex",
        state_fips = state_fips,
        save_to_db = save_to_db
      )
    }

    # Combine results
    return(c(results_other, results_syasex))
  }

  message("Processing population estimates for years: ", paste(years, collapse = ", "))

  # Function to download and process a single file
  process_file <- function(year, file_type) {
    base_url <- sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%s/counties/asrh/", year)
    filename <- sprintf("cc-est%s-%s-%s.csv", year, file_type, state_fips)
    file_url <- paste0(base_url, filename)

    # More professional message without URL
    message("Retrieving ", file_type, " population estimates for ", year)

    tryCatch({
      # Download and read the file
      df <- readr::read_csv(file_url, show_col_types = FALSE)

      # Add metadata
      df <- df %>%
        dplyr::mutate(
          Year_pulled = year,
          File_type = file_type
        )

      return(df)
    }, error = function(e) {
      warning("Error retrieving ", file_type, " data for ", year, ": ", e$message)
      return(NULL)
    })
  }

  # Process each file type
  results <- purrr::map(file_types, function(ft) {
    file_data <- purrr::map(years, function(yr) {
      process_file(yr, ft)
    }) %>% purrr::compact()

    if(length(file_data) > 0) {
      combined <- dplyr::bind_rows(file_data)

      # Save to database if requested
      if(save_to_db) {
        db_path <- Sys.getenv("CENSUS_DB_PATH", "census_data.duckdb")
        table_name <- paste0("pop_estimates_", ft)

        tryCatch({
          con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
          on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

          DBI::dbWriteTable(con, table_name, combined, overwrite = TRUE)
          message("Added ", nrow(combined), " rows of ", ft, " population estimates to database")
        }, error = function(e) {
          warning("Error saving to database: ", e$message)
        })
      }

      return(combined)
    } else {
      return(NULL)
    }
  }) %>% purrr::set_names(file_types) %>% purrr::compact()

  return(results)
}

#' Get Population Estimates Data
#'
#' @description Retrieves population estimates data from the database.
#'
#' @param file_type Character. Type of estimates to retrieve. Options are
#'   'agesex' (default), 'alldata', or 'syasex'.
#' @param years Numeric vector. Specific years to filter for. If NULL (default),
#'   returns data for all available years.
#' @param counties Character vector. County names to filter for. If NULL (default),
#'   returns data for all counties.
#'
#' @return A data frame of population estimates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all age-sex population estimates
#' pop_data <- get_pop_estimates()
#'
#' # Get specific data
#' atlantic_data <- get_pop_estimates(file_type = "agesex",
#'                                   counties = "Atlantic")
#' }
get_pop_estimates <- function(file_type = "agesex",
                              years = NULL,
                              counties = NULL) {

  valid_file_types <- c('agesex', 'alldata', 'syasex')
  if(!file_type %in% valid_file_types) {
    stop("Invalid file type. Must be one of: ", paste(valid_file_types, collapse = ", "))
  }

  # Connect to database
  db_path <- Sys.getenv("CENSUS_DB_PATH", "census_data.duckdb")
  if(!file.exists(db_path)) {
    stop("Census database not found. Please run init_census_data() first.")
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  table_name <- paste0("pop_estimates_", file_type)

  # Check if table exists
  if(!DBI::dbExistsTable(con, table_name)) {
    stop("Population estimates data not found in database. Please run process_pop_estimates() first.")
  }

  # Build query
  query <- sprintf("SELECT * FROM \"%s\"", table_name)

  # Add filters if provided
  where_clauses <- c()

  if(!is.null(years)) {
    years_str <- paste(years, collapse = ", ")
    where_clauses <- c(where_clauses, sprintf("Year_pulled IN (%s)", years_str))
  }

  if(!is.null(counties)) {
    counties_str <- paste0("'", counties, "'", collapse = ", ")
    where_clauses <- c(where_clauses, sprintf("CTYNAME IN (%s)", counties_str))
  }

  if(length(where_clauses) > 0) {
    query <- paste0(query, " WHERE ", paste(where_clauses, collapse = " AND "))
  }

  # Execute query
  result <- DBI::dbGetQuery(con, query)

  # Remove " County" from county names
  result <- result %>%
    dplyr::mutate(CTYNAME = gsub(" County$", "", CTYNAME))

  # Format result
  class(result) <- unique(c("pop_estimates", class(result)))

  return(result)
}

#' Print Population Estimates Data
#' @param x Population estimates data frame
#' @param ... Additional arguments passed to print
#' @export
print.pop_estimates <- function(x, ...) {
  cat("\nPopulation Estimates Data\n")
  cat("\nSummary View (use str() or View() for full data):\n\n")

  display_cols <- intersect(
    c("CTYNAME", "YEAR", "AGEGRP", "TOT_POP", "TOT_MALE", "TOT_FEMALE", "Year_pulled", "File_type"),
    names(x)
  )

  display_data <- x[, display_cols]

  if(nrow(display_data) > 0) {
    print.data.frame(utils::head(display_data, 10), row.names = FALSE)

    if(nrow(x) > 10) {
      cat(sprintf("\n... showing 10 rows of %s total rows\n",
                  format(nrow(x), big.mark = ",")))
    }
  } else {
    cat("No data available\n")
  }

  invisible(x)
}
