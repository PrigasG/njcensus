#' Read Census Data from DuckDB
#'
#' @description Retrieves census data for specified demographic group, gender, and year.
#' If no specific parameters are provided, returns a preview of all demographics.
#' Returns data in a formatted table view for easy reading.
#'
#' @param demographic Character. Demographic group (e.g., "white", "asian", "boaa").
#'        If NULL, returns preview of all groups.
#' @param gender Character. Gender ("male" or "female"). Defaults to "male".
#' @param year Numeric. Census year (2010 or 2020). Defaults to 2020.
#' @param preview_limit Numeric. Number of rows to return when no demographic specified.
#'        Defaults to 10.
#' @param counties Character vector. County names to filter for. If NULL (default),
#'        returns data for all counties.
#' @param municipalities Character vector. Municipality names to filter for. If NULL (default),
#'        returns data for all municipalities.
#'
#' @return A data frame of class 'census_data' containing the requested census data,
#'         with formatted printing capabilities.
#' @export
#' @examples
#' \dontrun{
#' # Get preview of all demographics
#' preview <- get_census_data()
#'
#' # Get data with defaults (white males in 2020)
#' default_data <- get_census_data("white")
#'
#' # Get specific data
#' asian_female_2010 <- get_census_data("asian", "female", 2010)
#'
#' # Filter by county
#' atlantic_data <- get_census_data("white", counties = "Atlantic")
#'
#' # Filter by municipality
#' jersey_city_data <- get_census_data("white", municipalities = "Jersey City")
#' }
get_census_data <- function(demographic = NULL,
                            gender = "male",
                            year = 2020,
                            preview_limit = 10,
                            counties = NULL,
                            municipalities = NULL) {
  # Define valid options
  valid_demographics <- c("white", "boaa", "aian", "asian", "nhpi", "others", "two_more")
  valid_genders <- c("male", "female")
  valid_years <- c(2010, 2020)

  # Validate and set defaults for gender and year
  gender <- match.arg(gender, valid_genders)
  if(!year %in% valid_years) {
    warning("Invalid decennial year. Using default year 2020.")
    year <- 2020
  }

  # Check database existence
  db_path <- Sys.getenv("CENSUS_DB_PATH", "census_data.duckdb")
  if(!file.exists(db_path)) {
    stop("Census database not found. Please run init_census_data() first.")
  }

  # Connect to database
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(dbDisconnect(con, shutdown = TRUE))

  # Handle NULL demographic (preview mode)
  if(is.null(demographic)) {
    tables <- dbListTables(con)
    pattern <- paste0("_", gender, "_", year, "$")
    relevant_tables <- grep(pattern, tables, value = TRUE)
    result <- data.frame()

    for(table in relevant_tables) {
      # Build query with filters
      query <- sprintf("SELECT *, '%s' as demographic FROM \"%s\"",
                       sub(paste0("_", gender, "_", year), "", table),
                       table)

      # Add WHERE clause if filters are provided
      where_clauses <- c()

      if(!is.null(counties)) {
        counties_str <- paste0("'", counties, "'", collapse = ", ")
        where_clauses <- c(where_clauses, sprintf("county_name IN (%s)", counties_str))
      }

      if(!is.null(municipalities)) {
        munis_str <- paste0("'", municipalities, "'", collapse = ", ")
        where_clauses <- c(where_clauses, sprintf("municipality_name IN (%s)", munis_str))
      }

      if(length(where_clauses) > 0) {
        query <- paste0(query, " WHERE ", paste(where_clauses, collapse = " AND "))
      }

      # Execute query
      data <- dbGetQuery(con, query)
      result <- if(nrow(result) == 0) data else rbind(result, data)
    }

    # Remove rows with NA in county_name or municipality_name
    result <- subset(result,
                     !is.na(county_name) & !is.na(municipality_name))

    # Format for display
    result <- result %>%
      dplyr::arrange(county_name, municipality_name) %>%
      dplyr::select(demographic, county_name, municipality_name,
                    dplyr::matches("_years$"), Total, dplyr::everything()) %>%
      utils::head(preview_limit)
  } else {
    # Validate demographic if provided
    if(!demographic %in% valid_demographics) {
      stop("Invalid demographic. Must be one of: ",
           paste(valid_demographics, collapse = ", "))
    }

    # Get specific data
    table_name <- paste(demographic, gender, year, sep = "_")
    if(!dbExistsTable(con, table_name)) {
      stop(sprintf("Invalid combination of demographic/gender/year: %s", table_name))
    }

    # Build query
    query <- sprintf("SELECT * FROM \"%s\"", table_name)

    # Add WHERE clause if filters are provided
    where_clauses <- c()

    if(!is.null(counties)) {
      counties_str <- paste0("'", counties, "'", collapse = ", ")
      where_clauses <- c(where_clauses, sprintf("county_name IN (%s)", counties_str))
    }

    if(!is.null(municipalities)) {
      munis_str <- paste0("'", municipalities, "'", collapse = ", ")
      where_clauses <- c(where_clauses, sprintf("municipality_name IN (%s)", munis_str))
    }

    if(length(where_clauses) > 0) {
      query <- paste0(query, " WHERE ", paste(where_clauses, collapse = " AND "))
    }

    # Execute query
    result <- dbGetQuery(con, query)

    # Remove rows with NA in county_name or municipality_name
    result <- subset(result,
                     !is.na(county_name) & !is.na(municipality_name))

    # Format for display
    result <- result %>%
      dplyr::arrange(county_name, municipality_name) %>%
      dplyr::select(county_name, municipality_name,
                    dplyr::matches("_years$"), Total, dplyr::everything())
  }

  class(result) <- unique(c("census_data", class(result)))
  return(result)
}

#' Initialize Census Database
#'
#' @description Creates or initializes the census database with both 2010 and 2020 census data.
#' Data is fetched from the Census API.
#'
#' @param worker_threads Integer. Number of threads for parallel processing when fetching from API.
#' @param memory_limit Character. Memory limit for DuckDB (default: "4GB").
#' @param include_pop_estimates Logical. Whether to include population estimates data. Default is FALSE.
#' @param pop_estimate_years Numeric vector. Years to include for population estimates. Default is c(2021, 2022, 2023).
#'
#' @details
#' The function fetches data from the Census API and processes it into a DuckDB database.
#' - Census data for 2010 and 2020 is always fetched
#' - Population estimates can be optionally included
#' - If the database already exists, it will check for existing tables and only add missing ones
#'
#' @return None. Creates or initializes the census database.
#' @export
#'
#' @examples
#' \dontrun{
#' # Initialize with just census data
#' init_census_data()
#'
#' # Initialize with census data and population estimates
#' init_census_data(include_pop_estimates = TRUE)
#'
#' # Customize API fetch settings
#' init_census_data(
#'   worker_threads = 4,
#'   memory_limit = "8GB"
#' )
#' }
init_census_data <- function(worker_threads = parallel::detectCores() - 1,
                             memory_limit = "4GB",
                             include_pop_estimates = FALSE,
                             pop_estimate_years = c(2021, 2022, 2023)) {

  # Use API to fetch data
  db_path <- Sys.getenv("CENSUS_DB_PATH", "census_data.duckdb")
  db_exists <- file.exists(db_path)

  # Check if database exists and has census tables
  if(db_exists) {
    tryCatch({
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
      tables <- DBI::dbListTables(con)
      has_census_tables <- any(grepl("_20(10|20)$", tables))
      DBI::dbDisconnect(con, shutdown = TRUE)

      if(has_census_tables) {
        message("Using existing census database at ", db_path)

        # Only process population estimates if requested and not already in database
        if(include_pop_estimates) {
          has_pop_estimates <- any(grepl("^pop_estimates_", tables))

          if(!has_pop_estimates) {
            message("Adding population estimates...")
            process_pop_estimates(years = pop_estimate_years, save_to_db = TRUE)
          } else {
            message("Population estimates already exist in database")
          }
        }

        return(invisible(NULL))
      }
    }, error = function(e) {
      message("Error checking existing database: ", e$message)
      message("Will create new database")
    })
  }

  # Initialize new database
  message("Initializing census database...")

  tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

    # Configure database
    DBI::dbExecute(con, sprintf("SET memory_limit='%s'", memory_limit))
    DBI::dbExecute(con, sprintf("SET threads=%d", worker_threads))

    # Process decennial census data
    message("Retrieving decennial census data for 2010 and 2020...")
    process_census_data(2010)
    process_census_data(2020)
    message("Census database initialized successfully")

    # Only add population estimates if explicitly requested
    if(include_pop_estimates) {
      message("Adding population estimates...")
      process_pop_estimates(years = pop_estimate_years, save_to_db = TRUE)
    }
  }, error = function(e) {
    stop("Error initializing database: ", e$message)
  })
}

#' Get Database Path
#'
#' @description Get the current database path
#' @return Character string with the database path
#' @export
#' @examples
#' get_db_path()
get_db_path <- function() {
  Sys.getenv("CENSUS_DB_PATH", "census_data.duckdb")
}

#' Set Database Path
#'
#' @description Set a custom path for the census database
#' @param path Character string with the new database path
#' @return Invisibly returns the new path
#' @export
#' @examples
#' \dontrun{
#' set_db_path("/path/to/my/census_data.duckdb")
#' }
set_db_path <- function(path) {
  Sys.setenv(CENSUS_DB_PATH = path)
  invisible(path)
}

#' Print Census Data
#' @param x Census data frame from get_census_data
#' @param ... Additional arguments passed to print
#' @importFrom knitr kable
#' @keywords internal
#' @export
print.census_data <- function(x, ...) {
  # Determine available columns
  base_cols <- intersect(
    c("county_name", "municipality_name", "Total"),
    names(x)
  )

  # Add demographic if it exists
  if("demographic" %in% names(x)) {
    display_cols <- c("demographic", base_cols, "year")
  } else {
    display_cols <- c(base_cols, "year")
  }

  display_data <- x[, display_cols]

  # Create title with more context
  title <- if("demographic" %in% names(x)) {
    sprintf("\nCensus Data Preview (All Demographics, %d)\n", x$year[1])
  } else {
    sprintf("\nCensus Data by Municipality (%d)\n", x$year[1])
  }

  cat(title)
  cat("\nSummary View (use str() or View() for full data):\n\n")

  if(nrow(display_data) > 0) {
    # Add thousands separator for Total
    display_data$Total <- format(display_data$Total, big.mark = ",")
    print.data.frame(utils::head(display_data, 10), row.names = FALSE)

    # Show column info
    age_cols <- grep("_years$", names(x), value = TRUE)
    if(length(age_cols) > 0) {
      cat("\nAvailable age columns:",
          paste(age_cols, collapse = ", "),
          "\n")
    }

    if(nrow(x) > 10) {
      cat(sprintf("\n... showing %d rows of %s total rows\n",
                  10,
                  format(nrow(x), big.mark = ",")))
    }
  } else {
    cat("No data available\n")
  }

  invisible(x)
}
