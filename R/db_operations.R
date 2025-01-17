#' Read Census Data from DuckDB
#'
#' @description Retrieves census data for specified demographic group, gender, and year
#' @param demographic Character. Demographic group (e.g., "white", "asian", "boaa")
#' @param gender Character. Gender ("male" or "female")
#' @param year Numeric. Census year (2010 or 2020)
#' @return A data frame containing the requested census data
#' @import DBI
#' @import duckdb
#' @export
#' @examples
#' \dontrun{
#' white_male_2020 <- get_census_data("white", "male", 2020)
#' asian_female_2010 <- get_census_data("asian", "female", 2010)
#' }
get_census_data <- function(demographic = NULL, gender = NULL, year = 2020) {
  # Validate inputs
  valid_demographics <- c("white", "boaa", "aian", "asian", "nhpi", "others", "two_more")
  valid_genders <- c("male", "female")
  valid_years <- c(2010, 2020)

  if(is.null(demographic) || !demographic %in% valid_demographics) {
    stop("Invalid demographic. Must be one of: ", paste(valid_demographics, collapse = ", "))
  }
  if(is.null(gender) || !gender %in% valid_genders) {
    stop("Invalid gender. Must be one of: ", paste(valid_genders, collapse = ", "))
  }
  if(!year %in% valid_years) {
    stop("Invalid year. Must be one of: ", paste(valid_years, collapse = ", "))
  }

  # Get database path from environment variable or use default
  db_path <- Sys.getenv("CENSUS_DB_PATH", "census_data.duckdb")

  if(!file.exists(db_path)) {
    stop("Census database not found at ", db_path, ". Please run init_census_data() first.")
  }

  # Try to connect to database
  tryCatch({
    con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
    on.exit(dbDisconnect(con, shutdown = TRUE))

    table_name <- paste(demographic, gender, year, sep = "_")

    if(!dbExistsTable(con, table_name)) {
      stop(sprintf("Invalid combination of demographic/gender/year: %s", table_name))
    }

    data <- dbGetQuery(con, sprintf("SELECT * FROM \"%s\"", table_name))
    return(data)

  }, error = function(e) {
    stop("Error accessing database: ", e$message)
  })
}

#' Initialize Census Database
#'
#' @description Creates or initializes the census database with both 2010 and 2020 data.
#' Either uses pre-packaged data (recommended) or fetches from Census API.
#'
#' @param use_packaged_data Logical. If TRUE, uses pre-packaged data. If FALSE, fetches from Census API.
#' @param worker_threads Integer. Number of threads for parallel processing when fetching from API.
#' @param memory_limit Character. Memory limit for DuckDB (default: "4GB").
#'
#' @details
#' The function provides two methods of initializing the census database:
#' 1. Using pre-packaged data (faster, reliable, no API limits)
#' 2. Fetching from Census API (for custom updates)
#'
#' Pre-packaged data is recommended for most users as:
#' - It's faster to initialize
#' - Doesn't depend on API availability
#' - Avoids API rate limits
#' - Census data for 2010 and 2020 is static
#'
#' @return None. Creates or initializes the census database.
#' @export
#'
#' @examples
#' \dontrun{
#' # Use pre-packaged data (recommended)
#' init_census_data()
#'
#' # Fetch fresh data from Census API
#' init_census_data(use_packaged_data = FALSE)
#'
#' # Customize API fetch settings
#' init_census_data(
#'   use_packaged_data = FALSE,
#'   worker_threads = 4,
#'   memory_limit = "8GB"
#' )
#' }
init_census_data <- function(use_packaged_data = TRUE,
                             worker_threads = parallel::detectCores() - 1,
                             memory_limit = "4GB") {
  if(use_packaged_data) {
    # Use included database
    source_db_path <- system.file("extdata", "census_data.duckdb", package = "njcensus")
    target_db_path <- Sys.getenv("CENSUS_DB_PATH", "census_data.duckdb")

    if(!file.exists(target_db_path)) {
      message("Copying packaged census database...")
      file.copy(source_db_path, target_db_path)
      message("Census database ready to use at ", target_db_path)
    } else {
      message("Using existing census database at ", target_db_path)
    }
  } else {
    # Use API to fetch data
    db_path <- Sys.getenv("CENSUS_DB_PATH", "census_data.duckdb")
    message("Fetching census data for 2010 and 2020... This may take a few minutes.")

    tryCatch({
      con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
      on.exit(dbDisconnect(con, shutdown = TRUE))

      # Configure database
      dbExecute(con, sprintf("SET memory_limit='%s'", memory_limit))
      dbExecute(con, sprintf("SET threads=%d", worker_threads))

      # Verify settings
      actual_memory <- dbGetQuery(con, "SELECT current_setting('memory_limit')")[[1]]
      message("Configured memory limit: ", actual_memory)

      # Process data
      process_census_data(2010)
      process_census_data(2020)

      message("Census database ready to use at ", db_path)
    }, error = function(e) {
      stop("Error initializing database: ", e$message)
    })
  }
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


