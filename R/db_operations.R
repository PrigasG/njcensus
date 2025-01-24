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
#' }
get_census_data <- function(demographic = NULL,
                            gender = "male",
                            year = 2020,
                            preview_limit = 10) {
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
  if(!file.exists("census_data.duckdb")) {
    stop("Census database not found. Please run init_census_data() first.")
  }

  # Connect to database
  con <- dbConnect(duckdb::duckdb(), dbdir = "census_data.duckdb", read_only = FALSE)
  on.exit(dbDisconnect(con, shutdown = TRUE))

  # Handle NULL demographic (preview mode)
  if(is.null(demographic)) {
    tables <- dbListTables(con)
    pattern <- paste0("_", gender, "_", year, "$")
    relevant_tables <- grep(pattern, tables, value = TRUE)

    result <- data.frame()
    for(table in relevant_tables) {
      data <- dbGetQuery(con, sprintf("SELECT * FROM \"%s\" LIMIT %d",
                                      table, preview_limit))
      data$demographic <- sub(paste0("_", gender, "_", year), "", table)
      result <- if(nrow(result) == 0) data else rbind(result, data)
    }

    # Remove rows with NA in county_name or municipality_name
    result <- subset(result,
                     !is.na(county_name) & !is.na(municipality_name))

    class(result) <- unique(c("census_data", class(result)))
    return(result)
  }

  # get demographic query
  table_name <- paste(demographic, gender, year, sep = "_")
  if(!dbExistsTable(con, table_name)) {
    stop(sprintf("Invalid combination of demographic/gender/year: %s", table_name))
  }

  data <- dbGetQuery(con, sprintf("SELECT * FROM \"%s\"", table_name))

  # Remove rows with NA in county_name or municipality_name
  data <- subset(data, !is.na(county_name) & !is.na(municipality_name))

  class(data) <- unique(c("census_data", class(data)))
  return(data)
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

#' Print Census Data
#' @param x Census data frame from get_census_data
#' @param ... Additional arguments passed to print
#' @importFrom knitr kable
#' @export
print.census_data <- function(x, ...) {
  # Select relevant columns for display
  display_cols <- intersect(
    c("demographic", "county_name", "municipality_name", "Total", "year"),
    names(x)
  )

  display_data <- x[, display_cols]

  # Create title with more context
  title <- if("demographic" %in% names(x)) {
    sprintf("Census Data Preview (%s, %d)", x$gender[1], x$year[1])
  } else {
    sprintf("Census Data for %s (%d)", paste(unique(x$county_name), collapse = ", "), x$year[1])
  }

  # Format output
  cat("\n", title, "\n\n")

  if(nrow(display_data) > 0) {
    if(interactive()) {
      # Add thousands separator for Total column
      display_data$Total <- format(display_data$Total, big.mark = ",")
      print.data.frame(head(display_data, 10), row.names = FALSE)
    } else {
      print(knitr::kable(head(display_data, 10),
                         format = "pipe",
                         caption = title,
                         format.args = list(big.mark = ",")))
    }

    if(nrow(x) > 10) {
      cat(sprintf("\n... showing first 10 rows of %s total rows\n",
                  format(nrow(x), big.mark = ",")))
    }
  } else {
    cat("No data available\n")
  }

  invisible(x)
}
