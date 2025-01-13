library(DBI)
library(duckdb)
library(httr)
library(jsonlite)
library(dplyr)

test_that("process_census_data handles invalid year gracefully", {
  expect_error(process_census_data(2000), "Invalid year")
})



# test_that("process_census_data handles invalid year gracefully", {
#   expect_error(process_census_data(2000), "Error during data processing") # Invalid year
# })

# Fix 1: Update test-data_processing.R
test_that("process_census_data writes correct tables", {
  skip_on_cran()

  temp_db <- tempfile(fileext = ".duckdb")
  withr::with_envvar(
    new = c("CENSUS_DB_PATH" = temp_db),
    code = {
      message("Starting test with database at: ", temp_db)

      # Create initial connection to ensure database exists
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
      DBI::dbDisconnect(con, shutdown = TRUE)

      message("Running process_census_data...")
      process_census_data(2020)

      # Try to connect and check tables
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

      tables <- DBI::dbListTables(con)
      message("Database connection status: ", !is.null(con))
      message("Tables found: ", paste(tables, collapse = ", "))

      expect_true(any(grepl("_2020$", tables)),
                  info = sprintf("Expected tables with '_2020' suffix.\nActual tables: %s\nDatabase path: %s",
                                 paste(tables, collapse = ", "),
                                 temp_db))
    }
  )

  unlink(temp_db)
})

# Fix 2: Update test-db_operations.R
# Helper function for setting up test database
create_test_db <- function() {
  test_db <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = test_db)
  on.exit(dbDisconnect(con, shutdown = TRUE))

  # Create sample test data
  test_data <- data.frame(
    state = "34",
    county = "001",
    county_subdivision = "00100",
    Total = 1000,
    municipality_name = "Test City",
    county_name = "Test County",
    year = 2020
  )

  # Create test table
  table_name <- "white_male_2020"
  dbWriteTable(con, table_name, test_data)

  test_db
}

test_that("get_census_data retrieves data correctly", {
  test_db <- create_test_db()
  withr::with_envvar(
    new = c("CENSUS_DB_PATH" = test_db),
    code = {
      result <- get_census_data("white", "male", 2020)
      expect_s3_class(result, "data.frame")
      expect_true(all(c("state", "county", "county_subdivision") %in% names(result)))
    }
  )
  unlink(test_db)
})

test_that("init_census_data configures database correctly", {
  skip_on_cran()
  temp_db <- tempfile(fileext = ".duckdb")

  withr::with_envvar(
    new = c("CENSUS_DB_PATH" = temp_db),
    code = {
      init_census_data(force_refresh = TRUE)

      con <- dbConnect(duckdb::duckdb(), temp_db)
      on.exit(dbDisconnect(con, shutdown = TRUE))

      memory_limit <- dbGetQuery(con, "SELECT current_setting('memory_limit')")[[1]]
      message("Raw memory limit: ", memory_limit)

      # Convert to numeric GB for comparison
      memory_gb <- as.numeric(gsub("[^0-9.]", "", memory_limit))
      if(grepl("GiB", memory_limit)) {
        memory_gb <- memory_gb * 1.074 # Convert GiB to GB
      }

      # Debug output
      message("Converted memory (GB): ", memory_gb)

      # Check if memory is at least 4GB
      expect_true(memory_gb >= 4,
                  info = sprintf("Memory limit %f GB is less than required 4GB", memory_gb))
    }
  )

  unlink(temp_db)
})
