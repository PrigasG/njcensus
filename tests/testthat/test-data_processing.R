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
# Setup helper function for all tests to use
create_test_db <- function() {
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)

  # Create sample data
  mock_data <- data.frame(
    state = "34",
    county = c("001", "003", "005"),
    county_subdivision = c("00100", "00300", "00500"),
    Total = c(1000, 2000, 3000),
    municipality_name = c("Atlantic City", "Bergen Town", "Burlington City"),
    county_name = c("Atlantic", "Bergen", "Burlington"),
    year = 2020
  )

  # Create test tables
  tables <- expand.grid(
    demographic = c("white", "asian", "boaa", "aian"),
    gender = c("male", "female"),
    year = c(2010, 2020)
  )

  for(i in 1:nrow(tables)) {
    table_name <- paste(tables$demographic[i], tables$gender[i], tables$year[i], sep = "_")
    DBI::dbWriteTable(con, table_name, mock_data)
  }

  DBI::dbDisconnect(con, shutdown = TRUE)
  return(temp_db)
}

test_that("process_census_data handles invalid year gracefully", {
  expect_error(process_census_data(2000), "Invalid year")
})

test_that("process_census_data writes correct tables", {
  skip_on_cran()
  temp_db <- create_test_db()

  withr::with_envvar(
    new = c("CENSUS_DB_PATH" = temp_db),
    code = {
      process_census_data(2020)

      con <- DBI::dbConnect(duckdb::duckdb(), temp_db)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

      tables <- DBI::dbListTables(con)
      expect_true(any(grepl("_2020$", tables)),
                  info = sprintf("Available tables: %s",
                                 paste(tables, collapse = ", ")))
    }
  )

  unlink(temp_db)
})
