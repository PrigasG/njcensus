library(testthat)
library(DBI)
library(duckdb)

# Helper function to create test database
create_test_db <- function() {
  test_db <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = test_db)

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

  # Create test tables
  dbWriteTable(con, "white_male_2020", test_data)
  dbWriteTable(con, "asian_female_2010", test_data)

  dbDisconnect(con, shutdown = TRUE)
  return(test_db)
}

# Tests
withr::with_file(create_test_db(), {
  test_that("get_census_data validates input parameters correctly", {
    # Test invalid demographic
    expect_error(
      get_census_data("invalid", "male", 2020),
      "Invalid demographic"
    )

    # Test invalid gender
    expect_error(
      get_census_data("white", "other", 2020),
      "Invalid gender"
    )

    # Test invalid year
    expect_error(
      get_census_data("white", "male", 2015),
      "Invalid year"
    )

    # Test NULL parameters
    expect_error(
      get_census_data(NULL, "male", 2020),
      "Invalid demographic"
    )
    expect_error(
      get_census_data("white", NULL, 2020),
      "Invalid gender"
    )
  })

  test_that("get_census_data handles database connection properly", {
    # Test non-existent database
    temp_dir <- tempdir()
    withr::with_dir(temp_dir, {
      expect_error(
        get_census_data("white", "male", 2020),
        "Census database not found"
      )
    })
  })

  test_that("get_census_data retrieves data correctly", {
    temp_db <- tempfile(fileext = ".duckdb")
    withr::with_envvar(
      new = c("CENSUS_DB_PATH" = temp_db),
      code = {
        # Set up test database
        con <- dbConnect(duckdb::duckdb(), dbdir = temp_db)
        on.exit(dbDisconnect(con, shutdown = TRUE))

        # Create test data
        test_data <- data.frame(
          state = "34",
          county = "001",
          county_subdivision = "00100",
          Total = 1000,
          municipality_name = "Test City",
          county_name = "Test County",
          year = 2010
        )
        dbWriteTable(con, "white_male_2010", test_data)

        # Test data retrieval
        result <- get_census_data("white", "male", 2010)
        expect_s3_class(result, "data.frame")
        expect_true(all(c("state", "county", "county_subdivision") %in% names(result)))
      }
    )
    unlink(temp_db)
  })

  test_that("get_census_data handles defaults correctly", {
    skip_on_cran()
    temp_db <- tempfile(fileext = ".duckdb")

    withr::with_envvar(
      new = c("CENSUS_DB_PATH" = temp_db),
      code = {
        # Set up test database
        con <- dbConnect(duckdb::duckdb(), dbdir = temp_db)
        on.exit(dbDisconnect(con, shutdown = TRUE))

        # Create test data for multiple demographics
        demographics <- c("white", "asian")
        for(demo in demographics) {
          test_data <- data.frame(
            state = "34",
            county = "001",
            county_subdivision = "00100",
            Total = 1000,
            municipality_name = "Test City",
            county_name = "Test County",
            year = 2020
          )
          table_name <- paste(demo, "male", "2020", sep = "_")
          dbWriteTable(con, table_name, test_data)
        }

        # Test preview mode (NULL demographic)
        preview <- get_census_data()
        expect_s3_class(preview, "data.frame")
        expect_true("demographic" %in% names(preview))
        expect_lte(nrow(preview), 10 * length(demographics))

        # Test default gender and year
        default_data <- get_census_data("white")
        expect_s3_class(default_data, "data.frame")

        # Test warning for invalid year
        expect_warning(
          get_census_data("white", year = 2015),
          "Invalid year"
        )
      }
    )

    unlink(temp_db)
  })

  test_that("init_census_data handles packaged data correctly", {
    skip_on_cran()
    temp_db <- tempfile(fileext = ".duckdb")

    withr::with_envvar(
      new = c("CENSUS_DB_PATH" = temp_db),
      code = {
        expect_message(
          init_census_data(use_packaged_data = TRUE),
          "Census database ready to use"
        )
      }
    )
    unlink(temp_db)
  })

  test_that("init_census_data handles API fetch correctly", {
    skip_on_cran()
    temp_db <- tempfile(fileext = ".duckdb")

    withr::with_envvar(
      new = c("CENSUS_DB_PATH" = temp_db),
      code = {
        expect_message(
          init_census_data(use_packaged_data = FALSE),
          "Fetching census data"
        )
      }
    )
    unlink(temp_db)
  })

  test_that("Database connection is properly closed", {
    # Test that connection is closed even if error occurs
    expect_error({
      con <- dbConnect(duckdb::duckdb(), "census_data.duckdb")
      stop("Simulated error")
      dbDisconnect(con, shutdown = TRUE)
    })

    # Verify we can still open a new connection
    expect_no_error({
      con <- dbConnect(duckdb::duckdb(), "census_data.duckdb")
      dbDisconnect(con, shutdown = TRUE)
    })
  })
})

# Clean up any remaining test files
unlink("census_data.duckdb")
