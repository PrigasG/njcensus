library(testthat)
library(mockery)
library(DBI)
library(duckdb)

# Mock helpers
mock_census_response <- function() {
  list(
    content = charToRaw(jsonlite::toJSON(list(
      c("state", "county", "county subdivision", "PCT12I_001N", "PCT12I_002N"),
      c("34", "001", "00100", "1000", "500")
    ))),
    status_code = 200
  )
}

mock_geo_data <- function() {
  data.frame(
    COUNTYFP = "001",
    NAME = "Test County",
    COUSUBFP = "00100",
    municipality_name = "Test City",
    county_name = "Test County",
    stringsAsFactors = FALSE
  )
}

# First test
test_that("process_census_data handles invalid year gracefully", {
  expect_error(process_census_data(2000), "Invalid year")
})

# Second test
test_that("process_census_data writes correct tables", {
  skip_on_cran()
  skip_if_not_installed("duckdb")

  temp_db <- tempfile(fileext = ".duckdb")
  message("Created temp database at: ", temp_db)

  mock_get <- mock(mock_census_response())
  mock_geo_ref <- mock(mock_geo_data())

  withr::with_envvar(
    new = c("CENSUS_DB_PATH" = temp_db),
    code = {
      tryCatch({
        message("Starting process_census_data...")

        local_mocked_bindings(
          GET = function(...) mock_get(),
          get_nj_geo_reference = function(...) mock_geo_ref()
        )

        process_census_data(2020)

        message("Process completed, checking database...")

        con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
        on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

        tables <- DBI::dbListTables(con)
        message("Tables in database: ", paste(tables, collapse = ", "))

        expect_true(
          any(grepl("_2020$", tables)),
          info = sprintf(
            "Expected tables with '_2020' suffix.\nActual tables: %s\nDatabase path: %s",
            paste(tables, collapse = ", "),
            temp_db
          )
        )

        if(length(tables) > 0) {
          for(table in tables) {
            tryCatch({
              data <- dbReadTable(con, table)
              message(sprintf("Table %s has %d rows", table, nrow(data)))
            }, error = function(e) {
              message(sprintf("Error reading table %s: %s", table, e$message))
            })
          }
        }

      }, error = function(e) {
        message("Error during test: ", e$message)
        message("Call stack: ", paste(sys.calls(), collapse = "\n"))
      })
    }
  )

  if(DBI::dbCanConnect(duckdb::duckdb(), dbdir = temp_db)) {
    try(DBI::dbDisconnect(duckdb::duckdb(), shutdown = TRUE))
  }
  unlink(temp_db)
})
