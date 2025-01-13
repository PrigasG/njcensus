library(testthat)
library(httr)
library(jsonlite)

# Mock functions for testing
mock_GET <- function(url) {
  if (grepl("error", url)) {
    structure(list(
      status_code = 404,
      content = charToRaw("Not Found")
    ))
  } else {
    structure(list(
      status_code = 200,
      content = charToRaw(jsonlite::toJSON(list(
        c("PCT12I001N", "state", "county", "county subdivision"),
        c("100", "34", "001", "00100"),
        c("200", "34", "001", "00200")
      )))
    ))
  }
}

# Tests for generate_patterns
test_that("generate_patterns creates correct number of patterns", {
  initial_pattern <- "PCT12I001"
  result <- generate_patterns(initial_pattern, 10, 3, 2020)

  # Check list length
  expect_length(result, 3)

  # Check pattern format for 2020
  expect_match(result[[1]], "PCT12I.*N", all = FALSE)
})

test_that("generate_patterns handles remainders correctly", {
  initial_pattern <- "PCT12I001"
  # 7 patterns split into 3 parts should have remainder
  result <- generate_patterns(initial_pattern, 7, 3, 2020)

  # Calculate expected pattern distribution
  patterns_per_part <- floor(7/3)  # Should be 2
  last_part_patterns <- length(unlist(strsplit(result[[3]], ",")))

  # Last part should have extra patterns
  expect_gt(last_part_patterns, patterns_per_part)
})

test_that("generate_patterns handles different years correctly", {
  initial_pattern <- "PCT12I001"
  result_2020 <- generate_patterns(initial_pattern, 5, 2, 2020)
  result_2010 <- generate_patterns(initial_pattern, 5, 2, 2010)

  # 2020 patterns should end with N
  expect_match(result_2020[[1]], "N$", all = FALSE)
  # 2010 patterns should not end with N
  expect_no_match(result_2010[[1]], "N$", all = FALSE)
})

# Tests for convert_process_df
test_that("convert_process_df handles successful API calls", {
  # Mock httr::GET
  mockery::stub(convert_process_df, "GET", mock_GET)

  result <- convert_process_df(
    "http://api.example.com",
    "?get=",
    list("PCT12I001N"),
    "&for=county:*",
    max_retries = 1
  )

  expect_type(result, "list")
  expect_length(result, 1)
  expect_s3_class(result[[1]], "data.frame")
  expect_true(all(c("state", "county", "county subdivision") %in% names(result[[1]])))
})

test_that("convert_process_df handles failed API calls with retries", {
  # Mock httr::GET to always fail
  mockery::stub(convert_process_df, "GET",
                function(url) structure(list(status_code = 500)))

  expect_error(
    convert_process_df(
      "http://api.example.com",
      "?get=",
      list("error_pattern"),
      "&for=county:*",
      max_retries = 2
    ),
    "Failed to fetch data after 2 attempts"
  )
})

test_that("convert_process_df converts numeric columns correctly", {
  mockery::stub(convert_process_df, "GET", mock_GET)

  result <- convert_process_df(
    "http://api.example.com",
    "?get=",
    list("PCT12I001N"),
    "&for=county:*",
    max_retries = 1
  )

  # Check if PCT columns are numeric
  numeric_cols <- grep("PCT", names(result[[1]]))
  expect_true(all(sapply(result[[1]][numeric_cols], is.numeric)))
})

test_that("convert_process_df handles multiple patterns", {
  mockery::stub(convert_process_df, "GET", mock_GET)

  result <- convert_process_df(
    "http://api.example.com",
    "?get=",
    list("PCT12I001N", "PCT12I002N"),
    "&for=county:*",
    max_retries = 1
  )

  expect_length(result, 2)
  expect_true(all(sapply(result, is.data.frame)))
})
