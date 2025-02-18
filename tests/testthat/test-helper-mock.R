mock_census_response <- function() {
  list(
    content = charToRaw(jsonlite::toJSON(list(
      # Header row
      c("state", "county", "county subdivision", "P0010001"),
      # Data row
      c("34", "001", "00100", "1000")
    ))),
    status_code = 200
  )
}

mock_geo_data <- function() {
  structure(
    list(
      COUNTYFP = "001",
      NAME = "Test County",
      COUSUBFP = "00100",
      municipality_name = "Test City",
      county_name = "Test County"
    ),
    class = c("sf", "data.frame")
  )
}
