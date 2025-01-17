dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
# Run your data fetching once
process_census_data(2010)
process_census_data(2020)
# Copy the database to package
file.copy("census_data.duckdb", "inst/extdata/census_data.duckdb")
