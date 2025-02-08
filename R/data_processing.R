#' Process Census Data for New Jersey
#'
#' @description Downloads and processes census demographic data for New Jersey for the specified year. The data is written to a DuckDB database for further analysis.
#'
#' @param year Integer. The census year to process data for. Must be either `2010` or `2020`.
#' @import dplyr DBI duckdb
#' @details
#' The function fetches demographic data (e.g., race and gender-specific counts) for all counties and municipalities in New Jersey.
#' It utilizes the Census Bureau API and processes data into a DuckDB database. Progress is tracked via console messages.
#'
#' ### Key Features
#' - Handles both 2010 and 2020 census data.
#' - Dynamically generates API queries and column mappings based on the year.
#' - Writes results to tables in a DuckDB database.
#'
#' @return None. Results are stored in a local DuckDB database file (`census_data.duckdb`).
#' @export
#' @examples
#' \dontrun{
#' # Process data for the 2020 census
#' process_census_data(2020)
#'
#' # Process data for the 2010 census
#' process_census_data(2010)
#' }
process_census_data <- function(year) {
  if (!year %in% c(2010, 2020)) {
    stop("Invalid year. Only 2010 and 2020 are supported.")
  }

  db_path <- Sys.getenv("CENSUS_DB_PATH", "census_data.duckdb")
  message("Using database at: ", db_path)

  tryCatch({
    message(sprintf("\n=== Starting data processing for %d ===\n", year))
    spinner <- create_spinner()

    # Get geographic reference data first
    spinner("Loading geographic reference data...", 0)
    geo_ref <- get_nj_geo_reference(year)
    spinner("Loading geographic reference data...", 100)

    message("\nGeographic reference columns:")
    print(names(geo_ref))

    # Create single database connection that will be used throughout
    con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
    on.exit(dbDisconnect(con, shutdown = TRUE))

    suppressMessages({
      # Set endpoint based on year
      endpoint <- if(year == 2020) {
        'https://api.census.gov/data/2020/dec/dhc'
      } else {
        'https://api.census.gov/data/2010/dec/sf1'
      }

      # Define base patterns based on year
      base_patterns <- if(year == 2020) {
        list(
          white_male = 'PCT12I_001N',
          boaa_male = 'PCT12J_001N',
          aian_male = 'PCT12K_001N',
          asian_male = 'PCT12L_001N',
          nhpi_male = 'PCT12M_001N',
          others_male = 'PCT12N_001N',
          two_more_male = 'PCT12O_001N',
          white_female = 'PCT12I_106N',
          boaa_female = 'PCT12J_106N',
          aian_female = 'PCT12K_106N',
          asian_female = 'PCT12L_106N',
          nhpi_female = 'PCT12M_106N',
          others_female = 'PCT12N_106N',
          two_more_female = 'PCT12O_106N'
        )
      } else {
        list(
          white_male = 'PCT012I001',
          boaa_male = 'PCT012J001',
          aian_male = 'PCT012K001',
          asian_male = 'PCT012L001',
          nhpi_male = 'PCT012M001',
          others_male = 'PCT012N001',
          two_more_male = 'PCT012O001',
          white_female = 'PCT012I106',
          boaa_female = 'PCT012J106',
          aian_female = 'PCT012K106',
          asian_female = 'PCT012L106',
          nhpi_female = 'PCT012M106',
          others_female = 'PCT012N106',
          two_more_female = 'PCT012O106'
        )
      }

      # Generate patterns
      patterns <- lapply(names(base_patterns), function(name) {
        count <- if(grepl("female", name)) 104 else 105
        generate_patterns(base_patterns[[name]], count, 3, year)
      })
      names(patterns) <- names(base_patterns)

      # API parameters
      query <- '?get='
      filter_sub <- '&for=county%20subdivision:*&in=state:34&in=county:*'

      # Process patterns
      male_patterns <- patterns[grep("_male$", names(patterns))]
      female_patterns <- patterns[grep("_female$", names(patterns))]
    })

    # Process patterns with progress tracking
    message("\n=== Processing demographic patterns ===")
    total_patterns <- length(c(names(male_patterns), names(female_patterns)))
    current_pattern <- 0

    # Add debug message for database check
    message("Current tables in database before processing: ",
            paste(dbListTables(con), collapse = ", "))

    # Process male patterns
    for(name in names(male_patterns)) {
      current_pattern <- current_pattern + 1
      percent <- round((current_pattern/total_patterns) * 100)
      spinner(sprintf("Writing %s to database...", name), percent)

      dfs <- convert_process_df(endpoint, query, male_patterns[[name]], filter_sub)

      # Data processing for males
      merged_df <- Reduce(function(x, y) {
        merge(x, y, by = c("state", "county", "county subdivision"))
      }, dfs)

      merged_df <- merged_df %>%
        left_join(geo_ref, by = c("county" = "COUNTYFP", "county subdivision" = "county subdivision")) %>%
        mutate(year = !!year)

      new_names <- c("state", "county", "county_subdivision",
                     "Total", "Total_Male", "Under_1_year",
                     paste0(1:99, "_years"),
                     "100_104_years", "105_109_years", "110_and_over_years",
                     "municipality_name", "county_name", "year")
      names(merged_df)[1:length(new_names)] <- new_names

      # Add debug messages around table writing
      table_name <- paste0(name, "_", year)
      message("\n", paste(rep("=", 80), collapse = ""))
      message(sprintf("Attempting to write table %s to database at %s", table_name, db_path))
      dbWriteTable(con, table_name, merged_df, overwrite = TRUE)
      message(sprintf("✓ Successfully wrote table: %s", table_name))
      message(paste(rep("=", 80), collapse = ""), "\n")

      cat("\n")
    }

    # Process female patterns
    for(name in names(female_patterns)) {
      current_pattern <- current_pattern + 1
      percent <- round((current_pattern/total_patterns) * 100)
      spinner(sprintf("Writing %s to database...", name), percent)

      dfs <- convert_process_df(endpoint, query, female_patterns[[name]], filter_sub)

      merged_df <- Reduce(function(x, y) {
        merge(x, y, by = c("state", "county", "county subdivision"))
      }, dfs)

      merged_df <- merged_df %>%
        left_join(geo_ref, by = c("county" = "COUNTYFP", "county subdivision" = "county subdivision")) %>%
        mutate(year = !!year)

      new_names <- c("State", "county", "county_subdivision",
                     "Total_Female", "Under_1_year",
                     paste0(1:99, "_years"),
                     "100_104_years", "105_109_years", "110_and_over_years",
                     "municipality_name", "county_name", "Year")
      names(merged_df)[1:length(new_names)] <- new_names

      # Add debug messages around table writing
      table_name <- paste0(name, "_", year)
      message("\n", paste(rep("=", 80), collapse = ""))
      message(sprintf("Attempting to write table %s to database at %s", table_name, db_path))
      dbWriteTable(con, table_name, merged_df, overwrite = TRUE)
      message(sprintf("✓ Successfully wrote table: %s", table_name))
      message(paste(rep("=", 80), collapse = ""), "\n")

      cat("\n")
    }

    # Add final debug message
    message("Final tables in database: ", paste(dbListTables(con), collapse = ", "))
    message("Data processing completed successfully")

  }, error = function(e) {
    message("Error during data processing: ", e$message)
    print(e)
  })
}
