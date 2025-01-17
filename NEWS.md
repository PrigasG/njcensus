# njcensus 0.2.0

## Major changes

-   Added pre-packaged census data support to improve reliability and speed
-   Modified `init_census_data()` to use packaged data by default

## Features

-   New parameter `use_packaged_data` in `init_census_data()`
-   Improved database initialization stability
-   Reduced dependency on Census API

## Bug fixes and improvements

-   Added proper system dependencies
-   Improved error handling in database operations
-   Enhanced test coverage

# njcensus 0.1.0

## New features

-   Added initial database operations with `init_census_data()` and `get_census_data()`
-   Implemented Census API data retrieval for 2010 and 2020
-   Added support for demographic data by race and gender
-   Created DuckDB backend for efficient data storage

## Bug fixes

-   Fixed database path handling in test environments
-   Corrected memory limit settings in DuckDB configuration

## Documentation

-   Added comprehensive vignette with examples
-   Created README with installation and basic usage instructions
