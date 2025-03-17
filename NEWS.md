# njcensus 0.2.0

## Major changes

-   Added pre-packaged census data support to improve reliability and speed
-   Modified `init_census_data()` to use packaged data by default
-   Added population estimates data functionality (2021-2023)
-   Added filtering capabilities for counties and municipalities

## Features

-   New parameter `use_packaged_data` in `init_census_data()`
-   New parameter `include_pop_estimates` to optionally include population estimates
-   Added `get_pop_estimates()` function for accessing population estimates data
-   Added county and municipality filtering in `get_census_data()`
-   Improved database initialization with smart detection of existing data
-   Reduced dependency on Census API

## Bug fixes and improvements

-   Added proper system dependencies
-   Improved error handling in database operations
-   Enhanced test coverage
-   Made console messages more professional
-   Fixed syasex file availability issue (only available for 2023+)
-   Improved namespace management with zzz.R approach

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
