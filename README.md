# njcensus

<!-- badges: start -->

[![R-CMD-check](https://github.com/yourusername/njcensus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourusername/njcensus/actions/workflows/R-CMD-check.yaml) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/PrigasG/njcensus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PrigasG/njcensus/actions/workflows/R-CMD-check.yaml) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## Overview

`njcensus` is an R package for downloading, processing, and analyzing New Jersey census demographic data. It supports both 2010 and 2020 census years and provides tools for working with demographic indicators across counties and municipalities.

## Features

-   Downloads demographic data from the Census Bureau API
-   Processes population data by race and gender
-   Stores results in a DuckDB database for efficient querying
-   Supports both 2010 and 2020 census years
-   Handles county and municipality level data

## Installation

You can install the development version of njcensus from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("PrigasG/njcensus")
```

## Usage

### Basic Usage

``` r
library(njcensus)

# Initialize using pre-packaged data (recommended)
init_census_data()

# Or fetch fresh data from Census API (optional)
init_census_data(use_packaged_data = FALSE)

# Get data for white males in 2020
white_male_data <- get_census_data("white", "male", 2020)

# Get data for Asian females in 2010
asian_female_data <- get_census_data("asian", "female", 2010)
```

## Perfomance Note
The package includes pre-packaged Census data for better performance and reliability:

* Faster initialization
* No API rate limits
* Works offline
* Consistent data access

## Available Demographics

The package supports the following demographic groups:

-   white
-   boaa (Black or African American)
-   aian (American Indian and Alaska Native)
-   asian
-   nhpi (Native Hawaiian and Pacific Islander)
-   others
-   two_more (Two or more races)

## Examples

Process census data for a specific year:

``` r
# Process 2020 census data
process_census_data(2020)
```

Querying demographic data

``` r
# Get data for specific demographics
data <- get_census_data(
  demographic = "asian",
  gender = "female",
  year = 2020
)

# View the first few rows
head(data)
```

## Database Structure

Data is stored in a DuckDB database with tables named according to the pattern: `{demographic}_{gender}_{year}` Example table names:

-   white_male_2020
-   asian_female_2010

## Contributing

Please submit issues and pull requests on GitHub. Contributions are welcome!

1.  Fork the repository
2.  Create your feature branch (`git checkout -b feature/amazing-feature`)
3.  Commit your changes (`git commit -m 'Add amazing feature'`)
4.  Push to the branch (`git push origin feature/amazing-feature`)
5.  Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Authors

George Arthur - Initial work

## Acknowledgments

-   Census Bureau API documentation
-   DuckDB documentation
-   R Spatial community
