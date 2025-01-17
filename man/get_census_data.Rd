% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_operations.R
\name{get_census_data}
\alias{get_census_data}
\title{Read Census Data from DuckDB}
\usage{
get_census_data(demographic = NULL, gender = NULL, year = 2020)
}
\arguments{
\item{demographic}{Character. Demographic group (e.g., "white", "asian", "boaa")}

\item{gender}{Character. Gender ("male" or "female")}

\item{year}{Numeric. Census year (2010 or 2020)}
}
\value{
A data frame containing the requested census data
}
\description{
Retrieves census data for specified demographic group, gender, and year
}
\examples{
\dontrun{
white_male_2020 <- get_census_data("white", "male", 2020)
asian_female_2010 <- get_census_data("asian", "female", 2010)
}
}
