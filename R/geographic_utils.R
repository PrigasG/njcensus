#' Geographic Reference Data Cache
#'
#' @keywords internal
.geo_cache <- new.env()

#' Get New Jersey Geographic Reference Data
#'
#' @description Creates geographic reference data for New Jersey counties and municipalities
#' @param year Census year (2010 or 2020)
#' @return A data frame containing geographic reference information
#' @import tigris
#' @import sf
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' geo_ref <- get_nj_geo_reference(2020)
#' }
get_nj_geo_reference <- function(year) {
  cache_key <- paste0("geo_ref_", year)
  if(exists(cache_key, envir = .geo_cache)) {
    return(get(cache_key, envir = .geo_cache))
  }

  nj_counties <- tigris::counties(state = "NJ", cb = TRUE, year = year) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(COUNTYFP, NAME) %>%
    dplyr::rename(county_name = NAME)

  # Get municipalities with conditional column selection
  nj_municipalities <- tigris::county_subdivisions(state = "NJ", cb = TRUE, year = year) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_drop_geometry() %>%
    {
      if(year == 2020) {
        dplyr::select(., COUSUBFP, NAME, COUNTYFP) %>%
          dplyr::rename(
            municipality_name = NAME,
            `county subdivision` = COUSUBFP
          )
      } else {
        dplyr::select(., COUSUB, NAME, COUNTYFP) %>%
          dplyr::rename(
            municipality_name = NAME,
            `county subdivision` = COUSUB
          )
      }
    }

  # Join to create reference table
  result <- nj_municipalities %>%
    dplyr::left_join(nj_counties, by = "COUNTYFP")

  assign(cache_key, result, envir = .geo_cache)
  return(result)
}
