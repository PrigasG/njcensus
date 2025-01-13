#' Create a spinning progress indicator
#'
#' @description Creates a spinning animation with optional percentage progress
#' @return A function that updates the spinner display
#' @examples
#' spinner <- create_spinner()
#' spinner("Processing...", 50)
#' @export
create_spinner <- function() {
  chars <- c("-", "\\", "|", "/")
  i <- 1
  function(text = "", percent = NULL) {
    progress_text <- if(!is.null(percent)) {
      sprintf("%s [%d%%]", text, percent)
    } else {
      text
    }
    cat(sprintf("\r%s %s", chars[i], progress_text))
    i <<- if(i >= length(chars)) 1 else i + 1
    flush.console()
  }
}

#' Extract pattern for Census API queries
#'
#' @param x Base pattern string
#' @param n Number to increment
#' @param year Census year (2010 or 2020)
#' @return Modified pattern string
#' @keywords internal
extract_pattern <- function(x, n, year) {
  first_part <- substr(x, 1, 7)
  second_part <- as.integer(substr(x, 8, 10))
  if(year == 2020) {
    next_pattern <- paste0(first_part, sprintf("%03d", second_part + n), "N")
  } else {
    next_pattern <- paste0(first_part, sprintf("%03d", second_part + n))
  }
  return(next_pattern)
}
