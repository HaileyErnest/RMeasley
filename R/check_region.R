#' Helper: Check that a region is valid
#'
#' @param region A string input of a region name. Valid regions are "Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", and "Western Pacific".
#'
#' @returns A cleaned region name if the region is valid.

check_region <- function(region) {

  if (!is.character(region)) {
    stop("Please enter a string value.")
  }
  region_input <- stringr::str_to_title(region)

  valid_regions <- c(
    "Africa",
    "Americas",
    "Eastern Mediterranean",
    "Europe",
    "South-East Asia",
    "Western Pacific"
  )

  if (!region_input %in% valid_regions) {
    stop("Please enter a valid region.")
  }

  region_input
}
