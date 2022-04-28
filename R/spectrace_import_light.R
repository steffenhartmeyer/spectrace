
#' Import raw spectrace light data
#'
#' This function imports the raw light data of the spectrace sensors. It assumes
#' that the data is stored in CSV format.
#'
#' @param lightFile Path to the file containing the light data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_light <- function(lightFile, tz) {
  # Read light data from CSV
  col_names <- c(
    "unix", "lux", "ch0", "ch1", "uv", "410", "435", "460", "485",
    "510", "535", "560", "585", "610", "645", "680", "705", "730",
    "760", "810", "860", "900", "940"
  )
  lightData <-
    readr::read_csv(lightFile,
      col_names = col_names,
      col_types = readr::cols(.default = "d")
    ) %>%
    dplyr::mutate(datetime = lubridate::as_datetime(unix, tz = tz)) %>%
    dplyr::relocate(datetime)

  # Return
  return(lightData)
}
