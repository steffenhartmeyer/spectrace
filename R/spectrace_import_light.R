#' Import raw spectrace light data
#'
#' This function imports the raw light data of the spectrace sensors. It assumes
#' that the data is stored in CSV format.
#'
#' @param lightFile Path to the file containing the light data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#' @param serial_number Serial number of spectrace device. Defaults to NULL.
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_light <- function(lightFile,
                                   tz,
                                   serial_number = NULL) {

  # Get header
  header <- readr::read_csv(
    lightFile,
    col_names = FALSE,
    col_types = readr::cols(.default = "c"),
    n_max = 3
  )

  # Check type of file (including header or not)
  if (header$X1[1] == "SERIAL") {
    serial_number <- header$X2[1]
    lightData <- readr::read_csv(
      lightFile,
      skip = 4,
      col_types = readr::cols(.default = "d")
    )
  } else {
    # Check whether serial number available
    if (is.null(serial_number)) {
      stop("No serial number specified!")
    }
    col_names <- c(
      "unix", "lux", "ch0", "ch1", "uv", "410nm", "435nm", "460nm", "485nm",
      "510nm", "535nm", "560nm", "585nm", "610nm", "645nm", "680nm", "705nm", "730nm",
      "760nm", "810nm", "860nm", "900nm", "940nm"
    )
    lightData <-
      readr::read_csv(
        lightFile,
        col_names = col_names,
        col_types = readr::cols(.default = "d")
      )
  }

  lightData <-
    lightData %>%
    dplyr::mutate(
      datetime = lubridate::as_datetime(unix, tz = tz),
      serial = serial_number
    ) %>%
    dplyr::relocate(serial, datetime)

  # Return
  return(lightData)
}
