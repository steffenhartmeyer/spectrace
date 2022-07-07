#' Import raw spectrace battery data
#'
#' This function imports the raw battery data of the spectrace sensors. It assumes
#' that the data is stored in CSV format.
#'
#' @param batteryFile Path to the file containing the battery data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#' @param serial_number Serial number of spectrace device. Defaults to NULL.
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_battery <- function(batteryFile, tz, serial_number = NULL) {

  # Get header
  header <- readr::read_csv(
    batteryFile,
    col_names = FALSE,
    col_types = readr::cols(.default = "c"),
    n_max = 3
  )

  # Check type of file (including header or not)
  if (header$X1[1] == "SERIAL") {
    serial_number <- header$X2[1]
    batData <- readr::read_csv(
      batteryFile,
      skip = 4,
      col_types = readr::cols(.default = "d")
    )
  } else {
    # Check whether serial number available
    if (is.null(serial_number)) {
      stop("No serial number specified!")
    }
    col_names <- c("unix", "voltage", "charge", "flags")
    batData <-
      readr::read_csv(
        batteryFile,
        col_names = col_names,
        col_types = readr::cols(.default = "d")
      )
  }

  batData <-
    batData %>%
    dplyr::mutate(
      datetime = lubridate::as_datetime(unix, tz = tz),
      serial = serial_number
    ) %>%
    dplyr::relocate(serial, datetime)

  # Return
  return(batData)
}
