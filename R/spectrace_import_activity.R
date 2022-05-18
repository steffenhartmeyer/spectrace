#' Import raw spectrace activity data
#'
#' This function imports the raw activity data of the spectrace sensors. It assumes
#' that the data is stored in CSV format.
#'
#' @param actFile Path to the file containing the activity data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_activity <- function(actFile, tz) {

  # Get header
  header <- readr::read_csv(
    actFile,
    col_names = FALSE,
    col_types = readr::cols(.default = "c"),
    n_max = 3
  )

  # Check type of file (including header or not)
  if (header$X1[1] == "SERIAL") {
    serial_number <- header$X2[1]
    actData <- readr::read_csv(
      actFile,
      skip = 4,
      col_types = readr::cols(.default = "d")
    )
  } else {
    # Check whether serial number available
    if (is.null(serial_number)) {
      stop("No serial number specified!")
    }
    col_names <- c("unix", "activity")
    actData <-
      readr::read_csv(
        actFile,
        col_names = col_names,
        col_types = readr::cols(.default = "d")
      )
  }

  actData <-
    actData %>%
    dplyr::mutate(
      datetime = lubridate::as_datetime(unix, tz = tz),
      serial = serial_number
    ) %>%
    dplyr::relocate(serial, datetime)

  # Return
  return(actData)
}
