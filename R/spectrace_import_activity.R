#' Import raw spectrace activity data
#'
#' This function imports the raw activity data of the spectrace sensors. It assumes
#' that the data is stored in CSV format.
#'
#' @param actFile Path to the file containing the activity data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#' @param serial_number Serial number of spectrace device. Defaults to NULL.
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_activity <- function(actFile, tz, serial_number = NA) {
  # Get file type (CSV or TSV)
  if (endsWith(actFile, ".csv")) {
    sep <- ","
  } else if (endsWith(actFile, ".tsv")) {
    sep <- "\t"
  } else {
    stop("Unsupported file format! Must be CSV or TSV.")
  }

  # Get header
  header <- readr::read_delim(
    actFile,
    col_names = FALSE,
    col_types = readr::cols(.default = "c"),
    n_max = 3,
    delim = sep,
    skip_empty_rows = FALSE
  )

  # Check type of file (including header or not)
  if (header$X1[1] == "SERIAL") {
    serial_number <- header$X2[1]
    actData <- utils::read.delim(
      actFile,
      skip = 5,
      header = FALSE,
      sep = sep
    ) %>% dplyr::select(c(1, 2))
  }
  # Version 3 file
  else if (header$X1[1] == "Raw Spectrace Data") {
    serial_number <- header$X2[3]
    actData <- utils::read.delim(
      actFile,
      skip = 6,
      header = FALSE,
      sep = sep
    ) %>% dplyr::select(c(1, 2))
  } else {
    # Check whether serial number available
    if (is.na(serial_number)) {
      warning("No serial number specified!")
    }
    actData <- utils::read.delim(
      actFile,
      header = FALSE,
      sep = sep
    ) %>% dplyr::select(c(1, 2))
  }

  col_names <- c("unix", "activity")
  names(actData) <- col_names

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
