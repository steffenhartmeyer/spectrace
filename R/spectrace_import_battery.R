
#' Import raw spectrace battery data
#'
#' This function imports the raw battery data of the spectrace sensors. It assumes
#' that the data is stored in CSV format.
#'
#' @param actFile Path to the file containing the battery data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_battery <- function(batteryFile, tz) {
  # Read activity data from CSV
  batData <-
    readr::read_csv(batteryFile,
      col_names = c("unix", "voltage", "charge", "flags"),
      col_types = readr::cols(.default = "d")
    ) %>%
    dplyr::mutate(datetime = lubridate::as_datetime(unix, tz = tz)) %>%
    dplyr::relocate(datetime)

  # Return
  return(batData)
}
