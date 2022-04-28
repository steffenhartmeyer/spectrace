
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
spectrace_import <- function(actFile, tz) {
  # Read activity data from CSV
  actData <-
    readr::read_csv(actFile,
      col_names = c("unix", "activity"),
      col_types = readr::cols(.default = "d")
    ) %>%
    dplyr::mutate(datetime = lubridate::as_datetime(unix, tz = tz)) %>%
    dplyr::relocate(datetime)

  # Return
  return(actData)
}
