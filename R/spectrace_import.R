
#' Import raw spectrace data
#'
#' This function imports the raw output data of the spectrace sensors. It assumes
#' that the data is stored in CSV format. If activity data is provided, these data
#' will be merged with the light data.
#'
#' @param lightFile Path to the file containing the light data.
#' @param actFile Path to the file containing the activity data. Defaults to NULL.
#' @param offset Time offset between activity and light data in secs (activity - light).
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import <- function(lightFile,
                             actFile = NULL,
                             offset = 0,
                             tz) {
  # Read light data from CSV
  lightData <- readr::read_csv(lightFile,
    col_names = FALSE,
    col_types = readr::cols(.default = "d")
  )
  names(lightData) <- c(
    "unix", "lux", "ch0", "ch1", "uv", "410", "435", "460", "485",
    "510", "535", "560", "585", "610", "645", "680", "705", "730",
    "760", "810", "860", "900", "940"
  )

  # If activity file available merge with light data. Convert unix timestamp to
  # POSIXct datetime format with given time zone.
  if (!is.null(actFile)) {
    actData <- readr::read_csv(actFile,
      col_names = c("unix", "activity"),
      col_types = readr::cols(.default = "d")
    )
    lightData <- lightData %>%
      dplyr::left_join(dplyr::mutate(actData, unix = unix - offset), by = "unix") %>%
      dplyr::mutate(datetime = lubridate::as_datetime(unix, tz = tz)) %>%
      dplyr::select(datetime, unix:"940", activity)
  } else {
    lightData <- lightData %>%
      dplyr::mutate(datetime = lubridate::as_datetime(unix, tz = tz)) %>%
      dplyr::select(datetime, unix:"940")
  }

  # Return
  return(lightData)
}
