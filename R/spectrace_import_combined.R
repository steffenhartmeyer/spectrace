#' Import combined spectrace data
#'
#' This function imports the light, activity, and battery spectrace data from
#' separate files and combines it into one file by matching up the timestamps of
#' the activity and battery data with the light data.
#'
#' @param lightFile Path to the file containing the light data.
#' @param activityFile Path to the file containing the activity data.
#' @param batteryFile Path to the file containing the battery data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#' @param serial_number Serial number of spectrace device. Defaults to NULL.
#'
#' @return A data frame.
#' @export
#'
#' @examples
spectrace_import_combined <- function(lightFile,
                                      activityFile = NA,
                                      batteryFile = NA,
                                      tz = NA,
                                      serial_number = NA) {
  # Get light data
  lightData <- spectrace_import_light(lightFile, tz, serial_number)

  # Import activity data
  if (!is.na(activityFile)) {
    activityData <-
      spectrace_import_activity(activityFile, tz, serial_number) %>%
      dplyr::mutate(
        unix = signal::interp1(
          as.numeric(lightData$unix),
          as.numeric(lightData$unix),
          as.numeric(.$unix),
          "nearest"
        ),
      ) %>%
      dplyr::select(unix, activity) %>%
      dplyr::ungroup()

    lightData <- lightData %>%
      dplyr::left_join(activityData, by = "unix")
  }

  # Import battery data
  if (!is.na(batteryFile)) {
    batteryData <-
      spectrace_import_battery(batteryFile, tz, serial_number)

    batteryData <- batteryData %>%
      dplyr::mutate(
        unix = signal::interp1(
          as.numeric(lightData$unix),
          as.numeric(lightData$unix),
          as.numeric(.$unix),
          "nearest"
        ),
      ) %>%
      dplyr::select(unix, battery_voltage, battery_percent, battery_isCharging) %>%
      dplyr::ungroup()

    lightData <- lightData %>%
      dplyr::left_join(batteryData, by = "unix")
  }

  return(lightData)
}
