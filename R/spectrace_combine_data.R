#' Title
#'
#' @param lightData
#' @param activityData
#' @param batteryData
#'
#' @return
#' @export
#'
#' @examples
spectrace_combine_data = function(lightData,
                                  activityData = NULL,
                                  batteryData = NULL){
  if(!is.null(activityData)){
    actData = activityData %>%
      dplyr::mutate(
        unix = signal::interp1(as.numeric(activityData$unix),
                               as.numeric(activityData$unix),
                               as.numeric(.$unix),
                               "nearest"),
      ) %>%
      dplyr::select(unix, activity)

    lightData = lightData %>%
      dplyr::left_join(actData, by = "unix")
  }

  if(!is.null(batteryData)){
    batData = batteryData %>%
      dplyr::mutate(
        unix = signal::interp1(as.numeric(batteryData$unix),
                               as.numeric(batteryData$unix),
                               as.numeric(.$unix),
                               "nearest"),
      ) %>%
      dplyr::select(unix, battery_voltage, battery_percent, battery_isCharging)

    lightData = lightData %>%
      dplyr::left_join(batData, by = "unix")
  }

  return(lightData)
}
