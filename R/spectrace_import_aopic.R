#' Import raw spectrace data, calibrate data, and calculate alpha-opic quantities.
#'
#' This function imports raw spectrace output and calculates alpha-opic quantities.
#' See \code{\link{spectrace_import_light}},
#' \code{\link{spectrace_calibrate_light}} and \code{\link{spectrace_aopic}}
#' for more information.
#'
#' @param lightFile Path to the file containing the light data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#' @param serial_number Serial number of spectrace device. Defaults to NULL.
#' @param cal_data Data frame with calibration factors. Must consist of the
#'    following columns: "serial", "lux", "410nm", "435nm", "460nm", "485nm",
#'    "510nm", "535nm","560nm", "585nm", "610nm", "645nm", "680nm", "705nm",
#'    "730nm", "760nm". If not provided, the stored calibration data
#'    will be used (default).
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_aopic <- function(lightFile,
                                   tz,
                                   serial_number = NULL,
                                   cal_data = NULL) {
  df_raw <- spectrace_import_light(lightFile, tz)
  df_cal <- spectrace_calibrate_light(df_raw, cal_data)
  df_aopic <- spectrace_aopic(df_cal)

  return(df_aopic)
}
