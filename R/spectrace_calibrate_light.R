#' Calibrate raw spectrace data
#'
#' This function calibrates the raw light data of the spectrace sensors based
#' on the provided calibration data.
#'
#' @param lightData Data frame with light data.
#' @param cal_data Data frame with calibration factors. Must consist of the
#'    following columns: "serial", "lux", "410nm", "435nm", "460nm", "485nm",
#'    "510nm", "535nm","560nm", "585nm", "610nm", "645nm", "680nm", "705nm",
#'    "730nm", "760nm".
#' @param gain_correction Logical. Should calibration factors be corrected for
#'    high gain when UV > 9? Defaults to TRUE.
#'
#' @return Data frame with calibrated spectral irradiance data in W/m2 (Watts
#'    per square-meter). Columns >730nm are removed.
#' @export
#'
#' @examples
spectrace_calibrate_light <- function(lightData,
                                      cal_data = NULL,
                                      gain_correction = TRUE) {
  # Check whether custom calibration data provided
  if (is.null(cal_data)) {
    cal_data <- calibration
  }

  # Stop if column names of calibration data do not match
  col_names <- c(
    "serial", "lux", "410nm", "435nm",
    "460nm", "485nm", "510nm", "535nm",
    "560nm", "585nm", "610nm", "645nm",
    "680nm", "705nm", "730nm", "760nm"
  )
  if (!all(col_names == names(cal_data))) {
    stop("Calibration file columns are not correct!")
  }

  # Serials without calibration data
  no_serial = setdiff(unique(lightData$serial), unique(cal_data$serial))
  if(length(no_serial) > 0){
    warning(sprintf(
      "For %d serial numbers no calibration data exists!\n  Average calibration data is used for the following serials:\n%s",
      length(no_serial), paste0("    ", no_serial, collapse = "\n")))
  }

  # Get calibration factors
  cal_factors <- cal_data %>%
    dplyr::add_row(calibration_avg) %>%
    dplyr::mutate(cal_serial = serial) %>%
    dplyr::select(!c("760nm", serial)) %>%
    dplyr::rename_at(
      dplyr::vars(lux, "410nm":"730nm"),
      ~ paste0("c", .x, "_factor")
    )

  # Calibrate light data
  lightData <- lightData %>%
    dplyr::mutate(cal_serial = ifelse(serial %in% no_serial,"Unknown", serial)) %>%
    dplyr::select(!c("760nm":"940nm")) %>%
    dplyr::rename_at(dplyr::vars("410nm":"730nm"), ~ paste0("c", .x)) %>%
    dplyr::left_join(cal_factors, by = c("cal_serial")) %>%
    dplyr::mutate(
      lux = lux / clux_factor,
      "410nm" = c410nm / c410nm_factor,
      "435nm" = c435nm / c435nm_factor,
      "460nm" = c460nm / c460nm_factor,
      "485nm" = c485nm / c585nm_factor,
      "510nm" = c510nm / c510nm_factor,
      "535nm" = c535nm / c535nm_factor,
      "560nm" = c560nm / c560nm_factor,
      "585nm" = c585nm / c585nm_factor,
      "610nm" = c610nm / c610nm_factor,
      "645nm" = c645nm / c645nm_factor,
      "680nm" = c680nm / c680nm_factor,
      "705nm" = c705nm / c705nm_factor,
      "730nm" = c730nm / c730nm_factor
    ) %>%
    dplyr::mutate(device_cal = ifelse(cal_serial == "Unknown", 0, 1)) %>%
    dplyr::select(!c410nm:c730nm_factor)

  # UV gain correction
  uv_factor <- 3.5
  if (gain_correction) {
    lightData <- lightData %>%
      dplyr::mutate_at(
        dplyr::vars("410nm":"730nm"),
        ~ ifelse(uv > 9, .x * uv_factor, .x)
      )
  }

  return(lightData)
}
