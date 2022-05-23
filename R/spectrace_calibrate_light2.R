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
#'
#' @return Data frame with calibrated data. Columns >730nm are removed.
#' @export
#'
#' @examples
spectrace_calibrate_light2 <- function(lightData, cal_data = NULL) {
  if (is.null(cal_data)) {
    cal_data <- calibration
  }

  # Stop if column names do not match
  col_names <- c(
    "serial", "lux", "410nm", "435nm",
    "460nm", "485nm", "510nm", "535nm",
    "560nm", "585nm", "610nm", "645nm",
    "680nm", "705nm", "730nm", "760nm"
  )
  if (!all(col_names == names(cal_data))) {
    stop("Calibration file columns are not correct!")
  }

  # Stop if more than one serial number in data
  serial_number <- unique(lightData$serial)
  if (length(serial_number) > 1) {
    stop("More than one serial number in the provided data!")
  }

  # Calibration factors spectral data
  cal_factors <- cal_data %>%
    dplyr::filter(serial == serial_number) %>%
    dplyr::select("410nm":"730nm") %>%
    as.numeric()

  # Calibration factor lux data
  lux_factor <- cal_data %>%
    dplyr::filter(serial == serial_number) %>%
    dplyr::select(lux) %>%
    as.numeric()

  # Raw spectral output
  rawData <- lightData %>%
    dplyr::select("410nm":"730nm")

  # Convert raw spectrace output to irradiance (W/m2)
  irrData <- rawData / cal_factors[col(rawData)]

  # Return data frame
  lightData.cal <- lightData %>%
    dplyr::select(!c("410nm":"730nm")) %>%
    tibble::add_column(irrData) %>%
    dplyr::mutate(lux = lux / lux_factor)
  return(lightData.cal)
}
