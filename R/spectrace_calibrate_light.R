#' Calibrate raw spectrace data
#'
#' This function calibrates the raw light data of the spectrace sensors based
#' on the provided calibration data.
#'
#' @param lightData Data frame with light data.
#' @param cal_data Data frame with calibration factors. Must consist of the
#'    following columns: "serial", "lux", "410nm", "435nm", "460nm", "485nm",
#'    "510nm", "535nm","560nm", "585nm", "610nm", "645nm", "680nm", "705nm",
#'    "730nm", "760nm", "gain".
#'
#' @return Data frame with calibrated spectral irradiance data in W/m2 (Watts
#'    per square-meter). Columns >760nm are removed.
#' @export
#'
#' @examples
spectrace_calibrate_light <- function(lightData,
                                       cal_data = NULL
) {
  # Ungroup data
  if (dplyr::is_grouped_df(lightData)) {
    warning("Data frame is grouped and will be ungrouped.")
    lightData <- lightData %>% dplyr::ungroup()
  }

  # Check whether custom calibration data provided
  if (is.null(cal_data)) {
    cal_data <- calibration
  }

  # Stop if column names of calibration data do not match
  col_names <- c(
    "serial", "lux", "410nm", "435nm",
    "460nm", "485nm", "510nm", "535nm",
    "560nm", "585nm", "610nm", "645nm",
    "680nm", "705nm", "730nm", "760nm", "gain"
  )
  if (!all(col_names == names(cal_data))) {
    stop("Calibration file columns are not correct!")
  }

  # Serials without calibration data
  no_serial <- setdiff(unique(lightData$serial), unique(cal_data$serial))
  if (length(no_serial) > 0) {
    warning(sprintf(
      "For %d serial numbers no calibration data exists!\n  Average calibration data is used for the following serials:\n%s",
      length(no_serial), paste0("    ", no_serial, collapse = "\n")
    ))
  }

  # Get calibration factors
  cal_factors <- cal_data %>%
    dplyr::rename(cal_serial = serial) %>%
    dplyr::rename_at(dplyr::vars(lux, "410nm":"760nm", gain), ~paste0("c", .x, "_factor"))

  # Calibrate light data
  lightData <- lightData %>%
    spectrace_interpolate_spectra("spectrace") %>%
    # Set negatives to zero
    dplyr::mutate(dplyr::across(c(lux, dplyr::matches("^\\d{3}nm$")), ~ ifelse(.x < 0, 0, .x))) %>%
    # Divide by calibration factors
    dplyr::mutate(cal_serial = ifelse(serial %in% no_serial, "Unknown", serial)) %>%
    dplyr::rename_at(dplyr::vars("410nm":"760nm"), ~ paste0("c", .x)) %>%
    dplyr::left_join(cal_factors, by = c("cal_serial")) %>%
    dplyr::mutate(
      lux = lux / clux_factor,
      "410nm" = c410nm / c410nm_factor,
      "435nm" = c435nm / c435nm_factor,
      "460nm" = c460nm / c460nm_factor,
      "485nm" = c485nm / c485nm_factor,
      "510nm" = c510nm / c510nm_factor,
      "535nm" = c535nm / c535nm_factor,
      "560nm" = c560nm / c560nm_factor,
      "585nm" = c585nm / c585nm_factor,
      "610nm" = c610nm / c610nm_factor,
      "645nm" = c645nm / c645nm_factor,
      "680nm" = c680nm / c680nm_factor,
      "705nm" = c705nm / c705nm_factor,
      "730nm" = c730nm / c730nm_factor,
      "760nm" = c760nm / c760nm_factor
    ) %>%
    dplyr::mutate(dplyr::across(c("410nm":"760nm"), ~ ifelse(uv > 9, .x * cgain_factor, .x))) %>%
    dplyr::select(!c(
      c410nm:c760nm, c410nm_factor:c760nm_factor, clux_factor,
      cal_serial, cgain_factor
    ))

  return(lightData)
}
