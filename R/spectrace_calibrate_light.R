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
spectrace_calibrate_light <- function(lightData, cal_data = NULL) {
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

  cal_factors = cal_data %>%
    dplyr::select(!"760nm") %>%
    dplyr::rename_at(dplyr::vars(lux, "410nm":"730nm"),
                     ~paste0("c",.x, "_factor"))

  lightData.cal = lightData %>%
    dplyr::rename_at(dplyr::vars("410nm":"730nm"),
                     ~paste0("c",.x)) %>%
    dplyr::left_join(cal_factors, by = c("serial")) %>%
    dplyr::mutate(lux = lux / clux_factor,
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
    dplyr::select(!c410nm:c730nm_factor)

  return(lightData.cal)
}
