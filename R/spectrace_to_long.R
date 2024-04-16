#' Spectral data to long format
#'
#' Reshapes spectral data to long format, with one variable for the wavelength and
#' one for the power per wavelength.
#'
#' @param lightData Data frame with light data. Must contain one column per
#'    wavelength, named '[wavelength]nm' (e.g., '380nm').
#'
#' @return Reshaped data.
#' @export
#'
#' @examples
spectrace_to_long <- function(lightData) {
  lightData %>%
    tidyr::gather(wl, val, dplyr::matches("^\\d{3}nm$")) %>%
    dplyr::mutate(wl = as.numeric(sub("nm", "", wl)))
}
