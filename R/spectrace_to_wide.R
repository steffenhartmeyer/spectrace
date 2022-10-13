#' Spectral data to wide format
#'
#' Reshapes spectral data from long to wide format, with one column per wavelength.
#'
#' @param lightData
#'
#' @return Reshaped data.
#' @export
#'
#' @examples
spectrace_to_wide = function(lightData){
  lightData %>%
    dplyr::mutate(wl = paste0(wl, "nm")) %>%
    tidyr::pivot_wider(names_from = wl, values_from = val)
}
