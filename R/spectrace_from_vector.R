#' Unpack vector spectral data into columns
#'
#' This function unpacks spectral data stored in a vector into wavelength columns.
#'
#' @param lightData Data frame containing the light data. Column with vectors of
#'    spectral data must be named "spectrum" and with wavelength data must be named
#'    "wavelength".
#' @return The data frame with the spectral data with one column per wavelength.
#' @export
#'
#' @examples
spectrace_from_vector <- function(lightData) {

  # Get wavelengths
  wavelength <- lightData$wavelength[[1]]
  col_names <- paste0(wavelength, "nm")

  # Get spectra
  irr_data <- lightData$spectrum %>%
    unlist() %>%
    matrix(nrow = nrow(lightData), byrow = TRUE) %>%
    data.frame()
  names(irr_data) <- col_names

  # Add to data frame and return
  lightData %>%
    dplyr::select(!c(spectrum, wavelength)) %>%
    tibble::add_column(irr_data)
}
