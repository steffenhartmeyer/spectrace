#' Pack spectral data into vectors
#'
#' This function packs spectral data into a vector.
#'
#' @param lightData Data frame containing the light data. Column names for
#'    spectral data must be the wavelength followed by "nm", for example: "380nm".
#' @return The data frame with the spectrum and wavelength as vectors.
#' @export
#'
#' @examples
spectrace_to_vector <- function(lightData) {
  # Get spectra
  irrData <- lightData %>%
    dplyr::select(dplyr::matches("\\d{3}nm")) %>%
    as.matrix()

  # Input wavelengths
  wl.in <- sub("nm", "", colnames(irrData)) %>%
    as.numeric()

  # Make vectors
  spectra <- data.frame(idx = 1:nrow(irrData))
  spectra$spectrum <- lapply(seq_len(nrow(irrData)), function(i) irrData[i, ])
  spectra$wavelength <- rep_len(list(wl.in), nrow(irrData))
  spectra <- dplyr::select(spectra, spectrum, wavelength)

  # Add to data frame and return
  lightData %>%
    dplyr::select(!dplyr::matches("\\d{3}nm"))
  tibble::add_column(spectra)
}
