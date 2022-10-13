#' Convert spectrum to Spectrace-like spectrum
#'
#' This function converts a spectrum to the normalized spectrum as Spectrace would "see" it.
#' For this, the original spectrum is convolved with the Spectrace response curves
#' of each channel and then normalized. By default the resulting spectrum is
#' interpolated back to the original spectrum's resolution, but a custom resolution
#' or no interpolation can be selected as well.
#'
#' @param spectralData Data frame containing the spectral data in reach row with
#'    wavelength as column names (e.g. "380nm", "385nm", ..., "780nm"). The
#'    wavelengths must be between 380-780nm in either 1nm or 5nm resolution.
#'    Additional variables are allowed.
#' @param output_resolution String specifying the resolution of the output
#'    spectrum. Can be one of ("original", "spectrace", "1nm", "5mm")).
#'    Default is the resolution of the original spectrum. If "spectrace" is
#'    specified, no interpolation will be performed.
#' @param interp_method The interpolation method. Can be either "pchip" (default)
#'    or "linear". Pchip (piecewise cubic hermetic interpolation) results in a
#'    smooth spectrum while preserving the source values as local minima/maxima.
#'    Will be ignored if \code{output_resolution} is "spectrace".
#'
#' @return The original data frame with the spectral data replaced by the new
#'    spectral data (normalized).
#' @export
#'
#' @examples
spectra_to_spectrace <- function(spectralData,
                                 output_resolution = c(
                                   "original", "spectrace",
                                   "1nm", "5nm"
                                 ),
                                 interp_method = c("pchip", "linear")) {

  # Match arguments
  output_resolution <- match.arg(output_resolution)
  interp_method <- match.arg(interp_method)

  # Subset spectral data
  spectra <- spectralData %>%
    dplyr::select("380nm":"780nm") %>%
    as.matrix()

  # Get wavelengths
  wl <- as.numeric(gsub("nm", "", colnames(spectra)))

  # Get resolution
  resolution <- paste0(wl[2] - wl[1], "nm")
  if (!(resolution %in% c("1nm", "5nm"))) {
    stop("Spectral data has wrong resolution. Must be either 1nm or 5nm!")
  }

  # Choose spectrace responses in matching resolution
  spectrace_responses <- switch(resolution,
    "1nm" = spectrace_responses_1nm,
    "5nm" = spectrace_responses_5nm
  )

  # Concolve with Spectrace responses
  responses.spectra <- spectrace_responses %>%
    apply(2, function(x) spectra %*% as.numeric(x)) %>%
    apply(1, function(x) x / max(x)) %>%
    t() %>%
    tibble::as_tibble()

  # Interpolate to desired resolution
  if (output_resolution != "spectrace") {
    if (output_resolution == "original") {
      output_resolution <- resolution
    }
    responses.spectra <- responses.spectra %>%
      spectrace_interpolate_spectra(
        resolution = output_resolution,
        interp_method = interp_method
      )
  }

  # Add new spectra back into data
  spectralData <- spectralData %>%
    dplyr::select(!c("380nm":"780nm")) %>%
    tibble::add_column(responses.spectra)
}
