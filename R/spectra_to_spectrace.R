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
#' @param normalize Logical. Normalize the resulting spectrum to peak = 1?
#'    Default is TRUE.
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
                                 interp_method = c("pchip", "linear"),
                                 normalize = TRUE) {
  # Match arguments
  output_resolution <- match.arg(output_resolution)
  interp_method <- match.arg(interp_method)

  # Get number of rows
  N <- nrow(spectralData)

  # Ungroup data
  if (dplyr::is_grouped_df(spectralData)) {
    warning("Data frame is grouped and will be ungrouped.")
    spectralData <- spectralData %>% dplyr::ungroup()
  }

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
  reso.num <- as.numeric(substr(resolution, 1, 1))

  # Choose spectrace responses in matching resolution
  spectrace_responses <- switch(resolution,
    "1nm" = spectrace_responses_1nm,
    "5nm" = spectrace_responses_1nm[seq(1,401,5),]
  )

  # Concolve with Spectrace responses
  responses.spectra <- spectrace_responses %>%
    apply(2, function(x) spectra %*% as.numeric(x) * reso.num)

  # As data frame
  if (N == 1) {
    responses.spectra <- responses.spectra %>% as.list()
  }
  responses.spectra <- responses.spectra %>%
    tibble::as_tibble()

  # Normalize spectrum
  if (normalize) {
    responses.spectra <- responses.spectra %>%
      spectrace_normalize_spectra(keepNormCoefficient = FALSE)
  }

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
