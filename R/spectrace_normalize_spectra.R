#' Normalize spectrace data
#'
#' This function normalizes the spectral data.
#'
#' @param lightData Data frame containing the (calibrated) light data. Data needs
#'    to be in wide format (see \code{\link{spectrace_to_wide}}).
#' @param method String specifying the normalization method. If method is "peak"
#'    (default), data is normalized such that the peak (max value) is equal to 1.
#'    If method is "AUC", data is normalized such that the area under the curve
#'    is equal to 1. If method is "wavelength", data is normalized to 1 at specified
#'    wavelength.
#' @param wavelength Numeric. Wavelength to normalize at. Must be specified if
#'   method is "wavelength".
#' @param keepNormCoefficient Logical. Should the normalization coefficient be
#'    kept in the data? Defaults to TRUE.
#'
#' @return The original data frame with the spectral data replaced by the
#'    normalized spectral data.
#' @export
#'
#' @examples
spectrace_normalize_spectra <- function(lightData,
                                        method = c("peak", "AUC", "wavelength"),
                                        wavelength = NULL,
                                        keepNormCoefficient = TRUE) {
  # Match arguments
  method <- match.arg(method)

  # Ungroup data
  if (dplyr::is_grouped_df(lightData)) {
    warning("Data frame is grouped and will be ungrouped.")
    lightData <- lightData %>% dplyr::ungroup()
  }

  # Get spectra
  spectra <- lightData %>%
    dplyr::select(dplyr::matches("\\d{3}nm"))
  col_names <- names(spectra)
  wl.in <- sub("nm", "", col_names) %>% as.numeric()
  spectra <- as.matrix(spectra)

  if (method == "wavelength") {
    if (is.null(wavelength)) {
      stop("No wavelength for normalization specified!")
    }
    if (!(wavelength %in% wl.in)) {
      stop("Specified wavelength not in data!")
    }
  }

  # Normalize
  norm.coefficient <- switch(method,
    "peak" = apply(spectra, 1, max),
    "AUC" = apply(spectra, 1, sum),
    "wavelength" = spectra[, wl.in == wavelength]
  )
  norm.coefficient[norm.coefficient == 0] = 1
  spectra.norm <- (spectra / norm.coefficient) %>% data.frame()
  names(spectra.norm) <- col_names

  # Add to light data
  lightData <- lightData %>%
    dplyr::select(!dplyr::matches("\\d{3}nm")) %>%
    tibble::add_column(spectra.norm)

  if (keepNormCoefficient) {
    lightData <- lightData %>%
      tibble::add_column(norm.coefficient = norm.coefficient)
  }

  return(lightData)
}
