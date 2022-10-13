#' Normalize spectrace data
#'
#' This function normalizes the spectral data.
#'
#' @param lightData Data frame containing the (calibrated) light data.
#' @param method String specifying the normalization method. If method is "peak"
#'    (default), data is normalized such that the peak (max value) is equal to 1.
#'    If method is "AUC", data is normalized such that the area under the curve
#'    is equal to 1.
#'
#' @return The original data frame with the spectral data replaced by the
#'    normalized spectral data.
#' @export
#'
#' @examples
spectrace_normalize_spectra <- function(lightData,
                                        method = c("peak", "AUC")) {
  # Match arguments
  method <- match.arg(method)

  # Get spectra
  spectra <- lightData %>%
    dyplr::select(dplyr::matches("\\d{3}nm")) %>%
    as.matrix()

  # Normalize
  spectra.norm <- switch(method,
    "peak" = apply(spectra, 1, function(x) x / max(x)),
    "AUC" = apply(spectra, 1, function(x) x / sum(x))
  )

  # Return data
  lightData <- lightData %>%
    dyplr::select(!dplyr::matches("\\d{3}nm")) %>%
    tibble::add_column(spectra.norm)
}
