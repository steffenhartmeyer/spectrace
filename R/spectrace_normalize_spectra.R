#' Normalize spectrace data
#'
#' This function normalizes the spectral data.
#'
#' @param lightData Data frame containing the (calibrated) light data.
#' @param method String specifying the normalization method. If method is "peak"
#'    (default), data is normalized such that the peak (max value) is equal to 1.
#'    If method is "AUC", data is normalized such that the area under the curve
#'    is equal to 1.
#' @param keepNormCoefficient Logical. Should the normalization coefficient be
#'    kept in the data? Defaults to TRUE.
#'
#' @return The original data frame with the spectral data replaced by the
#'    normalized spectral data.
#' @export
#'
#' @examples
spectrace_normalize_spectra <- function(lightData,
                                        method = c("peak", "AUC"),
                                        keepNormCoefficient = TRUE) {
  # Match arguments
  method <- match.arg(method)

  # Get spectra
  spectra <- lightData %>%
    dplyr::select(dplyr::matches("\\d{3}nm"))
  col_names <- names(spectra)
  spectra <- as.matrix(spectra)

  # Normalize
  norm.coefficient <- switch(method,
    "peak" = apply(spectra, 1, max),
    "AUC" = apply(spectra, 1, sum)
  )
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
