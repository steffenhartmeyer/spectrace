#' Classify aggregated spectra
#'
#' This function classifies (groups) of spectra by first aggregating the data and
#' then correlating the aggregated spectra to reference spectra, identifying the best
#' matching reference spectra.
#'
#' @param lightData (Grouped) data frame containing the (calibrated) light data. Data needs
#'    to be in wide format (see \code{\link{spectrace_to_wide}}). Classification
#'    will be performed on the aggregated data (e.g. median) per group.
#' @param referenceData Data frame containing the reference spectra. Needs to
#'    contain the same wavelength columns as `lightData`. Must consist of unique
#'    spectra and must contain a column named `spectrum_id` identifying each
#'    spectrum.
#' @param aggregation Aggregation method. Must be one of ['median', 'mean']. Defaults
#'    to 'median'.
#' @param method Classification method. Must be one of ['correlation']. Defaults to
#'    'correlation'.
#' @param n.classes Integer indicating the number of predicted classes per group
#'    to include in the output. Defaults to 5.
#'
#' @return A data frame with the best 'n.classes' classifications (i.e. the
#'    'spectrum_id') and corresponding coefficients per group.
#' @export
#'
#' @examples
spectrace_classify_spectra <- function(lightData,
                                       referenceData,
                                       aggregation = c("median", "mean"),
                                       method = c("correlation"),
                                       n.classes = 5) {
  # Function to pick the n largest values
  maxn <- function(x, n) {
    partial <- length(x) - n + 1
    x[x >= sort(x, partial = partial)[partial]]
  }

  # Match arguments
  aggregation <- match.arg(aggregation)
  method <- match.arg(method)

  # Grouping vars
  group_vars <- group_vars(lightData)

  # Spectral channels
  wl.names <- lightData %>%
    ungroup() %>%
    select(dplyr::matches("\\d{3}nm")) %>%
    names()

  # Check reference data
  if (!"spectrum_id" %in% names(referenceData)) {
    stop("Reference data must contain a 'spectrum_id' column!")
  }
  if (!all(wl.names %in% names(referenceData))) {
    stop("Reference data must contain the same spectral columns as the light data!")
  }

  # Reference data as matrix
  referenceData.mat <- referenceData %>%
    spectrace_normalize_spectra(method = "AUC") %>%
    dplyr::select(spectrum_id, all_of(wl.names)) %>%
    spectrace_to_long() %>%
    tidyr::pivot_wider(names_from = spectrum_id, values_from = val) %>%
    dplyr::select(!wl)

  # Aggregate light data
  lightData.aggregated <- lightData %>%
    dplyr::summarise_at(dplyr::vars(dplyr::matches("\\d{3}nm")), aggregation) %>%
    dplyr::ungroup()

  # Classify light data
  if (method == "correlation") {
    classification <- lightData.aggregated %>%
      spectrace_normalize_spectra(method = "AUC") %>%
      dplyr::nest_by(across(all_of(group_vars))) %>%
      dplyr::mutate(cor(as.numeric(data), referenceData.mat) %>% tibble::as_tibble()) %>%
      dplyr::select(!data) %>%
      dplyr::nest_by(across(all_of(group_vars))) %>%
      dplyr::mutate(
        classification = list(names(data)[which(as.numeric(data) %in% maxn(as.numeric(data), n.classes))]),
        coeff = list(round(maxn(as.numeric(data), n.classes), 4))
      ) %>%
      tidyr::unnest(cols = c(classification, coeff)) %>%
      dplyr::select(!data) %>%
      dplyr::arrange(desc(coeff), .by_group = TRUE)
  }

  return(classification)
}
