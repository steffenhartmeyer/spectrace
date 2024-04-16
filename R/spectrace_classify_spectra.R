#' Classify spectra
#'
#' This function classifies each spectrum (i.e. row) by encoding the spectra using PCA
#' on the reference data and calculating the pairwise distance between the reference
#' and measured spectra. The reference spectrum with the minimum distance to the
#' measured spectrum is taken as the classification.
#'
#' @param lightData (Grouped) data frame containing the (calibrated) light data. Data needs
#'    to be in wide format (see \code{\link{spectrace_to_wide}}). Classification
#'    will be performed on the aggregated data (e.g. median) per group.
#' @param referenceData (Optional) data frame containing the reference spectra. Needs to
#'    contain the same wavelength columns as `lightData`. Must consist of unique
#'    spectra and must contain a column named `spectrum_id` identifying each
#'    spectrum. If not provided, the in-built reference spectra are used (see
#'    \code{\link{spectrace_reference_spectra}}).
#' @param distance Distance metric to use. Options are: "euclidian" (default).
#' @param return.encoded Logical. Add encoded principal components to data frame.
#'    Defaults to FALSE.
#'
#' @return The original data with a new column for the classification. If `return.encoded`
#'    is TRUE then columns with the principal component loadings are added.
#' @export
#'
#' @examples
spectrace_classify_spectra <- function(lightData,
                                       referenceData = NULL,
                                       distance = c("euclidian"),
                                       return.encoded = FALSE) {

  distance = match.arg(distance)

  # Spectral channels
  wl.names <- lightData %>%
    ungroup() %>%
    select(dplyr::matches("^\\d{3}nm$")) %>%
    names()

  if (is.null(referenceData)){
    referenceData = spectrace_reference_spectra(resolution = "1nm")
  }
  else{
    # Check reference data
    if (!"spectrum_id" %in% names(referenceData)) {
      stop("Reference data must contain a 'spectrum_id' column!")
    }
    if (!all(wl.names %in% names(referenceData))) {
      stop("Reference data must contain the same spectral columns as the light data!")
    }
  }

  # PCA on reference data
  refData.pca <- referenceData %>%
    spectrace_normalize_spectra(method = "AUC") %>%
    dplyr::select(dplyr::matches("^\\d{3}nm$")) %>%
    stats::prcomp(center = T)
  PCs <- refData.pca %>%
    broom::tidy(matrix = "eigenvalues") %>%
    dplyr::filter(percent >= 0.01 | cumulative <= 0.95)
  refData.encoded <- refData.pca %>%
    broom::augment() %>%
    dplyr::rename_with(~ gsub(".fitted", "", .x)) %>%
    dplyr::select(paste0("PC", PCs$PC)) %>%
    as.matrix()

  # Encode light data
  lightData.encoded <- lightData %>%
    spectrace_normalize_spectra(method = "AUC") %>%
    dplyr::select(dplyr::matches("^\\d{3}nm$")) %>%
    stats::predict(refData.pca, newdata=.) %>%
    tibble::as_tibble() %>%
    dplyr::select(paste0("PC", PCs$PC)) %>%
    as.matrix()

  # Calculate distance matrix
  if(distance == "euclidian"){
    distmat <- dist.euclidian(lightData.encoded, refData.encoded) %>% t()
  }

  # Classify by closest distance
  indices.closest <- apply(distmat, 1, FUN = which.min)
  lightData <- lightData %>%
    tibble::add_column(classification = referenceData$spectrum_id[indices.closest])

  if(return.encoded){
    lightData <- lightData %>%
      tibble::add_column(lightData.encoded)
  }

  return(lightData)
}
