#' Title
#'
#' @param lightData
#' @param referenceData
#'
#' @return
#' @export
#'
#' @examples
spectrace_encode_data <- function(lightData,
                                  referenceData = NULL) {
  # Source python scripts
  reticulate::source_python(system.file("python", "autoencoder_clustering.py",
    package = "spectrace"
  ))

  # Ungroup data
  if (dplyr::is_grouped_df(lightData)) {
    warning("Data frame is grouped and will be ungrouped.")
    lightData <- lightData %>% dplyr::ungroup()
  }

  # Spectral channels
  wl.names <- lightData %>%
    ungroup() %>%
    select(dplyr::matches("\\d{3}nm")) %>%
    names()

  if (is.null(referenceData)){
    referenceData = spectrace_reference_spectra(resolution = "1nm")
  }
  else{
    # Check reference data
    if (!"type" %in% names(referenceData)) {
      stop("Reference data must contain a 'type' column!")
    }
    if (!all(wl.names %in% names(referenceData))) {
      stop("Reference data must contain the same spectral columns as the light data!")
    }
  }

  # Prepare spectrace data
  irr_data <- lightData %>%
    spectrace_normalize_spectra() %>%
    dplyr::select(dplyr::matches("\\d{3}nm"))
  ref_data <- referenceData %>%
    spectrace_normalize_spectra() %>%
    dplyr::select(type, all_of(wl.names))

  # Encode data
  data.encoded <- py_encode_data(irr_data, ref_data)
  names(data.encoded) <- c("data", "model")

  return(data.encoded)
}
