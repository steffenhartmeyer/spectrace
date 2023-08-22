#' Title
#'
#' @param lightData
#' @param referenceData
#'
#' @return
#' @export
#'
#' @examples
spectrace_encode_data = function(lightData,
                                 referenceData = NA){
  # Source python scripts
  reticulate::source_python(system.file("python", "autoencoder_clustering.py",
                                        package = "spectrace"))

  # Ungroup data
  if (dplyr::is_grouped_df(lightData)) {
    warning("Data frame is grouped and will be ungrouped.")
    lightData <- lightData %>% dplyr::ungroup()
  }

  # Prepare spectrace data
  irr_data = lightData %>% dplyr::select(dplyr::matches("\\d{3}nm"))

  # Check reference data
  if (!"type" %in% names(referenceData)){
    stop("Reference data must contain a 'type' column!")
  }
  if (!all(names(irr_data) %in% names(referenceData))){
    stop("Reference data must contain the same spectral columns as the light data!")
  }
  ref_data = referenceData %>% dplyr::select(type, names(irr_data))

  # Encode data
  data.encoded = py_encode_data(irr_data, ref_data)
  names(data.encoded) = c("data", "model")

  return(data.encoded)
}
