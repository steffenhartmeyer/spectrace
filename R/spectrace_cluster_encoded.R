#' Title
#'
#' @param lightData
#' @param lightData.encoded
#' @param referenceData
#' @param model
#' @param n.clusters
#' @param n.init
#' @param n.predict
#'
#' @return
#' @export
#'
#' @examples
spectrace_cluster_encoded <- function(lightData,
                                      lightData.encoded,
                                      referenceData,
                                      model,
                                      n.clusters,
                                      n.init,
                                      n.predict) {
  # Source python scripts
  reticulate::source_python(system.file("python", "autoencoder_clustering.py",
    package = "spectrace"
  ))

  # Ungroup data
  if (dplyr::is_grouped_df(lightData)) {
    warning("Data frame is grouped and will be ungrouped.")
    lightData <- lightData %>% dplyr::ungroup()
  }

  # Prepare spectrace data
  irr_data <- lightData %>% dplyr::select(dplyr::matches("^\\d{3}nm$"))

  # Check reference data
  if (!"type" %in% names(referenceData)) {
    stop("Reference data must contain a 'type' column!")
  }
  if (!all(names(irr_data) %in% names(referenceData))) {
    stop("Reference data must contain the same spectral columns as the light data!")
  }
  ref_data <- referenceData %>% dplyr::select(type, names(irr_data))

  data.clustered <-
    py_cluster_encoded(
      irr_data,
      lightData.encoded,
      unique(referenceData$type),
      model,
      as.integer(n.clusters),
      as.integer(n.init),
      as.integer(n.predict)
    )
  data <- lightData %>%
    dplyr::select(!dplyr::matches("^\\d{3}nm$")) %>%
    tibble::add_column(data.clustered[[1]])
  classification <- data.clustered[[2]] %>%
    dplyr::mutate_if(is_list, unlist)

  return(list(data = data, classification = classification))
}
