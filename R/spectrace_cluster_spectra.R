#' Cluster spectrace data
#'
#' This function allows to cluster spectrace data.
#'
#' @param lightData Data frame containing the (calibrated) light data. Data needs
#'    to be in wide format (see \code{\link{spectrace_to_wide}}).
#' @param method Clustering method. Must be one of ['kmeans', 'kmedoids-pam',
#'    'kmedoids-clara']. Defaults to 'kmeans'.
#' @param encoding Encoding method. Must be one of ['PCA', 'none']. Defaults to
#'    'PCA'.
#' @param normalization Normalization method (see \code{\link{spectrace_normalize_spectra}}).
#'    Defaults to 'AUC'.
#' @param n.clusters Integer, indicating the number of clusters.
#' @param n.init Integer, indicting the number of random initializations for kmeans.
#'    Defaults to 100.
#' @param n.samples Integer, indicating the number of random samples for the clara
#'    algorithm and bootstrapped silhouette score for the kmeans algorithm. Defaults
#'    to 100.
#' @param samplesize Integer, indicating the size of each sample for the clara
#'    algorithm and bootstrapped silhouette score for the kmeans algorithm. Can be
#'    between 1 and N, with N being the number of observations. Defaults to
#'    `min(N, 100 * n.clusters)`.
#' @param classify Logical, indicating whether the cluster medians should be classified.
#'    See \code{\link{spectrace_classify_spectra}}. Defaults to FALSE.
#' @param referenceData Data frame with the reference data for classification.
#'    See \code{\link{spectrace_classify_spectra}}. If not provided (default), the
#'    in-built reference data will be used.
#' @param clusters.only Logical, indicating whether only a vector with the cluster
#'    ids should be returned. Defaults to FALSE
#' @param return.sil Logical, indicting whether average silhouette scores per cluster
#'    should be returned. Defaults to FALSE.
#' @param return.encoded Logical, indicting whether the encoded data should be returned.
#'    Defaults to FALSE.
#' @param return.plot Logical, indicating whether the plot should be returned.
#'    Defaults to FALSE.
#' @param return.classification Logical, indicating whether the classification
#'    should be returned. Defaults to FALSE.
#'
#' @return The original `lightData` with an additional column named 'cluster_id'
#'    indicating the cluster of each spectrum in the data. If `clusters.only=TRUE`,
#'    a vector with the cluster_ids is returned. If `return.sil=TRUE` a named list
#'    is returned with two entries: `data` holding the data frame or cluster_id vector
#'    and `sil_scores` the silhouette scores.
#' @export
#'
#' @examples
spectrace_cluster_spectra <- function(lightData,
                                      method = c("kmeans", "kmedoids-pam", "kmedoids-clara"),
                                      encoding = c("PCA", "none"),
                                      normalization = c("AUC", "peak"),
                                      n.clusters,
                                      n.init = 100,
                                      n.samples = 100,
                                      samplesize = 100 * n.clusters,
                                      classify = FALSE,
                                      referenceData = NULL,
                                      clusters.only = FALSE,
                                      return.sil = FALSE,
                                      return.encoded = FALSE,
                                      return.plot = FALSE,
                                      return.classification = FALSE) {
  # Set random number generator
  set.seed(1)

  # Match arguments
  encoding <- match.arg(encoding)
  normalization <- match.arg(normalization)
  method <- match.arg(method)

  N <- nrow(lightData)
  if (samplesize > N) {
    samplesize <- N
  }

  # Normalisation
  lightData <- lightData %>%
    spectrace_normalize_spectra(method = normalization)

  # PCA
  if (encoding == "PCA") {
    lightData.encoded <- lightData %>%
      dplyr::select(dplyr::matches("\\d{3}nm")) %>%
      prcomp(center = T)

    # Select PCs to retain
    PCs <- lightData.encoded %>%
      broom::tidy(matrix = "eigenvalues") %>%
      dplyr::filter(percent >= 0.01 | cumulative <= 0.95)
    lightData.encoded <- lightData.encoded %>%
      broom::augment() %>%
      dplyr::rename_with(~ gsub(".fitted", "", .x)) %>%
      dplyr::select(paste0("PC", PCs$PC))
  }
  if (encoding == "none") {
    lightData.encoded <- lightData %>%
      dplyr::select(dplyr::matches("\\d{3}nm"))
  }

  # kmeans clustering
  if (method == "kmeans") {
    kmeans <- lightData.encoded %>%
      kmeans_sil(k = n.clusters, n.init, 100, n.samples, samplesize)
    lightData.clustered <- lightData %>% tibble::add_column(cluster_id = kmeans$clustering)
    sil.scores <- kmeans$clus.avg.widths
  }
  # kmedoids clustering using pam
  if (method == "kmedoids-pam") {
    pam <- lightData.encoded %>% cluster::pam(k = n.clusters, variant = "faster")
    lightData.clustered <- lightData %>% tibble::add_column(cluster_id = pam$clustering)
    sil.scores <- pam$silinfo$clus.avg.widths
  }
  # kmedoids clustering using clara
  if (method == "kmedoids-clara") {
    clara <- lightData.encoded %>%
      cluster::clara(k = n.clusters, samples = n.samples, sampsize = samplesize, rngR = TRUE)
    lightData.clustered <- lightData %>% tibble::add_column(cluster_id = clara$clustering)
    sil.scores <- clara$silinfo$clus.avg.widths
  }

  # Sil scores to data frame
  sil.scores <- tibble::tibble(cluster_id = 1:n.clusters, sil_score = sil.scores)

  # PCA in plot
  if (encoding == "PCA") {
    lightData.PCA = lightData.encoded %>%
      tibble::add_column(cluster_id = lightData.clustered$cluster_id)
  }
  else{
    lightData.PCA = NULL
  }

  # Classification
  if (classify) {
    if(is.null(referenceData)){
      referenceData = spectrace_reference_spectra()
    }
    classification = lightData.clustered %>%
      dplyr::group_by(cluster_id) %>%
      spectrace_classify_clusters(referenceData, n.classes = 5) %>%
      dplyr::rename(spectrum_id = classification)

    classification.best = dplyr::slice(classification, 1, .by = "cluster_id")
  }
  else{
    classification = NULL
    classification.best = NULL
  }

  # Plot clusters
  plot <- spectrace_plot_clusters(
    lightData = lightData.clustered,
    lightData.PCA = lightData.PCA,
    classification = classification.best,
    sil.scores = sil.scores,
    samplesize = 500
  )

  # Print summary
  print.header = "SPECTRACE CLUSTERING SUMMARY"
  print.encoding = sprintf("Encoding method: %s", encoding)
  if (encoding == "PCA") {
    print.pca =
      sprintf(
        "Number of principal components: %s\nVariance explained by PCs: %s",
        nrow(PCs),
        str_flatten(paste0(round(PCs$percent,2)*100, "%"), collapse = " ")
      )
    print.encoding = paste(print.encoding, print.pca, sep = "\n")
  }
  print.clustering =
    sprintf(
      "Clustering method: %s\nNumber of clusters: %s\nMean silhouette score: %s",
      method,
      n.clusters,
      round(mean(sil.scores$sil_score), 4)
    )
  cat(paste(print.header, print.encoding, print.clustering, sep = "\n\n"), "\n")


  # Return clustering vector instead of data frame
  if (clusters.only) {
    lightData.clustered <- lightData.clustered$cluster_id
  }
  # Return as list
  if (return.sil | return.encoded | return.plot | return.classification) {
    return.dat <- list(data = lightData.clustered)
  }
  else{
    return.dat <- lightData.clustered
  }
  # Return encoded data
  if (return.encoded) {
    return.dat <- c(return.dat, list(data_encoded = lightData.encoded))
  }
  # Return silhouette scores
  if (return.sil) {
    return.dat <- c(return.dat, list(sil_scores = sil.scores))
  }
  # Return plot
  if (return.plot) {
    return.dat <- c(return.dat, list(plot = plot))
  }
  # Return classification
  if (return.classification) {
    return.dat <- c(return.dat, list(classification = classification))
  }

  return(return.dat)
}
