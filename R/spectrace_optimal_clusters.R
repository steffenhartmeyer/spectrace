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
spectrace_optimal_clusters <- function(lightData,
                                      method = c("kmeans", "kmedoids-pam", "kmedoids-clara"),
                                      encoding = c("PCA", "none"),
                                      normalization = c("AUC", "peak"),
                                      max.n.clusters = 10,
                                      n.init = 100,
                                      n.samples = 100) {
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

  scores = list()
  i = 0
  for(k in 1:max.n.clusters){
    i = i + 1
    # kmeans clustering
    if (method == "kmeans") {
      kmeans <- lightData.encoded %>%
        kmeans(centers = k, nstart = n.init, algorithm = "MacQueen", iter.max = 100)
      lightData.encoded.clustered <-lightData.encoded %>%
        tibble::add_column(cluster_id = kmeans$cluster)

      n.clust = lightData.encoded.clustered %>% dplyr::count(cluster_id)
      if(any(n.clust$n < 2)){
        stop("At least one cluster consists of less than 2 observations.
           Data cannot be clustered further")
      }

      # Calculate silhouette scores
      subsample <- function(x) {
        sample <- lightData.encoded.clustered %>%
          dplyr::group_by(cluster_id) %>%
          dplyr::slice_sample(n = floor(samplesize / k)) %>%
          dplyr::ungroup()
        x <- sample$cluster_id
        d <- sample %>% dplyr::select(!cluster_id) %>% dist()
        summary(cluster::silhouette(x, d))$clus.avg.widths
      }
      sil.scores <- sapply(1:n.samples, subsample) %>%
        apply(1, mean) %>%
        as.numeric()
    }
    # kmedoids clustering using pam
    if (method == "kmedoids-pam") {
      pam <- lightData.encoded %>% cluster::pam(k = k, variant = "faster")
      sil.scores <- pam$silinfo$clus.avg.widths
    }

    # kmedoids clustering using clara
    if (method == "kmedoids-clara") {
      clara <- lightData.encoded %>%
        cluster::clara(k = k, samples = n.samples, sampsize = samplesize, rngR = TRUE)
      sil.scores <- clara$silinfo$clus.avg.widths
    }

    # Sil scores to data frame
    scores[[i]] <- tibble::tibble(n_clusters = k, cluster_id = 1:k, sil_score = sil.scores)
  }

  return(scores)
}
