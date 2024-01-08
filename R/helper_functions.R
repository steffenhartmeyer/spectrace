sam <- function(s, r) {
  acos(sum(s * r) / sqrt(sum(s^2) * sum(r^2)))
}

scm <- function(s, r) {
  1 - ((1 + cor(s, r)) / 2)
}

ecs <- function(s, r) {
  sqrt(sum((cumsum(s) - cumsum(r))^2))
}

parse_timeunit_tosecs <- function(input) {
  if (!stringr::str_detect(input, "\\d+ (s|m|h|d)")) {
    stop("Wrong time unit specification! Must be '[numeric] ['s','m','h','d']'.")
  }
  parsed <- stringr::str_split(input, " ")[[1]]
  list(
    secs = to.secs(as.numeric(parsed[1]), parsed[2]),
    time = parsed[1],
    unit = parsed[2]
  )
}

find_clusters <- function(x, min_length, max_interrupt = 0, cluster_name = "cluster") {
  # Replace NA with FALSE
  x <- tidyr::replace_na(x, FALSE)

  # Find the start and end indices of each potential cluster period
  start_indices <- which(x & !dplyr::lag(x, default = FALSE))
  end_indices <- which(x & !dplyr::lead(x, default = FALSE))
  ranges <- as.numeric(matrix(rbind(start_indices, end_indices), nrow = 2))

  # Remove ranges < min_length
  intra_diff <- diff(ranges)[1:(length(ranges) - 1) %% 2 != 0] + 1
  exclude_ranges <- c(
    which(intra_diff < min_length) * 2,
    which(intra_diff < min_length) * 2 - 1
  )
  if (length(exclude_ranges) > 0) {
    ranges <- ranges[-exclude_ranges]
  }

  # Combine ranges with inter-range difference <= max_interrupt
  inter_diff <- diff(ranges)[1:(length(ranges) - 1) %% 2 == 0] - 1
  exclude_ranges <- c(
    which(inter_diff <= max_interrupt) * 2,
    which(inter_diff <= max_interrupt) * 2 + 1
  )
  if (length(exclude_ranges) > 0) {
    ranges <- ranges[-exclude_ranges]
  }

  # Make intervals
  if (length(ranges) > 0) {
    intervals <-
      matrix(ranges, ncol = 2, byrow = TRUE) %>%
      as.data.frame() %>%
      magrittr::set_names(c("cluster_start", "cluster_end")) %>%
      dplyr::mutate(cluster_idx = 1:length(cluster_start)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(idx = list(seq(cluster_start, cluster_end))) %>%
      tidyr::unnest(cols = c(idx)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(is_cluster = TRUE) %>%
      dplyr::relocate(idx, is_cluster, cluster_idx, cluster_start, cluster_end) %>%
      dplyr::rename_with(~ gsub("cluster", cluster_name, .x))
  } else {
    intervals <-
      tibble::tibble(
        idx = 1, is_cluster = NA, cluster_idx = NA,
        cluster_start = NA, cluster_end = NA
      ) %>%
      dplyr::rename_with(~ gsub("cluster", cluster_name, .x))
  }

  return(intervals)
}

find_cluster_timings = function(x, datetime, sampling_int = 60){
  x1 = x
  x2 = c(Inf, x[1:length(x)-1])
  start_indices = which(x1 != x2 | xor(is.na(x1), is.na(x2)))

  x1 = c(x[2:length(x)], Inf)
  x2 = x
  end_indices = which(x1 != x2 | xor(is.na(x1), is.na(x2)))

  ranges <- as.numeric(matrix(rbind(start_indices, end_indices), nrow = 2))

  intervals <-
    matrix(ranges, ncol = 2, byrow = TRUE) %>%
    as.data.frame() %>%
    magrittr::set_names(c("cluster_start", "cluster_end")) %>%
    dplyr::mutate(cluster_idx = 1:length(cluster_start)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(idx = list(seq(cluster_start, cluster_end))) %>%
    tidyr::unnest(cols = c(idx)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cluster_start = datetime[cluster_start],
                  cluster_end = datetime[cluster_end]+sampling_int) %>%
    dplyr::select(cluster_idx, cluster_start, cluster_end)

  return(intervals)
}

find_clusters2 <- function(x,
                           min_length,
                           max_interrupt = 0,
                           prop_interrupt = 1,
                           cluster_name = "cluster") {
  # Replace NA with FALSE
  x <- tidyr::replace_na(x, FALSE)

  # Find the start and end indices of each potential non-wear period
  start_indices <- which(x & !dplyr::lag(x, default = FALSE))
  end_indices <- which(x & !dplyr::lead(x, default = FALSE))
  ranges <- as.numeric(matrix(rbind(start_indices, end_indices), nrow = 2))

  # Remove ranges < min_length
  intra_diff <- diff(ranges)[1:(length(ranges) - 1) %% 2 != 0] + 1
  exclude_ranges <- c(
    which(intra_diff < min_length) * 2,
    which(intra_diff < min_length) * 2 - 1
  )
  if (length(exclude_ranges) > 0) {
    ranges <- ranges[-exclude_ranges]
  }

  # Calculate cumulative ranges
  intra_diff <- diff(ranges)[1:(length(ranges) - 1) %% 2 != 0] + 1
  intra_cumsum <- intra_diff[1:length(intra_diff)-1] + intra_diff[2:length(intra_diff)]

  # Inter-range differences
  inter_diff <- diff(ranges)[1:(length(ranges) - 1) %% 2 == 0] - 1

  # Proportion inter-range difference and cumulative range sums
  interrupt_ratio <- inter_diff / intra_cumsum

  # Combine ranges with inter-range difference <= max_interrupt &
  # interrupt ratio <= prop_interrupt
  exclude_ranges <- c(
    which(inter_diff <= max_interrupt & interrupt_ratio <= prop_interrupt) * 2,
    which(inter_diff <= max_interrupt & interrupt_ratio <= prop_interrupt) * 2 + 1
  )
  if (length(exclude_ranges) > 0) {
    ranges <- ranges[-exclude_ranges]
  }

  # Make intervals
  if (length(ranges) > 0) {
    intervals <-
      matrix(ranges, ncol = 2, byrow = TRUE) %>%
      as.data.frame() %>%
      magrittr::set_names(c("cluster_start", "cluster_end")) %>%
      dplyr::mutate(cluster_idx = 1:length(cluster_start)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(idx = list(seq(cluster_start, cluster_end))) %>%
      tidyr::unnest(cols = c(idx)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(is_cluster = TRUE) %>%
      dplyr::relocate(idx, is_cluster, cluster_idx, cluster_start, cluster_end) %>%
      dplyr::rename_with(~ gsub("cluster", cluster_name, .x))
  } else {
    intervals <-
      tibble::tibble(
        idx = 1, is_cluster = NA, cluster_idx = NA,
        cluster_start = NA, cluster_end = NA
      ) %>%
      dplyr::rename_with(~ gsub("cluster", cluster_name, .x))
  }

  return(intervals)
}


# Kmeans with silhouette score

kmeans_sil = function(data, k, n.start, iter.max, n.samples, samplesize){
  kmeans = data %>%
    stats::kmeans(centers = k, nstart = n.start, iter.max = iter.max, algorithm = "MacQueen")

  data.clustered = data %>% tibble::add_column(cluster_id = kmeans$cluster)

  n.clust = data.clustered %>% dplyr::count(cluster_id)
  if(any(n.clust$n < 2)){
    stop("At least one cluster consists of less than 2 observations.
           Data cannot be clustered further")
  }

  # Calculate silhouette scores
  subsample <- function(x) {
    sample <- data.clustered %>%
      dplyr::group_by(cluster_id) %>%
      dplyr::slice_sample(n = floor(samplesize / k)) %>%
      dplyr::ungroup()
    x <- sample$cluster_id
    d <- sample %>% dplyr::select(!cluster_id) %>% dist()
    summary(cluster::silhouette(x, d))$clus.avg.widths
  }
  sil.scores <- sapply(1:n.samples, subsample) %>% apply(1, mean) %>% as.numeric()

  list(clustering = kmeans$cluster, clus.avg.widths = sil.scores)
}
# Distance function helper
dist.euclidian = function(S, R){
  sqrt(outer(rowSums(R^2), rowSums(S^2), "+") - 2 * tcrossprod(R, S))
}

# Calculates CLA
CLA = function(irr_interp, v_lambda, cie_s26e, reso.num){
  arod1 = 2.3
  arod2 = 1.6
  a_bminusY = 0.21
  g1 = 1.00
  g2 = 0.16
  k =  0.2616
  rodSat =  6.5
  f = 1548

  vl_response <-as.numeric((irr_interp %*% as.numeric(v_lambda$CIE1924_v2)) * reso.num)
  scone_response <- as.numeric((irr_interp %*% as.numeric(cie_s26e$scone)) * reso.num)
  rod_response <- as.numeric((irr_interp %*% as.numeric(cie_s26e$rod)) * reso.num)
  mel_response <- as.numeric((irr_interp %*% as.numeric(cie_s26e$mel)) * reso.num)

  rod_mel = (rod_response/(vl_response + g1*scone_response))
  rod_BminusY = (rod_response/(vl_response + g2*scone_response))

  # Main calculation for CLA
  BminusY = scone_response-k*vl_response

  CS = double(length(BminusY))

  # BminusY >= 0 (cool light)
  CS1 = mel_response[BminusY >= 0]
  CS1[CS1 < 0] = 0

  CS2 = a_bminusY*(BminusY[BminusY >= 0])
  CS2[CS2 < 0] = 0

  Rod = arod2*(rod_BminusY[BminusY >= 0]*((1-exp(-rod_response[BminusY >= 0]/rodSat))))
  Rodmel = arod1*(rod_mel[BminusY >= 0]*((1-exp(-rod_response[BminusY >= 0]/rodSat))))

  CS[BminusY >= 0] = CS1 + CS2 - Rod - Rodmel

  # BminusY < 0 (warm light)
  CS1 = mel_response[BminusY < 0]
  Rodmel = arod1*(rod_mel[BminusY < 0]*((1-exp(-rod_response[BminusY < 0]/rodSat))))

  CS[BminusY < 0] = CS1 - Rodmel
  CS[CS < 0] = 0

  CLA = CS*f
  return(CLA)
}


