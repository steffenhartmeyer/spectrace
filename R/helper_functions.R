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

find_cluster_timings = function(x, datetime){
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
                  cluster_end = datetime[cluster_end]+60) %>%
    dplyr::select(cluster_idx, cluster_start, cluster_end)

  return(intervals)
}

# flag_clusters3 = function(x, min_length, max_interrupt = min_length*0.25, max_prop=0.25){
#   x = replace_na(x, FALSE)
#   targets = c(0, x, 0)
#   absdiff = abs(diff(targets))
#   ranges = which(absdiff == 1)
#
#   # Remove ranges < min_length
#   intra_diff = diff(ranges)[1:(length(ranges)-1) %% 2 != 0]
#   exclude_ranges = c(which(intra_diff < min_length) * 2,
#                      which(intra_diff < min_length) * 2 - 1)
#   if(length(exclude_ranges) > 0 )
#     ranges = ranges[-exclude_ranges]
#
#   # Combine ranges with ratio of inter-range difference to total length <= max_prop
#   diff = diff(ranges)
#   inter_diff = diff[1:(length(ranges)-1) %% 2 == 0]
#   total = zoo::rollapply(diff, 3, sum, by=2)
#   exclude_ranges = c(which(inter_diff/total <= max_prop &
#                              inter_diff <= max_interrupt) * 2,
#                      which(inter_diff/total <= max_prop &
#                              inter_diff <= max_interrupt) * 2 + 1)
#   if(length(exclude_ranges) > 0 )
#     ranges = ranges[-exclude_ranges]
#
#   # Make intervals
#   ranges = matrix(ranges, ncol = 2, byrow=TRUE)
#   intervals = ranges %>% apply(MARGIN = 1, function(z) list(z[1]:x[2])) %>% unlist()
#   indices = seq(1:length(x))
#   return(indices %in% intervals)
# }

find_clusters2 <- function(x,
                           min_length,
                           max_interrupt = 0,
                           prop_interrupt = 0.25,
                           cluster_name = "cluster") {
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
