#' Find invalid Specrtrace data
#'
#' This function detects periods of invalid Spectrace data, that is, when the sensor
#' was not worn (nonwear) or covered. Invalid periods are detected by finding
#' clusters of consecutive values of low activity (nonwear) or illuminance (covered).
#'
#' If the data frame is grouped (see \link[dplyr]{group_by}), invalid periods are
#' detected per group. It is highly recommended to group the data before passing
#' it to this function, otherwise the data is considered as one continuous timeseries.
#'
#' @param lightData Data frame containing the calibrated light data. The data needs
#'    to contain the activity column. If the data frame is grouped
#'    (see \link[dplyr]{group_by}), invalid periods are detected per group. It is
#'    highly recommended to group the data before passing it to this function,
#'    otherwise the data is considered as one continuous timeseries.
#' @param flag_only Logical. Should only the logical flag for whether the data
#'    is invalid be returned as a column? If FALSE (default) the cluster index,
#'    and start and end index for each invalid cluster are returned as a column.
#' @param nonwear_min_length String. Minimum length for consecutive low activity
#'    values to be considered as invalid. Must be in the format "[numeric] [unit]",
#'    with possible units ("seconds","minutes","hours","days"). Units can be
#'    abbreviated.
#' @param nonwear_max_interrupt String. Maximum length of consecutive high activity
#'    values interrupting consecutive low activity values. Invalid periods can
#'    then contain interruptions of up to the specified length. Must be specified
#'    in the format "[numeric] [unit]", with possible units ("seconds","minutes",
#'    "hours","days"). Units can be abbreviated.
#' @param nonwear_smooth_window String. Size of smoothing window for detecting low
#'    activity periods. Must be specified in the format "[numeric] [unit]", with
#'    possible units ("seconds","minutes","hours","days"). Units can be abbreviated.
#' @param covered_min_length String. Minimum length for consecutive low illuminance
#'    values to be considered as invalid. Must be in the format "[numeric] [unit]",
#'    with possible units ("seconds","minutes","hours","days"). Units can be
#'    abbreviated.
#' @param covered_max_interrupt String. Maximum length of consecutive high illuminance
#'    values interrupting consecutive low illuminance values. Invalid periods can
#'    then contain interruptions of up to the specified length. Must be specified
#'    in the format "[numeric] [unit]", with possible units ("seconds","minutes",
#'    "hours","days"). Units can be abbreviated.
#' @param covered_threshold Numeric. Threshold illuminance to consider sensor as
#'    covered. Defaults to 1 lux.
#'
#' @return Original data frame with additional columns indicating whether a sample
#'    is covered or nonwear and if `flag_only = FALSE` indicating the cluster indices,
#'    and indices of start and end of each invalid cluster.
#' @export
#'
#' @examples
spectrace_find_invalid <- function(lightData,
                                   flag_only = FALSE,
                                   nonwear_min_length = "10 mins",
                                   nonwear_max_interrupt = "2 mins",
                                   nonwear_smooth_window = "10 mins",
                                   covered_min_length = "10 mins",
                                   covered_max_interrupt = "2 mins",
                                   covered_threshold = 1) {
  groups <- lightData %>% dplyr::group_vars()
  lightData <- lightData %>%
    dplyr::ungroup() %>%
    dplyr::nest_by(dplyr::pick(groups)) %>%
    dplyr::mutate(
      data = list(
        find_invalid(
          data,
          flag_only,
          nonwear_min_length,
          nonwear_max_interrupt,
          nonwear_smooth_window,
          covered_min_length,
          covered_max_interrupt,
          covered_threshold
        )
      )
    ) %>%
    tidyr::unnest(cols = data) %>%
    dplyr::ungroup()

  return(lightData)
}

find_invalid <- function(data,
                         flag_only = FALSE,
                         nonwear_min_length = "10 mins",
                         nonwear_max_interrupt = "2 mins",
                         nonwear_smooth_window = "10 mins",
                         covered_min_length = "10 mins",
                         covered_max_interrupt = "2 mins",
                         covered_threshold = 1) {
  # Detect epoch
  epoch <- diff(as.numeric(data$datetime))
  if (length(unique(epoch)) > 1) {
    warning("Data not regularly spaced. Selecting shortest epoch.")
    epoch <- sort(epoch)
  }
  epoch <- epoch[1]

  # Parse time units
  nonwear_min_length <- parse_timeunit_tosecs(nonwear_min_length)$secs
  nonwear_max_interrupt <- parse_timeunit_tosecs(nonwear_max_interrupt)$secs
  nonwear_smooth_window <- parse_timeunit_tosecs(nonwear_smooth_window)$secs
  covered_min_length <- parse_timeunit_tosecs(covered_min_length)$secs
  covered_max_interrupt <- parse_timeunit_tosecs(covered_max_interrupt)$secs

  # Check whether parameters are longer than epoch
  if (any(c(
    nonwear_min_length, nonwear_max_interrupt, nonwear_smooth_window * 2,
    covered_min_length, covered_max_interrupt
  ) < epoch)) {
    stop("Time parameters must be equal to or longer than the epoch.")
  }

  # Convert to sample counts
  nonwear_min_length <- round(nonwear_min_length / epoch)
  nonwear_max_interrupt <- round(nonwear_max_interrupt / epoch)
  nonwear_smooth_window <- round(nonwear_smooth_window / epoch)
  covered_min_length <- round(covered_min_length / epoch)
  covered_max_interrupt <- round(covered_max_interrupt / epoch)

  # Add index column to data
  data <- data %>% dplyr::mutate(idx = 1:nrow(.))

  # Find non-wear periods
  act_smooth <-
    slider::slide_vec(
      log10(data$activity + 1),
      median,
      .before = floor(nonwear_smooth_window / 2),
      .after = ceiling(nonwear_smooth_window / 2),
      complete = FALSE
    )
  act_norm <- ifelse(max(act_smooth, na.rm = TRUE) > 0,
                     act_smooth / max(act_smooth, na.rm = TRUE), 0)
  low_act <- act_norm < 0.1
  nonwear <- find_clusters(
    low_act, nonwear_min_length,
    nonwear_max_interrupt, "nonwear"
  )
  if (flag_only) {
    nonwear <- nonwear %>% dplyr::select(idx, is_nonwear)
  }

  data <- data %>%
    dplyr::left_join(nonwear, by = "idx") %>%
    dplyr::mutate(is_nonwear = tidyr::replace_na(is_nonwear, FALSE))

  # Find periods where sensor was covered
  low_light <- data$lux < covered_threshold & !data$is_nonwear
  covered <- find_clusters(
    low_light, covered_min_length,
    covered_max_interrupt, "covered"
  )
  if (flag_only) {
    covered <- covered %>% dplyr::select(idx, is_covered)
  }
  data <- data %>%
    dplyr::left_join(covered, by = "idx") %>%
    dplyr::mutate(is_covered = tidyr::replace_na(is_covered, FALSE))

  data <- data %>% dplyr::select(!idx)
  return(data)
}
