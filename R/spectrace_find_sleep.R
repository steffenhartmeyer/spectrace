#' Title
#'
#' @param lightData
#' @param actigraphyVar
#' @param flag_only
#' @param min_length
#' @param max_interrupt
#' @param smooth_window
#' @param light_threshold
#'
#' @return
#' @export
#'
#' @examples
spectrace_find_sleep<- function(lightData,
                                actigraphyVar,
                                flag_only = FALSE,
                                min_length = "2 hours",
                                max_interrupt = "60 mins",
                                smooth_window = "10 mins",
                                light_threshold = 10) {
  groups <- lightData %>% dplyr::group_vars()
  lightData <- lightData %>%
    dplyr::ungroup() %>%
    dplyr::nest_by(dplyr::pick(groups)) %>%
    dplyr::mutate(
      data = list(
        find_sleep(
          data,
          {{actigraphyVar}},
          flag_only,
          min_length,
          max_interrupt,
          smooth_window,
          light_threshold
        )
      )
    ) %>%
    tidyr::unnest(cols = data) %>%
    dplyr::ungroup()

  return(lightData)
}

find_sleep <- function(data,
                       actigraphyVar,
                       flag_only,
                       min_length,
                       max_interrupt,
                       smooth_window,
                       light_threshold){
  # Detect epoch
  epoch <- abs(diff(as.numeric(data$datetime)))
  if (length(unique(epoch)) > 1) {
    warning("Data not regularly spaced. Selecting shortest epoch.")
    epoch <- sort(epoch)
  }
  epoch <- epoch[1]

  # Parse time units
  min_length <- parse_timeunit_tosecs(min_length)$secs
  max_interrupt <- parse_timeunit_tosecs(max_interrupt)$secs
  smooth_window <- parse_timeunit_tosecs(smooth_window)$secs

  # Check whether parameters are longer than epoch
  if (any(c(
    min_length, max_interrupt, smooth_window * 2) < epoch)) {
    stop("Time parameters must be equal to or longer than the epoch.")
  }

  # Convert to sample counts
  min_length <- round(min_length / epoch)
  max_interrupt <- round(max_interrupt / epoch)
  smooth_window <- round(smooth_window / epoch)

  # Add index column to data
  data <- data %>% dplyr::mutate(idx = 1:nrow(.))

  # Find low actigraphy
  actigraphy_smooth <-
    slider::slide_vec(
      log10(pull(data, {{actigraphyVar}}) + 1),
      median,
      .before = floor(smooth_window / 2),
      .after = ceiling(smooth_window / 2),
      complete = FALSE
    )
  actigraphy_median <- max(actigraphy_smooth, na.rm = TRUE)
  actigraphy_norm <- (actigraphy_smooth / actigraphy_median) %>% ifelse(is.infinite(.), 0, .)

  # Low Spectrace activity
  act_smooth <-
    slider::slide_vec(
      log10(data$activity + 1),
      median,
      .before = floor(smooth_window / 2),
      .after = ceiling(smooth_window / 2),
      complete = FALSE
    )
  act_max <- max(act_smooth, na.rm = TRUE)
  act_norm <- (act_smooth / act_max) %>% ifelse(is.infinite(.), 0, .)

  sleep_candidate <-
    act_norm < 0.1 &
    actigraphy_norm < 0.5 &
    data$lux < light_threshold

  sleep <- find_clusters(sleep_candidate, min_length, max_interrupt, "sleep")

  if (flag_only) {
    sleep <- sleep %>% dplyr::select(idx, is_sleep)
  }

  data <- data %>%
    dplyr::left_join(sleep, by = "idx") %>%
    dplyr::mutate(is_sleep = tidyr::replace_na(is_sleep, FALSE))

  data <- data %>% dplyr::select(!idx)
  return(data)
}
