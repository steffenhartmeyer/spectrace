#' Import raw spectrace data and calculate alpha-opic quantities.
#'
#' This function imports raw spectrace output and calculates alpha-opic quantities.
#' See \code{\link{spectrace_import}} and \code{\link{spectrace_aopic}}
#' for more information.
#'
#' @param include_raw Logical. Should the raw irradiance output be included?
#' Defaults to FALSE.
#' @inheritParams spectrace_import
#' @inheritParams spectrace_aopic
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_aopic <- function(lightFile,
                                   actFile = NULL,
                                   offset = 0,
                                   tz,
                                   cal = NULL,
                                   include_raw = FALSE) {
  df_raw <- spectrace_import(lightFile, actFile, offset, tz)
  df_proc <- spectrace_aopic(df_raw[, 7:20], cal)
  df <- cbind(df_raw, df_proc)
  if (!include_raw) df <- df %>% dplyr::select(datetime, lux, uv, ill:CCT)
  return(df)
}
