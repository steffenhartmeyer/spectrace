#' Import raw spectrace light data
#'
#' This function imports the raw light data of the spectrace sensors. It assumes
#' that the data is stored in CSV or TSV format.
#'
#' @param lightFile Path to the file containing the light data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#' @param serial_number Serial number of spectrace device. Defaults to NULL.
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_light <- function(lightFile, tz, serial_number = NA) {
  # Get file type (CSV or TSV)
  if (endsWith(lightFile, ".csv")) {
    sep <- ","
  } else if (endsWith(lightFile, ".tsv")) {
    sep <- "\t"
  } else {
    stop("Unsupported file format! Must be CSV or TSV.")
  }

  # Get header
  header <- readr::read_delim(
    lightFile,
    col_names = FALSE,
    col_types = readr::cols(.default = "c"),
    n_max = 3,
    delim = sep,
    skip_empty_rows = FALSE
  )

  # Version 2 file
  if (header$X1[1] == "SERIAL") {
    serial_number <- header$X2[1]
    lightData <- utils::read.delim(
      lightFile,
      skip = 5,
      header = FALSE,
      col.names = paste0("X", 1:24),
      sep = sep
    )
  }
  # Version 3 file
  else if (header$X1[1] == "Raw Spectrace Data") {
    serial_number <- header$X2[3]
    lightData <- utils::read.delim(
      lightFile,
      skip = 6,
      header = FALSE,
      col.names = paste0("X", 1:24),
      sep = sep
    )
  } else {
    # Check whether serial number available
    if (is.na(serial_number)) {
      warning("No serial number specified!")
    }
    lightData <- utils::read.delim(
      lightFile,
      header = FALSE,
      col.names = paste0("X", 1:24),
      sep = sep
    )
  }

  # Add index
  lightData <- lightData %>%
    dplyr::mutate(idx = seq(1, nrow(.))) %>%
    dplyr::relocate(idx)

  # Data column names
  col_names <- c(
    "idx", "unix", "lux", "ch0", "ch1", "uv", "410nm", "435nm", "460nm", "485nm",
    "510nm", "535nm", "560nm", "585nm", "610nm", "645nm", "680nm", "705nm", "730nm",
    "760nm", "810nm", "860nm", "900nm", "940nm"
  )

  # Fix issue when decimal separator is a comma
  # Incorrect data are when lux is not an integer
  # (ch0 holds now the fraction of lux, ch1 holds ch2 etc. -> 24 columns)
  lightData.incorrect <- lightData %>%
    dplyr::filter(!is.na(X24)) %>%
    dplyr::mutate(X2 = as.numeric(paste(X2, X3, sep = "."))) %>%
    dplyr::select(!X3)
  names(lightData.incorrect) <- col_names

  # Correct data are when lux is an integer
  lightData.correct <- lightData %>%
    dplyr::filter(is.na(X24)) %>%
    dplyr::select(!X24)
  names(lightData.correct) <- col_names

  # Combine data
  lightData <- dplyr::bind_rows(lightData.incorrect, lightData.correct) %>%
    dplyr::arrange(idx) %>%
    dplyr::select(!idx)

  # Add datetime and serial number
  lightData <-
    lightData %>%
    dplyr::mutate(
      datetime = lubridate::as_datetime(unix, tz = tz),
      serial = serial_number
    ) %>%
    dplyr::relocate(serial, datetime)

  # Return
  return(lightData)
}
