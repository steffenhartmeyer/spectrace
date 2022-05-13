
#' Import raw spectrace light data
#'
#' This function imports the raw light data of the spectrace sensors. It assumes
#' that the data is stored in CSV format.
#'
#' @param lightFile Path to the file containing the light data.
#' @param tz Time zone to be used for datetime conversion. See supported time zones
#'    by calling \code{\link[base]{OlsonNames}}.
#'
#' @return Data frame.
#' @export
#'
#' @examples
spectrace_import_light <- function(lightFile, tz) {

  # Check type of file (including header or not)
  header = readr::read_csv(lightFile,
                           col_names = c("descriptor", "value"),
                           col_types = readr::cols(.default = "c"),
                           n_max = 3)
  if(startsWith(header$descriptor[1], "SERIAL")){
    #Header information
    lightData <-
      readr::read_csv(lightFile,
                      skip = 4,
                      col_types = readr::cols(.default = "d")
                      )
  }
  else{
    # No header information
    col_names <- c(
      "unix", "lux", "ch0", "ch1", "uv", "410nm", "435nm", "460nm", "485nm",
      "510nm", "535nm", "560nm", "585nm", "610nm", "645nm", "680nm", "705nm", "730nm",
      "760nm", "810nm", "860nm", "900nm", "940nm"
    )
    lightData <-
      readr::read_csv(lightFile,
                      col_names = col_names,
                      col_types = readr::cols(.default = "d")
      )
  }


   %>%
    dplyr::mutate(datetime = lubridate::as_datetime(unix, tz = tz)) %>%
    dplyr::relocate(datetime)

  # Return
  return(lightData)
}
