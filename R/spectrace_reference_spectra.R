#' Return Reference Spectra
#'
#' @param as_spectrace Logical. Should the spectra be returned as spectrace spectra?
#'    Defualts to TRUE.
#' @param resolution String specifying the resolution of the output
#'    spectrum. Can be "5nm" (default), "1nm", or "spectrace". The latter will result
#'    in the original 14 spectrace channels.
#'
#' @return Data frame with reference spectra
#' @export
#'
#' @examples
spectrace_reference_spectra = function(as_spectrace = TRUE,
                                       resolution = "5nm"
                                       ){
  reference_spectra_1nm %>%
    spectra_to_spectrace() %>%
    spectrace_interpolate_spectra(resolution = resolution)
}

#' Return CIE Illuminants
#'
#' @param as_spectrace Logical. Should the spectra be returned as spectrace spectra?
#'    Defualts to TRUE.
#' @param resolution String specifying the resolution of the output
#'    spectrum. Can be "5nm" (default), "1nm", or "spectrace". The latter will result
#'    in the original 14 spectrace channels.
#'
#' @return Data frame with CIE illuminants
#' @export
#'
#' @examples
spectrace_cie_illuminants = function(as_spectrace = TRUE,
                                     resolution = "5nm"
                                     ){
  cie_illuminants_1nm %>%
    spectra_to_spectrace() %>%
    spectrace_interpolate_spectra()
}
