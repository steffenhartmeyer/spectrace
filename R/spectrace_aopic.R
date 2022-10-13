
#' Calculate alpha-opic quantities from raw spectrace data
#'
#' This function calculates photopic and alpha-opic quantities as defined in the
#' CIE s26e standard, from the calibrated spectrace data. Spectral irradiance is
#' interpolated to 5nm resolution. CCT is calculated
#' with McCamy's approximation.
#'
#' @param lightData Data frame containing the calibrated light data.x = data.
#' @param interp_method Method for interpolation. Can be "pchip" (smooth
#'    piecewise hermetic interpolation) or "linear". Defaults to "pchip".
#' @param keep_spectral_data Logical. Should the spectral irradiance columns be kept?
#'    Defaults to TRUE.
#'
#' @return Data frame extended with values for illuminance, alpha-opic irradiances,
#'    alpha-opic EDI, alpha-opic ELR, alpha-opic DER, and CCT.
#'    If \code{keep_spectral_data} is FALSE then the spectral data columns will be
#'    removed from the original data frame.
#' @export
#'
#' @examples
spectrace_aopic <- function(lightData,
                            interp_method = c("pchip", "linear", "none"),
                            keep_spectral_data = TRUE) {
  # Match arguments
  interp_method <- match.arg(interp_method)

  wls <- paste0(seq(380, 780, 5), "nm")
  if (all(wls %in% names(lightData))) {
    if (interp_method != "none") {
      warning("Interpolation method is not 'none', but data seems to already be interpolated.")
    }
    irr_interp <- lightData %>%
      dplyr::select(wls) %>%
      as.matrix()
  } else {
    if (interp_method == "none") {
      stop("Interpolation method is 'none', but data seems not to be interpolated.")
    }
    irr_interp <- lightData %>%
      spectrace_interpolate_spectra(
        resolution = "5nm",
        interp_method = interp_method
      ) %>%
      dplyr::select("380nm":"780nm") %>%
      as.matrix()
  }

  # Calculate photopic illuminance
  ill <- as.numeric((irr_interp %*% as.numeric(cmf$y)) * 683 * 5)

  # Calculate alpha-opic irradiance and ELR using CIE s26e opsin templates
  scone <- as.numeric((irr_interp %*% as.numeric(cie_s26e$scone)) * 5)
  mcone <- as.numeric((irr_interp %*% as.numeric(cie_s26e$mcone)) * 5)
  lcone <- as.numeric((irr_interp %*% as.numeric(cie_s26e$lcone)) * 5)
  rod <- as.numeric((irr_interp %*% as.numeric(cie_s26e$rod)) * 5)
  mel <- as.numeric((irr_interp %*% as.numeric(cie_s26e$mel)) * 5)
  aopic <- cbind(scone, mcone, lcone, mel, rod)
  elr <- aopic / ill

  # Calculate alpha-opic EDI and DER
  KavD65 <- c(0.8173, 1.4558, 1.6289, 1.4497, 1.3262) / 1000
  aopic_edi <- aopic / (KavD65)[col(aopic)]
  der <- elr / (KavD65)[col(elr)]

  # Calculate CIE XYZ using CIE color matching functions
  x <- (irr_interp %*% as.numeric(cmf$x)) * 5
  y <- (irr_interp %*% as.numeric(cmf$y)) * 5
  z <- (irr_interp %*% as.numeric(cmf$z)) * 5
  cie_x <- x / (x + y + z)
  cie_y <- y / (x + y + z)

  # Calculate CCT using McCamy's approximation
  n <- (cie_x - 0.3320) / (0.1858 - cie_y)
  CCT <- 437 * n^3 + 3601 * n^2 + 6861 * n + 5517

  # Combine into data frame
  cData <- data.frame(cbind(ill, aopic, aopic_edi, elr, der, CCT))
  names(cData) <- c(
    "ill", "sc", "mc", "lc", "mel", "rod",
    "scEDI", "mcEDI", "lcEDI", "melEDI", "rodEDI",
    "scELR", "mcELR", "lcELR", "melELR", "rodELR",
    "scDER", "mcDER", "lcDER", "melDER", "rodDER",
    "CCT"
  )

  # Add to data
  lightData <- lightData %>%
    tibble::add_column(cData)

  # Return data frame
  if (keep_spectral_data) {
    return(lightData)
  } else {
    return(dplyr::select(lightData, !dplyr::matches("\\d{3}nm")))
  }
}
