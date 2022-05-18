
#' Calculate alpha-opic quantities from raw spectrace data
#'
#' This function calculates photopic and alpha-opic quantities as defined in the
#' CIE s26e standard, from the raw spectrace data. Before calculations, the raw
#' spectrace data is converted to spectral irradiance using provided or default
#' calibration data. Spectral irradiance is then interpolated to 5nm resolution
#' using piecewise cubic hermitean interpolation polynomials. CCT is calculated
#' with McCamy's approximation.
#'
#' @param lightData Data frame containing the calibrated light data
#' for the channels from 410nm to 730nm.
#'
#' @return Data frame with illuminance, alpha-opic irradiances, alpha-opic EDI,
#' alpha-opic ELR, alpha-opic DER, and CCT.
#' @export
#'
#' @examples
spectrace_aopic <- function(lightData) {

  # Irradiance data
  irr_data <- lightData %>%
    dplyr::select("410nm":"730nm") %>%
    as.matrix()

  # Interpolate to 1nm data using PCHIP
  interp_fun <- function(y) {
    wl_out <- seq(380, 780, 5)
    wl_spectrace <- c(
      410, 435, 460, 485, 510, 535, 560,
      585, 610, 645, 680, 705, 730
    )
    y <- c(0, y, 0)
    wl <- c(wl_out[1], wl_spectrace, wl_out[length(wl_out)])
    if (!any(is.na(y))) {
      pracma::pchip(wl, y, wl_out)
    } else {
      rep(NA, length(wl_out))
    }
  }
  irr_interp <- t(apply(irr_data, 1, interp_fun))
  irr_interp[irr_interp < 0] <- 0

  # Calculate photopic illuminance
  ill <- apply(irr_interp, 1, function(x) 683 * sum(x * as.numeric(cmf$y) * 5))

  # Calculate alpha-opic irradiance and ELR using CIE s26e opsin templates
  scone <- apply(irr_interp, 1, function(x) sum(x * as.numeric(cie_s26e$scone) * 5))
  mcone <- apply(irr_interp, 1, function(x) sum(x * as.numeric(cie_s26e$mcone) * 5))
  lcone <- apply(irr_interp, 1, function(x) sum(x * as.numeric(cie_s26e$lcone) * 5))
  mel <- apply(irr_interp, 1, function(x) sum(x * as.numeric(cie_s26e$mel) * 5))
  rod <- apply(irr_interp, 1, function(x) sum(x * as.numeric(cie_s26e$rod) * 5))
  aopic <- cbind(scone, mcone, lcone, mel, rod)
  elr <- aopic / ill

  # Calculate alpha-opic EDI and DER
  KavD65 <- c(0.8173, 1.4558, 1.6289, 1.4497, 1.3262) / 1000
  aopic_edi <- aopic / (KavD65)[col(aopic)]
  der <- elr / (KavD65)[col(elr)]

  # Calculate CIE XYZ using CIE color matching functions
  x <- apply(irr_interp, 1, function(x) sum(x * as.numeric(cmf$x) * 5))
  y <- apply(irr_interp, 1, function(x) sum(x * as.numeric(cmf$y) * 5))
  z <- apply(irr_interp, 1, function(x) sum(x * as.numeric(cmf$z) * 5))
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

  # Return data frame
  lightData.out <- lightData %>%
    dplyr::select(serial:uv) %>%
    tibble::add_column(cData)

  # Return
  return(lightData.out)
}
