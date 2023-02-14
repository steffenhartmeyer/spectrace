#' Calculate (alpha-opic) quantities from calibrated spectrace data
#'
#' This function calculates photopic and alpha-opic quantities as defined in the
#' CIE s26e standard, from the calibrated spectrace data. Spectral irradiance is
#' interpolated to 5nm resolution. CCT is calculated
#' with McCamy's approximation.
#'
#' @param lightData Data frame containing the calibrated light data.x = data.
#' @param quantities Quantities to be calculated. Can be any or multiple of:
#'    ("ALL", "sc", "mc", "lc", "mel", "rod", "scEDI", "mcEDI", "lcEDI",
#'    "melEDI", "rodEDI", "scELR", "mcELR", "lcELR", "melELR", "rodELR",
#'    "scDER", "mcDER", "lcDER", "melDER", "rodDER", "ill", "CCT"). If "ALL"
#'    (the default), all quantities will be calculated and added to the data frame.
#' @param interp_method Method for interpolation. Can be "pchip" (smooth
#'    piecewise hermetic interpolation) or "linear". Defaults to "pchip".
#' @param keep_spectral_data Logical. Should the spectral irradiance columns be kept?
#'    Defaults to TRUE.
#'
#' @return Data frame extended with specified quantities.
#'    If \code{keep_spectral_data} is FALSE then the spectral data columns will be
#'    removed from the original data frame.
#' @export
#'
#' @examples
spectrace_calculate_quantities <- function(lightData,
                                           quantities =
                                             c(
                                               "ALL",
                                               "sc", "mc", "lc", "mel", "rod",
                                               "scEDI", "mcEDI", "lcEDI", "melEDI", "rodEDI",
                                               "scELR", "mcELR", "lcELR", "melELR", "rodELR",
                                               "scDER", "mcDER", "lcDER", "melDER", "rodDER",
                                               "ill", "CCT"
                                             ),
                                           resolution = c("5nm", "1nm"),
                                           interp_method = c("pchip", "linear", "none"),
                                           keep_spectral_data = TRUE) {
  # Match arguments
  resolution <- match.arg(resolution)
  interp_method <- match.arg(interp_method)
  quants <- match.arg(quantities, several.ok = TRUE)

  # Irradiance data
  irr_data <- lightData %>%
    dplyr::select(dplyr::matches("\\d{3}nm"))

  # Input wavelengths
  wl.in <- sub("nm", "", names(irr_data)) %>%
    as.numeric()

  # Get desired resolution
  reso.num <- as.numeric(substr(resolution, 1, 1))
  wl.1nm <- seq(380, 780)
  wl.out <- seq(380, 780, reso.num)

  print(wl.in)
  print(wl.out)

  if (wl.out == wl.in) {
    if (interp_method != "none") {
      warning("Data seems already interpolated. Proceeding without interpolation.")
    }
    irr_interp <- irr_data %>%
      as.matrix()
  } else {
    if (wl.in == wl.1nm) {
      warning("Resolution lower than that of data. Proceeding with original resolution")
      irr_interp <- irr_data %>%
        as.matrix()
      reso.num <- 1
    } else {
      if (interp_method == "none" && !all(wl.out %in% wl.in)) {
        stop("Interpolation method is 'none', but data seems not to be interpolated.")
      }
      irr_interp <- irr_data %>%
        spectrace_interpolate_spectra(
          resolution = resolution,
          interp_method = interp_method
        ) %>%
        as.matrix()
    }
  }

  # Check for negative values
  negatives <- irr_interp < 0
  if (any(negatives)) {
    warning("Data containes negative values. Replaced negative values by zero.")
    irr_interp[negatives] <- 0
  }

  # Choose matching response functions
  if (reso.num == 5) {
    cmf <- cmf.5nm
    cie_s26e <- cie_s26e.5nm
  } else {
    cmf <- cmf.1mm
    cie_s26e <- cie_s26e.1nm
  }

  # Calculate photopic illuminance
  K_m <- 683
  ill <- as.numeric((irr_interp %*% as.numeric(cmf$y)) * K_m * reso.num)

  # Calculate alpha-opic irradiance and ELR using CIE s26e opsin templates
  scone <- as.numeric((irr_interp %*% as.numeric(cie_s26e$scone)) * reso.num)
  mcone <- as.numeric((irr_interp %*% as.numeric(cie_s26e$mcone)) * reso.num)
  lcone <- as.numeric((irr_interp %*% as.numeric(cie_s26e$lcone)) * reso.num)
  rod <- as.numeric((irr_interp %*% as.numeric(cie_s26e$rod)) * reso.num)
  mel <- as.numeric((irr_interp %*% as.numeric(cie_s26e$mel)) * reso.num)
  aopic <- cbind(scone, mcone, lcone, mel, rod)
  elr <- aopic / ill

  # Calculate alpha-opic EDI and DER
  KavD65 <- c(0.8173, 1.4558, 1.6289, 1.4497, 1.3262) / 1000
  aopic_edi <- aopic / (KavD65)[col(aopic)]
  der <- elr / (KavD65)[col(elr)]

  # Calculate CIE XYZ using CIE color matching functions
  x <- (irr_interp %*% as.numeric(cmf$x)) * reso.num
  y <- (irr_interp %*% as.numeric(cmf$y)) * reso.num
  z <- (irr_interp %*% as.numeric(cmf$z)) * reso.num
  cie_x <- x / (x + y + z)
  cie_y <- y / (x + y + z)

  # Calculate CCT using McCamy's approximation
  n <- (cie_x - 0.3320) / (cie_y - 0.1858)
  CCT <- -449 * n^3 + 3525 * n^2 - 6823.3 * n + 5520.33

  # Combine into data frame
  cData <- data.frame(cbind(ill, aopic, aopic_edi, elr, der, CCT))
  names(cData) <- c(
    "ill", "sc", "mc", "lc", "mel", "rod",
    "scEDI", "mcEDI", "lcEDI", "melEDI", "rodEDI",
    "scELR", "mcELR", "lcELR", "melELR", "rodELR",
    "scDER", "mcDER", "lcDER", "melDER", "rodDER",
    "CCT"
  )

  # Select quantities
  if (!("ALL" %in% quants)) {
    cData <- cData %>%
      dplyr::select(quants)
  }

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
