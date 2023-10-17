#' Calculate (alpha-opic) quantities from calibrated spectrace data
#'
#' This function calculates selected optical quantities from the calibrated
#' spectrace data. Spectral irradiance is interpolated to desired resolution.
#' CCT is calculated with McCamy's approximation.
#'
#' @param lightData Data frame containing the calibrated light data.
#' @param quantities Quantities to be calculated. Can be any or multiple of:
#'    ("ALL", "sc", "mc", "lc", "mel", "rod", "scEDI", "mcEDI", "lcEDI",
#'    "melEDI", "rodEDI", "scELR", "mcELR", "lcELR", "melELR", "rodELR",
#'    "scDER", "mcDER", "lcDER", "melDER", "rodDER", "cie1924_v2_lux",
#'    "cie2008_v2_lux", "cie2008_v10_lux", "CCT", "cie1931_x", "cie1931_y",
#'    "cie1931_z", "cie1964_x", "cie1964_y"). If "ALL" (the default), all
#'    quantities will be calculated and added to the data frame.
#' @param resolution String specifying the resolution of the output
#'    spectrum. Can be "5nm" (default) or "1nm".
#' @param interp_method Method for interpolation. Can be "pchip" (smooth
#'    piecewise hermetic interpolation), "linear", or "none". Defaults to "pchip".
#' @param keep_spectral_data Logical. Should the spectral irradiance columns be
#'    kept? Defaults to TRUE.
#'
#' @return Data frame extended with specified quantities.
#'    If \code{keep_spectral_data} is FALSE then the spectral data columns will be
#'    removed from the original data frame.
#' @export
#'
#' @examples
spectrace_calculate_quantities <- function(
    lightData,
    quantities =
      c(
        "ALL",
        "sc", "mc", "lc", "mel", "rod",
        "scEDI", "mcEDI", "lcEDI", "melEDI", "rodEDI",
        "scELR", "mcELR", "lcELR", "melELR", "rodELR",
        "scDER", "mcDER", "lcDER", "melDER", "rodDER",
        "cie1924_v2_lux", "cie2008_v2_lux", "cie2008_v10_lux",
        "CCT", "cie1931_XYZ", "cie1931_x", "cie1931_y",
        "cie1964_x", "cie1964_y"
      ),
    resolution = c("5nm", "1nm"),
    interp_method = c("pchip", "linear", "none"),
    keep_spectral_data = TRUE) {
  # Ungroup data
  if (dplyr::is_grouped_df(lightData)) {
    warning("Data frame is grouped and will be ungrouped.")
    lightData <- lightData %>% dplyr::ungroup()
  }

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
  wl.1nm <- seq(380, 780, 1)
  wl.out <- seq(380, 780, reso.num)

  if (setequal(wl.out, wl.in)) {
    if (interp_method != "none") {
      warning("Data seems already interpolated. Proceeding without interpolation.")
    }
    irr_interp <- irr_data
  } else {
    if (setequal(wl.in, wl.1nm)) {
      warning("Resolution lower than that of data. Proceeding with original resolution")
      irr_interp <- irr_data
      reso.num <- 1
      wl.out <- wl.1nm
    } else {
      if (interp_method == "none" && !all(wl.out %in% wl.in)) {
        stop("Interpolation method is 'none', but data seems not to be interpolated.")
      }
      irr_interp <- irr_data %>%
        spectrace_interpolate_spectra(
          resolution = resolution,
          interp_method = interp_method
        )
    }
  }

  # Check for negative values
  negatives <- irr_interp < 0
  if (any(negatives)) {
    warning("Data containes negative values. Replaced negative values by zero.")
    irr_interp[negatives] <- 0
  }

  # Normalize data
  irr_interp_norm <- irr_interp %>%
    spectrace_normalize_spectra() %>%
    as.matrix()

  # As matrix
  irr_interp <- irr_interp %>%
    as.matrix()

  # Match response functions to resolution
  v_lambda <- v_lambda_1nm %>% dplyr::filter(wl %in% wl.out)
  cie_s26e <- cie_s26e_1nm %>% dplyr::filter(wl %in% wl.out)
  cie_xyz <- cie_xyz_1nm %>% dplyr::filter(wl %in% wl.out)

  # Calculate photopic illuminances
  K_m <- 683.0015478
  cie1924_v2_lux <-
    as.numeric((irr_interp %*% as.numeric(v_lambda$CIE1924_v2)) * K_m * reso.num)
  cie2008_v2_lux <-
    as.numeric((irr_interp %*% as.numeric(v_lambda$CIE2008_v2)) * K_m * reso.num)
  cie2008_v10_lux <-
    as.numeric((irr_interp %*% as.numeric(v_lambda$CIE2008_v10)) * K_m * reso.num)

  # Calculate alpha-opic irradiance and ELR using CIE s26e opsin templates
  scone <- as.numeric((irr_interp %*% as.numeric(cie_s26e$scone)) * reso.num)
  mcone <- as.numeric((irr_interp %*% as.numeric(cie_s26e$mcone)) * reso.num)
  lcone <- as.numeric((irr_interp %*% as.numeric(cie_s26e$lcone)) * reso.num)
  rod <- as.numeric((irr_interp %*% as.numeric(cie_s26e$rod)) * reso.num)
  mel <- as.numeric((irr_interp %*% as.numeric(cie_s26e$mel)) * reso.num)
  aopic <- cbind(scone, mcone, lcone, mel, rod)
  elr <- aopic / cie1924_v2_lux

  # Calculate alpha-opic EDI and DER
  Kav_D65 <- c(0.8173, 1.4558, 1.6289, 1.4497, 1.3262) / 1000
  aopic_edi <- aopic / (Kav_D65)[col(aopic)]
  der <- elr / (Kav_D65)[col(elr)]

  # Calculate CIE XYZ using CIE1931 color matching functions
  CIE1931_X <- (irr_interp_norm %*% as.numeric(cie_xyz$CIE1931_x)) * reso.num
  CIE1931_Y <- (irr_interp_norm %*% as.numeric(cie_xyz$CIE1931_y)) * reso.num
  CIE1931_Z <- (irr_interp_norm %*% as.numeric(cie_xyz$CIE1931_z)) * reso.num
  CIE1931_xyz <- CIE1931_X + CIE1931_Y + CIE1931_Z
  cie1931_x <- CIE1931_X / CIE1931_xyz
  cie1931_y <- CIE1931_Y / CIE1931_xyz
  cie1931_XYZ <- paste(CIE1931_X, CIE1931_Y, CIE1931_Z, sep = ",")

  # Calculate CIE XYZ using CIE1964 color matching functions
  CIE1964_X <- (irr_interp_norm %*% as.numeric(cie_xyz$CIE1964_x)) * reso.num
  CIE1964_Y <- (irr_interp_norm %*% as.numeric(cie_xyz$CIE1964_y)) * reso.num
  CIE1964_Z <- (irr_interp_norm %*% as.numeric(cie_xyz$CIE1964_z)) * reso.num
  CIE1964_xyz <- CIE1964_X + CIE1964_Y + CIE1964_Z
  cie1964_x <- CIE1964_X / CIE1964_xyz
  cie1964_y <- CIE1964_Y / CIE1964_xyz

  # Calculate CCT using McCamy's approximation
  n <- (cie1931_x - 0.3320) / (cie1931_y - 0.1858)
  CCT <- -449 * n^3 + 3525 * n^2 - 6823.3 * n + 5520.33

  # Combine into data frame
  cData <- data.frame(
    aopic, aopic_edi, elr, der,
    cie1924_v2_lux, cie2008_v2_lux, cie2008_v10_lux,
    cie1931_XYZ, cie1931_x, cie1931_y, cie1964_x, cie1964_y, CCT
  )
  names(cData) <- c(
    "sc", "mc", "lc", "mel", "rod",
    "scEDI", "mcEDI", "lcEDI", "melEDI", "rodEDI",
    "scELR", "mcELR", "lcELR", "melELR", "rodELR",
    "scDER", "mcDER", "lcDER", "melDER", "rodDER",
    "cie1924_v2_lux", "cie2008_v2_lux", "cie2008_v10_lux", "cie1931_XYZ",
    "cie1931_x", "cie1931_y", "cie1964_x", "cie1964_y", "CCT"
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
