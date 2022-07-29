#' Interpolate spectral data
#'
#' This function interpolates Spectrace's spectral irradiance output to 5nm
#' resolution.
#'
#' @param lightData Data frame containing the calibrated light data
#'    for the channels from 410nm to 730nm.
#' @param interp_method Method for interpolation. Can be "pchip" (default) or
#'    "linear". Linear interpolation is considerably faster than pchip.
#'
#' @return Data frame with illuminance, alpha-opic irradiances, alpha-opic EDI,
#' alpha-opic ELR, alpha-opic DER, and CCT.
#' @export
#'
#' @examples
spectrace_interpolate_spectra <- function(lightData, interp_method = "pchip") {

  # Irradiance data
  irr_data <- lightData %>%
    dplyr::select("410nm":"730nm") %>%
    as.matrix()

  # Reshape matrix to single vector
  wl <- c(380, 410, 435, 460, 485, 510, 535, 560,
          585, 610, 645, 680, 705, 730, 775)
  zeros = rep(0, nrow(irr_data))
  y = as.numeric(t(cbind(zeros, irr_data, zeros)))
  x = rep(wl, nrow(irr_data)) +
    as.numeric(t(matrix(
      rep(seq(0, 400*(nrow(irr_data)-1), 400), length(wl)),
      ncol = length(wl))))
  x_out = seq(380,775+400*(nrow(irr_data)-1),5)

  # Interpolate to 5nm resolution
  if(interp_method == "pchip"){
    irr_interp = signal::pchip(x, y, x_out)
  }
  else if(interp_method == "linear"){
    irr_interp = approx(x, y, x_out, method = "linear", rule = 2)[[2]]
  }
  else{
    stop("Wrong interpolation method!")
  }

  # Reshape vector to matrix
  irr_interp = t(matrix(irr_interp, ncol = nrow(irr_data)))
  irr_interp = cbind(irr_interp, zeros)
  irr_interp[irr_interp < 0] <- 0

  irr_interp = data.frame(irr_interp)
  names(irr_interp) = paste0(seq(380,780, 5), "nm")

  lightData %>%
    dplyr::select(!c("410nm":"730nm")) %>%
    add_column(irr_interp)
}

