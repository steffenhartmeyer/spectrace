#' Interpolate spectral data
#'
#' This function interpolates Spectrace's spectral irradiance output to 5nm
#' resolution.
#'
#' @param lightData Data frame containing the calibrated light data
#'    for the channels from 410nm to 730nm.
#' @param resolution Integer. Resolution of the
#' @param interp_method Method for interpolation. Can be "pchip" (default) or
#'    "linear".
#'
#' @return Data frame with illuminance, alpha-opic irradiances, alpha-opic EDI,
#' alpha-opic ELR, alpha-opic DER, and CCT.
#' @export
#'
#' @examples
spectrace_interpolate_spectra <- function(lightData,
                                          resolution = c("5nm", "1nm"),
                                          interp_method = c("pchip", "linear")) {

  # Match arguments
  resolution <- match.arg(resolution)
  interp_method <- match.arg(interp_method)

  # Irradiance data
  irr_data <- lightData %>%
    dplyr::select("410nm":"730nm") %>%
    as.matrix()

  # Input wavelengths
  reso.num <- as.numeric(substr(resolution, 1, 1))
  wl <- c(
    380, 410, 435, 460, 485, 510, 535, 560,
    585, 610, 645, 680, 705, 730, 780 - reso.num
  )
  x <- rep(wl, nrow(irr_data)) +
    as.numeric(t(matrix(
      rep(seq(0, 400 * (nrow(irr_data) - 1), 400), length(wl)),
      ncol = length(wl)
    )))

  # Output wavelengths
  x_out <- switch(resolution,
    "5nm" = seq(380, 775 + 400 * (nrow(irr_data) - 1), 5),
    "1nm" = seq(380, 779 + 400 * (nrow(irr_data) - 1), 1),
    stop("Wrong resolution!")
  )

  # Reshape irradiance data to single vector
  zeros <- rep(0, nrow(irr_data))
  y <- as.numeric(t(cbind(zeros, irr_data, zeros)))

  # Interpolate
  irr_interp <- switch(interp_method,
    "pchip" = signal::pchip(x, y, x_out),
    "linear" = approx(x, y, x_out, method = "linear", rule = 2)[[2]],
    stop("Wrong interpolation method!")
  )

  # Reshape vector to matrix
  irr_interp <- t(matrix(irr_interp, ncol = nrow(irr_data)))
  irr_interp <- cbind(irr_interp, zeros)
  irr_interp[irr_interp < 0] <- 0

  irr_interp <- data.frame(irr_interp)
  names(irr_interp) <- paste0(seq(380, 780, reso.num), "nm")

  lightData %>%
    dplyr::select(!c("410nm":"730nm")) %>%
    tibble::add_column(irr_interp)
}