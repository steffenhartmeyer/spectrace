#' Interpolate spectral data
#'
#' This function interpolates Spectrace's spectral irradiance output to visible
#' range (380-780nm) with 1nm or 5nm resolution.
#'
#' @param lightData Data frame containing the calibrated light data
#'    for the channels from 410nm to 730nm. Additional variables are allowed.
#' @param resolution String specifying the resolution of the output
#'    spectrum. Can be "5nm" (default) or "1nm".
#' @param interp_method The interpolation method. Can be either "pchip" (default)
#'    or "linear". Pchip (piecewise cubic hermetic interpolation) results in a
#'    smooth spectrum while preserving the source values as local minima/maxima.
#' @param normalize Logical. Should the interpolated spectrum be normalized to
#'    peak = 1?
#'
#' @return The original data frame with the spectral data replaced by the
#'    interpolated spectral data.
#' @export
#'
#' @examples
spectrace_interpolate_spectra <- function(lightData,
                                          resolution = c("5nm", "1nm"),
                                          interp_method = c("pchip", "linear"),
                                          normalize = FALSE) {

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

  if(normalize){
    irr_interp <- irr_interp / apply(irr_interp, 1, max)
  }

  irr_interp <- data.frame(irr_interp)
  names(irr_interp) <- paste0(seq(380, 780, reso.num), "nm")

  lightData %>%
    dplyr::select(!c("410nm":"730nm")) %>%
    tibble::add_column(irr_interp)
}
