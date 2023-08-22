#' Interpolate spectral data
#'
#' This function interpolates Spectrace's spectral irradiance output to visible
#' range (380-780nm) with 1nm or 5nm resolution.
#'
#' @param lightData Data frame containing the calibrated light data
#'    for the channels from 410nm to 730nm. Additional variables are allowed.
#'    Data needs to be in wide format (see \code{\link{spectrace_to_wide}}).
#' @param resolution String specifying the resolution of the output
#'    spectrum. Can be "5nm" (default), "1nm", or "spectrace". The latter will result
#'    in the original 14 spectrace channels.
#' @param interp_method The interpolation method. Can be either "pchip" (default)
#'    or "linear". Pchip (piecewise cubic hermetic interpolation) results in a
#'    smooth spectrum while preserving the source values as local minima/maxima.
#'
#' @return The original data frame with the spectral data replaced by the
#'    interpolated spectral data.
#' @export
#'
#' @examples
spectrace_interpolate_spectra <- function(lightData,
                                          resolution = c("5nm", "1nm", "spectrace"),
                                          interp_method = c("pchip", "linear")) {
  # Match arguments
  resolution <- match.arg(resolution)
  interp_method <- match.arg(interp_method)

  # Ungroup data
  if (dplyr::is_grouped_df(lightData)) {
    warning("Data frame is grouped and will be ungrouped.")
    lightData <- lightData %>% dplyr::ungroup()
  }

  # Return spectrace resolution
  if (resolution == "spectrace"){
    irr_data <- lightData %>%
      dplyr::select("410nm","435nm","460nm","485nm","510nm","535nm",
                    "560nm","585nm","610nm","645nm","680nm","705nm","730nm","760nm")
    return(
      lightData %>% dplyr::select(!dplyr::matches("\\d{3}nm")) %>%
        tibble::add_column(irr_data)
    )
  }

  # Irradiance data
  irr_data <- lightData %>%
    dplyr::select(dplyr::matches("\\d{3}nm")) %>%
    as.matrix()

  # Input wavelengths
  wl.in <- sub("nm", "", colnames(irr_data)) %>%
    as.numeric()

  # Get desired resolution
  reso.num <- as.numeric(substr(resolution, 1, 1))
  wl.out <- seq(380, 780, reso.num)

  # Check whether already interpolated
  if (setequal(wl.out, wl.in)) {
    warning("Data seems already interpolated. Returning data without interpolation.")
    return(lightData)
  }

  N <- nrow(irr_data)

  pad.380 = ifelse(380 %in% wl.in, irr_data[, wl.in == 380], rep(0, N))
  pad.780 = ifelse(780 %in% wl.in, irr_data[, wl.in == 780], rep(0, N))

  irr_data <- irr_data[, wl.in > 380 & wl.in < 780]
  if (N == 1) {
    irr_data <- matrix(irr_data, nrow = 1)
  }

  wl <- c(380, wl.in[wl.in > 380 & wl.in < 780], 780)
  r <- seq(0, (400 + reso.num) * (N - 1), (400 + reso.num))

  # Reshape irradiance data to single vector
  y <- as.numeric(t(cbind(pad.380, irr_data, pad.780)))
  x.in <- (matrix(rep(wl, N), nrow = N, byrow = TRUE) + r) %>%
    t() %>%
    as.numeric()
  x.out <- (matrix(rep(wl.out, N), nrow = N, byrow = TRUE) + r) %>%
    t() %>%
    as.numeric()

  # Interpolate
  irr_interp <- switch(interp_method,
                       "pchip" = signal::pchip(x.in, y, x.out),
                       "linear" = approx(x.in, y, x.out, method = "linear", rule = 2)[[2]],
                       stop("Wrong interpolation method!")
  )

  # Reshape vector to matrix
  irr_interp <- t(matrix(irr_interp, ncol = N))
  irr_interp[irr_interp < 0] <- 0

  irr_interp <- data.frame(irr_interp)
  names(irr_interp) <- paste0(wl.out, "nm")

  lightData %>%
    dplyr::select(!dplyr::matches("\\d{3}nm")) %>%
    tibble::add_column(irr_interp)
}
