
# spectrace

<!-- badges: start -->
<!-- badges: end -->

The goal of spectrace is to provide functions to import raw Spectrace data and calculate relevant quantities.

## Installation

You can install the released version of spectrace using:

``` r
remotes::install_github("steffenhartmeyer/spectrace")
```

## Example

This is a basic example which shows you how to use the functions:

``` r
library(spectrace)

# Paths to example data
light_data = system.file("extdata", "example_light.csv", package = "spectrace")

# Import raw spectrace light data
data.raw = spectrace_import_light(light_data, 
                                  tz = "Europe/Berlin", 
                                  serial_number = "208731924656")

# Calibrate raw light data with default calibration data
data.cal = spectrace_calibrate_light(data.raw)

## Not run:
# Calibrate raw light data with custom calibration data
data.cal2 = spectrace_calibrate_light(data.raw, cal_data = custom_calibration)
## End(**Not run**)

# Calculate all quantities 
data.quantities = spectrace_calculate_quantities(data.cal)

# Calculate only specific quantities
data.quantities.2 = spectrace_calculate_quantities(data.cal, c("melEDI", "CCT"))

# Interpolate spectra to 5nm resolution
data.spectra = spectrace_interpolate_spectra(data.cal)

```

