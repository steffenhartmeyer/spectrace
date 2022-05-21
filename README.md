
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

# Calculate alpha-opic quantities 
data.aopic = spectrace_aopic(data.cal)

# Calculate alpha-opic quantities with faster linear interpolation
data.aopic2 = spectrace_aopic(data.cal, 
                              interp_method = "linear")

# Import raw data, calibrate, and calculate alpha-opic quantities with linear interpolation
data.aopic3 = spectrace_import_aopic(light_data, 
                                     tz = "Europe/Berlin", 
                                     serial_number = "208731924656", 
                                     interp_method = "linear")
```

