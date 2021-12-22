
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
activity_data = system.file("extdata", "example_activity.csv", package = "spectrace")

# Import raw spectrace light data
data.raw = spectrace_import(light_data, tz = "Europe/Berlin")

# Import raw spectrace light and activity data 
data.raw2 = spectrace_import(light_data, activity_data, offset = 2, tz = "Europe/Berlin")

# Calculate alpha-opic quantities with default calibration file
irradiance_data = data.raw[,7:20]
data.aopic = spectrace_aopic(irradiance_data)

# Calculate alpha-opic quantities with custom calibration data
#custom_cal = read.csv("custom_calibration.csv")
#data.aopic = spectrace_aopic(irradiance_data, custom_cal)

# Import raw data and calculate and add alpha-opic quantities to data 
data.all = spectrace_import_aopic(light_data, tz = "Europe/Berlin", include_raw = TRUE)
```

