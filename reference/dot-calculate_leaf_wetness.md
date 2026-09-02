# Calculate Leaf Wetness Duration (Wet-Hours) from Relative Humidity and Rainfall

Calculates daily leaf wetness duration as wet-hours (0 to 24) from
relative humidity, temperature, and rainfall. The output can be mapped
to an infection-rate multiplier (0 to 1) using an RcW curve or a simple
linear relationship.

## Usage

``` r
.calculate_leaf_wetness(wth, rhlim, rainlim, weighted_wetness = FALSE)
```

## Arguments

- wth:

  Weather data (data.frame or data.table) with columns:

  - `RHUM`: Mean daily relative humidity (%)

  - `RAIN`: Daily rainfall (mm)

  - `DOY`: Day of year (1-365)

  - `TEMP`: Mean daily temperature (degrees C); or both TMIN/TMAX

  - `TMIN`: Minimum daily temperature (degrees C) \[optional if TEMP
    present\]

  - `TMAX`: Maximum daily temperature (degrees C) \[optional if TEMP
    present\]

  - `LAT`: Latitude (degrees) \[required for hourly calculation\]

- rhlim:

  Relative humidity threshold (%, default 90) for determining leaf
  wetness. Values \>= `rhlim` are considered wet.

- rainlim:

  Rainfall threshold (mm, default 5) for contributing to wet-hours. Each
  mm of rain = 1/rainlim hours of additional wetness.

- weighted_wetness:

  Logical. If TRUE, compute weighted hourly RH (0-1 scale from 80% to
  `rhlim`); if FALSE (default), count binary wet hours (RH \>= `rhlim`).

## Value

Numeric vector of daily wet-hours (0-24), length = `nrow(wth)`. Can be
passed to an RcW curve to obtain the infection-rate multiplier (0-1).

## Calculation Methods

Two modes are supported:

- **Unweighted** (default): Counts hourly RH values \>= `rhlim` (0-24
  hours)

- **Weighted**: Converts hourly RH to a wetness weight (0 at 80%, 1 at
  `rhlim`%) and sums across 24 hours (0-24 wet-hours)

Rainfall contributes additional wet-hours (`RAIN / rainlim`, capped at
24) combined with humidity wet-hours without double-counting by scaling
to the remaining dry fraction.

**Fallback behavior:** If TMIN/TMAX are missing, returns binary wetness
(0 or 24 hours) based on `rhlim` and `rainlim` thresholds.

## Temperature and RH Interpolation

Hourly RH is derived from daily mean RH and temperature via:

1.  Compute vapour pressure from mean RH and mean temperature

2.  Calculate hourly temperature using a sine/exponential diurnal model

3.  Compute hourly saturated vapour pressure

4.  Calculate hourly RH = 100 \* (vapour pressure / saturated pressure)

This approach accounts for the fact that RH varies inversely with
temperature during the day (peaks at dawn/dusk, drops at noon) even when
vapour pressure is constant.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Example 1: Basic leaf wetness calculation
wth <- get_wth(
  lonlat = c(121.255669, 14.16742),
  dates = "2000-06-30",
  duration = 10L
)

# Unweighted wetness (count hours RH >= 90%)
wet_hours <- .calculate_leaf_wetness(
  wth = wth,
  rhlim = 90,
  rainlim = 5,
  weighted_wetness = FALSE
)
summary(wet_hours)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   10.00   11.50   13.00   13.27   14.50   18.00 
# Typical range: 0-24 hours per day

# Example 2: Weighted wetness for more sensitive pathogen model
wet_weighted <- .calculate_leaf_wetness(
  wth = wth,
  rhlim = 90,
  rainlim = 5,
  weighted_wetness = TRUE
)
head(cbind(wth$YYYYMMDD, wet_hours, wet_weighted), 10)
#>             wet_hours wet_weighted
#>  [1,] 11138        12           14
#>  [2,] 11139        14           18
#>  [3,] 11140        10           13
#>  [4,] 11141        17           21
#>  [5,] 11142        13           18
#>  [6,] 11143        15           20
#>  [7,] 11144        10           13
#>  [8,] 11145        11           15
#>  [9,] 11146        12           15
#> [10,] 11147        14           20

# Example 3: Using with RcW curve for infection rate
# Linear relationship: 0 hours = 0 modifier, 24 hours = 1 modifier
wet_hrs <- .calculate_leaf_wetness(wth, rhlim = 90, rainlim = 5)
Rc_wet <- pmin(1, wet_hrs / 24)  # Linear interpolation
plot(wth$DOY, wet_hrs, type = "l", xlab = "Day of Year", ylab = "Wet-Hours")


# Example 4: Effect of rainfall
# Create synthetic weather with same RH but varying rain
wth_dry <- wth[RAIN < 2]
wth_wet <- wth[RAIN >= 2]
wet_dry <- .calculate_leaf_wetness(wth_dry, rhlim = 90, rainlim = 5)
wet_rain <- .calculate_leaf_wetness(wth_wet, rhlim = 90, rainlim = 5)
mean(wet_dry, na.rm = TRUE)  # Without rain
#> [1] NaN
mean(wet_rain, na.rm = TRUE) # With rain
#> [1] 13.27273
```
