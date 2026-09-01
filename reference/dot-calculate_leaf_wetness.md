# Calculate leaf wetness duration (wet-hours) from relative humidity and rainfall

Calculates daily leaf wetness duration as wet-hours from 0 to 24 using
relative humidity and rainfall. The wet-hours output can then be mapped
to an infection-rate multiplier from 0 to 1 using an RcW curve (hours
-\> 0 to 1).

## Usage

``` r
.calculate_leaf_wetness(wth, rhlim, rainlim, weighted_wetness = FALSE)
```

## Arguments

- wth:

  Weather data in the proper format.

- rhlim:

  Relative humidity threshold (%) used to determine wetness.

- rainlim:

  Rainfall scaling threshold (mm per wet-hour equivalent).

- weighted_wetness:

  If TRUE, compute weighted hourly wetness (0 to 24); if FALSE, count
  hours where hourly RH \>= rhlim (0 to 24).

## Value

A numeric vector of daily wet-hours from 0 to 24, length nrow(wth). Use
with RcW (hours -\> 0 to 1) to obtain the wetness multiplier for
infection.

## Details

Two modes are supported:

- Unweighted (default): counts the number of hourly RH values \>= rhlim
  (0 to 24 wet-hours).

- Weighted (weighted_wetness = TRUE): converts each hourly RH value to a
  wetness weight from 0 to 1 (0 below 80%, 1 at/above rhlim) and sums
  weights across 24 hours (0 to 24 wet-hours).

Rainfall contributes additional wet-hours proportional to RAIN /
rainlim, capped at 24, and combined without double-counting by scaling
to the remaining dry fraction of the day. If TMIN/TMAX are missing, the
function falls back to a binary wetness day (0 or 24 wet-hours) based on
rhlim/rainlim thresholds.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-07-31")
)

wet_simple <- .calculate_leaf_wetness(
  wth = wth,
  rhlim = 90,
  rainlim = 5,
  weighted_wetness = FALSE
)
summary(wet_simple)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   4.708  11.025  13.604  13.173  15.610  18.433 
```
