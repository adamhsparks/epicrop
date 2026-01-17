# Check if the relative humidity is above the threshold

Check if the relative humidity is above the threshold

## Usage

``` r
.check_threshold(rh_hr, rhlim)
```

## Arguments

- rh_hr:

  A list of 24 relative humidity values for every DOY in the data.

- rhlim:

  The relative humidity threshold to check against.

## Examples

``` r
.check_threshold(rh_hr = rep(95, 24), rhlim = 95)
#> [1] 24
```
