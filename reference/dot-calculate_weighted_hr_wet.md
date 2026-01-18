# Calculates the weighted hourly leaf wetness value

Calculates the weighted hourly leaf wetness value

## Usage

``` r
.calculate_weighted_hr_wet(rh, rhlim)
```

## Arguments

- rh:

  A numeric vector of relative humidity values.

- rhlim:

  The relative humidity threshold to check against between `80` & `100`.

## Value

A `vector` of weighted values between 0 and 1 depending on the `rhlim`

## Note

This functionality is just a placeholder for future possible work. This
particular function is set up but not exposed to users yet.

## Examples

``` r
.calculate_weighted_hr_wet(rh = 86, rhlim = 95)
#> Error in .calculate_weighted_hr_wet(rh = 86, rhlim = 95): could not find function ".calculate_weighted_hr_wet"
```
