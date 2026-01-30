# Calculate day length for a given latitude on a given date

Calculate day length for a given latitude on a given date

## Usage

``` r
.day_length(lat, doy)
```

## Arguments

- lat:

  Latitude as provided by `wth` via
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

- doy:

  Sequential day of year (Julian date) as provided by `wth` via
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

## Value

A numeric vector of daylight hours based on latitude and date.

## References

William C. Forsythe and Edward J. Rykiel and Randal S. Stahl and Hsin-i
Wu and Robert M. Schoolfield. Ecological Modeling, Volume 80 (1995) pp.
87-95, "A Model Comparison for Day Length as a Function of Latitude and
Day of the Year",
[doi:10.1016/0304-3800(94)00034-F](https://doi.org/10.1016/0304-3800%2894%2900034-F)

## Author

Robert J. Hijmans, modifications by Adam H. Sparks,
<adamhsparks@gmail.com>

## Examples

``` r
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)
.day_length(lat = wth[, LAT], doy = wth[, DOY])
#> Error in .day_length(lat = wth[, LAT], doy = wth[, DOY]): could not find function ".day_length"
```
