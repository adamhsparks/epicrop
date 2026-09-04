# Calculate Hourly Relative Humidity for a Given Day

Derives hourly relative humidity from daily mean RH and hourly
temperature by maintaining constant vapour pressure (VP) throughout the
day.

## Usage

``` r
.diurnal_rh(rh, tmin, tmax, tmp, lat, doy)
```

## Arguments

- rh:

  Mean daily relative humidity (%).

- tmin:

  Minimum daily temperature (C).

- tmax:

  Maximum daily temperature (C).

- tmp:

  Mean daily temperature (C).

- lat:

  Latitude (degrees).

- doy:

  Day of year (1-365).

## Value

List of numeric vectors (one per day, 24 values each representing hourly
RH %).

## Algorithm

1.  Compute vapour pressure from mean RH and mean temperature (constant
    all day)

2.  Compute hourly saturated vapour pressure using diurnal temperature

3.  Hourly RH = \\100 \* (VP / saturated VP)\\, clamped to \[0, 100\]

This accounts for the inverse relationship between temperature and RH
even when actual water vapour content is unchanged.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Calculate hourly RH for 3 days
rh_list <- .diurnal_rh(
  rh = c(80, 85, 90),           # Mean daily RH (%)
  tmin = c(20, 19, 21),         # Min temp (C)
  tmax = c(30, 32, 29),         # Max temp (C)
  tmp = c(25, 25.5, 25),        # Mean temp (C)
  lat = c(14.16742, 14.16742, 14.16742),  # Latitude
  doy = c(182, 183, 184)        # Day of year
)
#> Error in .diurnal_rh(rh = c(80, 85, 90), tmin = c(20, 19, 21), tmax = c(30,     32, 29), tmp = c(25, 25.5, 25), lat = c(14.16742, 14.16742,     14.16742), doy = c(182, 183, 184)): could not find function ".diurnal_rh"

# Each element is 24-hour RH profile
str(rh_list)
#> Error: object 'rh_list' not found
plot(1:24, rh_list[[1]], type = "l", ylim = c(0, 100),
     xlab = "Hour of Day", ylab = "Relative Humidity (%)")
#> Error: object 'rh_list' not found
```
