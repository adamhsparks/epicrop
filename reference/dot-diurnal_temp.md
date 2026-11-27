# Calculate Hourly Temperature for a Given Day

Computes a full diurnal temperature curve from minimum, maximum, and
mean temperatures using a sine wave (day) and exponential decay (night)
model.

## Usage

``` r
.diurnal_temp(lat, doy, tmin, tmax)
```

## Arguments

- lat:

  Latitude (degrees; scalar or vector).

- doy:

  Day of year (1-365; scalar or vector).

- tmin:

  Minimum daily temperature (degrees C; scalar or vector).

- tmax:

  Maximum daily temperature (degrees C; scalar or vector).

## Value

Numeric vector of 24 hourly temperatures (for single day or vectorised
24\*n values for multiple days).

## Algorithm

Temperature varies sinusoidally during the day (sunrise to sunset) and
exponentially decays at night. Day length depends on latitude and day of
year. The model ensures continuity at sunrise and sunset.

## Author

Robert J. Hijmans (original); Adam H. Sparks (modifications)

## Examples

``` r
# Single day: latitude 14N, DOY 182 (Jul 1), Tmin=20, Tmax=30
hourly_temp <- .diurnal_temp(
  lat = 14.16742,
  doy = 182,
  tmin = 20,
  tmax = 30
)
#> Error in .diurnal_temp(lat = 14.16742, doy = 182, tmin = 20, tmax = 30): could not find function ".diurnal_temp"
plot(1:24, hourly_temp, type = "l", ylim = c(15, 35),
     xlab = "Hour of Day", ylab = "Temperature (degrees C)",
     main = "Diurnal Temperature Curve")
#> Error: object 'hourly_temp' not found
abline(h = c(20, 30), col = "red", lty = 2)
#> Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): plot.new has not been called yet

# Multiple days (vectorised)
hourly_temps <- .diurnal_temp(
  lat = rep(14.16742, 3),
  doy = 182:184,
  tmin = c(20, 19, 21),
  tmax = c(30, 32, 29)
)
#> Error in .diurnal_temp(lat = rep(14.16742, 3), doy = 182:184, tmin = c(20,     19, 21), tmax = c(30, 32, 29)): could not find function ".diurnal_temp"
length(hourly_temps)  # Should be 24 * 3 = 72
#> Error: object 'hourly_temps' not found
```
