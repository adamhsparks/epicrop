# Calculate Saturated Vapor Pressure

Computes saturated vapour pressure from temperature using the Alduchov
and Eskridge (1996) formula, valid from -40 to +50 degrees C.

## Usage

``` r
.saturated_vapour_pressure(tmp)
```

## Arguments

- tmp:

  Temperature (degrees C; scalar or vector).

## Value

Saturated vapour pressure (kPa; scalar or vector).

## Formula

    es = 0.61094 * exp((17.625 * T) / (T + 243.04))

where T is temperature in degrees C and es is in kilopascals (kPa).

## References

Alduchov, O. A., and R. E. Eskridge (1996). Improved Magnus form
approximation of saturation vapour pressure. *J. Appl. Meteor.*, 35:
601-609.
[doi:10.1175/1520-0450(1996)035\<0601:IMFAOS\>2.0.CO;2](https://doi.org/10.1175/1520-0450%281996%29035%3C0601%3AIMFAOS%3E2.0.CO%3B2)

## Author

Adam H. Sparks

## Examples

``` r
# Vapor pressure curve over typical temperature range
temps <- seq(0, 40, by = 5)
es <- .saturated_vapour_pressure(temps)
plot(temps, es, type = "b", xlab = "Temperature (degrees C)",
     ylab = "es (kPa)",
     main = "Saturated Vapor Pressure")


# Rapid increase at higher temps (why RH drops in afternoon)
es_20 <- .saturated_vapour_pressure(20)
es_30 <- .saturated_vapour_pressure(30)
es_30 / es_20  # ~1.5x increase for 10 degrees C rise
#> [1] 1.815624
```
