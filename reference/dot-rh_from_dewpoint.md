# Relative Humidity from Temperature and Dew Point

Computes relative humidity from air temperature and dew point
temperature using the improved Magnus form of Alduchov and Eskridge
(1996), `es(T) = 6.1094 * exp(17.625 * T / (T + 243.04))` (hPa), so that
`RH = 100 * es(Td) / es(T)`. Values are capped at 100% because
reanalysis dew point can marginally exceed air temperature.

## Usage

``` r
.rh_from_dewpoint(temp, dewpoint)
```

## Arguments

- temp:

  Numeric air temperature (degrees C).

- dewpoint:

  Numeric dew point temperature (degrees C).

## Value

Numeric relative humidity (%), `NA` where either input is `NA`.

## References

Alduchov, O. A. and Eskridge, R. E. (1996). Improved Magnus form
approximation of saturation vapor pressure. *Journal of Applied
Meteorology and Climatology*, 35(4), 601-609.
[doi:10.1175/1520-0450(1996)035\<0601:IMFAOS\>2.0.CO;2](https://doi.org/10.1175/1520-0450%281996%29035%3C0601%3AIMFAOS%3E2.0.CO%3B2)
