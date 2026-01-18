# Calculate saturated vapour pressure, es

Calculate saturated vapour pressure, es

## Usage

``` r
.saturated_vapour_pressure(tmp)
```

## Arguments

- tmp:

  Mean temperature as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

## Value

Single double precision value of es as kilopascals

## References

Alduchov and Eskridge 1995,
\<doi:10.1175/1520-0450(1996)035\<0601:IMFAOS\>2.0.CO;2\>

## Author

Adam H. Sparks

## Examples

``` r
.saturated_vapour_pressure(0)
#> [1] 0.61094
```
