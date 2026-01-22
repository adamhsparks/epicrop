# Check User Input Dates for Validity

Check User Input Dates for Validity

## Usage

``` r
.check_date(x)
```

## Arguments

- x:

  User entered date value

## Value

Validated date string as a `POSIXct` object.

## Note

This was taken from
[nasapower](https://CRAN.R-project.org/package=nasapower).

## Author

Adam H. Sparks <adamhsparks@curtin.edu.au>

## Examples

``` r
.check_date("2024-01-01")
#> Error in .check_date("2024-01-01"): could not find function ".check_date"
```
