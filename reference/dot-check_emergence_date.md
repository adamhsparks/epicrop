# Ensure that the establishment date is a single value

Ensure that the establishment date is a single value

## Usage

``` r
.check_emergence_date(x)
```

## Arguments

- x:

  a date object

## Value

Nothing, called for its side-effects of checking the user-input date.

## Examples

``` r
.check_establishment_date(x = "2000-01-01")
#> Error in .check_establishment_date(x = "2000-01-01"): could not find function ".check_establishment_date"
```
