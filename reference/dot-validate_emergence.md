# Validates dates, normally the emergence date.

Validates dates, normally the emergence date.

## Usage

``` r
.validate_emergence(x)
```

## Arguments

- x:

  a date input (character, Date, POSIXct).

## Value

A validated POSIXct date object.

## Examples

``` r
# passes
.validate_emergence("2023-10-15")
#> [1] "2023-10-15"

# fails
 .validate_emergence("120-100-2023")
#> Error in .validate_emergence("120-100-2023"): `x` is not in a valid date format.
```
