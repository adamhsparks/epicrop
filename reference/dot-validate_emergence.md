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
#> Error in .validate_emergence("2023-10-15"): could not find function ".validate_emergence"

# fails
 .validate_emergence("120-100-2023")
#> Error in .validate_emergence("120-100-2023"): could not find function ".validate_emergence"
```
