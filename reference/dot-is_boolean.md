# Check if value is a single boolean

Check if value is a single boolean

## Usage

``` r
.is_boolean(x)
```

## Arguments

- x:

  Value to check

## Value

TRUE if x is exactly TRUE or FALSE, FALSE otherwise (including NA)

## Details

This function checks that a value is:

- Logical type

- Length 1 (scalar)

- Not NA

## Examples

``` r
.is_boolean(TRUE)           # TRUE
#> Error in .is_boolean(TRUE): could not find function ".is_boolean"
.is_boolean(FALSE)          # TRUE
#> Error in .is_boolean(FALSE): could not find function ".is_boolean"
.is_boolean(NA)             # FALSE
#> Error in .is_boolean(NA): could not find function ".is_boolean"
.is_boolean(1)              # FALSE
#> Error in .is_boolean(1): could not find function ".is_boolean"
.is_boolean(c(TRUE, FALSE)) # FALSE
#> Error in .is_boolean(c(TRUE, FALSE)): could not find function ".is_boolean"
```
