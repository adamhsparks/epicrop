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
#> [1] TRUE
.is_boolean(FALSE)          # TRUE
#> [1] TRUE
.is_boolean(NA)             # FALSE
#> [1] FALSE
.is_boolean(1)              # FALSE
#> [1] FALSE
.is_boolean(c(TRUE, FALSE)) # FALSE
#> [1] FALSE
```
