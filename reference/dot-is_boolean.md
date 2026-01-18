# Boolean

Checks if provided object is a Boolean, *i.e.* a length-one logical
vector.

## Usage

``` r
.is_boolean(x)
```

## Source

<https://github.com/Rapporter/rapportools/blob/master/R/utils.R>

## Arguments

- x:

  an object to check.

## Value

a logical value indicating whether provided object is a `Boolean`.

## Examples

``` r
is_boolean(TRUE) # [1] TRUE
#> Error in is_boolean(TRUE): could not find function "is_boolean"
# the following will work on most systems, unless you have tweaked .Rprofile
.is_boolean(T) # [1] TRUE
#> Error in .is_boolean(T): could not find function ".is_boolean"
.is_boolean(1) # [1] FALSE
#> Error in .is_boolean(1): could not find function ".is_boolean"
```
