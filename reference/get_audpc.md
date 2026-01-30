# Get the AUDPC from an epicrop_sim Object

Get the AUDPC value from an epicrop model run object's attributes for
use in analysis, mapping, or reporting.

## Usage

``` r
get_audpc(x)
```

## Arguments

- x:

  an object of class `epicrop.sim` as returned by any of the modelling
  functions in epicrop.

## Value

A scalar numeric double precision value of the AUCPC for the given
object.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)
bb <- bacterial_blight(wth, emergence = "2000-07-01")
get_audpc(bb)
#> [1] 15.13818
```
