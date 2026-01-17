# Use approx() to Return a Modifier Value From an RcA or RcT Curve

Use approx() to Return a Modifier Value From an RcA or RcT Curve

## Usage

``` r
.fn_Rc(.Rc, .xout)
```

## Arguments

- .Rc:

  A matrix describing a growth curve for either temperature, `RcT`, age,
  `RcA`, or relative humidity, `RcRH`.

- .xout:

  a value for `x`, either a temperature, age or relative humidity
  modifier value.

## Value

A numeric value for modifying a growth curve in
[seir](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

## Note

This is a faster (and more simple) function that does what the original
cropsim `afgen()` does.

The original implementation of this function comes from
[ZeBook](https://CRAN.R-project.org/package=ZeBook) and was adapted for
use in epicrop by Adam H. Sparks.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>
