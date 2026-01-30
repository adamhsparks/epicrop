# Interpolate parameter values

Internal function to handle different parameter input types (scalar,
vector, matrix lookup table, or function) and interpolate to daily
values.

## Usage

``` r
.interp(param, age_vec)
```

## Arguments

- param:

  Parameter value(s). Can be:

  - Scalar: single value used for all days

  - Vector: must match length of `age_vec`

  - Matrix: 2-column lookup table (column 1 = age, column 2 = modifier)

  - Function: function of age that returns modifier values

- age_vec:

  Numeric vector of ages (days or DVS) to interpolate over

## Value

Numeric vector of length `length(age_vec)` with interpolated values

## Details

For matrix input, uses linear interpolation via
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) with
`rule = 2` (constant extrapolation at boundaries).

For function input, applies function to `age_vec` and sets any negative
values to 0.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>
