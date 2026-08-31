# Evaluate a parameter that may be a constant or an interpolation table

Wraps [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) so
that scalar parameters (e.g. a constant `RRG`) and matrix parameters
(e.g. a temperature-dependent `RcT` table) can be queried through the
same interface.

## Usage

``` r
epi_eval_param(param, at, rule = 2L)
```

## Arguments

- param:

  Either a numeric scalar, or a 2-column matrix/data.frame with columns
  `x` (driver value) and `y` (modifier or rate value), as stored in
  `.epi_params`.

- at:

  Numeric. The driver value(s) to evaluate at (e.g. current temperature
  or DVS). Ignored if `param` is a scalar.

- rule:

  Passed to [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html);
  default `2` extends the boundary value beyond the table's range rather
  than returning `NA`.

## Value

Numeric value(s), same length as `at` (or length 1 for a scalar
`param`).
