# Fast linear interpolation matching stats::approx()

Vectorized linear interpolation that produces identical results to
stats::approx(method = "linear", rule = 2) but with better performance
for repeated calls.

## Usage

``` r
.fast_interp_approx_internal(mat, xout)
```

## Arguments

- mat:

  Two-column numeric matrix \[x, y\]

- xout:

  Vector of x values to interpolate at

## Value

Vector of interpolated y values (identical to
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) output)

## Details

Automatically sorts by x column if unsorted, handles duplicates using
[`approx()`](https://rdrr.io/r/stats/approxfun.html)'s tie handling
(keeps first value), and uses constant extrapolation at boundaries (rule
= 2).
