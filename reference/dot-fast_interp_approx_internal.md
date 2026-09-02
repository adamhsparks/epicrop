# Fast Linear Interpolation with Boundary Handling

Core interpolation algorithm using
[`base::findInterval()`](https://rdrr.io/r/base/findInterval.html) and
vectorized linear interpolation. Matches
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) with
`rule = 2` exactly.

## Usage

``` r
.fast_interp_approx_internal(mat, xout)
```

## Arguments

- mat:

  Numeric 2-column matrix with x (col 1) and y (col 2) values. Rows may
  be unordered or contain duplicates.

- xout:

  Numeric vector of x-coordinates to interpolate at.

## Value

Numeric vector of interpolated y-values.

## Details

Internally calls:

1.  [`.prep_xy()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-prep_xy.md)
    to sort/deduplicate

2.  [`.interpolate()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-interpolate.md)
    to perform vectorized interpolation

Edge cases:

- Empty `xout`: returns numeric(0)

- Single row in matrix: returns y\[1\] repeated

- `xout` below min(x): constant extrapolation to y\[1\]

- `xout` above max(x): constant extrapolation to y\[n\]

## Algorithm

1.  Prepare x-y points (sorted, deduplicated)

2.  Use
    [`base::findInterval()`](https://rdrr.io/r/base/findInterval.html)
    to locate each output point in x-grid

3.  Categorize points:

    - Below \\min(x)\\: use \\y\[1\]\\ (constant)

    - Above \\max(x)\\: use \\y\[n\]\\ (constant)

    - Between \\x\[i\]\\ and \\x\[i+1\]\\: linear interpolation

4.  Return interpolated y-values

## Examples

``` r
# Simple lookup table
mat <- rbind(c(0, 0.05), c(60, 0.6), c(120, 0.6))
xout <- c(0, 30, 60, 90, 120, 150)
y_interp <- .fast_interp_approx_internal(mat, xout)
print(cbind(xout, y_interp))
#>      xout y_interp
#> [1,]    0    0.050
#> [2,]   30    0.325
#> [3,]   60    0.600
#> [4,]   90    0.600
#> [5,]  120    0.600
#> [6,]  150    0.600
#      xout y_interp
# [1,]    0     0.05     (exact match)
# [2,]   30     0.325    (linear interp: 0.05 + 30/60 * (0.6-0.05))
# [3,]   60     0.6      (exact match)
# [4,]   90     0.6      (plateau)
# [5,]  120     0.6      (exact match)
# [6,]  150     0.6      (extrapolated)
```
