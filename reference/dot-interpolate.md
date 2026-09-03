# Perform Vectorised Linear Interpolation

Core vectorised interpolation using pre-sorted x-y points and output
x-coordinates. Uses
[`base::findInterval()`](https://rdrr.io/r/base/findInterval.html) for
efficiency.

## Usage

``` r
.interpolate(x, y, n, xout)
```

## Arguments

- x:

  Numeric vector of x-coordinates (sorted, deduplicated).

- y:

  Numeric vector of y-coordinates (same length as x).

- n:

  Integer, length of x (pre-computed for efficiency).

- xout:

  Numeric vector of output x-coordinates.

## Value

Numeric vector of interpolated y-values.

## Method

Computes linear interpolation between consecutive x-y points:

\\y_interp = y\[i\] + w \* (y\[i+1\] - y\[i\])\\ where \\w = (x -
x\[i\]) / (x\[i+1\] - x\[i\])\\

Boundary handling:

- `x < min(x_vec)`: \\y = y\[1\]\\

- `x > max(x_vec)`: \\y = y\[n\]\\
