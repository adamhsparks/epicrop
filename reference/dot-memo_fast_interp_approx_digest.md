# Memoised Wrapper for Fast Interpolation with Content-Based Hashing

A memoised version of linear interpolation that caches results based on
the content (values) of the input matrix and x-coordinates, not their
object identity. Uses
[`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html)
to compute content hashes, enabling cache hits across function calls
even when inputs are reconstructed.

## Usage

``` r
.memo_fast_interp_approx_digest(mat, xout)
```

## Arguments

- mat:

  A numeric 2-column matrix with x-values (column 1) and y-values
  (column 2) defining the lookup table.

- xout:

  Numeric vector of x-coordinates to interpolate at.

## Value

A numeric vector of interpolated y-values, computed via
`.fast_interp_approx_internal()`.

## Details

**Caching strategy:**

- Converts `mat` to numeric vector and computes SHA1 hash via
  [`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html)

- Converts `xout` to numeric vector and computes SHA1 hash

- Combines hashes as memo key; cache hit if both match previous call

- Fallback to `.fast_interp_approx_internal()` if miss

**Performance:** Useful when:

- The same lookup tables and x-coordinates are reused across many calls

- Object identity changes but values remain constant

- Interpolation cost is high relative to hashing overhead

**Limitations:**

- Hashing overhead (~1-10 µs per call) may exceed interpolation time for
  small inputs

- Memo cache grows unbounded (no size limit by default)

- Not thread-safe with default memoise settings

## See also

- [`memoise::memoise()`](https://memoise.r-lib.org/reference/memoise.html)
  for memoisation mechanics

- [`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html)
  for content-based hashing

- `.fast_interp_approx_internal()` for the underlying interpolation
