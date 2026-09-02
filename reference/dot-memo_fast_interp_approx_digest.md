# Memoised Fast Interpolation with Content-Based Hashing

A memoised version of
[`.fast_interp_approx_internal()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-fast_interp_approx_internal.md)
that caches results based on content hashing. Enables efficient reuse
when the same lookup tables and x-coordinates recur across multiple
function calls, even if the objects are reconstructed.

## Usage

``` r
.memo_fast_interp_approx_digest(mat, xout)
```

## Arguments

- mat:

  Numeric 2-column matrix with x (col 1) and y (col 2).

- xout:

  Numeric vector of x-coordinates to interpolate at.

## Value

Numeric vector of interpolated y-values.

## Caching Strategy

Instead of caching by object identity (memory address), this function:

1.  Converts `mat` (matrix) to numeric vector and computes SHA1 hash via
    [`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html)

2.  Converts `xout` (output x-coords) to numeric vector and computes
    SHA1 hash

3.  Uses combined hash as memo key

4.  On cache hit, returns cached result directly

5.  On cache miss, calls
    [`.fast_interp_approx_internal()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-fast_interp_approx_internal.md)
    and caches result

This approach enables cross-call reuse even when matrices and vectors
are reconstructed (new objects with same values).

## Performance

**When to use:**

- Same lookup tables repeated across multiple
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
  runs

- Same disease models run on many locations/dates

- Interpolation involves large x-coordinate grids

**When not worth it:**

- One-off calls or unique matrices each time

- Very small matrices/xout (hashing overhead dominates)

- Real-time systems with strict latency requirements

Hashing typically costs 1-10 microseconds per call. Interpolation itself
is already very fast (~1 microsecond). Cache hit saves ~1 microsecond
but costs 1-10 microseconds in hashing.

## Limitations

- Memo cache grows unbounded (no size limit by default)

- Not thread-safe with default
  [`memoise::memoise()`](https://memoise.r-lib.org/reference/memoise.html)
  settings

- Hash collisions (SHA1) extremely rare but not impossible

- Cache persists for session lifetime; restart R to clear

## See also

- [`memoise::memoise()`](https://memoise.r-lib.org/reference/memoise.html)
  for memoisation mechanics and cache control

- [`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html)
  for content-based hashing algorithm (SHA1 by default)

- [`.fast_interp_approx_internal()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-fast_interp_approx_internal.md)
  for underlying interpolation

- [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) for
  comparison (non-memoised, R's standard interpolation)

## Examples

``` r
# Example 1: Repeated interpolation with same lookup table
RcA <- rbind(c(0, 0.05), c(60, 0.6), c(120, 0.6))

# First call: computes and caches
age_vec1 <- 0:119
result1 <- .memo_fast_interp_approx_digest(RcA, age_vec1)

# Second call: same matrix, same xout -> cache hit (fast)
age_vec2 <- 0:119
result2 <- .memo_fast_interp_approx_digest(RcA, age_vec2)
identical(result1, result2)  # TRUE
#> [1] TRUE

# Third call: same matrix, different xout -> cache miss (recomputes)
age_vec3 <- 0:100
result3 <- .memo_fast_interp_approx_digest(RcA, age_vec3)
length(result3)  # 101 (different from result1)
#> [1] 101

# Example 2: Demonstrate content-based caching
# Reconstructed matrix with same values -> cache hit
RcA_reconstructed <- rbind(c(0, 0.05), c(60, 0.6), c(120, 0.6))

# Same xout as result1
result4 <- .memo_fast_interp_approx_digest(RcA_reconstructed, 0:119)
identical(result1, result4)  # TRUE (content same, so cache hit)
#> [1] TRUE
```
