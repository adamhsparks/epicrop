# Apply Function-Based Parameter Over Age Vector

Evaluates a user-supplied function at each age value and clamps negative
results to 0.

## Usage

``` r
.interp_fn(param, age_vec)
```

## Arguments

- param:

  Function taking numeric vector (ages) and returning numeric vector
  (modifiers).

- age_vec:

  Numeric vector of ages to evaluate function at.

## Value

Numeric vector of function results, with negative values clamped to 0.

## Details

Catches function evaluation errors and aborts with informative message.
Negative results are silently converted to 0 (biologically reasonable
for growth/senescence rates).

## Examples

``` r
# Temperature-dependent infection rate
RcT_fn <- function(temp) {
  pmax(0, 1 - (abs(temp - 25) / 15))  # Optimum at 25 degrees C
}
temps <- seq(10, 40, by = 2)
modifiers <- .interp_fn(RcT_fn, temps)
#> Error in .interp_fn(RcT_fn, temps): could not find function ".interp_fn"
plot(temps, modifiers, type = "b", main = "Temperature Response")
#> Error: object 'modifiers' not found
```
