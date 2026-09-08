# Calculate Weighted Hourly Leaf Wetness

Converts hourly relative humidity to a weighted wetness value (0-1),
transitioning smoothly from 0 at 80% RH to 1 at `rhlim`% RH.

## Usage

``` r
.calculate_weighted_hr_wet(rh, rhlim)
```

## Arguments

- rh:

  Hourly relative humidity (%; scalar or vector, 0-100).

- rhlim:

  Relative humidity threshold for full wetness (%, \>= 80).

## Value

Numeric vector of weights (0-1) for each RH value.

## Weighting Scheme

- RH \< 80%: weight = 0 (no wetness)

- 80% \<= RH \< rhlim%: weight = (RH - 80) / (rhlim - 80) (linear
  transition)

- RH \>= rhlim%: weight = 1 (fully wet)

This creates a smooth response where partial wetness at intermediate RH
values contributes proportionally to the daily wet-hour total.

## Author

Adam H. Sparks

## Examples

``` r
# Weighted response curve
rh_vals <- 0:100
weights <- .calculate_weighted_hr_wet(rh_vals, rhlim = 90)
plot(rh_vals, weights, type = "l", ylim = c(0, 1),
     xlab = "Hourly RH (%)", ylab = "Wetness Weight",
     main = "Weighted Wetness (rhlim = 90%)")
abline(v = c(80, 90), col = c("blue", "red"), lty = 2)
abline(h = c(0, 1), col = "gray", lty = 3)


# Compare unweighted vs weighted
rh_test <- c(75, 85, 90, 95)
unweighted <- as.numeric(rh_test >= 90)  # 0, 0, 1, 1
weighted <- .calculate_weighted_hr_wet(rh_test, rhlim = 90)
cbind(RH = rh_test, unweighted, weighted)
#>      RH unweighted weighted
#> [1,] 75          0      0.0
#> [2,] 85          0      0.5
#> [3,] 90          1      1.0
#> [4,] 95          1      1.0
```
