# Calculate Area Under the Disease Progress Curve (AUDPC)

Computes the Area Under the Disease Progress Curve (AUDPC) from daily
disease intensity values using the trapezoidal rule. This function is
designed to work with
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
output where observations are equally spaced (one day apart).

## Usage

``` r
.calculate_audpc(intensity)
```

## Arguments

- intensity:

  Numeric vector of daily disease intensity values (typically from
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
  output). Values should be bounded \[0, 1\] where 0 = no disease, 1 =
  complete infection. Length \>= 2 required.

## Value

Numeric value (double) representing the area under the disease progress
curve. Returns 0 if `intensity` has fewer than 2 observations.

## AUDPC Details

AUDPC is a standard epidemiological metric summarizing cumulative
disease intensity over time. It:

- Ranges from 0 (no disease) to duration (maximum disease throughout)

- Accounts for both disease severity and duration

- Enables comparison across different growing seasons, locations, or
  cultivars

- Is commonly used in cultivar resistance screening and management
  trials

The trapezoidal rule approximates the area under the curve:

    AUDPC = sum((intensity[i] + intensity[i+1]) / 2) for all consecutive days

## When to Use

- **Direct use (rare):** Only when you have a vector of daily intensity
  values from
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
  and want the AUDPC

- **Typical use:** Access via
  [`get_audpc()`](https://adamhsparks.codeberg.page/epicrop/reference/get_audpc.md)
  or
  [`audpc_dt()`](https://adamhsparks.codeberg.page/epicrop/reference/audpc_dt.md)
  on model output (AUDPC is automatically calculated and stored as an
  attribute)

## References

Shaner, G., and R. E. Finney. 1977. The effect of nitrogen fertilization
on the expression of slow-mildewing resistance in Knox wheat.
*Phytopathology*, 67: 1051-1056.

Sparks, A. H., P. D. Esker, M. Bates, W. Dall'Acqua, Z. Guo, V. Segovia,
S. D. Silwal, S. Tolos, and K. A. Garrett. 2008. Ecology and
Epidemiology in R: Disease Progress over Time. *The Plant Health
Instructor*.
[doi:10.1094/PHI-A-2008-0129-02](https://doi.org/10.1094/PHI-A-2008-0129-02)
.

Madden, L. V., G. Hughes, and F. van den Bosch. 2007. *The Study of
Plant Disease Epidemics*. American Phytopathological Society, St. Paul,
MN. [doi:10.1094/9780890545058](https://doi.org/10.1094/9780890545058) .

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Example 1: Fetch weather and run disease model
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = "2000-06-30",
  duration = 120L
)

# Run bacterial blight model
bb_sim <- bacterial_blight(wth, emergence = "2000-06-30")

# Calculate AUDPC from intensity vector (already in object attribute)
audpc_value <- .calculate_audpc(intensity = bb_sim$intensity)
#> Error in .calculate_audpc(intensity = bb_sim$intensity): could not find function ".calculate_audpc"
print(audpc_value)
#> Error: object 'audpc_value' not found

# Compare with automatic calculation via get_audpc()
audpc_attr <- get_audpc(bb_sim)
all.equal(audpc_value, audpc_attr)  # Should be TRUE
#> Error: object 'audpc_value' not found

# Example 2: Effect of disease severity on AUDPC
# Mild disease (slow progression)
mild <- seq(0, 0.3, length.out = 120)
audpc_mild <- .calculate_audpc(mild)
#> Error in .calculate_audpc(mild): could not find function ".calculate_audpc"

# Severe disease (rapid progression)
severe <- seq(0, 1.0, length.out = 120)
audpc_severe <- .calculate_audpc(severe)
#> Error in .calculate_audpc(severe): could not find function ".calculate_audpc"

cbind(Scenario = c("Mild", "Severe"),
      AUDPC = c(audpc_mild, audpc_severe))
#> Error: object 'audpc_mild' not found

# Example 3: Compare resistant vs susceptible cultivars
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = "2000-06-30",
  duration = 120L
)

# Susceptible cultivar (default parameters)
susceptible <- leaf_blast(wth, emergence = "2000-06-30")
audpc_sus <- get_audpc(susceptible)

# Resistant cultivar (lower infection rate)
resistant <- leaf_blast(wth, emergence = "2000-06-30", RcOpt = 0.8)
#> Error in seir(wth = wth, emergence = emergence, RcA = RcA %||% params$RcA,     RcT = RcT %||% params$RcT, RRG = RRG %||% params$RRG, RRS = RRS %||%         params$RRS, onset = 15L, duration = 120L, rhlim = rhlim %||%         90L, rainlim = rainlim %||% 5L, H0 = 600L, I0 = 1L, RcOpt = 1.14,     p = 5L, i = 20L, a = 1L, Sx = 30000L, simple_wetness = TRUE,     age_driver = "day", ...): formal argument "RcOpt" matched by multiple actual arguments
audpc_res <- get_audpc(resistant)
#> Error: object 'resistant' not found

# Calculate resistance benefit
reduction <- (audpc_sus - audpc_res) / audpc_sus * 100
#> Error: object 'audpc_res' not found
sprintf("Resistant cultivar reduces AUDPC by %.1f%%", reduction)
#> Error: object 'reduction' not found
```
