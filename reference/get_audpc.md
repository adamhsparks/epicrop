# Extract AUDPC Value from Disease Model Output

Retrieves the pre-calculated Area Under the Disease Progress Curve
(AUDPC) from an `epicrop.sim` object. AUDPC is automatically computed by
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
and stored as an object attribute.

## Usage

``` r
get_audpc(x)
```

## Arguments

- x:

  An `epicrop.sim` object returned by
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
  [`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  or other disease model functions.

## Value

Numeric AUDPC value (double).

## Purpose

This function provides a safe, documented way to access AUDPC without
directly querying attributes. It includes validation to ensure the
object is a valid model result with a calculated AUDPC.

## AUDPC Interpretation

AUDPC values:

- Near 0: Minimal disease throughout season (resistant or unfavorable
  conditions)

- Mid-range: Moderate disease development

- Near duration: Severe, sustained disease pressure

Typical ranges depend on disease and duration:

- 120-day rice season: 0-120 (perfect health to constant epidemic)

- 240-day wheat season: 0-240

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Example 1: Basic usage - extract AUDPC from model output
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = "2000-06-30",
  duration = 120L
)

sim <- bacterial_blight(wth, emergence = "2000-06-30")
audpc <- get_audpc(sim)
print(paste("Bacterial blight AUDPC:", round(audpc, 2)))
#> [1] "Bacterial blight AUDPC: 37.74"

# Example 2: Compare AUDPC across multiple diseases
diseases <- list(
  bb = bacterial_blight(wth, emergence = "2000-06-30"),
  bs = brown_spot(wth, emergence = "2000-06-30"),
  lb = leaf_blast(wth, emergence = "2000-06-30"),
  sb = sheath_blight(wth, emergence = "2000-06-30")
)

audpc_values <- data.frame(
  disease = names(diseases),
  AUDPC = sapply(diseases, get_audpc)
)
print(audpc_values)
#>    disease      AUDPC
#> bb      bb 37.7406263
#> bs      bs  0.9795647
#> lb      lb 40.7539326
#> sb      sb 29.0220116
# Identify most damaging disease
worst <- audpc_values[which.max(audpc_values$AUDPC), ]
print(paste("Most severe:", worst$disease, "AUDPC =", round(worst$AUDPC, 1)))
#> [1] "Most severe: lb AUDPC = 40.8"

# Example 3: Use in analysis pipeline
locations <- list(
  IRRI = c(121.25562, 14.6774),
  Manila = c(120.985, 14.6042)
)

audpc_by_location <- sapply(locations, function(lonlat) {
  wth <- get_wth(lonlat = lonlat, dates = "2000-06-30", duration = 120L)
  sim <- leaf_blast(wth, emergence = "2000-06-30")
  get_audpc(sim)
})

print(audpc_by_location)
#>     IRRI   Manila 
#> 40.75393 40.75393 
```
