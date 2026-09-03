# Extract AUDPC Summary as data.table

Extracts the Area Under Disease Progress Curve (AUDPC) from an epicrop
simulation output along with spatial (lat/lon) and temporal (emergence)
metadata. Returns a compact summary table suitable for analysis,
mapping, or reporting.

## Usage

``` r
audpc_dt(x)
```

## Arguments

- x:

  An `epicrop.sim` object from
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
  disease model functions
  ([`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  etc.), or
  [`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)
  with `output = "full"`.

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with one row containing:

- `lat`: Latitude (double; NA if not available)

- `lon`: Longitude (double; NA if not available)

- `AUDPC`: Area under disease progress curve (double)

- `emergence`: Emergence date (character, "YYYY-MM-DD")

Table is keyed on `emergence` for efficient operations.

## Output Structure

The returned
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
includes:

- **lat**: Latitude (if available in the model object)

- **lon**: Longitude (if available in the model object)

- **AUDPC**: Area under disease progress curve (numeric)

- **emergence**: Emergence date as character (YYYY-MM-DD)

Columns are present only if the corresponding data exist in the input
object. The table is keyed on `emergence` for efficient sorting and
joining.

## Use Cases

- **Batch processing:** Quickly summarize results from
  [`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)

- **Mapping:** Combine with spatial data for disease risk mapping

- **Reporting:** Generate summary tables for publications or extension

- **Analysis:** Join with cultivar, management, or climate data

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Example 1: Extract AUDPC summary from single model run
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = "2000-06-30",
  duration = 120L
)

sim <- bacterial_blight(wth, emergence = "2000-06-30")
#> Error in epi_get_params("rice", "bacterial_blight"): could not find function "epi_get_params"
summary_table <- audpc_dt(sim)
#> Error: object 'sim' not found
print(summary_table)
#> Error: object 'summary_table' not found
# Output includes lat, lon, AUDPC, emergence

# Example 2: Batch summarize multiple emergence dates
emergence_dates <- seq(as.Date("2000-06-15"), as.Date("2000-07-15"), by = 7)
summaries <- lapply(emergence_dates, function(ed) {
  sim <- bacterial_blight(wth, emergence = as.character(ed))
  audpc_dt(sim)
})
#> Error in epi_get_params("rice", "bacterial_blight"): could not find function "epi_get_params"
summary_all <- data.table::rbindlist(summaries)
#> Error: object 'summaries' not found
print(summary_all)
#> Error: object 'summary_all' not found

# Example 3: Compare multiple locations and emergence dates
locations <- data.frame(
  name = c("IRRI", "Manila"),
  lon = c(121.25562, 120.985),
  lat = c(14.6774, 14.6042)
)

emergence_dates <- c("2000-06-30", "2000-07-15")

results <- expand.grid(
  location = locations$name,
  emergence = emergence_dates,
  stringsAsFactors = FALSE
)

results$AUDPC <- mapply(function(loc_name, em_date) {
  loc <- locations[locations$name == loc_name, ]
  wth <- get_wth(
    lonlat = c(loc$lon, loc$lat),
    dates = em_date,
    duration = 120L
  )
  sim <- leaf_blast(wth, emergence = em_date)
  get_audpc(sim)
}, results$location, results$emergence)
#> Error in epi_get_params("rice", "leaf_blast"): could not find function "epi_get_params"

print(results)
#> # Data frame like object (class data.frame) 2 x 4:
#>  │location│emergence 
#>  │<chr>   │<chr>     
#> 1│IRRI    │2000-06-30
#> 2│Manila  │2000-06-30
#> 3│IRRI    │2000-07-15
#> 4│Manila  │2000-07-15
# Identify highest risk scenario
worst <- results[which.max(results$AUDPC), ]
sprintf("Highest risk: %s, emergence %s, AUDPC = %.1f",
        worst$location, worst$emergence, worst$AUDPC)
#> character(0)

# Example 4: Export for mapping or further analysis
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = "2000-06-30",
  duration = 120L
)
sim <- brown_spot(wth, emergence = "2000-06-30")
#> Error in epi_get_params("rice", "brown_spot"): could not find function "epi_get_params"
summary_dt <- audpc_dt(sim)
#> Error: object 'sim' not found
```
