# Run an epicrop Disease Model Across Emergence Dates and Locations

A unified interface to execute any epicrop disease model
([`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
etc.) over multiple emergence dates and/or locations. Handles cross-year
and within-year weather windows automatically without global weather
binding.

## Usage

``` r
run_epicrop_model(
  model_fun,
  emergence_dates,
  wth_list,
  window_days = 120L,
  output = "audpc",
  ...
)
```

## Arguments

- model_fun:

  Function. An epicrop model function such as:
  [`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`leaf_rust()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  or
  [`s_tritici_blotch()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md).

- emergence_dates:

  Vector of emergence dates (character `"YYYY-MM-DD"`,
  [Date](https://rdrr.io/r/base/Dates.html), or
  [data.table::IDate](https://rdrr.io/pkg/data.table/man/IDateTime.html);
  must be of class `epicrop.emerge` from
  [`build_epicrop_emergence()`](https://adamhsparks.codeberg.page/epicrop/reference/build_epicrop_emergence.md)).

- wth_list:

  Named list of weather
  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)s,
  one per year.

  - **Names** must be year strings (*e.g.*, `"2001"`, `"2002"`)

  - Each table must have a `YYYYMMDD` column (Date, IDate, or ISO
    string)

  - Typically created by
    [`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)

  - Can also be an `epicrop.wth.bundle` (multiple locations)

- window_days:

  Integer. Days after emergence to simulate (default 120L). The model
  will run for exactly `window_days` rows when sufficient weather is
  available. If `emergence_dates` is at year-end, spans into next year.

- output:

  Character. Output format:

  - `"audpc"` (default): Compact summary with emergence date and AUDPC

  - `"full"`: All daily model columns plus `emergence` and `AUDPC`

- ...:

  Additional arguments forwarded to `model_fun` (*e.g.*, `RcOpt`,
  `simple_wetness`, custom modifier tables). Overrides model defaults.

## Value

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html):

- If `output = "audpc"`:

  - Columns: `emergence` (IDate), `AUDPC` (numeric)

  - One row per emergence date

  - `AUDPC = NA_real_` if weather data are insufficient

- If `output = "full"`:

  - All columns from the disease model (sites, latent, infectious, etc.)

  - Plus `emergence` (IDate) and `AUDPC` (numeric)

  - Multiple rows per emergence date (one per simulated day)

  - Class set to `epicrop.sim` for S3 methods

- If `wth_list` is a bundle (multiple locations):

  - Additional `location` column identifying source bundle member

## Workflow

This function bridges
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)
(weather prep) and
[`build_epicrop_emergence()`](https://adamhsparks.codeberg.page/epicrop/reference/build_epicrop_emergence.md)
(emergence date generation) with disease models. For each emergence
date, it:

1.  Slices weather data spanning emergence to emergence +
    `window_days - 1`

2.  Handles year boundaries (*e.g.*, Dec 15 – Jan 15)

3.  Executes the model with the sliced weather

4.  Returns either a compact AUDPC summary or full daily progression

Supports weather bundles (multiple locations) by processing each
location independently and combining results with a `location` column.

## Cross-Year Windows

When an emergence date + `window_days` spans two calendar years (*e.g.*,
Dec 15 – Feb 15), the function automatically:

- Fetches weather from both years

- Stitches them in chronological order

- Returns a contiguous window

Missing data (no weather for a required year) produces a warning and
`AUDPC = NA_real_`.

## Bundle Processing

If `wth_list` is an `epicrop.wth.bundle` (from
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)
with multiple locations or location-date pairs):

- Each bundled weather object is processed independently

- Results are combined with a `location` column

- Location labels extracted from bundle names (text before `"__"`)

## See also

- [`build_epicrop_emergence()`](https://adamhsparks.codeberg.page/epicrop/reference/build_epicrop_emergence.md)
  – Construct emergence date vectors

- [`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)
  – Prepare weather data

- [`get_audpc()`](https://adamhsparks.codeberg.page/epicrop/reference/get_audpc.md)
  – Extract/compute AUDPC from results

- [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
  – Core SEIR model

- [`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  [`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  etc. – Individual disease models

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# --- Example 1: Single location, multiple emergence dates ---

# Build emergence dates (every 15 days, June–July, years 2001–2003)
years <- 2001:2003
month_days <- c("-06-01", "-06-15", "-07-01")
emergence <- build_epicrop_emergence(years, month_days)

# Fetch weather (one year per element)
lonlat <- c(121.255669, 14.16742)
wth_list <- fetch_epicrop_weather_list(
  lonlat = lonlat,
  start_date = month_days,
  duration = 120L,
  years = years
)

# Run leaf blast model across all emergence dates
results <- run_epicrop_model(
  model_fun = leaf_blast,
  emergence_dates = emergence,
  wth_list = wth_list,
  window_days = 120L,
  output = "audpc"
)
head(results)
#> Key: <emergence, AUDPC>
#>           emergence    AUDPC
#>    <epicrop.emerge>    <num>
#> 1:       2001-06-01 38.19575
#> 2:       2001-06-15 36.86150
#> 3:       2001-07-01 43.86372
#> 4:       2002-06-01 40.34144
#> 5:       2002-06-15 38.03268
#> 6:       2002-07-01 37.42458

# --- Example 2: Full progression output for detailed analysis ---

full_results <- run_epicrop_model(
  model_fun = leaf_blast,
  emergence_dates = emergence[1:5],  # First 5 dates
  wth_list = wth_list,
  window_days = 120L,
  output = "full"
)

# Plot disease progression for first emergence date
first_date <- emergence[1]
sub <- full_results[emergence == first_date]
plot(sub$simday, sub$intensity, type = "l",
     main = paste("Leaf Blast:", first_date),
     xlab = "Days After Emergence", ylab = "Disease Intensity")


# --- Example 3: Sensitivity analysis (modify RcOpt) ---

# Susceptible cultivar
susceptible <- run_epicrop_model(
  model_fun = leaf_blast,
  emergence_dates = emergence,
  wth_list = wth_list,
  window_days = 120L,
  output = "audpc",
  RcOpt = 1.14  # Default
)
#> Error in seir(wth = wth, emergence = emergence, RcA = RcA %||% params$RcA,     RcT = RcT %||% params$RcT, RRG = RRG %||% params$RRG, RRS = RRS %||%         params$RRS, onset = 15L, duration = 120L, rhlim = rhlim %||%         90L, rainlim = rainlim %||% 5L, H0 = 600L, I0 = 1L, RcOpt = 1.14,     p = 5L, i = 20L, a = 1L, Sx = 30000L, simple_wetness = TRUE,     age_driver = "day", ...): formal argument "RcOpt" matched by multiple actual arguments

# Resistant cultivar (lower infection rate)
resistant <- run_epicrop_model(
  model_fun = leaf_blast,
  emergence_dates = emergence,
  wth_list = wth_list,
  window_days = 120L,
  output = "audpc",
  RcOpt = 0.8  # Lower
)
#> Error in seir(wth = wth, emergence = emergence, RcA = RcA %||% params$RcA,     RcT = RcT %||% params$RcT, RRG = RRG %||% params$RRG, RRS = RRS %||%         params$RRS, onset = 15L, duration = 120L, rhlim = rhlim %||%         90L, rainlim = rainlim %||% 5L, H0 = 600L, I0 = 1L, RcOpt = 1.14,     p = 5L, i = 20L, a = 1L, Sx = 30000L, simple_wetness = TRUE,     age_driver = "day", ...): formal argument "RcOpt" matched by multiple actual arguments

comparison <- data.table(
  emergence = susceptible$emergence,
  susceptible_AUDPC = susceptible$AUDPC,
  resistant_AUDPC = resistant$AUDPC
)
#> Error in data.table(emergence = susceptible$emergence, susceptible_AUDPC = susceptible$AUDPC,     resistant_AUDPC = resistant$AUDPC): could not find function "data.table"
comparison[, reduction := 1 - (resistant_AUDPC / susceptible_AUDPC)]
#> Error: object 'comparison' not found
comparison
#> Error: object 'comparison' not found

# --- Example 4: Bundle processing (multiple locations) ---

locations <- rbind(
  "IRRI" = c(121.255669, 14.16742),
  "Laguna" = c(121.2, 14.1)
)

wth_bundle <- fetch_epicrop_weather_list(
  lonlat = locations,
  start_date = month_days,
  duration = 120L,
  years = years,
  mode = "cross"  # All location × date combinations
)

bundle_results <- run_epicrop_model(
  model_fun = leaf_blast,
  emergence_dates = emergence,
  wth_list = wth_bundle,
  window_days = 120L,
  output = "audpc"
)

# Results now have a 'location' column
bundle_results[, .N, by = location]
#>    location     N
#>      <char> <int>
#> 1:     IRRI     9
#> 2:   Laguna     9
```
