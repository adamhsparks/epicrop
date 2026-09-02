# Calculate Development Stage (DVS) from Thermal Time

Computes growing degree days (GDD) cumulative sum and converts to a
normalized development stage (DVS) on scale 0–2 (emergence to maturity).

## Usage

``` r
.calculate_dvs(w, thermal_time)
```

## Arguments

- w:

  Weather data with a `TEMP` column (data.frame or data.table).

- thermal_time:

  List with thermal accumulation parameters:

  - `base_temp`: Base temperature for GDD accumulation (°C)

  - `gdd_to_maturity`: Cumulative GDD required to reach DVS = 2
    (°C·days)

## Value

Numeric vector of DVS values (0–2), length = nrow(w).

## Details

**Validation:** Checks that `w` is a data.frame or data.table with a
`TEMP` column (no NAs), and `thermal_time` is a list with valid numeric
fields.

## DVS Calculation

Development stage is computed as:

    GDD = cumsum(max(TEMP - base_temp, 0))
    DVS = min(2, 2 × GDD / gdd_to_maturity)

At maturity (GDD = gdd_to_maturity), DVS reaches 2.

## Examples

``` r
# Create sample weather and thermal parameters
w <- data.frame(TEMP = c(15, 20, 25, 30, 25, 20, 15, 10))
thermal_time <- list(base_temp = 10, gdd_to_maturity = 300)
dvs <- .calculate_dvs(w, thermal_time)
head(dvs)
#> [1] 0.03333333 0.10000000 0.20000000 0.33333333 0.43333333 0.50000000
# [1] 0.0000000 0.01666667 0.05000000 0.10000000 0.15000000 0.19666667
```
