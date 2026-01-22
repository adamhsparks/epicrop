# Calculate development stage (DVS)

Computes the crop development stage based on accumulated thermal time
(growing degree days). DVS ranges from 0 (emergence) to 2 (physiological
maturity).

## Usage

``` r
.calculate_dvs(w, thermal_time)
```

## Arguments

- w:

  A data.table containing weather data with a `TEMP` column.

- thermal_time:

  A list containing:

  - `base_temp` - Base temperature for degree day calculation

  - `gdd_to_maturity` - Total growing degree days required to reach
    maturity

## Value

Numeric vector of daily DVS values, ranging from 0 to 2, where:

- 0 = emergence

- 1 = flowering/anthesis

- 2 = physiological maturity

## Details

Growing degree days (GDD) are calculated as the cumulative sum of daily
thermal time, where daily thermal time = max(TEMP - base_temp, 0). DVS
is then calculated as 2 \* GDD / gdd_to_maturity, capped at 2.
