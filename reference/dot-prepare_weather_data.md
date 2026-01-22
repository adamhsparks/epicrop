# Prepare weather data for simulation

Validates and prepares weather data for use in disease simulation.
Ensures required columns exist, handles missing values appropriately,
and orders data chronologically.

## Usage

``` r
.prepare_weather_data(wth, duration)
```

## Arguments

- wth:

  A data.frame or data.table containing weather data with columns:

  - `YYYYMMDD` - Date in YYYYMMDD format

  - `TEMP` - Daily temperature (required, no NAs allowed)

  - `RAIN` - Daily rainfall (NAs treated as 0)

  - `RHUM` - Relative humidity (optional, NAs allowed)

- duration:

  Integer. Number of days for the simulation period.

## Value

A data.tablewith validated and ordered weather data, ensuring `RHUM`
column exists (filled with NA if not provided).

## Details

The function performs the following operations:

- Converts input to data.table if necessary

- Adds `RHUM` column if missing (filled with NA)

- Validates that `TEMP` contains no missing values

- Replaces NA values in `RAIN` with 0 (assumes no rainfall)

- Orders data by `YYYYMMDD` in ascending order
