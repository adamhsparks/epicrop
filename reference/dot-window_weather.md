# Window and Align Weather Data to Emergence Date

Adds a `dates` column from `YYYYMMDD` when absent, locates the emergence
date, checks data sufficiency for the requested simulation duration, and
returns the cropped weather table used by
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

## Usage

``` r
.window_weather(wth, emergence, duration)
```

## Arguments

- wth:

  A weather object (typically class `epicrop.wth`) containing `YYYYMMDD`
  and other required meteorological fields.

- emergence:

  Character date (`"YYYY-MM-DD"`) passed to
  [`.validate_emergence()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-validate_emergence.md).

- duration:

  Integer simulation length in days.

## Value

A `data.table` with exactly `duration` rows, starting on the emergence
date.

## Details

Internal helper that slices weather data to the simulation interval
beginning at emergence and spanning `duration` days.

## Warnings

Emits a warning when `duration < 50` days because short runs may produce
unrealistic epidemic dynamics.

## Error Conditions

Aborts when:

- emergence date is not present in weather records;

- there are not enough rows after emergence to cover `duration`.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
[`.validate_emergence()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-validate_emergence.md)
