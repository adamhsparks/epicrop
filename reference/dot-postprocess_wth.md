# Post-Process NASA POWER Weather Data

Reformats weather data retrieved from NASA POWER into the standard
epicrop format: renames columns, converts dates, orders columns, and
sets appropriate class attributes.

## Usage

``` r
.postprocess_wth(wth)
```

## Arguments

- wth:

  data.table from
  [`nasapower::get_power()`](https://docs.ropensci.org/nasapower/reference/get_power.html)
  result.

## Value

data.table of class `epicrop.wth`, ready for use in
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
and disease models.

## Steps

1.  Remove temporary year/month/day columns (YEAR, MM, DD)

2.  Convert YYYYMMDD to Date class using
    [`data.table::as.IDate()`](https://rdrr.io/pkg/data.table/man/IDateTime.html)

3.  Rename POWER columns to epicrop standards:

    - T2M → TEMP

    - T2M_MIN → TMIN

    - T2M_MAX → TMAX

    - RH2M → RHUM

    - PRECTOTCORR → RAIN

4.  Ensure DOY (day of year) is present

5.  Reorder columns to standard order (YYYYMMDD, DOY, TEMP, TMIN, ...)

6.  Set primary key on YYYYMMDD for efficient lookup

7.  Add `epicrop.wth` class attribute
