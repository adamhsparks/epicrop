# Finalise an epicrop.wth Object

Orders columns, sets the key on `YYYYMMDD` and adds the `epicrop.wth`
class. Shared by the POWER and ERA5-Land code paths so both return
identical structures.

## Usage

``` r
.as_epicrop_wth(wth)
```

## Arguments

- wth:

  data.table with epicrop-standard column names.

## Value

data.table of class `epicrop.wth`.
