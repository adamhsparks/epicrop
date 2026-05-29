# Cross‑year weather slicer (assumes `epicrop.wth.list` contract)

Returns a function that slices per-year weather tables using the IDate
column `YYYYMMDD`. No format parsing or coercion is performed—this
trusts the S3 class contract that tables are well-formed.

## Usage

``` r
.make_slicer(wth_list, window_days)
```

## Arguments

- wth_list:

  An `epicrop.wth.list` (named by year strings).

- window_days:

  Integer number of days after emergence to include (inclusive).

## Value

A function(ed) -\> data.table or NULL
