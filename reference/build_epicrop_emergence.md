# Build emergence dates from year × month_day combinations

Construct a vector of emergence dates by combining a set of years with a
set of month-day strings (e.g., "-06-01"), returning a sorted `IDate`
vector tagged with class `epicrop.emerge` for downstream use with
[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md).

## Usage

``` r
build_epicrop_emergence(years, month_day)
```

## Arguments

- years:

  Integer vector of years (*e.g.*, `2001:2003`).

- month_day:

  Character vector of month-day strings in the form `"-MM-DD"` (*e.g.*,
  `c("-06-01", "-06-14")`). If a string is supplied without a leading
  dash (*e.g.*, `"06-01"`), callers should normalize it in advance; this
  helper expects the leading `-` as part of the format `"YYYY-MM-DD"`.

## Value

A sorted vector of
[data.table::IDate](https://rdrr.io/pkg/data.table/man/IDateTime.html)
emergence dates with class `epicrop.emerge` (*i.e.*,
`inherits(x, "epicrop.emerge")` will be `TRUE`).

## See also

[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md),
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Basic usage: combine years with multiple month-day patterns
years <- 2001:2002
month_day <- c("-06-01", "-06-14", "-06-30")

em <- build_epicrop_emergence(years, month_day)
em
#> [1] "2001-06-01" "2001-06-14" "2001-06-30" "2002-06-01" "2002-06-14"
#> [6] "2002-06-30"

# Check class and structure
inherits(em, "epicrop.emerge")   # TRUE
#> [1] TRUE
head(em)                         # First few dates (IDate)
#> [1] "2001-06-01" "2001-06-14" "2001-06-30" "2002-06-01" "2002-06-14"
#> [6] "2002-06-30"
length(em)                       # 6 (2 years × 3 month-days)
#> [1] 6

# Typical downstream: pass to run_epicrop_model(...)
# run_epicrop_model(bacterial_blight, em, wth_list, output = "audpc")
```
