# Detect Month-Day Only Date Strings

Identifies date strings that contain only month and day without a year,
in formats like `"06-30"`, `"-06-30"`, or `"6/30"`.

## Usage

``` r
.is_md_only(x_chr)
```

## Arguments

- x_chr:

  Character vector of date strings.

## Value

Logical vector indicating which elements are month-day only.

## Patterns Matched

- `"06-30"` or `"-06-30"` (with optional leading dash)

- `"6-30"` or `"-6-30"` (single-digit month/day)

- Regex: `^-?\\d{1,2}[-/]\\d{1,2}$`

- Excludes full ISO dates (`YYYY-MM-DD`)

## Examples

``` r
.is_md_only(c("06-30", "2000-06-30", "6/30", "-06-30"))
#> Error in .is_md_only(c("06-30", "2000-06-30", "6/30", "-06-30")): could not find function ".is_md_only"
# [1]  TRUE FALSE  TRUE  TRUE
```
