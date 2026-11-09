# Normalise Month-Day Strings to Standard MM-DD Format

Converts month-day strings in various formats (`"6/30"`, `"-06-30"`,
etc.) to a consistent `"MM-DD"` format with zero-padded digits.

## Usage

``` r
.normalize_md(x_chr)
```

## Arguments

- x_chr:

  Character vector of month-day strings (with optional leading dash or
  slash separator).

## Value

Character vector in `"MM-DD"` format (*e.g.*, `"06-30"`).

## Transformations

- Slashes or dashes as separators (normalised to dashes)

- Optional leading dash (removed)

- Single or double-digit month/day (zero-padded)

## Examples

``` r
.normalize_md(c("6/30", "-6-30", "06-30"))
#> Error in .normalize_md(c("6/30", "-6-30", "06-30")): could not find function ".normalize_md"
# [1] "06-30" "06-30" "06-30"
```
