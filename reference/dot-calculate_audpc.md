# Calculate the Area Under the Disease Progress Curve (AUDPC)

This function is used to return the AUDPC in the output of
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).
Not to be used alone as it assumes that there is always only one day
between observations so it takes shortcuts in the calculation of the
values.

## Usage

``` r
.calculate_audpc(intensity)
```

## Arguments

- intensity:

  A `vector` of disease intensity from
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

## Value

A `numeric` value as `double`.

## References

Shaner G., and R.E. Finney, 1977. The effect of nitrogen fertilization
on the expression of slow-mildewing resistance in Knox wheat.
**Phytopathology** 67: 1051–1056.

Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia,
S.D. Silwal, S. Tolos, and K.A. Garrett, 2008. Ecology and Epidemiology
in R: Disease Progress over Time. **The Plant Health Instructor**.
[doi:10.1094/PHI-A-2008-0129-02](https://doi.org/10.1094/PHI-A-2008-0129-02)
.

Madden, L. V., G. Hughes, and F. van den Bosch. 2007. **The Study of
Plant Disease Epidemics**. American Phytopathological Society, St. Paul,
MN.
[doi:doi.org/10.1094/9780890545058](https://doi.org/doi.org/10.1094/9780890545058)
.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# get weather for IRRI Zeigler Experiment Station in wet season 2000
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)
bb <- bacterial_blight(wth, emergence = "2000-07-01")
.calculate_audpc(intensity = bb$intensity)
#> [1] 53.43405
```
