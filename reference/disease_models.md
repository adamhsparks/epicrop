# Rice and Wheat Disease Models

Helper functions that provide sensible defaults for disease-specific
simulations using
[`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

## Usage

``` r
bacterial_blight(wth, emergence, ...)

bb(wth, emergence, ...)

brown_spot(wth, emergence, ...)

bs(wth, emergence, ...)

leaf_blast(wth, emergence, ...)

lb(wth, emergence, ...)

sheath_blight(wth, emergence, ...)

sb(wth, emergence, ...)

tungro(wth, emergence, ...)

tg(wth, emergence, ...)

modified_kim_leaf_blast(wth, emergence, ...)

mk_lb(wth, emergence, ...)

modified_kim_sheath_blight(wth, emergence, ...)

mk_sb(wth, emergence, ...)

leaf_rust(wth, emergence, ...)

brown_rust(wth, emergence, ...)

lr(wth, emergence, ...)

s_tritici_blotch(wth, emergence, ...)

stb(wth, emergence, ...)
```

## Arguments

- wth:

  Weather data with a daily time-step, normally NASA POWER data from
  [`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md),
  but any [`base::data.frame()`](https://rdrr.io/r/base/data.frame.html)
  object that has been formatted using
  [`format_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/format_wth.md)
  will work.

- emergence:

  Expected date of crop emergence in `YYYY-MM-DD` format.

- ...:

  Additional arguments passed to
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

## Value

A
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
of disease intensity and infection sites. See
[`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md)
for a full description of the column values.

## Available Models

**Rice Diseases (Savary et al. 2012)**:

- `bacterial_blight()` / `bb()` - Bacterial blight (*Xanthomonas oryzae*
  pv. *oryzae*)

- `brown_spot()` / `bs()` - Brown spot (*Cochliobolus miyabeanus*)

- `leaf_blast()` / `lb()` - Leaf blast (*Magnaporthe oryzae*)

- `sheath_blight()` / `sb()` - Sheath blight (*Rhizoctonia solani*)

- `tungro()` / `tg()` - Tungro (*Rice Tungro Spherical Virus* and *Rice
  Tungro Bacilliform Virus*)

**Rice Diseases (Kim et al. 2015)**:

- `modified_kim_leaf_blast()` / `mk_lb()` - Modified leaf blast model

- `modified_kim_sheath_blight()` / `mk_sb()` - Modified sheath blight
  model

**Wheat Diseases (Savary et al. 2015)**:

- `leaf_rust()` / `brown_rust()` / `lr()` - Leaf/brown rust (*Puccinia
  triticina*)

- `s_tritici_blotch()` / `stb()` - Septoria tritici blotch
  (*Zymoseptoria tritici*)

## References

Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
(2012). Modeling and mapping potential epidemics of rice diseases
globally. *Crop Protection*, 34: 6-17.
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)

Kim, K. H., Cho, J., Lee, Y. H., & Lee, W. S. (2015). Predicting
potential epidemics of rice leaf blast and sheath blight in South Korea
*Agricultural and Forest Meteorology*, 203: 191-207.
[doi:10.1016/j.agrformet.
2015.01.011](https://doi.org/10.1016/j.agrformet.%202015.01.011)

Savary, S., Stetkiewicz, S., Brun, F., & Willocquet, L. (2015).
Modelling and mapping potential epidemics of wheat diseases. *European
Journal of Plant Pathology*, 142:771-790.
[doi:10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7)

## Author

Adam H. Sparks, <adamhsparks@gmail.com>
