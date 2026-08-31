# Retrieve internal parameter set for a crop/disease combination

Retrieve internal parameter set for a crop/disease combination

## Usage

``` r
epi_get_params(crop, disease)
```

## Arguments

- crop:

  Character. One of `names(.epi_params)`, e.g. "rice",
  "rice_modified_kim", "wheat".

- disease:

  Character. One of `names(.epi_params[[crop]])`.

## Value

A named list of parameters (`RcA`, `RcT`, `RRG`, and either `RRS` or
`RRPSN` depending on crop). Matrix-valued entries are 2-column (`x`,
`y`) interpolation tables; scalar entries are constants.
