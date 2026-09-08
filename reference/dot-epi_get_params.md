# Retrieve Internal Parameter Set for a Crop/Disease Combination

Looks up pre-calibrated SEIR model parameters from the internal
parameter database for a given crop and disease. Used by
disease-specific helper functions
([`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
etc.) to avoid hardcoding published parameter values.

## Usage

``` r
.epi_get_params(crop, disease)
```

## Arguments

- crop:

  Character. Crop name (case-insensitive). One of:

  - `"rice"` – Savary *et al.* (2012) parameterization

  - `"rice_modified_kim"` – Kim *et al.* (2015) simplifications

  - `"wheat"` – Savary *et al.* (2015) parameterization

- disease:

  Character. Disease name (case-insensitive). Must be a registered
  disease for the specified crop. Examples:

  - Rice: `"bacterial_blight"`, `"brown_spot"`, `"leaf_blast"`,
    `"sheath_blight"`, `"tungro"`

  - Wheat: `"leaf_rust"`, `"s_tritici_blotch"`

## Value

A named list of disease-specific SEIR parameters:

- `RcA`: Age modifier (matrix or scalar)

- `RcT`: Temperature modifier (matrix or scalar)

- `RRG`: Relative growth rate (numeric)

- `RRS` or `RRPSN`: Relative senescence rate (numeric)

Matrix-valued entries are 2-column interpolation tables suitable for
[`.interp()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-interp.md).
Scalar entries are constants applied uniformly.

## Parameter Database

The internal `.epi_params` list is organized hierarchically:

    .epi_params
      $rice
        $bacterial_blight -> list(RcA, RcT, RRG, RRS)
        $brown_spot       -> list(RcA, RcT, RRG, RRS)
        $leaf_blast       -> list(RcA, RcT, RRG, RRS)
        ...
      $rice_modified_kim
        $leaf_blast       -> list(RcA, RcT, RRG, RRS)
        $sheath_blight    -> list(RcA, RcT, RRG, RRS)
      $wheat
        $leaf_rust        -> list(RcA, RcT, RRG, RRPSN)
        $s_tritici_blotch -> list(RcA, RcT, RRG, RRPSN)

## Parameter Contents

Each parameter set is a named list containing:

- **RcA**: Age-based infection rate modifier (2-column matrix or
  numeric)

- **RcT**: Temperature-based infection rate modifier (2-column matrix or
  numeric)

- **RRG**: Relative growth rate (numeric scalar or vector)

- **RRS** (rice) or **RRPSN** (wheat): Relative senescence rate (numeric
  scalar or vector)

Matrix values use interpolation format: Column 1 is the x-axis
(age/temp), Column 2 is the y-axis (modifier 0-1).

## Typical Use

Not normally called directly. Instead, use disease-specific functions:

    # This internally calls epi_get_params("rice", "leaf_blast")
    sim <- leaf_blast(wth, emergence = "2000-06-30")

    # To override defaults, pass parameters directly:
    sim <- leaf_blast(wth, emergence = "2000-06-30", RcOpt = 0.8)

Direct use is only necessary when implementing new disease functions or
modifying parameter sets programmatically.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples
