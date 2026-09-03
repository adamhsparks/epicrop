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

  - `"rice"` – Savary et al. (2012) parameterization

  - `"rice_modified_kim"` – Kim et al. (2015) simplifications

  - `"wheat"` – Savary et al. (2015) parameterization

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

``` r
# Example 1: Retrieve rice leaf blast parameters
params_lb <- epi_get_params("rice", "leaf_blast")
#> Error in epi_get_params("rice", "leaf_blast"): could not find function "epi_get_params"
str(params_lb)
#> Error: object 'params_lb' not found
# List of 4
# $ RcA : num [1:3, 1:2] 0 60 120 0.05 0.6 ...
# $ RcT : num [1:5, 1:2] 15 20 25 30 35 ...
# $ RRG : num 0.1
# $ RRS : num 0.01

# Example 2: Compare parameters across crop variants
params_savary <- epi_get_params("rice", "leaf_blast")
#> Error in epi_get_params("rice", "leaf_blast"): could not find function "epi_get_params"
params_kim <- epi_get_params("rice_modified_kim", "leaf_blast")
#> Error in epi_get_params("rice_modified_kim", "leaf_blast"): could not find function "epi_get_params"

# Check parameter differences
cat("Savary RcOpt would use RcT:\n")
#> Savary RcOpt would use RcT:
print(params_savary$RcT)
#> Error: object 'params_savary' not found
cat("\nKim RcT:\n")
#> 
#> Kim RcT:
print(params_kim$RcT)
#> Error: object 'params_kim' not found

# Example 3: Inspect wheat parameters
wheat_stb <- epi_get_params("wheat", "s_tritici_blotch")
#> Error in epi_get_params("wheat", "s_tritici_blotch"): could not find function "epi_get_params"
cat("Wheat STB uses RRPSN for senescence:\n")
#> Wheat STB uses RRPSN for senescence:
print(wheat_stb$RRPSN)
#> Error: object 'wheat_stb' not found

# Example 4: List all available diseases for a crop
rice_diseases <- names(.epi_params$rice)
cat("Rice diseases in database:\n")
#> Rice diseases in database:
print(rice_diseases)
#> [1] "bacterial_blight" "brown_spot"       "leaf_blast"       "sheath_blight"   
#> [5] "tungro"          

# Example 5: Programmatic parameter modification
# Get defaults, then override
params <- epi_get_params("rice", "sheath_blight")
#> Error in epi_get_params("rice", "sheath_blight"): could not find function "epi_get_params"
params$RRG <- 0.15  # Modify growth rate
#> Error: object 'params' not found
# Pass to seir() via disease function:
# sim <- sheath_blight(wth, emergence, RRG = params$RRG)
```
