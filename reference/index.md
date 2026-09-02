# Package index

## Helper Functions for Rice Disease Simulations

Run
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
using pre-defined models

- [`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`leaf_rust()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`s_tritici_blotch()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`bb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`bs()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`lb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`sb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`tg()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`mk_lb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`mk_sb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`lr()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`brown_rust()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`stb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  : Rice and Wheat Disease Models

## Helper Functions for Multiple Simulations

- [`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)
  : Run an epicrop Disease Model Across Emergence Dates and Locations
- [`build_epicrop_emergence()`](https://adamhsparks.codeberg.page/epicrop/reference/build_epicrop_emergence.md)
  : Build Emergence Dates from Year and Month-Day Combinations
- [`audpc_dt()`](https://adamhsparks.codeberg.page/epicrop/reference/audpc_dt.md)
  : Extract AUDPC Summary as data.table

## SEIR

A susceptible-exposed-infectious-removed (SEIR) model and the main
function that drives {epicrop}

- [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
  : Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework with
  Lesion Expansion

## Weather Data

Functions for fetching and formatting weather data for use in {epicrop}

- [`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
  : Get Weather Data for Disease Modelling
- [`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)
  : Fetch Weather for One or More Locations and Split by Calendar Year
- [`format_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/format_wth.md)
  : Format Weather Data for Use in seir() Simulations

## Miscellaneous

Other functions

- [`get_audpc()`](https://adamhsparks.codeberg.page/epicrop/reference/get_audpc.md)
  : Extract AUDPC Value from Disease Model Output
