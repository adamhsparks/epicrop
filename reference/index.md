# Package index

## Helper Functions for Rice Disease Simulations

Run
[`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md)
using pre-defined models

- [`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`bb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`bs()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`lb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`sb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`tg()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`mk_lb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`mk_sb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`leaf_rust()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`brown_rust()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`lr()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`s_tritici_blotch()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  [`stb()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
  : Rice and Wheat Disease Models

## Helper Functions for Multiple Simulations

- [`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)
  : Run an epicrop disease model across emergence dates and/or locations
- [`build_epicrop_emergence()`](https://adamhsparks.codeberg.page/epicrop/reference/build_epicrop_emergence.md)
  : Build emergence dates from year × month_day combinations
- [`audpc_dt()`](https://adamhsparks.codeberg.page/epicrop/reference/audpc_dt.md)
  : Create an AUDPC data frame for locations and emergence dates

## SEIR

A susceptible-exposed-infectious-removed (SEIR) model and the main
function that drives {epicrop}

- [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md)
  : Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework with
  Lesion Expansion

## Weather Data

Functions for fetching and formatting weather data for use in {epicrop}

- [`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
  : Get Weather Data for Modelling Disease
- [`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)
  : Fetch weather once (single or multiple locations) and split by
  calendar year
- [`format_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/format_wth.md)
  : Format Weather Data for Use in epicrop_sim() Simulations

## Miscellaneous

Other functions

- [`get_audpc()`](https://adamhsparks.codeberg.page/epicrop/reference/get_audpc.md)
  : Get the AUDPC from an epicrop_sim Object
