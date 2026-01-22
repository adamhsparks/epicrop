# Assemble output data.table

Constructs the final simulation output data.table with all daily state
variables, dates, and calculated summary statistics.

## Usage

``` r
.assemble_output(core, run_dates, duration, wth)
```

## Arguments

- core:

  A list containing daily vectors of simulation state variables:

  - `sites` - Number of infection sites

  - `latent` - Latent (infected but not infectious) sites

  - `infectious` - Infectious sites

  - `removed` - Removed sites

  - `senesced` - Senesced sites

  - `rateinf` - Rate of infection

  - `rlex` - Rate of latent expansion

  - `rtransfer` - Rate of transfer (latent to infectious)

  - `rremoved` - Rate of removal

  - `rgrowth` - Rate of growth

  - `rsenesced` - Rate of senescence

  - `diseased` - Total diseased sites

  - `intensity` - Disease intensity

- run_dates:

  A Date vector of simulation dates.

- duration:

  Integer. Number of simulation days.

- wth:

  A data.table containing weather data with optional `LAT` and `LON`
  columns for location coordinates.

## Value

A data.tableof class `c("epicrop.sim", "data.table", "data.frame")`
containing:

- `simday` - Sequential day number (1 to duration)

- `dates` - Calendar dates

- All state variables from `core`

- `lat` and `lon` columns if available in weather data

- `AUDPC` attribute containing Area Under Disease Progress Curve

## Details

The AUDPC (Area Under Disease Progress Curve) is calculated from disease
intensity and stored as an attribute of the returned data.table.
