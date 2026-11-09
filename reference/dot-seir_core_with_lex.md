# SEIR Core Simulation (With Lesion Expansion)

Internal workhorse for SEIR model with lesion expansion enabled. Extends
the standard SEIR model by adding a direct H→I pathway, allowing healthy
sites to become infectious without passing through the latent stage.

## Usage

``` r
.seir_core_with_lex(
  duration,
  onset_idx,
  H0,
  Iprim,
  Rc,
  p_days,
  i_days,
  RRG_vals,
  RRS_vals,
  RRLEX_vals,
  a_vals,
  Sx
)
```

## Arguments

- duration:

  Simulation duration (integer days).

- onset_idx:

  1-indexed day of primary inoculum injection (integer).

- H0:

  Initial healthy sites (integer).

- Iprim:

  Vector of primary inoculum injection on each day (numeric).

- Rc:

  Daily infection rate (numeric vector, length = duration).

- p_days:

  Daily latent period (numeric vector, length = duration).

- i_days:

  Daily infectious period (numeric vector, length = duration).

- RRG_vals:

  Daily relative growth rate (numeric vector).

- RRS_vals:

  Daily relative senescence rate (numeric vector).

- RRLEX_vals:

  Daily lesion expansion rate (numeric vector).

- a_vals:

  Daily aggregation coefficient (numeric vector, \>= 1).

- Sx:

  Maximum site capacity (integer).

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns:

- `sites`, `latent`, `infectious`, `removed`, `senesced`: state vars

- `rateinf`, `rlex`, `rtransfer`, `rremoved`, `rgrowth`, `rsenesced`:
  flow rates

- `diseased`, `intensity`: derived

## Details

**Lesion Expansion (rlex):** On day t, computed as:

    rlex[t] = RRLEX_vals[t] × now_infectious[t-from-stage-A] × COFR

where COFR = 1 - (diseased / total_living). This reduces direct lesion
expansion in crowded conditions, similar to *Rc*-driven infection.

## Algorithm

Uses a two-stage update to avoid circular dependency:

1.  Compute windowed infectious sites before today's lesion expansion

2.  Compute rlex from pre-expansion infectious count and crowding factor

3.  Finalise infectious window including today's lesion expansion

This ensures that *Rc*-driven infections and lesion expansion are
properly sequenced and both respond to the same crowding factor.
