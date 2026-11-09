# SEIR Core Simulation (No Lesion Expansion)

Internal workhorse for SEIR model without lesion expansion. Implements
the standard Susceptible-Exposed-Infectious-Removed compartmental model
using efficient cumulative sum techniques to avoid O(n²) nested loops.

## Usage

``` r
.seir_core_no_lex(
  duration,
  onset_idx,
  H0,
  Iprim,
  Rc,
  p_days,
  i_days,
  RRG_vals,
  RRS_vals,
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

**rlex** is always 0 in this variant (no lesion expansion).

**Cumulative Sum Optimisation:** Instead of summing over a sliding
window (latent/infectious durations), this implementation maintains
`cs_lat` and `cs_inf` (cumulative sums of latency inflows and infectious
inflows). The count of sites currently infectious is:
`cs_inf[t] - cs_inf[t - i_days[t] - 2]`, avoiding O(n) inner loops.

## Algorithm

Computes daily transitions using lagged cumulative sums to efficiently
extract windowed counts (infectious periods are handled via integer
delays into history arrays). Day 1 initialises pools; main loop (days
2+) updates compartments based on infection rate, latency exit, and
recovery.
