# Build inoculum schedule

Creates a vector of daily inoculum values for the simulation period
based on the specified inoculum mode and parameters.

## Usage

``` r
.build_inoculum(I0, onset, duration)
```

## Arguments

- I0:

  Either a numeric scalar (simple impulse mode) or a list containing:

  - `mode` - Character: "impulse", "uniform", "triangle", or "series"

  - `I0` - Numeric: total inoculum amount (not used for "series" mode)

  - `start_offset` - Integer: day to begin inoculum (defaults to
    `onset`)

  - `duration` - Integer: number of days for distribution (for "uniform"
    and "triangle" modes)

  - `series` - Numeric vector: custom daily inoculum values (for
    "series" mode)

- onset:

  Integer. Default day for inoculum introduction if `start_offset` not
  specified in list mode.

- duration:

  Integer. Total number of simulation days.

## Value

Numeric vector of length `duration` containing daily inoculum amounts.

## Details

**Inoculum modes:**

- **impulse**: All inoculum applied on a single day

- **uniform**: Inoculum distributed evenly over specified duration

- **triangle**: Inoculum distributed with triangular weighting (peak at
  midpoint)

- **series**: Custom daily values provided by user

If `start_offset` or `onset` falls outside \[1, duration\], a warning is
issued and no inoculum is applied (returns vector of zeros).
