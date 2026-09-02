# Build Task List for Weather Fetching

Constructs a list of (location, start_date, duration) tasks based on
input locations, dates, and pairing mode.

## Usage

``` r
.build_tasks(locations, location_names, sd_char, duration, years, mode)
```

## Arguments

- locations:

  A numeric 2-column matrix (LON, LAT).

- location_names:

  Character vector of location names.

- sd_char:

  Character vector of start dates.

- duration:

  Integer or vector of durations.

- years:

  Optional integer vector of years.

- mode:

  Pairing mode: `"pairwise"` or `"cross"`.

## Value

A list of task objects (lists) with elements:

- `location_id`: integer index

- `location_name`: character name

- `lonlat`: numeric vector c(lon, lat)

- `start_chr`: character start date

- `duration`: integer duration (days)

## Pairing Modes

- **pairwise** (default): Recycles locations and dates by position.

  - `locations[1]` × `dates[1]`, `locations[2]` × `dates[2]`, etc.

  - Excess elements are recycled.

- **cross**: Forms all combinations.

  - Every location × every date combination.
