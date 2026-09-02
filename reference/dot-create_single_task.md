# Create a Single Task for Weather Fetching

Constructs a single task object with resolved date and duration
information.

## Usage

``` r
.create_single_task(
  locations,
  location_names,
  loc_idx,
  sd_char,
  date_idx,
  dur,
  years
)
```

## Arguments

- locations:

  A numeric 2-column matrix (LON, LAT).

- location_names:

  Character vector of location names.

- loc_idx:

  Integer location index.

- sd_char:

  Character vector of start dates.

- date_idx:

  Integer date index.

- dur:

  Integer duration (days).

- years:

  Optional integer vector of years.

## Value

A list with task information:

- `location_id`: integer index

- `location_name`: character name

- `lonlat`: numeric vector c(lon, lat)

- `start_chr`: character start date

- `duration`: integer duration
