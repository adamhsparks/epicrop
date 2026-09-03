# Finalize SEIR Output Table and Attributes

Internal helper that appends calendar/spatial columns, orders output
fields, and assigns standard simulation attributes.

## Usage

``` r
.finalize_output(out, w, duration)
```

## Arguments

- out:

  Core simulation output `data.table`.

- w:

  Weather window used for simulation (contains `dates`, optionally
  `LAT`/`LON`).

- duration:

  Integer simulation length.

## Value

A finalized `data.table` of class `epicrop.sim` with:

- ordered state/flow/derived columns;

- `simday` and `dates` columns;

- optional `lat`/`lon` columns;

- `AUDPC` attribute computed from `intensity`.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
[`.calculate_audpc()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-calculate_audpc.md)
