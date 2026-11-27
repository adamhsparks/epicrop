# Assemble Results from Multiple Runs

Combines results from multiple model runs across all emergence dates
into a single output table. Handles `NULL` entries, deduplication (AUDPC
mode), and class assignment.

## Usage

``` r
.assemble(pieces, output)
```

## Arguments

- pieces:

  List of
  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)s.

- output:

  Character. `"audpc"` or `"full"`.

## Value

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
(or empty template if all pieces are `NULL`).

## Logic

**AUDPC mode:**

- Combines rows from each emergence date

- Deduplicates (in case of replication)

- Returns compact table (emergence, AUDPC)

**Full mode:**

- Combines all daily rows across all emergence dates and model runs

- Filters out NULL entries (failed runs due to missing weather)

- Sets class to `epicrop.sim` for S3 methods
