# Assemble model outputs into final result tables

Combines per-emergence outputs into the requested shape. For "audpc",
returns unique rows by (emergence, AUDPC).

## Usage

``` r
.assemble_results(pieces, output)
```

## Arguments

- pieces:

  List of per-emergence outputs.

- output:

  "audpc" or "full".

## Value

data.table
