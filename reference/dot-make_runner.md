# Create a per‑emergence model‑execution function

Ensures we only auto‑inject `duration` when the user has *not* supplied
it (including unique partial matches or positional args).

## Usage

``` r
.make_runner(model_fun, output, slicer, window_days, ...)
```

## Arguments

- model_fun:

  epicrop model function to call.

- output:

  "audpc" or "full".

- slicer:

  Slicer function producing a weather slice for an emergence date.

- window_days:

  Integer; passed for reference only.

- ...:

  Extra args forwarded to `model_fun`.

## Value

A function(ed) -\> data.table (summary or full)
