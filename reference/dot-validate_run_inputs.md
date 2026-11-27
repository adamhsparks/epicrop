# Validate Inputs for run_epicrop_model

Checks that `emergence_dates` and `wth_list` are the correct class and
structure for model execution.

## Usage

``` r
.validate_run_inputs(emergence_dates, wth_list)
```

## Arguments

- emergence_dates:

  Vector of emergence dates (must be class `epicrop.emerge`).

- wth_list:

  Named list of weather tables (must be `epicrop.wth.list` or
  `epicrop.wth.bundle`).

## Value

NULL (invisibly). Called for side effects (validation/abort).

## Details

Aborts with helpful messages if:

- `emergence_dates` is not of class `epicrop.emerge`

- `wth_list` is neither `epicrop.wth.list` nor `epicrop.wth.bundle`
