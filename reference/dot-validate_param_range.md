# Validate Parameter Range

Validate Parameter Range

## Usage

``` r
.validate_param_range(
  value,
  name,
  min = NULL,
  max = NULL,
  exclusive_min = FALSE,
  exclusive_max = FALSE
)
```

## Arguments

- value:

  Parameter value

- name:

  Parameter name

- min:

  Minimum value (inclusive unless exclusive_min = TRUE)

- max:

  Maximum value (inclusive unless exclusive_max = TRUE)

- exclusive_min:

  If TRUE, value must be \> min (not \>=)

- exclusive_max:

  If TRUE, value must be \< max (not \<=)
