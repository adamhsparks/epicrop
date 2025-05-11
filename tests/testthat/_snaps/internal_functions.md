# non numeric values are not accepted

    Code
      .check_inputs(var_args)
    Condition
      Error:
      ! `onset` has a value of `twenty` and must be a numeric value.

# Formatting fails if weather are not daily values

    Code
      .check_inputs(var_args)
    Condition
      Error:
      ! `simple_wetness` must be a Boolean (`TRUE/FALSE`) value.

# Failure if `wth` is not of class `epicrop.wth`

    Code
      .check_inputs(var_args)
    Condition
      Error:
      ! `wth` must be an object of epicrop.wth.  You can create your `wth` object using either, `get_wth()` to fetch weather data from the NASA POWER API or format your own with `format_wth()`.

