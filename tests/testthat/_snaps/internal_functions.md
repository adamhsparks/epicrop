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

# Failure if `duration` is <50

    Code
      .check_inputs(var_args)
    Condition
      Error in `cli::cli_alert_danger()`:
      ! unused argument (call = rlang::caller_env())

# Failure if `rhlim` is >100

    Code
      .check_inputs(var_args)
    Condition
      Error:
      ! The value for `rhlim`, {rhlim}, is invalid. Valid values are any value between or equal to 0 to 100.

# Warning if `duration` is > max RcA days

    Code
      .check_inputs(var_args)
    Condition
      Error:
      ! `RcA` has a duration, {max_rca_day} that is less than `duration`, {duration} days. Please provide a matrix of values that aligns with `duration`.

# Warning if `duration` is < max RcA days

    Code
      .check_inputs(var_args)
    Condition
      Error in `cli::cli_alert_info()`:
      ! unused argument (call = rlang::caller_env())

# Failure if TMIN/TMAX not supplied with `simple_wetness = FALSE`

    Code
      .check_inputs(var_args)
    Condition
      Error:
      ! You have set `simple_wetness` to `TRUE` but you do not have 'TMIN' and 'TMAX' columns in your weather data. Please set `simple_wetness` to `FALSE` or provide 'TMIN' and 'TMAX' columns in your `wth` data.

# Failure if object should be date but cannot be formatted as such

    Code
      .check_date(x = "string")
    Condition
      Error in `value[[3L]]()`:
      ! string is not in a valid date format. Please enter a valid date format.

# Vectors of establishment dates only use the first

    Code
      .check_emergence_date(x = c("2000-06-01", "2000-06-02"))
    Message
      ! Only one day is used for emergence. Only the first value will be used.

