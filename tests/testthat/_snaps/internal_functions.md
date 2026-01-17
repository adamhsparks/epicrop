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

