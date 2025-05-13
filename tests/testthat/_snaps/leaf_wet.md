# weighted_wetness stops if `rhlim < 80`

    Code
      .calculate_leaf_wetness(weighted_wetness = TRUE, wth = wth_formatted, rhlim = 70,
        rainlim = 5)
    Condition
      Error in `lapply()`:
      ! You have set your `rhlim` below 80%. This value must be between `80` and `100` to properly work with weighted hourly leaf wetness.

