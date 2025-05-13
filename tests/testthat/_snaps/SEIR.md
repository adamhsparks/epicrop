# `seir()` switches to the complex leaf wetness calculation

    Code
      seir(wth = wth_formatted, emergence = "2000-07-01", onset = 20, duration = 120,
        rhlim = 90, rainlim = 5, RcA = RcA, RcT = RcT, I0 = 1, RcOpt = 0.61, p = 6,
        i = 19, H0 = 600, a = 1, Sx = 100000, RRS = 0.01, RRG = 0.1, RcW = NULL,
        simple_wetness = FALSE)
    Condition
      Error in `seir()`:
      ! You have set `simple_wetness` to `TRUE` but you have not provided a matrix of weighted values for `RcW`

