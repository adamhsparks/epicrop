test_that(".check_inputs() stops if values aren't numeric", {
  expect_error(
    .check_inputs(
      wth = wth_formatted,
      emergence = "emergence",
      onset = 30L,
      duration = 120L,
      rhlim = 90L,
      rainlim = 5L,
      H0 = 25L,
      I0 = 1L,
      RcA = cbind(
        c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
        c(
          0.84,
          0.84,
          0.84,
          0.84,
          0.84,
          0.84,
          0.84,
          0.88,
          0.88,
          1.0,
          1.0,
          1.0,
          1.0
        )
      ),
      RcT = cbind(
        c(12L, 16L, 20L, 24L, 28L, 32L, 36L, 40L),
        c(0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0)
      ),
      RcW = cbind(
        c(8L, 9L, 12L, 15L, 18L, 21L, 24L),
        c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1.0)
      ),
      RcOpt = 0.46,
      p = 3L,
      i = 120L,
      Sx = 800L,
      a = 2.8,
      RRS = 0.005,
      RRG = 0.2
    ),
    regexp = "`emergence` has a value of `emergence` and must be a numeric value."
  )
})


test_that(".check_inputs() stops if values are <0", {
  expect_error(
    .check_inputs(
      wth = wth_formatted,
      emergence = 5,
      onset = 30L,
      duration = -120L,
      rhlim = 90L,
      rainlim = 5L,
      H0 = 25L,
      I0 = 1L,
      RcA = cbind(
        c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
        c(
          0.84,
          0.84,
          0.84,
          0.84,
          0.84,
          0.84,
          0.84,
          0.88,
          0.88,
          1.0,
          1.0,
          1.0,
          1.0
        )
      ),
      RcT = cbind(
        c(12L, 16L, 20L, 24L, 28L, 32L, 36L, 40L),
        c(0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0)
      ),
      RcW = cbind(
        c(8L, 9L, 12L, 15L, 18L, 21L, 24L),
        c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1.0)
      ),
      RcOpt = 0.46,
      p = 3L,
      i = 120L,
      Sx = 800L,
      a = 2.8,
      RRS = 0.005,
      RRG = 0.2
    )
  )
})
