# Internal helpers for this file only ----------------------------------------

.valid_scalar_args <- function(overrides = list()) {
  defaults <- list(
    onset = 20,
    duration = 120,
    rhlim = 90,
    rainlim = 5,
    H0 = 600,
    RcOpt = 0.61,
    Sx = 3200,
    a = 1,
    steps_per_day = 1
  )
  utils::modifyList(defaults, overrides)
}

.mk_min_wth <- function(
  cols = c("YYYYMMDD", "TEMP", "RAIN", "RHUM", "TMIN", "TMAX")
) {
  base <- data.table::data.table(
    YYYYMMDD = seq.Date(as.Date("2000-01-01"), by = 1, length.out = 5),
    TEMP = 25,
    RAIN = 2,
    RHUM = 80,
    TMIN = 20,
    TMAX = 30
  )
  base <- base[, ..cols]
  class(base) <- c("epicrop.wth", "data.table", "data.frame")
  base
}

# ---- .is_boolean ------------------------------------------------------

test_that(".is_boolean identifies valid single booleans", {
  expect_true(.is_boolean(TRUE))
  expect_true(.is_boolean(FALSE))
})

test_that(".is_boolean rejects NA, non-logical, and non-scalar values", {
  expect_false(.is_boolean(NA))
  expect_false(.is_boolean(1))
  expect_false(.is_boolean("TRUE"))
  expect_false(.is_boolean(c(TRUE, FALSE)))
  expect_false(.is_boolean(logical(0)))
  expect_false(.is_boolean(NULL))
})

# ---- .validate_param_range ---------------------------------------------

test_that(".validate_param_range accepts inclusive boundaries by default", {
  expect_true(.validate_param_range(5, "x", min = 0, max = 10))
  expect_true(.validate_param_range(0, "x", min = 0, max = 10))
  expect_true(.validate_param_range(10, "x", min = 0, max = 10))
})

test_that(".validate_param_range errors below inclusive min and above inclusive max", {
  expect_error(
    .validate_param_range(-1, "x", min = 0),
    "must be >= 0"
  )
  expect_error(
    .validate_param_range(11, "x", max = 10),
    "must be <= 10"
  )
})

test_that(".validate_param_range enforces exclusive bounds", {
  expect_error(
    .validate_param_range(0, "x", min = 0, exclusive_min = TRUE),
    "must be > 0"
  )
  expect_true(
    .validate_param_range(
      0.0001,
      "x",
      min = 0,
      exclusive_min = TRUE
    )
  )

  expect_error(
    .validate_param_range(10, "x", max = 10, exclusive_max = TRUE),
    "must be < 10"
  )
  expect_true(
    .validate_param_range(
      9.9999,
      "x",
      max = 10,
      exclusive_max = TRUE
    )
  )
})

test_that(".validate_param_range skips checks when min/max are NULL", {
  expect_true(
    .validate_param_range(-1000, "x", min = NULL, max = NULL)
  )
  expect_true(.validate_param_range(1e9, "x", min = NULL))
})

test_that(".validate_param_range error message includes parameter name and value", {
  err <- tryCatch(
    .validate_param_range(-5, "onset", min = 0),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "onset")
  expect_match(conditionMessage(err), "-5")
})

# ---- .validate_weather ---------------------------------------------------

test_that(".validate_weather accepts a well-formed epicrop.wth object", {
  expect_true(.validate_weather(wth_formatted))
})

test_that(".validate_weather rejects objects without class epicrop.wth", {
  wth_bad_class <- as.data.frame(wth_formatted)
  expect_error(.validate_weather(wth_bad_class), "epicrop.wth")
})

test_that(".validate_weather errors when required columns are missing", {
  w <- data.table::copy(wth_formatted)
  w[, TEMP := NULL]
  expect_error(.validate_weather(w), "TEMP")

  w2 <- data.table::copy(wth_formatted)
  w2[, RAIN := NULL]
  expect_error(.validate_weather(w2), "RAIN")

  w3 <- data.table::copy(wth_formatted)
  w3[, YYYYMMDD := NULL]
  expect_error(.validate_weather(w3), "YYYYMMDD")
})

test_that(".validate_weather lists all missing columns at once", {
  w <- data.table::data.table(RHUM = 80)
  class(w) <- c("epicrop.wth", class(w))
  err <- tryCatch(.validate_weather(w), error = function(e) e)
  expect_match(conditionMessage(err), "YYYYMMDD")
  expect_match(conditionMessage(err), "TEMP")
  expect_match(conditionMessage(err), "RAIN")
})

# ---- .validate_scalars: individual parameter ranges ------------------------

test_that(".validate_scalars accepts valid defaults", {
  expect_true(do.call(.validate_scalars, .valid_scalar_args()))
})

test_that(".validate_scalars enforces onset >= 0", {
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(onset = -1))
    ),
    "onset"
  )
})

test_that(".validate_scalars enforces duration >= 1", {
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(duration = 0))
    ),
    "duration"
  )
})

test_that(".validate_scalars enforces rhlim within [0, 100]", {
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(rhlim = -1))
    ),
    "rhlim"
  )
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(rhlim = 101))
    ),
    "rhlim"
  )
})

test_that(".validate_scalars enforces rainlim >= 0", {
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(rainlim = -1))
    ),
    "rainlim"
  )
  expect_true(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(rainlim = 0))
    )
  )
})

test_that(".validate_scalars enforces H0 > 0 (exclusive)", {
  expect_error(
    do.call(.validate_scalars, .valid_scalar_args(list(H0 = 0))),
    "H0"
  )
  expect_true(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(H0 = 0.0001))
    )
  )
})

test_that(".validate_scalars enforces RcOpt >= 0 (inclusive)", {
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(RcOpt = -0.1))
    ),
    "RcOpt"
  )
  expect_true(
    do.call(.validate_scalars, .valid_scalar_args(list(RcOpt = 0)))
  )
})

test_that(".validate_scalars enforces Sx > 0 (exclusive)", {
  expect_error(
    do.call(.validate_scalars, .valid_scalar_args(list(Sx = 0))),
    "Sx"
  )
})

test_that(".validate_scalars enforces steps_per_day >= 1", {
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(steps_per_day = 0))
    ),
    "steps_per_day"
  )
})

# ---- .validate_scalars: `a` special-case handling --------------------------

test_that(".validate_scalars accepts scalar a >= 1 and rejects a < 1", {
  expect_true(
    do.call(.validate_scalars, .valid_scalar_args(list(a = 1)))
  )
  expect_true(
    do.call(.validate_scalars, .valid_scalar_args(list(a = 2.8)))
  )
  expect_error(
    do.call(.validate_scalars, .valid_scalar_args(list(a = 0.5))),
    ">= 1"
  )
})

test_that(".validate_scalars accepts vector a where all values >= 1", {
  expect_true(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(a = c(1, 2, 3)))
    )
  )
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(a = c(1, 0.5, 3)))
    ),
    ">= 1"
  )
})

test_that(".validate_scalars validates only the value column of a matrix a", {
  # Column 1 (ages) can be anything; column 2 (values) must be >= 1
  a_ok <- cbind(c(0, 60, 120), c(1, 2, 1))
  expect_true(
    do.call(.validate_scalars, .valid_scalar_args(list(a = a_ok)))
  )

  a_bad <- cbind(c(0, 60, 120), c(1, 0.5, 1))
  expect_error(
    do.call(.validate_scalars, .valid_scalar_args(list(a = a_bad))),
    ">= 1"
  )
})

test_that(".validate_scalars does not check values for a single-column matrix a", {
  # `ncol(a) >= 2` short-circuits the value check for 1-column matrices, so
  # out-of-range values are silently accepted. This test documents that
  # current behavior rather than asserting it's the ideal outcome.
  a_one_col <- matrix(c(-5, -10, -15), ncol = 1)
  expect_true(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(a = a_one_col))
    )
  )
})

test_that(".validate_scalars accepts a as a function without evaluating it", {
  a_fn <- function(age) 1 + age / 100
  expect_true(
    do.call(.validate_scalars, .valid_scalar_args(list(a = a_fn)))
  )
})

test_that(".validate_scalars rejects a of an invalid type", {
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(a = "high"))
    ),
    "numeric, matrix, or function"
  )
  expect_error(
    do.call(
      .validate_scalars,
      .valid_scalar_args(list(a = list(1, 2)))
    ),
    "numeric, matrix, or function"
  )
})

# ---- .validate_wetness_params -----------------------------------------

test_that(".validate_wetness_params passes through when simple_wetness = TRUE regardless of columns/RcW", {
  w_no_temp <- .mk_min_wth(c("YYYYMMDD", "TEMP", "RAIN", "RHUM"))
  expect_true(
    .validate_wetness_params(TRUE, w_no_temp, RcW = NULL)
  )
  expect_true(
    .validate_wetness_params(TRUE, w_no_temp, RcW = "garbage")
  )
})

test_that(".validate_wetness_params errors when simple_wetness = FALSE and TMIN/TMAX missing", {
  w_no_temp <- .mk_min_wth(c("YYYYMMDD", "TEMP", "RAIN", "RHUM"))
  expect_error(
    .validate_wetness_params(FALSE, w_no_temp, RcW = NULL),
    "TMIN"
  )
})

test_that(".validate_wetness_params reports only the missing temperature column(s)", {
  w_no_tmax <- .mk_min_wth(c("YYYYMMDD", "TEMP", "RAIN", "RHUM", "TMIN"))
  err <- tryCatch(
    .validate_wetness_params(FALSE, w_no_tmax, RcW = NULL),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "TMAX")
})

test_that(".validate_wetness_params succeeds when simple_wetness = FALSE and TMIN/TMAX present", {
  w_full <- .mk_min_wth()
  expect_true(.validate_wetness_params(FALSE, w_full, RcW = NULL))
})

test_that(".validate_wetness_params validates RcW when simple_wetness = FALSE and RcW supplied", {
  w_full <- .mk_min_wth()
  RcW_ok <- cbind(c(0, 24), c(0, 1))
  expect_true(
    .validate_wetness_params(FALSE, w_full, RcW = RcW_ok)
  )

  RcW_bad <- cbind(c(-1, 24), c(0, 1))
  expect_error(
    .validate_wetness_params(FALSE, w_full, RcW = RcW_bad),
    "RcW"
  )
})

# ---- .validate_RcW_matrix ------------------------------------------------

test_that(".validate_RcW_matrix accepts a well-formed matrix", {
  RcW <- cbind(c(0, 6, 12, 18, 24), c(0, 0.25, 0.5, 0.75, 1))
  expect_true(.validate_RcW_matrix(RcW))
})

test_that(".validate_RcW_matrix rejects non-numeric input", {
  expect_error(
    .validate_RcW_matrix(matrix(c("a", "b", "c", "d"), ncol = 2)),
    "numeric"
  )
})

test_that(".validate_RcW_matrix rejects non-matrix input and wrong column counts", {
  expect_error(
    .validate_RcW_matrix(c(1, 2, 3, 4)),
    "two-column matrix"
  )
  expect_error(
    .validate_RcW_matrix(matrix(1:9, ncol = 3)),
    "two-column matrix"
  )
})

test_that(".validate_RcW_matrix rejects hours outside [0, 24]", {
  expect_error(
    .validate_RcW_matrix(cbind(c(-1, 24), c(0, 1))),
    "RcW hours"
  )
  expect_error(
    .validate_RcW_matrix(cbind(c(0, 25), c(0, 1))),
    "RcW hours"
  )
})

test_that(".validate_RcW_matrix rejects modifiers outside [0, 1]", {
  expect_error(
    .validate_RcW_matrix(cbind(c(0, 24), c(-0.1, 1))),
    "RcW modifiers"
  )
  expect_error(
    .validate_RcW_matrix(cbind(c(0, 24), c(0, 1.1))),
    "RcW modifiers"
  )
})

test_that(".validate_RcW_matrix accepts boundary values 0/24 and 0/1", {
  expect_true(.validate_RcW_matrix(cbind(c(0, 24), c(0, 1))))
})

# ---- .validate_emergence -------------------------------------------------

test_that(".validate_emergence accepts Date, IDate, and POSIXt inputs (returned as IDate)", {
  d <- as.Date("2000-07-01")
  expect_identical(.validate_emergence(d), data.table::as.IDate(d))

  idt <- data.table::as.IDate("2000-07-01")
  expect_identical(.validate_emergence(idt), idt)

  pt <- as.POSIXct("2000-07-01 12:00:00", tz = "UTC")
  expect_identical(
    .validate_emergence(pt),
    data.table::as.IDate(pt)
  )
})

test_that(".validate_emergence parses each documented character date format", {
  expected <- data.table::as.IDate("2000-07-01")
  formats <- c(
    "2000-07-01",
    "2000/07/01",
    "01 Jul 2000",
    "01 July 2000",
    "Jul 01 2000",
    "July 01 2000"
  )

  for (f in formats) {
    expect_identical(
      .validate_emergence(f),
      expected,
      info = sprintf("format: %s", f)
    )
  }
})

test_that(".validate_emergence errors on an unparseable character string", {
  expect_error(
    .validate_emergence("not-a-date"),
    "not in a valid date format"
  )
})

test_that(".validate_emergence errors on unsupported input types", {
  expect_error(
    .validate_emergence(20000701),
    "must be a Date, IDate, POSIXt, or character"
  )
  expect_error(
    .validate_emergence(list("2000-07-01")),
    "must be a Date, IDate, POSIXt, or character"
  )
  expect_error(
    .validate_emergence(TRUE),
    "must be a Date, IDate, POSIXt, or character"
  )
})

test_that(".validate_emergence errors on NA character input", {
  expect_error(.validate_emergence(NA_character_))
})
