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
