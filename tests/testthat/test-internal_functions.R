# Define modifier curves used across tests
RcA <- cbind(
  c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
  c(
    1.0,
    1.0,
    1.0,
    0.9,
    0.62,
    0.43,
    0.42,
    0.42,
    0.41,
    0.41,
    0.41,
    0.41,
    0.41
  )
)

RcT <- cbind(
  c(16L, 19L, 22L, 25L, 28L, 31L, 34L, 37L, 40L),
  c(0.0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88, 0.01, 0.0)
)

# Helper to create valid args for . check_inputs()
make_valid_args <- function(...) {
  defaults <- list(
    wth = wth_formatted,
    emergence = "2000-07-01",
    onset = 20L,
    duration = 120L,
    rhlim = 90,
    rainlim = 5,
    H0 = 600,
    I0 = 1,
    RcA = RcA,
    RcT = RcT,
    RcOpt = 0.61,
    p = 6L,
    i = 19L,
    Sx = 100000,
    a = 1,
    RRS = 0.01,
    RRG = 0.1,
    RcW = NULL,
    simple_wetness = TRUE
  )

  utils::modifyList(defaults, list(...))
}

# Tests for input validation
test_that("non numeric values are not accepted", {
  var_args <- make_valid_args(onset = "twenty")

  expect_error(
    .check_inputs(var_args),
    regexp = "onset.*numeric"
  )
})

test_that("Formatting fails if simple_wetness is not boolean", {
  var_args <- make_valid_args(simple_wetness = "no")

  expect_error(
    .check_inputs(var_args),
    regexp = "simple_wetness.*Boolean"
  )
})

test_that("Failure if `wth` is not of class `epicrop.wth`", {
  # Create a plain data. frame without epicrop.wth class
  wth_no_class <- as.data.frame(wth)
  class(wth_no_class) <- "data.frame"

  # Build var_args manually
  var_args <- list(
    wth = wth_no_class,
    emergence = "2000-07-01",
    onset = 20L,
    duration = 120L,
    rhlim = 90,
    rainlim = 5,
    H0 = 600,
    I0 = 1,
    RcA = RcA,
    RcT = RcT,
    RcOpt = 0.61,
    p = 6L,
    i = 19L,
    Sx = 100000,
    a = 1,
    RRS = 0.01,
    RRG = 0.1,
    RcW = NULL,
    simple_wetness = TRUE
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "epicrop.wth"
  )
})

test_that("Failure if `duration` is <50", {
  var_args <- make_valid_args(duration = 45)

  expect_error(
    .check_inputs(var_args),
    regexp = "duration.*<\\s*50"
  )
})

test_that("Failure if `rhlim` is >100", {
  var_args <- make_valid_args(rhlim = 190)

  expect_error(
    .check_inputs(var_args),
    regexp = "rhlim.*100"
  )
})

test_that("Error if `duration` is > max RcA days", {
  var_args <- make_valid_args(duration = 140)

  expect_error(
    .check_inputs(var_args),
    regexp = "RcA.*duration.*less"
  )
})

test_that("Warning if `duration` is < max RcA days", {
  var_args <- make_valid_args(duration = 100)

  expect_warning(
    .check_inputs(var_args),
    regexp = "RcA.*duration.*greater"
  )
})

test_that("Failure if TMIN/TMAX not supplied with `simple_wetness = FALSE`", {
  wth2 <- wth_formatted[, -c("TMIN", "TMAX")]
  var_args <- make_valid_args(
    wth = wth2,
    simple_wetness = FALSE
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "TMIN.*TMAX|simple_wetness"
  )
})

# Tests for negative parameter validation
test_that("validation catches negative values in key parameters", {
  test_params <- c(
    "H0",
    "I0",
    "RcOpt",
    "p",
    "i",
    "Sx",
    "a",
    "RRS",
    "rhlim",
    "rainlim"
  )

  for (param in test_params) {
    var_args <- make_valid_args()
    var_args[[param]] <- -1

    expect_error(
      .check_inputs(var_args),
      regexp = param,
      info = paste("Failed to catch negative", param)
    )
  }
})

test_that("validation catches RcA/RcT with non-numeric values", {
  var_args <- make_valid_args(
    RcA = cbind(1:5, c("a", "b", "c", "d", "e"))
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcA.*numeric"
  )
})

test_that("validation handles RcW hour validation", {
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = cbind(
      c(0, 6, 12, 30), # 30 exceeds 24
      c(0, 0.5, 0.75, 1)
    )
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW.*24"
  )
})

test_that("validation handles RcW modifier out of bounds", {
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = cbind(
      c(0, 6, 12, 18, 24),
      c(0, 0.5, 1.5, 0.9, 1) # 1.5 exceeds 1
    )
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW.*modifier.*0.*1"
  )
})

# Tests for . check_date()
test_that("Failure if object should be date but cannot be formatted as such", {
  expect_snapshot(
    .check_date(x = "string"),
    error = TRUE
  )
})

test_that(". check_date() handles various date formats", {
  expect_s3_class(.check_date("2000-01-01"), "POSIXct")
  expect_s3_class(.check_date("01/01/2000"), "POSIXct")
  expect_s3_class(.check_date(as.Date("2000-01-01")), "POSIXct")
})

# Tests for .check_emergence_date()
test_that("Vectors of establishment dates only use the first", {
  expect_snapshot(
    .check_emergence_date(x = c("2000-06-01", "2000-06-02"))
  )
})

test_that(".check_emergence_date() handles single date without warning", {
  expect_no_warning(.check_emergence_date(x = "2000-06-01"))
})

# Tests for .is_boolean()
test_that(".is_boolean() correctly identifies booleans", {
  expect_true(.is_boolean(TRUE))
  expect_true(.is_boolean(FALSE))
  expect_false(.is_boolean(1))
  expect_false(.is_boolean("TRUE"))
  expect_false(.is_boolean(c(TRUE, FALSE)))
})

# Additional validation tests
test_that("validation catches aggregation coefficient < 1", {
  var_args <- make_valid_args(a = 0.5)

  expect_error(
    .check_inputs(var_args),
    regexp = "a.*aggregation|>=\\s*1"
  )
})

test_that("validation catches zero or negative Sx", {
  var_args <- make_valid_args(Sx = 0)

  expect_error(
    .check_inputs(var_args),
    regexp = "Sx"
  )

  var_args <- make_valid_args(Sx = -100)

  expect_error(
    .check_inputs(var_args),
    regexp = "Sx"
  )
})

test_that("validation catches onset < 1", {
  var_args <- make_valid_args(onset = 0)

  expect_error(
    .check_inputs(var_args),
    regexp = "onset.*>=\\s*1"
  )
})

test_that("validation catches duration <= 0", {
  # duration < 50 check happens first, so we just check for "duration" in error
  var_args <- make_valid_args(duration = 0)

  expect_error(
    .check_inputs(var_args),
    regexp = "duration"
  )

  var_args <- make_valid_args(duration = -5)

  expect_error(
    .check_inputs(var_args),
    regexp = "duration"
  )
})

test_that("validation handles RcT with non-numeric values", {
  var_args <- make_valid_args(
    RcT = cbind(15:20, c("a", "b", "c", "d", "e", "f"))
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcT.*numeric"
  )
})

test_that("validation handles RRG as matrix with non-numeric values", {
  RRG_bad <- cbind(1:120, rep("bad", 120))
  var_args <- make_valid_args(RRG = RRG_bad)

  expect_error(
    .check_inputs(var_args),
    regexp = "RRG.*numeric"
  )
})

test_that("validation handles missing weather columns for complex wetness", {
  # Create a minimal weather object without TMIN/TMAX
  wth_no_temp <- data.table::data.table(
    YYYYMMDD = seq.Date(as.Date("2000-07-01"), by = "day", length.out = 185),
    DOY = 182:366,
    TEMP = rep(25, 185),
    RHUM = rep(80, 185),
    RAIN = rep(5, 185),
    LAT = rep(14.6774, 185),
    LON = rep(121.25562, 185)
  )
  class(wth_no_temp) <- c("epicrop.wth", "data.table", "data.frame")

  # Verify TMIN/TMAX are NOT in this weather object
  expect_false("TMIN" %in% names(wth_no_temp))
  expect_false("TMAX" %in% names(wth_no_temp))
  expect_true("epicrop.wth" %in% class(wth_no_temp))

  # Build var_args manually
  var_args <- list(
    wth = wth_no_temp,
    emergence = "2000-07-01",
    onset = 20L,
    duration = 120L,
    rhlim = 90,
    rainlim = 5,
    H0 = 600,
    I0 = 1,
    RcA = RcA,
    RcT = RcT,
    RcOpt = 0.61,
    p = 6L,
    i = 19L,
    Sx = 100000,
    a = 1,
    RRS = 0.01,
    RRG = 0.1,
    RcW = cbind(c(0, 6, 12, 18, 24), c(0, 0.25, 0.5, 0.75, 1)),
    simple_wetness = FALSE
  )

  # Verify the wth in var_args doesn't have TMIN/TMAX
  expect_false("TMIN" %in% names(var_args$wth))
  expect_false("TMAX" %in% names(var_args$wth))

  # Now test - should error about missing TMIN/TMAX
  expect_error(
    .check_inputs(var_args),
    regexp = "TMIN|TMAX"
  )
})

test_that("validation requires RcW when simple_wetness = FALSE", {
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = NULL
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW|matrix|weighted"
  )
})

test_that("validation rejects scalar RcW when simple_wetness = FALSE", {
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = 1
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW.*scalar|matrix"
  )
})

test_that("validation checks RcW is a proper 2-column matrix", {
  # List (not matrix) - will fail numeric check first
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = list(c(0, 6, 12), c(0, 0.5, 1))
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW" # Either numeric or matrix error
  )

  # Wrong number of columns
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = cbind(c(0, 6, 12), c(0, 0.5, 1), c(1, 2, 3))
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW.*two-column"
  )
})

test_that("validation checks RcW hour values are 0-24", {
  # Negative hours
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = cbind(c(-1, 6, 12, 18, 24), c(0, 0.25, 0.5, 0.75, 1))
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW.*0.*24"
  )

  # Hours > 24
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = cbind(c(0, 6, 12, 18, 25), c(0, 0.25, 0.5, 0.75, 1))
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW.*24"
  )
})

test_that("validation checks RcW modifier values are 0-1", {
  # Negative modifier
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = cbind(c(0, 6, 12, 18, 24), c(-0.1, 0.25, 0.5, 0.75, 1))
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW.*modifier.*0"
  )

  # Modifier > 1
  var_args <- make_valid_args(
    simple_wetness = FALSE,
    RcW = cbind(c(0, 6, 12, 18, 24), c(0, 0.25, 0.5, 0.75, 1.5))
  )

  expect_error(
    .check_inputs(var_args),
    regexp = "RcW.*modifier.*1"
  )
})

test_that(".check_date() preserves timezone information", {
  # Test with explicit timezone
  date_utc <- .check_date("2000-01-01")
  expect_s3_class(date_utc, "POSIXct")

  # Test with Date object
  date_obj <- as.Date("2000-01-01")
  date_converted <- .check_date(date_obj)
  expect_s3_class(date_converted, "POSIXct")
})

test_that("validation handles empty or NULL weather data", {
  var_args <- make_valid_args(wth = NULL)

  expect_error(
    .check_inputs(var_args),
    regexp = "wth|epicrop.wth"
  )
})

test_that("validation handles data.frame weather without epicrop.wth class", {
  # Create a plain data. frame by converting to regular data.frame
  wth_df <- as.data.frame(wth_formatted)

  # Set class to only data.frame (removing all other classes)
  class(wth_df) <- "data.frame"

  # Verify only data.frame class remains
  expect_identical(class(wth_df), "data.frame")
  expect_false(inherits(wth_df, "epicrop.wth"))

  # Build var_args manually instead of using make_valid_args
  var_args <- list(
    wth = wth_df,
    emergence = "2000-07-01",
    onset = 20L,
    duration = 120L,
    rhlim = 90,
    rainlim = 5,
    H0 = 600,
    I0 = 1,
    RcA = RcA,
    RcT = RcT,
    RcOpt = 0.61,
    p = 6L,
    i = 19L,
    Sx = 100000,
    a = 1,
    RRS = 0.01,
    RRG = 0.1,
    RcW = NULL,
    simple_wetness = TRUE
  )

  # Verify it's still just a data.frame in var_args
  expect_false(inherits(var_args$wth, "epicrop.wth"))

  expect_error(
    .check_inputs(var_args),
    regexp = "epicrop.wth"
  )
})

test_that("validation catches missing required arguments in seir()", {
  # Test missing wth - need to provide simple_wetness explicitly
  expect_error(
    seir(
      # missing wth
      emergence = "2000-07-01",
      onset = 20L,
      duration = 120L,
      rhlim = 90,
      rainlim = 5,
      H0 = 600,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.61,
      p = 6L,
      i = 19L,
      Sx = 100000,
      a = 1,
      RRS = 0.01,
      RRG = 0.1,
      simple_wetness = TRUE # Add this
    )
  )

  # Test missing emergence
  expect_error(
    seir(
      wth = wth_formatted,
      # missing emergence
      onset = 20L,
      duration = 120L,
      rhlim = 90,
      rainlim = 5,
      H0 = 600,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.61,
      p = 6L,
      i = 19L,
      Sx = 100000,
      a = 1,
      RRS = 0.01,
      RRG = 0.1
    )
  )

  # Test missing RcA
  expect_error(
    seir(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20L,
      duration = 120L,
      rhlim = 90,
      rainlim = 5,
      H0 = 600,
      I0 = 1,
      # missing RcA
      RcT = RcT,
      RcOpt = 0.61,
      p = 6L,
      i = 19L,
      Sx = 100000,
      a = 1,
      RRS = 0.01,
      RRG = 0.1
    )
  )
})

test_that("validation catches missing required arguments in seir()", {
  # Test missing wth - validation checks class before rlang::check_required
  expect_error(
    seir(
      # missing wth
      emergence = "2000-07-01",
      onset = 20L,
      duration = 120L,
      rhlim = 90,
      rainlim = 5,
      H0 = 600,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.61,
      p = 6L,
      i = 19L,
      Sx = 100000,
      a = 1,
      RRS = 0.01,
      RRG = 0.1,
      simple_wetness = TRUE
    )
  )

  # Test missing emergence
  expect_error(
    seir(
      wth = wth_formatted,
      # missing emergence
      onset = 20L,
      duration = 120L,
      rhlim = 90,
      rainlim = 5,
      H0 = 600,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.61,
      p = 6L,
      i = 19L,
      Sx = 100000,
      a = 1,
      RRS = 0.01,
      RRG = 0.1
    )
  )

  # Test missing RcT (RcA validation happens first if RcA is missing)
  expect_error(
    seir(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20L,
      duration = 120L,
      rhlim = 90,
      rainlim = 5,
      H0 = 600,
      I0 = 1,
      RcA = RcA,
      # missing RcT
      RcOpt = 0.61,
      p = 6L,
      i = 19L,
      Sx = 100000,
      a = 1,
      RRS = 0.01,
      RRG = 0.1
    )
  )

  # Test missing onset
  expect_error(
    seir(
      wth = wth_formatted,
      emergence = "2000-07-01",
      # missing onset
      duration = 120L,
      rhlim = 90,
      rainlim = 5,
      H0 = 600,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.61,
      p = 6L,
      i = 19L,
      Sx = 100000,
      a = 1,
      RRS = 0.01,
      RRG = 0.1
    )
  )
})

test_that("validation handles extremely large duration values", {
  var_args <- make_valid_args(duration = 10000L)

  # Should error because RcA doesn't cover that many days
  expect_error(
    .check_inputs(var_args),
    regexp = "RcA.*duration"
  )
})

test_that("validation handles edge case:  a = 1 (no aggregation)", {
  var_args <- make_valid_args(a = 1)

  # Should not error
  expect_no_error(.check_inputs(var_args))
})

test_that("validation handles edge case: rhlim = 0", {
  var_args <- make_valid_args(rhlim = 0)

  # Should not error (0 is valid, means never wet)
  expect_no_error(.check_inputs(var_args))
})

test_that("validation handles edge case: rhlim = 100", {
  var_args <- make_valid_args(rhlim = 100)

  # Should not error (100 is valid)
  expect_no_error(.check_inputs(var_args))
})

test_that("validation handles emergence date outside weather data range", {
  # This is tested in seir() itself, not .check_inputs
  # But we can verify the date checking works
  var_args <- make_valid_args(emergence = "1999-01-01")

  # . check_inputs validates the date format, not the range
  expect_no_error(.check_inputs(var_args))

  # But seir() should catch it
  expect_error(
    do.call(seir, var_args),
    regexp = "Incomplete weather|dates.*align"
  )
})
