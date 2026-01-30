# Validation & Internals for epicrop_sim --------------------------------------

testthat::test_that("setup: extended weather available", {
  testthat::skip_if(
    !exists("wth_formatted") ||
      !is.data.frame(wth_formatted) ||
      nrow(wth_formatted) < 185,
    "Need extended weather data for validation tests"
  )
})

# --- Shared constants (inline) ----------------------------------------------

.emergence_date <- "2000-07-01"

# Base modifier curves used across tests
RcA_base <- cbind(
  c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
  c(1.0, 1.0, 1.0, 0.9, 0.62, 0.43, 0.42, 0.42, 0.41, 0.41, 0.41, 0.41, 0.41)
)

RcT_base <- cbind(
  c(16L, 19L, 22L, 25L, 28L, 31L, 34L, 37L, 40L),
  c(0.0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88, 0.01, 0.0)
)

# Columns expected from epicrop_sim outputs
.expected_cols <- c(
  "simday",
  "dates",
  "sites",
  "latent",
  "infectious",
  "removed",
  "senesced",
  "rateinf",
  "rlex",
  "rtransfer",
  "rremoved",
  "rgrowth",
  "rsenesced",
  "diseased",
  "intensity"
)

# --- Inline helpers ----------------------------------------------------------

# Build a base argument list for epicrop_sim; override via modifyList
.base_args <- function(wth = wth_formatted) {
  list(
    wth = wth,
    emergence = .emergence_date,
    onset = 20,
    duration = 120,
    H0 = 100,
    I0 = 1,
    RcA = RcA_base,
    RcT = RcT_base,
    RcOpt = 0.87,
    p = 5,
    i = 30,
    Sx = 3200,
    RRS = 0.01,
    RRG = 0.1,
    a = 4, # scalar; engine requires a >= 1
    rhlim = 90, # scalar [0..100]
    rainlim = 5, # scalar >= 0
    simple_wetness = TRUE,
    age_driver = "day"
  )
}

# Safe wrapper to run epicrop_sim with overrides
run_epicrop <- function(overrides = list(), wth = wth_formatted) {
  args <- .base_args(wth)
  if (length(overrides)) {
    for (nm in names(overrides)) {
      args[[nm]] <- overrides[[nm]]
    } # shallow replace
  }
  do.call(epicrop_sim, args)
}

# Build modifiers with minimal duplication
build_modifiers <- function(
  w,
  age_vec = seq(0, nrow(w) - 1),
  duration = length(age_vec),
  overrides = list()
) {
  args <- list(
    w = w,
    age_vec = age_vec,
    duration = duration,
    RcA = RcA_base,
    RcT = RcT_base,
    RcW = NULL,
    RcOpt = 0.87,
    p = 5,
    i = 30,
    RRS = 0.01,
    a = 4,
    RRLEX = 0,
    RRG = 0.1,
    rhlim = 90,
    rainlim = 5,
    simple_wetness = TRUE
  )
  do.call(
    epicrop:::.prepare_modifiers,
    utils::modifyList(args, overrides, keep.null = TRUE)
  )
}

# Basic output checker
expect_valid_epicrop <- function(x, n_rows = 120L) {
  testthat::expect_s3_class(x, c("epicrop.sim", "data.table", "data.frame"))
  testthat::expect_identical(nrow(x), n_rows)
  testthat::expect_true(all(.expected_cols %in% names(x)))
  testthat::expect_true(all(x$intensity >= 0 & x$intensity <= 1))
  testthat::expect_false(is.null(attr(x, "AUDPC")))
  invisible(x)
}

# =============================================================================
# Validation Tests
# =============================================================================

testthat::test_that(".validate_all_inputs catches non-numeric values", {
  testthat::expect_error(
    run_epicrop(list(onset = "twenty")), # invalid type
    regexp = "numeric"
  )
})

testthat::test_that(".validate_all_inputs catches non-boolean simple_wetness", {
  testthat::expect_error(
    run_epicrop(list(simple_wetness = "no")), # invalid type
    regexp = "Boolean"
  )
})

testthat::test_that(".validate_all_inputs catches invalid epicrop.wth class", {
  wth_no_class <- as.data.frame(wth_formatted)
  class(wth_no_class) <- "data.frame"

  # Disease wrappers enforce class too
  testthat::expect_error(
    bacterial_blight(wth_no_class, emergence = .emergence_date),
    regexp = "epicrop.wth"
  )
})

testthat::test_that(".validate_all_inputs warns about short duration", {
  wth_short <- data.table::copy(wth_formatted)[1:60]
  RcA_short <- cbind(c(0L, 20L, 40L), c(1.0, 0.8, 0.6))

  expect_warning(
    run_epicrop(list(wth = wth_short, duration = 45, RcA = RcA_short)),
    regexp = "duration.*50"
  )
})

testthat::test_that(".validate_all_inputs catches invalid rhlim", {
  testthat::expect_error(
    run_epicrop(list(rhlim = 190)), # out of [0..100]
    regexp = "rhlim.*100"
  )
})

testthat::test_that(".validate_all_inputs catches TMIN/TMAX missing with complex wetness", {
  wth_no_temp <- data.table::data.table(
    YYYYMMDD = wth_formatted$YYYYMMDD,
    DOY = wth_formatted$DOY,
    TEMP = wth_formatted$TEMP,
    RHUM = wth_formatted$RHUM,
    RAIN = wth_formatted$RAIN
  )
  class(wth_no_temp) <- c("epicrop.wth", "data.table", "data.frame")

  testthat::expect_error(
    run_epicrop(list(wth = wth_no_temp, simple_wetness = FALSE)),
    regexp = "TMIN.*TMAX"
  )
})

testthat::test_that(".validate_all_inputs validates RcW when simple_wetness = FALSE", {
  RcW_bad_hours <- cbind(c(-1, 6, 12, 18, 30), c(0, 0.25, 0.5, 0.75, 1))
  testthat::expect_error(
    run_epicrop(list(simple_wetness = FALSE, RcW = RcW_bad_hours)),
    regexp = "RcW.*24"
  )

  RcW_bad_mods <- cbind(c(0, 6, 12, 18, 24), c(-0.1, 0.25, 0.5, 0.75, 1.5))
  testthat::expect_error(
    run_epicrop(list(simple_wetness = FALSE, RcW = RcW_bad_mods)),
    regexp = "RcW.*modifier.*0.*1"
  )
})

testthat::test_that(".validate_all_inputs catches negative/invalid scalar parameters", {
  # Disease wrappers: parameters that can be overridden
  testthat::expect_error(
    bacterial_blight(wth_formatted, emergence = .emergence_date, H0 = -10),
    regexp = "H0"
  )
  testthat::expect_error(
    bacterial_blight(wth_formatted, emergence = .emergence_date, I0 = -1),
    regexp = "I0"
  )
  testthat::expect_error(
    bacterial_blight(wth_formatted, emergence = .emergence_date, RcOpt = -0.5),
    regexp = "RcOpt"
  )
  testthat::expect_error(
    bacterial_blight(wth_formatted, emergence = .emergence_date, p = -5),
    regexp = "p"
  )
  testthat::expect_error(
    bacterial_blight(wth_formatted, emergence = .emergence_date, i = -20),
    regexp = "i"
  )

  # Direct engine calls
  testthat::expect_error(
    run_epicrop(list(Sx = -100)),
    regexp = "Sx"
  )
  testthat::expect_error(
    run_epicrop(list(a = 0.5)), # must be >= 1
    regexp = "a.*1"
  )
})

testthat::test_that(".validate_all_inputs catches zero or negative Sx", {
  testthat::expect_error(
    run_epicrop(list(Sx = 0)),
    regexp = "Sx"
  )
  testthat::expect_error(
    run_epicrop(list(Sx = -100)),
    regexp = "Sx"
  )
})

testthat::test_that(".validate_all_inputs catches onset < 1", {
  testthat::expect_error(
    bacterial_blight(wth_formatted, emergence = .emergence_date, onset = 0),
    regexp = "onset"
  )
})

testthat::test_that(".validate_all_inputs catches duration <= 0", {
  testthat::expect_error(
    run_epicrop(list(duration = 0)),
    regexp = "duration"
  )
  testthat::expect_error(
    run_epicrop(list(duration = -5)),
    regexp = "duration"
  )
})

# =============================================================================
# Date Handling Tests
# =============================================================================

testthat::test_that(".validate_emergence handles various date formats", {
  testthat::expect_s3_class(epicrop:::.validate_emergence("2000-01-01"), "POSIXct")
  testthat::expect_s3_class(epicrop:::.validate_emergence("01/01/2000"), "POSIXct")
  testthat::expect_s3_class(
    epicrop:::.validate_emergence(as.Date("2000-01-01")),
    "POSIXct"
  )
})

testthat::test_that(".validate_emergence fails with invalid date string", {
  testthat::expect_error(
    epicrop:::.validate_emergence("not-a-date"),
    regexp = "valid date"
  )
})

testthat::test_that(".validate_emergence preserves timezone information", {
  date_utc <- epicrop:::.validate_emergence("2000-01-01")
  testthat::expect_s3_class(date_utc, "POSIXct")
})

# =============================================================================
# Weather Preparation Tests
# =============================================================================

testthat::test_that(".prepare_weather_data converts to data.table", {
  wth_df <- as.data.frame(wth_formatted)
  wth_prep <- epicrop:::.prepare_weather_data(wth_df, 120)
  testthat::expect_s3_class(wth_prep, "data.table")
})

testthat::test_that(".prepare_weather_data catches NA in TEMP", {
  wth_na_temp <- data.table::copy(wth_formatted)
  wth_na_temp[1, TEMP := NA]
  testthat::expect_error(
    epicrop:::.prepare_weather_data(wth_na_temp, 120),
    regexp = "TEMP.*NA"
  )
})

testthat::test_that(".prepare_weather_data handles NA in RAIN", {
  wth_na_rain <- data.table::copy(wth_formatted)
  wth_na_rain[1:5, RAIN := NA]

  testthat::expect_warning(
    wth_prep <- epicrop:::.prepare_weather_data(wth_na_rain, 120),
    regexp = "RAIN.*NA"
  )
  testthat::expect_equal(wth_prep$RAIN[1:5], rep(0, 5))
})

# =============================================================================
# Simulation Window Tests
# =============================================================================

testthat::test_that(".determine_run_window extracts correct date range", {
  run_info <- epicrop:::.determine_run_window(
    wth_formatted,
    .emergence_date,
    120
  )

  testthat::expect_identical(nrow(run_info$wth_subset), 120L)

  # Day 0 is the day before emergence
  testthat::expect_equal(run_info$run_dates[1], as.Date("2000-06-30"))
  testthat::expect_equal(run_info$run_dates[120], as.Date("2000-06-30") + 119)

  # Sequential dates
  testthat::expect_equal(diff(as.numeric(run_info$run_dates)), rep(1, 119))
})

testthat::test_that(".determine_run_window catches insufficient weather data", {
  testthat::expect_error(
    epicrop:::.determine_run_window(wth_formatted, "2000-01-01", 120),
    regexp = "Insufficient weather"
  )
})

testthat::test_that(".determine_run_window warns about multiple emergence dates", {
  testthat::expect_warning(
    epicrop:::.determine_run_window(
      wth_formatted,
      c("2000-07-01", "2000-07-02"),
      120
    ),
    regexp = "Only one day.*emergence"
  )
})

# =============================================================================
# DVS Calculation Tests
# =============================================================================

testthat::test_that(".calculate_dvs produces values between 0 and 2", {
  w <- wth_formatted[1:120, ]
  thermal_time <- list(base_temp = 0, gdd_to_maturity = 1100)

  dvs <- epicrop:::.calculate_dvs(w, thermal_time)

  testthat::expect_length(dvs, 120)
  testthat::expect_true(all(dvs >= 0))
  testthat::expect_true(all(dvs <= 2))
})

testthat::test_that(".calculate_dvs increases monotonically", {
  w <- wth_formatted[1:120, ]
  thermal_time <- list(base_temp = 0, gdd_to_maturity = 1100)

  dvs <- epicrop:::.calculate_dvs(w, thermal_time)

  testthat::expect_true(all(diff(dvs) >= 0))
})

# =============================================================================
# Modifier Preparation Tests
# =============================================================================

testthat::test_that(".prepare_modifiers produces correct length vectors", {
  w <- wth_formatted[1:120, ]
  age_vec <- seq(0, 119)

  modifiers <- build_modifiers(w, age_vec = age_vec, duration = 120)

  testthat::expect_length(modifiers$Rc, 120)
  testthat::expect_length(modifiers$RRLEX, 120)
  testthat::expect_length(modifiers$RRG, 120)
  testthat::expect_length(modifiers$RRS, 120)
  testthat::expect_length(modifiers$p, 120)
  testthat::expect_length(modifiers$i, 120)
  testthat::expect_length(modifiers$a, 120)
})

testthat::test_that(".prepare_modifiers produces values in valid ranges", {
  w <- wth_formatted[1:120, ]
  age_vec <- seq(0, 119)

  modifiers <- build_modifiers(w, age_vec = age_vec, duration = 120)

  testthat::expect_true(all(modifiers$Rc >= 0))
  testthat::expect_true(all(modifiers$RRLEX >= 0))
  testthat::expect_true(all(modifiers$RRG >= 0))
  testthat::expect_true(all(modifiers$RRS >= 0))
  testthat::expect_true(all(modifiers$p > 0))
  testthat::expect_true(all(modifiers$i > 0))
  testthat::expect_true(all(modifiers$a >= 1))
})

# =============================================================================
# Inoculum Building Tests
# =============================================================================

testthat::test_that(".build_inoculum creates impulse at onset", {
  Iprim <- epicrop:::.build_inoculum(I0 = 1, onset = 20, duration = 120)

  testthat::expect_length(Iprim, 120)
  testthat::expect_equal(Iprim[20], 1)
  testthat::expect_equal(sum(Iprim), 1)
})

testthat::test_that(".build_inoculum handles list mode - uniform", {
  I0_list <- list(mode = "uniform", I0 = 10, duration = 5)

  Iprim <- epicrop:::.build_inoculum(I0_list, onset = 20, duration = 120)

  testthat::expect_length(Iprim, 120)
  testthat::expect_equal(sum(Iprim), 10)
  testthat::expect_equal(sum(Iprim[20:24]), 10)
})

testthat::test_that(".build_inoculum handles list mode - triangle", {
  I0_list <- list(mode = "triangle", I0 = 10, duration = 5)

  Iprim <- epicrop:::.build_inoculum(I0_list, onset = 20, duration = 120)

  testthat::expect_length(Iprim, 120)
  testthat::expect_equal(sum(Iprim), 10, tolerance = 0.01)

  # Peak should be in middle of the 5-day window (positions 21–22)
  peak_idx <- which.max(Iprim[20:24]) + 19
  testthat::expect_true(peak_idx %in% 21:22)
})

testthat::test_that(".build_inoculum handles list mode - series", {
  custom_series <- rep(0, 120)
  custom_series[20:25] <- c(1, 2, 3, 2, 1, 0.5)

  I0_list <- list(mode = "series", series = custom_series)

  Iprim <- epicrop:::.build_inoculum(I0_list, onset = 20, duration = 120)

  testthat::expect_identical(Iprim, custom_series)
})

testthat::test_that(".build_inoculum warns when onset outside window", {
  testthat::expect_warning(
    Iprim <- epicrop:::.build_inoculum(I0 = 1, onset = 150, duration = 120),
    regexp = "onset.*outside"
  )
  testthat::expect_equal(sum(Iprim), 0)
})

# =============================================================================
# Output Assembly Tests
# =============================================================================

# Small helper to mock a stable 'core' shape for .assemble_output
.mock_core <- function(n = 120L) {
  list(
    sites = rep(100, n),
    latent = rep(10, n),
    infectious = rep(5, n),
    removed = rep(2, n),
    senesced = rep(1, n),
    rateinf = rep(0.1, n),
    rlex = rep(0, n),
    rtransfer = rep(0.5, n),
    rremoved = rep(0.3, n),
    rgrowth = rep(1, n),
    rsenesced = rep(0.1, n),
    diseased = rep(17, n),
    intensity = seq(0, 0.5, length.out = n)
  )
}

testthat::test_that(".assemble_output produces correct structure", {
  core <- .mock_core(120L)
  run_dates <- seq(as.Date(.emergence_date), by = "day", length.out = 120)

  result <- epicrop:::.assemble_output(core, run_dates, 120, wth_formatted)

  testthat::expect_s3_class(
    result,
    c("epicrop.sim", "data.table", "data.frame")
  )
  testthat::expect_identical(nrow(result), 120L)
  testthat::expect_true("simday" %in% names(result))
  testthat::expect_true("dates" %in% names(result))
  testthat::expect_false(is.null(attr(result, "AUDPC")))
})

testthat::test_that(".assemble_output includes lat/lon when available", {
  core <- .mock_core(120L)
  run_dates <- seq(as.Date(.emergence_date), by = "day", length.out = 120)

  result <- epicrop:::.assemble_output(core, run_dates, 120, wth_formatted)

  # lat/lon presence & plausibility
  testthat::expect_true("lat" %in% names(result))
  testthat::expect_true("lon" %in% names(result))
  testthat::expect_type(result$lat, "double")
  testthat::expect_type(result$lon, "double")
  testthat::expect_length(unique(result$lat), 1L)
  testthat::expect_length(unique(result$lon), 1L)
  testthat::expect_false(is.na(unique(result$lat)))
  testthat::expect_false(is.na(unique(result$lon)))
  testthat::expect_true(abs(unique(result$lat)) > 0)
  testthat::expect_true(abs(unique(result$lon)) > 0)
})

# =============================================================================
# Helper Function Tests
# =============================================================================

testthat::test_that(".is_boolean correctly identifies booleans", {
  testthat::expect_true(epicrop:::.is_boolean(TRUE))
  testthat::expect_true(epicrop:::.is_boolean(FALSE))
  testthat::expect_false(epicrop:::.is_boolean(1))
  testthat::expect_false(epicrop:::.is_boolean("TRUE"))
  testthat::expect_false(epicrop:::.is_boolean(c(TRUE, FALSE)))
  testthat::expect_false(epicrop:::.is_boolean(NA))
})
