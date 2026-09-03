# ---- Test fixtures ----------------------------------------------------

# A lightweight stand-in for a real epicrop disease model (e.g.
# bacterial_blight()). It mimics seir()'s real contract: it errors if the
# weather slice it receives has fewer rows than `duration`, rather than
# silently truncating, so tests here reflect what a real model would do.
mock_model <- function(
  wth,
  emergence,
  duration = 30L,
  RcOpt = 1,
  fail = FALSE,
  ...
) {
  if (fail) {
    stop("mock model failure")
  }
  if (nrow(wth) < duration) {
    cli::cli_abort(
      "Insufficient weather data to run {duration} days after emergence"
    )
  }
  out <- data.table::data.table(
    simday = seq_len(duration),
    dates = wth$YYYYMMDD[seq_len(duration)],
    intensity = seq(0, 0.5, length.out = duration),
    RcOpt = RcOpt
  )
  data.table::setattr(out, "AUDPC", sum(out$intensity))
  data.table::setattr(out, "class", c("epicrop.sim", class(out)))
  out[]
}

.mk_year_wth <- function(year, temp = 25) {
  dates <- seq(
    as.Date(sprintf("%d-01-01", year)),
    as.Date(sprintf("%d-12-31", year)),
    by = "1 day"
  )
  dt <- data.table::data.table(
    YYYYMMDD = data.table::as.IDate(dates),
    TEMP = temp,
    RAIN = 2,
    RHUM = 80
  )
  class(dt) <- c("epicrop.wth", "data.table", "data.frame")
  dt
}

.mk_wth_list <- function(years, ...) {
  wl <- stats::setNames(
    lapply(years, .mk_year_wth, ...),
    as.character(years)
  )
  class(wl) <- c("epicrop.wth.list", class(wl))
  wl
}

.mk_emergence <- function(dates) {
  d <- data.table::as.IDate(dates)
  class(d) <- union("epicrop.emerge", class(d))
  d
}

# ---- .validate_run_inputs ---------------------------------------------

test_that(".validate_run_inputs errors when emergence_dates lacks class epicrop.emerge", {
  wl <- .mk_wth_list(2001)

  expect_error(
    .validate_run_inputs(as.Date("2001-06-01"), wl),
    "epicrop.emerge"
  )
})

test_that(".validate_run_inputs errors when wth_list lacks a recognized class", {
  em <- .mk_emergence("2001-06-01")
  bad_wl <- list("2001" = .mk_year_wth(2001)) # no class tag

  expect_error(.validate_run_inputs(em, bad_wl))
})

test_that(".validate_run_inputs accepts epicrop.wth.list and epicrop.wth.bundle classes", {
  em <- .mk_emergence("2001-06-01")
  wl <- .mk_wth_list(2001)

  expect_no_error(.validate_run_inputs(em, wl))

  bundle <- list(siteA = wl)
  class(bundle) <- c("epicrop.wth.bundle", class(bundle))
  expect_no_error(.validate_run_inputs(em, bundle))
})

# ---- .is_wth_bundle -----------------------------------------------------

test_that(".is_wth_bundle detects an explicit epicrop.wth.bundle class", {
  x <- list(a = 1)
  class(x) <- c("epicrop.wth.bundle", class(x))
  expect_true(.is_wth_bundle(x))
})

test_that(".is_wth_bundle returns FALSE for a plain per-year list (4-digit names)", {
  wl <- .mk_wth_list(2001:2003)
  expect_false(.is_wth_bundle(wl))
})

test_that(".is_wth_bundle returns TRUE when names are not all 4-digit years", {
  wl <- .mk_wth_list(2001:2002)
  names(wl) <- c("siteA", "siteB")
  expect_true(.is_wth_bundle(wl))
})

test_that(".is_wth_bundle returns TRUE for an unnamed list", {
  wl <- unname(.mk_wth_list(2001:2002))
  expect_true(.is_wth_bundle(wl))
})

test_that(".is_wth_bundle returns TRUE when only some names are 4-digit years", {
  wl <- .mk_wth_list(2001:2002)
  names(wl) <- c("2001", "siteB")
  expect_true(.is_wth_bundle(wl))
})

# ---- .slice_weather -------------------------------------------------

test_that(".slice_weather returns exactly window_days rows within a single year", {
  wl <- .mk_wth_list(2001)
  ed <- data.table::as.IDate("2001-06-01")

  seg <- .slice_weather(ed, wl, window_days = 10L)

  expect_identical(nrow(seg), 10L)
  expect_identical(seg$YYYYMMDD[1], ed)
  expect_identical(seg$YYYYMMDD[10], ed + 9L)
})

test_that(".slice_weather combines segments spanning two years", {
  wl <- .mk_wth_list(2001:2002)
  ed <- data.table::as.IDate("2001-12-28")

  seg <- .slice_weather(ed, wl, window_days = 10L)

  expect_identical(nrow(seg), 10L)
  expect_identical(seg$YYYYMMDD[1], ed)
  expect_identical(seg$YYYYMMDD[10], ed + 9L)
  expect_setequal(data.table::year(seg$YYYYMMDD), c(2001, 2002))
})

test_that(".slice_weather warns and returns only the available year when one year is missing", {
  wl <- .mk_wth_list(2001) # 2002 not present
  ed <- data.table::as.IDate("2001-12-28")

  expect_warning(
    seg <- .slice_weather(ed, wl, window_days = 10L),
    "no weather for year 2002"
  )
  expect_identical(nrow(seg), 4L) # 2001-12-28 .. 2001-12-31
  expect_true(all(data.table::year(seg$YYYYMMDD) == 2001))
})

test_that(".slice_weather returns NULL and warns when no years are available at all", {
  wl <- .mk_wth_list(2005) # unrelated year
  ed <- data.table::as.IDate("2001-06-01")

  expect_warning(
    seg <- .slice_weather(ed, wl, window_days = 10L),
    "no weather for year 2001"
  )
  expect_null(seg)
})

test_that(".slice_weather warns and skips a year with zero matching rows", {
  wl <- .mk_wth_list(2001:2002)
  wl[["2002"]] <- wl[["2002"]][0] # table exists but is empty

  ed <- data.table::as.IDate("2001-12-28")

  expect_warning(
    seg <- .slice_weather(ed, wl, window_days = 10L),
    "no rows in year 2002"
  )
  expect_identical(nrow(seg), 4L)
  expect_true(all(data.table::year(seg$YYYYMMDD) == 2001))
})

test_that(".slice_weather produces no warnings when the window fits entirely within one year", {
  wl <- .mk_wth_list(2001)
  ed <- data.table::as.IDate("2001-03-01")

  expect_no_warning(
    seg <- .slice_weather(ed, wl, window_days = 30L)
  )
  expect_identical(nrow(seg), 30L)
})

# ---- run_epicrop_model(): basic behaviour --------------------------------

test_that("run_epicrop_model returns audpc output for a single emergence date", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 30L,
    output = "audpc"
  )

  expect_s3_class(res, "data.table")
  expect_named(res, c("emergence", "AUDPC"))
  expect_identical(nrow(res), 1L)
})

test_that("run_epicrop_model returns one row per emergence date", {
  wl <- .mk_wth_list(2001:2002)
  em <- .mk_emergence(c("2001-06-01", "2002-06-01", "2002-06-14"))

  res <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 30L,
    output = "audpc"
  )

  expect_identical(nrow(res), 3L)
})

test_that("run_epicrop_model returns full output with model columns plus emergence/AUDPC", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 30L,
    output = "full"
  )

  expect_s3_class(res, "epicrop.sim")
  expect_true(all(
    c("simday", "intensity", "emergence", "AUDPC") %in% names(res)
  ))
  expect_identical(nrow(res), 30L)
})

test_that("run_epicrop_model defaults to window_days = 120L and output = 'audpc'", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(mock_model, em, wl) # mock_model default duration = 30L

  expect_named(res, c("emergence", "AUDPC"))
  expect_identical(nrow(res), 1L)
})

test_that("run_epicrop_model errors for an invalid output argument", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  expect_error(run_epicrop_model(mock_model, em, wl, output = "invalid_choice"))
})

test_that("run_epicrop_model errors on malformed emergence_dates or wth_list", {
  wl <- .mk_wth_list(2001)

  expect_error(
    run_epicrop_model(mock_model, as.Date("2001-06-01"), wl),
    "epicrop.emerge"
  )
  expect_error(
    run_epicrop_model(
      mock_model,
      .mk_emergence("2001-06-01"),
      list("2001" = .mk_year_wth(2001))
    )
  )
})

test_that("run_epicrop_model forwards ... arguments to model_fun", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 30L,
    output = "full",
    RcOpt = 99
  )

  expect_true(all(res$RcOpt == 99))
})

test_that("run_epicrop_model produces identical results for repeated calls", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence(c("2001-06-01", "2001-06-14"))

  r1 <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 30L,
    output = "audpc"
  )
  r2 <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 30L,
    output = "audpc"
  )

  expect_identical(r1, r2)
})

# ---- run_epicrop_model(): missing-weather degradation --------------------

test_that("run_epicrop_model warns and produces NA AUDPC when weather doesn't cover an emergence date", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2005-06-01") # no matching weather year

  expect_warning(
    res <- run_epicrop_model(
      mock_model,
      em,
      wl,
      window_days = 30L,
      output = "audpc"
    )
  )

  expect_true(is.na(res$AUDPC))
})

test_that("run_epicrop_model drops missing-weather dates entirely for output = 'full'", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence(c("2001-06-01", "2005-06-01"))

  expect_warning(
    res <- run_epicrop_model(
      mock_model,
      em,
      wl,
      window_days = 30L,
      output = "full"
    )
  )

  expect_identical(nrow(res), 30L) # only the valid 2001 date contributes rows
  expect_true(all(res$emergence == data.table::as.IDate("2001-06-01")))
})

# ---- run_epicrop_model(): bundle mode ------------------------------------

test_that("run_epicrop_model recurses through bundle input and adds a location column", {
  bundle <- list(
    siteA = .mk_wth_list(2001, temp = 20),
    siteB = .mk_wth_list(2001, temp = 30)
  )
  class(bundle) <- c("epicrop.wth.bundle", class(bundle))
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    mock_model,
    em,
    bundle,
    window_days = 30L,
    output = "audpc"
  )

  expect_true("location" %in% names(res))
  expect_setequal(res$location, c("siteA", "siteB"))
  expect_identical(nrow(res), 2L)
})

test_that("run_epicrop_model bundle mode strips '__suffix' from location labels", {
  bundle <- list(
    siteA__rep1 = .mk_wth_list(2001),
    siteA__rep2 = .mk_wth_list(2001)
  )
  class(bundle) <- c("epicrop.wth.bundle", class(bundle))
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    mock_model,
    em,
    bundle,
    window_days = 30L,
    output = "audpc"
  )

  expect_true(all(res$location == "siteA"))
})

test_that("run_epicrop_model bundle mode auto-labels unnamed elements as locationNN", {
  bundle <- list(.mk_wth_list(2001), .mk_wth_list(2001))
  class(bundle) <- c("epicrop.wth.bundle", class(bundle))
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    mock_model,
    em,
    bundle,
    window_days = 30L,
    output = "audpc"
  )

  expect_setequal(res$location, c("location01", "location02"))
})

test_that("run_epicrop_model rejects a bundle-shaped list that lacks the epicrop.wth.bundle class", {
  # Same shape .is_wth_bundle() would recognize as a bundle (non-4-digit
  # names), but without the required class tag -- validation should still
  # reject it, since .validate_run_inputs() checks class, not name shape.
  untagged_bundle <- list(
    siteA = .mk_wth_list(2001),
    siteB = .mk_wth_list(2001)
  )
  em <- .mk_emergence("2001-06-01")

  expect_error(
    run_epicrop_model(mock_model, em, untagged_bundle, window_days = 30L),
    "epicrop.wth.list.*epicrop.wth.bundle|epicrop.wth.bundle.*epicrop.wth.list"
  )
})

# ---- run_epicrop_model(): documents the window_days/duration gap ---------

test_that("explicitly passing duration via ... resolves the window_days gap", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 10L,
    output = "full",
    duration = 10L
  )

  expect_identical(nrow(res), 10L)
})

# ---- .model_has_duration_formal ------------------------------------------

test_that(".model_has_duration_formal detects an explicit duration formal", {
  expect_true(.model_has_duration_formal(mock_model))
  expect_true(.model_has_duration_formal(seir))
})

test_that(".model_has_duration_formal returns FALSE when duration is not a formal (only reachable via ...)", {
  wrapped <- function(wth, emergence, RcOpt = 1, ...) {
    mock_model(wth, emergence, duration = 30L, RcOpt = RcOpt, ...)
  }
  expect_false(.model_has_duration_formal(wrapped))
})

# ---- run_epicrop_model(): window_days -> duration auto-fill --------------

test_that("window_days is automatically wired into model_fun's duration when model_fun exposes a formal duration parameter", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 10L,
    output = "full"
  )

  expect_identical(nrow(res), 10L)
})

test_that("explicitly passing duration via ... overrides the auto-filled window_days duration", {
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 30L,
    output = "full",
    duration = 5L
  )

  expect_identical(nrow(res), 5L)
})

test_that("duration auto-fill is skipped for model_fun's without a formal duration parameter (e.g. wrapped disease models)", {
  # Mirrors bacterial_blight() and friends: duration is hardcoded internally
  # and reachable only via `...`, so run_epicrop_model() must not attempt to
  # auto-inject it -- doing so would collide with the hardcoded value.
  no_duration_formal_model <- function(wth, emergence, RcOpt = 1, ...) {
    mock_model(wth, emergence, duration = 30L, RcOpt = RcOpt, ...)
  }

  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  res <- run_epicrop_model(
    no_duration_formal_model,
    em,
    wl,
    window_days = 40L,
    output = "full"
  )

  # Uses the wrapper's own hardcoded duration = 30L, not window_days = 40L
  expect_identical(nrow(res), 30L)
})

test_that("duration auto-fill uses the actual sliced weather length, not window_days blindly", {
  # Only 1 year of weather is available; requesting a window that runs past
  # year-end but window_days itself is achievable within that single year,
  # so nrow(w_sub) should equal window_days here (sanity check for the
  # nrow(w_sub)-based fill, not window_days-based).
  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-11-01") # 61 days to year end, window_days=30 fits

  res <- run_epicrop_model(
    mock_model,
    em,
    wl,
    window_days = 30L,
    output = "full"
  )

  expect_identical(nrow(res), 30L)
})

test_that("run_epicrop_model does not break real hardcoded-duration wrapper functions", {
  # A stand-in shaped exactly like bacterial_blight(): duration hardcoded
  # inside the call to a lower-level engine, no `duration` formal exposed.
  hardcoded_wrapper <- function(wth, emergence, RcA = NULL, RcT = NULL, ...) {
    mock_model(wth, emergence, duration = 20L, ...)
  }

  wl <- .mk_wth_list(2001)
  em <- .mk_emergence("2001-06-01")

  expect_no_error(
    run_epicrop_model(
      hardcoded_wrapper,
      em,
      wl,
      window_days = 120L,
      output = "full"
    )
  )
})
