# Test epicrop_sim core simulation engine

testthat::test_that("setup: extended weather available", {
  testthat::skip_if(
    !exists("wth_formatted") ||
      !is.data.frame(wth_formatted) ||
      nrow(wth_formatted) < 185,
    "Need extended weather data for epicrop_sim tests"
  )
})

# ---- Shared constants ------------------------------------------------------

.emergence <- "2000-07-01"

RcA_test <- cbind(c(0, 30, 60, 90, 120), c(1.0, 0.9, 0.7, 0.5, 0.4))
RcT_test <- cbind(c(15, 20, 25, 30, 35), c(0.2, 0.6, 1.0, 0.8, 0.3))

# Columns required of ANY epicrop_sim model output
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

# ---- Helpers ---------------------------------------------------------------

# Accept commonly overridden params as formals to avoid duplicate arg names.
run_sim <- function(
  RcA = RcA_test,
  RcT = RcT_test,
  a = 3,
  rhlim = 90, # scalar (validator checks 0..100)
  rainlim = 0, # scalar
  simple_wetness = TRUE,
  age_driver = "day",
  thermal_time = NULL, # only used when age_driver == "dvs"
  RRLEX = 0,
  ...
) {
  args <- list(
    wth = wth_formatted,
    emergence = .emergence,
    onset = 20,
    duration = 120,
    H0 = 100,
    I0 = 1,
    RcA = RcA,
    RcT = RcT,
    RcOpt = 0.8,
    p = 5,
    i = 20,
    Sx = 10000,
    RRS = 0.01,
    RRG = 0.1,
    a = a,
    rhlim = rhlim,
    rainlim = rainlim,
    simple_wetness = simple_wetness,
    age_driver = age_driver,
    RRLEX = RRLEX
  )

  # Only pass thermal_time when DVS is explicitly requested
  if (identical(age_driver, "dvs")) {
    args$thermal_time <- thermal_time
  }

  do.call(epicrop_sim, c(args, list(...)))
}

expect_valid_epicrop <- function(x, n_rows = 120L) {
  testthat::expect_s3_class(x, c("epicrop.sim", "data.table", "data.frame"))
  testthat::expect_identical(nrow(x), n_rows)
  testthat::expect_true(all(.expected_cols %in% names(x)))
  testthat::expect_true(all(x$intensity >= 0 & x$intensity <= 1))
  testthat::expect_gt(attr(x, "AUDPC"), 0)
  invisible(x)
}

# ---- Core parameter combinations ------------------------------------------

testthat::test_that("epicrop_sim runs under parameter variations", {
  variants <- list(
    basic = list(),
    complex_wetness = list(simple_wetness = FALSE),
    dvs_driver = list(
      RcA = cbind(
        c(0, 0.5, 1.0, 1.5, 2.0),
        c(1.0, 0.9, 0.8, 0.7, 0.6)
      ),
      age_driver = "dvs",
      thermal_time = list(base_temp = 0, gdd_to_maturity = 1100)
    ),
    lesion_expansion = list(RRLEX = 0.05)
  )

  for (nm in names(variants)) {
    res <- do.call(run_sim, variants[[nm]])
    expect_valid_epicrop(res)

    if (nm == "lesion_expansion") {
      # Weather-dependent: don't force > 0; validate capability & non-negativity
      testthat::expect_true("rlex" %in% names(res))
      testthat::expect_true(is.numeric(res$rlex))
      testthat::expect_true(all(res$rlex >= 0))
    }
  }
})

# ---- Disease progression sanity -------------------------------------------

testthat::test_that("epicrop_sim produces valid disease progression", {
  res <- run_sim()
  testthat::expect_true(all(res$sites >= 0))
  testthat::expect_true(all(res$latent >= 0))
  testthat::expect_true(all(res$infectious >= 0))
  testthat::expect_true(all(res$intensity >= 0 & res$intensity <= 1))
})

# ---- Reproducibility -------------------------------------------------------

testthat::test_that("epicrop_sim is reproducible", {
  r1 <- run_sim()
  r2 <- run_sim()
  testthat::expect_identical(r1, r2)
})
