.emergence_date <- "2000-07-01"

.expected_names <- c(
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
  "intensity",
  "lat",
  "lon"
)

run_model <- function(
  fun,
  wth = wth_formatted,
  emergence = .emergence_date,
  ...
) {
  fun(wth, emergence = emergence, ...)
}

expect_epicrop_layout <- function(x, n_rows, expected_names = .expected_names) {
  expect_s3_class(x, c("epicrop.sim", "data.table", "data.frame"))
  expect_named(x, expected_names, ignore.order = FALSE)
  expect_identical(nrow(x), as.integer(n_rows))
  expect_true(all(x$intensity >= 0 & x$intensity <= 1))
  expect_gt(attr(x, "AUDPC"), 0)
  invisible(x)
}

# ---- Parameterized structure tests ----------------------------------------

test_that("all rice models produce expected output structure", {
  models <- list(
    bacterial_blight = 120L,
    brown_spot = 120L,
    leaf_blast = 120L,
    sheath_blight = 120L,
    tungro = 120L,
    modified_kim_leaf_blast = 77L,
    modified_kim_sheath_blight = 88L
  )

  for (nm in names(models)) {
    fn <- get(nm, mode = "function")
    res <- run_model(fn)
    expect_epicrop_layout(res, n_rows = models[[nm]])
  }
})

# ---- Aliases ---------------------------------------------------------------

test_that("rice model aliases work identically", {
  alias_pairs <- list(
    bacterial_blight = bb,
    brown_spot = bs,
    leaf_blast = lb,
    sheath_blight = sb,
    tungro = tg,
    modified_kim_leaf_blast = mk_lb,
    modified_kim_sheath_blight = mk_sb
  )

  for (nm in names(alias_pairs)) {
    ref <- run_model(get(nm))
    alt <- run_model(alias_pairs[[nm]])
    expect_identical(ref, alt)
  }
})

# ---- Snapshot (representative model only) ---------------------------------

test_that("rice model snapshot is stable", {
  withr::local_options(list(scipen = 999L))
  local_reproducible_output()

  bb <- run_model(bacterial_blight)
  snap <- as.data.frame(bb)

  snap$dates <- as.character(snap$dates)
  num <- vapply(snap, is.numeric, logical(1))
  snap[num] <- lapply(snap[num], round, 4)

  expect_snapshot_value(snap, style = "json2")
})

# ---- Smoke test ------------------------------------------------------------

test_that("bacterial_blight runs without errors", {
  res <- run_model(bacterial_blight)
  expect_s3_class(res, "epicrop.sim")
  expect_identical(nrow(res), 120L)
})

# ---- Core engine custom parameter acceptance -------------------------------

test_that("seir accepts all custom parameters (rice)", {
  RcA_custom <- cbind(
    c(0L, 30L, 60L, 90L, 120L),
    c(1.0, 0.9, 0.7, 0.5, 0.4)
  )

  RcT_custom <- cbind(
    c(16L, 22L, 28L, 34L, 40L),
    c(0.0, 0.5, 1.0, 0.5, 0.0)
  )

  res <- seir(
    wth = wth_formatted,
    emergence = .emergence_date,
    onset = 25,
    duration = 100,
    H0 = 200,
    I0 = 5,
    RcA = RcA_custom,
    RcT = RcT_custom,
    RcOpt = 0.9,
    p = 7,
    i = 25,
    Sx = 5000,
    a = 3,
    RRS = 0.02,
    RRG = 0.15
  )

  expect_s3_class(res, "epicrop.sim")
  expect_identical(nrow(res), 100L)
})

# ---- Deterministic reproducibility -----------------------------------------

test_that("rice models are reproducible", {
  r1 <- run_model(bacterial_blight)
  r2 <- run_model(bacterial_blight)
  expect_identical(r1, r2)
})

# ---- Custom RcA/RcT/RRG/RRS overrides actually change output --------------

test_that("rice model functions respect custom RcA/RcT/RRG/RRS overrides", {
  custom_RcA <- cbind(c(0, 120), c(0.1, 0.1))
  custom_RcT <- cbind(c(15, 35), c(0.05, 0.05))

  default_res <- run_model(bacterial_blight)
  custom_res <- run_model(
    bacterial_blight,
    RcA = custom_RcA,
    RcT = custom_RcT,
    RRG = 0.5,
    RRS = 0.5
  )

  expect_s3_class(custom_res, "epicrop.sim")
  expect_false(identical(default_res$intensity, custom_res$intensity))
})

test_that("modified_kim models respect custom RcA/RcT/RRG/RRS overrides", {
  default_res <- run_model(modified_kim_sheath_blight)
  custom_res <- run_model(
    modified_kim_sheath_blight,
    RRG = 0.5,
    RRS = 0.5
  )

  expect_s3_class(custom_res, "epicrop.sim")
  expect_false(identical(default_res$rgrowth, custom_res$rgrowth))
})

# ---- Defaults trace back to .epi_params via epi_get_params -----------------

test_that("rice model defaults match the underlying epi_params store", {
  models <- list(
    bacterial_blight = list(
      fun = bacterial_blight,
      crop = "rice",
      disease = "bacterial_blight"
    ),
    brown_spot = list(
      fun = brown_spot,
      crop = "rice",
      disease = "brown_spot"
    ),
    leaf_blast = list(
      fun = leaf_blast,
      crop = "rice",
      disease = "leaf_blast"
    ),
    sheath_blight = list(
      fun = sheath_blight,
      crop = "rice",
      disease = "sheath_blight"
    ),
    tungro = list(
      fun = tungro,
      crop = "rice",
      disease = "tungro"
    ),
    modified_kim_leaf_blast = list(
      fun = modified_kim_leaf_blast,
      crop = "rice_modified_kim",
      disease = "leaf_blast"
    ),
    modified_kim_sheath_blight = list(
      fun = modified_kim_sheath_blight,
      crop = "rice_modified_kim",
      disease = "sheath_blight"
    )
  )

  for (nm in names(models)) {
    mdl <- models[[nm]]
    params <- epi_get_params(mdl$crop, mdl$disease)

    default_res <- run_model(mdl$fun)
    explicit_res <- run_model(
      mdl$fun,
      RcA = params$RcA,
      RcT = params$RcT,
      RRG = params$RRG,
      RRS = params$RRS
    )

    expect_identical(default_res, explicit_res)
  }
})

# ---- ... passthrough: non-hardcoded args work, hardcoded ones collide -----

test_that("rice models accept non-hardcoded seir arguments via ...", {
  default_res <- run_model(brown_spot)
  custom_res <- run_model(brown_spot, rhlim = 95, rainlim = 10)

  expect_s3_class(custom_res, "epicrop.sim")
  expect_identical(nrow(custom_res), nrow(default_res))
})

test_that("hardcoded seir arguments cannot be overridden via ... (rice)", {
  # onset, duration, H0, I0, RcOpt, p, i, Sx, and a are all set explicitly
  # inside each wrapper's seir() call, so passing them again via `...`
  # produces a duplicate-argument error rather than silently overriding.
  expect_error(run_model(bacterial_blight, RcOpt = 0.5), "formal argument")
  expect_error(run_model(bacterial_blight, duration = 100), "formal argument")
  expect_error(run_model(leaf_blast, onset = 10), "formal argument")
})
