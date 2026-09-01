# ---- Coverage of every crop/disease pair -----------------------------------

test_that("epi_get_params retrieves parameters for every crop/disease pair", {
  crops <- names(.epi_params)
  expect_gt(length(crops), 0)

  for (crop in crops) {
    diseases <- names(.epi_params[[crop]])
    expect_gt(length(diseases), 0)

    for (disease in diseases) {
      p <- epi_get_params(crop, disease)
      expect_type(p, "list")
      expect_true(all(c("RcA", "RcT", "RRG") %in% names(p)))
      # Rice diseases use RRS; wheat diseases use RRPSN
      expect_true("RRS" %in% names(p) || "RRPSN" %in% names(p))
    }
  }
})

test_that("epi_get_params RcA/RcT tables are 2-column numeric matrices", {
  for (crop in names(.epi_params)) {
    for (disease in names(.epi_params[[crop]])) {
      p <- epi_get_params(crop, disease)
      for (nm in c("RcA", "RcT")) {
        tbl <- p[[nm]]
        expect_true(is.matrix(tbl) || is.data.frame(tbl))
        expect_identical(ncol(tbl), 2L)
        expect_type(tbl[, 1], "double")
        expect_type(tbl[, 2], "double")
      }
    }
  }
})

test_that("epi_get_params RRG/RRS/RRPSN entries are scalar or 2-col tables", {
  for (crop in names(.epi_params)) {
    for (disease in names(.epi_params[[crop]])) {
      p <- epi_get_params(crop, disease)
      rate_names <- intersect(c("RRG", "RRS", "RRPSN"), names(p))
      for (nm in rate_names) {
        val <- p[[nm]]
        is_scalar <- is.numeric(val) && length(val) == 1L
        is_table <- (is.matrix(val) || is.data.frame(val)) && ncol(val) == 2L
        expect_true(
          is_scalar || is_table,
          info = sprintf(
            "%s/%s/%s is neither scalar nor 2-col table",
            crop,
            disease,
            nm
          )
        )
      }
    }
  }
})

# ---- Lookup behaviour --------------------------------------------------

test_that("epi_get_params is case-insensitive for crop and disease", {
  p_lower <- epi_get_params("rice", "leaf_blast")
  p_upper <- epi_get_params("RICE", "LEAF_BLAST")
  p_mixed <- epi_get_params("Rice", "Leaf_Blast")

  expect_identical(p_lower, p_upper)
  expect_identical(p_lower, p_mixed)
})

test_that("epi_get_params returns the exact objects stored in .epi_params", {
  expect_identical(
    epi_get_params("rice", "tungro"),
    .epi_params$rice$tungro
  )
  expect_identical(
    epi_get_params("wheat", "s_tritici_blotch"),
    .epi_params$wheat$s_tritici_blotch
  )
  expect_identical(
    epi_get_params("rice_modified_kim", "sheath_blight"),
    .epi_params$rice_modified_kim$sheath_blight
  )
})

# ---- Error handling ----------------------------------------------------

test_that("epi_get_params errors informatively for unknown crop", {
  expect_error(epi_get_params("barley", "leaf_blast"), "Unknown crop")
  expect_error(epi_get_params("barley", "leaf_blast"), "barley")
})

test_that("epi_get_params errors informatively for unknown disease", {
  expect_error(epi_get_params("rice", "stem_rust"), "Unknown disease")
  expect_error(epi_get_params("rice", "stem_rust"), "stem_rust")
})

test_that("epi_get_params error messages list the available options", {
  err <- tryCatch(
    epi_get_params("wheat", "not_a_disease"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "leaf_rust")
  expect_match(conditionMessage(err), "s_tritici_blotch")

  err2 <- tryCatch(
    epi_get_params("not_a_crop", "x"),
    error = function(e) e
  )
  expect_match(conditionMessage(err2), "rice")
  expect_match(conditionMessage(err2), "wheat")
})

# ---- .epi_params is internal, not part of the public API ------------------

test_that(".epi_params is not exported from the package namespace", {
  expect_false(".epi_params" %in% getNamespaceExports("epicrop"))
})
