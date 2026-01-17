# Test common behavior across all disease models
test_that("all disease models produce expected output structure", {
  models <- list(
    bacterial_blight = list(fn = bacterial_blight, duration = 120L),
    brown_spot = list(fn = brown_spot, duration = 120L),
    leaf_blast = list(fn = leaf_blast, duration = 120L),
    sheath_blight = list(fn = sheath_blight, duration = 120L),
    tungro = list(fn = tungro, duration = 120L),
    modified_kim_leaf_blast = list(
      fn = modified_kim_leaf_blast,
      duration = 77L
    ),
    modified_kim_sheath_blight = list(
      fn = modified_kim_sheath_blight,
      duration = 88L
    )
  )

  expected_names <- c(
    "simday",
    "dates",
    "sites",
    "latent",
    "infectious",
    "removed",
    "senesced",
    "rateinf",
    "rtransfer",
    "rgrowth",
    "rsenesced",
    "diseased",
    "intensity",
    "AUDPC",
    "lat",
    "lon"
  )

  for (model_name in names(models)) {
    out <- models[[model_name]]$fn(wth_formatted, emergence = "2000-07-01")

    expect_named(
      out,
      expected_names,
      ignore.order = FALSE,
      info = paste(model_name, "column names")
    )
    expect_s3_class(
      out,
      c("data.table", "data. frame")
    )
    expect_identical(
      nrow(out),
      models[[model_name]]$duration,
      info = paste(model_name, "row count")
    )
    expect_true(
      all(out$intensity >= 0 & out$intensity <= 1),
      info = paste(model_name, "intensity bounds")
    )
    expect_true(out$AUDPC[1] > 0, info = paste(model_name, "AUDPC calculated"))
  }
})

test_that("shorthand aliases work identically to full functions", {
  expect_identical(
    bacterial_blight(wth_formatted, emergence = "2000-07-01"),
    bb(wth_formatted, emergence = "2000-07-01")
  )

  expect_identical(
    brown_spot(wth_formatted, emergence = "2000-07-01"),
    bs(wth_formatted, emergence = "2000-07-01")
  )

  expect_identical(
    leaf_blast(wth_formatted, emergence = "2000-07-01"),
    lb(wth_formatted, emergence = "2000-07-01")
  )

  expect_identical(
    sheath_blight(wth_formatted, emergence = "2000-07-01"),
    sb(wth_formatted, emergence = "2000-07-01")
  )

  expect_identical(
    modified_kim_leaf_blast(wth_formatted, emergence = "2000-07-01"),
    mk_lb(wth_formatted, emergence = "2000-07-01")
  )

  expect_identical(
    modified_kim_sheath_blight(wth_formatted, emergence = "2000-07-01"),
    mk_sb(wth_formatted, emergence = "2000-07-01")
  )
})

test_that("all disease models handle edge cases consistently", {
  # Test with minimal weather data
  wth_minimal <- wth_formatted[1:60, ]

  expect_error(
    bacterial_blight(wth_minimal, emergence = "2000-07-01"),
    regexp = "Incomplete weather|dates"
  )

  expect_error(
    brown_spot(wth_minimal, emergence = "2000-07-01"),
    regexp = "Incomplete weather|dates"
  )
})
