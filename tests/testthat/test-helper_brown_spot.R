withr::local_options(list(scipen = 999L))

test_that("Values are as expected", {
  # wth_formatted is loaded in setup-testdata.R
  bs <- brown_spot(wth_formatted, emergence = "2000-07-01")

  expect_named(
    bs,
    c(
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
  )

  expect_is(bs, c("data.table", "data.frame"))
  expect_identical(nrow(bs), 120L)

  expect_equal(bs[[1L, "sites"]], 600.0)
  expect_equal(bs[[120L, "sites"]], 84949.3, tolerance = 0.1)

  expect_equal(bs[[1L, "latent"]], 0.0, tolerance = 0.1)
  expect_equal(bs[[120L, "latent"]], 1670.0, tolerance = 0.1)

  expect_equal(bs[[1L, "infectious"]], 0.0, tolerance = 0.1)
  expect_equal(bs[[120L, "infectious"]], 1183.0, tolerance = 0.1)

  expect_equal(bs[[1L, "removed"]], 0.0, tolerance = 0.1)
  expect_equal(bs[[120L, "removed"]], 97.8, tolerance = 0.1)

  expect_equal(bs[[1L, "senesced"]], 0.0, tolerance = 0.1)
  expect_equal(bs[[120L, "senesced"]], 55963.4, tolerance = 0.1)

  expect_equal(bs[[1L, "rateinf"]], 0.0, tolerance = 0.1)
  expect_equal(bs[[120L, "rateinf"]], 679.0, tolerance = 0.1)

  expect_equal(bs[[1L, "rtransfer"]], 0.0, tolerance = 0.1)
  expect_equal(bs[[120L, "rtransfer"]], 189.0, tolerance = 0.1)

  expect_equal(bs[[1L, "rgrowth"]], 59.6, tolerance = 0.1)
  expect_equal(bs[[120L, "rgrowth"]], 877.2, tolerance = 0.1)

  expect_equal(bs[[1L, "rsenesced"]], 6.0, tolerance = 0.1)
  expect_equal(bs[[120L, "rsenesced"]], 887.3, tolerance = 0.1)

  expect_equal(bs[[1L, "diseased"]], 0.0, tolerance = 0.1)
  expect_equal(bs[[120L, "diseased"]], 2951.0, tolerance = 0.1)

  expect_equal(bs[[1L, "intensity"]], 0.0, tolerance = 0.1)
  expect_equal(bs[[120L, "intensity"]], 0.03, tolerance = 0.01)

  expect_equal(bs[[1L, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(bs[[1L, "lon"]], 121.2556, tolerance = 0.01)
})

withr::deferred_clear()
