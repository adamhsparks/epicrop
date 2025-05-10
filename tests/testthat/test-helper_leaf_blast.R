withr::local_options(list(scipen = 999L))

test_that("Values are as expected", {
  # wth_formatted is loaded in setup-testdata.R
  lb <- leaf_blast(wth_formatted, emergence = "2000-07-01")

  expect_named(
    lb,
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

  expect_is(lb, c("data.table", "data.frame"))
  expect_identical(nrow(lb), 120L)

  expect_equal(lb[[1L, "sites"]], 600.0)
  expect_equal(lb[[120L, "sites"]], 26932.6, tolerance = 0.1)

  expect_equal(lb[[1L, "latent"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[120L, "latent"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "infectious"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[120L, "infectious"]], 0.275, tolerance = 0.001)

  expect_equal(lb[[1L, "removed"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[120L, "removed"]], 38.4, tolerance = 0.1)

  expect_equal(lb[[1L, "senesced"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[120L, "senesced"]], 20606.0, tolerance = 0.1)

  expect_equal(lb[[1L, "rateinf"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[120L, "rateinf"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "rtransfer"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[120L, "rtransfer"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "rgrowth"]], 58.8, tolerance = 0.1)
  expect_equal(lb[[120L, "rgrowth"]], 271.4, tolerance = 0.1)

  expect_equal(lb[[1L, "rsenesced"]], 6.0, tolerance = 0.1)
  expect_equal(lb[[120L, "rsenesced"]], 269.3, tolerance = 0.1)

  expect_equal(lb[[1L, "diseased"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[120L, "diseased"]], 38.7, tolerance = 0.1)

  expect_equal(lb[[1L, "intensity"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[120L, "intensity"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(lb[[1L, "lon"]], 121.2556, tolerance = 0.01)
})

withr::deferred_clear()
