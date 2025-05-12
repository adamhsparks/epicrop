withr::local_options(list(scipen = 999L))

test_that("Values are as expected", {
  # wth_formatted is loaded in setup-testdata.R
  sb <- modified_kim_sheath_blight(wth_formatted, emergence = "2000-07-01")

  expect_named(
    sb,
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

  expect_s3_class(sb, c("data.table", "data.frame"))
  expect_identical(nrow(sb), 88L)

  expect_identical(sb[[1L, "sites"]], 90.0)
  expect_equal(sb[[88L, "sites"]], 172.1, tolerance = 0.1)

  expect_equal(sb[[1L, "latent"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[88L, "latent"]], 51.0, tolerance = 0.1)

  expect_equal(sb[[1L, "infectious"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[88L, "infectious"]], 288.3, tolerance = 0.1)

  expect_equal(sb[[1L, "removed"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[88L, "removed"]], 0.0, tolerance = 0.1)

  expect_equal(sb[[1L, "senesced"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[88L, "senesced"]], 211.96, tolerance = 0.1)

  expect_equal(sb[[1L, "rateinf"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[88L, "rateinf"]], 7.9, tolerance = 0.01)

  expect_equal(sb[[1L, "rtransfer"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[88L, "rtransfer"]], 11.3, tolerance = 0.1)

  expect_equal(sb[[1L, "rgrowth"]], 11.98125, tolerance = 0.1)
  expect_equal(sb[[88L, "rgrowth"]], 0.004725293, tolerance = 0.1)

  expect_equal(sb[[1L, "rsenesced"]], 0.45, tolerance = 0.1)
  expect_equal(sb[[88L, "rsenesced"]], 0.86, tolerance = 0.1)

  expect_equal(sb[[1L, "diseased"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[88L, "diseased"]], 375.4256, tolerance = 0.1)

  expect_equal(sb[[1L, "intensity"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[88L, "intensity"]], 0.7, tolerance = 0.1)

  expect_equal(sb[[1L, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(sb[[1L, "lon"]], 121.2556, tolerance = 0.01)
})

withr::deferred_clear()
