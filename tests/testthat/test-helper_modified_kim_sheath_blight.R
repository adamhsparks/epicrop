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

  expect_is(sb, c("data.table", "data.frame"))
  expect_identical(nrow(sb), 100L)

  expect_identical(sb[[1L, "sites"]], 90.0)
  expect_equal(sb[[100L, "sites"]], 127.061, tolerance = 0.1)

  expect_equal(sb[[1L, "latent"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[100L, "latent"]], 14.3, tolerance = 0.1)

  expect_equal(sb[[1L, "infectious"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[100L, "infectious"]], 360.9595, tolerance = 0.1)

  expect_equal(sb[[1L, "removed"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[100L, "removed"]], 0.0, tolerance = 0.1)

  expect_equal(sb[[1L, "senesced"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[100L, "senesced"]], 211.96, tolerance = 0.1)

  expect_equal(sb[[1L, "rateinf"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[100L, "rateinf"]], 3.654148, tolerance = 0.01)

  expect_equal(sb[[1L, "rtransfer"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[100L, "rtransfer"]], 0.0, tolerance = 0.1)

  expect_equal(sb[[1L, "rgrowth"]], 11.98125, tolerance = 0.1)
  expect_equal(sb[[100L, "rgrowth"]], 0.004725293, tolerance = 0.1)

  expect_equal(sb[[1L, "rsenesced"]], 0.45, tolerance = 0.1)
  expect_equal(sb[[100L, "rsenesced"]], 0.635305, tolerance = 0.1)

  expect_equal(sb[[1L, "diseased"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[100L, "diseased"]], 375.4256, tolerance = 0.1)

  expect_equal(sb[[1L, "intensity"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[100L, "intensity"]], 0.7, tolerance = 0.1)

  expect_equal(sb[[1L, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(sb[[1L, "lon"]], 121.2556, tolerance = 0.01)
})
