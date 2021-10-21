
test_that("Values are as expected", {
  wth <- epicrop:::wth
  sb <- predict_sheath_blight(wth, emergence = "2000-07-01")

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
      "audpc",
      "lat",
      "lon"
    )
  )

  expect_is(sb, c("data.table", "data.frame"))
  expect_equal(nrow(sb), 120)

  expect_equal(sb[[1, "sites"]], 25)
  expect_equal(sb[[120, "sites"]], 165.1, tolerance = 0.1)

  expect_equal(sb[[1, "latent"]], 0, tolerance = 0.1)
  expect_equal(sb[[120, "latent"]], 14.3, tolerance = 0.1)

  expect_equal(sb[[1, "infectious"]], 0, tolerance = 0.1)
  expect_equal(sb[[120, "infectious"]], 603.1, tolerance = 0.1)

  expect_equal(sb[[1, "removed"]], 0, tolerance = 0.1)
  expect_equal(sb[[120, "removed"]], 0, tolerance = 0.1)

  expect_equal(sb[[1, "senesced"]], 0, tolerance = 0.1)
  expect_equal(sb[[120, "senesced"]], 281.8, tolerance = 0.1)

  expect_equal(sb[[1, "rateinf"]], 0, tolerance = 0.1)
  expect_equal(sb[[120, "rateinf"]], 4.14, tolerance = 0.01)

  expect_equal(sb[[1, "rtransfer"]], 0, tolerance = 0.1)
  expect_equal(sb[[120, "rtransfer"]], 0, tolerance = 0.1)

  expect_equal(sb[[1, "rgrowth"]], 4.8, tolerance = 0.1)
  expect_equal(sb[[120, "rgrowth"]], 0.8, tolerance = 0.1)

  expect_equal(sb[[1, "rsenesced"]], 0.1, tolerance = 0.1)
  expect_equal(sb[[120, "rsenesced"]], 0.8, tolerance = 0.1)

  expect_equal(sb[[1, "diseased"]], 0, tolerance = 0.1)
  expect_equal(sb[[120, "diseased"]], 614.8, tolerance = 0.1)

  expect_equal(sb[[1, "intensity"]], 0, tolerance = 0.1)
  expect_equal(sb[[120, "intensity"]], 0.7, tolerance = 0.1)

  expect_equal(sb[[1, "lat"]], 14.67741, tolerance = 0.00001)
  expect_equal(sb[[1, "lon"]], 121.2556, tolerance = 0.0001)
})
