
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
      "lat",
      "lon"
    )
  )

  expect_is(sb, c("data.table", "data.frame"))
  expect_equal(nrow(sb), 121)

  expect_equal(sb[[1, "sites"]], 25)
  expect_equal(sb[[121, "sites"]], 165.1, tolerance = 0.1)

  expect_equal(sb[[1, "latent"]], 0, tolerance = 0.1)
  expect_equal(sb[[121, "latent"]], 11.6, tolerance = 0.1)

  expect_equal(sb[[1, "infectious"]], 0, tolerance = 0.1)
  expect_equal(sb[[121, "infectious"]], 603.1, tolerance = 0.1)

  expect_equal(sb[[1, "removed"]], 0, tolerance = 0.1)
  expect_equal(sb[[121, "removed"]], 0, tolerance = 0.1)

  expect_equal(sb[[1, "senesced"]], 0, tolerance = 0.1)
  expect_equal(sb[[121, "senesced"]], 281.8, tolerance = 0.1)

  expect_equal(sb[[1, "rateinf"]], 0, tolerance = 0.1)
  expect_equal(sb[[121, "rateinf"]], 3.4, tolerance = 0.1)

  expect_equal(sb[[1, "rtransfer"]], 0, tolerance = 0.1)
  expect_equal(sb[[121, "rtransfer"]], 4.1, tolerance = 0.1)

  expect_equal(sb[[1, "rgrowth"]], 4.8, tolerance = 0.1)
  expect_equal(sb[[121, "rgrowth"]], 0.8, tolerance = 0.1)

  expect_equal(sb[[1, "rsenesced"]], 0.1, tolerance = 0.1)
  expect_equal(sb[[121, "rsenesced"]], 0.8, tolerance = 0.1)

  expect_equal(sb[[1, "diseased"]], 0, tolerance = 0.1)
  expect_equal(sb[[121, "diseased"]], 614.8, tolerance = 0.1)

  expect_equal(sb[[1, "intensity"]], 0, tolerance = 0.1)
  expect_equal(sb[[121, "intensity"]], 0.7, tolerance = 0.1)

  expect_equal(sb[[1, "lat"]], 14.67741, tolerance = 0.00001)
  expect_equal(sb[[1, "lon"]], 121.2556, tolerance = 0.0001)
})
