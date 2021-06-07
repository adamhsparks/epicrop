
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
      "severity",
      "lat",
      "lon"
    )
  )
  expect_is(sb, c("data.table", "data.frame"))
  expect_equal(nrow(sb), 121)

  # check total sites
  expect_equal(sb[[1, 3]], 25.00)
  expect_equal(sb[[121, 3]], 166, tolerance = 0.1)

  # check latent sites
  expect_equal(sb[[1, 4]], 0)
  expect_equal(sb[[121, 4]], 11.95, tolerance = 0.01)

  # check infectious sites
  expect_equal(sb[[1, 5]], 0)
  expect_equal(sb[[121, 5]], 602.01, tolerance = 0.1)

  # check removed sites
  expect_equal(sb[[1, 6]], 0)
  expect_equal(sb[[121, 6]], 0, tolerance = 0.1)

  # check senesced sites
  expect_equal(sb[[1, 7]], 0)
  expect_equal(sb[[121, 7]], 281.7, tolerance = 0.1)

  # check rateinf
  expect_equal(sb[[1, 8]], 0)
  expect_equal(sb[[121, 8]], 3.486, tolerance = 0.01)

  # check rtransfer
  expect_equal(sb[[1, 9]], 0)
  expect_equal(sb[[121, 9]], 4.256, tolerance = 0.001)

  # check rgrowth
  expect_equal(sb[[1, 10]], 4.8438, tolerance = 0.01)
  expect_equal(sb[[121, 10]], 0.8299, tolerance = 0.0001)

  # check rsenesced
  expect_equal(sb[[1, 11]], 0.1250)
  expect_equal(sb[[121, 11]], 0.8299, tolerance = 0.0001)

  # check diseased
  expect_equal(sb[[1, 12]], 0)
  expect_equal(sb[[121, 12]], 614, tolerance = 0.1)

  # check severity values
  expect_equal(sb[[1, 13]], 0)
  expect_equal(sb[[121, 13]], 78.72, tolerance = 0.01)

  # check lat/lon values
  expect_equal(sb[[1, 14]], 14.68, tolerance = 0.01)
  expect_equal(sb[[1, 15]], 121.3, tolerance = 0.1)
})
