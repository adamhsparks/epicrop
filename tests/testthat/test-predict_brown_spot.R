
test_that("Values are as expected", {
  wth <- epicrop:::wth
  bs <- predict_brown_spot(wth, emergence = "2000-07-01")

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
      "severity",
      "intensity",
      "lat",
      "lon"
    )
  )
  expect_is(bs, c("data.table", "data.frame"))
  expect_equal(nrow(bs), 121)

  # check total sites
  expect_equal(bs[[1, 3]], 600) # 600 default to start sites
  expect_equal(bs[[121, 3]], 84122.52, tolerance = 0.001)

  # check latent sites
  expect_equal(bs[[1, 4]], 0) # 600 default to start sites
  expect_equal(bs[[121, 4]], 2958.45, tolerance = 0.001)

  # check infectious sites
  expect_equal(bs[[1, 5]], 0)
  expect_equal(bs[[121, 5]], 2375.05, tolerance = 0.001)

  # check removed sites
  expect_equal(bs[[1, 6]], 0)
  expect_equal(bs[[121, 6]], 182.56, tolerance = 0.001)

  # check senesced sites
  expect_equal(bs[[1, 7]], 0)
  expect_equal(bs[[121, 7]], 55944.43, tolerance = 0.001)

  # check rateinf
  expect_equal(bs[[1, 8]], 0)
  expect_equal(bs[[121, 8]], 1349.43, tolerance = 0.001)

  # check rtransfer
  expect_equal(bs[[1, 9]], 0)
  expect_equal(bs[[120, 9]], 364.85, tolerance = 0.001)

  # check rgrowth
  expect_equal(bs[[1, 10]], 59.64, tolerance = 0.001)
  expect_equal(bs[[121, 10]], 871.63, tolerance = 0.001)

  # check rsenesced
  expect_equal(bs[[1, 11]], 6)
  expect_equal(bs[[121, 11]], 841.23, tolerance = 0.001)

  # check diseased
  expect_equal(bs[[1, 12]], 0)
  expect_equal(bs[[121, 12]], 5516.06, tolerance = 0.001)

  # check severity values
  expect_equal(bs[[1, 13]], 0)
  expect_equal(bs[[121, 13]], 6.15, tolerance = 0.001)

  # check intensity values
  expect_equal(bs[[1, 14]], 0)
  expect_equal(bs[[121, 14]], 5.96, tolerance = 0.001)

  # check lat/lon values
  expect_equal(bs[[1, 15]], 14.67, tolerance = 0.001)
  expect_equal(bs[[1, 16]], 121.25, tolerance = 0.001)
})
