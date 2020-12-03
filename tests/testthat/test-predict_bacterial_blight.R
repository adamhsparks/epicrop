
context("predict_bacterial_blight()")
test_that("Values are as expected", {
   wth <- get_wth(
     lonlat = c(121.25562, 14.6774),
     dates = c("2000-05-15", "2000-12-31")
   )
   bb <- predict_bacterial_blight(wth, emergence = "2000-05-18")

  expect_named(
    bb,
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
  expect_is(bb, c("data.table", "data.frame"))
  expect_equal(nrow(bb), 121)

  # check total sites
  expect_equal(bb[[1, 3]], 100) # 600 default to start sites
  expect_equal(bb[[121, 3]], 1387.2, tolerance = 0.1)

  # check latent sites
  expect_equal(bb[[1, 4]], 0) # 600 default to start sites
  expect_equal(bb[[121, 4]], 110.2, tolerance = 0.1)

  # check infectious sites
  expect_equal(bb[[1, 5]], 0)
  expect_equal(bb[[121, 5]], 698.3, tolerance = 0.1)

  # check removed sites
  expect_equal(bb[[1, 6]], 0)
  expect_equal(bb[[121, 6]], 434.7, tolerance = 0.1)

  # check senesced sites
  expect_equal(bb[[1, 7]], 0)
  expect_equal(bb[[121, 7]], 2411.816, tolerance = 0.001)

  # check rateinf
  expect_equal(bb[[1, 8]], 0)
  expect_equal(bb[[121, 8]], 16.04, tolerance = 0.001)

  # check rtransfer
  expect_equal(bb[[1, 9]], 0)
  expect_equal(bb[[121, 9]], 28.30, tolerance = 0.01)

  # check rgrowth
  expect_equal(bb[[1, 10]], 9.688, tolerance = 0.001)
  expect_equal(bb[[121, 10]], 24.687, tolerance = 0.001)

  # check rsenesced
  expect_equal(bb[[1, 11]], 1)
  expect_equal(bb[[121, 11]], 13.872, tolerance = 0.001)

  # check diseased
  expect_equal(bb[[1, 12]], 0)
  expect_equal(bb[[121, 12]], 1243, tolerance = 0.1)

  # check severity values
  expect_equal(bb[[1, 13]], 0)
  expect_equal(bb[[121, 13]], 36.82, tolerance = 0.01)

  # check lat/lon values
  expect_equal(bb[[1, 14]], 14.68, tolerance = 0.01)
  expect_equal(bb[[1, 15]], 121.3, tolerance = 0.1)
})
