
test_that("Values are as expected", {
  wth <- epicrop:::wth
  bb <- predict_bacterial_blight(wth, emergence = "2000-07-01")

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
      "intensity",
      "lat",
      "lon"
    )
  )
  expect_is(bb, c("data.table", "data.frame"))
  expect_equal(nrow(bb), 121)

  # check total sites
  expect_equal(bb[[1, 3]], 100) # 600 default to start sites
  expect_equal(bb[[121, 3]], 1082, tolerance = 0.1)

  # check latent sites
  expect_equal(bb[[1, 4]], 0) # 600 default to start sites
  expect_equal(bb[[121, 4]], 37, tolerance = 0.1)

  # check infectious sites
  expect_equal(bb[[1, 5]], 0)
  expect_equal(bb[[121, 5]], 812.2, tolerance = 0.1)

  # check removed sites
  expect_equal(bb[[1, 6]], 0)
  expect_equal(bb[[121, 6]], 584, tolerance = 0.1)

  # check senesced sites
  expect_equal(bb[[1, 7]], 0)
  expect_equal(bb[[121, 7]], 2452, tolerance = 0.1)

  # check rateinf
  expect_equal(bb[[1, 8]], 0)
  expect_equal(bb[[121, 8]], 9.05, tolerance = 0.1)

  # check rtransfer
  expect_equal(bb[[1, 9]], 0)
  expect_equal(bb[[118, 9]], 18.7, tolerance = 0.1)

  # check rgrowth
  expect_equal(bb[[1, 10]], 9.688, tolerance = 0.1)
  expect_equal(bb[[121, 10]], 24.6, tolerance = 0.1)

  # check rsenesced
  expect_equal(bb[[1, 11]], 1)
  expect_equal(bb[[121, 11]], 54.158, tolerance = 0.1)

  # check diseased
  expect_equal(bb[[1, 12]], 0)
  expect_equal(bb[[121, 12]], 1292, tolerance = 0.1)

  # check intensity values
  expect_equal(bb[[1, 13]], 0)
  expect_equal(bb[[121, 13]], 40.17, tolerance = 0.1)

  # check lat/lon values
  expect_equal(bb[[1, 14]], 14.68, tolerance = 0.1)
  expect_equal(bb[[1, 15]], 121.3, tolerance = 0.1)
})
