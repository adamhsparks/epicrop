
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
  expect_equal(nrow(bb), 120)

  expect_equal(bb[[1, "sites"]], 100)
  expect_equal(bb[[120, "sites"]], 1074.7, tolerance = 0.1)

  expect_equal(bb[[1, "latent"]], 0, tolerance = 0.1)
  expect_equal(bb[[120, "latent"]], 31.2, tolerance = 0.1)

  expect_equal(bb[[1, "infectious"]], 0, tolerance = 0.1)
  expect_equal(bb[[120, "infectious"]], 762.6, tolerance = 0.1)

  expect_equal(bb[[1, "removed"]], 0, tolerance = 0.1)
  expect_equal(bb[[120, "removed"]], 593.5, tolerance = 0.1)

  expect_equal(bb[[1, "senesced"]], 0, tolerance = 0.1)
  expect_equal(bb[[120, "senesced"]], 2427.0, tolerance = 0.1)

  expect_equal(bb[[1, "rateinf"]], 0, tolerance = 0.1)
  expect_equal(bb[[120, "rateinf"]], 7.8, tolerance = 0.1)

  expect_equal(bb[[1, "rtransfer"]], 0, tolerance = 0.1)
  expect_equal(bb[[120, "rtransfer"]], 0, tolerance = 0.1)

  expect_equal(bb[[1, "rgrowth"]], 9.6, tolerance = 0.1)
  expect_equal(bb[[120, "rgrowth"]], 24.6, tolerance = 0.1)

  expect_equal(bb[[1, "rsenesced"]], 1, tolerance = 0.1)
  expect_equal(bb[[120, "rsenesced"]], 59.3, tolerance = 0.1)

  expect_equal(bb[[1, "diseased"]], 0, tolerance = 0.1)
  expect_equal(bb[[120, "diseased"]], 1392.4, tolerance = 0.1)

  expect_equal(bb[[1, "intensity"]], 0, tolerance = 0.1)
  expect_equal(bb[[120, "intensity"]], 0.43, tolerance = 0.01)

  expect_equal(bb[[1, "lat"]], 14.67741, tolerance = 0.00001)
  expect_equal(bb[[1, "lon"]], 121.2556, tolerance = 0.0001)
})
