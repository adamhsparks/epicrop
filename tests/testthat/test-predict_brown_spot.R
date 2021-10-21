
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
      "intensity",
      "audpc",
      "lat",
      "lon"
    )
  )

  expect_is(bs, c("data.table", "data.frame"))
  expect_equal(nrow(bs), 120)

  expect_equal(bs[[1, "sites"]], 600)
  expect_equal(bs[[120, "sites"]], 84949.3, tolerance = 0.1)

  expect_equal(bs[[1, "latent"]], 0, tolerance = 0.1)
  expect_equal(bs[[120, "latent"]], 1670, tolerance = 0.1)

  expect_equal(bs[[1, "infectious"]], 0, tolerance = 0.1)
  expect_equal(bs[[120, "infectious"]], 1183, tolerance = 0.1)

  expect_equal(bs[[1, "removed"]], 0, tolerance = 0.1)
  expect_equal(bs[[120, "removed"]], 97.8, tolerance = 0.1)

  expect_equal(bs[[1, "senesced"]], 0, tolerance = 0.1)
  expect_equal(bs[[120, "senesced"]], 55963.4, tolerance = 0.1)

  expect_equal(bs[[1, "rateinf"]], 0, tolerance = 0.1)
  expect_equal(bs[[120, "rateinf"]], 679, tolerance = 0.1)

  expect_equal(bs[[1, "rtransfer"]], 0, tolerance = 0.1)
  expect_equal(bs[[120, "rtransfer"]], 189, tolerance = 0.1)

  expect_equal(bs[[1, "rgrowth"]], 59.6, tolerance = 0.1)
  expect_equal(bs[[120, "rgrowth"]], 877.2, tolerance = 0.1)

  expect_equal(bs[[1, "rsenesced"]], 6, tolerance = 0.1)
  expect_equal(bs[[120, "rsenesced"]], 887.3, tolerance = 0.1)

  expect_equal(bs[[1, "diseased"]], 0, tolerance = 0.1)
  expect_equal(bs[[120, "diseased"]], 2951, tolerance = 0.1)

  expect_equal(bs[[1, "intensity"]], 0, tolerance = 0.1)
  expect_equal(bs[[120, "intensity"]], 0.03, tolerance = 0.01)

  expect_equal(bs[[1, "lat"]], 14.67741, tolerance = 0.00001)
  expect_equal(bs[[1, "lon"]], 121.2556, tolerance = 0.0001)
})
