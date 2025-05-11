withr::local_options(list(scipen = 999L))

test_that("Values are as expected", {
  # wth_formatted is loaded in setup-testdata.R
  bb <- bacterial_blight(wth_formatted, emergence = "2000-07-01")

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
      "AUDPC",
      "lat",
      "lon"
    )
  )

  expect_is(bb, c("data.table", "data.frame"))
  expect_identical(nrow(bb), 120L)

  expect_equal(bb[[1L, "sites"]], 100.0)
  expect_equal(bb[[120L, "sites"]], 1286.3, tolerance = 0.1)

  expect_equal(bb[[1L, "latent"]], 0.0, tolerance = 0.1)
  expect_equal(bb[[120L, "latent"]], 64.1, tolerance = 0.1)

  expect_equal(bb[[1L, "infectious"]], 0.0, tolerance = 0.1)
  expect_equal(bb[[120L, "infectious"]], 926.1, tolerance = 0.1)

  expect_equal(bb[[1L, "removed"]], 0.0, tolerance = 0.1)
  expect_equal(bb[[120L, "removed"]], 352.6, tolerance = 0.1)

  expect_equal(bb[[1L, "senesced"]], 0.0, tolerance = 0.1)
  expect_equal(bb[[120L, "senesced"]], 2286.4, tolerance = 0.1)

  expect_equal(bb[[1L, "rateinf"]], 0.0, tolerance = 0.1)
  expect_equal(bb[[120L, "rateinf"]], 17.0, tolerance = 0.1)

  expect_equal(bb[[1L, "rtransfer"]], 0.0, tolerance = 0.1)
  expect_equal(bb[[120L, "rtransfer"]], 0.0, tolerance = 0.1)

  expect_equal(bb[[1L, "rgrowth"]], 9.6, tolerance = 0.1)
  expect_equal(bb[[120L, "rgrowth"]], 22.9, tolerance = 0.1)

  expect_equal(bb[[1L, "rsenesced"]], 1.0, tolerance = 0.1)
  expect_equal(bb[[120L, "rsenesced"]], 47.5, tolerance = 0.1)

  expect_equal(bb[[1L, "diseased"]], 0.0, tolerance = 0.1)
  expect_equal(bb[[120L, "diseased"]], 1342.9, tolerance = 0.1)

  expect_equal(bb[[1L, "intensity"]], 0.0, tolerance = 0.1)
  expect_equal(bb[[120L, "intensity"]], 0.43, tolerance = 0.1)

  expect_equal(bb[[1L, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(bb[[1L, "lon"]], 121.2556, tolerance = 0.01)
})
