test_that("Values are as expected", {
  withr::local_options(list(scipen = 999L))
  # wth_formatted is loaded in setup-testdata.R
  bb <- bacterial_blight(wth_formatted, emergence = "2000-07-01")

  expected_names <- c(
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
  expect_named(bb, expected_names, ignore.order = FALSE)
  expect_s3_class(bb, c("data.table", "data.frame"))
  expect_identical(nrow(bb), 120L)

  bb_snap <- as.data.frame(bb)
  bb_snap$dates <- as.character(bb_snap$dates)

  num <- vapply(bb_snap, is.numeric, logical(1))
  bb_snap[num] <- lapply(bb_snap[num], \(x) round(x, 4))

  expect_snapshot_value(bb_snap, style = "json2")
})

test_that("bb() alias works identically", {
  full <- bacterial_blight(wth_formatted, emergence = "2000-07-01")
  alias <- bb(wth_formatted, emergence = "2000-07-01")

  expect_identical(full, alias)
})
