test_that("Values are as expected", {
  withr::local_options(list(scipen = 999L))
  # wth_formatted is loaded in setup-testdata.R
  tg <- tungro(wth_formatted, emergence = "2000-07-01")

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
  expect_named(tg, expected_names, ignore.order = FALSE)
  expect_s3_class(tg, c("data.table", "data.frame"))
  expect_identical(nrow(tg), 120L)

  tg_snap <- as.data.frame(tg)
  tg_snap$dates <- as.character(tg_snap$dates)

  num <- vapply(tg_snap, is.numeric, logical(1))
  tg_snap[num] <- lapply(tg_snap[num], \(x) round(x, 4))

  expect_snapshot_value(tg_snap, style = "json2")
})
