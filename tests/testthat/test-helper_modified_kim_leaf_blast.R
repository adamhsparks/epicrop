test_that("Values are as expected", {
  withr::local_options(list(scipen = 999L))
  # wth_formatted is loaded in setup-testdata.R
  mk_lb <- modified_kim_leaf_blast(wth_formatted, emergence = "2000-07-01")

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
  expect_named(mk_lb, expected_names, ignore.order = FALSE)
  expect_s3_class(mk_lb, c("data.table", "data.frame"))
  expect_identical(nrow(mk_lb), 77L)

  mk_lb_snap <- as.data.frame(mk_lb)
  mk_lb_snap$dates <- as.character(mk_lb_snap$dates)

  num <- vapply(mk_lb_snap, is.numeric, logical(1))
  mk_lb_snap[num] <- lapply(mk_lb_snap[num], \(x) round(x, 4))

  expect_snapshot_value(mk_lb_snap, style = "json2")
})
