no_weight_default <- .calculate_leaf_wetness(
  weighted_wetness = FALSE,
  wth = wth_formatted,
  rhlim = 90.0,
  rainlim = 5.0
)

no_weight_low_limit <- .calculate_leaf_wetness(
  weighted_wetness = FALSE,
  wth = wth_formatted,
  rhlim = 80.0,
  rainlim = 1.0
)

no_weight_high_limit <- .calculate_leaf_wetness(
  weighted_wetness = FALSE,
  wth = wth_formatted,
  rhlim = 95.0,
  rainlim = 10.0
)

weighted_default <- .calculate_leaf_wetness(
  weighted_wetness = TRUE,
  wth = wth_formatted,
  rhlim = 90.0,
  rainlim = 5.0
)

test_that("a vector of daily summed hourly values is returned w/ no weights", {
  expect_length(no_weight_default, 185L)
  expect_equal(no_weight_default[[1L]], 13.101, tolerance = 0.1)
})

test_that("Values increase as limits decrease w/ no weights", {
  expect_length(no_weight_low_limit, 185L)
  expect_equal(no_weight_low_limit[[1L]], 20.7250, tolerance = 0.1)
  expect_gt(no_weight_low_limit[[1L]], no_weight_default[[1L]])
})


test_that("Values decrease as limits increase w/ no weights", {
  expect_length(no_weight_high_limit, 185L)
  expect_equal(no_weight_high_limit[[1L]], 9.688125, tolerance = 0.1)
  expect_gt(no_weight_default[[1L]], no_weight_high_limit[[1L]])
})
