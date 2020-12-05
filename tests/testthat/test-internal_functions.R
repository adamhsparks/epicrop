
# saturated vapour pressure ----------------------------------------------------
context("es calculations")
test_that("es is properly calculated", {
 expect_equal(.saturated_vapour_pressure(0), 0.6109, tolerance = 0.0001)
})
