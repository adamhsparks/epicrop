
# saturated vapour pressure ----------------------------------------------------
context("es calculations")
test_that("es is properly calculated", {
 expect_equal(.saturated_vapor_pressure(0), 0.611, tolerance = 0.001)
})
