test_that("conversion function is correct", {

  expect_equal(Temperature2Isotopes(-44.5, 0.8, -8.1), 0.8 * (-44.5) - 8.1)

})
