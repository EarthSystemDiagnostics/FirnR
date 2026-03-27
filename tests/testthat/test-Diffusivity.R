test_that("diffusivity calculation is correct", {

  expected <- 2.148976
  actual <- round(Diffusivity(350, 273, 650) * 1e10, 6)

  expect_equal(actual, expected)

  expected <- 1.933631
  actual <- round(Diffusivity(350, 273, 650, dD = TRUE) * 1e10, 6)

  expect_equal(actual, expected)


})
