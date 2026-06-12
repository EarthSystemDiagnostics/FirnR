test_that("diffusion length calculation is correct", {

  # test based on cm units

  expected <- c(2.942327, 3.612284)
  actual <- round(
    1.e2 * DiffusionLength(depth = 0 : 1, rho = c(350, 400)), 6)

  expect_equal(actual, expected)

  expected <- c(2.675427, 3.284613)
  actual <- round(
    1.e2 * DiffusionLength(depth = 0 : 1, rho = c(350, 400), dD = TRUE), 6)

  expect_equal(actual, expected)

})
