context("utility functions")

test_that("calculation of CIce is correct", {

  x <- c(273, 273 - 45)

  actual <- CIce(x)
  expected <- 152.5 + 7.122 * x

  expect_equal(actual, expected)

})

test_that("calculation of KIce is correct", {

  x <- c(273.15, 273.15 - 45)

  actual <- round(KIce(x), 3)
  expected <- round(2.22 * (1 - 0.0067 * (x - 273.15)), 3)

  expect_equal(actual, expected)

})

test_that("calculation of KappaIce is correct", {

  x <- c(273.15, 273.15 - 45)

  actual <- KappaIce(x)
  expected <- 2.22 * (1 - 0.0067 * (x - 273.15)) / (920. * (152.5 + 7.122 * x))

  expect_equal(actual, expected)

})

test_that("calculation of KFirn is correct", {

  x <- c(273.15, 273.15 - 45)
  rho <- c(350, 500)

  m <- "'T' and 'rho' must have equal lengths, if both have lengths > 1."

  expect_error(KFirn(x, c(rho, 800)), m, fixed = TRUE)
  expect_error(KFirn(c(x, 273.15 - 60), rho), m, fixed = TRUE)

  actual <- KFirn(x, rho)
  expected <- 2.22 * (1 - 0.0067 * (x - 273.15)) *
    (rho / 920.)^(2 - 0.5 * (rho / 920.))

  expect_equal(actual, expected)

  actual <- KFirn(x, rho[1])
  expected <- 2.22 * (1 - 0.0067 * (x - 273.15)) *
    (rho[1] / 920.)^(2 - 0.5 * (rho[1] / 920.))

  expect_equal(actual, expected)

  actual <- KFirn(x[2], rho)
  expected <- 2.22 * (1 - 0.0067 * (x[2] - 273.15)) *
    (rho / 920.)^(2 - 0.5 * (rho / 920.))

  expect_equal(actual, expected)

})

test_that("calculation of KappaFirn is correct", {

  x <- c(273.15, 273.15 - 45)
  rho <- c(350, 500)

  m <- "'T' and 'rho' must have equal lengths, if both have lengths > 1."

  expect_error(KappaFirn(x, c(rho, 800)), m, fixed = TRUE)
  expect_error(KappaFirn(c(x, 273.15 - 60), rho), m, fixed = TRUE)

  expected <- KFirn(x, rho[2]) / (rho[2] * CIce(x))
  actual <- KappaFirn(x, rho[2])

  expect_equal(actual, expected)

  expect_equal(KappaIce(x), KappaFirn(x, 920.))

})

test_that("temperature-isotope conversion function is correct", {

  expect_equal(Temperature2Isotopes(-44.5, 0.8, -8.1), 0.8 * (-44.5) - 8.1)

})
