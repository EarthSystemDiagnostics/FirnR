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

  m <- "Lengths of 'T' and 'rho' must be equal if both are > 1."

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

  m <- "Lengths of 'T' and 'rho' must be equal if both are > 1."

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

test_that("saturation vapour pressure calculation is correct", {

  x <- c(273.15, 273.15 - 45)

  expected <- exp(9.5504 - 5723.265 / x + 3.53 * log(x) - 0.0073 * x)
  actual <- pSat(x)

  expect_equal(actual, expected)

})

test_that("fractionation factor calculation is correct", {

  x <- 273.14 - 45

  expected <- round(0.9098 * exp(16288. / x^2), 4)
  actual <- round(alphaIso(x, dD = TRUE), 4)

  expect_equal(actual, expected)

  expected <- round(0.9722 * exp(11.839 / x), 4)
  actual <- round(alphaIso(x), 4)

  expect_equal(actual, expected)

})

test_that("vapour diffusivity calculation is correct", {

  m <- "Lengths of 'T' and 'P' must be equal if both are > 1."

  expect_error(DAir(c(200, 250), c(500, 600, 700)), m, fixed = TRUE)
  expect_error(DAir(c(200, 250, 300), c(500, 600)), m, fixed = TRUE)

  x <- 273.15 - 45
  p <- 650

  expected.std <- 0.211 * (x / 273.15)^(1.94) * (1013.25 / p) * 1e-4
  expected.oxy <- 0.211 * (x / 273.15)^(1.94) * (1013.25 / p) * 1e-4 / 1.0285
  expected.dtr <- 0.211 * (x / 273.15)^(1.94) * (1013.25 / p) * 1e-4 / 1.0251

  actual.std <- DAir(x, p)
  actual.oxy <- DAir(x, p, species = "oxygen")
  actual.dtr <- DAir(x, p, species = "deuterium")

  expect_equal(actual.std, expected.std)
  expect_equal(actual.oxy, expected.oxy)
  expect_equal(actual.dtr, expected.dtr)

})

test_that("tortuosity calculation is correct", {

  expected <- 1 - 1.3 * (578 / 920)^2
  actual   <- tauFirn(578)

  expect_equal(actual, expected)

  expected <- 1 / (1 - 1.3 * (578 / 920)^2)
  actual   <- tauFirn(578, inverse = FALSE)

  expect_equal(actual, expected)

  expected <- 1 - 1.78 * (578 / 920)^2
  actual   <- tauFirn(578, b = 1.78)

  expect_equal(actual, expected)

  rho <- seq(800, 920)

  expected <- which(rho >= 920/sqrt(1.1))
  actual   <- which(tauFirn(rho, b = 1.1) == 0)

  expect_equal(actual, expected)

})
