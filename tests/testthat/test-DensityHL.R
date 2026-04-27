test_that("correct density profile is obtained", {

  rho.surface <- 340.
  rho.c <- 550.
  rho.i <- 920.
  rho.w <- 1000.
  T <- 273.15 - 44.5
  bdot <- 70

  k0 <- 11 * exp(-10160 / (8.314478 * T))
  k1 <- 575 * exp(-21400 / (8.314478 * T))

  depth1 <- 10
  depth2 <- 75

  z0 <- exp(rho.i * k0 / rho.w * depth1 +
            log(rho.surface / (rho.i - rho.surface)))
  rho1 <- rho.i * z0 / (1 + z0)
  dwe1 <- (1 / k0) * log((rho.i - rho.surface) / (rho.i - rho1))

  actual <- FirnR::DensityHL(depth1, rho.surface = rho.surface, T = T,
                             bdot = bdot)

  expect_true(is.data.frame(actual))
  expect_named(actual, c("depth.we", "rho"))

  expect_equal(actual, data.frame(depth.we = dwe1, rho = rho1))

  zc <- rho.w / (rho.i * k0) * (log(rho.c / (rho.i - rho.c)) -
                               log(rho.surface / (rho.i - rho.surface)))
  z1 <- exp(rho.i * k1 / rho.w * (depth2 - zc) / sqrt(bdot / rho.w) +
            log(rho.c / (rho.i - rho.c)))
  rho2 <- rho.i * z1 / (1 + z1)
  dwec <- (1 / k0) * log((rho.i - rho.surface) / (rho.i - rho.c))
  dwe2 <- 1 / (k1 * sqrt(bdot / rho.w)) *
    log((rho.i - rho.c) / (rho.i - rho2)) * (bdot / rho.w) + dwec

  actual <- FirnR::DensityHL(depth2, rho.surface = rho.surface, T = T,
                             bdot = bdot)
  expect_equal(actual, data.frame(depth.we = dwe2, rho = rho2))

  # with Johnsen correction

  k0 <- 0.85 * k0
  k1 <- 1.15 * k1

  z0 <- exp(rho.i * k0 / rho.w * depth1 +
            log(rho.surface / (rho.i - rho.surface)))
  rho1 <- rho.i * z0 / (1 + z0)
  dwe1 <- (1 / k0) * log((rho.i - rho.surface) / (rho.i - rho1))

  actual <- FirnR::DensityHL(depth1, rho.surface = rho.surface, T = T,
                             bdot = bdot, JohnsenCorr = TRUE)
  expect_equal(actual, data.frame(depth.we = dwe1, rho = rho1))

  zc <- rho.w / (rho.i * k0) * (log(rho.c / (rho.i - rho.c)) -
                               log(rho.surface / (rho.i - rho.surface)))
  z1 <- exp(rho.i * k1 / rho.w * (depth2 - zc) / sqrt(bdot / rho.w) +
            log(rho.c / (rho.i - rho.c)))
  rho2 <- rho.i * z1 / (1 + z1)
  dwec <- (1 / k0) * log((rho.i - rho.surface) / (rho.i - rho.c))
  dwe2 <- 1 / (k1 * sqrt(bdot / rho.w)) *
    log((rho.i - rho.c) / (rho.i - rho2)) * (bdot / rho.w) + dwec

  actual <- FirnR::DensityHL(depth2, rho.surface = rho.surface, T = T,
                             bdot = bdot, JohnsenCorr = TRUE)
  expect_equal(actual, data.frame(depth.we = dwe2, rho = rho2))

})
