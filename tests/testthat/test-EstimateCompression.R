test_that("error handling works", {

  msg <- "All input parameters must have length 1."
  expect_error(EstimateCompression(z2 = 1 : 2, a = 1, rate = 1), msg)
  expect_error(EstimateCompression(z2 = 1, a = rnorm(30), rate = 1), msg)
  expect_error(
    EstimateCompression(z1 = 1 : 5, z2 = 2, a = 1, rate = rnorm(3)), msg)

  msg <- "Missing values in input."
  expect_error(EstimateCompression(z2 = NA, a = 1, rate = 1), msg)
  expect_error(EstimateCompression(z2 = 1, a = NA, rate = 1), msg)
  expect_error(EstimateCompression(z2 = 1, a = 1, rate = NA), msg)
  expect_error(EstimateCompression(z1 = NA, z2 = 1, a = 1, rate = 1), msg)

  msg <- "'z2' must be > 'z1'."
  expect_error(EstimateCompression(z2 = -1, a = 1, rate = 1), msg, fixed = TRUE)
  expect_error(EstimateCompression(z1 = 4, z2 = 4, a = 1, rate = 1),
               msg, fixed = TRUE)
  expect_error(EstimateCompression(z1 = 4, z2 = 3, a = 1, rate = 1),
               msg, fixed = TRUE)

  msg <- "'a' must be >= 0."
  expect_error(EstimateCompression(z2 = 2, a = -1, rate = 1),
               msg, fixed = TRUE)

  msg <- "'rate' must be > 0."
  expect_error(EstimateCompression(z2 = 2, a = 1, rate = 0),
               msg, fixed = TRUE)
  expect_error(EstimateCompression(z2 = 2, a = 1, rate = -1.2),
               msg, fixed = TRUE)

})

test_that("compression calculation works", {

  actual <- EstimateCompression(z2 = 2, a = 0, rate = 1)
  expect_equal(actual, 0)

  actual <- (EstimateCompression(z2 = 1, a = 0.5, rate = 0.02) * 1e2) %>%
    round(6)
  expect_equal(actual, 0.970965)

})
