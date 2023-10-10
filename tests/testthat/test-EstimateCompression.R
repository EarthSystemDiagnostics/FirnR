test_that("compression calculation works", {

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

  actual <- EstimateCompression(z2 = 2, a = 0, rate = 1)
  expect_equal(actual, 0)

  actual <- (EstimateCompression(z2 = 1, a = 0.5, rate = 0.02) * 1e2) %>%
    round(6)
  expect_equal(actual, 0.970965)

})
