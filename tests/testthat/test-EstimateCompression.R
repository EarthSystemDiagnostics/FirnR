test_that("error handling works", {

  msg <- "All input parameters must have length 1."
  expect_error(EstimateCompression(length = 1 : 2, a = 1, rate = 1), msg)
  expect_error(EstimateCompression(length = 1, a = rnorm(30), rate = 1), msg)
  expect_error(
    EstimateCompression(top.depth = 1 : 5, length = 2, a = 1, rate = rnorm(3)),
    msg)

  msg <- "Missing values in input."
  expect_error(EstimateCompression(length = NA, a = 1, rate = 1), msg)
  expect_error(EstimateCompression(length = 1, a = NA, rate = 1), msg)
  expect_error(EstimateCompression(length = 1, a = 1, rate = NA), msg)
  expect_error(EstimateCompression(top.depth = NA, length = 1,
                                   a = 1, rate = 1), msg)

  msg <- "'length' must be > 0."
  expect_error(EstimateCompression(length = -1, a = 1, rate = 1),
               msg, fixed = TRUE)
  expect_error(EstimateCompression(top.depth = 4, length = 0, a = 1, rate = 1),
               msg, fixed = TRUE)

  msg <- "'advection' must be >= 0."
  expect_error(EstimateCompression(length = 2, a = -1, rate = 1),
               msg, fixed = TRUE)

  msg <- "'rate' must be > 0."
  expect_error(EstimateCompression(length = 2, a = 1, rate = 0),
               msg, fixed = TRUE)
  expect_error(EstimateCompression(length = 2, a = 1, rate = -1.2),
               msg, fixed = TRUE)

})

test_that("compression calculation works", {

  actual <- EstimateCompression(length = 2, advection = 0, rate = 1)
  expect_equal(actual, 0)

  actual <- (EstimateCompression(length = 1, advection = 0.5,
                                 rate = 0.02) * 1e2) %>%
    round(6)
  expect_equal(actual, 0.970965)

})
