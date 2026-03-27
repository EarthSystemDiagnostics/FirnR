test_that("bin averaging works.", {

  x <- 1 : 9
  y <- x

  breaks <- seq(0.5, 9.5, 3)

  expected <- list(
    breaks = breaks,
    centers = c(2, 5, 8),
    avg = c(2, 5, 8),
    nobs = rep(3, 3)
  )

  actual <- paleospec.AvgToBin(x, y, breaks = breaks)

  expect_equal(actual, expected)

  xx <- x
  xx[4 : 6] <- NA

  expected$nobs <- c(3, 0, 3)

  actual <- paleospec.AvgToBin(xx, y, breaks = breaks, bFill = TRUE)

  expect_equal(actual, expected)

  expected <- list(
    breaks = c(0, 5, 10),
    centers = c(2.5, 7.5),
    avg = c(3, 7.5),
    nobs = c(5, 4)
  )

  actual <- paleospec.AvgToBin(x, y, N = 2)

  expect_equal(actual, expected)

  actual <- paleospec.AvgToBin(x, y)

  expect_equal(actual, expected)

  # close the bins on the left side
  actual <- paleospec.AvgToBin(x, y, right = FALSE)

  expected <- list(
    breaks = c(0, 5, 10),
    centers = c(2.5, 7.5),
    avg = c(2.5, 7),
    nobs = c(4, 5)
  )

  expect_equal(actual, expected)

  # expect error
  expect_error(paleospec.AvgToBin(1, y))

})

test_that("apply filter function works.", {

  # test paleospec.ApplyFilter with a simple running mean filter across three bins

  x <- 1 : 10
  filter <- rep(1 / 3, 3)

  # test error check
  expect_error(paleospec.ApplyFilter(x, filter, method = 10))

  # test various endpoint constraint methods

  # method = 0
  expected <- ts(c(NA, 2 : 9, NA))
  actual <- paleospec.ApplyFilter(x, filter, method = 0)

  expect_equal(actual, expected)

  # method = 1
  expected <- ts(round(c(8.5 / 3, 2 : 9, 24.5 / 3), 2))
  actual <- round(paleospec.ApplyFilter(x, filter, method = 1), 2)

  expect_equal(actual, expected)

  # method = 2
  expected <- ts(round(c(4 / 3, 2 : 9, 29 / 3), 2))
  actual <- round(paleospec.ApplyFilter(x, filter, method = 2), 2)

  expect_equal(actual, expected)

  # method = 3
  expected <- ts(round(c(4 / 3, 2 : 9, 29 / 3), 2))
  actual <- round(paleospec.ApplyFilter(x, filter, method = 3), 2)

  expect_equal(actual, expected)

  # method = 4
  expected <- ts(round(c(13 / 3, 2 : 9, 20 / 3), 2))
  actual <- round(paleospec.ApplyFilter(x, filter, method = 4), 2)

  expect_equal(actual, expected)

})

test_that("paleospec.ApplyFilter's NA removal works.", {

  # remove leading/trailing NA's
  x <- c(NA, NA, 1 : 10, NA)
  filter <- rep(1 / 3, 3)

  expected <- ts(c(NA, NA, NA, 2 : 9, NA, NA))
  actual <- paleospec.ApplyFilter(x, filter, method = 0)

  expect_equal(actual, expected)

  expected <- ts(round(c(NA, NA, 13 / 3, 2 : 9, 20 / 3, NA), 2))
  actual <- round(paleospec.ApplyFilter(x, filter, method = 4), 2)

  expect_equal(actual, expected)

  # but not internal NA's
  x <- c(1 : 5, NA, 7 : 10)

  expected <- ts(c(NA, 2 : 4, NA, NA, NA, 8 : 9, NA))
  actual <- paleospec.ApplyFilter(x, filter, method = 0)

  expect_equal(actual, expected)

  # ... if the internal NA's are not explicitly interpolated
  expected <- ts(c(NA, 2 : 9, NA))
  actual <- paleospec.ApplyFilter(x, filter, method = 0, na.rm = TRUE)

  expect_equal(actual, expected)

  # test combined case
  x <- c(NA, 1 : 11, NA, 13 : 14, NA, NA, 17 : 20, rep(NA, 3))
  expected <- ts(c(NA, NA, 2 : 19, NA, rep(NA, 3)))
  actual <- paleospec.ApplyFilter(x, filter, method = 0, na.rm = TRUE)

  expect_equal(actual, expected)

  # test output if duplicate input values exist by chance
  x1 <- c(NA, NA, rep(1 : 3, 3), NA)
  x2 <- c(NA, NA, 1 : 3, 1, NA, 3, 1 : 3, NA)

  expected <- ts(round(c(NA, NA, 4 / 3, rep(2, 7), 8 / 3, NA), 2))

  actual <- round(paleospec.ApplyFilter(x1, filter, method = 2), 2)
  expect_equal(actual, expected)

  actual <- round(paleospec.ApplyFilter(x2, filter, method = 2, na.rm = TRUE), 2)
  expect_equal(actual, expected)

  expected <- ts(round(
    c(NA, NA, 4 / 3, rep(2, 2), rep(NA, 3), rep(2, 2), 8 / 3, NA), 2))
  actual <- round(paleospec.ApplyFilter(x2, filter, method = 2), 2)

  expect_equal(actual, expected)

})
