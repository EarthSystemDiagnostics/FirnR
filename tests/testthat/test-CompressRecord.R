test_that("error handling works", {

  msg <- "'record' must be a data.frame."
  expect_error(CompressRecord(numeric(1), compression = 1), msg, fixed = TRUE)
  expect_error(CompressRecord(list(depth = 1, y = 1), compression = 1),
               msg, fixed = TRUE)

  msg <- "Expected column names for 'record' are: 'depth', 'y'."
  expect_error(CompressRecord(data.frame(foo = 1, bar = 1), compression = 1),
               msg, fixed = TRUE)
  expect_error(CompressRecord(data.frame(depth = 1, bar = 1), compression = 1),
                 msg, fixed = TRUE)
  expect_error(CompressRecord(data.frame(foo = 1, y = 1), compression = 1),
               msg, fixed = TRUE)

  msg <- "Length of proxy record needs to be > 1."
  expect_error(CompressRecord(data.frame(depth = 1, y = 1), compression = 1),
               msg, fixed = TRUE)

  msg <- "'compression' needs to be of length 1."
  expect_error(
    CompressRecord(data.frame(depth = 1 : 2, y = 1 : 2), compression = c(1, 3)),
    msg, fixed = TRUE)

  msg <- "Missing value passed for 'compression'."
  expect_error(
    CompressRecord(data.frame(depth = 1 : 2, y = 1 : 2), compression = NA),
    msg, fixed = TRUE)

  msg <- "Compression value >= range (max - min) of original depth scale."
  expect_error(CompressRecord(data.frame(depth = 1 : 5, y = 1 : 5),
                              compression = 4), msg, fixed = TRUE)
  expect_error(CompressRecord(data.frame(depth = 1 : 5, y = 1 : 5),
                              compression = 8), msg, fixed = TRUE)
  

})

test_that("compression simulation works", {

  d <- 1 : 10
  yin <- 1 : 10
  ycp <- c(1, 3, 5, 7, 9, rep(NA, 5))

  original <- data.frame(depth = d, y = yin)
  expected <- data.frame(depth = d, y = ycp)
  actual <- CompressRecord(original, compression = 4.5)

  expect_equal(expected, actual)

  original <- data.frame(depth = c(0, 0.5, 0.75, d), y = c(rep(NA, 3), yin))
  expected <- data.frame(depth = c(0, 0.5, 0.75, d), y = c(rep(NA, 3), ycp))
  actual <- CompressRecord(original, compression = 4.5)

  expect_equal(expected, actual)

  original <- data.frame(depth = c(d, 11, 12), y = c(yin, rep(NA, 2)))
  expected <- data.frame(depth = c(d, 11, 12), y = c(ycp, rep(NA, 2)))
  actual <- CompressRecord(original, compression = 4.5)

  expect_equal(expected, actual)

  original <- data.frame(depth = c(0, 0.5, 0.75, d, 11, 12),
                         y = c(rep(NA, 3), yin, rep(NA, 2)))
  expected <- data.frame(depth = c(0, 0.5, 0.75, d, 11, 12),
                         y = c(rep(NA, 3), ycp, rep(NA, 2)))
  actual <- CompressRecord(original, compression = 4.5)

  expect_equal(expected, actual)

  # test correct return data class

  original <- dplyr::as_tibble(original)
  expected <- dplyr::as_tibble(expected)
  actual <- CompressRecord(original, compression = 4.5)

  expect_equal(expected, actual)

  # test record stretching

  ycp <- c(1., 1.9, 2.8, 3.7, 4.6, 5.5, 6.4, 7.3, 8.2, 9.1)
  original <- data.frame(depth = d, y = yin)
  expected <- data.frame(depth = d, y = ycp)
  actual <- CompressRecord(original, compression = -1)

  expect_equal(expected, actual)

})
