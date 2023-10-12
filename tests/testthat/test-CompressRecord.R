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
  
  msg <- "Negative 'compression' parameter yields longer record."
  expect_warning(
    CompressRecord(data.frame(depth = 1 : 10, y = 1 : 10), compression = -1),
    msg, fixed = TRUE)

})

test_that("compression simulation works", {

  original <- data.frame(depth = 1 : 10, y = 1 : 10)
  expected <- data.frame(depth = 1 : 10, y = c(1, 3, 5, 7, 9, rep(NA, 5)))
  actual <- CompressRecord(original, compression = 4.5)

  expect_equal(expected, actual)

})
