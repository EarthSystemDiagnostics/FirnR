test_that("compression simulation works", {

  msg <- "Expected column names for 'record' are: 'depth', 'y'."
  expect_error(CompressRecord(data.frame(foo = 1, bar = 1), stretch = 1),
               msg, fixed = TRUE)
  expect_error(CompressRecord(data.frame(depth = 1, bar = 1), stretch = 1),
                 msg, fixed = TRUE)
  expect_error(CompressRecord(data.frame(foo = 1, y = 1), stretch = 1),
               msg, fixed = TRUE)

  msg <- "Proxy record is of length 1."
  expect_error(CompressRecord(data.frame(depth = 1, y = 1), stretch = 1),
               msg, fixed = TRUE)

  msg <- "Compression value >= range (max - min) of original depth scale."
  expect_error(CompressRecord(data.frame(depth = 1 : 5, y = 1 : 5), stretch = 4),
               msg, fixed = TRUE)
  expect_error(CompressRecord(data.frame(depth = 1 : 5, y = 1 : 5), stretch = 8),
               msg, fixed = TRUE)
  
  msg <- "Negative 'stretch' parameter yields longer record."
  expect_warning(
    CompressRecord(data.frame(depth = 1 : 10, y = 1 : 10), stretch = -1),
    msg, fixed = TRUE)

  original <- data.frame(depth = 1 : 10, y = 1 : 10)
  expected <- data.frame(depth = 1 : 10, y = c(1, 3, 5, 7, 9, rep(NA, 5)))
  actual <- CompressRecord(original, stretch = 4.5)

  expect_equal(expected, actual)

})
