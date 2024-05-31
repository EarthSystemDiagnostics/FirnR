test_that("error handling works", {

  msg <- "'record' must be a data.frame."
  expect_error(AdvectRecord(numeric(1), advection = 1), msg, fixed = TRUE)
  expect_error(AdvectRecord(list(depth = 1, y = 1), advection = 1),
               msg, fixed = TRUE)

  msg <- "Expected column names for 'record' are: 'depth', 'y'."
  expect_error(AdvectRecord(data.frame(foo = 1, bar = 1), advection = 1),
               msg, fixed = TRUE)
  expect_error(AdvectRecord(data.frame(depth = 1, bar = 1), advection = 1),
                 msg, fixed = TRUE)
  expect_error(AdvectRecord(data.frame(foo = 1, y = 1), advection = 1),
               msg, fixed = TRUE)

  msg <- "Length of proxy record needs to be > 1."
  expect_error(AdvectRecord(data.frame(depth = 1, y = 1), advection = 1),
               msg, fixed = TRUE)

  msg <- "'advection' needs to be of length 1."
  expect_error(
    AdvectRecord(data.frame(depth = 1 : 2, y = 1 : 2), advection = c(1, 3)),
    msg, fixed = TRUE)
  expect_no_error(
    AdvectRecord(data.frame(depth = 1 : 2, y = 1 : 2), advection = c(1, NULL)))

  msg <- "Missing value passed for 'advection'."
  expect_error(
    AdvectRecord(data.frame(depth = 1 : 2, y = 1 : 2), advection = NA),
    msg, fixed = TRUE)

  msg <- "'advection' value needs to be >= 0."
  expect_error(
    AdvectRecord(data.frame(depth = 1 : 2, y = 1 : 2), advection = -3.7),
    msg, fixed = TRUE)

  msg <- "Require constant depth resolution."
  expect_error(
    AdvectRecord(data.frame(depth = c(1, 4, 5, 9), y = 1 : 4), advection = 1),
    msg, fixed = TRUE)

})

test_that("advection shift works", {

  # output = input for zero shift (actual, or virtual from internal rounding)

  record <- data.frame(depth = 1 : 4, y = 1 : 4)
  expect_equal(AdvectRecord(record, advection = 0), record)
  expect_equal(AdvectRecord(record, advection = 0, clip = FALSE), record)

  record <- data.frame(depth = seq(1.5, 10.5, 3), y = 1 : 4)
  expect_equal(AdvectRecord(record, advection = 1), record)

  # with clipping

  actual <- data.frame(depth = 1 : 10, y = 1 : 10) %>%
    AdvectRecord(advection = 3)
  expected <- data.frame(depth = 1 : 10, y = c(rep(NA, 3), 1 : 7))

  expect_equal(actual, expected)

  actual <- data.frame(depth = 1 : 10, y = 1 : 10) %>%
    AdvectRecord(advection = 8.45)
  expected <- data.frame(depth = 1 : 10, y = c(rep(NA, 8), 1 : 2))

  expect_equal(actual, expected)

  actual <- data.frame(depth = seq(1.5, 22.5, 3), y = 1 : 8) %>%
    AdvectRecord(advection = 3)
  expected <- data.frame(depth = seq(1.5, 22.5, 3), y = c(NA, 1 : 7))

  expect_equal(actual, expected)

  # without clipping

  actual <- data.frame(depth = 1 : 10, y = 1 : 10) %>%
    AdvectRecord(advection = 3, clip = FALSE)
  expected <- data.frame(depth = 1 : 13, y = c(rep(NA, 3), 1 : 10))

  expect_equal(actual, expected)

  actual <- data.frame(depth = seq(1.5, 22.5, 3), y = 1 : 8) %>%
    AdvectRecord(advection = 12.47, clip = FALSE)
  expected <- data.frame(depth = seq(1.5, 34.5, 3), y = c(rep(NA, 4), 1 : 8))

  expect_equal(actual, expected)

  # test correct return data class

  actual <- dplyr::tibble(depth = seq(1.5, 22.5, 3), y = 1 : 8) %>%
    AdvectRecord(advection = 12.47)
  expected <- dplyr::tibble(depth = seq(1.5, 22.5, 3), y = c(rep(NA, 4), 1 : 4))

  expect_equal(actual, expected)

  actual <- dplyr::tibble(depth = seq(1.5, 22.5, 3), y = 1 : 8) %>%
    AdvectRecord(advection = 12.47, clip = FALSE)
  expected <- dplyr::tibble(depth = seq(1.5, 34.5, 3), y = c(rep(NA, 4), 1 : 8))

  expect_equal(actual, expected)

})
