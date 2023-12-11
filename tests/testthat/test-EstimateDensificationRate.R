test_that("error handling works", {

  msg <- "'data' must be a data.frame."
  expect_error(EstimateDensificationRate(numeric(1), bottom.depth = 1),
               msg, fixed = TRUE)
  expect_error(EstimateDensificationRate(list(depth = 1, density = 1),
                                         bottom.depth = 1), msg, fixed = TRUE)

  msg <- "Expected column names for 'data' are: 'depth', 'density'."
  expect_error(EstimateDensificationRate(data.frame(foo = 1, bar = 1),
                                         bottom.depth = 1),
               msg, fixed = TRUE)
  expect_error(EstimateDensificationRate(data.frame(depth = 1, bar = 1),
                                         bottom.depth = 1),
               msg, fixed = TRUE)
  expect_error(EstimateDensificationRate(data.frame(foo = 1, density = 1),
                                         bottom.depth = 1),
               msg, fixed = TRUE)
  expect_no_error(EstimateDensificationRate(
    data.frame(depth = 1 : 3, density = 1 : 3,
               foo = rnorm(3), bar = runif(3)), bottom.depth = 1))

  msg <- "'bottom.depth' needs to be of length 1."
  expect_error(
    EstimateDensificationRate(data.frame(depth = 1 : 3, density = 1 : 3),
                              bottom.depth = c(1, 3)),
    msg, fixed = TRUE)

  msg <- "Missing value passed for 'bottom.depth'."
  expect_error(
    EstimateDensificationRate(data.frame(depth = 1 : 3, density = 1 : 3),
                              bottom.depth = NA),
    msg, fixed = TRUE)

  msg <- "'bottom.depth' must be > 0."
  expect_error(EstimateDensificationRate(
    data.frame(depth = 1 : 3, density = 1 : 3), bottom.depth = 0),
    msg, fixed = TRUE)
  expect_error(EstimateDensificationRate(
    data.frame(depth = 1 : 3, density = 1 : 3), bottom.depth = -14.5),
    msg, fixed = TRUE)

  msg <- "Bottom depth of data < 'bottom.depth'."
  expect_warning(EstimateDensificationRate(
    data.frame(depth = 1 : 3, density = 1 : 3), bottom.depth = 5),
    msg, fixed = TRUE)

})

test_that("densification rate calculation works", {

  actual <- EstimateDensificationRate(b41.b42.density$stack,
                                      bottom.depth = 2) %>%
    round(8)
  expect_equal(actual, 2.267678 * 1e-2)
  
})
