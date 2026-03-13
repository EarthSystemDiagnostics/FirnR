test_that("alternating sequence generation works", {

  expect_error(gen_alt_seq(1.234), "'n' must be integer.", fixed = TRUE)
  expect_error(gen_alt_seq("pi"), "'n' must be integer.", fixed = TRUE)

  expect_error(gen_alt_seq(0L), "'n' must be > 0.", fixed = TRUE)
  expect_error(gen_alt_seq(-123456789L), "'n' must be > 0.", fixed = TRUE)

  expect_equal(gen_alt_seq(1L), 1)
  expect_equal(gen_alt_seq(3L), c(1, -1, 1))
  expect_equal(gen_alt_seq(4L), c(1, -1, 1, -1))

  expect_equal(sum(gen_alt_seq(1000L)), 0)
  expect_equal(sum(gen_alt_seq(1001L)), 1)

})

test_that("alternating cumulative sum works", {

  expect_error(cumsum_alt(letters), "'x' must be numeric.", fixed = TRUE)

  expect_equal(cumsum_alt(1 : 5), c(1, -1, 2, -2, 3))

  expect_equal(sum(cumsum_alt(rep(1, 10))), 5)

})


test_that("simple approx version works", {

  x <- 0 : 10
  y <- rnorm(11)
  xout <- seq(0.5, 9.5, 1)

  expect_equal(approx(x, y, xout)$y, approx.y(x, y, xout))
  expect_equal(approx(x, y, n = 15)$y, approx.y(x, y, n = 15))

})
