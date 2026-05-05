test_that("input arguments are valid", {

  # check record data frame input
  msg1 <- "'record' must be a data.frame."
  msg2 <- "Expected column names for 'record' are: 'depth', 'y'."
  msg3 <- "Length of 'record' needs to be > 1."

  expect_error(DiffuseRecord(numeric(1), sigma = 1), msg1, fixed = TRUE)
  expect_error(DiffuseRecord(list(depth = 1, y = 1), sigma = 1),
               msg1, fixed = TRUE)

  expect_error(DiffuseRecord(data.frame(foo = 1, bar = 1), sigma = 1),
               msg2, fixed = TRUE)
  expect_error(DiffuseRecord(data.frame(depth = 1, bar = 1), sigma = 1),
               msg2, fixed = TRUE)
  expect_error(DiffuseRecord(data.frame(foo = 1, y = 1), sigma = 1),
               msg2, fixed = TRUE)

  expect_error(DiffuseRecord(data.frame(depth = 1, y = 1), sigma = 1),
               msg3, fixed = TRUE)

  # test equidistance of depth scale
  msg1 <- "Require constant depth resolution for diffusion."
  record <- data.frame(depth = c(1., 5., 6., 20.), y = rnorm(4))
  expect_error(DiffuseRecord(record, sigma = 1), msg1, fixed = TRUE)

  # test for missing or invalid diffusion lengths
  record <- data.frame(depth = seq(1000), y = rnorm(1000))
  msg1 <- "No diffusion length passed as input."
  msg2 <- "Missing values passed as diffusion length."
  msg3 <- "Diffusion length neither of length 1 nor matches length of record."

  expect_error(DiffuseRecord(record), msg1)
  expect_error(DiffuseRecord(record, sigma = NA), msg2)
  expect_error(DiffuseRecord(record, sigma = c(NA, rep(1, 999))), msg2)
  expect_error(DiffuseRecord(record, sigma = c(1, 2)), msg3)

  # test recycling of diffusion length
  expect_error(DiffuseRecord(record, sigma = 1), NA)

})

test_that("diffusion works", {

  # test return of input for zero diffusion length

  record <- data.frame(depth = seq(100), y = rnorm(100))

  diffused1 <- DiffuseRecord(record, sigma = 0)

  sigma <- seq(0.1, 10, 0.1)
  i <- c(11, 33, 56, 98)
  sigma[i] <- 0
  diffused2 <- DiffuseRecord(record, sigma = sigma)

  expect_equal(diffused1, record$y)
  expect_equal(diffused2[i], record$y[i])

  # test diffusion for constant diffusion length

  record <- data.frame(depth = seq(0, 1000, 0.1), y = sin(seq(0, 1000, 0.1)))
  diffused <- DiffuseRecord(record, sigma = sqrt(2))

  expected <- round(record$y * exp(-1), 3)
  diffused <- round(diffused, 3)

  # use only subset to circumvent edge effects
  edgeLength <- ceiling(5 * sqrt(2) / 0.1) # as defined in code
  compare <- (edgeLength + 1) : (nrow(record) - edgeLength)
  expect_equal(diffused[compare], expected[compare])

  # results need to be different in the edge areas
  expect_false(all(diffused[-compare] == expected[-compare]))

  # switch off padding
  diffused <- DiffuseRecord(record, sigma = sqrt(2), pad = FALSE)
  diffused <- round(diffused, 3)
  compare <- which(!is.na(diffused))
  expect_equal(diffused[compare], expected[compare])

  # check padding length
  expect_equal(length(which(is.na(diffused))), 2 * edgeLength)

})
