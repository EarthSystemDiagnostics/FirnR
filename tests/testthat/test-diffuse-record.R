test_that("input arguments are valid", {

  rec <- rnorm(1000)
  msg1 <- "No diffusion length passed as input."
  msg2 <- "Missing values passed as diffusion length."
  msg3 <- "Diffusion length neither of length 1 nor matches length of record."

  # test for missing or invalid diffusion lengths
  expect_error(DiffuseRecord(rec), msg1)
  expect_error(DiffuseRecord(rec, sigma = NA), msg2)
  expect_error(DiffuseRecord(rec, sigma = c(NA, rep(1, 999))), msg2)

  # test for conflicting length of diffusion length
  expect_error(DiffuseRecord(rec, sigma = c(1, 2)), msg3)

  # test recycling of diffusion length
  expect_error(DiffuseRecord(rec, sigma = 1), NA)
})

test_that("diffusion works", {

  # test return of input for zero diffusion length

  rec <- rnorm(100)

  diffused1 <- DiffuseRecord(rec, sigma = 0)

  sigma <- seq(0.1, 10, 0.1)
  i <- c(11, 33, 56, 98)
  sigma[i] <- 0
  diffused2 <- DiffuseRecord(rec, sigma = sigma)

  expect_equal(diffused1, rec)
  expect_equal(diffused2[i], rec[i])

  # test diffusion for constant diffusion length

  rec <- sin(seq(0, 1000, 0.1))
  diffused <- DiffuseRecord(rec, sigma = sqrt(2), res = 0.1)

  expected <- round(rec * exp(-1), 3)
  diffused <- round(diffused, 3)

  # use only subset to circumvent edge effects
  edgeLength <- ceiling(5 * sqrt(2) / 0.1) # as defined in code
  compare <- (edgeLength + 1) : (length(rec) - edgeLength)
  expect_equal(diffused[compare], expected[compare])

  # results need to be different in the edge areas
  expect_false(all(diffused[-compare] == expected[-compare]))

  # switch off padding
  diffused <- DiffuseRecord(rec, sigma = sqrt(2), res = 0.1, pad = FALSE)
  diffused <- round(diffused, 3)
  compare <- which(!is.na(diffused))
  expect_equal(diffused[compare], expected[compare])

  # check padding length
  expect_equal(length(which(is.na(diffused))), 2 * edgeLength)

})
