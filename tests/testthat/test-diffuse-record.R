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

  # ----------------------------------------------------------------------------
  # test return of input for zero diffusion length

  record <- data.frame(depth = seq(100), y = rnorm(100))

  diffused_zero_sigma <- DiffuseRecord(record, sigma = 0)

  sigma <- seq(0.1, 10, 0.1)
  i <- c(11, 33, 56, 98)
  sigma[i] <- 0
  diffused_some_zero_sigma <- DiffuseRecord(record, sigma = sigma)

  expect_true(is.data.frame(diffused_zero_sigma))
  expect_true(is.data.frame(diffused_some_zero_sigma))

  expect_equal(diffused_zero_sigma, record)
  expect_equal(diffused_some_zero_sigma$depth, record$depth)
  expect_equal(diffused_some_zero_sigma$y[i], record$y[i])

  # ----------------------------------------------------------------------------
  # for record with sinusoidal profile compare numerical diffusion, applying a
  # constant diffusion length, with the expected exponential decay

  depth <- seq(1000, 2000, 0.1)
  record <- data.frame(depth = depth, y = sin(seq(0, 1000, 0.1)))

  diffused <- DiffuseRecord(record, sigma = sqrt(2))

  # check tibble conservation
  expect_equal(tibble::as_tibble(DiffuseRecord(record, sigma = sqrt(2))),
               tibble::as_tibble(diffused))

  # take rounded data to avoid numerical differences and use only subset to
  # circumvent edge effects

  diffused_rounded <- diffused %>%
    dplyr::mutate(y = round(y, 3))

  # expected diffusion effect is a simple decay with an exponential envelope
  expected <- data.frame(depth = record$depth,
                         y = round(record$y * exp(-1), 3))

  edgeLength <- ceiling(5 * sqrt(2) / 0.1) # as defined in code
  compare <- (edgeLength + 1) : (nrow(record) - edgeLength)

  expect_equal(diffused$depth, expected$depth)
  expect_equal(diffused_rounded$y[compare], expected$y[compare])

  # results need to be different in the edge areas

  expect_false(all(diffused_rounded$y[-compare] == expected$y[-compare]))

  # switch off padding

  diffused_no_pad <- DiffuseRecord(record, sigma = sqrt(2), pad = FALSE)
  diffused_no_pad$y <- round(diffused_no_pad$y, 3)
  compare <- which(!is.na(diffused_no_pad$y))

  expect_equal(diffused_no_pad$y[compare], expected$y[compare])

  # check padding length

  expect_equal(length(which(is.na(diffused_no_pad$y))), 2 * edgeLength)

  # ----------------------------------------------------------------------------
  # check trimming of NA

  depth_lead <- seq(900, 999.9, 0.1)
  depth_trail <- seq(2000.1, 2200, 0.1)
  record_with_na <- data.frame(depth = c(depth_lead, depth, depth_trail),
                               y = c(rep(NA, length(depth_lead)),
                                     sin(seq(0, 1000, 0.1)),
                                     rep(NA, length(depth_trail))))
  diffused_with_na <- DiffuseRecord(record_with_na, sigma = sqrt(2)) %>%
    zoo::na.trim()

  # reset row numbers
  rownames(diffused_with_na) <- 1 : nrow(diffused_with_na)

  expect_equal(diffused_with_na, diffused)

})
