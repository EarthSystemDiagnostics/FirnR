test_that("error checks work", {

  msg <- "All input vectors must have the same length."

  expect_error(SimProfile(time = c(1, 2), precip = 10,
                          temperature = -30),
               msg, fixed = TRUE)
  expect_error(SimProfile(time = c(1, 2), precip = c(10, 12),
                          temperature = -30),
               msg, fixed = TRUE)
  expect_error(SimProfile(time = c(1, 2), precip = 10,
                          temperature = c(-30, -32)),
               msg, fixed = TRUE)
  expect_error(SimProfile(time = 1, precip = c(10, 12),
                          temperature = c(-30, -32)),
               msg, fixed = TRUE)
  expect_error(SimProfile(time = c(1, 2), precip = c(10, 12),
                          temperature = c(-30, -32), data = 1),
               msg, fixed = TRUE)

})

test_that("SimProfile reproduces target data", {

  # create input data as was used for target simulation
  set.seed(20260312)
  nyr <- 5
  precip <- rep(70 / 365, times = nyr * 365)
  seasonal.par <- c(-44.5, 13, 5, 10, 50)
  temperature <-
    rep(HarmonicModel(seasonal.par), nyr) + rnorm(length(precip), sd = 2)
  time <- as.Date(-1 * (length(precip) : 1), origin = "2020-01-01")

# run simulation
  actual <- list()
  actual$nodiff <- SimProfile(time, precip, temperature, diffuse = FALSE)
  actual$diff   <- SimProfile(time, precip, temperature)

  # remove varying simulation date from output
  attr(actual$nodiff, "date") <- NULL
  attr(actual$diff, "date") <- NULL

  # test output structure

  expect_true(is.data.frame(actual$nodiff))
  expect_true(is.data.frame(actual$diff))

  nms <- c("depth", "time", "d18O")
  expect_equal(names(actual$nodiff), nms)
  expect_equal(names(actual$diff), nms)

  # compare with target simulated record

  file <- test_path("test_data", "SimProfile_testthat_target.rda")
  load(file)

  expect_equal(actual$nodiff$depth, target$nodiff$depth)
  expect_equal(actual$nodiff$time, target$nodiff$time)
  expect_equal(actual$nodiff$d18O, target$nodiff$d18O)

  expect_equal(actual$diff$depth, target$diff$depth)
  expect_equal(actual$diff$time, target$diff$time)
  expect_equal(actual$diff$d18O, target$diff$d18O)

  # compare attributes, allowing column names to change
  names(actual$nodiff) <- NULL
  names(target$nodiff) <- NULL
  names(actual$diff) <- NULL
  names(target$diff) <- NULL

  expect_equal(attributes(actual$nodiff), attributes(target$nodiff))
  expect_equal(attributes(actual$diff), attributes(target$diff))

})
