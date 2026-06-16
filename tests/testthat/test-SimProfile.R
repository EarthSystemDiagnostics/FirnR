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

  msg <- "`dz.out` must have length 1."
  expect_error(SimProfile(time = c(1, 2), precip = c(10, 12),
                          temperature = c(-30, -32), dz.out = c(1, 5.6)),
               msg, fixed = TRUE)
  msg <- "Invalid `dz.out` value."
  expect_error(SimProfile(time = c(1, 2), precip = c(10, 12),
                          temperature = c(-30, -32), dz.out = Inf),
               msg, fixed = TRUE)
  expect_error(SimProfile(time = c(1, 2), precip = c(10, 12),
                          temperature = c(-30, -32), dz.out = NA),
               msg, fixed = TRUE)
  msg <- "`dz.out` must be > 0."
  expect_error(SimProfile(time = c(1, 2), precip = c(10, 12),
                          temperature = c(-30, -32), dz.out = 0),
               msg, fixed = TRUE)
  expect_error(SimProfile(time = c(1, 2), precip = c(10, 12),
                          temperature = c(-30, -32), dz.out = -12.),
               msg, fixed = TRUE)

})

test_that("SimProfile reproduces target data", {

  # create input data as was used for target simulation
  set.seed(20260312)
  nyr <- 5
  precip <- rep(70 / 365, times = nyr * 365)
  seasonal.par <- c(A0 = -44.5, A1 = 13, A2 = 5, phi1 = 10, phi2 = 50)
  temperature <-
    rep(CreateHarmonicSeries(seasonal.par), nyr) + rnorm(length(precip), sd = 2)
  time <- as.Date(-1 * (length(precip) : 1), origin = "2020-01-01")

# run simulation
  actual <- list()
  actual$hires  <- SimProfile(time, precip, temperature, diffuse = FALSE)
  actual$nodiff <- SimProfile(time, precip, temperature, dz.out = 0.03,
                              diffuse = FALSE)
  actual$diff   <- SimProfile(time, precip, temperature, dz.out = 0.03)

  # w/o block-averaging, depth resolution should per code always be >= 0.1 mm
  dz <- diff(actual$hires$depth)[1]
  expect_true(dz >= 1.e-4)
  # and dz.out should be set to internally defined resolution
  expect_true(attr(actual$hires, "output resolution [cm]") == 100. * dz)

  # and for a block-average resolution request smaller than the in-code
  # simulated resolution, dz.out should be reset to the simulation value
  msg <- paste("Requested `dz.out` < simulated resolution;",
               "resetting it to the latter.")
  expect_warning(tmp <- SimProfile(time, precip, temperature,
                                   diffuse = FALSE, dz.out = 1.e-5),
                 msg, fixed = TRUE)
  expect_equal(attr(actual$hires, "output resolution [cm]"),
               attr(tmp, "output resolution [cm]"))

  # remove varying simulation date from output
  attr(actual$nodiff, "date") <- NULL
  attr(actual$diff, "date") <- NULL

  # test output structure

  expect_true(is.data.frame(actual$nodiff))
  expect_true(is.data.frame(actual$diff))

  nms <- c("depth", "time", "y")
  expect_equal(names(actual$nodiff), nms)
  expect_equal(names(actual$diff), nms)

  # compare with target simulated record

  file <- test_path("test_data", "SimProfile_testthat_target.rda")
  load(file)

  expect_equal(actual$nodiff$depth, target$nodiff$depth)
  expect_equal(actual$nodiff$time, target$nodiff$time)
  expect_equal(actual$nodiff$y, target$nodiff$d18O)

  expect_equal(actual$diff$depth, target$diff$depth)
  expect_equal(actual$diff$time, target$diff$time)
  expect_equal(actual$diff$y, target$diff$d18O)

  # compare attributes, allowing column names to change
  names(actual$nodiff) <- NULL
  names(target$nodiff) <- NULL
  names(actual$diff) <- NULL
  names(target$diff) <- NULL

  expect_equal(attributes(actual$nodiff), attributes(target$nodiff))
  expect_equal(attributes(actual$diff), attributes(target$diff))

})
