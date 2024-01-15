test_that("error handling works", {

  msg <- "Input 'data' must be a list."
  expect_error(PlotModificationRMSD(1 : 10), msg, fixed = TRUE)
  expect_error(PlotModificationRMSD(matrix(NA, 3, 5)), msg, fixed = TRUE)

  msg <- "Input 'data' structure lacks required elements."
  expect_error(PlotModificationRMSD(list(advection = 1)), msg, fixed = TRUE)
  expect_error(PlotModificationRMSD(list(advection = 1, sigma = 1)),
               msg, fixed = TRUE)
  expect_error(PlotModificationRMSD(list(advection = 1, sigma = 1,
                                         compression = 1)), msg, fixed = TRUE)
  expect_error(PlotModificationRMSD(list(advection = 1, sigma = 1,
                                         compression = 1, optimum = 1)),
               msg, fixed = TRUE)
  expect_error(PlotModificationRMSD(list(advection = 1, sigma = 1,
                                         compression = 1, optimum = 1,
                                         rmsd = 0)),
               msg, fixed = TRUE)

  msg <- "Element 'optimum' must have length 4."
  expect_error(PlotModificationRMSD(list(advection = 1 : 5,
                                         sigma = c(0.5, 1.5, 2.5),
                                         compression = c(1, 4),
                                         optimum = 0, RMSD = 0)),
               msg, fixed = TRUE)

  msg <- "Missing required elements for 'optimum'."
  optimum <- c(rmsd = 0, advection = 0, foo = 1, bar = 2)
  expect_error(PlotModificationRMSD(list(advection = 1 : 5,
                                         sigma = c(0.5, 1.5, 2.5),
                                         compression = c(1, 4),
                                         optimum = optimum, RMSD = 0)),
               msg, fixed = TRUE)
  optimum <- c(rmsd = 0, advection = 0, sigma = 1, comprss = 2)
  expect_error(PlotModificationRMSD(list(advection = 1 : 5,
                                         sigma = c(0.5, 1.5, 2.5),
                                         compression = c(1, 4),
                                         optimum = optimum, RMSD = 0)),
               msg, fixed = TRUE)

  msg <- paste("Dimensions of 'RMSD' array do not match",
               "number of modification parameters.")
  optimum <- c(rmsd = 0, advection = 1, sigma = 0.5, compression = 1)
  expect_error(PlotModificationRMSD(list(advection = 1 : 5,
                                         sigma = c(0.5, 1.5, 2.5),
                                         compression = c(1, 4),
                                         optimum = optimum,
                                         RMSD = array(dim = c(2, 2, 2)))),
               msg, fixed = TRUE)
  expect_error(PlotModificationRMSD(list(advection = 1 : 5,
                                         sigma = c(0.5, 1.5, 2.5),
                                         compression = c(1, 4),
                                         optimum = optimum,
                                         RMSD = array(dim = c(2, 3, 5)))),
               msg, fixed = TRUE)

})

test_that("plotting works", {

  n <- 100
  record <- reference <- data.frame(depth = seq(n), y = seq(n))

  rmsd.data <- LoopRecordModifications(
    record, reference,
    advection = seq(10) - 1,
    sigma = 0 : 5,
    compression = 0 : 5,
    verbose = FALSE
  )

  expect_no_error(PlotModificationRMSD(rmsd.data))

})
