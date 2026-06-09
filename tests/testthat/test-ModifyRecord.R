test_that("error handling works", {

  msg <- "'output.res' needs to be of length 1."
  expect_error(ModifyRecord(data.frame(depth = 1 : 10, y = rnorm(10)),
                            output.res = c(3, 7, 2)),
               msg, fixed = TRUE)
  expect_no_error(
    ModifyRecord(data.frame(depth = 1 : 10, y = 1 : 10),
                 advection = 1, output.res = c(1, NULL)))

  msg <- "Missing value passed for 'output.res'."
  expect_error(ModifyRecord(data.frame(depth = 1 : 10, y = rnorm(10)),
                            output.res = NA),
               msg, fixed = TRUE)

  msg <- "'output.res' value needs to be > 0."
  expect_error(ModifyRecord(data.frame(depth = 1 : 10, y = rnorm(10)),
                            output.res = -1),
               msg, fixed = TRUE)

})

test_that("modifications work", {

  # ----------------------------------------------------------------------------
  # test normal data and seperate modification processes

  record <- data.frame(depth = 1 : 10, y = 1 : 10)

  advection = 4.5
  actual1 <- ModifyRecord(record, advection = advection)
  expected1 <- AdvectRecord(record, advection = advection)

  compression = 2.1
  actual2 <- ModifyRecord(record, compression = compression)
  expected2 <- CompressRecord(record, compression = compression)

  diffusion = 1.5
  actual3 <- ModifyRecord(record, diffusion = diffusion)
  expected3 <- DiffuseRecord(record, sigma = diffusion)

  expect_equal(actual1, expected1)
  expect_equal(actual2, expected2)
  expect_equal(actual3, expected3)

  # ----------------------------------------------------------------------------
  # test data with leading and trailing NA and seperate modification processes
  
  nr <- 36
  n1 <- 13
  record <- data.frame(
    depth = 1 : nr,
    y = c(rep(NA, n1), 1 : 10, rep(NA, nr - n1 - 10))
  )

  advection = 7
  actual1 <- ModifyRecord(record, advection = advection)
  expected1 <- AdvectRecord(record, advection = advection)

  compression = 4.5
  actual2 <- ModifyRecord(record, compression = compression)
  expected2 <- CompressRecord(record, compression = compression)

  diffusion = 2.3
  actual3 <- ModifyRecord(record, diffusion = diffusion)
  expected3 <- DiffuseRecord(record, sigma = diffusion)

  expect_equal(actual1, expected1)
  expect_equal(actual2, expected2)
  expect_equal(actual3, expected3)

  # ----------------------------------------------------------------------------
  # test combined modification processes

  record <- data.frame(depth = 1 : 10, y = 1 : 10)

  advection = 4.5
  compression = 2.1
  diffusion = 1.5

  # advection and diffusion
  actual1 <- ModifyRecord(record, advection = advection, diffusion = diffusion)

  tmp <- DiffuseRecord(record, sigma = diffusion)
  expected1 <- AdvectRecord(tmp, advection = advection)

  # advection and compresion
  actual2 <- ModifyRecord(record, advection = advection,
                          compression = compression)

  tmp <- CompressRecord(record, compression = compression)
  expected2 <- AdvectRecord(tmp, advection = advection)

  # compression and diffusion
  actual3 <- ModifyRecord(record, diffusion = diffusion,
                          compression = compression)

  tmp <- DiffuseRecord(record, sigma = diffusion)
  expected3 <- CompressRecord(tmp, compression = compression)

  # all three
  actual4 <- ModifyRecord(record, advection = advection, diffusion = diffusion,
                          compression = compression)

  tmp1 <- DiffuseRecord(record, sigma = diffusion)
  tmp2 <- CompressRecord(tmp1, compression = compression)
  expected4 <- AdvectRecord(tmp2, advection = advection)

  expect_equal(actual1, expected1)
  expect_equal(actual2, expected2)
  expect_equal(actual3, expected3)
  expect_equal(actual4, expected4)

  # ----------------------------------------------------------------------------
  # test different output resolution

  output.res <- 3
  actual5 <- ModifyRecord(record, advection = advection, diffusion = diffusion,
                          compression = compression, output.res = output.res)

  tmp1 <- DiffuseRecord(record, sigma = diffusion)
  tmp2 <- CompressRecord(tmp1, compression = compression)
  tmp3 <- AdvectRecord(tmp2, advection = advection)

  new.depth <- seq(record$depth[1], max(record$depth), by = output.res)
  expected5 <- data.frame(
    depth = new.depth, y = approx(tmp3$depth, tmp3$y, new.depth)$y
  )

  expect_equal(actual5, expected5)

  # ----------------------------------------------------------------------------
  # test tibble conservation

  actual6 <- ModifyRecord(dplyr::as_tibble(record), advection = advection,
                          diffusion = diffusion, compression = compression,
                          output.res = output.res)
  expected6 <- dplyr::as_tibble(expected5)

  expect_equal(actual6, expected6)

})
