test_that("error handling works", {

  msg <- "'record' must be a data.frame."
  expect_error(LoopRecordModifications(record = list(),
                                       reference = data.frame()),
               msg, fixed = TRUE)

  msg <- "'reference' must be a data.frame."
  expect_error(LoopRecordModifications(record = data.frame(),
                                       reference = list()),
               msg, fixed = TRUE)

  msg <- "Expected column names for 'record' are: 'depth', 'y'."
  expect_error(LoopRecordModifications(record = data.frame(foo = 1, bar = 1),
                                       reference = data.frame(foo = 1, bar = 1)),
               msg, fixed = TRUE)

  msg <- "Expected column names for 'reference' are: 'depth', 'y'."
  expect_error(LoopRecordModifications(record = data.frame(depth = 1, y = 1),
                                       reference = data.frame(foo = 1, bar = 1)),
               msg, fixed = TRUE)

  msg <- "'record' and 'reference' must be of the same length."
  expect_error(
    LoopRecordModifications(record = data.frame(depth = 1 : 10, y = 1 : 10),
                            reference = data.frame(depth = 1 : 5, y = 1 : 5)),
    msg, fixed = TRUE)

  msg <- "Require constant depth resolution."
  expect_error(
    LoopRecordModifications(record = data.frame(depth = c(1, 4, 5), y = 1 : 3),
                            reference = data.frame(depth = 1 : 3, y = 1 : 3)),
    msg, fixed = TRUE)

  msg <- "Depth resolutions of 'record' and 'reference' do not match."
  expect_error(
    LoopRecordModifications(record = data.frame(depth = 1 : 4, y = 1 : 4),
                            reference = data.frame(depth = c(10, 12, 13, 18),
                                                   y = 1 : 4)),
    msg, fixed = TRUE)

})

test_that("calculations work", {

  actual <- LoopRecordModifications(data.frame(depth = 1: 10, y = 1 : 10),
                                    data.frame(depth = 1: 10, y = 11 : 20),
                                    advection = 3, sigma = 1, compression = 2,
                                    verbose = FALSE)

  expect_type(actual, "list")
  expect_length(actual, 5)
  expect_length(actual$advection, 1)
  expect_length(actual$sigma, 1)
  expect_length(actual$compression, 1)
  expect_length(actual$optimum, 4)
  expect_equal(dim(actual$RMSD), c(1, 1, 1))
  expect_equal(names(actual$optimum),
               c("rmsd", "advection", "sigma", "compression"))

  record <- data.frame(depth = 1: 10, y = 1 : 10)
  reference <- record
  actual <- LoopRecordModifications(record, reference,
                                    advection = c(0, 5), sigma = 0 : 1,
                                    compression = c(0, 3), verbose = FALSE)

  y1 <- ModifyRecord(record, sigma = 0, compression = 0, advection = 0)
  y2 <- ModifyRecord(record, sigma = 0, compression = 0, advection = 5)
  y3 <- ModifyRecord(record, sigma = 1, compression = 0, advection = 0)
  y4 <- ModifyRecord(record, sigma = 1, compression = 0, advection = 5)
  y5 <- ModifyRecord(record, sigma = 0, compression = 3, advection = 0)
  y6 <- ModifyRecord(record, sigma = 0, compression = 3, advection = 5)
  y7 <- ModifyRecord(record, sigma = 1, compression = 3, advection = 0)
  y8 <- ModifyRecord(record, sigma = 1, compression = 3, advection = 5)

  rmsd <- function(v1, v2) {sqrt(mean((v1 - v2)^2, na.rm = TRUE))}

  expected <- sapply(list(y1, y2, y3, y4, y5, y6, y7, y8), function(x) {
    rmsd(x$y, reference$y)}) %>%
    array(dim = c(2, 2, 2))

  expect_equal(actual$RMSD, expected)
  expect_equal(actual$optimum,
               c(rmsd = 0, advection = 0, sigma = 0, compression = 0))

})
