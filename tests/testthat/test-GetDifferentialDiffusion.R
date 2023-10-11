test_that("error handling works", {

  msg <- "'data' must be a data.frame."
  expect_error(GetDifferentialDiffusion(numeric(1), 0, 1, 1, 2),
               msg, fixed = TRUE)
  expect_error(GetDifferentialDiffusion(list(depth = 1, sigma = 1), 0, 1, 1, 2),
               msg, fixed = TRUE)

  msg <- "'data' must have length > 1."
  expect_error(GetDifferentialDiffusion(data.frame(), 0, 1, 1, 2),
               msg, fixed = TRUE)
  expect_error(
    GetDifferentialDiffusion(data.frame(depth = 1, sigma = 1), 0, 1, 1, 2),
    msg, fixed = TRUE)

  msg <- "Expected column names for 'data' are: 'depth', 'sigma'."
  expect_error(
    GetDifferentialDiffusion(data.frame(foo = 1 : 5, bar = 1 : 5),
                             0, 1, 1, 2),
    msg, fixed = TRUE)
  expect_error(
    GetDifferentialDiffusion(data.frame(depth = 1 : 5, foo = 1 : 5),
                             0, 1, 1, 2),
    msg, fixed = TRUE)
  expect_error(
    GetDifferentialDiffusion(data.frame(foo = 1 : 5, sigma = 1 : 5),
                             0, 1, 1, 2),
    msg, fixed = TRUE)

  msg <- paste("depth intervals [z00, z01], [z10, z11] must lie",
               "within depth range of data.")
  expect_error(
    GetDifferentialDiffusion(data.frame(depth = 1 : 10, sigma = 1 : 10),
                             z00 = 0, z01 = 5, z10 = 3, z11 = 7),
    msg, fixed = TRUE)
  expect_error(
    GetDifferentialDiffusion(data.frame(depth = 1 : 10, sigma = 1 : 10),
                             z00 = 1, z01 = 5, z10 = 3, z11 = 12),
    msg, fixed = TRUE)
  expect_error(
    GetDifferentialDiffusion(data.frame(depth = 1 : 10, sigma = 1 : 10),
                             z00 = 1, z01 = 5, z10 = 12, z11 = 19),
    msg, fixed = TRUE)

  msg <- "'z01' must be > 'z00'."
  expect_error(
    GetDifferentialDiffusion(data.frame(depth = 1 : 10, sigma = 1 : 10),
                             z00 = 2, z01 = 1, z10 = 3, z11 = 8),
    msg, fixed = TRUE)
  msg <- "'z11' must be > 'z10'."
  expect_error(
    GetDifferentialDiffusion(data.frame(depth = 1 : 10, sigma = 1 : 10),
                             z00 = 2, z01 = 5, z10 = 8, z11 = 4),
    msg, fixed = TRUE)

})

test_that("differential diffusion calculation works", {
  
  actual <- GetDifferentialDiffusion(data.frame(depth = 1 : 10, sigma = 1 : 10),
                                     z00 = 2, z01 = 5, z10 = 4, z11 = 8)
  expected <- sqrt(mean(4 : 8)^2 - mean(2 : 5)^2)

  expect_equal(round(actual, 6), round(expected, 6))
  
})
