test_that("input arguments are valid", {

  rec <- rnorm(1000)

  # test for missing or invalid diffusion lengths
  expect_error(DiffuseRecord(rec))
  expect_error(DiffuseRecord(rec, sigma = NA))
  expect_error(DiffuseRecord(rec, sigma = c(NA, rep(1, 999))))

  # test for conflicting length of diffusion length
  expect_error(DiffuseRecord(rec, sigma = c(1, 2)))

  # test recycling of diffusion length
  expect_error(DiffuseRecord(rec, sigma = 1), NA)
})

test_that("diffusion works", {

  # test return of input for zero diffusion length
  # TBD

  # test diffusion for constant diffusion length

  rec <- sin(seq(0, 1000, 0.1))
  diffused <- DiffuseRecord(rec, sigma = sqrt(2), res = 0.1)

  expected <- round(rec * exp(-1), 3)
  diffused <- round(diffused, 3)

  # use only subset to circumvent edge effects
  expect_equal(diffused[2000:8000], expected[2000:8000])

})
