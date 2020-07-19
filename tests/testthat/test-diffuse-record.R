test_that("diffusion works", {

  # test diffusion for constant diffusion length
  rec <- sin(seq(0, 1000, 0.1))
  diffused <- FirnR::DiffuseRecord(rec, sigma = rep(sqrt(2), length(rec)),
                                   res = 0.1)

  expected <- round(rec * exp(-1), 3)
  diffused <- round(diffused, 3)

  # test only subset to circumvent edge effects
  expect_equal(diffused[2000:8000], expected[2000:8000])

})
