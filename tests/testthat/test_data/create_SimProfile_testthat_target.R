##
## Thomas Muench, 2026/03/12
##
## script to make a run of SimProfile, using simple test case input data, and
## store the run's output as a testthat target to ensure reproducibility of the
## function output under future code updates.
##
## Date of run: 2026/03/12
## !OBS! This approach assumes that SimProfile is bug-free as of this date;
## the test target should hence not need updating, unless the format of the
## function output should change, or any result breaking bug is found in the
## future.
##

# set random seed
set.seed(20260312)

# create simple input data (from function's examples)
nyr <- 5
precip <- rep(70 / 365, times = nyr * 365)

seasonal.par <- c(A0 = -44.5, A1 = 13, A2 = 5, phi1 = 10, phi2 = 50)
temperature <-
  rep(CreateHarmonicSeries(seasonal.par), nyr) + rnorm(length(precip), sd = 2)

time <- as.Date(-1 * (length(precip) : 1), origin = "2020-01-01")

# run simulation
target <- list()
target$nodiff <- SimProfile(time, precip, temperature, diffuse = FALSE)
target$diff   <- SimProfile(time, precip, temperature)

# simulation date will of course always be different
attr(target$nodiff, "date") <- NULL
attr(target$diff, "date") <- NULL

# save test target
save(target, file = "./tests/testthat/test_data/SimProfile_testthat_target.rda")

