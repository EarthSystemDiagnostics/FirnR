##
## Read Kohnen B41 and B42 CT density data and provide it as package data
##
## Thomas Muench, AWI, 10/2023
##

# set working directory to your package source folder
# setwd("<path>")

# required packages
library(dplyr)   # v>=1.1.3
library(usethis) # v>=2.2.1

# ------------------------------------------------------------------------------
# Read B41, B42 data, convert to kg/m3

b41.full <- read.table("data-raw/density/calibdensity_B41.txt",
                       header = TRUE, sep = "\t") %>%
  setNames(c("density", "depth")) %>%
  dplyr::as_tibble() %>%
  dplyr::select(depth, density) %>%
  dplyr::mutate(density = 1000. * density)

b42.full <- read.table("data-raw/density/calibdensity_B42.txt",
                       header = TRUE, sep = "\t") %>%
  setNames(c("density", "depth")) %>%
  dplyr::as_tibble() %>%
  dplyr::select(depth, density) %>%
  dplyr::mutate(density = 1000. * density)

# ------------------------------------------------------------------------------
# Build stacked record on B41 depth scale

stack <- b41.full %>%
  dplyr::rename(b41 = density) %>%
  dplyr::mutate(b42 = approx(b42.full$depth, b42.full$density, depth)$y) %>%
  dplyr::mutate(density = rowMeans(dplyr::select(., "b41", "b42"))) %>%
  dplyr::select(c("depth", "density"))

# ------------------------------------------------------------------------------
# Make 2nd order polynomial fit of stacked data

depth <- stack$depth
tmp   <- stack$density
m <- stats::nls(tmp ~ a + b * (depth)^(1/2) + c * depth,
                start = list(a = 0.3, b = 0.01, c = 0))
coef <- coefficients(m)

stack <- stack %>%
  dplyr::mutate(fitDensity =
                  coef[1] + coef[2] * (depth)^(1/2) + coef[3] * depth)

# ------------------------------------------------------------------------------
# Use only first 25 m to store in package to reduce file size

b41.b42.density <- list(
  b41   = dplyr::filter(b41.full, depth <= 25),
  b42   = dplyr::filter(b42.full, depth <= 25),
  stack = dplyr::filter(stack, depth <= 25))

usethis::use_data(b41.b42.density, overwrite = TRUE)

