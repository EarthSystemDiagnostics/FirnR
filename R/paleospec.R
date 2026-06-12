# -----------------------------------------------------
#
# functions ported from package `PaleoSpec`,
# <https://github.com/EarthSystemDiagnostics/PaleoSpec>
# MIT
# Copyright (C) 2019 Thomas Laepple
#
# -----------------------------------------------------

#' Bin averaging
#'
#' Average a vector into bins.
#'
#' This function averages the vector \code{y} into bins according to the positon
#' of \code{x} within the breaks. You can either specify a desired number N of
#' breaks which are used to calculate the actual breaks via \code{pretty(x, N)},
#' or directly specify the N + 1 break positions. For \code{right = TRUE} (the
#' default) the averaging bins are defined via \code{x > breaks[i]} and \code{x
#' <= breaks[i + 1]}, else they are defined via \code{x >= breaks[i]} and
#' \code{x < breaks[i + 1]}. If \code{bFill = TRUE}, empty bins are filled using
#' linear interpolation from the neighbours to the center of the bin.
#'
#' Probably the binning could be considerably speeded up by using \code{?cut}.
#'
#' @param x vector of values on which the data in \code{y} is tabulated;
#'   e.g. depth or time points.
#' @param y vector of observation values to be averaged into bins. Must have the
#'   same length as \code{x}.
#' @param N desired number of breaks (ignored if \code{breaks} are supplied
#'   directly).
#' @param breaks vector of break point positions to define the averagig bins; if
#'   omitted, break point positions are calculated from the range of \code{x}
#'   and the desired number of breaks given by \code{N}.
#' @param right logical; indicate whether the bin intervals should be closed on
#'   the right and open on the left (\code{TRUE}, the default), or vice versa
#'   (\code{FALSE}).
#' @param bFill logical; if \code{TRUE}, fill empty bins using linear
#'   interpolation from the neighbours to the center of the bin.
#'
#' @return a list with four elements:
#' \describe{
#' \item{\code{breaks}:}{numeric vector of the used break point positions.}
#' \item{\code{centers}:}{numeric vector with the positions of the bin centers.}
#' \item{\code{avg}:}{numeric vector with the bin-averaged values.}
#' \item{\code{nobs}:}{numeric vector with the number of observations
#'   contributing to each bin average.}
#' }
#'
#' @author Thomas Laepple
#' @source This function is originally hosted in the package "PaleoSpec"
#'   (<https://github.com/EarthSystemDiagnostics/PaleoSpec>).
#'
paleospec.AvgToBin <- function(x, y, N = 2, breaks = pretty(x, N),
                               right = TRUE, bFill = FALSE) {

  if (length(x) != length(y)) {
    stop("'x' and 'y' must have the same length.", call. = FALSE)
  }

  nBins <- length(breaks) - 1

  centers <- (breaks[1 : nBins] + breaks[2 : (nBins + 1)]) / 2
  nObs <- avg <- rep(NA, nBins)

  for (i in 1 : nBins) {

    if (right) {
      selection <- y[which((x > breaks[i]) & (x <= breaks[i + 1]))]
    } else {
      selection <- y[which((x >= breaks[i]) & (x < breaks[i + 1]))]
    }

    avg[i]  <- mean(stats::na.omit(selection))
    nObs[i] <- sum(!is.na(selection))

  }

  if ((sum(missing <- is.na(avg)) > 0) & (bFill)) {

    avg[missing] <- (stats::approx(x, y, centers)$y)[missing]

  }

  list(breaks = breaks, centers = centers, avg = avg, nobs = nObs)

}

#' Filter time series
#'
#' Apply a given filter to a time series using different endpoint constraints.
#'
#' Note that when passing objects of class \code{ts}, the time step provided is
#' not used; thus, for time series with a time step different from 1, the filter
#' has to be adapted accordingly.
#'
#' Leading and trailing NA values are automatically stripped from the input
#' vector so that they do not spread into the filtered data when applying the
#' endpoint constraints, but added in again after filtering so that the output
#' vector has the same length as the input. This does not apply to any internal
#' NA values, which instead are handled by \code{na.rm}.
#'
#' The function applies endpoint constrains following Mann et al., GRL, 2004;
#' available methods are:
#' \itemize{
#'   \item method = 0: no constraint (loss at both ends);
#'   \item method = 1: minimum norm constraint;
#'   \item method = 2: minimum slope constraint;
#'   \item method = 3: minimum roughness constraint;
#'   \item method = 4: circular filtering.
#' }
#'
#' @param data numeric vector with the input timeseries (standard or ts object).
#' @param filter numeric vector of filter weights.
#' @param method single integer for choosing an endpoint constraint method;
#'   available choices are integers 0-4, see details.
#' @param na.rm logical; control the handling of internal NA values in
#'   \code{data}. If set to \code{TRUE}, any internal NA values are removed by
#'   linear interpolation from the neighbouring values; defaults to
#'   \code{FALSE}.
#' @return a ts object with the filtered timeseries.
#' @author Thomas Laepple
#' @source The endpoint constraint methods are based on the study:\cr
#'   Michael E. Mann, On smoothing potentially non‐stationary climate time
#'   series, Geophys. Res. Lett., 31, L07214, doi:10.1029/2004GL019569, 2004.
#'   This function is originally hosted in the package "PaleoSpec"
#'   (<https://github.com/EarthSystemDiagnostics/PaleoSpec>).
#'
paleospec.ApplyFilter <- function(data, filter, method = 0, na.rm = FALSE) {

  if (!method %in% (0 : 4))
    stop("Unknown method; only 0 : 4 available.")

  result <- rep(NA, length(data))

  # remove leading and trailing NA's
  x <- c(zoo::na.trim(data))
  n <- length(x)

  # linearly interpolate internal NA's if requested
  if (na.rm) {x <- stats::approx(1 : n, x, 1 : n)$y}

  circular = FALSE

  if (method == 0 | method == 4) {

    if (method == 4) {circular = TRUE}

    xf <- stats::filter(x, filter, circular = circular)

  } else {

    N <- floor(length(filter) / 2)

    if (method == 1) {

      before <- rep(mean(x), N)
      after  <- rep(mean(x), N)

    } else if (method == 2 | method == 3) {

      before <- x[N : 1]
      after  <- x[n : (n - N + 1)]

      if (method == 3) {

        before <- x[1] - (before - mean(before))
        after  <- x[n] - (after - mean(after))

      }
    }

    xf <- stats::filter(c(before, x, after), filter, circular = circular)
    xf <- xf[(N + 1) : (N + n)]

  }

  i <- seq(match(x[1], data), by = 1, length.out = n)
  result[i] <- xf

  return(stats::ts(result, frequency = stats::frequency(data)))

}
