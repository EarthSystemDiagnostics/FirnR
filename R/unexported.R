#
# unexported helper functions
#

#' Alternating sequence
#'
#' Generate a sequence of 1's alternating between positive and negative values.
#'
#' @param n integer length of the generated sequence.
#' @return the generated sequence of alternating 1's.
#'
#' @examples
#' gen_alt_seq(3L)
#' gen_alt_seq(4L)
#'
#' @author Thomas Münch
#'
gen_alt_seq <- function(n) {

  if (!is.integer(n)) stop("'n' must be integer.")
  if (n < 1) stop("'n' must be > 0.")

  i <- 0 : (n - 1)

  (-1)^i

}

#' Alternating cumulative sum
#'
#' Calculate the alternating cumulative sum of a numeric vector; i.e. for a
#' vector \code{x = c(x1, x2, x3, x4, ...)} this function calculates the
#' cumulative sum of \code{x = c(x1, -x2, x3, -x4, ...)}.
#'
#' @param x a numeric vector.
#' @return the alternating cumulative sum of \code{x}.
#'
#' @examples
#' cumsum(1 : 6)
#' cumsum_alt(1 : 6)
#' @seealso \code{\link[base]{cumsum}}
#'
#' @author Thomas Münch
#'
cumsum_alt <- function(x) {

  if (!is.numeric(x)) stop("'x' must be numeric.")

  n <- length(x)

  c_alt <- gen_alt_seq(n)

  cumsum(x * c_alt)

}

#' Simpler approx version
#'
#' Wrapper around stats::approx which only returns the interpolated vector.
#'
#' @param ... parameters passed on to \code{\link[stats]{approx}}.
#' @return a numeric vector with the interpolated values.
#'
#' @examples
#' x <- 0 : 10
#' y <- rnorm(11)
#' xout <- seq(0.5, 9.5, 1)
#' approx(x, y, xout)
#' approx.y(x, y, xout)
#' @seealso \code{\link[stats]{approx}}
#'
#' @author Thomas Münch
#'
approx.y <- function(...) {

  stats::approx(...)$y

}

#' Check for equidistant resolution
#'
#' This function checks whether a numeric vector (e.g. a depth vector or from a
#' time series) has equidistant increments, i.e. a constant resolution. This
#' check includes a numerical tolerance that accounts for the machine
#' respresentation of floating-point numbers, which circumvents the problems
#' popular methods of checking equidistance have which use, e.g., \code{sd()} or
#' \code{unique()} on the difference vector of \code{x}.
#'
#' @param x a numeric vector.
#' @return a logical value: \code{TRUE} if \code{x} has constant resolution,
#'   \code{FALSE} otherwise.
#'
#' @examples
#'
#' FirnR:::is.equidistant(1 : 10)
#' FirnR:::is.equidistant(c(2.5, 5, 7.5, 10, 12.5))
#' FirnR:::is.equidistant(x <- seq(0, 12, 0.1)) # compare this to sd(diff(x)) == 0!
#'
#' FirnR:::is.equidistant(c(1 : 10, 18))
#'
#' @author Andrew Dolman, Thomas Münch
#'
is.equidistant <- function(x) {

  if (!is.numeric(x)) stop("'x' needs to be numeric.")

  if ((xl <- length(x)) == 1) return(TRUE)

  xd <- diff(x)
  r <- all.equal(xd, rep(xd[1], xl - 1))

  if (is.logical(r)) return(TRUE) else return(FALSE)

}
