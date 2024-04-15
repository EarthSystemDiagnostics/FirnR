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
