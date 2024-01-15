#' Estimate record's length change
#'
#' This function provides the analytical solution to the length change
#' (compression) of a firn proxy record due to densification upon advection
#' downwards in the firn. The analytical solution is given under the assumption
#' of a linear change in density with depth.
#'
#' The analyical solution is found from conservation of mass by equating the
#' mass integrals, \code{int_z{rho(z)dz}}, before and after advection and
#' solving for the unknown bottom depth of the advected record under the
#' assumption of a linear density profile: \code{rho(z) = rho_0 + beta * z}
#' with a relative densification rate of \code{beta / rho_0}.
#'
#' @param top.depth top depth of initial record.
#' @param length length of initial record.
#' @param advection advection of the initial record; i.e. the amount of downward
#'   movement of the record measured in the same depth units as \code{top.depth}
#'   and \code{length}.
#' @param rate densification rate relative to surface (units of inverse depth).
#' @return value of how much shorter the advected record is relative to the
#'   initial record length.
#' @author Thomas Münch
#' @examples
#'
#' # estimate mentioned in Münch et al. (2017):
#' EstimateCompression(top.depth = 0, length = 1,
#'                     advection = 0.5, rate = 0.02) * 1e2 # display in cm
#'
#' @references
#'
#' Münch, T., et al., Constraints on post-depositional isotope modifications
#' in East Antarctic firn from analysing temporal changes of isotope profiles,
#' The Cryosphere, 11(5), 2175-2188, doi:10.5194/tc-11-2175-2017, 2017.
#'
#' @export
#'
EstimateCompression <- function(top.depth = 0, length, advection, rate) {

  if (!all(sapply(list(top.depth, length, advection, rate), length) == 1)) {
    stop("All input parameters must have length 1.", call. = FALSE)
  }
  if (any(is.na(list(top.depth, length, advection, rate)))) {
    stop("Missing values in input.", call. = FALSE)
  }

  z1 <- top.depth
  z2 <- top.depth + length
  a  <- advection

  if (z2 <= z1) stop("'length' must be > 0.", call. = FALSE)
  if (a < 0) stop("'advection' must be >= 0.", call. = FALSE)
  if (rate <= 0) stop("'rate' must be > 0.", call. = FALSE)

  # auxiliary variable (inverse densification rate)
  g <- 1 / rate

  # solution for bottom depth of advected record
  z <- -g + sqrt(g^2 + 2 * g * (z2 + a) + z2^2 + 2 * z1 * a + a^2)

  # return length change (compression)
  z2 + a - z

}
