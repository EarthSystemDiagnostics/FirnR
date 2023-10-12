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
#' @param z1 top depth of initial record.
#' @param z2 bottom depth of initial record.
#' @param a advection of the initial record; i.e. the amount of downward
#'   movement of the record measured in the same depth units as \code{z1} and
#'   \code{z2}.
#' @param rate densification rate relative to surface (units of inverse depth).
#' @return value of how much shorter the advected record is relative to the
#'   initial record length \code{z2 - z1}.
#' @author Thomas Münch
#' @examples
#'
#' # estimate mentioned in Münch et al. (2017):
#' EstimateCompression(z1 = 0, z2 = 1, a = 0.5, rate = 0.02) * 1e2 # in cm
#'
#' @references
#'
#' Münch, T., et al., Constraints on post-depositional isotope modifications
#' in East Antarctic firn from analysing temporal changes of isotope profiles,
#' The Cryosphere, 11(5), 2175-2188, doi:10.5194/tc-11-2175-2017, 2017.
#'
#' @export
#'
EstimateCompression <- function(z1 = 0, z2, a, rate) {

  if (!all(sapply(list(z1, z2, a, rate), length) == 1)) {
    stop("All input parameters must have length 1.", call. = FALSE)
  }
    if (any(is.na(list(z1, z2, a, rate)))) {
    stop("Missing values in input.", call. = FALSE)
  }

  if (z2 <= z1) stop("'z2' must be > 'z1'.", call. = FALSE)
  if (a < 0) stop("'a' must be >= 0.", call. = FALSE)
  if (rate <= 0) stop("'rate' must be > 0.", call. = FALSE)

  # auxiliary variable (inverse densification rate)
  g <- 1 / rate

  # solution for bottom depth of advected record
  z <- -g + sqrt(g^2 + 2 * g * (z2 + a) + z2^2 + 2 * z1 * a + a^2)

  # return length change (compression)
  z2 + a - z

}
