#' Linear estimate of firn densification rate
#'
#' Given a record of firn densities, this function calculates the relative
#' densification rate in units of inverse depth over a specified depth interval
#' starting from the surface, as estimated from a linear regression of the firn
#' density profile against depth.
#'
#' @param data a data frame providing a firn density profile via the two
#'   elements 'depth' and 'density'.
#' @param drange bottom depth (must be in the same units as \code{data$depth})
#'   for the analysis: the densification rate is estimated over the depth
#'   interval from the surface to this bottom depth.
#' @return relative linear densification rate (in units of inverse depth) from
#'   the surface to the depth in \code{drange}.
#' @author Thomas Münch
#' @examples
#'
#' # estimates mentioned in Münch et al. (2017) in units of % per m:
#' EstimateDensificationRate(b41.b42.density$stack, drange = 2) * 1e2
#' EstimateDensificationRate(b41.b42.density$stack, drange = 5) * 1e2
#'
#' @references
#'
#' Münch, T., et al., Constraints on post-depositional isotope modifications
#' in East Antarctic firn from analysing temporal changes of isotope profiles,
#' The Cryosphere, 11(5), 2175-2188, doi:10.5194/tc-11-2175-2017, 2017.
#'
#' @export
#'
EstimateDensificationRate <- function(data, drange) {

  if (!is.data.frame(data)) stop("'data' must be a data.frame.", call. = FALSE)
  if (any(is.na(match(c("depth", "density"), colnames(data))))) {
    stop("Expected column names for 'data' are: 'depth', 'density'.",
         call. = FALSE)
  }
  if (length(drange) != 1) stop("'drange' needs to be of length 1.")
  if (is.na(drange)) stop("Missing value passed for 'drange'.")
  if (drange <= 0) stop("'drange' must be > 0.", call. = FALSE)
  if (drange > max(data$depth)) {
    warning("Bottom depth of data < 'drange'.", call. = FALSE)
  }

  # linear regression
  i <- which(data$depth <= drange)
  regression <- coefficients(lm(data$density[i] ~ data$depth[i]))

  # relative densification rate (1 per depth unit)
  as.numeric(regression[2] / regression[1])

}
