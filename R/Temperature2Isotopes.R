##' Temperature to isotope conversion
##'
##' Convert temperature to istopic data given a linear calibration regression.
##'
##' @param temperature numeric vector of temperatures in degree Celsius.
##' @param alpha slope of the calibration regression; in permil/(degree
##'   Celsius).
##' @param beta intercept of the calibration regression; in degree Celsius.
##' @return Numeric vector of isotopic data in permil.
##' @examples
##'
##' # spatial d18O to temperature calibration for Antarctica
##' # (Masson-Delmotte et al., J. Clim., 21(13), 2008)
##' Temperature2Isotopes(-44.5, 0.8, -8.1)
##'
##' # spatial d2H to temperature calibration for Antarctica
##' # (Masson-Delmotte et al., J. Clim., 21(13), 2008)
##' Temperature2Isotopes(-44.5, 6.3, -62.7)
##'
##' @author Thomas Münch
##' @export
Temperature2Isotopes <- function(temperature, alpha, beta) {

  alpha * temperature + beta

}
