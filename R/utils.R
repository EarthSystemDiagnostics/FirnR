#' Specific heat capacity of ice
#'
#' Calculate the specific heat capacity of ice at constant pressure depending on
#' ambient temperature, based on the formula given in Goujon et al. (2003).
#'
#' @param T numeric vector of ice temperature in [K].
#' @return numeric vector of the specific heat capacity at temperature \code{T}
#'   in [J/(kg*K)].
#'
#' @references
#' Goujon, C., Barnola, J.-M., and Ritz, C.: Modeling the densification of
#'   polar firn including heat diffusion: Application to close-off
#'   characteristics and gas isotopic fractionation for Antarctica and Greenland
#'   sites, J. Geophys. Res., 108(D24), 4792,
#'   https://doi.org/10.1029/2002JD003319, 2003.
#'
#' @author Thomas Laepple
#' @examples
#'
#' CIce(c(273.15, 273.15 - 45))
#'
#' @export
#'
CIce <- function(T) {

  return(152.5 + 7.122 * T)

}

#' Thermal conductivity of ice
#'
#' Calculate the thermal conductivity of ice depending on ambient temperature,
#' based on the formula given in Goujon et al. (2003).
#'
#' @param T numeric vector of ice temperature in [K].
#' @return numeric vector of thermal conductivity of ice at temperature \code{T}
#'   in [W/(m*K)].
#'
#' @references
#' Goujon, C., Barnola, J.-M., and Ritz, C.: Modeling the densification of
#'   polar firn including heat diffusion: Application to close-off
#'   characteristics and gas isotopic fractionation for Antarctica and Greenland
#'   sites, J. Geophys. Res., 108(D24), 4792,
#'   https://doi.org/10.1029/2002JD003319, 2003.
#'
#' @author Thomas Laepple
#' @examples
#'
#' KIce(c(273.15, 273.15 - 45))
#'
#' @export
#'
KIce <- function(T) {

  return(2.22 * (1 + 0.0067 * (273.15 - T)))

}

#' Thermal conductivity of firn
#'
#' Calculate the thermal conductivity of firn based on the thermal conductivity
#' of ice and the firn density, as given by the formula in Goujon et
#' al. (2003).
#'
#' @param T numeric vector of ambient ice temperature in [K].
#' @param rho numeric vector of firn density in [kg/m^3].
#' @return numeric vector of the thermal conductivity of firn in [W/(m*K)].
#'
#' @references
#' Goujon, C., Barnola, J.-M., and Ritz, C.: Modeling the densification of
#'   polar firn including heat diffusion: Application to close-off
#'   characteristics and gas isotopic fractionation for Antarctica and Greenland
#'   sites, J. Geophys. Res., 108(D24), 4792,
#'   https://doi.org/10.1029/2002JD003319, 2003.
#'
#' @author Thomas Laepple
#' @seealso \code{\link{KIce}}
#' @examples
#'
#' # variable lengths of 'T' and 'rho' are supported:
#'
#' KFirn(273.15, 350)
#' KFirn(273.15, c(350, 500))
#' KFirn(c(273.15, 273.15 - 45), 500)
#'
#' # but lengths > 1 need to be equal
#' \dontrun{
#'  KFirn(c(273.15, 273.15 - 45), c(350, 500, 800))
#' }
#' KFirn(c(273.15, 273.15 - 45), c(350, 500))
#'
#' @export
#'
KFirn <- function(T, rho) {

  # check input vector lengths
  if ((length(T) != 1 & length(rho) != 1) & (length(T) != length(rho)))
    stop("'T' and 'rho' must have equal lengths, if both have lengths > 1.")

  kRhoIce <- 920. # ice density

  r <- rho / kRhoIce
  a <- 2 - 0.5 * r

  return(r^a * KIce(T))

}

#' Thermal diffusivity of ice
#'
#' Calculate the thermal diffusivity of ice from its thermal conductivity,
#' density and heat capacity at constant pressure.
#'
#' @param T numeric vector of ice temperature in [K].
#' @return numeric vector of thermal diffusivity at temperature \code{T} in
#'   [m^2/s].
#'
#' @references https://en.wikipedia.org/wiki/Thermal_diffusivity
#'
#' @author Thomas Laepple
#' @seealso \code{\link{KIce}}, \code{\link{CIce}}
#' @examples
#'
#' KappaIce(c(273.15, 273.15 - 45))
#' 
#' @export
#'
KappaIce <- function(T) {

  kRhoIce <- 920.  # ice density

  return(KIce(T) / (kRhoIce * CIce(T)))

}

#' Thermal diffusivity of firn
#'
#' Calculate the thermal diffusivity of firn based on its thermal
#' conductivity and density and on the heat capacity of ice at constant
#' pressure.
#'
#' @param T numeric vector of firn temperature [K].
#' @param rho numeric vector of firn density [kg/m^3].
#' @return numeric vector of thermal diffusivity of firn in [m^2/s].
#'
#' @references https://en.wikipedia.org/wiki/Thermal_diffusivity
#'
#' @author Thomas Laepple
#' @seealso \code{\link{KappaIce}}, \code{\link{KFirn}}, \code{\link{CIce}}
#' @examples
#'
#' # variable lengths of 'T' and 'rho' are supported:
#'
#' KappaFirn(273.15, 350)
#' KappaFirn(273.15, c(350, 500))
#' KappaFirn(c(273.15, 273.15 - 45), 500)
#'
#' # but lengths > 1 need to be equal
#' \dontrun{
#'  KappaFirn(c(273.15, 273.15 - 45), c(350, 500, 800))
#' }
#' KappaFirn(c(273.15, 273.15 - 45), c(350, 500))
#'
#' # firn becomes ice at a density of 920 kg/m^3, therefore
#' all.equal(KappaFirn(273.15 - 45, 920.), KappaIce(273.15 - 45)) # is TRUE
#' 
#' @export
#'
KappaFirn<-function(T, rho) {

  # 'T' and 'rho' must have same length if *both* are not vectors of length 1
  if ((length(T) != 1 & length(rho) != 1) & (length(T) != length(rho)))
    stop("'T' and 'rho' must have equal lengths, if both have lengths > 1.")

  return(KFirn(T, rho) / (rho * CIce(T)))
  
}

#' Temperature to isotope conversion
#'
#' Convert temperature to istopic data given a linear calibration regression.
#'
#' @param temperature numeric vector of temperatures in degree Celsius.
#' @param alpha slope of the calibration regression; in permil/(degree
#'   Celsius).
#' @param beta intercept of the calibration regression; in degree Celsius.
#' @return numeric vector of isotopic data in permil.
#'
#' @examples
#'
#' # spatial d18O to temperature calibration for Antarctica
#' # (Masson-Delmotte et al., J. Clim., 21(13), 2008)
#' Temperature2Isotopes(-44.5, 0.8, -8.1)
#'
#' # spatial d2H to temperature calibration for Antarctica
#' # (Masson-Delmotte et al., J. Clim., 21(13), 2008)
#' Temperature2Isotopes(-44.5, 6.3, -62.7)
#'
#' @author Thomas Münch
#' @export
#'
Temperature2Isotopes <- function(temperature, alpha, beta) {

  alpha * temperature + beta

}
