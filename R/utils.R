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
    stop("Lengths of 'T' and 'rho' must be equal if both are > 1.")

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
    stop("Lengths of 'T' and 'rho' must be equal if both are > 1.")

  return(KFirn(T, rho) / (rho * CIce(T)))
  
}

#' Linear data transformation
#'
#' Apply a linear transformation (calibration) on input (proxy) data.
#'
#' @param x numeric vector of data to be linearly transformed.
#' @param alpha slope of the transformation.
#' @param beta intercept of the transformation.
#' @return numeric vector of the transformed data.
#'
#' @examples
#'
#' # spatial d18O to temperature calibration for Antarctica
#' # (Masson-Delmotte et al., J. Clim., 21(13), 2008)
#' CalibrateLinear(-44.5, 0.8, -8.1)
#'
#' # spatial d2H to temperature calibration for Antarctica
#' # (Masson-Delmotte et al., J. Clim., 21(13), 2008)
#' CalibrateLinear(-44.5, 6.3, -62.7)
#'
#' @author Thomas Münch
#' @export
#'
CalibrateLinear <- function(x, alpha, beta) {

  alpha * x + beta

}

#' Saturation vapour pressure over ice
#'
#' Calculate the saturation vapour pressure over ice, based on the
#' parameterization given in van der Wel et al. (2015) (Eq. 5).
#'
#' @param T numeric vector of ambient temperature in [K].
#' @return numeric vecor of the saturation vapour pressure in [Pa].
#'
#' @references
#' van der Wel, L. G., H. A. Been, R. S. W. van de Wal, C. J. P. P. Smeets and
#'   H. A. J. Meijer: Constraints on the d2H diffusion rate in firn from field
#'   measurements at Summit, Greenland. The Cryosphere, 9, 1089–1103,
#'   https://doi.org/10.5194/tc-9-1089-2015, 2015.
#'
#' @examples
#'
#' pSat(273.15 - c(0, 45))
#'
#' @author Thomas Münch
#' @export
#'
pSat <- function(T) {

  exp(9.5504 + 3.53 * log(T) - 5723.265 / T - 0.0073 * T)

}

#' Isotopic fractionation factors
#'
#' Calculate the fractionation factors of oxygen-18 and deuterium isotopes,
#' i.e. the difference in ratio of rare to abundant isotopes, in water vapour
#' over ice under equilibrium conditions, based on the expressions given in
#' Johnsen et al. (2000).
#'
#' @param T numeric vector of firn temperature in [K].
#' @param dD logical; of \code{TRUE} return the fractionation factor for
#'   deuterium, else for oxygen-18 (the default).
#' @return numeric vector of the equilibrium fractionation factor for oxygen-18
#'   or deuterium isotopes.
#'
#' @references
#' Johnsen, S. J., Clausen, H. B., Cuffey, K. M., Hoffmann, G., Schwander, J.,
#'   and Creyts, T.: Diffusion of stable isotopes in polar firn and ice: the
#'   isotope effect in firn diffusion, in: Physics of ice core records, edited
#'   by: Hondoh, T., vol. 159, Hokkaido Univ. Press, Sapporo, Japan, 121–140,
#'   2000.
#'
#' @examples
#'
#' alphaIso(273.15 - 45)
#' alphaIso(273.15 - 45, dD = TRUE)
#'
#' @author Thomas Münch
#' @export
#'
alphaIso <- function(T, dD = FALSE) {

  if (dD) {
    alpha <- exp(16288 / (T^2) - 9.45e-2)
  } else {
    alpha <- exp(11.839 / T - 28.224e-3)
  }

  return(alpha)

}

#' Water vapour diffusivity in air
#'
#' Calculate the water vapour diffusivity in air depending on ambient
#' temperature and pressure for the majour isotopologue species, based on the
#' formulae given in Johnsen et al. (2000) with the numerical isotopologue
#' factors as in Merlivat and Jouzel (1979).
#'
#' @param T numeric vector of ambient air temperature in [K].
#' @param P numeric vector of local surface pressure in [mbar].
#' @param species the isotopologue species; one of "abundant" (standard H2O
#'   molecule), "oxygen" (H2-18O molecule), or "deuterium" (HDO molecule).
#' @return numeric vector of water vapour diffusivity in air in [m^2/s] for the
#'   given isotopologue species.
#'
#' @references
#' Johnsen, S. J., Clausen, H. B., Cuffey, K. M., Hoffmann, G., Schwander, J.,
#'   and Creyts, T.: Diffusion of stable isotopes in polar firn and ice: the
#'   isotope effect in firn diffusion, in: Physics of ice core records, edited
#'   by: Hondoh, T., vol. 159, Hokkaido Univ. Press, Sapporo, Japan, 121–140,
#'   2000.
#'
#' Merlivat, L. and J. Jouzel: Global climatic interpretation of the
#'   deuterium-oxygen 18 relationship for precipitation. J. Geophys. Res.:
#'   Oceans, 84, C8, 5029-5033, https://doi.org/10.1029/JC084iC08p05029, 1979.
#'
#' @examples
#'
#' # diffusivity depends on the isotopic species
#' DAir(273.15 - 45, 650)
#' DAir(273.15 - 45, 650, species = "oxygen")
#' DAir(273.15 - 45, 650, species = "deuterium")
#'
#' # variable lengths of 'T' and 'P' are supported:
#'
#' DAir(273.15 - 45, c(650, 800))
#' DAir(c(273.15, 273.15 - 45), 650)
#'
#' # but lengths > 1 need to be equal
#' \dontrun{
#'  DAir(c(273.15, 273.15 - 45), c(650, 800, 1000))
#' }
#' DAir(c(273.15, 273.15 - 45), c(650, 800))
#'
#' @author Thomas Münch
#' @export
#'
DAir <- function(T, P, species = "abundant") {

  # check input vector lengths
  if ((length(T) != 1 & length(P) != 1) & (length(T) != length(P)))
    stop("Lengths of 'T' and 'P' must be equal if both are > 1.")

  # retrieve isotopologue factor
  fac <- match.arg(species, c("abundant", "oxygen", "deuterium")) %>%
    c(abundant = 1., oxygen = 1.0285, deuterium = 1.0251)[.] %>%
    unname()

  return(2.11e-5 * (T / 273.15)^(1.94) * (1013.25 / P) / fac)

}

#' Tortuosity factor in firn
#'
#' Calculate the tortuosity factor in firn as a function of firn density. The
#' tortuosity accounts for the shape of the open channels in the firn,
#' controlling the effective diffusivity of vapour molecules within the
#' pore space.
#'
#' @param rho numeric vector of firn density in [kg/m^3].
#' @param b numeric constant in the tortuosity expression from a fit in firn
#'   density to measured tortuosities; defaults to the value given in Johnsen et
#'   al., 2000 (see also Schwander et al., 1988).
#' @param inverse logical controlling the return value; for the default, the
#'   return value is the inverse of the tortuosity factor, i.e. the effective
#'   porosity for diffusive flux, which decreases from a value of 1 at zero
#'   density to 0 at the critical density of \code{rho_ice / sqrt(b)}
#'   (approximately 806.9 kg/m^3 for rho_ice = 920 kg/m^3 and b = 1.3).
#' @return numerical vector of tortuosity (for \code{inverse = FALSE}) or
#'   inverse tortuosity (for \code{inverse = TRUE}).
#'
#' @references
#'
#' Johnsen, S. J., Clausen, H. B., Cuffey, K. M., Hoffmann, G., Schwander, J.,
#'   and Creyts, T.: Diffusion of stable isotopes in polar firn and ice: the
#'   isotope effect in firn diffusion, in: Physics of ice core records, edited
#'   by: Hondoh, T., vol. 159, Hokkaido Univ. Press, Sapporo, Japan, 121–140,
#'   2000.
#'
#' Schwander, J., Stauffer, B., and Sigg, A.: Air Mixing in Firn and the Age of
#'   the Air at Pore Close-Off. Ann. Glac., 10, 141–145,
#'   https://doi.org/10.3189/S0260305500004328, 1988.
#'
#' @examples
#'
#' tauFirn(seq(400, 920, 10))
#'
#' @author Thomas Münch
#' @export
#'
tauFirn <- function(rho, b = 1.3, inverse = TRUE) {

  kRhoIce <- 920. # ice density

  invtau <- 1 - b * (rho / kRhoIce)^2
  invtau[invtau <= 0] <- 0

  if (inverse) return(invtau) else return(1 / invtau)

}

#' Bimodal harmonic model
#'
#' This function calculates a harmonic series with two modes for one annual
#' cycle at daily resolution given mean, amplitudes and phases. The harmonic
#' series is implemented with cosine functions; thus, zero phase shift
#' corresponds to the cycle maximum (January 1st).
#'
#' @param x Named numeric vector with five elements:
#' \describe{
#' \item{"A0":}{Mean value over one annual cycle [a.u.];}
#' \item{"A1":}{Amplitude (half peak-peak) of first mode (same unit as mean);}
#' \item{"A2":}{Amplitude of second mode (same unit as mean);}
#' \item{"phi1":}{Phase shift of first mode in [degree];}
#' \item{"phi2":}{Phase shift of second mode in [degree].}
#' }
#' @return Numeric vector of length 365 corresponding to one annual cycle of the
#'   harmonic series.
#'
#' @examples
#'
#' # Different harmonic series
#'
#' days <- 1 : 365
#' x1 <- c(A0 = -45, A1 = 10, A2 = 0, phi1 = 0, phi2 = 0)
#' x2 <- c(A0 = -45, A1 = 10, A2 = 5, phi1 = 10, phi2 = 50)
#'
#' plot(days, CreateHarmonicSeries(x1), type = "l",
#'      las = 1, ylim = c(-60, -30),
#'      xlab = "day of year", ylab = "annual cycle (a.u.)",
#'      main = "Different harmonic models")
#' lines(days, CreateHarmonicSeries(x2), col = 4)
#'
#' legend("topleft",
#'        c("simple sinusoid", "bi-modal with differing amplitude and phase"),
#'        col = c(1, 4), lty = 1, bty = "n")
#'
#' @author Thomas Laepple
#' @export
#'
CreateHarmonicSeries <- function(x) {

  if (!is.numeric(x)) stop("`x` must be numeric.")
  if (length(x) != 5) stop("`x` must have length 5.")
  if (!length(names(x))) stop("`x` must be a named vector.")

  if (!"A0" %in% names(x)) stop("Missing input `A0`.")
  if (!"A1" %in% names(x)) stop("Missing input `A1`.")
  if (!"A2" %in% names(x)) stop("Missing input `A2`.")
  if (!"phi1" %in% names(x)) stop("Missing input `phi1`.")
  if (!"phi2" %in% names(x)) stop("Missing input `phi2`.")

  A0 <- x["A0"]
  A1 <- x["A1"]
  A2 <- x["A2"]

  # convert input phases from degree to radian
  deg2rad <- pi / 180.
  phi1 <- deg2rad * x["phi1"]
  phi2 <- deg2rad * x["phi2"]

  days <- 1 : 365
  n <- length(days)
  omega <- 2 * pi / n

  h1 <- cos(omega * days + phi1)
  h2 <- cos(2 * omega * days + phi2)

  return(A0 + A1 * h1 + A2 * h2)

}
