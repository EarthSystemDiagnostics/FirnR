#' Firn diffusivity
#'
#' Calculate the diffusivity in polar firn for the stable water isotopologues
#' H2-18O (oxygen-16 atom replaced by oxygen-18) and HDO (one hydrogen atom
#' replaced by deuterium), depending on site-specific parameters.
#'
#' The calculation of the firn diffusivity is primarily based on the expressions
#' in Johnsen et al. (2000): The general expression for the diffusivity in firn
#' of oxygen-18 and deuterium is given by Eq. (17). The tortuosity of the firn
#' is accounted for by applying Eq. (18) with a tortuosity constant b = 1.3. For
#' the diffusivity of water vapour in air we use Eq. (19). The temperature
#' dependence of the fractionation factors as well as the diffusivities in air
#' for oxygen-18 and deuterium are given in the text on p. 127. However, note
#' that Johnsen et al. here accidentally mix the factors controlling the isotope
#' diffusivities in air; the correct factors are given in Merlivat and Jouzel,
#' 1979. For the water vapour saturation vapour pressure over ice we use instead
#' of Johnsen et al.'s expression the newer parameterization given in van der
#' Wel et al. (2015) (Eq. 5).
#'
#' @param rho numeric vector of firn density in [kg/m^3] at which diffusivity is
#'   calculated.
#' @param T firn temperature in [K].
#' @param P local surface pressure in [mbar].
#' @param dD if \code{TRUE} the diffusivity for deuterium is returned,
#'     otherwise for oxygen-18. Defaults to \code{FALSE}.
#' @return numeric vector of firn diffusivitiy in [m^2/s].
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
#' van der Wel, L. G., H. A. Been, R. S. W. van de Wal, C. J. P. P. Smeets and
#'   H. A. J. Meijer: Constraints on the d2H diffusion rate in firn from field
#'   measurements at Summit, Greenland. The Cryosphere, 9, 1089–1103,
#'   https://doi.org/10.5194/tc-9-1089-2015, 2015.
#'
#' @examples
#'
#' # temperature dependence of diffusivity
#' T <- (-60 : 0)
#' plot(T, Diffusivity(rho = 340, T = 273.15 + T, P = 650, dD = FALSE) * 10^10,
#'      type = "l", lwd = 2, las = 1,
#'      main = "Temperature dependence of diffusivity",
#'      xlab = "Temperature (Celcius)", ylab = "Diffusivity (10^-6 cm^2/s)")
#' lines(T, Diffusivity(rho = 340, T = 273.15 + T, P = 650, dD = TRUE) * 10^10,
#'       col = "red", lwd = 2)
#' legend("topleft", c("d18O", "dD"), col = c("black", "red"),
#'        lwd = 2, bty = "n")
#'
#' # density dependence of diffusivity
#' rho <- seq(300, 900, 10)
#' plot(rho,
#'      Diffusivity(rho = rho, T = 273.15 - 45, P = 650, dD = FALSE) * 10^10,
#'      type = "l", lwd = 2, las = 1,
#'      main = "Density dependence of diffusivity",
#'      xlab = "Density (kg/m^3)", ylab = "Diffusivity (10^-6 cm^2/s)")
#'
#' @author Thomas Münch, Thomas Laepple
#' @seealso \code{\link{pSat}}, \code{\link{alphaIso}}, \code{\link{DAir}},
#'   \code{\link{tauFirn}}
#' @export
#'
Diffusivity <- function(rho, T, P, dD = FALSE) {

  # physical constants
  kR <- 8.314478       # gas constant [J/(K * mol)]
  kM <- 18.02e-3       # molar weight of H2O molecule [kg/mol]
  kRhoIce <- 920.      # density of ice [kg/m3]

  # requested isotopologue species
  species <- c("oxygen", "deuterium")[c(!dD, dD)]

  # diffusivity-controlling variables
  p      <- pSat(T)                   # saturation vapour pressure
  alpha  <- alphaIso(T, dD = dD)      # fractionation factor
  Dai    <- DAir(T, P, species)       # water isotopologue diffusivity in air
  invtau <- tauFirn(rho)              # inverse tortuosity

  # calculate isotope diffusivity in firn in [m^2/s]
  D <- (kM * p * invtau * Dai * (1 / rho - 1 / kRhoIce)) / (kR * T * alpha)

  return(D)

}
