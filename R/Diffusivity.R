
##' Calculate the diffusivity in polar firn.
##'
##' This function calculates the diffusivity in polar firn for the stable water
##' isotopes oxygen-18 and deuterium, depending on site-specific parameters.
##'
##' The calculations are primarily based on the following expressions in Johnsen
##' et al. (2000): The general expression for the diffusivity in firn of
##' oxygen-18 and deuterium is given by Eq. (17). The tortuosity of the firn is
##' accounted for by applying Eq. (18) with a tortuosity constant b = 1.3.  For
##' the diffusivity of water vapour in air we use Eq. (19). The temperature
##' dependence of the fractionation factors as well as the diffusivities in air
##' for oxygen-18 and deuterium are given in the text on p. 127. Note that here,
##' Johnsen et al. accidentally mix the factors controlling the isotope
##' diffusivities in air (compare the original reference Merlivat and Jouzel,
##' 1979). This has been accounted for. For the water vapour saturation vapour
##' pressure over ice we use, instead of the expression given on p. 126, the
##' parameterization given in van der Wel et al. (2015) (Eq. 5).
##' @references
##' Johnsen, S. J., Clausen, H. B., Cuffey, K. M., Hoffmann, G., Schwander, J.,
##' and Creyts, T.: Diffusion of stable isotopes in polar firn and ice: the
##' isotope effect in firn diffusion, in: Physics of ice core records, edited
##' by: Hondoh, T., vol. 159, Hokkaido Univ. Press, Sapporo, Japan, 121–140,
##' 2000.
##' 
##' Merlivat, L. and J. Jouzel: Global climatic interpretation of the
##' deuterium-oxygen 18 relationship for precipitation. J. Geophys. Res.:
##' Oceans, 84, C8, 5029-5033, doi: 10.1029/JC084iC08p05029, 1979.
##' 
##' van der Wel, L. G., H. A. Been, R. S. W. van de Wal, C. J. P. P. Smeets and
##' H. A. J. Meijer: Constraints on the d2H diffusion rate in firn from
##' field measurements at Summit, Greenland. The Cryosphere, 9, 1089–1103, doi:
##' 10.5194/tc-9-1089-2015, 2015.
##' @param rho Numeric vector of firn densities [kg/m^3] for which diffusivity
##'     is calculated. 
##' @param T firn temperature in [K].
##' @param P local surface pressure in [mbar].
##' @param dD if \code{TRUE} the diffusivity for deuterium is returned,
##'     otherwise for oxygen-18. Defaults to \code{FALSE}.
##' @return A numeric vector of calculated diffusivities in [cm^2/s] for the
##'     firn densities given by \code{rho}.
##' @author Thomas Muench, modified by Thomas Laepple
##' @examples
##' T <- (-60 : 0)
##' plot(T, Diffusivity(rho = 340, T = 273.15 + T, P = 650, dD = FALSE) * 10^6,
##'      type = "l", lwd = 2, las = 1,
##'      main = "temperature dependence of diffusivity",
##'      xlab="temperature (Celcius)", ylab = "diffusivity (10^-6 cm^2/s)")
##' lines(T, Diffusivity(rho = 340, T = 273.15 + T, P = 650, dD = TRUE) * 10^6,
##'       col = "red", lwd = 2)
##' legend("topleft", c("d18O", "dD"), col = c("black", "red"),
##'        lwd = 2, bty = "n")
##' @export
Diffusivity <- function(rho, T, P, dD = FALSE) {
    
    # Set physical constants
    kR <- 8.314478               # Gas constant [J/(K * mol)]
    kM <- 18.02e-3               # molar weight of H2O molecule [kg/mol]
    kP0 <- 1013.25               # standard atmospheric pressure [mbar]
    kRhoIce <- 920.              # density of ice [kg/m3]

    # Saturation vapour pressure over ice [Pa]
    p <- exp(9.5504 + 3.53 * log(T) - 5723.265 / T - 0.0073 * T)
    # Tortuosity constant
    b <- 1.3

    # Set fractionation factor
    if (dD) {
        alpha <- exp(16288 / (T^2) - 9.45e-2)
    } else {
        alpha <- exp(11.839 / T - 28.224e-3)
    }
    
    # Calculate water vapour diffusivity in air [m^2/s]
    Da <- 2.11e-5 * (T / 273.15)^(1.94) * (kP0 / P)
    if (dD) {
        isotopeFactor <- 1.0251
    } else {
        isotopeFactor <- 1.0285
    }
    Dai <- Da / isotopeFactor

    # Calculate tortuosity
    invtau <- rep(NA, length(rho))
    for (i in 1 : length(rho))
        {
            if (rho[i] <= kRhoIce / sqrt(b)) {
                invtau[i] <- 1 - b * (rho[i] / kRhoIce)^2
            } else {
                invtau[i] <- 0
            }
        }

    # Calculate isotope diffusivity in firn [m^2/s]
    D <- (kM * p * invtau * Dai * (1 / rho - 1 / kRhoIce)) / (kR * T * alpha)

    # Return firn diffusivity in [cm^2/s]
    return(D * 1e4)
    
}
