
##' Simulate firn temperature.
##' 
##' This function simulates depth-dependent firn temperature based on the heat
##' transfer equation in firn for a surface temperature variation given as the
##' superposition of the first two harmonics of the annual cycle in air
##' temperature.
##'
##' The temperature profile of the firn simulated here is the solution to the
##' general heat transfer equation for constant thermal diffusivity, neglecting
##' heat advection, assuming no internal heat production, and given the boundary
##' condition of sinousoidal surface temperature variations (Paterson, 2002,
##' pp. 224, 206-207). Note that the phase \code{phi1 = 0}, \code{phi2 = 0} is
##' chosen such that the temperature maximum occurs at January 1.
##' @references
##' Paterson, W. S. B.: The Physics of Glaciers, Butterworth-Heinemann, 3rd
##' Edn. 1994, 496 p., reprinted with corrections 1998, 2001, 2002, 2002.
##' @param t Time in [years].
##' @param depth Numeric vector of snow depth in [m] increasing from 0 at the
##' surface downwards.
##' @param core List of the site parameters \code{A1}, \code{A2}, \code{phi1},
##' \code{phi2}, \code{T0}, \code{rho.surface} and \code{bdot} (see below),
##' alternativly \code{NULL}. In this case, the parameters have to be given
##' explicitly.
##' @param A1 Amplitude (half peak-peak) of the first harmonic of the seasonal
##' cycle in temperature, units of [K].
##' @param A2 Amplitude of the second harmonic in [K].
##' @param phi1 Phase of the first harmonic of the seasonal cycle in
##' temperature, units of [degree].
##' @param phi2 Phase of the second harmonic in [degree].
##' @param T0 Mean firn temperature in [K].
##' @param rho.surface Surface density in [kg/m^3].
##' @param bdot Local mass accumulation rate in [kg/m^2/year].
##' @param kappa Thermal diffusivity of firn in [m^2/s]. Defaults to \code{NULL}
##' which forces internal calculation with the surface density and mean firn
##' temperature as provided by \code{core} or given explicitly.
##' @return Numeric vector of temperature at the depth levels given by
##' \code{depth} evaluated at time \code{t}.
##' @author Thomas Laepple
##' @examples
##' ## Simulated EDML firn temperature profile for each month of the year
##' 
##' t <- (1 : 12) / 12
##' depth <- (0 : 1000) / 50
##' core <- list(name = "Kohnen", rho.surface = 345, T0 = 273.15 - 44.5,
##'              A1 = 13.2, A2 = 4.9, phi1 = -3.2, phi2 = 5.9)
##' 
##' plot(1 : 10, type = "n", xlim = c(-55, -25), ylim = c(20, 0), las = 1,
##'      xlab = "firn temperature (degree C)", ylab = "depth (m)",
##'      main = "Firn temperature at Kohnen (EDML)")
##' for (i in t) {
##'     j <- 12 * i
##'     T.firn <- FirnTemperature(i, depth, core)
##'     lines(T.firn - 273.15, depth, col = j, lty = ifelse(j <= 8, 1, 2))
##' }
##' legend("bottomright", month.name, lty = c(rep(1, 8), rep(2, 4)),
##'        col = 1 : 12, bty = "n")
##' @export
FirnTemperature <- function(t, depth, core = NULL, A1 = core$A1, A2 = core$A2,
                            phi1 = core$phi1, phi2 = core$phi2, T0 = core$T0,
                            rho.surface = core$rho.surface, bdot = core$bdot,
                            kappa = NULL) {

    # convert input phases from degree to radian
    deg2rad <- pi / 180.
    phi1 <- deg2rad * phi1
    phi2 <- deg2rad * phi2
    
    # convert input years to seconds
    seconds.in.year <- 3600 * 24 * 365
    t.second <- t * seconds.in.year

    # constant thermal firn diffusivity calculated from
    # surface density and mean temperature (default)
    if (is.null(kappa)) kappa <- KappaFirn(T0, rho.surface)
    
    omega <- (2 * pi) / seconds.in.year
    omega.rel <- omega / (2 * kappa)

    T.firn <- T0
    T.firn <- T.firn + A1 * exp(-depth * sqrt(omega.rel)) *
        cos(phi1 + omega * t.second - depth * sqrt(omega.rel))
    T.firn <- T.firn + A2 * exp(-depth * sqrt(2 * omega.rel)) *
        cos(phi2 + 2 * omega * t.second - depth * sqrt(2 * omega.rel))
    
    return(T.firn)
    
}
