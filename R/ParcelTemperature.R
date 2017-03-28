
##' Simulate snow parcel temperature.
##' 
##' This function simulates the temperature against depth experienced by a snow
##' parcel starting at a particular time of the year based on the simple
##' vertical temperature profile of the firn for sinusoidally varying surface
##' temperatures.
##'
##' For each given parcel depth, the time elapsed since the parcel has been at
##' the surface is calculated assuming constant layer thickness, thus ignoring
##' densification. For each of these times, the firn temperature at the
##' corresponding depth is extracted from the current seasonal firn temperature
##' profile. The approximation of constant layer thickness is reasonable here
##' since the firn temperature is already below 5 m close to constant.
##' @param startTime Time of the year when the parcel is started at the surface
##' [years].
##' @param depth Numeric vector of snow depths in [m] marking the positions of
##' the snow parcel as it moves downwards in the firn.
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
##' @return Numeric vector of temperatures experienced by the snow parcel at the
##' depths given by \code{depth}.
##' @author Thomas Laepple
##' @seealso \code{\link{FirnTemperature}}
##' @examples
##' ## Simulated EDML temperature profile of a snow parcel starting in June
##' 
##' core <- list(name = "Kohnen", rho.surface = 345, T0 = 273.15 - 44.5,
##'              bdot = 72, A1 = 13.2, A2 = 4.9, phi1 = -3.2, phi2 = 5.9)
##' depth <- seq(from = 0, to = 10, by = 1 / 100)
##' plot(depth, ParcelTemperature(0.5, depth, core) - 273.15,
##'      type = "l", las = 1, ylim = c(-55, -25),
##'      main = "Temperature of a snow parcel at Kohnen started in June",
##'      xlab = "depth (m)", ylab = "parcel temperature (degree C)")
##' @export
ParcelTemperature <- function(startTime, depth, core = NULL, A1 = core$A1,
                              A2 = core$A2, phi1 = core$phi1, phi2 = core$phi2,
                              T0 = core$T0, rho.surface = core$rho.surface,
                              bdot = core$bdot, kappa = NULL) {

    # fill core list if empty
    if (is.null(core)) {
        core <- list(
            A1 = A1, A2 = A2, phi1 = phi1, phi2 = phi2, T0 = T0,
            rho.surface = rho.surface, bdot = bdot)
    }
    
    T.parcel <- vector()

    # constant thermal firn diffusivity calculated from
    # surface density and mean temperature (default)
    if (is.null(kappa)) kappa <- KappaFirn(T0, rho.surface)
    
    for (i in 1 : length(depth)) {

        # time elapsed since the parcel has been at the surface assuming
        # constant layer thickness
        delta.t <- depth[i] * rho.surface / bdot

        T.parcel[i] <- FirnTemperature(t = startTime + delta.t,
                                       depth = depth[i], core = core,
                                       kappa = kappa)
        
    }
    
    return(T.parcel)
    
}

