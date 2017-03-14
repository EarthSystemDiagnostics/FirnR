
##' Thermal conductivity of ice.
##'
##' This function calculates the thermal conductivity of ice depending on
##' ambient temperature.
##'
##' The implementation is based on the formula given in Goujon et al. (2003).
##' @references
##' Goujon, C., Barnola, J.-M., and Ritz, C.: Modeling the densification of
##' polar firn including heat diffusion: Application to close-off
##' characteristics and gas isotopic fractionation for Antarctica and Greenland
##' sites, J. Geophys. Res., 108(D24), 4792, 2003.
##' @param T Numeric vector of ambient firn temperature in [K].
##' @return Numeric vector of thermal conductivity of ice in [W/(m*K)].
##' @author Thomas Laepple
##' @export
KIce<-function(T) {
            
    return(2.22 * (1 - 0.0067 * (T - 273.15)))

}
