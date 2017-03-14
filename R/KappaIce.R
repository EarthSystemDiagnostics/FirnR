
##' Thermal diffusivity of ice.
##'
##' This function calculates the thermal diffusivity of ice from its thermal
##' conductivity, density and heat capacity.
##' @param T Numeric vector of ice temperature in [K].
##' @return Numeric vector of thermal diffusivity [m^2/s].
##' @author Thomas Laepple
##' @seealso \code{\link{KIce}}, \code{\link{CIce}}
##' @export
KappaIce<-function(T) {

    kRhoIce <- 920.
    kappa <- KIce(T) / (kRhoIce * CIce(T))
    
    return(kappa)

}
