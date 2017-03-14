
##' Thermal diffusivity of firn.
##'
##' This function calculates the thermal diffusivity of firn from its thermal
##' conductivity and density and from the heat capacity of ice.
##'
##' Different lengths of \code{T} and \code{rho} are supported: The function
##' accepts both parameters constant, constant \code{T} and varying \code{rho},
##' or vive cersa, and varying \code{T} and \code{rho}. However, in the latter
##' case both vectors must have the same length, otherwise the function exits
##' with an error.
##' @param T Numeric vector of firn temperature [K].
##' @param rho Numeric vector of firn density [kg/m^3].
##' @return Numeric vector of thermal diffusivity of firn [m^2/s].
##' @author Thomas Laepple
##' @seealso \code{\link{KFirn}}, \code{\link{CIce}}
##' @export
KappaFirn<-function(T, rho) {

    # 'T' and 'rho' must have same length if *both* are not vectors of length 1
    if ((length(T) != 1 & length(rho) != 1) & (length(T) != length(rho)))
        stop("INPUT: 'T' and 'rho' have conflicting lengths different from 1.")

    kappa <- KFirn(T, rho) / (rho * CIce(T)) # Correct to just use CIce here??
    
    return(kappa)
    
}

