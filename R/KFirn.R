
##' Thermal conductivity of firn.
##'
##' This function calculates the thermal conductivity of firn based on its
##' density and the thermal conductivity of ice.
##'
##' The implementation is based on the formula given in Goujon et al. (2003).
##'
##' Different lengths of \code{T} and \code{rho} are supported: The function
##' accepts both parameters constant, constant \code{T} and varying \code{rho},
##' or vive cersa, and varying \code{T} and \code{rho}. However, in the latter
##' case both vectors must have the same length, otherwise the function exits
##' with an error.
##' @references
##' Goujon, C., Barnola, J.-M., and Ritz, C.: Modeling the densification of
##' polar firn including heat diffusion: Application to close-off
##' characteristics and gas isotopic fractionation for Antarctica and Greenland
##' sites, J. Geophys. Res., 108(D24), 4792, 2003.
##' @param T Numeric vector of ambient firn temperature in [K].
##' @param rho Numeric vector of firn density [kg/m^3].
##' @return Numeric vector of the thermal conductivity of firn in [W/(m*K)].
##' @author Thomas Laepple
##' @seealso \code{\link{KIce}}
##' @export
KFirn<-function(T, rho) {

    # 'T' and 'rho' must have same length if *both* are not vectors of length 1
    if ((length(T) != 1 & length(rho) != 1) & (length(T) != length(rho)))
        stop("INPUT: 'T' and 'rho' have conflicting lengths different from 1.")
    
    kRhoIce <- 920.
    ratio <- rho / kRhoIce
    a <- 2 - 0.5 * ratio
    k.firn <- KIce(T) * ratio^a
    
    return(k.firn)
    
}

