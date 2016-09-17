##' @title thermal diffusivity of firn
##' @param T  firn temperature [K]
##' @param rho firn density [kg/m^3]
##' @return thermal diffusivity of firn m^2/s
##' @author Thomas Laepple
##' @export
KappaFirn<-function(T,rho) 
    { 
         
        return(KFirn(T,rho)/(rho*CIce(T)))
    }
