##' Thermal conductivity of firn W/(m K) Schwander et al., 1997
##' copied from Goujon, C., J.-M. Barnola, and C. Ritz (2003), Modeling the densification of polar firn including heat diffusion: Application to close-off charact##' eristics and gas isotopic fractionation for Antarctica and Greenland sites, J. Geophys. Res., 108(D24), 4792, doi:10.1029/2002JD003319.
##' 
##' @title Thermal conductivity of firn
##' @param T   firn temperature [K]
##' @param rho  firn density [kg/m^3]
##' @return Thermal conductivity of firn W/(mK)
##' @author Thomas Laepple
##' @references  Schwander et al., 1997
##' @export
KFirn<-function(T,rho)
    {
        
        rho.ice=920 
        return(KIce(T)*(rho/rho.ice)^(2-0.5*(rho/rho.ice)))
    }
