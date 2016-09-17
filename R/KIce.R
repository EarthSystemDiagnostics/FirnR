##' Thermal conductivity of ice W/(mK) Weller and Schwerdtfeger 1971
##' copied from Goujon, C., J.-M. Barnola, and C. Ritz (2003), Modeling the densification of polar firn including heat diffusion:
##' Application to close-off characteristics and gas isotopic fractionation for Antarctica and Greenland sites,
##' J. Geophys. Res., 108(D24), 4792, doi:10.1029/2002JD003319.
##'
##' @title Thermal conductivity of ice W/(mK)
##' @param T  firn temperature [K]
##' @return  Thermal conductivity of ice W/(m K)
##' @author Thomas Laepple
##' @export
KIce<-function(T)
    {
            
        return(2.22*(1-0.0067*(T-273.15)))
    }
