
##' #Paterson 1994
##'copied from Goujon, C., J.-M. Barnola, and C. Ritz (2003), Modeling the densification of polar firn including heat diffusion:
##' Application to close-off characteristics and gas isotopic fractionation for Antarctica and Greenland sites,
##' J. Geophys. Res., 108(D24), 4792, doi:10.1029/2002JD003319.
##'
##' 
##' @title Specific heat capacity of ice 
##' @param T  ice temperature [K]
##' @return specific heat capacity of ice J/(kg K)
##' @author Thomas Laepple
##' @export
CIce<-function(T) #checked at 273 degree
    {
            
        return(152.5+7.122*T)
    }
