##' @title thermal diffusivity of ice
##' @param T ice temperature [K]
##' @return thermal diffusivity m^2/s
##' @author Thomas Laepple
##' @export
KappaIce<-function(T) #Checked at 0 degree
    {
          rho.ice=920 
        return(KIce(T)/(rho.ice*CIce(T)))
      }
