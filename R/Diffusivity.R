
##' Diffusivity of water isotopes in firn
##'
##' Thomas: add details / references
##' @title Diffusivity of water isotopes in firn
##' @param rho firn density [kg/m^3]
##' @param T annual mean temperature [K]
##' @param P surface pressure [mBar]
##' @param dD dD  - *logical*; if true, the diffusivity for d2H is returned, otherwise for d18O
##' @return diffusivity in [cm^2/s]
##' @author Thomas Muench modified by Thomas Laepple
##' @examples
##' T<-(-60:0)
##' plot(T,Diffusivity(rho=340,T=273.15+T,P=650,dD=FALSE),type="l",lwd=2,main="temperature dependence of diffusivity",xlab="T (Celcius)",ylab="diffusivity (cm^2/s)")
##' lines(T,Diffusivity(rho=340,T=273.15+T,P=650,dD=TRUE),col="red",lwd=2)
##' legend("topleft",col=c("black","red"),lwd=2,c("d18O","dD"))
##' @export
Diffusivity <- function(rho,T,P,dD=FALSE)
{
    
                                        # Set constants
    R=8.314478                                    # Gas constant
    m=18.02e-3                                    # molar weight of H2O
    p=exp(9.5504+3.53*log(T)-5723.265/T-0.0073*T) # saturation vapour pressure #OK
    P0=1013.25                                        # standard atmospheric pressure [mbar]
    rho_i=920.                                    # density of ice [kg/m3]
    b=1.3                                         # tortuosity constant [Johnsen 2000]

                                        # Set fractionation factor
    if (dD) alpha=exp(16288/T^2-9.45e-2) else alpha=exp(11.839/T-28.224e-3)
    
                                        # Calculate diffusivity in air [m^2/s]
    Da=2.1e-5*(T/273.15)^(1.94)*(P0/P)
    if (dD) isotopeFactor=1.0251 else isotopeFactor=1.0285
    Dai=Da/isotopeFactor

                                        # Calculate tortuosity (after Johnsen, 2000)
    invtau=rep(NA,length(rho))
    for (ix in 1:length(rho))
        {
            if (rho[ix]<=rho_i/sqrt(b))
                invtau[ix]=1-b*(rho[ix]/rho_i)^(2)
            else
                invtau[ix]=0
        }

    ## Calculate isotope diffusivity in firn [m^2/s]
    D=m*p*invtau*Dai*(1/rho-1/rho_i)/(R*T*alpha)

    ## Return diffusivity in [cm^2/s]
    return(D*1e4)
}
