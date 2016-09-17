
##' Diffusion length in firn
##'
##' Thomas add details
##' @title water isotope diffusion length in firn
##' @param rho  firn density *vector* [kg/m^3]; if not provided, a linear density profile is used
##' @param z firn depth *vector* [m]; observation points of 'rho', must be of same length as 'rho'
##' @param dz depth resolution *vector* [m]; may vary over 'z', must be of same length as 'z'
##' @param T temperature [K], either a scalar value or a vector of the same length a rho
##' @param P surface pressure [atm]
##' @param b annual mean accumulation rate m.w.e per year?  
##' @param dD *logical*; if true, the diffusion length for d2H is returned, otherwise for d18O
##' @return list(z=z,rho=rho,sigma=sigma), firn depth, density, diffusion length in cm
##' @author Thomas Muench modified by Thomas Laepple
##' @examples
##' depth<-0:150
##' t.mean<-273.15-30
##' bdot=200
##' rho<-DensityHL(rho.surface=340,t.mean=273.15-31.5,bdot=bdot,depth=depth)$rho
##' result.dO18<-DiffusionLength(rho,depth,dz=1,T=t.mean,b=bdot/1000,dD=FALSE)
##' result.dD<-DiffusionLength(rho,depth,dz=1,T=t.mean,b=bdot/1000,dD=TRUE)
##' plot(result.dO18$sigma,result.dO18$z,ylim=c(150,0),xlim=c(0,12),xlab="diffusion length (cm)",ylab="depth (m)",type="l",lwd=2,main="NorthGrip, no thinning")
##' lines(result.dD$sigma,result.dD$z,lwd=2,col="red")
##' legend("topleft",col=c("black","red"),lwd=2,c("d18O","dD"),bty="n")
##' @export
DiffusionLength <- function(rho=NULL,z,dz,T,P=0.65,b=0.064,dD=FALSE){
                                        # Set constants
    R=8.314478    # Gas constant
    m=18.02e-3    # molar weight of H2O
    rho_s = 350.  # surface density [kg/m3]
    rho_d = 804.  # density at which ice becomes impermeable to diffusion
    rho_w = 1000. # density of water


    if (length(T)==1) T<-rep(T,length(rho))
    if (length(T) != length(rho)) stop("T and rho have a different length")
                                        # Set density profile if not given as input
    if (!length(rho)){
        z=seq(0,100,length.out=1000) # linear depth scale
        z=z[-length(z)];z=z+z[2]
        dz=diff(z)[1]
        
        rho=seq(rho_s,rho_d,length.out=length(z))
        rho=rho[-length(z)]
        z=z[-length(z)]
    }

                                        # Set time scale accounting for densification
    time_d=cumsum(dz/b*rho/rho_w)
    ts=time_d*365.25*24*3600      # convert years to seconds

                                        # Integrate diffusivity along the density gradient to obtain diffusion length
    drho = diff(rho)
    dtdrho = diff(ts)/diff(rho)

    D<-vector()
    for (i in 1:length(rho)) D[i]<-Diffusivity(rho[i],T[i],P,dD=dD)
    
    D=D[-length(D)]
    rho=rho[-length(rho)]
    z=z[-length(z)]

                                        # Integrate diffusion length [cm]
    sigma_sqrd_dummy = 2*(rho^2)*dtdrho*D
    sigma_sqrd = cumsum(sigma_sqrd_dummy*drho)
    sigma=sqrt(1/(rho^2)*sigma_sqrd)

    return(sigma=list(z=z,rho=rho,sigma=sigma))

}








