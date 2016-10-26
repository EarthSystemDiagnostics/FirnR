
##' Diffusion length in firn
##'
##' Thomas add details
##' @title water isotope diffusion length in firn
##' @param depth firn depth [m] *vector*
##' @param rho firn density [kg/m^3];, either scalar or *vector*  of size as z
##' @param T temperature [K], either scalar value or  vector of size as z
##' @param P surface pressure [mbar]
##' @param bdot accumulation rate in [kg/m^2/year]
##' @param dD *logical*; if true, the diffusion length for d2H is returned, otherwise for d18O
##' @param bFill *logical* if true, than fill the last value for which the density gradients are unknown with the same diffusion lengthcc
##' @param z firn depth *vector* [m]; depth at which the diffusion is calculated 
##' @return list(z=z,rho=rho,sigma=sigma), firn depth, density, diffusion length in cm
##' @author Thomas Muench modified by Thomas Laepple
##' @examples
##' depth<-0:150  
##' t.mean<-273.15-30
##' bdot=200
##' rho<-DensityHL(rho.surface=340,t.mean=t.mean,bdot=bdot,depth=depth)
##' sigma.dO18<-DiffusionLength(depth,rho,T=t.mean,bdot=bdot,dD=FALSE)
##' sigma.dD<-DiffusionLength(depth,rho,T=t.mean,bdot=bdot,dD=TRUE)
##' plot(sigma.dO18,depth,ylim=c(150,0),xlim=c(0,12),xlab="diffusion length (cm)",ylab="depth (m)",type="l",lwd=2,main="NorthGrip, no thinning")
##' lines(sigma.dD,depth,lwd=2,col="red")
##' legend("topleft",col=c("black","red"),lwd=2,c("d18O","dD"),bty="n") 
##' @export
DiffusionLength <- function(depth,rho,T,P=650,bdot,dD=FALSE,bFill=TRUE){
                                        # Set constants
    R=8.314478    # Gas constant
    m=18.02e-3    # molar weight of H2O
    rho_s = 350.  # surface density [kg/m3]
    rho_d = 804.  # density at which ice becomes impermeable to diffusion
    rho_w = 1000. # density of water

    z=depth

    if (length(T)==1) T<-rep(T,length(z))
    if (length(rho)==1) rho<-rep(rho,length(z))

    if (length(T) != length(rho)) stop("T and rho have a different length")
    if (length(z) != length(rho)) stop("z and rho have a different length")
    
                                        # Set density profile if not given as input
    dz<-c(diff(z),mean(diff(z)))        #get dz in (cm) from the z vector (in m) + extend with the mean   #CHECK#
    
                                        # Set time scale accounting for densification
    time_d=cumsum(dz/(bdot/1000)*rho/rho_w)
    ts=time_d*365.25*24*3600      # convert years to seconds

                                        # Integrate diffusivity along the density gradient to obtain diffusion length
    drho = c(diff(rho),0)
    dtdrho = c(diff(ts)/diff(rho),0) #extend with 0 to continue with a constant diffusion; if bFill = FALSE, this value gets not returned

    D<-vector()
    for (i in 1:length(rho)) D[i]<-Diffusivity(rho[i],T[i],P,dD=dD)
   

                                        # Integrate diffusion length [cm]
    sigma_sqrd_dummy = 2*(rho^2)*dtdrho*D
    sigma_sqrd = cumsum(sigma_sqrd_dummy*drho)
    sigma=sqrt(1/(rho^2)*sigma_sqrd)

    if (!bFill) sigma<-sigma[-length(sigma)]  #Don't return the last value (filled with a constant diffusion rate) if bFill=FALSE

    return(sigma)

}








