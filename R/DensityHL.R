                                       
##' Herron Langway firn density model, analytical solution
##'
##' based on the Matlab Code from Alan Grinsted
##'  #https://www.mathworks.com/matlabcentral/fileexchange/47386-steady-state-snow-and-firn-density-model/content//densitymodel.m
##' @title Herron Langway firn density
##' @param rho.surface surface density in kg/m3
##' @param t.mean 10 firn mean temperature in Kelvin
##' @param bdot  accumulation rate in kg/m^2/year
##' @param depth snow depth in m
##' @return list(depth, rho) with snow depth and density in kg/m^3
##' @author Thomas Laepple
##' examples
##' result=DensityHL(rho.surface=340,t.mean=273.15-31.5,bdot=177,depth=0:150)
##' plot(result$rho,result$depth,ylim=c(150,0),xlim=c(200,1000),xlab="firn density",ylab="depth (m)",type="l",lwd=2,main="NorthGrip simulated density")
##' @export
 
DensityHL<-function(rho.surface,t.mean,bdot,depth=0:9000/100)
{   
    kRho.ice=920
    kRho.c=550 #density thresshold where the behaviour changes
    kRho.water=1000
    
    kR=8.314 #Gas Constant

    
    c0=11*(bdot/kRho.water)*exp(-10160/(kR*t.mean)) #0.008640288
    c1=575*sqrt(bdot/kRho.water)*exp(-21400/(kR*t.mean))
    k0=c0/bdot  # 0.0003119572
    k1=c1/bdot

    #critical depth at which rho=rhoc
    z.c=(log(kRho.c/(kRho.ice-kRho.c))-log(rho.surface/(kRho.ice-rho.surface)))/(k0*kRho.ice)

    index.upper<-which(depth<=z.c)
    index.lower<-which(depth>z.c)

    q<-rep(NA,length(depth))
    q[index.upper]=exp(k0*kRho.ice*depth[index.upper]+log(rho.surface/(kRho.ice-rho.surface)))
    q[index.lower]=exp(k1*kRho.ice*(depth[index.lower]-z.c)+log(kRho.c/(kRho.ice-kRho.c)))
    rho=q*kRho.ice/(1+q);
    
    #There might be a bug in the water equivalent conversion which leads to slight deviations, thus this part is not used 
   # t<-rep(NA,length(depth))
   # tc=(log(kRho.ice-rho.surface)-log(kRho.ice-kRho.c))/c0; 
   # t[index.upper]=(log(kRho.ice-rho.surface)-log(kRho.ice-rho[index.upper]))/c0;
   # t[index.lower]=(log(kRho.ice-kRho.c)-log(kRho.ice-rho[index.lower]))/c1+tc; 
                                        # depth.we=t*bdot/kRho.ice
    #
    return(list(depth=depth,rho=rho))

}
