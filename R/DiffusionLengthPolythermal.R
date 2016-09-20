

##' Diffusion length, accounting for the seasonal cycle of temperature in the firn
##'
##' 
##' @title Diffusion length, accounting for the seasonal cycle of temperature in the firn
##' @param core list of core parameters (minimum A1,A2,phi1,phi2,T0,bdot,P)
##' have to be given explicitly
##' @param depth snow depth in m, vector of same length as rho, alterativly this can be supplied in the core list
##' @param rho snow density in (kg/m3), vector of same length as depth, alterativly this can be supplied in the core list
##' @param bParcel (FALSE) calculate the mean diffusion of the firn columns of every month, (TRUE) calculate the mean diffusion of snow parcels, starting every month
##' @param dD *logical*; if true, the diffusion length for d2H is returned, otherwise for d18O
##' @return vector of diffusion length in (cm) on the depths provided by the depth vector
##' @author Thomas Laepple
##'
##' @examples
##' core<-list(lat=-75.00,lon=0,bdot=72,rho.surface=345,T0=273.15-44.5,A1=16.7,A2=6.6,phi1=0,phi2=0,P=650,name="Kohnen")
##' depth<-seq(from=0,to=20,by=1/100)
##' rho<-DensityHL(core$rho.surface,t.mean=core$T0,bdot=core$bdot,depth=depth)$rho
##' sigma.dO18.uni<-DiffusionLength(depth,rho,T=core$T0,bdot=core$bdot,dD=FALSE)
##' sigma.dO18.Tparcel<-DiffusionLengthPolythermal(core,depth,rho,bParcel=TRUE,dD=FALSE)
##' sigma.dO18.TFirn<-DiffusionLengthPolythermal(core,depth,rho,bParcel=FALSE,dD=FALSE)
##' plot(sigma.dO18.uni,depth,ylim=rev(range(depth)),xlim=c(0,10),xlab="diffusion length (cm)",ylab="depth (m)",type="l",lwd=2,main="Kohnen d18O")
##' lines(sigma.dO18.Tparcel,depth,lwd=2,col="red")
##'  lines(sigma.dO18.TFirn,depth,lwd=2,col="green")
##' legend("topleft",col=c("black","red","green"),lwd=2,c("mean Temperature","parcel T","firn T"),bty="n") 
##'
##' @export
DiffusionLengthPolythermal<-function(core=NULL,depth=core$depth, rho=core$rho, bParcel=FALSE,dD=FALSE)
{
    if (length(rho) != length(depth)) stop("depth and rho vector have to have the same length")
    save<-matrix(NA,12,length(depth))
    for (iTime in 1:12)
        {
            if (bParcel)
                {
                    T<-ParcelTemperature(iTime/12,depth,core)
                } else
                    {
                        T<-FirnTemperature(iTime/12,depth,core,kappa=KappaFirn(core$T0,rho=rho))
                    }
            
            sigma<-DiffusionLength(depth=depth,rho=rho,T=T,P=core$P,bdot=core$bdot,dD=dD)
            save[iTime,]<-sigma
        }
    return(colMeans(save))
}


