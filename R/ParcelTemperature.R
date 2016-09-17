
##' Simulate temperature against depth for a snow parcel starting at a particular season
##' based on the Fourier law of heat diffusion. This assumes a constant layer thickness, thus ignoring densification. As the temperature below 5m is close to constant, this approximation should be reasonable. 
##' @title Temperature of a snow parcel 
##' @param startTime time in the year when the parcel started (years)
##' @param depth depth vector (m)
##' @param core list containing the core parameters (here A1,A2,phi1,phi2,T0,density and accum) are used
##' @return vector of temperatures at the depths given by the depth vector
##' @author Thomas Laepple
##' @examples
##' 
##' kohnen<-list(lat=-75.00,lon=0,accum=72,density=345,T0=273.15-44.5,A1=16.7,A2=6.6,phi1=0,phi2=0,P=0.65,name="Kohnen")
##' depth<-seq(from=0,to=10,by=1/100)
##' plot(depth,ParcelTemperature(0.5,depth,kohnen),main="Temperature of a parcel at Kohnen",xlab="snow depth",ylab="T",type="l")
##' 
##' @export
ParcelTemperature<-function(startTime,depth,core)
    {
        TProfile<-vector()

        for (i in 1:length(depth))
            {
                deltaTime<-depth[i]/(core$accum/core$density) #time elapsed to since the surface
                TProfile[i]<-FirnTemperature(startTime+deltaTime,depth[i],core,kappa=KappaFirn(core$T0,rho=core$density))
            }
        return(TProfile)
    }
