


##' 
##'
##' The signal is created against time (on a w.e. scale) and than transferred to snow-depth
##' @title Simulate isotope depth series against snow-depth
##' @param core list with core parameters, used are (depth, rho, rho.surface, bdot)
##' @param fractionSeasonal fraction seasonal cycle vs. noise (1 = pure seasonal cycle, 0 = pure noise). Ratio in variance units with the noise variance determined by integrating the spectrum from 0 to f.integrate.variance
##' @param simLength length of the simulation in m snow
##' @param res resolution of the simulation in m snow (e.g. 1/1000 = 1mm)
##' @param res.we resolution of the simulation in m w.e. snow (by default 1/3 of res)
##' @param bRandomPhase TRUE: use a random phasing of the seasonal cyle, FALSE = starts at the peak / summer at the surface
##' @param noise.snow NULL: simulate new noise, if not: vector with the noise; to be used if multiple simulations with the same noise are needed
##' @param rescaling.factor slope between temperature and the signal of interest (e.g. dO18)
##' @param f.cutoff.noise cutoff frequency on the noise in 1/[surface snow length in m] (e.g. 1/(1/100) = 100) = 10mm
##' @param f.integrate.variance frequency that determines which part of the spectrum is considered in the noise variance 1/[snow length in m] (e.g. 1/(1/50) = 50) = corresponding to a sampling on 1cm = Nyquist of 2cm
##' @param method specifies the interpolation method to be used. Choices are
##'   \code{"linear"} or \code{"constant"}; see \code{\link[stats]{approx}}.
##' @return  list(depth=depth.snow,time=time,signal=signal*rescaling.factor,noise.snow=noise.snow)  snow depth in m, time in years, signal, noise.snow (for further use)
##' @author Thomas Laepple
##' @examples
##' \dontrun{
##'  ##' 
##' 
##'   core<-list(lat=-75.00,lon=0,bdot=72,rho.surface=345,T0=273.15-44.5,A1=16.7,A2=6.6,phi1=0,phi2=0,P=650,name="Kohnen")
##'   core$depth<-seq(from=0,to=20,by=1/100)
##'   core$rho<-DensityHL(rho.surface=core$rho.surface,T=core$T0,bdot=core$bdot,depth=core$depth)
##' 
##' signal.1<-SimulateFirnSignal(core,simLength=30,fractionSeasonal=1)
##' signal.05<-SimulateFirnSignal(core,simLength=30,fractionSeasonal=0.5)
##' signal.05.var1mm<-SimulateFirnSignal(core,simLength=30,fractionSeasonal=0.5,f.integrate.variance=500)
##' signal.05.noise.1cm<-SimulateFirnSignal(core,simLength=30,fractionSeasonal=0.5,f.cutoff.noise=50)
##' 
##' par(mfrow=c(3,2))
##' plot(signal.1$depth,signal.1$signal,type="l",xlab="snow depth m",ylab="d18O",main="100% seasonal cycle")
##' plot(signal.1$time,signal.1$signal,type="l",xlab="time (year)",ylab="d18O",xlim=c(0,5),main="100% seasonal cycle")
##' plot(signal.1$depth,signal.1$signal,type="l",xlab="snow depth m",ylab="d18O",main="100% seasonal cycle",xlim=c(0,1))
##' plot(signal.1$depth,signal.05$signal,type="l",xlab="snow depth m",ylab="d18O",main="50% seasonal cycle, meas 1cm resolution",xlim=c(0,1))
##' plot(signal.1$depth,signal.05.var1mm$signal,type="l",xlab="snow depth m",ylab="d18O",main="50% seasonal cycle, meas 1mm resolution",xlim=c(0,1))
##' plot(signal.1$depth,signal.05.noise.1cm$signal,type="l",xlab="snow depth m",ylab="d18O",main="50% seasonal cycle, meas and band limited 1cm resolution",xlim=c(0,1))
##' 
##' 
##' quartz()
##' LPlot(SpecMTM(pTs(signal.1$signal,signal.1$depth)),conf=FALSE)
##' LLines(SpecMTM(pTs(signal.05$signal,signal.1$depth)),col="red",conf=FALSE)
##' LLines(SpecMTM(pTs(signal.05.noise.1cm$signal,signal.1$depth)),col="green",conf=FALSE)
##' LLines(SpecMTM(pTs(signal.05.var1mm$signal,signal.1$depth)),col="cyan",conf=FALSE)
##' 
##' 
##' 
##' ##Repeat with no densification
##' 
##' 
##'   core<-list(lat=-75.00,lon=0,bdot=72,rho.surface=345,T0=273.15-44.5,A1=16.7,A2=6.6,phi1=0,phi2=0,P=650,name="Kohnen")
##'   core$depth<-seq(from=0,to=20,by=1/100)
##'   core$rho<-rep(300,length(core$depth))
##' 
##' signal.1<-SimulateFirnSignal(core,simLength=30,fractionSeasonal=1)
##' signal.05<-SimulateFirnSignal(core,simLength=30,fractionSeasonal=0.5)
##' signal.05.var1mm<-SimulateFirnSignal(core,simLength=30,fractionSeasonal=0.5,f.integrate.variance=500)
##' signal.05.noise.1cm<-SimulateFirnSignal(core,simLength=30,fractionSeasonal=0.5,f.cutoff.noise=50)
##' 
##' par(mfrow=c(3,2))
##' plot(signal.1$depth,signal.1$signal,type="l",xlab="snow depth m",ylab="d18O",main="100% seasonal cycle")
##' plot(signal.1$time,signal.1$signal,type="l",xlab="time (year)",ylab="d18O",xlim=c(0,5),main="100% seasonal cycle")
##' plot(signal.1$depth,signal.1$signal,type="l",xlab="snow depth m",ylab="d18O",main="100% seasonal cycle",xlim=c(0,1))
##' plot(signal.1$depth,signal.05$signal,type="l",xlab="snow depth m",ylab="d18O",main="50% seasonal cycle, meas 1cm resolution",xlim=c(0,1))
##' plot(signal.1$depth,signal.05.var1mm$signal,type="l",xlab="snow depth m",ylab="d18O",main="50% seasonal cycle, meas 1mm resolution",xlim=c(0,1))
##' plot(signal.1$depth,signal.05.noise.1cm$signal,type="l",xlab="snow depth m",ylab="d18O",main="50% seasonal cycle, meas and band limited 1cm resolution",xlim=c(0,1))
##' 
##' 
##' quartz()
##' LPlot(SpecMTM(pTs(signal.1$signal,signal.1$depth)),conf=FALSE)
##' LLines(SpecMTM(pTs(signal.05$signal,signal.1$depth)),col="red",conf=FALSE)
##' LLines(SpecMTM(pTs(signal.05.noise.1cm$signal,signal.1$depth)),col="green",conf=FALSE)
##' LLines(SpecMTM(pTs(signal.05.var1mm$signal,signal.1$depth)),col="cyan",conf=FALSE)
##' 
##' }
##' @export
SimulateFirnSignal<-function(core,simLength,fractionSeasonal=1,res=1/1000,res.we=res/3,bRandomPhase=FALSE,noise.snow=NULL,rescaling.factor=0.5,f.cutoff.noise=NULL,
                      f.integrate.variance=50,method="constant") 
    {

        k.filter=4
        
        if (!is.null(f.cutoff.noise))
            if (f.integrate.variance > f.cutoff.noise) warning("Noise cutoff is at a lower frequency as the variance measure frequency")
        
        targetVariance<-(core$A1^2/2)+(core$A2^2/2) #Variance of the pure seasonal cycle
        depth.snow<-seq(from=0,to=simLength,by=res) #snow depth on which the simulation takes place (in m)
        
        rho.interpolated<-stats::approx(core$depth,core$rho,depth.snow,rule=2)$y #interpolate the density to the target snow-depth
        depth.we<-Convert2WE(rho.interpolated,depth.snow)  #and get the (non-equidistant) w.e. depth corresponding to the target snow-depth layers

        time=depth.we/core$bdot*1000 #get the corresponding time in years
        
        if (is.null(noise.snow))
            {
                depth.we.equidistant<-seq(from=0,to=max(depth.we)+res.we,by=res.we) #equidistant depth in w.eq in m, a bit longer to ensure no missing value after interpolation
                
                if (!is.null(f.cutoff.noise))
                    {
                         #Cutoff frequency for the noise in water equivalent = increases the frequency as we compress it
                        f.cutoff.noise.we<-f.cutoff.noise/(core$rho.surface)*1000
                        
                        filter.length<-round((1/(f.cutoff.noise.we*res.we)))*k.filter+1
                        filter.lp<-Lowpass(f.cutoff.noise.we,filter.length,sample=1/res.we)

                        noise.we<-stats::rnorm(length(depth.we.equidistant)+filter.length+2)
                        noise.we.filtered<-ApplyFilter(noise.we,filter.lp)[seq(depth.we.equidistant)+filter.length/2+1]
                    } else noise.we.filtered<-stats::rnorm(length(depth.we.equidistant))
                

                 #frequency that determines which part of the spectrum is considered in the noise variance in water equivalent = increases the frequency as we compress it
                                       
                f.integrate.variance.we<-f.integrate.variance/(core$rho.surface)*1000

             
                f.nyquist<-0.5/res.we
                variance.noise<-f.integrate.variance.we / f.nyquist
                noise.we.filtered.rescaled<-noise.we.filtered/sqrt(variance.noise)*sqrt(targetVariance)
                noise.snow<-stats::approx(depth.we.equidistant, noise.we.filtered.rescaled,depth.we,method=method)$y
            }

        if (length(noise.snow) != length(depth.snow)) stop("length of noise.snow vector != required length")
        
        if (fractionSeasonal == 0) signal=noise.snow
        else {
            phase=0
            if (bRandomPhase) phase=stats::runif(1,min=0,max=2*pi)
            signal.seasonal<-core$A1*cos(phase+depth.we*2*pi/core$bdot*1000)+core$A2*cos(2*phase+depth.we*4*pi/core$bdot*1000)
         
            
            if (fractionSeasonal>0.99) signal=signal.seasonal
            else
                {
                    signal<-sqrt(1-fractionSeasonal)*noise.snow+signal.seasonal*sqrt(fractionSeasonal)
                }
        } 
        return(list(depth=depth.snow,time=time,signal=signal*rescaling.factor,noise.snow=noise.snow))
        
    }



