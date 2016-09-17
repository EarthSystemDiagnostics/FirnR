##' Simulate the firn gemperature based on the Fourier law of heat diffusion
##' with an input signal given as the superposition of the annual cycle and its second harmonics
##' 
##'
##'
##' @title Simulate the Firn temperature
##' @param t time in years
##' @param z snow depth in m, surface = 0, 1 = 1m deep
##' @param core list of core parameters or alternativly NULL, in this case A1,A2 ...
##' have to be given explicitly
##' @param A1 Amplitude of first harmonic (~1/2 of seasonal range)
##' @param A2 Amplitude of second harmonic
##' @param phi1 Phase of first harmonic
##' @param phi2 Phase of second harmonic
##' @param T0 Mean temperature in Kelvin
##' @param kappa Thermal diffusivity of firn  m^2/s
##' @return vector of temperature at time t at the depth levels z
##' @author Thomas Laepple
##' @export
##' @examples
##'
##' plot(1:10,type="n",xlim=c(-45,-10),ylim=c(20,0),main="NGRIP")
##' for (iMonth in 1:12)
##' lines(FirnTemperature(iMonth/12,(0:1000)/50,A1=16.5,A2=3,phi1=0,phi2=0,T0=-31.5,kappa=KappaFirn(273-31.5,rho=320)),(0:1000)/50,col=iMonth)
##'
##' 
FirnTemperature<-function(t,z,core=NULL,A1=core$A1,A2=core$A2,phi1=core$phi1,phi2=core$phi2,T0=core$T0,kappa)
    {
        seconds.in.year=3600*24*365
        t.second=t*seconds.in.year
        omega=(2*pi)/seconds.in.year
        return(T0+A1*exp(-z*sqrt(omega/(2*kappa)))*cos(phi1+omega*t.second-z*sqrt(omega/(2*kappa)))
               +A2*exp(-z*sqrt((2*omega)/(2*kappa)))*cos(phi2+(2*omega)*t.second-z*sqrt((2*omega)/(2*kappa))))
    }
