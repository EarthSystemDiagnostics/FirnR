##' function to diffuse a given record with depth-dependent diffusion length
##' by convolving it with a gaussian.
##' 
##' @title Diffuse a record 
##' @param rec record to be diffused
##' @param sigma either scalar = constant diffusion length
##' or vector of the same length as rec to define a
##' depth-dependent diffusion length [cm]
##' @param res resolution of record [cm]
##' @return diffused record on the original resolution
##' @author Thomas Muench, modified my Thomas Laepple
##' @examples
##' rec<-rnorm(n=1000)
##' var.sigma<-seq(1,8,length.out=1000)
##' diffused<-DiffuseRecord(rec=rec,sigma=var.sigma,res=1)
##' plot(rec,type='l',xlab="depth in cm")
##' lines(diffused,col="red")
##'
##' @export
DiffuseRecord <- function(rec,sigma,res=1){

## INPUT:
## rec: record to be diffused
## sigma: depth-dependent diffusion length [cm]
## res: resolution of record [cm]
    
NR=length(rec)

## record and sigma have to be of same length
if (NR!=length(sigma))
    stop('INPUT: rec and sigma must be of same length')

## scale diffusion length according to resolution of record
sigma=sigma/res

## pad end of record with mean of record to avoid NA's at the end of diffused record
rec=c(rec,rep(mean(rec,na.rm=TRUE),10*max(sigma)))

## updated record length
nNR=length(rec)

## vector to store diffused data
rec_t=rep(NA,nNR)

## loop over record
for (iM in 1:nNR){

    ## diffusion length for current depth
    if (iM>NR){
        sig=sigma[NR]
    } else {
        sig=sigma[iM]
    }
   
    ## set range (=2N+1) of convolution integral to ~5 times sig
    N=ceiling(5*sig)
    range=(iM-N):(iM+N)
    ## skip range above surface
    range=range[range>0]
    ## relative range for convolution kernel
    rel.range=iM-range
    
    ## kernel
    kernel=exp(-(rel.range)^2/(2*sig^2))
    kernel=kernel/sum(kernel)
    
    ## diffused data for given bin
    rec_t[iM]=sum(rec[range]*kernel)
    
}

return(rec_t[1:NR])

}
