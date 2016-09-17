##' Convert data in water equivalent depth to snow depth
##'
##' 
##' @title Water equivalent depth to snow depth conversion
##' @param rho vector of density [kg/m^3]; 
##' @param depth.we vector of depth in m w.e
##' @param data some data vector
##' @param dZOut output resolution (m) 
##' @return list(depth,data,rho) snow depth and data / rho interpolated to the snow depth
##' @author Thomas Laepple
##' @examples
##'  temp<-DensityHL(rho.surface=340,t.mean=273.15-31.5,bdot=177,depth=0:150)
##'  depth.we<-Convert2We(temp$rho,temp$depth)
##'  #Add zero depth
##'  depth.we<-c(0,depth.we)
##'  density<-c(temp$rho[1],temp$rho)
##'  inSnow<-Convert2SnowDepth(rho=density,depth.we=depth.we,data=density)
##'  plot(temp$depth,temp$rho,type="l",xlab="snow depth",ylab="density")
##'  lines(inSnow$depth,inSnow$rho,col="red")
##' legend("topleft",col=c("black","red"),lwd=2,c("original","after forth and back conversion"))
##' 
##' @export

Convert2SnowDepth<-function(rho,depth.we,data,dZOut=0.01)
{
 ### Convert the data from water equivalents to snow-depth
    ## --------------------------------------------------------------------------------------------
    ## INPUT:
    ## rho - vector of density [kg/m^3]; 
    ## depth.we  - vector of depth in m.w.e, has to be equidistant in the moment
    ## data   - vector of scalar values in m.w.e
   
    ## --------------------------------------------------------------------------------------------
    ## Output:
    ## List of (depth, rho,data)
    ## --------------------------------------------------------------------------------------------   # Known bugs: depth has to start from 0

                                       
    if (diff(range(length(rho),length(depth.we),length(data)))>0) stop("All vectors have to have the same length")

    if (depth.we[1] != 0) stop("depth has to start at 0 in this preelimnary version")
    
    depth.we.midpoint<-(depth.we[-1]+depth.we[-length(depth.we)])/2
    rho.midpoint<-approx(depth.we,rho,depth.we.midpoint)$y/1000
    
    depth.water<-c(0,cumsum(diff(depth.we)/rho.midpoint))

    outDepth<-seq(from=0,to=max(depth.water),by=dZOut)
    data.out<-approx(depth.water,data,outDepth)$y
    rho.out<-approx(depth.water,rho,outDepth)$y

    return(list(depth=outDepth,data=data.out,rho=rho.out))

}
