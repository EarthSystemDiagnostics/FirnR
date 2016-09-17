

##' Convert snow depth to water equivalent depth by summing up the layers, unfinished
##'
##' often the first density measurement is not directly at the surface. In this case
##' the water equivalent depth is missing the top part, if bCorrectStart is choosen, the mean density of the approx. one year
##' from the exisiting density is estimated and the depth is extrapolated to the surface
##' @title Convert snow depth to water equivalent depth
##' @param rho vector of density in [kg/m^3]; 
##' @param depth snow depth in m, vector of same length as rho
##' @param bCorrectStart if TRUE, correct for the starting depth using the 
##' @param accum  annual mean accumulation rate kg/m2/year, only has to be supplied if bCorrectStart=TRUE
##' @return vector of depth in m w.e.
##' @author Thomas Laepple
##' @examples
##'  temp<-DensityHL(rho.surface=340,t.mean=273.15-31.5,bdot=177,depth=0:150)
##'  Convert2WE(temp$rho,temp$depth)
##' @export
Convert2WE<-function(rho,depth,bCorrectStart=FALSE,accum)
{
    warning("not 100% checked, normally start and end layers should be provided, unclear what should be done at the top boundary")
    if (length(rho) != length(depth)) stop("rho and depth vector have different lengths")
    depthdifference<-diff(depth)
                                        #We want the layer thickness at the middle of the layers
    temp<-c(depthdifference[1],depthdifference,depthdifference[length(depthdifference)])
    layerthickness<-(temp[-1]+temp[-length(temp)])/2
                                        #set a first and last point
    layerwe<-layerthickness*rho/1000
    depth.we<-cumsum(layerwe)

    if (bCorrectStart)
        {
            index<-which(depth.we<accum)
            layer0 = mean(na.omit(rho)[index])*depth[1]/1000
            depth.we<-depth.we+layer0
        }
    return(depth.we)
}
