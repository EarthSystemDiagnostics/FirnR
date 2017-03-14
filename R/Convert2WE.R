
##' Conversion from snow depth to water equivalent depth.
##'
##' This function converts a given snow depth scale to water equivalent depths
##' according to given firn densities. The implementation is preliminary and
##' unfinished!
##'
##' Often the first density measurement is not directly at the surface. In this
##' case, the water equivalent depth is missing the top part. If bCorrectStart
##' is chosen, the mean density of the approx. one year from the exisiting
##' density is estimated and the depth is extrapolated to the surface.
##' @param rho Numeric vector of firn density [kg/m^3] corresponding to the
##' depths given by \code{depth}.
##' @param depth Numeric vector of snow depths in [m] of the same length as
##' \code{rho}. Important: What do these depths denote -- position of midpoints
##' or top/bottom points of layer?
##' @param bCorrectStart if \code{TRUE} correct for the starting depth. Defaults
##' to \code{FALSE}.
##' @param bdot  annual mean accumulation rate -> [kg/m2/year] or [m w.e./year]?
##' Only has to be supplied if \code{bCorrectStart = TRUE}.
##' @return Numeric vector of depths in [m w.e.].
##' @author Thomas Laepple
##' @examples
##' ## Convert Herron-Langway firn density from snow depth to water equivalent
##' ## depth scale and compare it to original solution on w.e. scale
##'
##' ## In this current implementation, the converted w.e. scale is shifted by
##' ## one bin compared to the 'true' HL scale! (see following example)
##' 
##' depth <- 0 : 150
##' hl <- DensityHL(depth = depth, rho.surface = 340, T = 273.15 - 31.5,
##'                 bdot = 177)
##' depth.we <- Convert2WE(rho = hl$rho, depth = depth)
##' 
##' plot(hl$depth.we, hl$rho, type = "l", las = 1,
##'      xlab = "depth (m w.e.)", ylab = "density (kg/m^3)")
##' lines(depth.we, hl$rho, col = "red")
##' legend("topleft", c("original", "after conversion"), lty = 1,
##'        col = c("black", "red"), bty = "n")
##' @export
Convert2WE<-function(rho, depth, bCorrectStart = FALSE, bdot) {

    warning.message <- paste("Not 100% checked. Normally start and end layers",
                             "should be provided. Unclear what should be done",
                             "at the top boundary")
    warning(warning.message)
    
    if (length(rho) != length(depth)) {
        stop("Conflicting INPUT: 'rho' and 'depth' must have the same length.")
    }

    # Density of water
    kRhoW <- 1000.
    
    depthdifference <- diff(depth)
    # We want the layer thickness at the middle of the layers
    temp <- c(depthdifference[1], depthdifference,
              depthdifference[length(depthdifference)])
    layerthickness <- 0.5 * (temp[-length(temp)] + temp[-1])
    # Set a first and last point
    layerwe  <- layerthickness * (rho / kRhoW)
    depth.we <- cumsum(layerwe)

    if (bCorrectStart) {
        
        index    <- which(depth.we < bdot)
        layer0   <- mean(na.omit(rho)[index]) * (depth[1] / kRhoW)
        depth.we <- depth.we + layer0
    }

    return(depth.we)
}
