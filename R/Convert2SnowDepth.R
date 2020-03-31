##' Conversion from water equivalent depth to snow depth.
##'
##' This function converts data recorded on a water equivalent depth scale to
##' true snow depths based on given firn densities and linear interpolation.
##' The implementation is preliminary and unfinished!
##' @param rho Numeric vector of firn density [kg/m^3] corresponding to the
##' depths given by \code{depth}.
##' @param depth.we Numeric vector of the depths in [m w.e] at which the data in
##' \code{data} are given.
##' @param data Numeric vector providing the data to be converted.
##' @param dZOut Output resolution of the snow depth scale in [m]. 
##' @return A list with three elements:
##'     \itemize{
##'     \item \code{depth}: Numeric vector of snow depths in [m] onto which
##'     \code{rho} and \code{depth} have been interpolated.
##'     \item \code{data}: Input data interpolated to snow depths.
##'     \item \code{rho}: Input density interpolated to snow depths.}
##' @author Thomas Laepple
##' @examples
##' ## Convert Herron-Langway firn density from water equivalent to snow depth
##' ## scale and compare it to original solution on snow depth scale
##' 
##' depth <- 0 : 150
##' hl <- DensityHL(depth = depth, rho.surface = 340, T = 273.15 - 31.5,
##'                 bdot = 177)
##' in.snow <- Convert2SnowDepth(rho = hl$rho, depth.we = hl$depth.we,
##'                              data = hl$rho, dZOut = 1)
##' 
##' plot(depth, hl$rho, type = "l", las = 1,
##'      xlab = "snow depth (m)", ylab = "density (kg/m^3)")
##' lines(in.snow$depth, in.snow$rho, col = "red")
##' legend("topleft", c("original", "after conversion"), lty = 1,
##'        col = c("black", "red"), bty = "n")
##' @export
Convert2SnowDepth <- function(rho, depth.we, data, dZOut = 0.01) {

    # Known bugs: depth has to start from 0
    
    if (diff(range(length(rho), length(depth.we), length(data))) > 0) {
        stop("Conflicting INPUT: All vectors must have the same length.")
    }
    
    if (depth.we[1] != 0) stop("depth has to start at 0 in this preelimnary version")

    # Density of water
    kRhoW <- 1000.

    # Get midpoint values of given water eq. depth scale
    depth.we.midpoint <- 0.5 * (depth.we[-length(depth.we)] + depth.we[-1])
    # Interpolate density to midpoint values
    rho.midpoint <- stats::approx(depth.we, rho, depth.we.midpoint)$y

    # Snow depth scale corresponding to given water eq. depth scale
    depth.snow <- c(0, cumsum(diff(depth.we) / rho.midpoint)) * kRhoW

    # Interpolate onto equidistant snow depth scale
    outDepth <- seq(from = 0, to = max(depth.snow), by = dZOut)
    data.out <- stats::approx(depth.snow, data, outDepth)$y
    rho.out  <- stats::approx(depth.snow, rho, outDepth)$y

    return(list(depth = outDepth, data = data.out, rho = rho.out))

}
