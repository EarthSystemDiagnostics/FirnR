
##' Diffuse a record.
##' 
##' This function diffuses a time series or record with a given depth-dependent
##' diffusion length by convolution with a Gaussian kernel.
##'
##' This function expects a numeric vector with the depth-dependent diffusion
##' lengths of the same length as \code{rec}. To calculate the simple case of a
##' constant diffusion length \code{sigma.const}, provide
##' \code{sigma = rep(sigma.const, length(rec))} as input.
##'
##' The input diffusion length is internally scaled according to the resolution
##' of the record given by \code{res}. The convolution integral is then
##' solved by a simple summation over the kernel width set to ~ 10 times the
##' current diffusion length. To avoid \code{NAs} at both ends of the diffused
##' version of \code{rec} resulting from the kernel extending beyond the record
##' ends, the kernel is clipped at the upper end to the range below the
##' surface. At the lower end, the record is extended by ~ 10 times the maximum
##' of \code{sigma} and filled with the mean average value of \code{rec}.
##' @param rec Numeric vector containing the record that is to be diffused.
##' @param sigma Numeric vector of the diffusion lengths corresponding to the
##' depths at which \code{rec} is tabulated. In units of the resolution of
##' \code{rec} (typically [cm]).
##' @param res Resolution of \code{rec} in the same units as \code{sigma}.
##' @return Numeric vector containing the diffused version of \code{rec}.
##' @author Thomas Muench, modified my Thomas Laepple
##' @examples
##' ## Diffuse white noise with a linearly increasing diffusion length
##' rec <- rnorm(n = 1000)
##' var.sigma <- seq(1, 8, length.out = 1000)
##' diffused <- DiffuseRecord(rec = rec, sigma = var.sigma)
##' plot(rec, type = 'l', las = 1, xlab = "depth in cm", ylab = "data", main = 
##'      "white noise diffusion with linearly increasing diffusion length")
##' lines(diffused, col = "red")
##' legend('topleft', c("original record", "diffused record"),
##'        lty = 1, col = 1 : 2, bty = "n")
##' @export
DiffuseRecord <- function(rec, sigma, res = 1){
    
    n <- length(rec)

    # record and sigma must have same length
    if (n != length(sigma))
        stop("Conflicting INPUT: rec and sigma must have same length.")

    # scale diffusion length according to resolution of record
    sigma <- sigma / res

    # pad end of record with mean of record to avoid NA's
    # at the end of diffused record 
    rec <- c(rec, rep(mean(rec, na.rm = TRUE), 10 * max(sigma)))

    # update record length
    nn <- length(rec)

    # vector to store diffused data
    rec.diffused <- rep(NA, nn)

    # loop over record
    for (i in 1 : nn){

        # diffusion length for current depth;
        # set to last existing value for the extended part of the record
        if (i > n){
            sig <- sigma[n]
        } else {
            sig <- sigma[i]
        }
   
        # set range of convolution integral (= 2*imax + 1) to ~ 10*sig
        imax <- ceiling(5 * sig)
        range <- (i - imax) : (i + imax)
        # skip part of range that extends above surface
        range <- range[range > 0]
        # relative range for convolution kernel
        rel.range <- i - range
        
        # convolution kernel
        kernel <- exp(-(rel.range)^2 / (2 * sig^2))
        kernel <- kernel / sum(kernel)
        
        # diffuse data at current depth bin
        rec.diffused[i] <- sum(rec[range] * kernel)
    
    }

    return(rec.diffused[1 : n])

}
