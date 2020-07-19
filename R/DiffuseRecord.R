
##' Diffuse a record.
##' 
##' This function diffuses a time series or record with a given depth-dependent
##' diffusion length by convolution with a Gaussian kernel.
##'
##' This function expects a numeric vector with the depth-dependent diffusion
##' lengths of the same length as \code{rec}, or a single value to use a
##' constant diffusion length.
##'
##' The input diffusion length is internally scaled according to the resolution
##' of the record given by \code{res}. The convolution integral is then
##' solved by a simple summation over the kernel width set to ~ 10 times the
##' current diffusion length.
##'
##' For \code{debug = FALSE}: To avoid \code{NAs} at both ends of the diffused
##' version of \code{rec} resulting from the kernel extending beyond the record
##' ends, the kernel is clipped at the upper end to the range below the
##' surface. At the lower end, the record is extended by ~ 10 times the maximum
##' of \code{sigma} and filled with the mean average value of \code{rec}.
##' @param rec Numeric vector containing the record that is to be diffused.
##' @param sigma Numeric vector of the diffusion lengths corresponding to the
##' depths at which \code{rec} is tabulated, or of length one to diffuse
##' \code{rec} with a constant diffusion length. Must be in units of the
##' resolution of \code{rec} (typically [cm]).
##' @param res Resolution of \code{rec} in the same units as \code{sigma}.
##' @param debug if \code{TRUE} the values at top and bottom of the diffused
##' record which are potentially affected by the finite record length are set to
##' \code{NA}. Defaults to \code{FALSE}. See also Details.
##' @return Numeric vector containing the diffused version of \code{rec}.
##' @author Thomas Muench, modified by Thomas Laepple
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
DiffuseRecord <- function(rec, sigma, res = 1, debug = FALSE){

    if (missing(sigma)) {
      stop("No diffusion length passed as input.", call. = FALSE)
    }
    if (any(!is.finite(sigma))) {
      stop("Missing values passed as diffusion length.", call. = FALSE)
    }

    ns <- length(sigma)
    n <- length(rec)

    if (ns != 1 & ns != n) {
      stop("Diffusion length neither of length 1 nor matches length of record.",
           call. = FALSE)
    }

    # recycle sigma if needed
    if (ns == 1) {
      sigma <- rep(sigma, n)
    }

    # scale diffusion length according to resolution of record
    sigma <- sigma / res

    # pad end of record with mean of record to avoid NA's
    # at the end of diffused record
    if (!debug) rec <- c(rec, rep(mean(rec, na.rm = TRUE), 10 * max(sigma)))

    # vector to store diffused data
    rec.diffused <- rep(NA, n)

    # loop over record
    for (i in 1 : n) {

        # diffusion length for current depth
        sig <- sigma[i]

        if (sig == 0) {

            diff.value <- rec[i]

        } else {

            # set range of convolution integral (= 2*imax + 1) to ~ 10*sig
            imax <- ceiling(5 * sig)
            ran <- (i - imax) : (i + imax)

            # if part of range extends above surface, set diffused value to 'NA'
            # for 'debug = TRUE', else skip that part of range in the
            # convolution integral

            if (!all(ran > 0) & debug) {
                diff.value <- NA
            } else {
                ran <- ran[ran > 0]
                # relative range for convolution kernel
                rel.ran <- i - ran
        
                # convolution kernel
                kernel <- exp(-(rel.ran)^2 / (2 * sig^2))
                kernel <- kernel / sum(kernel)

                # diffuse data at current depth bin
                diff.value <- sum(rec[ran] * kernel)
            }
          }

        rec.diffused[i] <- diff.value
    }

    return(rec.diffused)

}
