##' Bimodal harmonic model.
##'
##' This function calculates a harmonic model with two modes for one annual
##' cycle at daily resolution given mean, amplitudes and phases.
##'
##' The model is implemented with cosine functions; thus, zero phase shift
##' corresponds to the cycle maximum (January 1st).
##' @param x Numeric vector with five components:
##' \enumerate{
##' \item Mean value over one annual cycle [a.u.].
##' \item Amplitude (half peak-peak) of first mode (same unit as mean).
##' \item Amplitude of second mode (same unit as mean).
##' \item Phase shift of first mode in [degree].
##' \item Phase shift of second mode in [degree].}
##' @return Numeric vector with 365 elements corresponding to one annual cycle of
##'     the model.
##' @author Thomas Laepple
##' @examples
##' ## Different harmonic models
##' days <- 1 : 365
##' x1 <- c(-45, 10, 0, 0, 0)
##' x2 <- c(-45, 10, 5, 10, 50)
##' 
##' plot(days, HarmonicModel(x1), type = "l", las = 1, ylim = c(-60, -30),
##'      xlab = "day of year", ylab = "annual cycle (a.u.)",
##'      main = "Different harmonic models")
##' lines(days, HarmonicModel(x2), col = 4)
##' 
##' legend("topleft",
##'        c("simple sinusoid", "bi-modal with differing amplitude and phase"),
##'        col = c(1, 4), lty = 1, bty = "n")
##' @export
HarmonicModel <- function(x) {

    if (length(x) != 5) stop("Invalid length of input vector.")

    A0 <- x[1]
    A1 <- x[2]
    A2 <- x[3]
    
    # convert input phases from degree to radian
    deg2rad <- pi / 180.
    phi1 <- deg2rad * x[4]
    phi2 <- deg2rad * x[5]
    
    days <- 1 : 365
    n <- length(days)
    omega <- 2 * pi / n

    h1 <- A1 * cos(omega * days + phi1)
    h2 <- A2 * cos(2 * omega * days + phi2)

    h <- A0 + h1 + h2
    
    return(h)

}

