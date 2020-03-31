##' Fit harmonic model with two modes to a daily climatology.
##'
##' This function fits a two-mode harmonic model to given daily data based on
##' a standard R optimization routine.
##'
##' For optimization, the base R function \code{\link[stats]{optim}} is used
##' applying the \code{"L-BFGS-B"} method. Per default, the optimization
##' procedure is initialised with a standard sinusoid, thus with \code{mean =
##' mean(data)}, amplitude of first  mode equal to half peak-peak (\code{0.5 *
##' diff(range(data))}), and all other parameters zero. Alternatively, initial
##' parameters can be specified directly. Results can be monitored on the fly by
##' setting \code{bPlot = TRUE}. The optimization routine may result in negative
##' amplitudes for the harmonic modes which are corrected afterwards. Optimal
##' phase shifts are constrained to the interval [-180, 180] degree.
##' @param data Numeric vector of the climatology data to which the model
##' shall be fitted. Must contain 365 values.
##' @param initial.par Optional numeric vector with five elements giving initial
##' parameters to initialize the optimization (for format see return value of
##' function). Defaults to \code{NULL} which forces sinusoidal initial
##' parameters (see Details).
##' @param bPlot if \code{TRUE} a plot showing the original data together with
##' the harmonic models according to the initial and optimized parameters is
##' shown. The optimal parameters and the minimum data-model misfit are
##' displayed along the graph. Defaults to \code{FALSE}.
##' @return Numeric vector with five named elements giving the optimized model
##' parameters:
##' \describe{
##' \item{T0:}{Mean value over one annual cycle (units of \code{data}).}
##' \item{A1:}{Amplitude of first mode (units of \code{data}).}
##' \item{A2:}{Amplitude of second mode (units of \code{data}).}
##' \item{phi1:}{Phase shift of first mode in [degree].}
##' \item{phi2:}{Phase shift of second mode in [degree].}}
##' @author Thomas Laepple
##' @seealso \code{\link[stats]{optim}}, \code{\link{HarmonicModel}}
##' @examples
##' ## Optimize standard sinusoid to fit an arbitrary two-mode harmonic
##' days <- 1 : 365
##' x <- c(-45, 10, 5, 10, 50)
##' data <- HarmonicModel(x)
##' optim <- FitHarmonicModel(data, bPlot = TRUE)
##' @export
FitHarmonicModel <- function(data, initial.par = NULL, bPlot = FALSE) {

    if (length(data) != 365)
        stop("Invalid data: input vector must contain 365 values.")
    
    if (is.null(initial.par)) {
        initial.par <- c(mean(data), diff(range(data)) / 2, 0, 0, 0)
    } else {
        if (length(initial.par) != 5) {
            stop("Invalid length of user-supplied initial parameters.")
        }
    }

    rmse <- function(x, y) return(sqrt(mean((x - y)^2)))
    fn <- function(x, data) return(rmse(data, HarmonicModel(x)))

    fit <- stats::optim(par = initial.par, fn = fn,
                 method = "L-BFGS-B", data = data)

    optim.par <- fit$par
    names(optim.par) <- c("T0", "A1", "A2", "phi1", "phi2")

    # perform phase-shift of 180 degrees if fitted amplitude is < 0
    ampl <- optim.par[c(2, 3)]
    phi  <- optim.par[c(4, 5)]
    if (length(i <- which(ampl < 0)) != 0) {
        ampl[i] <- -1 * ampl[i]
        phi[i]  <- phi[i] + 180
    }

    # constrain phase shifts to interval [-180, 180] degree
    for (i in 1 : 2) {
        if (phi[i] < 0) {
            while (phi[i] < -180) phi[i] <- phi[i] + 360
        } else {
            while (phi[i] >  180) phi[i] <- phi[i] - 360
        }
    }

    optim.par[c(2, 3)] <- ampl
    optim.par[c(4, 5)] <- phi
    
    if (bPlot) {

        graphics::plot(data, type = "l", las = 1, lwd = 2.5,
             xlab = "day of year", ylab = "data (a.u.)",
             main = paste("optimal parameters (T0, A1, A2, phi1, phi2):\n",
                          toString(sprintf("%2.2f", optim.par))))
        graphics::lines(HarmonicModel(initial.par), lty = 2)
        graphics::lines(HarmonicModel(optim.par), col = "blue")
        graphics::legend("bottomleft",
               c("data", "initial fit", "optimized fit"),
               lty = c(1, 2.5, 1), lwd = c(2, 1, 1), col = c(1, 1, 4),
               bty = "n")
        graphics::legend("bottomright",
                         sprintf("minimum misfit: %1.2g", fit$value),
                         lty = NULL, bty = "n")
    }

    return(optim.par)
}





test <- 200
while (test > 180) {test <- test-5}






