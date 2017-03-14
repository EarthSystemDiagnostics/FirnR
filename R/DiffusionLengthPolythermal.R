
##' Calculate diffusion length accounting for seasonal firn temperatures.
##'
##' This function calculates the diffusion length in polar firn for the stable
##' water isotopes oxygen-18 and deuterium, depending on site-specific
##' parameters, accounting for the seasonal cycle of firn temperature.
##'
##' For \code{bParcel = TRUE}, 12 depth-dependent firn temperature realizations
##' are calculated as experienced by snow parcels starting monthly from January
##' to December; see \code{\link{ParcelTemperature}}. For \code{bParcel =
##' FALSE}, 12 depth-dependent firn temperatures are obtained from the 12
##' temperature profiles as observed in each month of the year; see
##' \code{\link{FirnTemperature}}. For both sets of realizations, diffusion
##' lengths are then calculated accounting for depth-dependent
##' temperatures. The function returns the average over the diffusion length
##' values for either of the 12 realizations of firn temperatures. This
##' implementation is motivated by the work of Simonsen et al. (2012).
##' @references
##' Simonsen, S. B., Johnsen, S. J., Popp, T. J., Vinther, B. M., Gkinis, V.,
##' and Steen-Larsen, H. C.: Past surface temperatures at the NorthGRIP drill
##' site from the difference in firn diffusion of water isotopes, Clim. Past,
##' 7(4), 1327–1335, 2011.
##' @param core List containing the site parameters; here the minimum of
##'     \code{A1}, \code{A2}, \code{phi1}, \code{phi2}, \code{T0},
##'     \code{rho.surface}, \code{bdot} and \code{P} are needed.
##' @param depth Numeric vector of firn depths [m] at which the diffusion
##'     lengths are calculated. Alternatively, this argument can be supplied via
##'     the \code{core} list.
##' @param rho Numeric vector of firn density [kg/m^3] of same length as
##'     \code{depth}. Alternatively, this argument can be supplied via the
##'     \code{core} list.
##' @param bParcel if \code{FALSE} (the default) calculate the mean diffusion
##'     length across the firn columns for every month; if \code{TRUE} calculate
##'     the mean diffusion length corresponding to snow parcels that start every
##'     month.
##' @param dD if \code{TRUE} the diffusion length for deuterium is returned,
##'     otherwise for oxygen-18. Defaults to \code{FALSE}.
##' @param bFill if \code{TRUE} (the default) use the last known density
##'     and related gradients for the value at the bottom of \code{depth} to
##'     calculate the final diffusion length; see
##'     \code{\link{DiffusionLength}}.
##' @return Numeric vector of the calculated diffusion lengths in [cm] at the
##'     depths given by \code{depth}.
##' @author Thomas Laepple
##' @seealso \code{\link{FirnTemperature}} and \code{\link{ParcelTemperature}}
##' for calculating depth-dependent firn temperatures;
##' \code{\link{DiffusionLength}} for calculating depth-dependent diffusion
##' lengths
##' @examples
##' ## Comparison of firn diffusion lengths at Kohnen depending on firn
##' ## temperature input
##'
##' core <- list(name = "Kohnen", rho.surface = 345, T0 = 273.15 - 44.5,
##'              bdot = 72, A1 = 13.2, A2 = 4.9, phi1 = -3.2, phi2 = 5.9,
##'              P = 677)
##' depth <- seq(from = 0, to = 20, by = 1 / 100)
##' rho <- DensityHL(depth = depth, core$rho.surface,
##'                  T = core$T0, bdot = core$bdot)$rho
##' 
##' sigma.d18O.uni <-     DiffusionLength(depth, rho, T = core$T0,
##'                                       bdot = core$bdot)
##' sigma.d18O.tparcel <- DiffusionLengthPolythermal(core, depth, rho,
##'                                                  bParcel = TRUE)
##' sigma.d18O.tfirn <-   DiffusionLengthPolythermal(core, depth, rho,
##'                                                  bParcel = FALSE)
##'
##' plot(sigma.d18O.uni, depth, type = "l", las = 1, lwd = 2,
##'      xlim = c(0, 10), ylim = rev(range(depth)),
##'      xlab = "diffusion length (cm)", ylab = "depth (m)",
##'      main = "d18O diffusion length at Kohnen")
##' lines(sigma.d18O.tparcel, depth, lwd = 2, col = "red")
##' lines(sigma.d18O.tfirn, depth, lwd = 2, col = "blue")
##' legend("bottomleft", c("mean firn temperature", "seasonal firn temperature",
##'                        "parcel temperature"),
##'        col = c("black", "blue", "red"), lwd = 2, bty = "n")
##' @export
DiffusionLengthPolythermal<-function(core = NULL, depth = core$depth,
                                     rho = core$rho, bParcel = FALSE,
                                     dD = FALSE, bFill = TRUE) {

    if (length(rho) != length(depth))
        stop("Conflicting INPUT: 'depth' and 'rho' have different lengths")
    
    save <- matrix(NA, 12, length(depth))
    for (i in 1 : 12) {
        
        if (bParcel) {
            T <- ParcelTemperature(i / 12, depth, core)
        } else {
            T <- FirnTemperature(i / 12, depth, core,
                                 kappa = KappaFirn(core$T0, rho = rho))
        }
        
        sigma <- DiffusionLength(depth = depth, rho = rho, T = T, P = core$P,
                                 bdot = core$bdot, dD = dD, bFill = bFill)
        save[i, ] <- sigma
    }
    
    return(colMeans(save))

}


