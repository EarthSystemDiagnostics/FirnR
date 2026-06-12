#' Calculate the diffusion length in polar firn.
#'
#' This function calculates the diffusion length in polar firn for the stable
#' water isotopes oxygen-18 and deuterium, depending on site-specific
#' parameters.
#'
#' The calculation of the diffusion length in firn is an implementation of
#' Eq. (8) in Gkinis et al. (2014) and is partly inspired by the corresponding
#' implementation of the PRYSM model
#' (\url{https://github.com/sylvia-dee/PRYSM}) presented in Dee et al. (2015).
#'
#' As input, a depth and a density vector have to be provided. If only a single
#' density value is passed to the function, the function silently repeats this
#' density value to build a density vector that matches the length of
#' \code{depth} and calculates the diffusion length for zero strain rate (not
#' yet implemented!). For a single temperature value as input, one diffusivity
#' value from calling the \code{\link{Diffusivity}} function is used to
#' calculate the diffusion length. Providing a vector of temperatures (which
#' length has to match \code{depth}, otherwise the function exits with an
#' error), results in polythermal diffusivity calculation where for each set of
#' depth, density and temperature, the diffusivity is calculated. This
#' depth-dependent diffusivity is then used to calculate the diffusion
#' lengths.
#'
#' \code{bFill} controls the output of the final diffusion length value at the
#' bottom of \code{depth}. This value depends on the unknown density and
#' related gradients at this position. For \code{bFill = TRUE} (the default),
#' the last known gradients are used for the calculation of the final diffusion
#' length value. Otherwise \code{NA} is returned.
#' @references
#' Gkinis, V., Simonsen, S. B., Buchardt, S. L., White, J. W. C., and Vinther,
#' B. M.: Water isotope diffusion rates from the North-GRIP ice core for the
#' last 16,000 years – Glaciological and paleoclimatic implications, Earth
#' Planet. Sc. Lett., 405, 132–141, doi:10.1016/j.epsl.2014.08.022, 2014.
#'
#' Dee, S., Emile-Geay, J., Evans, M. N., Allam, A., Steig, E. J., and
#' Thompson, D. M.: PRYSM: An open-source framework for PRoxY System Modeling,
#' with applications to oxygen-isotope systems, J. Adv. Model. Earth Syst., 7,
#' 1220–1247, doi:10.1002/2015MS000447, 2015.
#' @param depth Numeric vector of firn depths [m] at which the diffusion
#'     lengths are calculated.
#' @param rho Numeric vector of firn density [kg/m^3], either of length one or
#'     of same length as \code{depth}.
#' @param T Numeric vector of firn temperature [K], either of length one or of
#'     same length as \code{depth}. Defaults to 10 m firn temperature at Kohnen
#'     Station.
#' @param P local surface pressure in [mbar]. Defaults to mean AWS9 value at
#'     Kohnen Station.
#' @param bdot local mass accumulation rate in [kg/m^2/year]. Defaults to
#'     long-time mean value at Kohnen Station.
#' @param dD if \code{TRUE} the diffusion length for deuterium is returned,
#'     otherwise for oxygen-18. Defaults to \code{FALSE}.
#' @param bFill if \code{TRUE} (the default) use the last known density
#'     and related gradients for the value at the bottom of \code{depth} to
#'     calculate the final diffusion length; see Details.
#' @return Numeric vector of the calculated diffusion lengths in [m] at the
#'     depths given by \code{depth}.
#' @author Thomas Muench, modified by Thomas Laepple
#' @seealso \code{\link{Diffusivity}}
#' @examples
#'
#' # Diffusion length for NGRIP site
#'
#' depth <- 0 : 150
#' t.mean <- 273.15 - 31.5
#' bdot <- 200
#' pressure <- 650
#'
#' rho <- DensityHL(depth = depth, rho.surface = 340,
#'                  T = t.mean, bdot = bdot)$rho
#'
#' sigma.d18O <- DiffusionLength(depth, rho, T = t.mean,
#'                               P = pressure, bdot = bdot)
#' sigma.dD <-   DiffusionLength(depth, rho, T = t.mean,
#'                               P = pressure,bdot = bdot,
#'                               dD = TRUE)
#'
#' # returned values are in SI units [m];
#' # for plotting it is more usual to display sigma in [cm]:
#' plot(1.e2 * sigma.d18O, depth, type = "l",
#'      xlim = c(0, 12), ylim = c(150, 0),
#'      xlab = "Diffusion length (cm)", ylab = "Depth (m)",
#'      main = "NorthGRIP, no thinning", lwd = 2, las = 1)
#' lines(1.e2 * sigma.dD, depth, lwd = 2, col = "red")
#' legend("topleft", c("d18O", "dD"),
#'        col = c("black", "red"), lwd = 2, bty = "n")
#' @export
DiffusionLength <- function(depth, rho, T = 273.15 - 44.5, P = 677, bdot = 64,
                            dD = FALSE, bFill = TRUE) {
    
    # Density of water
    kRhoW <- 1000.

    z <- depth

    # TODO (tmuench): implement zero-strain rate solution
    #if (length(rho) == 1) rho <- rep(rho, length(z))
    if (length(rho) != length(z))
        stop("Conflicting INPUT: 'depth' and 'rho' have different lengths")
    
    # Depth increments
    # CHECK (tlaepple): get dz in (m) from the z vector (in m) + extend with the
    # mean
    dz <- c(diff(z), mean(diff(z)))
    
    # Set time scale accounting for densification
    time_d <- cumsum(dz / (bdot / kRhoW) * (rho / kRhoW))
    # Convert from years to seconds
    ts <- time_d * 365.25 * 24 * 3600

    # Approximate density and related gradients
    drho <- diff(rho)
    dtdrho <- diff(ts) / diff(rho)

    # Fill unknown gradients at final depth
    ifelse(bFill,
           fill.gradient <- c(drho[length(drho)], dtdrho[length(dtdrho)]),
           fill.gradient <- rep(NA, 2))
    drho <- c(drho, fill.gradient[1])
    dtdrho <- c(dtdrho, fill.gradient[2])

    # Calculate diffusivity
    D <- vector(mode = "numeric", length = length(rho))
    if (length(T) == 1) {
        D <- Diffusivity(rho, T, P, dD = dD)
    } else {
        if (length(T) != length(rho))
            stop(paste("T and depth must have the same length",
                       "for polythermal diffusivity calculation"))
        for (i in 1 : length(rho))
            D[i] <- Diffusivity(rho[i], T[i], P, dD = dD)
    }
    
    # Integrate diffusivity along the density gradient
    # to obtain diffusion length in [m]
    sigma_sqrd_dummy <- 2 * (rho^2) * dtdrho * D
    sigma_sqrd <- cumsum(sigma_sqrd_dummy * drho)
    sigma <- sqrt(1 / (rho^2) * sigma_sqrd)

    # return (units [m])
    return(sigma)

}
