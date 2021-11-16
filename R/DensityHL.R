
##' Analytical solution of Herron-Langway firn densification model.
##'
##' This function calculates firn density depending on depth based on the
##' analytical solution of the steady-state Herron-Langway densification model.
##'
##' The empirical, steady-state Herron-Langway [HL] model of firn densification
##' together with its analytical solution is described in Herron and Langway
##' (1980). Its implementation here is based on the MATLAB code by Aslak
##' Grinsted (2014) which follows the nomenclature by Arthern et al. (2010). For
##' details of expressing the analytical solution, see also van der Wel (2012).
##'
##' The HL model is matched to density observations from Greenlandic and
##' Antarctic firn cores. The correction factors of Johnsen et al. (2000) (set
##' for \code{JohnsenCorr = TRUE}) have been introduced to further improve the
##' match with central Greenland firn core data.
##' @references
##' Herron, M. M. and Langway Jr., C. C.: Firn densification: an empirical
##' model, J. Glaciol., 25(93), 373-385, 1980.
##'
##' Grinsted, A.: Steady state snow and firn density model,
##' \url{https://www.mathworks.com/matlabcentral/fileexchange/47386-steady-state-snow-and-firn-density-model/content//densitymodel.m}, 2014. 
##'
##' Arthern, J. A., Vaughan, D. G., Rankin, A. M., Mulvaney, R., and Thomas,
##' E. R.: In situ measurements of Antarctic snow compaction compared with
##' predictions of models, J. Geophys. Res., 115(F03011), 2010.
##'
##' van der Wel, L. G.: Analyses of water isotope diffusion in firn:
##' contributions to a better palaoclimatic interpretation of ice cores, Doctor
##' of Philosophy, University of Groningen,
##' \url{http://www.rug.nl/research/portal/files/2408704/Thesis.pdf}, 2012.
##' 
##' Johnsen, S. J., Clausen, H. B., Cuffey, K. M., Hoffmann, G., Schwander, J.,
##' and Creyts, T.: Diffusion of stable isotopes in polar firn and ice: the
##' isotope effect in firn diffusion, in: Physics of ice core records, edited
##' by: Hondoh, T., vol. 159, Hokkaido Univ. Press, Sapporo, Japan, 121–140,
##' 2000.
##' @param depth Numeric vector of firn depths [m] at which firn density is
##'     calculated.
##' @param rho.surface surface density in [kg/m^3].
##' @param T mean firn temperature in [K].
##' @param bdot local mass accumulation rate in [kg/m^2/year].
##' @param JohnsenCorr logical; whether or not to apply the Johnsen correction
##'   to the Arrhenius rate constants for central Greenland sites.
##' @return A list with two elements:
##'     \itemize{
##'     \item \code{depth.we}: Numeric vector of water-equivalent depth in [m]
##'     corresponding to the true firn depths given by \code{depth}.
##'     \item \code{rho}: Numeric vector (length of \code{depth}) of firn
##'     density in [kg/m^3].}
##' @author Thomas Laepple, modified by Thomas Muench
##' @examples
##' ## Firn density for NGRIP site
##' depth <- 0 : 150  
##' t.mean <- 273.15 - 31.5
##' bdot <- 200
##' rho.s <- 360
##' result1 <- DensityHL(depth = depth, rho.surface = rho.s,
##'                      T = t.mean, bdot = bdot)
##' result2 <- DensityHL(depth = depth, rho.surface = rho.s,
##'                      T = t.mean, bdot = bdot, JohnsenCorr = TRUE)
##' 
##' ## Plot against true depth
##' plot(result1$rho, depth, type = "l", las = 1, lwd = 2,
##'      xlab = "firn density (kg/m^3)", ylab = "depth (m)",
##'      main = "NGRIP simulated density",
##'      ylim = c(150, 0), xlim = c(200, 1000))
##' lines(result2$rho, depth, lwd = 1.5, lty = 2)
##' legend("bottomleft", c("HL steady state",
##'                     "HL steady state with Johnsen (2000) correction"),
##'        col = 1, lwd = c(2, 1.5), lty = c(1, 2), bty = "n")
##' 
##' ## Plot against water-equivalent depth
##' plot(result1$rho, result1$depth.we, type = "l", las = 1, lwd = 2,
##'      xlab = "firn density (kg/m^3)", ylab = "water-equivalent depth (m)",
##'      main = "NGRIP simulated density",
##'      ylim = c(125, 0), xlim = c(200, 1000))
##' lines(result2$rho, result2$depth.we, lwd = 1.5, lty = 2)
##' legend("bottomleft", c("HL steady state",
##'                     "HL steady state with Johnsen (2000) correction"),
##'        col = 1, lwd = c(2, 1.5), lty = c(1, 2), bty = "n")
##' @export
DensityHL <- function(depth = (0 : 9000) / 100, rho.surface, T, bdot,
                      JohnsenCorr = FALSE) {

    # Constants
    kRhoIce <- 920.      # Density of ice [kg/m^3]
    kRhoW <- 1000.       # Density of water [kg/m^3]
    kR <- 8.314478       # Gas constant [J/(K * mol)]

    # Critical density of point between settling and creep-dominated stages
    kRhoC <- 550

    # Herron-Langway Arrhenius rate constants
    k0 <- 11 * exp(-10160 / (kR * T))
    k1 <- 575 * exp(-21400 / (kR * T))

    # Johnsen et al. (2000) correction for central Greenland cores
    if (JohnsenCorr) {
        k0 <- 0.85 * k0
        k1 <- 1.15 * k1
    }

    # Rate constants for time-dependent densification
    # (original Eq. (4) in Herron and Langway et al. (1980))
    A <- bdot / kRhoW
    c0 <- k0 * A
    c1 <- k1 * sqrt(A)

    # Rate constants for depth-dependent steady-state densification
    # (from converting the full time derivative to a depth derivative
    # neglecting the partial time derivative to get steady-state solution)
    d0 <- c0 / bdot
    d1 <- c1 / bdot

    fac.r0 <- rho.surface / (kRhoIce - rho.surface)
    fac.rc <- kRhoC / (kRhoIce - kRhoC)
    
    # Critical depth at which density reaches kRhoC
    z.c <- log(fac.rc / fac.r0) / (kRhoIce * d0)

    # Steady-state density profile
    q.upper <- fac.r0 * exp(d0 * kRhoIce * depth[depth <= z.c])
    q.lower <- fac.rc * exp(d1 * kRhoIce * (depth[depth > z.c] - z.c))
    rho.upper <- kRhoIce * (q.upper / (1 + q.upper))
    rho.lower <- kRhoIce * (q.lower / (1 + q.lower))

    # Time when critical depth is reached
    tmp <- (kRhoIce - rho.surface) / (kRhoIce - kRhoC)
    t.c <- log(tmp) / c0
    
    # Steady-state time - water-equivalent depth relation
    tmp <- (kRhoIce - rho.surface) / (kRhoIce - rho.upper)
    t.upper <- log(tmp) / c0
    tmp <- (kRhoIce - kRhoC) / (kRhoIce - rho.lower)
    t.lower <- log(tmp) / c1 + t.c

    rho <- c(rho.upper, rho.lower)
    depth.we <- A * c(t.upper, t.lower)

    return(list(depth.we = depth.we, rho = rho))

}
