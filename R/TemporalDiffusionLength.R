#' Calculate the diffusion length in years
#'
#' This function calculates the diffusion length in firn in time units (years)
#' based on calculating the diffusion length in depth units
#' (see \code{\link{DiffusionLength}}) and converting it from depth units in
#' temporal units using the firn density from the Herron-Langway model
#' (see \code{\link{DensityHL}}).
#'
#' The diffusion length can be calculated for several sites with varying
#' climatic input parameters \code{T}, \code{P}, \code{bdot} and
#' \code{rho.surface}. Note that for this all input parameter vectors must
#' either have the same length. Else, length-one vectors are recycled to match
#' the length of the longest input; if this still results in varying vector
#' lengths, an error is issued.
#'
#' @param core.length the simulated core length in metre for calculating the
#'   Herron-Langway firn density and the diffusion length. If this length is
#'   not sufficient to cover the requested time span given by \code{nt} and
#'   \code{t.res}, diffusion length values for the remaining time points are
#'   filled with the last properly obtained value. This issues only a warning
#'   since if the simulated core is long enough to reach the ice, the diffusion
#'   length is anyway constant, but it is a problem if the simulated core is
#'   far too shallow.
#' @param z.res the resolution in metre of the simulated firn core.
#' @param nt the number of time points for the output temporal diffusion
#'   length.
#' @param t.res the temporal resolution for the diffusion length in time units
#'   [yr]; i.e. the total time span covered is \code{t.res * nt}.
#' @param T local annual mean surface temperature (10-m firn temperature) in
#'   [K].
#' @param P local surface pressure in [mbar].
#' @param bdot local mass accumulation rate in [kg/m^2/yr].
#' @param rho.surface local surface firn density in [kg/m^3].
#' @param dD logical; if \code{TRUE} the diffusion length for deuterium is
#'   returned, otherwise for oxygen-18. Defaults to \code{FALSE}.
#' @param JohnsenCorr logical; whether or not to apply the Johnsen correction
#'   to the Arrhenius rate constants in the Herron-Langway model for central
#'   Greenland sites.
#' @param names optional character vector of site names.
#' @return the temporal diffusion lengths in a data frame of \code{nt} rows and
#'   a minimum of two columns, where the first column is the time axis and the
#'   second or other columns are the diffusion lengths for the requested
#'   site(s).
#' @seealso \code{\link{DiffusionLength}}, \code{\link{DensityHL}}
#' @examples
#' # do the calculation for the default parameters at Kohnen Station:
#' sigma.years <- TemporalDiffusionLength()
#'
#' # mimicing two sites by adjusting one parameter to have length 2
#' # (all other parameters are then recycled to length 2):
#' sigma.years <- TemporalDiffusionLength(T = c(-30, -50) + 273.15,
#'                                        names = c("Site A", "Site B"))
#' @author Thomas Münch
#' @export
TemporalDiffusionLength <- function(core.length = 1000, z.res = 0.01,
                                    nt = 2000, t.res = 1,
                                    T = 273.15 - 44.5, P = 677,
                                    bdot = 64, rho.surface = 340,
                                    dD = FALSE, JohnsenCorr = FALSE,
                                    names = NULL) {

  nn <- c(length(T), length(P), length(bdot), length(rho.surface))
  if (stats::sd(nn) > 0) {

    if (nn[1] == 1) T <- rep(T, max(nn))
    if (nn[2] == 1) P <- rep(P, max(nn))
    if (nn[3] == 1) bdot <- rep(bdot, max(nn))
    if (nn[4] == 1) rho.surface <- rep(rho.surface, max(nn))

    nn <- c(length(T), length(P), length(bdot), length(rho.surface))

    if (stats::sd(nn) > 0) {
      stop("Inconsistent input length of 'T', 'P', 'bdot', 'rho.surface'.",
           call. = FALSE)
    }
  }

  n <- nn[1]

  # Density of water
  kRhoW <- 1000.

  # Depth profile
  depth <- seq(z.res, core.length, z.res)

  # Equidistant age scale
  t.equi <- seq_len(nt) * t.res

  # Loop over individual core sites
  sigma <- sapply(1 : n, function(i) {

    # Herron-Langway density profile for site 'i'
    HL  <- DensityHL(depth = depth,
                     rho.surface = rho.surface[i], T = T[i], bdot = bdot[i],
                     JohnsenCorr = JohnsenCorr)

    # Age of core according to Herron-Langway solution and const. acc. rate
    t <- HL$depth.we * kRhoW / bdot[i]

    # Check simulated vs. requested age
    if (max(t) < nt * t.res) {
      warning("Max. of simulated age too small; increase core length.",
              call. = FALSE)
    }

    # Diffusion length in [cm] for site 'i' as a function of depth
    sig.m <- DiffusionLength(depth = depth, rho = HL$rho, T = T[i],
                             P = P[i], bdot = bdot[i], dD = dD)

    # convert diffusion length from [m] to [yr]
    sig.yr <- sig.m * (HL$rho / bdot[i])
    
    # diffusion length in [yr] on equidistant time grid
    sigma <- stats::approx(t / t.res, sig.yr, seq_len(nt), rule = 2)$y

    return(sigma)

  })

  res <- data.frame(t.equi, sigma)

  colnames(res) <- c("Time", names)

  return(res)
    
}
