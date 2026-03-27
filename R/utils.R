#
# unexported utility functions functions
#

# ------------------------------------------------------------------------------
# firn thermal properties
# ------------------------------------------------------------------------------

#' Specific heat capacity of ice
#'
#' Calculate the specific heat capacity of ice at constant pressure depending on
#' ambient temperature, based on the formula given in Goujon et al. (2003).
#'
#' @param T numeric vector of ice temperature in [K].
#' @return numeric vector of the specific heat capacity at temperature \code{T}
#'   in [J/(kg*K)].
#'
#' @references
#' Goujon, C., Barnola, J.-M., and Ritz, C.: Modeling the densification of
#'   polar firn including heat diffusion: Application to close-off
#'   characteristics and gas isotopic fractionation for Antarctica and Greenland
#'   sites, J. Geophys. Res., 108(D24), 4792,
#'   https://doi.org/10.1029/2002JD003319, 2003.
#'
#' @author Thomas Laepple
#'
CIce <- function(T) {

  return(152.5 + 7.122 * T)

}

#' Thermal conductivity of ice
#'
#' Calculate the thermal conductivity of ice depending on ambient temperature,
#' based on the formula given in Goujon et al. (2003).
#'
#' @param T numeric vector of ice temperature in [K].
#' @return numeric vector of thermal conductivity of ice at temperature \code{T}
#'   in [W/(m*K)].
#'
#' @references
#' Goujon, C., Barnola, J.-M., and Ritz, C.: Modeling the densification of
#'   polar firn including heat diffusion: Application to close-off
#'   characteristics and gas isotopic fractionation for Antarctica and Greenland
#'   sites, J. Geophys. Res., 108(D24), 4792,
#'   https://doi.org/10.1029/2002JD003319, 2003.
#'
#' @author Thomas Laepple
#'
KIce <- function(T) {

  return(2.22 * (1 + 0.0067 * (273.15 - T)))

}

#' Thermal conductivity of firn
#'
#' Calculate the thermal conductivity of firn based on the thermal conductivity
#' of ice and the firn density, as given by the formula in Goujon et
#' al. (2003).
#'
#' @param T numeric vector of ambient ice temperature in [K].
#' @param rho numeric vector of firn density in [kg/m^3]. If both \code{T} and
#'   \code{rho} have length > 1, the lengths must be the same.
#' @return numeric vector of the thermal conductivity of firn in [W/(m*K)].
#'
#' @references
#' Goujon, C., Barnola, J.-M., and Ritz, C.: Modeling the densification of
#'   polar firn including heat diffusion: Application to close-off
#'   characteristics and gas isotopic fractionation for Antarctica and Greenland
#'   sites, J. Geophys. Res., 108(D24), 4792,
#'   https://doi.org/10.1029/2002JD003319, 2003.
#'
#' @author Thomas Laepple
#' @seealso \code{\link{KIce}}
#'
KFirn <- function(T, rho) {

  # check input vector lengths
  if ((length(T) != 1 & length(rho) != 1) & (length(T) != length(rho)))
    stop("Lengths of 'T' and 'rho' must be equal if both are > 1.")

  kRhoIce <- 920. # ice density

  r <- rho / kRhoIce
  a <- 2 - 0.5 * r

  return(r^a * KIce(T))

}

#' Thermal diffusivity of ice
#'
#' Calculate the thermal diffusivity of ice from its thermal conductivity,
#' density and heat capacity at constant pressure.
#'
#' @param T numeric vector of ice temperature in [K].
#' @return numeric vector of thermal diffusivity at temperature \code{T} in
#'   [m^2/s].
#'
#' @references https://en.wikipedia.org/wiki/Thermal_diffusivity
#'
#' @author Thomas Laepple
#' @seealso \code{\link{KIce}}, \code{\link{CIce}}
#'
KappaIce <- function(T) {

  kRhoIce <- 920.  # ice density

  return(KIce(T) / (kRhoIce * CIce(T)))

}

#' Thermal diffusivity of firn
#'
#' Calculate the thermal diffusivity of firn based on its thermal
#' conductivity and density and on the heat capacity of ice at constant
#' pressure.
#'
#' @param T numeric vector of firn temperature [K].
#' @param rho numeric vector of firn density [kg/m^3]. If both \code{T} and
#'   \code{rho} have length > 1, the lengths must be the same.
#' @return numeric vector of thermal diffusivity of firn in [m^2/s].
#'
#' @references https://en.wikipedia.org/wiki/Thermal_diffusivity
#'
#' @author Thomas Laepple
#' @seealso \code{\link{KappaIce}}, \code{\link{KFirn}}, \code{\link{CIce}}
#' @examples
#'
#' # firn becomes ice at a density of 920 kg/m^3, therefore
#' all.equal(FirnR:::KappaFirn(273.15 - 45, 920.),
#'           FirnR:::KappaIce(273.15 - 45))
#' # is TRUE
#'
KappaFirn<-function(T, rho) {

  # 'T' and 'rho' must have same length if *both* are not vectors of length 1
  if ((length(T) != 1 & length(rho) != 1) & (length(T) != length(rho)))
    stop("Lengths of 'T' and 'rho' must be equal if both are > 1.")

  return(KFirn(T, rho) / (rho * CIce(T)))
  
}

# ------------------------------------------------------------------------------
# firn diffusion parameters
# ------------------------------------------------------------------------------

#' Saturation vapour pressure over ice
#'
#' Calculate the saturation vapour pressure over ice, based on the
#' parameterization given in van der Wel et al. (2015) (Eq. 5).
#'
#' @param T numeric vector of ambient temperature in [K].
#' @return numeric vecor of the saturation vapour pressure in [Pa].
#'
#' @references
#' van der Wel, L. G., H. A. Been, R. S. W. van de Wal, C. J. P. P. Smeets and
#'   H. A. J. Meijer: Constraints on the d2H diffusion rate in firn from field
#'   measurements at Summit, Greenland. The Cryosphere, 9, 1089–1103,
#'   https://doi.org/10.5194/tc-9-1089-2015, 2015.
#'
#' @author Thomas Münch
#'
pSat <- function(T) {

  exp(9.5504 + 3.53 * log(T) - 5723.265 / T - 0.0073 * T)

}

#' Isotopic fractionation factors
#'
#' Calculate the fractionation factors of oxygen-18 and deuterium isotopes,
#' i.e. the difference in ratio of rare to abundant isotopes, in water vapour
#' over ice under equilibrium conditions, based on the expressions given in
#' Johnsen et al. (2000).
#'
#' @param T numeric vector of firn temperature in [K].
#' @param dD logical; of \code{TRUE} return the fractionation factor for
#'   deuterium, else for oxygen-18 (the default).
#' @return numeric vector of the equilibrium fractionation factor for oxygen-18
#'   or deuterium isotopes.
#'
#' @references
#' Johnsen, S. J., Clausen, H. B., Cuffey, K. M., Hoffmann, G., Schwander, J.,
#'   and Creyts, T.: Diffusion of stable isotopes in polar firn and ice: the
#'   isotope effect in firn diffusion, in: Physics of ice core records, edited
#'   by: Hondoh, T., vol. 159, Hokkaido Univ. Press, Sapporo, Japan, 121–140,
#'   2000.
#'
#' @author Thomas Münch
#'
alphaIso <- function(T, dD = FALSE) {

  if (dD) {
    alpha <- exp(16288 / (T^2) - 9.45e-2)
  } else {
    alpha <- exp(11.839 / T - 28.224e-3)
  }

  return(alpha)

}

#' Water vapour diffusivity in air
#'
#' Calculate the water vapour diffusivity in air depending on ambient
#' temperature and pressure for the majour isotopologue species, based on the
#' formulae given in Johnsen et al. (2000) with the numerical isotopologue
#' factors as in Merlivat and Jouzel (1979).
#'
#' @param T numeric vector of ambient air temperature in [K].
#' @param P numeric vector of local surface pressure in [mbar]. If both \code{T} and
#'   \code{P} have length > 1, the lengths must be the same.
#' @param species the isotopologue species; one of "abundant" (standard H2O
#'   molecule), "oxygen" (H2-18O molecule), or "deuterium" (HDO molecule).
#' @return numeric vector of water vapour diffusivity in air in [m^2/s] for the
#'   given isotopologue species.
#'
#' @references
#' Johnsen, S. J., Clausen, H. B., Cuffey, K. M., Hoffmann, G., Schwander, J.,
#'   and Creyts, T.: Diffusion of stable isotopes in polar firn and ice: the
#'   isotope effect in firn diffusion, in: Physics of ice core records, edited
#'   by: Hondoh, T., vol. 159, Hokkaido Univ. Press, Sapporo, Japan, 121–140,
#'   2000.
#'
#' Merlivat, L. and J. Jouzel: Global climatic interpretation of the
#'   deuterium-oxygen 18 relationship for precipitation. J. Geophys. Res.:
#'   Oceans, 84, C8, 5029-5033, https://doi.org/10.1029/JC084iC08p05029, 1979.
#'
#' @author Thomas Münch
#'
DAir <- function(T, P, species = "abundant") {

  # check input vector lengths
  if ((length(T) != 1 & length(P) != 1) & (length(T) != length(P)))
    stop("Lengths of 'T' and 'P' must be equal if both are > 1.")

  # retrieve isotopologue factor
  fac <- match.arg(species, c("abundant", "oxygen", "deuterium")) %>%
    c(abundant = 1., oxygen = 1.0285, deuterium = 1.0251)[.] %>%
    unname()

  return(2.11e-5 * (T / 273.15)^(1.94) * (1013.25 / P) / fac)

}

#' Tortuosity factor in firn
#'
#' Calculate the tortuosity factor in firn as a function of firn density. The
#' tortuosity accounts for the shape of the open channels in the firn,
#' controlling the effective diffusivity of vapour molecules within the
#' pore space.
#'
#' @param rho numeric vector of firn density in [kg/m^3].
#' @param b numeric constant in the tortuosity expression from a fit in firn
#'   density to measured tortuosities; defaults to the value given in Johnsen et
#'   al., 2000 (see also Schwander et al., 1988).
#' @param inverse logical controlling the return value; for the default, the
#'   return value is the inverse of the tortuosity factor, i.e. the effective
#'   porosity for diffusive flux, which decreases from a value of 1 at zero
#'   density to 0 at the critical density of \code{rho_ice / sqrt(b)}
#'   (approximately 806.9 kg/m^3 for rho_ice = 920 kg/m^3 and b = 1.3).
#' @return numerical vector of tortuosity (for \code{inverse = FALSE}) or
#'   inverse tortuosity (for \code{inverse = TRUE}).
#'
#' @references
#'
#' Johnsen, S. J., Clausen, H. B., Cuffey, K. M., Hoffmann, G., Schwander, J.,
#'   and Creyts, T.: Diffusion of stable isotopes in polar firn and ice: the
#'   isotope effect in firn diffusion, in: Physics of ice core records, edited
#'   by: Hondoh, T., vol. 159, Hokkaido Univ. Press, Sapporo, Japan, 121–140,
#'   2000.
#'
#' Schwander, J., Stauffer, B., and Sigg, A.: Air Mixing in Firn and the Age of
#'   the Air at Pore Close-Off. Ann. Glac., 10, 141–145,
#'   https://doi.org/10.3189/S0260305500004328, 1988.
#'
#' @author Thomas Münch
#'
tauFirn <- function(rho, b = 1.3, inverse = TRUE) {

  kRhoIce <- 920. # ice density

  invtau <- 1 - b * (rho / kRhoIce)^2
  invtau[invtau <= 0] <- 0

  if (inverse) return(invtau) else return(1 / invtau)

}

# ------------------------------------------------------------------------------
# mathematical and technical utility fuctions
# ------------------------------------------------------------------------------

#' Alternating sequence
#'
#' Generate a sequence of 1's alternating between positive and negative values.
#'
#' @param n integer length of the generated sequence.
#' @return the generated sequence of alternating 1's.
#'
#' @examples
#' FirnR:::gen_alt_seq(3L)
#' FirnR:::gen_alt_seq(4L)
#'
#' @author Thomas Münch
#'
gen_alt_seq <- function(n) {

  if (!is.integer(n)) stop("'n' must be integer.")
  if (n < 1) stop("'n' must be > 0.")

  i <- 0 : (n - 1)

  (-1)^i

}

#' Alternating cumulative sum
#'
#' Calculate the alternating cumulative sum of a numeric vector; i.e. for a
#' vector \code{x = c(x1, x2, x3, x4, ...)} this function calculates the
#' cumulative sum of \code{x = c(x1, -x2, x3, -x4, ...)}.
#'
#' @param x a numeric vector.
#' @return the alternating cumulative sum of \code{x}.
#'
#' @examples
#' cumsum(1 : 6)
#' FirnR:::cumsum_alt(1 : 6)
#' @seealso \code{\link[base]{cumsum}}
#'
#' @author Thomas Münch
#'
cumsum_alt <- function(x) {

  if (!is.numeric(x)) stop("'x' must be numeric.")

  n <- length(x)

  c_alt <- gen_alt_seq(n)

  cumsum(x * c_alt)

}

#' Simpler approx version
#'
#' Wrapper around stats::approx which only returns the interpolated vector.
#'
#' @param ... parameters passed on to \code{\link[stats]{approx}}.
#' @return a numeric vector with the interpolated values.
#'
#' @examples
#' x <- 0 : 10
#' y <- rnorm(11)
#' xout <- seq(0.5, 9.5, 1)
#' approx(x, y, xout)
#' FirnR:::approx.y(x, y, xout)
#' @seealso \code{\link[stats]{approx}}
#'
#' @author Thomas Münch
#'
approx.y <- function(...) {

  stats::approx(...)$y

}

#' Check for equidistant resolution
#'
#' This function checks whether a numeric vector (e.g. a depth vector or from a
#' time series) has equidistant increments, i.e. a constant resolution. This
#' check includes a numerical tolerance that accounts for the machine
#' respresentation of floating-point numbers, which circumvents the problems
#' popular methods of checking equidistance have which use, e.g., \code{sd()} or
#' \code{unique()} on the difference vector of \code{x}.
#'
#' @param x a numeric vector.
#' @return a logical value: \code{TRUE} if \code{x} has constant resolution,
#'   \code{FALSE} otherwise.
#'
#' @examples
#'
#' FirnR:::is.equidistant(1 : 10)
#' FirnR:::is.equidistant(c(2.5, 5, 7.5, 10, 12.5))
#' FirnR:::is.equidistant(x <- seq(0, 12, 0.1)) # compare this to sd(diff(x)) == 0!
#'
#' FirnR:::is.equidistant(c(1 : 10, 18))
#'
#' @author Andrew Dolman, Thomas Münch
#'
is.equidistant <- function(x) {

  if (!is.numeric(x)) stop("'x' needs to be numeric.")

  if ((xl <- length(x)) == 1) return(TRUE)

  xd <- diff(x)
  r <- all.equal(xd, rep(xd[1], xl - 1))

  if (is.logical(r)) return(TRUE) else return(FALSE)

}

#' Linear data transformation
#'
#' Apply a linear transformation (calibration) on input (proxy) data.
#'
#' @param x numeric vector of data to be linearly transformed.
#' @param alpha slope of the transformation.
#' @param beta intercept of the transformation.
#' @return numeric vector of the transformed data.
#'
#' @examples
#'
#' # spatial d18O to temperature calibration for Antarctica
#' # (Masson-Delmotte et al., J. Clim., 21(13), 2008)
#' FirnR:::CalibrateLinear(-44.5, 0.8, -8.1)
#'
#' # spatial d2H to temperature calibration for Antarctica
#' # (Masson-Delmotte et al., J. Clim., 21(13), 2008)
#' FirnR:::CalibrateLinear(-44.5, 6.3, -62.7)
#'
#' @author Thomas Münch
#'
CalibrateLinear <- function(x, alpha, beta) {

  alpha * x + beta

}

#' Bimodal harmonic model
#'
#' This function calculates a harmonic series with two modes for one annual
#' cycle at daily resolution given mean, amplitudes and phases. The harmonic
#' series is implemented with cosine functions; thus, zero phase shift
#' corresponds to the cycle maximum (January 1st).
#'
#' @param x Named numeric vector with five elements:
#' \describe{
#' \item{"A0":}{Mean value over one annual cycle [a.u.];}
#' \item{"A1":}{Amplitude (half peak-peak) of first mode (same unit as mean);}
#' \item{"A2":}{Amplitude of second mode (same unit as mean);}
#' \item{"phi1":}{Phase shift of first mode in [degree];}
#' \item{"phi2":}{Phase shift of second mode in [degree].}
#' }
#' @return Numeric vector of length 365 corresponding to one annual cycle of the
#'   harmonic series.
#'
#' @examples
#'
#' # Different harmonic series
#'
#' days <- 1 : 365
#' x1 <- c(A0 = -45, A1 = 10, A2 = 0, phi1 = 0, phi2 = 0)
#' x2 <- c(A0 = -45, A1 = 10, A2 = 5, phi1 = 10, phi2 = 50)
#'
#' plot(days, FirnR:::CreateHarmonicSeries(x1), type = "l",
#'      las = 1, ylim = c(-60, -30),
#'      xlab = "day of year", ylab = "annual cycle (a.u.)",
#'      main = "Different harmonic models")
#' lines(days, FirnR:::CreateHarmonicSeries(x2), col = 4)
#'
#' legend("topleft",
#'        c("simple sinusoid", "bi-modal with differing amplitude and phase"),
#'        col = c(1, 4), lty = 1, bty = "n")
#'
#' @author Thomas Laepple
#'
CreateHarmonicSeries <- function(x) {

  if (!is.numeric(x)) stop("`x` must be numeric.")
  if (length(x) != 5) stop("`x` must have length 5.")
  if (!length(names(x))) stop("`x` must be a named vector.")

  if (!"A0" %in% names(x)) stop("Missing input `A0`.")
  if (!"A1" %in% names(x)) stop("Missing input `A1`.")
  if (!"A2" %in% names(x)) stop("Missing input `A2`.")
  if (!"phi1" %in% names(x)) stop("Missing input `phi1`.")
  if (!"phi2" %in% names(x)) stop("Missing input `phi2`.")

  A0 <- x["A0"]
  A1 <- x["A1"]
  A2 <- x["A2"]

  # convert input phases from degree to radian
  deg2rad <- pi / 180.
  phi1 <- deg2rad * x["phi1"]
  phi2 <- deg2rad * x["phi2"]

  days <- 1 : 365
  n <- length(days)
  omega <- 2 * pi / n

  h1 <- cos(omega * days + phi1)
  h2 <- cos(2 * omega * days + phi2)

  return(A0 + A1 * h1 + A2 * h2)

}

#' ColorBrewer colour palettes
#'
#' Obtain a colour palette from the ColorBrewer 2.0 collection for use in
#' standard plots, image plots, or filled contour plots.
#'
#' @param name a palette name from the ColorBrewer 2.0 collection; defaults to
#'   the diverging \code{RdYlBu} palette.
#' @param n.in integer number of different input colours in the palette, minimum
#'   is 3, the possible maximum number of colours depends on the chosen
#'   palette. Default \code{NULL} means to use this maximum number of colours.
#' @param n.out integer number of colours for the output vector if \code{fun =
#'   FALSE}; the default is to output as many colours as used for the palette,
#'   so \code{n.out} is set to the value of \code{n.in}. If a different number
#'   is specified here, the corresponding colours are obtained from
#'   interpolation between the range of colours spanned by the palette for the
#'   given number \code{n.in}; see the examples.
#' @param rev logical; set to \code{TRUE} to reverse the order of colours in the
#'   palette.
#' @param fun logical to control the return type; if set to \code{FALSE} (the
#'   default) returned is simply a character vector of length \code{n.out} with
#'   the hexadecimal colour codes from the requested palette. Set to \code{TRUE}
#'   to instead obtain a function that takes an integer argument (the required
#'   number of colours) and returns a character vector of hexadecimal colour
#'   codes, which is needed, e.g., for a filled contour plot
#'   (\code{\link[graphics]{filled.contour}}).
#' @return either a character vector of hexadecimal colour codes, or a function
#'   to return a colour code vector.
#' @author Thomas Münch
#' @seealso \code{\link[grDevices]{colorRampPalette}};
#'   \code{\link[RColorBrewer]{brewer.pal}}
#' @source The ColorBrewer 2.0 collection can be viewed interactively at
#'   <https://colorbrewer2.org>. This function is originally hosted in the
#'   package "grfxtools" (<https://github.com/EarthSystemDiagnostics/grfxtools>).
#'
ColorPal <- function(name = "RdYlBu", n.in = NULL, n.out = NULL,
                     rev = FALSE, fun = FALSE) {

  i <- match(name, rownames(RColorBrewer::brewer.pal.info))

  if (is.na(i)) stop("Unknown ColorBrewer palette.", call. = FALSE)

  if (!length(n.in))  n.in  <- RColorBrewer::brewer.pal.info$maxcolors[i]
  if (!length(n.out)) n.out <- n.in

  cols <- RColorBrewer::brewer.pal(n.in, name)
  if (rev) cols <- rev(cols)

  pal <- grDevices::colorRampPalette(cols)

  if (fun) {
    return(pal)
  } else {
    return(pal(n.out))
  }
}
