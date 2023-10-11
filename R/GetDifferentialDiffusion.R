#' Relative change in firn diffusion length with depth
#'
#' Calculate the differential firn diffusion length, as defined in Münch et
#' al. (2007), over a specified depth interval given a diffusion length
#' profile.
#'
#' @param data a data frame providing a firn diffusion length profile via the
#'   two elements 'depth' and 'sigma'.
#' @param z00 top of the first depth interval.
#' @param z01 bottom of the first depth interval.
#' @param z10 top of the second depth interval.
#' @param z11 bottom of the second depth interval.
#' @return the differential diffusion length value as calculated from the mean
#'   diffusion length values in the depth intervals \code{[z00, z01]} and
#'   \code{[z10, z11]}.
#' @author Thomas Münch
#' @examples
#'
#' # use polynomial fit of Kohnen density data to calculate
#' # diffusion length profiles for d18O and dD
#'
#' d <- b41.b42.density$stack$depth
#' r <- b41.b42.density$stack$fitDensity
#'
#' data1 <- data.frame(
#'   depth = d,
#'   sigma = FirnR::DiffusionLength(d, r, P = 650, T = 228.5, bdot = 64)
#' )
#' data2 <- data.frame(
#'   depth = d,
#'   sigma = FirnR::DiffusionLength(d, r, P = 650, T = 228.5, bdot = 64,
#'                                  dD = TRUE)
#' )
#'
#' # calculate differential diffusion length over example depth intervals
#'
#' GetDifferentialDiffusion(data1, 0, 1, 0.5, 1.5)
#' GetDifferentialDiffusion(data2, 0, 1, 0.5, 1.5)
#'
#' @source Eq. (2) in Münch et al. (2017)
#' @references
#'
#' Münch, T., et al., Constraints on post-depositional isotope modifications
#' in East Antarctic firn from analysing temporal changes of isotope profiles,
#' The Cryosphere, 11(5), 2175-2188, doi:10.5194/tc-11-2175-2017, 2017.
#'
#' @export
#'
GetDifferentialDiffusion <- function(data, z00, z01, z10, z11) {
  
  if (!is.data.frame(data)) stop("'data' must be a data.frame.", call. = FALSE)
  if (nrow(data) <= 1) stop("'data' must have length > 1.", call. = FALSE)
  if (any(is.na(match(c("depth", "sigma"), colnames(data))))) {
    stop("Expected column names for 'data' are: 'depth', 'sigma'.",
         call. = FALSE)
  }
  if (!all(c(z00, z01, z10, z11) >= min(data$depth)) |
      !all(c(z00, z01, z10, z11) <= max(data$depth))) {
    stop("depth intervals [z00, z01], [z10, z11] must lie ",
         "within depth range of data.", call. = FALSE)
  }
  if (z01 <= z00) stop("'z01' must be > 'z00'.", call. = FALSE)
  if (z11 <= z10) stop("'z11' must be > 'z10'.", call. = FALSE)

  # average diffusion length values over specified depth intervals
  s1 <- mean(data$sigma[which(data$depth >= z00 & data$depth <= z01)])
  s2 <- mean(data$sigma[which(data$depth >= z10 & data$depth <= z11)])

  # differential diffusion length value
  sqrt(s2^2 - s1^2)

}
