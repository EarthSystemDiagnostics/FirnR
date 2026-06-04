#' Diffuse a proxy record.
#'
#' This function diffuses a proxy time series or record with a given diffusion
#' length by convolution with a Gaussian kernel.
#'
#' The function expects a numeric vector with the depth or time-dependent
#' diffusion length of the same length as \code{rec}, or a single value to use
#' a constant diffusion length. The input diffusion length is internally
#' rescaled according to the resolution of the record given by \code{res}, so
#' that the diffusion length corresponds to the number of record bins
#' effectively being smoothed. The convolution integral is then solved by a
#' simple summation over the kernel width which is set to ~ 10 times the local
#' diffusion length.
#'
#' The parameter \code{pad} controls the behaviour at the ends of the
#' record. Without "padding" (\code{pad = FALSE}), both ends of the diffused
#' record will contain \code{NA} values as a result of the kernel extending
#' beyond the record ends, with the number of \code{NA} values depending on the
#' local width of the diffusion kernel, thus on the values of the diffusion length
#' around the upper and lower end of the record. These \code{NA} values can be
#' avoided by setting \code{pad = TRUE}, which is the default setting. At the
#' upper end, the kernel is then clipped to the range below the surface; at the
#' lower end, the record is extended with the mean average value of \code{rec}
#' to an additional length of ~ 10 times the overall maximum value of
#' \code{sigma}.
#'
#' @param rec numeric vector of the record to be diffused, tabulated at an
#'   equidistant resolution given by \code{res}.
#' @param sigma numeric vector of diffusion lengths; either of the same length
#'   as \code{rec} to provide local diffusion lengths corresponding to every
#'   depth or time point at which \code{rec} is tabulated, or of length one to
#'   diffuse \code{rec} with a constant diffusion length. The diffusion length
#'   must be given in the same units as is the resolution of \code{rec}.
#' @param res numeric; single value of the equidistant resolution of \code{rec}
#'   in the same units as \code{sigma}; e.g., a record resolution of 1 cm
#'   (\code{res = 1}) requires that the diffusion lengths are also given in
#'   units of cm, a resolution of 5 years (\code{res = 5}) requires diffusion
#'   lengths given in units of years. This is needed for rescaling the
#'   diffusion length into units of bin size; see also Details.
#' @param pad logical; the default setting \code{TRUE} avoids \code{NA} values
#'   at the top and bottom of the diffused record, which would otherwise result
#'   from the finite record length, by clipping the diffusion kernel at the top
#'   and extending the record with its mean at the bottom; see also Details.
#' @return numeric vector of the diffused version of \code{rec}.
#' @author Thomas Münch, with contributions by Thomas Laepple
#' @examples
#'
#' ## Diffuse white noise with a linearly increasing diffusion length
#' rec <- rnorm(n = 1000)
#' var.sigma <- seq(1, 8, length.out = 1000)
#' diffused <- DiffuseRecord(rec = rec, sigma = var.sigma)
#' plot(rec, type = 'l', las = 1, xlab = "depth in cm", ylab = "data", main = 
#'      "white noise diffusion with linearly increasing diffusion length")
#' lines(diffused, col = "red")
#' legend('topleft', c("original record", "diffused record"),
#'        lty = 1, col = 1 : 2, bty = "n")
#'
#' @export
#'
DiffuseRecord <- function(record, sigma, pad = TRUE) {

  if (!is.data.frame(record)) {
    stop("'record' must be a data.frame.", call. = FALSE)
  }
  if (any(is.na(match(c("depth", "y"), colnames(record))))) {
    stop("Expected column names for 'record' are: 'depth', 'y'.",
         call. = FALSE)
  }
  if ((nr <- nrow(record)) <= 1) stop("Length of 'record' needs to be > 1.")

  if (missing(sigma)) {
    stop("No diffusion length passed as input.", call. = FALSE)
  }
  if (any(!is.finite(sigma))) {
    stop("Missing values passed as diffusion length.", call. = FALSE)
  }

  ns <- length(sigma)

  if (ns != 1 & ns != nr) {
    stop("Diffusion length neither of length 1 nor matches length of record.",
         call. = FALSE)
  }

  if (!is.equidistant(record$depth))
    stop("Require constant depth resolution for diffusion.", call. = FALSE)

  # recycle sigma if needed
  if (ns == 1) {
    sigma <- rep(sigma, nr)
  }

  # scale diffusion length according to resolution of record
  res <- diff(record$depth)[1]
  sigma <- sigma / res

  # remove any leading and trailing NA's and extract record data
  x <- zoo::na.trim(record)
  rec <- x$y
  nr <- length(rec)

  # pad record end with its mean to avoid NA's at the end of diffused record
  if (pad) {
    rec <- c(rec, rep(mean(rec, na.rm = TRUE), ceiling(10 * max(sigma))))
  }

  # vector to store diffused data
  rec.diffused <- rep(NA, nr)

  # loop over record
  for (i in 1 : nr) {

    # diffusion length for current depth
    sig <- sigma[i]

    if (sig == 0) {

      rec.diffused[i] <- rec[i]

    } else {

      # set range of convolution integral (= 2 * max + 1) to ~ 10 * sig
      max <- ceiling(5 * sig)
      I <- -max : max
      range <- (i - max) : (i + max)

      # for pad = TRUE, skip that part of range in the convolution
      # integral which is extending above surface, else set diffused value
      # to 'NA' there
      if (max >= i) {

        if (pad) {
          keep <- range > 0
          range <- range[keep]
          I <- I[keep]
        } else {
          rec.diffused[i] <- NA
          next
        }
      }

      # convolution kernel
      kernel <- exp(-I^2 / (2 * sig^2))

      # diffuse data at current depth bin
      rec.diffused[i] <- sum(kernel * rec[range]) / sum(kernel)
    }
  }

  # merge with original record to retain original length in case of NA trimming
  # (left_join step makes a tibble if input record was a tibble)
  data.frame(depth = x$depth, ydiff = rec.diffused) %>%
    dplyr::left_join(record, ., by = dplyr::join_by("depth")) %>%
      dplyr::select(-y) %>%
      dplyr::rename(y = ydiff)

}
