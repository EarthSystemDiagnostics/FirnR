#' Diffuse a proxy record
#'
#' This function diffuses a proxy time series or record with a given diffusion
#' length by convolution with a Gaussian kernel.
#'
#' The function expects a numeric vector with the depth or time-dependent
#' diffusion length of the same length as the \code{record}, or a single value to use
#' a constant diffusion length. The input diffusion length is internally
#' rescaled according to the resolution of the record, so that the diffusion
#' length corresponds to the number of record bins effectively being
#' smoothed. It is thus mandatory for the user to ensure that the record's
#' sampling scale (e.g., depth or time) and the diffusion length are measured in
#' the same physical units.
#'
#' The convolution integral is solved by a simple summation over the kernel
#' width which is set to ~ 10 times the local diffusion length.
#'
#' The parameter \code{pad} controls the behaviour at the ends of the
#' record. Without "padding" (\code{pad = FALSE}), both ends of the diffused
#' record will contain \code{NA} values as a result of the diffusion kernel
#' extending beyond the record ends, with the number of \code{NA} values
#' depending on the local width of the kernel, thus on the values of the
#' diffusion length around the upper and lower end of the record. These
#' \code{NA} values can be avoided by setting \code{pad = TRUE} (the default),
#' which causes the diffusion kernel to be clipped at the upper end to the range
#' below the surface, and the \code{record} at the lower end to be extended with
#' its mean value to an additional length of ~ 10 times the overall maximum
#' value of \code{sigma}.
#'
#' @param record a data frame with components \code{depth} and \code{y} holding
#'   the sampling (e.g., depth or time) scale and the proxy values to be
#'   diffused. The sampling scale must be equidistant. It can also contain any
#'   additional columns which remain unchanged.
#' @param sigma numeric vector of diffusion lengths; either of the same length
#'   as the \code{record} to provide local diffusion lengths corresponding to
#'   every sampling point of the \code{record}, or of length one to diffuse the
#'   \code{record} with a constant diffusion length. The diffusion length must
#'   be given in the same units as the record's sampling scale, see Details.
#' @param pad logical, defaults to \code{TRUE}; normally, the finite record
#'   length leads to \code{NA} values at the top and bottom of the diffused
#'   record. The default setting of \code{pad} avoids this by clipping the
#'   diffusion kernel at the top of the record and extending the record with its
#'   mean at the bottom; see also Details.
#' @return a data frame with the diffused version of the proxy \code{record}.
#' @author Thomas Münch, with contributions by Thomas Laepple
#' @examples
#'
#' # Diffuse white noise with a linearly increasing diffusion length
#' n <- 1000
#' record <- data.frame(depth = seq(n), y = rnorm(n))
#' sigma <- seq(1, 8, length.out = n)
#' diffused <- DiffuseRecord(record, sigma)
#' plot(record, type = 'l', las = 1,
#'      xlab = "Depth (a.u.)", ylab = "Data (a.u.)",
#'      main =
#'        "White noise diffusion with linearly increasing diffusion length")
#' lines(diffused, col = "red")
#' legend("topleft", c("Original record", "Diffused record"),
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
  # (last select step reestablishes original column order)
  data.frame(depth = x$depth, ydiff = rec.diffused) %>%
    dplyr::left_join(record, ., by = dplyr::join_by("depth")) %>%
      dplyr::select(-"y") %>%
      dplyr::rename(y = "ydiff") %>%
      dplyr::select(colnames(record))

}
