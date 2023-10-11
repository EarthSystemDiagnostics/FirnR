#' Linearly interpolate proxy values onto a shorter depth scale
#'
#' This function simulates the effect of firn densification by linearly
#' interpolating given data onto a compressed (shorter) depth scale. This
#' compressed depth scale is found by subtracting the amount of compression
#' (\code{stretch}) from the maximum depth of the original depth scale and
#' dividing it into equal bins the same number as in the original depth
#' vector. The proxy values on the compressed depth scale are found by linear
#' interpolation of the original values from the compressed depth scale onto the
#' original depth scale.
#'
#' @param record a data frame with components \code{depth} and \code{y} holding
#'   the original depth scale and proxy values.
#' @param stretch numeric value of the amount of compression of the original
#'   depth scale measured in the same physical units as \code{depth} in
#'   \code{record}.
#' @return a data frame with components \code{depth} and \code{y} holding the
#'   original depth scale and the interpolated (compressed) proxy values.
#' @author Thomas Münch
#' @examples
#'
#' original <- data.frame(
#'   depth = 1 : 30,
#'   y = sin((2 * pi / 10) * (1 : 30)) + rnorm(30))
#'
#' plot(original, type = "l")
#' lines(CompressRecord(original, stretch = 2.8), col = "red")
#' legend("topright", c("original", "compressed"), lty = 1,
#'        col = c("black", "red"), bty = "n")
#' 
#' @export
#'
CompressRecord <- function(record, stretch) {

  if (any(is.na(match(c("depth", "y"), colnames(record))))) {
    stop("Expected column names for 'record' are: 'depth', 'y'.",
         call. = FALSE)
  }
  if (nrow(record) == 1) stop("Proxy record is of length 1.")
  if (stretch >= (max(record$depth) - record$depth[1])) {
    stop("Compression value >= range (max - min) of original depth scale.")
  }
  if (stretch < 0) warning("Negative 'stretch' parameter yields longer record.")

  # depth scale after densification
  new.depth <- seq(record$depth[1], max(record$depth) - stretch,
                   length.out = nrow(record))

  # approximate record on original depth scale
  data.frame(
    depth = record$depth,
    y = approx(new.depth, record$y, record$depth)$y
  )

}

