#' Linearly interpolate proxy values onto a shorter depth scale
#'
#' This function simulates the effect of firn densification by linearly
#' interpolating given data onto a compressed (shorter) depth scale. This
#' compressed depth scale is obtained from subtracting the amount of compression
#' (\code{stretch}) from the length of the original record and
#' dividing this shorter length into bins with size proportional to the original
#' record's resolution. The proxy values on the compressed depth scale are found
#' by linear interpolation of the original values from the compressed depth
#' scale onto the original depth scale.
#'
#' @param record a data frame with components \code{depth} and \code{y} holding
#'   the original depth scale and proxy values.
#' @param stretch numeric value of the amount of compression of the original
#'   depth scale measured in the same physical units as \code{depth} in
#'   \code{record}; cannot be larger than the length of the original record.
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

  if (!is.data.frame(record)) {
    stop("'record' must be a data.frame.", call. = FALSE)
  }
  if (any(is.na(match(c("depth", "y"), colnames(record))))) {
    stop("Expected column names for 'record' are: 'depth', 'y'.",
         call. = FALSE)
  }
  if (nrow(record) <= 1) stop("Length of proxy record needs to be > 1.")
  if (length(stretch) != 1) stop("'stretch' needs to be of length 1.")
  if (is.na(stretch)) stop("Missing value passed for 'stretch'.")

  if (stretch >= (len <- diff(range(record$depth)))) {
    stop("Compression value >= range (max - min) of original depth scale.")
  }
  if (stretch < 0) warning("Negative 'stretch' parameter yields longer record.")

  # new bin sizes
  new.bin.s <- diff(record$depth) * (1 - (stretch / len))
  # depth scale after densification
  new.depth <- c(record$depth[1], record$depth[1] + cumsum(new.bin.s))

  # approximate record on original depth scale
  data.frame(
    depth = record$depth,
    y = approx(new.depth, record$y, record$depth)$y
  )

}

