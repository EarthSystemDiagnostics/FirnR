#' Linearly interpolate proxy values onto a shorter depth scale
#'
#' This function simulates the effect of firn densification by linearly
#' interpolating given data onto a compressed (shorter) depth scale. This
#' compressed depth scale is obtained from subtracting the amount of compression
#' from the length of the original record and dividing this shorter length into
#' bins with size proportional to the original record's resolution. The proxy
#' values on the compressed depth scale are found by linear interpolation of the
#' original values from the compressed depth scale onto the original depth
#' scale. Note that leading and trailing missing values are automatically
#' removed from the input vector and so the original record length is based on
#' the trimmed data. After applying the compression, the trimmed NA values are
#' added in again to ensure the output data frame has the same number of rows as
#' the input.
#'
#' @param record a data frame with components \code{depth} and \code{y} holding
#'   the original depth scale and proxy values. Any additional columns in
#'   \code{record} are ambiguous to handle and therefore dropped with a
#'   warning.
#' @param compression numeric value of the amount of compression of the original
#'   depth scale measured in the same physical units as \code{depth} in
#'   \code{record}; must not be larger than the length of the original
#'   record. Note that for general applicability a value < 0 is also allowed,
#'   yielding a stretched record, which, however, cannot be the result of a
#'   densification process.
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
#' lines(CompressRecord(original, compression = 2.8), col = "red")
#' legend("topright", c("original", "compressed"), lty = 1,
#'        col = c("black", "red"), bty = "n")
#' 
#' @export
#'
CompressRecord <- function(record, compression) {

  if (!is.data.frame(record)) {
    stop("'record' must be a data.frame.", call. = FALSE)
  }
  nms <- colnames(record)
  if (any(is.na(match(c("depth", "y"), nms)))) {
    stop("Expected column names for 'record' are: 'depth', 'y'.",
         call. = FALSE)
  }
  if (nrow(record) <= 1) stop("Length of proxy record needs to be > 1.")
  if (length(compression) != 1) stop("'compression' needs to be of length 1.")
  if (is.na(compression)) stop("Missing value passed for 'compression'.")

  # remove any additional record columns
  if (length(nms) > 2) {
    drop <- names(dplyr::select(record, -c("depth", "y"))) %>%
      sapply(function(x) {sprintf(fmt = "`%s`", x)}) %>%
      paste(collapse = ", ")
    warning("Cannot handle additional columns when compressing; dropping ",
            drop, ".", call. = FALSE)

    record <- dplyr::select(record, c("depth", "y"))
  }

  # remove leading and trailing NA's
  x <- zoo::na.trim(record)

  if (compression >= (len <- diff(range(x$depth)))) {
    stop("Compression value >= range (max - min) of original depth scale.")
  }

  # new bin sizes
  new.bin.s <- diff(x$depth) * (1 - (compression / len))
  # depth scale after densification, with numerical precision considered
  digits.precision <- 12
  new.depth <- c(x$depth[1], x$depth[1] + cumsum(new.bin.s)) %>%
    round(digits.precision)

  # approximate record on original depth scale and
  # merge with trimmed part to retain original data frame length
  # (left_join step makes a tibble if input record was a tibble)
  data.frame(depth = x$depth, yc = stats::approx(new.depth, x$y, x$depth)$y) %>%
    dplyr::left_join(record, ., by = dplyr::join_by("depth")) %>%
    dplyr::select("depth", y = "yc")

}
