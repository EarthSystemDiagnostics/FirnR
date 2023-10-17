#' Move a record downwards in the firn
#'
#' Simulate the effect of new snowfall on the depth position of a given proxy
#' record for a coordinate system fixed at the snow/firn surface by shifting the
#' record a number of k bins downward, where k is given by the thickness of the
#' new snow layer (the 'advection' value) together with the depth resolution of
#' the record. 
#'
#' @param record a data frame of a proxy record with components \code{depth} and
#'   \code{y} holding the depth scale and the proxy values. The depth scale must
#'   be equidistant.
#' @param advection numeric value for the advection, i.e. the depth value by
#'   which the record is moved deeper into the firn, measured in the same
#'   physical units as component \code{depth} in \code{record}.
#' @param clip logical; the default method shifts the proxy record out through
#'   the "bottom" of the record so that the shifted bins are lost. For
#'   \code{clip = FALSE}, the record is extended by the number of bins shifts so
#'   that it can still hold all original proxy values; see the examples.
#' @importFrom rlang .data
#' @return a data frame with components \code{depth} and \code{y} holding the
#'   shifted (advected) proxy record.
#' @author Thomas Münch
#' @examples
#'
#' # in this example, the lowermost three proxy values are "lost" by the
#' # downward advection since clip = TRUE:
#' data.frame(depth = 1 : 10, y = 1 : 10) %>%
#'   AdvectRecord(advection = 3)
#'
#' # for clip = FALSE all proxy values are kept but we get a longer data frame:
#' data.frame(depth = 1 : 10, y = 1 : 10) %>%
#'   AdvectRecord(advection = 3, clip = FALSE)
#' 
#' @export
#'
AdvectRecord <- function(record, advection, clip = TRUE) {

  if (!is.data.frame(record)) {
    stop("'record' must be a data.frame.", call. = FALSE)
  }
  if (any(is.na(match(c("depth", "y"), colnames(record))))) {
    stop("Expected column names for 'record' are: 'depth', 'y'.",
         call. = FALSE)
  }
  if (nrow(record) <= 1) stop("Length of proxy record needs to be > 1.")
  if (length(advection) != 1) stop("'advection' needs to be of length 1.")
  if (is.na(advection)) stop("Missing value passed for 'advection'.")
  if (advection < 0) stop("'advection' value needs to be >= 0.")
  if (length(depth.res <- diff(record$depth)) > 1) {
    if (sd(depth.res) != 0) stop("Require constant depth resolution.")
  }

  depth.res <- depth.res[1]
  k <- round(advection / depth.res)

  if (clip) {

    dplyr::mutate(record, y = prxytools::Lag(.data$y, shift = k))

  } else {

    data.frame(
      depth = c(record$depth, max(record$depth) + depth.res * (1 : k)),
      y = prxytools::Lag(c(record$y, rep(NA, k)), shift = k)
    ) %>%
      {if (tibble::is_tibble(record)) {tibble::as_tibble(.)} else { . }}

  }
}
