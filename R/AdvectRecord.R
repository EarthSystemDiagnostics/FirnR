#' Move a record through the firn
#'
#' Simulate the effect of new snowfall on the depth position of a given proxy
#' record for a coordinate system fixed at the snow/firn surface by shifting the
#' record a number of k bins downward, where k is given by the thickness of the
#' new snow layer (the 'advection' value) together with the depth resolution of
#' the record. For the sake of completeness, also upward shifts ("negative
#' advection") are possible.
#'
#' @param record a data frame of a proxy record with components \code{depth} and
#'   \code{y} holding the depth scale and the proxy values. The depth scale must
#'   be equidistant. Any additional columns in \code{record} are ambiguous to
#'   handle and therefore dropped with a warning.
#' @param advection numeric value for the advection, i.e. the depth increment by
#'   which the record is moved through the firn, measured in the same physical
#'   units as component \code{depth} in \code{record}.
#' @param clip logical; for positive advection values, the default method shifts
#'   the proxy record out through the "bottom" of the record so that the shifted
#'   bins are lost. For \code{clip = FALSE}, the record is extended by the
#'   number of bin shifts so that it can still hold all original proxy values;
#'   see the examples. For negative advection values, the setting of \code{clip}
#'   has no effect.
#' @importFrom rlang .data
#' @return a data frame with components \code{depth} and \code{y} holding the
#'   shifted (advected) proxy record.
#' @author Thomas Münch
#' @examples
#'
#' df <- data.frame(depth = 1 : 10, y = 1 : 10)
#'
#' # in this example, the lowermost three proxy values are "lost" by the
#' # downward advection since clip = TRUE:
#' AdvectRecord(df, advection = 3)
#'
#' # for clip = FALSE all proxy values are kept but we get a longer data frame:
#' AdvectRecord(df, advection = 3, clip = FALSE)
#'
#' # note that for negative advection the setting of `clip` has no effect:
#' all.equal(AdvectRecord(df, advection = -3),
#'           AdvectRecord(df, advection = -3, clip = FALSE))
#' 
#' @export
#'
AdvectRecord <- function(record, advection, clip = TRUE) {

  if (!is.data.frame(record)) {
    stop("'record' must be a data.frame.", call. = FALSE)
  }
  nms <- colnames(record)
  if (any(is.na(match(c("depth", "y"), nms)))) {
    stop("Expected column names for 'record' are: 'depth', 'y'.",
         call. = FALSE)
  }
  if (nrow(record) <= 1) stop("Length of proxy record needs to be > 1.")
  if (length(advection) != 1) stop("'advection' needs to be of length 1.")
  if (is.na(advection)) stop("Missing value passed for 'advection'.")
  if (!is.equidistant(record$depth)) stop("Require constant depth resolution.")

  # remove any additional record columns
  if (length(nms) > 2) {
    drop <- names(dplyr::select(record, -c("depth", "y"))) %>%
      sapply(function(x) {sprintf(fmt = "`%s`", x)}) %>%
      paste(collapse = ", ")
    warning("Cannot handle additional columns when advecting; dropping ",
            drop, ".", call. = FALSE)

    record <- dplyr::select(record, c("depth", "y"))
  }

  depth.res <- diff(record$depth)[1]
  k <- round(advection / depth.res)

  if (k == 0) return(record)

  if (clip == FALSE & k > 0) {

    data.frame(
      depth = c(record$depth, max(record$depth) + depth.res * (1 : k)),
      y = Hmisc::Lag(c(record$y, rep(NA, k)), shift = k)
    ) %>%
      {if (tibble::is_tibble(record)) {tibble::as_tibble(.)} else { . }}

  } else {

    dplyr::mutate(record, y = Hmisc::Lag(.data$y, shift = k))

  }

}
