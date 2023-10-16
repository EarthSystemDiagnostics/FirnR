#' Model temporal change of firn profile
#'
#' Modify a firn proxy record (e.g., an isotope record) to simulate the changes
#' over time arising from downward advection, i.e. the virtual downward movement
#' of the record due to new snowfall, from compression by densification, and
#' from diffusional smoothing.
#'
#' The user has to ensure that the depth resolution of the input record is
#' sufficiently high to faithfully simulate the modification processes. A
#' possibly needed linear interpolation of the input record onto a higher
#' resolution has therefore be done prior to calling \code{ModifyRecord}. Then,
#' the parameter \code{output.res} can be used to obtain the original coarser
#' resolution of the record.
#'
#' @param record a data frame with components \code{depth} and \code{y} holding
#'   the original depth scale and proxy values.
#' @param sigma numeric value for the diffusion length to smooth the record;
#'   must be in the same physical units as the depth resolution of the
#'   \code{record}. If \code{NULL}, no diffusional smoothing is applied.
#' @param compression numeric value of the amount of compression of the original
#'   depth scale measured in the same physical units as component \code{depth}
#'   in \code{record}; cannot be larger than the length of the original
#'   record. If \code{NULL}, no compression by densification is modelled.
#' @param advection numeric value for the advection, i.e. the depth value by
#'   which the record is moved deeper into the firn, measured in the same
#'   physical units as component \code{depth} in \code{record}. If \code{NULL},
#'   no downward advection is modelled.
#' @param output.res optional numeric value for the depth resolution the
#'   modified record shall be interpolated to upon output; see Details. The
#'   default outputs the modified record on the depth resolution of the input.
#' @param clip logical to control whether the output data frame is extended by
#'   the amount of the downward advection; see \code{\link{AdvectRecord}} for
#'   details.
#' @return a data frame with components \code{depth} and \code{y} holding the
#'   modified proxy record. If no moficiation parameters are specified, returned
#'   is simply the input record.
#' @author Thomas Münch
#' @seealso \code{\link{DiffuseRecord}}; \code{\link{CompressRecord}};
#'   \code{\link{AdvectRecord}}
#' @examples
#'
#' original <- data.frame(depth = 1 : 10, y = 1 : 10)
#'
#' ModifyRecord(original) # = input
#' ModifyRecord(original, advection = 4.5, sigma = 1.5, compression = 2.1)
#' ModifyRecord(original, advection = 4.5, sigma = 1.5, compression = 2.1,
#'              output.res = 3)
#'
#' @export
#'
ModifyRecord <- function(record, sigma = NULL, compression = NULL,
                         advection = NULL, output.res = NULL, clip = TRUE) {

  if (!is.null(output.res)) {

    if (length(output.res) != 1) {
      stop("'output.res' needs to be of length 1.", call. = FALSE)
    }
    if (is.na(output.res)) {
      stop("Missing value passed for 'output.res'.", call. = FALSE)
    }
    if (output.res < 0) {
      stop("'output.res' value needs to be > 0.", call. = FALSE)
    }
  }

  if (is.null(c(sigma, compression, advection))) {

    warning("No modification parameters - returning input.", call. = FALSE)
    return(record)

  }

  diffuse  <- !is.null(sigma)
  compress <- !is.null(compression)
  advect   <- !is.null(advection)

  interpolate <- !is.null(output.res)
  
  # helper function for diffusion until DiffuseRecord can handle data frames
  .hlp.diffuse <- function(record, sigma) {

    if (length(depth.res <- diff(record$depth)) > 1) {
      if (sd(depth.res) != 0) {
        stop("Require constant depth resolution for diffusion.", call. = FALSE)
      }
    }

    # remove leading and trailing NA's, diffuse, and merge with trimmed part
    record %>%
      zoo::na.trim() %>%
      dplyr::mutate(yd = DiffuseRecord(.data$y, sigma, depth.res[1]),
                    .keep = "unused") %>%
      dplyr::left_join(record, ., by = dplyr::join_by("depth")) %>%
      dplyr::select("depth", y = "yd")

  }
  
  # make modifications

  output <- record %>%
    {if (diffuse) {.hlp.diffuse(., sigma)} else { . }} %>%
    {if (compress) {CompressRecord(., compression)} else { . }} %>%
    {if (advect) {AdvectRecord(., advection, clip = clip)} else { . }}

  # return output on requested resolution

  if (interpolate) {

    output.depth <- seq(output$depth[1], max(output$depth), by = output.res)

    output <- data.frame(
      depth = output.depth,
      y = approx(output$depth, output$y, output.depth)$y
    ) %>%
      {if (tibble::is_tibble(record)) {tibble::as_tibble(.)} else { . }}

  }

  return(output)

}
