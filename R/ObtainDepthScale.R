#' Obtain depth profile information
#'
#' Obtain a full profile of depth information, i.e. top, midpoint and bottom
#' depths as well as corresponding layer thicknesses, from a given subset of
#' these information. The possible subsets are providing a vector of layer
#' thicknesses, a vector of midpoint depths, or vectors of top and bottom
#' depths.
#'
#' If a vector of layer thicknesses is provided, or a vector of midpoint
#' depths, the additional information of how deep the top of the first layer is
#' actually below the surface is needed to complete the depth information. For a
#' given layer thickness this is straighforward, as top and bottom depths follow
#' from the cumulative sum of the thicknesses and are then merely shifted
#' \code{startDepth} units downwards relative to the surface. If midpoint depths
#' are provided, it is slightly ambiguous. For such a case, the implementation
#' here assumes that the first layer's top depth relative to the surface is also
#' known. This information is used to calculate the thickness of the first layer
#' around the first midpoint depth, with the consecutive layer thicknesses
#' folowing from iteration.
#'
#' @param thickness numeric vector of layer thicknesses; if provided,
#'   corresponding midpoint, top and bottom depth vectors are calculated (any
#'   actual input of the latter is ignored).
#' @param depth numeric vector of midpoint depths; used only if no
#'   \code{thickness} vector is provided. Then corresponding vectors of layer
#'   thickness, top and bottom depth are calculated (any actual input of
#'   \code{top} or \code{bottom} is ignored).
#' @param top numeric vector of top depths; used together with \code{bottom} to
#'   calculate corresponding midpoint depths and layer thicknesses, so both
#'   must have the same length. Only used if no \code{thickness} or
#'   \code{depth} vectors are provided.
#' @param bottom numeric vector of bottom depths; used together with \code{top}
#'   to calculate corresponding midpoint depths and layer thicknesses, so both
#'   must have the same length. Only used if no \code{thickness} or
#'   \code{depth} vectors are provided.
#' @param startDepth distance between the surface and the top of the first firn
#'   or ice layer. Mandatory input in case either \code{thickness} or
#'   \code{depth} is provided (see details). Must be equal to or larger than
#'   zero. The default assumes the first layer to be directly at the surface.
#' @return a data frame of four variables with the complete set of depth
#'   information of layer thicknesses as well as midpoint, top and bottom
#'   depths.
#' @examples
#'
#' # given a vector of layer thicknesses; assume top layer is at surface
#' thickness <- c(1, 3, 6, 2, 4)
#' ObtainDepthScale(thickness)
#' # the same but we know that the top layer is 5 units below the surface
#' ObtainDepthScale(thickness, startDepth = 5)
#'
#' # given a vector of midpoint depths;  assume top layer is at surface
#' depth <- c(0.5, 2.5, 7, 11, 14)
#' ObtainDepthScale(depth = depth)
#'
#' # given a vector of top and bottom depths
#' top <- c(0, 1, 4, 10, 12)
#' bottom <- c(1, 4, 10, 12, 16)
#' ObtainDepthScale(top = top, bottom = bottom)
#'
#' @author Thomas Münch
#' @export
ObtainDepthScale <- function(thickness, depth, top, bottom, startDepth = 0) {

  args <- which(
    c(!missing(thickness), !missing(depth), !missing(top), !missing(bottom)))

  if (!length(args)) {
    stop("Need at least 1 depth argument for 'ObtainDepthScale'.",
         call. = FALSE)
  }

  if (length(args) > 1) {

    if (args[1] == 1) {
      warning("Using only 'thickness' argument, ",
              "ignoring other arguments passed.",
              call. = FALSE)
    }

    if (args[1] == 2) {
      warning("Using only 'depth' argument, ",
              "ignoring other arguments passed.",
              call. = FALSE)
    }

  } else {

    if (args[1] == 3) stop("'top' depths provided, need also 'bottom' depths.")
    if (args[1] == 4) stop("'bottom' depths provided, need also 'top' depths.")

  }

  mode <- c("thickness", "midpoints", "top/bottom")[args[1]]

  if (mode %in% c("thickness", "midpoints")) {

    if (!length(startDepth)) {
      stop("Missing 'startDepth'.", call. = FALSE)
    }

    if (startDepth < 0 | is.na(startDepth)) {
      stop("'startDepth' either < 0 or NA.", call. = FALSE)
    }
  }

  if (mode == "midpoints") {

    if (startDepth >= depth[1]) {
      stop("Given 'startDepth' >= first midpoint depth, ",
           "which would yield zero or negative first layer thickness.",
           call. = FALSE)
    }

    n <- length(depth)

    thickness_1 <- 2 * (depth[1] - startDepth)

    # vectorized solution; is much faster than for-loop approach for large n
    c_alt <- gen_alt_seq(n - 1L)
    s_c   <- cumsum_alt(depth[-n])

    thickness_2_n <- 2 * (depth[-1] - 2 * s_c * c_alt + startDepth * c_alt)

    thickness <- c(thickness_1, thickness_2_n)

    mode <- "thickness"
  }

  if (mode == "thickness") {

    profile <- data.frame(thickness = thickness) %>%
      dplyr::mutate(bottom = startDepth + cumsum(.data$thickness)) %>%
      dplyr::mutate(top = c(startDepth, .data$bottom[-dplyr::n()])) %>%
      dplyr::mutate(depth = (.data$top + .data$bottom) / 2) %>%
      dplyr::select(depth, top, bottom, thickness)

  } else {

    if (length(top) != length(bottom)) {
      stop("'top' and 'bottom' must have the same length.", call. = FALSE)
    }

    profile <- data.frame(top = top, bottom = bottom) %>%
      dplyr::mutate(depth = (.data$top + .data$bottom) / 2) %>%
      dplyr::mutate(thickness = .data$bottom - .data$top) %>%
      dplyr::select(depth, top, bottom, thickness)

  }

  return(profile)

}
