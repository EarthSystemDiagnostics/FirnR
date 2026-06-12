#' Find a realistic set from a range of temporal record modifications
#'
#' Loop over a given parameter space of temporal modification values
#' (i.e. advection, diffusion and compression) to modify a given firn proxy
#' record accordingly, and determine the overall minimum root mean square
#' deviation (RMSD) from a reference record.
#'
#' Note that for computational efficiency, the implemented order of
#' modifications is (1) diffusion, (2) compression from densification, and (3)
#' downward advection. This is somewhat unphysical, since the diffusional
#' smoothing thus acts on the uncompressed depth scale. This, however, affects
#' the results only slightly within the domain of high diffusion lengths and
#' high compression values.
#'
#' @param record a data frame of a proxy record with components \code{depth} and
#'   \code{y} holding the depth scale and the proxy values. The depth scale must
#'   be equidistant. This record is modified according to every combination of
#'   \code{advection}, \code{diffusion} and \code{compression}.
#' @param reference a data frame with a reference record (components
#'   \code{depth} and \code{y}) against which the root mean square deviation of
#'   each realisation of the modified record is calculated; must have the same
#'   length and the same depth resolution as \code{record}. 
#' @param advection numeric vector with a set of advection values (i.e. depth
#'   values by which the record is moved downwards (or upwards) through the
#'   firn), measured in the same physical units as component \code{depth} in
#'   \code{record}.
#' @param diffusion numeric vector with a set of (differential) diffusion length
#'   values (see also \code{\link{GetDifferentialDiffusion}}), measured in the
#'   same physical units as component \code{depth} in \code{record}; must be
#'   >= 0.
#' @param compression numeric vector with a set of compression values of the
#'   original record's depth scale measured in the same physical units as
#'   \code{depth} in \code{record}; must not be larger than the length of the
#'   original record.
#' @param verbose logical whether to display progress messages while the code is
#'   running.
#' @return A list of five components:
#' \describe{
#'   \item{advection:}{copy of input \code{advection};}
#'   \item{diffusion:}{copy of input \code{diffusion};}
#'   \item{compression:}{copy of input \code{compression};}
#'   \item{optimum:}{a named vector with the overall minimum RMSD from the
#'     reference record and the corresponding set of optimal advection,
#'     diffusion and compression values.}
#'   \item{RMSD:}{an array of dimension \code{length(advection)} x
#'     \code{length(diffusion)} x \code{length(compression)} which contains the
#'     RMSD value between the reference and the modified input record for every
#'     combination of advection, diffusion and compression.}
#' }
#' @author Thomas Münch
#' @seealso \code{\link{ModifyRecord}}; \code{\link{DiffuseRecord}};
#'   \code{\link{CompressRecord}}; \code{\link{AdvectRecord}}
#' @export
#'
LoopRecordModifications <- function(record, reference, advection, diffusion,
                                    compression, verbose = TRUE) {

  if (!is.data.frame(record)) {
    stop("'record' must be a data.frame.", call. = FALSE)
  }
  if (!is.data.frame(reference)) {
    stop("'reference' must be a data.frame.", call. = FALSE)
  }
  if (any(is.na(match(c("depth", "y"), colnames(record))))) {
    stop("Expected column names for 'record' are: 'depth', 'y'.",
         call. = FALSE)
  }
  if (any(is.na(match(c("depth", "y"), colnames(reference))))) {
    stop("Expected column names for 'reference' are: 'depth', 'y'.",
         call. = FALSE)
  }
  if (nrow(record) != nrow(reference)) {
    stop("'record' and 'reference' must be of the same length.")
  }
  if (!is.equidistant(record$depth))
    stop("Require constant depth resolution.", call. = FALSE)
  if (!is.logical(all.equal(diff(record$depth), diff(reference$depth)))) {
    stop("Depth resolutions of 'record' and 'reference' do not match.",
         call. = FALSE)
  }

  na <- length(advection)
  nd <- length(diffusion)
  nc <- length(compression)
  RMSD <- array(dim = c(na, nd, nc))

  rmsd <- function(v1, v2) {sqrt(mean((v1 - v2)^2, na.rm = TRUE))}

  if (verbose) {

    progress <- (1 : nd) / nd * 100
    cat("\n")
  }

  for (i in 1 : nd) {

    record.d <- ModifyRecord(record, diffusion = diffusion[i])

    for (j in 1 : nc) {

      record.cd <- CompressRecord(record.d, compression = compression[j])

      RMSD[, i, j] <- sapply(advection, function(a) {

        record.cda <- AdvectRecord(record.cd, advection = a)
        rmsd(record.cda$y, reference$y)

      })
      
    }

    if (verbose) {cat(sprintf("%1.0f %%...", progress[i])); cat("\n")}

  }

  # best match
  i.min <- which(RMSD == min(RMSD), arr.ind = TRUE)

  # return results
  list(
    advection = advection,
    diffusion = diffusion,
    compression = compression,
    optimum = c(rmsd = RMSD[i.min], advection = advection[i.min[1]],
                diffusion = diffusion[i.min[2]],
                compression = compression[i.min[3]]),
    RMSD = RMSD
  )

}
