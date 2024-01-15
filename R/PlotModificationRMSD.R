#' Root-mean-square deviation contour plot of record modifications
#'
#' Make a contour plot of root-mean-square deviations (RMSD) between a reference
#' record and different versions of a record from modifying the original record
#' according to a range of "temporal modifications", i.e. downward advection,
#' (differential) diffusion and compression from densification (see also
#' Details).
#'
#' The function expects as input a list structure in the format as output from
#' \code{\link{LoopRecordModifications}}, so a convenient workflow is to
#' call that function first to create the RMSD data for different record
#' modifications and then pipe the results into
#' \code{PlotModificationRMSD}. If instead you want to create the RMSD 
#' data independently, you need to shape it into the following list format for
#' plotting it here:
#' \describe{
#'   \item{advection:}{vector of studied downward advection values;}
#'   \item{sigma:}{vector of studied (differential) diffusion lengths;}
#'   \item{compression:}{vector of studied compression values;}
#'   \item{optimum:}{a named vector with the overall minimum RMSD from the
#'     reference record and the corresponding set of optimal advection,
#'     diffusion and compression values.}
#'   \item{RMSD:}{an array of dimension \code{length(advection)} x
#'     \code{length(sigma)} x \code{length(compression)} which contains the
#'     RMSD value between the reference and the modified record for every
#'     combination of advection, diffusion and compression.}
#' }
#'
#' To visualize the three-dimensional RMSD data in two dimensions, the function
#' projects the RMSD data onto the two-dimensional surface of optimal advection
#' values, i.e. for every combination of compression and diffusion length, that
#' advection value which yields the minimum RMSD across all advection values is
#' extracted. These 2D optimal advection values are plotted as a filled contour
#' plot along with the corresponding (minimum) RMSD values as a simple contour
#' line plot. The global minimum in RMSD is added as a point.
#'
#' @param data a list with RMSD data; most conveniently obtained from
#'   \code{\link{LoopRecordModifications}}.
#' @param palette a colour palette function to be used to assign colours; the
#'   default is to calculate the colour palette internally from ten colours of
#'   the diverging \code{RdYlBu} palette in the ColorBrewer 2.0
#'   collection.
#' @param xlim x limits for the plot; default is to use the data range.
#' @param ylim same as \code{xlim} for the y limits.
#' @param zlim same as \code{xlim} for the z limits.
#' @param main optional character string to be placed above the plot.
#' @param unit character string of the physical unit in which the advection,
#'   diffusion length and compresion values are measured, used for x and y axes
#'   labels and the colour bar legend; hence it is assumed that all three
#'   quantities are measured in the same unit!
#' @param line.h margin line where to position the label for the horizontal
#'   plot axis.
#' @param line.v same as \code{line.h} for the vertical plot axis.
#' @param hadj adjustment of the colour bar legend in horizontal plot direction
#'   relative to the default positioning; measured in user coordinates.
#' @param cex character extension factor for the point marking the global
#'   minimum RMSD on the plot as a multiple of the current setting of
#'   \code{par{"cex"}}.
#' @param contour.labcex character extension factor for the contour labelling as
#'   an absolute size (**not** a multiple of \code{par{"cex"}}).
#' @author Thomas Münch
#' @seealso \code{\link{LoopRecordModifications}}
#' @examples
#'
#' library(dplyr)
#'
#' # dummy data for illustration
#'
#' n <- 100
#' record <- reference <- data.frame(depth = seq(n), y = seq(n))
#'
#' # study record modifications and pipe results direclt into plotting function
#' 
#' rmsd.data <- LoopRecordModifications(
#'   record, reference,
#'   advection = seq(10) - 1,
#'   sigma = 0 : 5,
#'   compression = 0 : 5
#'  ) %>%
#'    PlotModificationRMSD(cex = 2)
#'
#' @export
#'
PlotModificationRMSD <- function(data, palette = NULL, xlim = NULL, ylim = NULL,
                                 zlim = NULL, main = "", unit = "cm",
                                 line.h = 3, line.v = 3, hadj = 0, cex = 1.25,
                                 contour.labcex = 1) {

  # error handling

  if (!is.list(data)) stop("Input 'data' must be a list.", call. = FALSE)

  nm <- c("advection", "sigma", "compression", "optimum", "RMSD")
  if (any(is.na(match(nm, names(data))))) {
    stop("Input 'data' structure lacks required elements.", call. = FALSE)
  }

  if (length(data$optimum) != 4) {
    stop("Element 'optimum' must have length 4.", call. = FALSE)
  }

  nm <- c("rmsd", "advection", "sigma", "compression")
  if (any(is.na(match(nm, names(data$optimum))))) {
    stop("Missing required elements for 'optimum'.", call. = FALSE)
  }

  is.dim <- sapply(list(data$advection, data$sigma, data$compression), length)
  if (!identical(dim(data$RMSD), is.dim)) {
    stop("Dimensions of 'RMSD' array do not match ",
         "number of modification parameters.", call. = FALSE)
  }

  # project RMSD data onto surface of optimal advection values

  opt.adv.surface <- apply(data$RMSD, c(2, 3), which.min) %>%
    apply(c(1, 2), function(i) {data$advection[i]})

  RMSD.opt.adv.surface <- apply(data$RMSD, c(2, 3), min)  

  # make filled contour plot

  x <- data$sigma
  y <- data$compression
  z <- opt.adv.surface

  xlab <- sprintf("Differential diffusion length (%s)", unit)
  ylab <- sprintf("Compression (%s)", unit)
  zlab <- sprintf("Optimal downward advection (%s)", unit)

  if (is.null(xlim)) xlim <- range(x, finite = TRUE)
  if (is.null(ylim)) ylim <- range(y, finite = TRUE)
  if (is.null(zlim)) zlim <- range(data$advection, finite = TRUE)

  if (!length(palette)) {
    palette <- grfxtools::ColorPal("RdYlBu", 10, rev = TRUE, fun = TRUE)
  }

  minimum <- as.list(data$optimum[c("sigma", "compression")]) %>%
    setNames(c("x", "y"))

  graphics::filled.contour(x, y, z, xlim = xlim, ylim = ylim, zlim = zlim,
                           color.palette = palette,
                 plot.title = {
                   title(main = main);
                   mtext(xlab, side = 1, line = line.h,
                         cex = par()$cex.lab, font = par()$font.lab);
                   mtext(ylab, side = 2, line = line.v,
                         cex = par()$cex.lab, font = par()$font.lab, las = 0);
                   contour(data$sigma, data$compression, RMSD.opt.adv.surface,
                           add = TRUE, labcex = contour.labcex);
                   points(minimum, pch = 21, col = "black",
                          bg = "black", cex = cex)}
                 )
  
  # place colour legend label

  xpos <- 0.99 + hadj
  ypos <- 0.50
  srt  <- -90

  op <- par(usr = c(0, 1, 0, 1), xlog = FALSE, ylog = FALSE)
  text(xpos, ypos, labels = zlab, srt = srt, xpd = NA,
       cex = par()$cex.lab, font = par()$font.lab)
  par(op)

}
