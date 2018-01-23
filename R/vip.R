#' Variable Importance Plots
#'
#' Plot variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param bar Logical indicating whether or not to produce a barplot. Default
#' is \code{TRUE}. If \code{bar = FALSE}, then a dotchart is displayed instead.
#'
#' @param width Numeric value specifying the width of the bars when
#' \code{bar = TRUE}. Default is \code{0.75}.
#'
#' @param horizontal Logical indicating whether or not to plot the importance
#' scores on the x-axis (\code{TRUE}). Default is \code{TRUE}.
#'
#' @param alpha Numeric value between 0 and 1 giving the trasparency of the
#' bars.
#'
#' @param color Character string specifying the color to use for the borders of
#' the bars. Could also be a function, such as
#' \code{\link[grDevices]{heat.colors}}. Default is \code{"grey35"}.
#'
#' @param fill Character string specifying the color to use to fill the bars.
#' Could also be a function, such as \code{\link[grDevices]{heat.colors}}.
#' Default is \code{"grey35"}.
#'
#' @param ... Additional optional arguments to be passed onto \code{\link{vi}}.
#'
#' @rdname vip
#'
#' @export
#'
#' @examples
#' #
#' # A projection pursuit regression example
#' #
#'
#' # Load the sample data
#' data(mtcars)
#'
#' # Fit a projection pursuit regression model
#' mtcars.ppr <- ppr(mpg ~ ., data = mtcars, nterms = 1)
#'
#' # Construct variable importance plot
#' vip(mtcars.ppr, pred.var = names(subset(mtcars, select = -mpg)))
vip <- function(object, ...) {
  UseMethod("vip")
}


#' @rdname vip
#'
#' @export
vip.default <- function(object, bar = TRUE, width = 0.75, horizontal = TRUE,
                        alpha = 1, color = "grey35", fill = "grey35", ...) {
  imp <- vi(object, ...)  # variable importance scores
  x.string <- "reorder(Variable, Importance)"
  p <- ggplot2::ggplot(imp, ggplot2::aes_string(x = x.string, y = "Importance"))
  p <- if (bar) {
    p + ggplot2::geom_col(width = width, color = color,
                          fill = fill, alpha = alpha)
  } else {
    p + ggplot2::geom_point(color = color, alpha = alpha)
  }
  p <- p + ggplot2::xlab("")  # no need for x-axis label
  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }
  p + ggplot2::ylab(paste0("Importance (", attr(imp, "vi.type"), ")"))
}
