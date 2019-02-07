#' Variable Importance Plots
#'
#' Plot variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object) or
#' an object that inherits from class \code{"vi"}.
#'
#' @param num_features Integer specifying the number of variable importance
#' scores to plot. Default is \code{10}.
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
#' @param alpha Numeric value between 0 and 1 giving the transparency of the
#' bars (\code{bar = TRUE}) or points (\code{bar = FALSE}).
#'
#' @param color Character string specifying the color to use for the borders of
#' the bars. Could also be a function, such as
#' \code{\link[grDevices]{heat.colors}}. Default is \code{"grey35"}.
#'
#' @param fill Character string specifying the color to use to fill the bars.
#' Could also be a function, such as \code{\link[grDevices]{heat.colors}}.
#' Default is \code{"grey35"}.
#'
#' @param size Numeric value indicating the size to use for the points whenever
#' \code{bar = FALSE}. Default is \code{1}.
#'
#' @param shape Numeric value indicating the shape to use for the points
#' whenever \code{bar = FALSE}. Default is \code{1}.
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
#' model <- ppr(mpg ~ ., data = mtcars, nterms = 1)
#'
#' # Construct variable importance plot
#' vip(model, method = "ice")
#'
#' # Better yet, store the variable importance scores and then plot
#' vi_scores <- vi(model, method = "ice")
#' vip(vi_scores, bar = FALSE, size = 3, horiz = FALSE)
#'
#' # The \code{\link[magrittr]{\%T>\%}} operator is imported for convenience
#' vi_scores <- model %>%
#'   vi(method = "ice") %T>%
#'   {print(vip(.))}
#' vi_scores
vip <- function(object, ...) {
  UseMethod("vip")
}


#' @rdname vip
#'
#' @export
vip.default <- function(
  object,
  num_features = 10L,
  bar = TRUE,
  width = 0.75,
  horizontal = TRUE,
  alpha = 1,
  color = "grey35",
  fill = "grey35",
  size = 1,
  shape = 19,
  ...
) {
  imp <- if (inherits(object, what = "vi")) {
    object
  } else {
    vi(object = object, ...)  # compute variable importance scores
  }
  vi_type <- attr(imp, which = "type")  # subsetting removes this attribute!
  num_features <- as.integer(num_features)[1L]  # make sure num_features is a single integer
  if (num_features > nrow(imp) || num_features < 1L) {
    num_features <- nrow(imp)
  }
  imp <- imp[seq_len(num_features), ]  # only retain num_features variable importance scores
  x.string <- "reorder(Variable, Importance)"
  p <- ggplot2::ggplot(imp, ggplot2::aes_string(x = x.string, y = "Importance"))
  p <- if (bar) {
    if ("Sign" %in% names(imp)) {
      p + ggplot2::geom_col(ggplot2::aes_string(color = "Sign", fill = "Sign"),
                            width = width, alpha = alpha
      )
    } else {
      p + ggplot2::geom_col(
        width = width, color = color, fill = fill, alpha = alpha
      )
    }
  } else {
    if ("Sign" %in% names(imp)) {
      p + ggplot2::geom_point(ggplot2::aes_string(color = "Sign"),
                              alpha = alpha, size = size, shape = shape
      )
    } else {
      p + ggplot2::geom_point(
        color = color, alpha = alpha, size = size, shape = shape
      )
    }
  }
  p <- p + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::xlab("")  # no need for x-axis label
  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }
  p + ggplot2::ylab(paste0("Importance (", vi_type, ")"))
}
