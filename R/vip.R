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
#' @param all_permutations Logical indicating whether or not to plot all
#' permutation scores along with the average. Default is \code{FALSE}. (Only
#' used for permutation scores when \code{nsim > 1}.)
#'
#' @param jitter Logical indicating whether or not to jitter the raw permutation
#' scores. Default is \code{FALSE}. (Only used when
#' \code{all_permutations = TRUE}.)
#'
#' @param include_type Logical indicating whether or not to incude the iportance
#' type in the plot axis label. Default is \code{FALSE}.
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
#'
#' # Permutation scores
#' vip(model, method = "permute", train = mtcars, target = "mpg", nsim = 10,
#'     metric = "rmse", bar = FALSE, color = "red", size = 3,
#'     all_permutations = TRUE, jitter = FALSE)
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
  all_permutations = FALSE,
  jitter = FALSE,
  include_type = FALSE,
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

  # Construct plot
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

  # Plot raw permutation scores (if available and requested)
  if (!is.null(attr(imp, which = "raw_scores")) && all_permutations) {
    raw_scores <- as.data.frame(attr(imp, which = "raw_scores"))
    raw_scores$Variable <- rownames(raw_scores)
    raw_scores <- stats::reshape(
      data = raw_scores,
      varying = (1L:(ncol(raw_scores) - 1)),
      v.names = "Importance",
      direction = "long",
      sep = "_"
    )
    p <- if (jitter) {
      p + ggplot2::geom_jitter(
        data = raw_scores,
        alpha = alpha / 2,
        size = size / 1.5,
        shape = shape
      )
    } else {
      p + ggplot2::geom_point(
        data = raw_scores,
        alpha = alpha / 2,
        size = size / 1.5,
        shape = shape
      )
    }
  }

  # Add labels, titles, etc.
  p <- p + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::xlab("")  # no need for x-axis label
  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }
  if (include_type) {
    y_label <- paste0("Importance (", vi_type, ")")
  } else {
    y_label <- paste0("Importance")
  }
  p + ggplot2::ylab(y_label)

}


#' @rdname vip
#'
#' @export
vip.model_fit <- function(object, ...) {
  vip(object$fit, ...)
}

