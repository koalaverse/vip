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
#' @param geom Character string specifying which type of plot to construct.
#' The currently available options are described below.
#'
#'  \itemize{
#'
#'  \item \code{geom = "col"} uses \code{\link[ggplot2]{geom_col}} to construct
#'  a bar chart of the variable importance scores.
#'
#'  \item \code{geom = "point"} uses \code{\link[ggplot2]{geom_point}} to
#'  construct a Cleveland dot plot of the variable importance scores.
#'
#'  \item \code{geom = "boxplot"} uses \code{\link[ggplot2]{geom_boxplot}} to
#'  construct a boxplot plot of the variable importance scores. This option can
#'  only for the permutation-based importance method with \code{nsim > 1} and
#'  \code{keep = TRUE}; see \code{\link{vi_permute}} for details.
#'
#'  \item \code{geom = "violin"} uses \code{\link[ggplot2]{geom_violin}} to
#'  construct a violin plot of the variable importance scores. This option can
#'  only for the permutation-based importance method with \code{nsim > 1} and
#'  \code{keep = TRUE}; see \code{\link{vi_permute}} for details.
#'
#'  }
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. See example usage below.
#'
#' @param aesthetics List specifying additional arguments passed on to
#' \code{\link[ggplot2]{layer}}. These are often aesthetics, used to set an
#' aesthetic to a fixed value, like \code{colour = "red"} or \code{size = 3}.
#' See example usage below.
#'
#' @param horizontal Logical indicating whether or not to plot the importance
#' scores on the x-axis (\code{TRUE}). Default is \code{TRUE}.
#'
#' @param all_permutations Logical indicating whether or not to plot all
#' permutation scores along with the average. Default is \code{FALSE}. (Only
#' used for permutation scores when \code{nsim > 1}.)
#'
#' @param jitter Logical indicating whether or not to jitter the raw permutation
#' scores. Default is \code{FALSE}. (Only used when
#' \code{all_permutations = TRUE}.)
#'
#' @param include_type Logical indicating whether or not to include the type of
#' variable importance computed in the axis label. Default is \code{FALSE}.
#'
#' @param ... Additional optional arguments to be passed on to \code{\link{vi}}.
#'
#' @param bar Logical indicating whether or not to produce a barplot. Default is
#' \code{NULL}. \strong{WARNING:} This argument has been deprecated in favor of
#' the new \code{mapping} and \code{aesthetics} arguments. It will be removed in
#' version 0.3.0.
#'
#' @param width Numeric value specifying the width of the bars when
#' \code{bar = TRUE}. Default is \code{NULL}. \strong{WARNING:} This argument
#' has been deprecated in favor of the new \code{mapping} and \code{aesthetics}
#' arguments. It will be removed in version 0.3.0.
#'
#' @param alpha Numeric value between 0 and 1 giving the transparency of the
#' bars (\code{bar = TRUE}) or points (\code{bar = FALSE}). \strong{WARNING:}
#' This argument has been deprecated in favor of the new \code{mapping} and
#' \code{aesthetics} arguments. It will be removed in version 0.3.0.
#'
#' @param color Character string specifying the color to use for the borders of
#' the bars. Could also be a function, such as
#' \code{\link[grDevices]{heat.colors}}. Default is \code{NULL}.
#' \strong{WARNING:} This argument has been deprecated in favor of the new
#' \code{mapping} and \code{aesthetics} arguments. It will be removed in version
#' 0.3.0.
#'
#' @param fill Character string specifying the color to use to fill the bars.
#' Could also be a function, such as \code{\link[grDevices]{heat.colors}}.
#' Default is \code{NULL}. \strong{WARNING:} This argument has been deprecated
#' in favor of the new \code{mapping} and \code{aesthetics} arguments. It will be
#' removed in version 0.3.0.
#'
#' @param size Numeric value indicating the size to use for the points whenever
#' \code{bar = FALSE}. Default is \code{NULL}. \strong{WARNING:} This argument
#' has been deprecated in favor of the new \code{mapping} and \code{aesthetics}
#' arguments. It will be removed in version 0.3.0.
#'
#' @param shape Numeric value indicating the shape to use for the points
#' whenever \code{bar = FALSE}. Default is \code{NULL}. \strong{WARNING:} This
#' argument has been deprecated in favor of the new \code{mapping} and
#' \code{aesthetics} arguments. It will be removed in version 0.3.0.
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
#' vip(vi_scores, geom = "point", horiz = FALSE)
#' vip(vi_scores, geom = "point", horiz = FALSE, aesthetics = list(size = 3))
#'
#' # The `%T>\%` operator is imported for convenience; see ?magrittr::`%T>%`
#' # for details
#' vi_scores <- model %>%
#'   vi(method = "ice") %T>%
#'   {print(vip(.))}
#' vi_scores
#'
#' # Permutation scores (barplot w/ raw values and jittering)
#' pfun <- function(object, newdata) predict(object, newdata = newdata)
#' vip(model, method = "permute", train = mtcars, target = "mpg", nsim = 10,
#'     metric = "rmse", pred_wrapper = pfun,
#'     aesthetics = list(color = "grey50", fill = "grey50"),
#'     all_permutations = TRUE, jitter = TRUE)
#'
#' # Permutation scores (boxplot)
#' vip(model, method = "permute", train = mtcars, target = "mpg", nsim = 10,
#'     metric = "rmse", pred_wrapper = pfun, geom = "boxplot")
#'
#' # Permutation scores (boxplot colored by feature)
#' library(ggplot2)  # for `aes_string()` function
#' vip(model, method = "permute", train = mtcars, target = "mpg", nsim = 10,
#'     metric = "rmse", pred_wrapper = pfun, geom = "boxplot",
#'     all_permutations = TRUE, mapping = aes_string(fill = "Variable"),
#'     aesthetics = list(color = "grey35", size = 0.8))
vip <- function(object, ...) {
  UseMethod("vip")
}


#' @rdname vip
#'
#' @export
vip.default <- function(
  object,
  num_features = 10L,
  geom = c("col", "point", "boxplot", "violin"),
  mapping = NULL,
  aesthetics = list(),
  horizontal = TRUE,
  all_permutations = FALSE,
  jitter = FALSE,
  include_type = FALSE,
  ...,
  bar = NULL,    # deprecated
  width = NULL,  # deprecated
  alpha = NULL,  # deprecated
  color = NULL,  # deprecated
  fill = NULL,   # deprecated
  size = NULL,   # deprecated
  shape = NULL   # deprecated
) {

  # Deal with deprecated arguments
  if (!is.null(bar)) {
    warning("The `bar` argument has been deprecated in favor of the new ",
            "`geom` argument. It will be removed in version 0.3.0.")
    geom <- if (isTRUE(bar)) "col" else "point"
  } else {
    # Character string specifying which type of plot to construct
    geom <- match.arg(geom, several.ok = FALSE)
  }
  if (!(is.null(width) && is.null(alpha) && is.null(color) && is.null(fill) &&
        is.null(size) && is.null(shape))) {
    warning("Arguments `width`, `alpha`, `color`, `fill`, `size`, and `shape` ",
            "have all been deprecated in favor of the new `mapping` and ",
            "`aesthetics` arguments. They will be removed in version 0.3.0.")
    aesthetics <- list()
    if (!is.null(width)) {
      aesthetics <- c(aesthetics, list(width = width))
    }
    if (!is.null(alpha)) {
      aesthetics <- c(aesthetics, list(alpha = alpha))
    }
    if (!is.null(color)) {
      aesthetics <- c(aesthetics, list(color = color))
    }
    if (!is.null(fill)) {
      aesthetics <- c(aesthetics, list(fill = fill))
    }
    if (!is.null(size)) {
      aesthetics <- c(aesthetics, list(size = size))
    }
    if (!is.null(shape)) {
      aesthetics <- c(aesthetics, list(shape = shape))
    }
  }

  # Extract or compute importance scores
  imp <- if (inherits(object, what = "vi")) {
    object
  } else {
    vi(object = object, ...)  # compute variable importance scores
  }

  # Character string specifying the type of VI that was computed
  vi_type <- attr(imp, which = "type")  # subsetting removes this attribute!

  # Integer specifying the number of features to include in the plot
  num_features <- as.integer(num_features)[1L]  # make sure num_features is a single integer
  if (num_features > nrow(imp) || num_features < 1L) {
    num_features <- nrow(imp)
  }
  imp <- sort_importance_scores(imp, decreasing = TRUE)  # make sure these are sorted first!
  imp <- imp[seq_len(num_features), ]  # only retain num_features variable importance scores
  x.string <- "reorder(Variable, Importance)"

  # Clean up raw scores for permutation-based VI scores
  if (!is.null(attr(imp, which = "raw_scores"))) {
    raw_scores <- as.data.frame(attr(imp, which = "raw_scores"))
    raw_scores$Variable <- rownames(raw_scores)
    raw_scores <- stats::reshape(
      data = raw_scores,
      varying = (1L:(ncol(raw_scores) - 1)),
      v.names = "Importance",
      direction = "long",
      sep = "_"
    )
    raw_scores <- raw_scores[raw_scores$Variable %in% imp$Variable, ]
  }

  # Initialize plot
  p <- ggplot2::ggplot(imp, ggplot2::aes_string(x = x.string, y = "Importance"))

  # Construct a barplot
  if (geom == "col") {
    p <- p + do.call(
      what = ggplot2::geom_col,
      args = c(list(mapping = mapping), aesthetics)
    )
  }

  # Construct a (Cleveland) dotplot
  if (geom == "point") {
    p <- p + do.call(
      what = ggplot2::geom_point,
      args = c(list(mapping = mapping), aesthetics)
    )
  }

  # Construct a boxplot
  if (geom == "boxplot") {
    if (!is.null(attr(imp, which = "raw_scores"))) {
      p <- p + do.call(
        what = ggplot2::geom_boxplot,
        args = c(list(data = raw_scores, mapping = mapping), aesthetics)
      )
    } else {
      stop("To construct boxplots for permutation-based importance scores you ",
           "must specify `keep = TRUE` in the call `vi()` or `vi_permute()`. ",
           "Additionally, you also need to set `nsim >= 2`.",
           call. = FALSE)
    }
  }

  # Construct a violin plot
  if (geom == "violin") {
    if (!is.null(attr(imp, which = "raw_scores"))) {
      p <- p + do.call(
        what = ggplot2::geom_violin,
        args = c(list(data = raw_scores, mapping = mapping), aesthetics)
      )
    } else {
      stop("To construct violin plots for permutation-based importance scores ",
           "you must specify `keep = TRUE` in the call `vi()` or ",
           "`vi_permute()`. Additionally, you also need to set `nsim >= 2`.",
           call. = FALSE)
    }
  }

  # Plot raw permutation scores (if available and requested)
  if (!is.null(attr(imp, which = "raw_scores")) && all_permutations) {
    p <- if (jitter) {
      p + ggplot2::geom_jitter(data = raw_scores)
    } else {
      p + ggplot2::geom_point(data = raw_scores)
    }
  }

  # Add labels, titles, etc.
  p <- p + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::xlab("")  # no need for x-axis label
  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }
  if (isTRUE(include_type)) {
    p + ggplot2::ylab(paste0("Importance (", vi_type, ")"))
  } else {
    p + ggplot2::ylab("Importance")
  }

}


#' @rdname vip
#'
#' @export
vip.model_fit <- function(object, ...) {
  vip(object$fit, ...)
}

