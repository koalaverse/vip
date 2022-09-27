#' Variable importance plots
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
#'  \item \code{geom = "col"} uses \code{\link[ggplot2:geom_bar]{geom_col}} to construct
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
#' @rdname vip
#'
#' @export
#'
#' @examples
#' #
#' # A projection pursuit regression example using permutation-based importance
#' #
#'
#' # Load the sample data
#' data(mtcars)
#'
#' # Fit a projection pursuit regression model
#' model <- ppr(mpg ~ ., data = mtcars, nterms = 1)
#'
#' # Construct variable importance plot (permutation importance, in this case)
#' set.seed(825)  # for reproducibility
#' pfun <- function(object, newdata) predict(object, newdata = newdata)
#' vip(model, method = "permute", train = mtcars, target = "mpg", nsim = 10,
#'     metric = "rmse", pred_wrapper = pfun)
#'
#' # Better yet, store the variable importance scores and then plot
#' set.seed(825)  # for reproducibility
#' vis <- vi(model, method = "permute", train = mtcars, target = "mpg",
#'           nsim = 10, metric = "rmse", pred_wrapper = pfun)
#' vip(vis, geom = "point", horiz = FALSE)
#' vip(vis, geom = "point", horiz = FALSE, aesthetics = list(size = 3))
#'
#' # The `%T>\%` operator is imported for convenience; see ?magrittr::`%T>%`
#' # for details
#' vis<- model %>%
#'   vi(method = "permute", train = mtcars, target = "mpg",
#'      nsim = 10, metric = "rmse", pred_wrapper = pfun) %T>%
#'   {print(vip(.))}
#' vis
#'
#' # Plot unaggregated permutation scores (boxplot colored by feature)
#' library(ggplot2)  # for `aes_string()` function
#' vip(vis, geom = "boxplot", all_permutations = TRUE, jitter = TRUE,
#'     mapping = aes_string(fill = "Variable"),
#'     aesthetics = list(color = "grey35", size = 0.8))
#'
#' #
#' # A binary classification example
#' #
#' \dontrun{
#' library(rpart)  # for classification and regression trees
#'
#' # Load Wisconsin breast cancer data; see ?mlbench::BreastCancer for details
#' data(BreastCancer, package = "mlbench")
#' bc <- subset(BreastCancer, select = -Id)  # for brevity
#'
#' # Fit a standard classification tree
#' set.seed(1032)  # for reproducibility
#' tree <- rpart(Class ~ ., data = bc, cp = 0)
#'
#' # Prune using 1-SE rule (e.g., use `plotcp(tree)` for guidance)
#' cp <- tree$cptable
#' cp <- cp[cp[, "nsplit"] == 2L, "CP"]
#' tree2 <- prune(tree, cp = cp)  # tree with three splits
#'
#' # Default tree-based VIP
#' vip(tree2)
#'
#' # Computing permutation importance requires a prediction wrapper. For
#' # classification, the return value depends on the chosen metric; see
#' # `?vip::vi_permute` for details.
#' pfun <- function(object, newdata) {
#'   # Need vector of predicted class probabilities when using  log-loss metric
#'   predict(object, newdata = newdata, type = "prob")[, "malignant"]
#' }
#'
#' # Permutation-based importance (note that only the predictors that show up
#' # in the final tree have non-zero importance)
#' set.seed(1046)  # for reproducibility
#' vip(tree2, method = "permute", nsim = 10, target = "Class",
#'     metric = "logloss", pred_wrapper = pfun, reference_class = "malignant")
#' }
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
  ...
) {

  # Character string specifying which type of plot to construct
  geom <- match.arg(geom, several.ok = FALSE)

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
  vip(parsnip::extract_fit_engine(object), ...)
}


#' @rdname vip
#'
#' @export
vip.workflow <- function(object, ...) {
  vip(workflows::extract_fit_engine(object), ...)
}

#' @rdname vip
#'
#' @export
vip.WrappedModel <- function(object, ...) {  # package: mlr
  vip(object$learner.model, ...)
}


#' @rdname vip
#'
#' @export
vip.Learner <- function(object, ...) {  # package: mlr3
  if (is.null(object$model)) {
    stop("No fitted model found. Did you forget to call ",
         deparse(substitute(object)), "$train()?",
         call. = FALSE)
  }
  vip(object$model, ...)
}
