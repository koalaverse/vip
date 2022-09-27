#' Variable importance
#'
#' Compute variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object) or
#' an object that inherits from class \code{"vi"}.
#'
#' @param method Character string specifying the type of variable importance
#' (VI) to compute. Current options are \code{"model"} (the default), for
#' model-specific VI scores (see \code{\link{vi_model}} for details),
#' \code{"firm"}, for variance-based VI scores (see \code{\link{vi_firm}} for
#' details), \code{"permute"}, for permutation-based VI scores (see '
#' \code{\link{vi_permute}} for details), or \code{"shap"}, for Shapley-based
#' VI scores. For more details on the variance-based methods, see
#' \href{https://arxiv.org/abs/1805.04755}{Greenwell et al. (2018)} and
#' \href{https://arxiv.org/abs/1904.03959}{Scholbeck et al. (2019)}.
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest.
#'
#' @param FUN Deprecated. Use \code{var_fun} instead.
#'
#' @param var_fun List with two components, \code{"cat"} and \code{"con"},
#' containing the functions to use to quantify the variability of the feature
#' effects (e.g., partial dependence values) for categorical and continuous
#' features, respectively. If \code{NULL}, the standard deviation is used for
#' continuous features. For categorical features, the range statistic is used
#' (i.e., (max - min) / 4). Only applies when \code{method = "firm"}.
#'
#' @param ice Logical indicating whether or not to estimate feature effects
#' using \emph{individual conditional expectation} (ICE) curves.
#' Only applies when \code{method = "firm"}. Default is \code{FALSE}. Setting
#' \code{ice = TRUE} is preferred whenever strong interaction effects are
#' potentially present.
#'
#' @param abbreviate_feature_names Integer specifying the length at which to
#' abbreviate feature names. Default is \code{NULL} which results in no
#' abbreviation (i.e., the full name of each feature will be printed).
#'
#' @param sort Logical indicating whether or not to order the sort the variable
#' importance scores. Default is \code{TRUE}.
#'
#' @param decreasing Logical indicating whether or not the variable importance
#' scores should be sorted in descending (\code{TRUE}) or ascending
#' (\code{FALSE}) order of importance. Default is \code{TRUE}.
#'
#' @param scale Logical indicating whether or not to scale the variable
#' importance scores so that the largest is 100. Default is \code{FALSE}.
#'
#' @param rank Logical indicating whether or not to rank the variable
#' importance scores (i.e., convert to integer ranks). Default is \code{FALSE}.
#' Potentially useful when comparing variable importance scores across different
#' models using different methods.
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{\link{vi_model}}, \code{\link{vi_firm}}, \code{\link{vi_permute}},
#' or \code{\link{vi_shap}}.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with at least two
#' columns: \code{Variable} and \code{Importance}. For \code{"lm"/"glm"}-like
#' objects, an additional column, called \code{Sign}, is also included which
#' includes the sign (i.e., POS/NEG) of the original coefficient. If
#' \code{method = "permute"} and  \code{nsim > 1}, then an additional column,
#' \code{StDev}, giving the standard deviation of the permutation-based
#' variable importance scores is included.
#'
#' @references
#' Greenwell, B. M., Boehmke, B. C., and McCarthy, A. J. A Simple
#' and Effective Model-Based Variable Importance Measure. arXiv preprint
#' arXiv:1805.04755 (2018).
#'
#' @rdname vi
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
#' # Compute variable importance scores
#' vi(mtcars.ppr, method = "firm", ice = TRUE)
#' vi(mtcars.ppr, method = "firm", ice = TRUE,
#'    var_fun = list("con" = mad, "cat" = function(x) diff(range(x)) / 4))
#'
#' # Plot variable importance scores
#' vip(mtcars.ppr, method = "firm", ice = TRUE)
vi <- function(object, ...) {
  UseMethod("vi")
}


#' @rdname vi
#'
#' @export
vi.default <- function(
  object,
  method = c("model", "firm", "permute", "shap"),
  feature_names = NULL,
  FUN = NULL,  # deprecated
  var_fun = NULL,
  ice = FALSE,
  abbreviate_feature_names = NULL,
  sort = TRUE,
  decreasing = TRUE,
  scale = FALSE,
  rank = FALSE,
  ...
) {

  # Construct VI scores
  method <- match.arg(method)
  if (method == "firm") {
    if (is.null(feature_names)) {
      feature_names <- get_feature_names(object)
    }
  }

  # Catch deprecated arguments
  if (!is.null(FUN)) {
    stop("Argument `FUN` is deprecated; please use `var_fun` instead.",
         call. = FALSE)
  }
  if (method %in% c("pdp", "ice")) {
    stop("Methods \"pdp\" and \"ice\" are deprecated; use `method = \"firm\"` ",
         "instead. See `?vip::vi_firm` for details.", call. = FALSE)
  }

  # Construct tibble of VI scores
  tib <- switch(method,
    "model" = vi_model(object, ...),
    "firm" = vi_firm(object, feature_names = feature_names, var_fun = var_fun,
                     ice = ice, ...),
    "permute" = vi_permute(object, feature_names = feature_names, ...),
    vi_shap(object, feature_names = feature_names, ...)
  )

  # Save attribute
  vi_type <- attr(tib, which = "type")

  # Remove rows with NA
  tib <- stats::na.omit(tib)

  # Sort VI scores (if requested)
  if (sort) {
    tib <- sort_importance_scores(tib, decreasing = decreasing)
  }

  # Abbreviate feature names (if requested)
  if (!is.null(abbreviate_feature_names)) {
    tib <- abbreviate_names(tib, minlength = abbreviate_feature_names)
  }

  # Scale VI scores so that largest is 100
  if (scale) {
    tib$Importance <- tib$Importance / max(tib$Importance) * 100
  }

  # Rank VI scores (i.e., convert to integer ranks)
  if (rank) {
    tib$Importance <- rev(rank(tib$Importance, ties.method = "average"))
  }

  # Restore attribute
  attr(tib, which = "type") <- vi_type

  # Add "vi" class
  if (!inherits(tib, what = "vi")) {  # In case class gets stripped?
    class(tib) <- c("vi", class(tib))
  }

  # Return results
  tib

}
