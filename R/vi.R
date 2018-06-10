#' Variable Importance
#'
#' Compute variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param method Character string specifying the type of variable importance
#' (VI) to compute. Current options are \code{"model"} (for model-based VI
#' scores), \code{"pdp"} (for PDP-based VI scores), \code{"ice"} (for ICE-based
#' VI scores), and \code{"perm"} (for permutation-based VI scores). The default
#' is \code{"model"}. For details on the PDP/ICE-based method, see the reference
#' below. Also, \code{method = "perm"} is currently ignored, but will be
#' available in a future release.
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest.
#'
#' @param truncate_feature_names Integer specifying the length at which to
#' truncate feature names. Default is \code{NULL} which results in no truncation
#' (i.e., the full name of each feature will be printed).
#'
#' @param sort Logical indicating whether or not to order the sort the variable
#' importance scores. Default is \code{TRUE}.
#'
#' @param decreasing Logical indicating whether or not the variable importance
#' scores should be sorted in descending (\code{TRUE}) or ascending
#' (\code{FALSE}) order of importance. Default is \code{TRUE}.
#'
#' @param FUN List with two componenets, \code{"cat"} and \code{"con"},
#' containing the functions to use for categorical and continuous features,
#' respectively. If \code{NULL}, the standard deviation is used for continuous
#' features. For categorical features, the range statistic is used (i.e.,
#' (max - min) / 4).
#'
#' @param ... Additional optional arguments.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns:
#' \code{Variable} and \code{Importance}.
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
#' vi(mtcars.ppr, method = "ice")
#'
#' # Plot variable importance scores
#' vip(mtcars.ppr, method = "ice")
vi <- function(
  object, method = c("model", "pdp", "ice", "perm"), feature_names,
  truncate_feature_names = NULL, sort = TRUE, decreasing = TRUE, ...
) {

  # Construct variable importance scores
  method <- match.arg(method)
  if (method %in% c("pdp", "ice")) {
    if (missing(feature_names)) {
      feature_names <- get_feature_names(object)
    }
  }
  tib <- if (method == "model") {
    vi_model(object, ...)
  } else if (method == "pdp") {
    vi_pdp(object, feature_names = feature_names, ...)
  } else if (method == "ice") {
    vi_ice(object, feature_names = feature_names, ...)
  } else {
    stop("Permutation-based variable importance scores not yet implemented.",
         call. = FALSE)
  }

  # Sort variable importance scores (if requested)
  if (sort) {
    tib <- sort_importance_scores(tib, decreasing = decreasing)
  }

  # Truncate feature names (if requested)
  if (!is.null(truncate_feature_names)) {
    tib <- truncate_feature_names(tib, length = truncate_feature_names)
  }

  # Return results
  tib

}

