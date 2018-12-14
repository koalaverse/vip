#' Variable Importance
#'
#' Compute variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object) or
#' an object that inherits from class \code{"vi"}. Can also be a a data frame or
#' a matrix of predictors, or a formula of the form \code{response ~ features}
#' (only applicable when \code{method = "ace"}).
#'
#' @param y A vector of response values. Only udes when \code{method = "ace"}.
#'
#' @param data an optional data frame, list or environment (or object coercible
#' by \code{as.data.frame} to a data frame) containing the variables in the
#' model. If not found in \code{data}, the variables are taken from
#' \code{environment(formula)}, typically the environment from which
#' \code{\link{vi_ace}} was called. Only udes when \code{method = "ace"}.
#'
#' @param method Character string specifying the type of variable importance
#' (VI) to compute. Current options are \code{"ace"}, for ACE-based VI scores
#' (see \code{\link{vi_ace}} for details); \code{"model"}, for model-based VI
#' scores (see \code{\link{vi_model}} for details), \code{"pdp"}, for PDP-based
#' VI scores (see \code{\link{vi_pdp}} for details), \code{"ice"}, for ICE-based
#' VI scores (see \code{\link{vi_ice}} for details), and \code{"permute"}, for
#' permutation-based VI scores (see \code{\link{vi_permute}} for details). The
#' default depends on the class of \code{object}. For more details on the
#' PDP/ICE-based methods, see the reference below.
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest.
#'
#' @param FUN List with two components, \code{"cat"} and \code{"con"},
#' containing the functions to use for categorical and continuous features,
#' respectively. If \code{NULL}, the standard deviation is used for continuous
#' features. For categorical features, the range statistic is used (i.e.,
#' (max - min) / 4).
#'
#' @param abbreviate_feature_names Integer specifying the length at which to
#' abbreviate feature names. Default is \code{NULL} which results in no abbreviation
#' (i.e., the full name of each feature will be printed).
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
#' @param ... Additional optional arguments.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns:
#' \code{Variable} and \code{Importance}. For \code{"lm"/"glm"}-like objects, an
#' additional column, called \code{Sign}, is also included which includes the
#' sign (i.e., POS/NEG) of the original coefficient.
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
#'
#' #
#' # Non-model based approach using ACE
#' #
#'
vi <- function(
  object, y, data,
  method = c("ace", "model", "pdp", "ice", "permute"), feature_names,
  FUN = NULL, abbreviate_feature_names = NULL, sort = TRUE, decreasing = TRUE,
  scale = FALSE, rank = FALSE, ...
) {

  # Construct VI scores
  method <- match.arg(method)
  if (method %in% c("pdp", "ice")) {
    if (missing(feature_names)) {
      feature_names <- get_feature_names(object)
    }
  }

  # Construct tibble of VI scores
  tib <- if (method == "ace") {
    if (inherits(object, what = "formula") && !missing(data)) {
      vi_ace(object, data = data, ...)
    } else if ((class(object) %in% c("matrix", "data.frame")) && !missing(y)) {
      vi_ace(object, y = y, ...)
    } else {
      stop("No formula or data provided.", call. = FALSE)
    }
  } else {
    switch(method,
      "model" = vi_model(object, ...),
      "pdp" = vi_pdp(object, feature_names = feature_names, FUN = FUN, ...),
      "ice" = vi_ice(object, feature_names = feature_names, FUN = FUN, ...),
      vi_permute(object, feature_names = feature_names, ...)
    )
  }

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
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}

