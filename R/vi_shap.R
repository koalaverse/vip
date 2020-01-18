#' SHAP-based variable importance
#'
#' Compute SHAP-based VI scores for the predictors in a model. See details
#' below.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest. If \code{NULL} (the default) then the
#' internal `get_feature_names()` function will be called to try and extract
#' them automatically. It is good practice to always specify this argument.
#'
#' @param train A matrix-like R object (e.g., a data frame or matrix)
#' containing the training data. If \code{NULL} (the default) then the
#' internal `get_training_data()` function will be called to try and extract it
#' automatically. It is good practice to always specify this argument.
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{\link[fastshap]{explain}}.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns,
#' \code{Variable} and \code{Importance}, containing the variable name and its
#' associated importance score, respectively.
#'
#' @details This approach to computing VI scores is based on the mean absolute
#' value of the SHAP values for each feature; see, for example,
#' \url{https://github.com/slundberg/shap} and the references therein.
#'
#' Strumbelj, E., and Kononenko, I. Explaining prediction models and individual
#' predictions with feature contributions. Knowledge and information systems
#' 41.3 (2014): 647-665.
#'
#' @rdname vi_shap
#'
#' @export
vi_shap <- function(object, ...) {
  UseMethod("vi_shap")
}


#' @rdname vi_shap
#'
#' @export
vi_shap.default <- function(object, feature_names = NULL, train = NULL, ...) {

  # Check for fastshap package
  if (!requireNamespace("fastshap", quietly = TRUE)) {
    stop("Package \"fastshap\" needed for this function to work. ",
         "Please install it.", call. = FALSE)
  }

  # Try to extract feature names if not supplied
  if (is.null(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Try to extract training data if not supplied
  if (is.null(train)) {
    train <- get_training_data(object)
  }

  # Make sure only the feature columns are used (e.g., no response)
  train <- train[, feature_names, drop = FALSE]

  # Compute SHAP values
  shap <- fastshap::explain(
    object = object,
    feature_names = feature_names,
    X = train,
    ...
  )

  # Consruct SHAP-based variable importance scores
  tib <- tibble::tibble(
    "Variable" = names(shap),
    "Importance" = apply(shap, MARGIN = 2, FUN = function(x) mean(abs(x)))
  )
  attr(tib, which = "shap") <- shap
  attr(tib, which = "type") <- "mean(|Shapley value|)"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}
