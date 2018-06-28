#' Permutation-Based Variable Importance
#'
#' Compute permutation-based variable importance scores for the predictors in a
#' model. (This function is meant for internal use only.)
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest.
#'
#' @param train Optional data frame containing the original training data.
#'
#' @param pfun Optional prediction function that requires two arguments,
#' \code{object} and \code{newdata}. If specified, then the function must return
#' a vector of predictions (i.e., not a matrix or data frame). (In the future,
#' this argument may become optional.)
#'
#' @param obs Non-optional vector containing the original (i.e., training)
#' response values.
#'
#' @param metric Non-optional function for computing model performance (e.g.,
#' RMSE for regression or accuracy for binary classification). This function
#' requires two arguments, \code{pred} (for predicted values) and \code{obs}
#' (for observed values), and should return a single, numeric value.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns:
#' \code{Variable} and \code{Importance}. For \code{"glm"}-like object, an
#' additional column, called \code{Sign}, is also included which gives the sign
#' (i.e., POS/NEG) of the original coefficient.
#'
#' @param progress Character string giving the name of the progress bar to use.
#' See \code{\link[plyr]{create_progress_bar}} for details. Default is
#' \code{"none"}.
#'
#' @param parallel Logical indicating whether or not to run \code{vi_permute()}
#' in parallel (using a backend provided by the \code{foreach} package). Default
#' is \code{FALSE}. If \code{TRUE}, an appropriate backend must be provided by
#' \code{foreach}.
#'
#' @param paropts List containing additional options to be passed onto
#' \code{foreach} when \code{parallel = TRUE}.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @details Coming soon!
#'
#' @rdname vi_permute
#'
#' @export
vi_permute <- function(object, ...) {
  UseMethod("vi_permute")
}


#' @rdname vi_permute
#'
#' @export
vi_permute.default <- function(
  object,
  feature_names,
  train,
  pfun,
  obs,
  metric = "auto",  # add log loss, auc, mae, mape, etc.
  progress = "none",
  parallel = FALSE,
  paropts = NULL,
  ...
) {

  # Issue warning until this function is complete!
  warning("Setting `method = \"permute\"` is experimental, use at your own ",
          "risk!", call. = FALSE)

  # Get training data, if not supplied
  if (missing(train)) {
    train <- get_training_data(object)
  }

  # Get prediction function, if not supplied
  if (missing(pfun)) {
    pfun <- get_predictions(object)
  }

  # Performance metric
  if (metric == "auto") {
    metric <- get_default_metric(object)
  }

  mfun <- switch(
    metric,
    # Classification
    "auc" = ModelMetrics::auc,  # requires predicted class probabilities
    "error" = ModelMetrics::ce,  # requires predicted class labels
    "logloss" = ModelMetrics::logLoss,  # requires predicted class probabilities
    # "mauc" = ModelMetrics::mauc,
    # "mlogloss" = ModelMetrics::mlogLoss,
    # Regression
    "MSE" = ModelMetrics::mse,
    "R2" = function(actual, predicted) stats::cor(actual, predicted)^2,
    "RMSE" = ModelMetrics::rmse,
    print("Metric not supported.")
  )

  # Is smaller better?
  smaller_is_better <- switch(
    metric,
    "auto" = TRUE,
    # Classification
    "auc" = FALSE,
    "error" = TRUE,
    "logloss" = TRUE,
    # "mauc" = FALSE,
    # "mlogloss" = TRUE,
    # Regression
    "MSE" = TRUE,
    "R2" = FALSE,
    "RMSE" = TRUE,
    print("Metric not supported.")
  )

  # Compute baseline metric for comparison
  baseline <- mfun(actual = obs, predicted = pfun(object, newdata = train))

  # Construct VI scores
  #
  # Loop through each feature and do the following:
  #
  #   1. make a copy of the training data;
  #   2. permute the values of the original feature;
  #   3. get new predictions based on permuted data set;
  #   4. record difference in accuracy.
  vis <- unlist(plyr::llply(feature_names, .progress = progress,
    .parallel = parallel, .paropts = paropts,
    .fun = function(x) {
      copy <- train  # make copy
      copy[[x]] <- sample(copy[[x]])  # permute values
      permuted <- mfun(actual = obs, predicted = pfun(object, newdata = copy))
      if (smaller_is_better) {
        permuted - baseline
      } else {
        baseline - permuted
      }
    })
  )
  tib <- tibble::tibble(
    "Variable" = feature_names,
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- "permutation"

  # Return results
  tib

}
