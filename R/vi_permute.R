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
#' @param pred_fun Optional prediction function that requires two arguments,
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
#' @param pos_class Character string specifying which category in `obs`
#' represents the "positive" class (i.e., the class for which the predicted
#' class probabilties correspond to). Only needed for binary classification
#' problems.
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
  # performance_fun,
  pred_fun = stats::predict,
  obs,
  metric = "auto",  # add log loss, auc, mae, mape, etc.
  pos_class = NULL,
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

  # Performance metric
  metric <- if (metric == "auto") {
    get_default_metric(object)
  } else {
    tolower(metric)
  }

  perf_fun <- switch(
    metric,
    # Classification
    "auc" = perf_auc,  # requires predicted class probabilities
    "error" = perf_ce,  # requires predicted class labels
    "logloss" = perf_logLoss,  # requires predicted class probabilities
    "mauc" = perf_mauc,  # requires predicted class probabilities
    "mlogloss" = perf_mlogLoss,  # requires predicted class probabilities
    # Regression
    "mse" = perf_mse,
    "r2" = perf_rsquared,
    "rsquared" = perf_rsquared,
    "rmse" = perf_rmse,
    stop("Metric \"", metric, "\" is not supported.")
  )

  # Is smaller better?
  smaller_is_better <- switch(
    metric,
    "auto" = TRUE,
    # Classification
    "auc" = FALSE,
    "error" = TRUE,
    "logloss" = TRUE,
    "mauc" = FALSE,
    "mlogloss" = TRUE,
    # Regression
    "mse" = TRUE,
    "r2" = FALSE,
    "rsquared" = FALSE,
    "rmse" = TRUE,
    stop("Metric \"", metric, "\" is not supported.")
  )

  # Get prediction function, if not supplied
  prob_based_metrics <- c("auc", "mauc", "logloss", "mlogloss")
  if (missing(pred_fun)) {
    type <- if (metric %in% prob_based_metrics) {
      "prob"
    } else {
      "raw"
    }
    pred_fun <- get_predictions(object, type = type)
  }

  # Determine reference class
  if (!is.null(pos_class) && metric %in% prob_based_metrics) {
    obs <- ifelse(obs == pos_class, yes = 1, no = 0)
  }

  # Compute baseline metric for comparison
  baseline <- perf_fun(
    actual = obs,
    predicted = pred_fun(object, newdata = train)
  )

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
      permuted <- perf_fun(
        actual = obs,
        predicted = pred_fun(object, newdata = copy)
      )
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
