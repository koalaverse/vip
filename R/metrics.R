#' List metrics
#'
#' List all available performance metrics.
#'
#' @export
#'
#' @examples
#' (metrics <- list_metrics())
#' metrics[metrics$Task == "Multiclass classification", ]
list_metrics <- function() {
  data.frame(
    Metric = c("auc", "error", "logloss", "mauc", "mlogloss", "mse", "r2",
               "rsquared", "rmse"),
    Description = c("Area under (ROC) curve", "Misclassification error",
                    "Log loss", "Multiclass area under (ROC) curve",
                    "Multiclass log loss", "Mean squared error", "R squared",
                    "R squared", "Root mean squared error"),
    Task = c("Binary classification", "Binary classification",
             "Binary classification", "Multiclass classification",
             "Multiclass classification", "Regression", "Regression",
             "Regression", "Regression")
  )
}


# Regression -------------------------------------------------------------------

#' @keywords internal
perf_mse <- ModelMetrics::mse


#' @keywords internal
perf_rmse <- ModelMetrics::rmse


#' @keywords internal
perf_rsquared <- function(actual, predicted) {
  stats::cor(x = actual, y = predicted) ^ 2
}


# Classification ---------------------------------------------------------------

# Binary classification (i.e., 2 classes)

#' @keywords internal
perf_ce <- ModelMetrics::ce


#' @keywords internal
perf_auc <- function(actual, predicted) {
  # if (NCOL(predicted) != 2L) {
  #   stop("Expected a 2 column matrix of predicted class probabilities.")
  # }
  ModelMetrics::auc(
    actual = actual,
    # predicted = predicted[, 1L, drop = TRUE]
    predicted = predicted
  )
}


#' @keywords internal
perf_logLoss <- function(actual, predicted) {
  # if (NCOL(predicted) != 2L) {
  #   stop("Expected a 2 column matrix of predicted class probabilities.")
  # }
  ModelMetrics::logLoss(
    actual = actual,
    # predicted = predicted[, 1L, drop = TRUE]
    predicted = predicted
  )
}


# Multiclass classification (i.e., >2 classes)

#' @keywords internal
perf_mauc <- function(actual, predicted) {
  if (NCOL(predicted) <= 2L) {
    stop("Expected a >2 column matrix of predicted class probabilities.")
  }
  ModelMetrics::mauc(
    actual = actual,
    predicted = predicted
  )$mauc
}


#' @keywords internal
perf_mlogLoss <- function(actual, predicted) {
  if (NCOL(predicted) <= 2L) {
    stop("Expected a >2 column matrix of predicted class probabilities.")
  }
  ModelMetrics::mlogLoss(
    actual = actual,
    predicted = predicted
  )
}


# Auto -------------------------------------------------------------------------

#' @keywords internal
get_default_metric <- function(object) {
  UseMethod("get_default_metric")
}


#' @keywords internal
get_default_metric.ranger <- function(object) {
  "rmse"
}


#' @keywords internal
get_default_metric.ranger <- function(object) {
  tree_type <- object$treetype
  switch(tree_type,
         "Regression" = "rmse",
         "Classification" = "error",
         "Probability estimation" = "auc",
         stop("No support for \"ranger\" objects of type \"", tree_type, "\"."))
}
