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
metric_mse <- ModelMetrics::mse


#' @keywords internal
metric_rmse <- ModelMetrics::rmse


#' @keywords internal
metric_rsquared <- function(actual, predicted) {
  stats::cor(x = actual, y = predicted) ^ 2
}


# Classification ---------------------------------------------------------------

# Binary classification (i.e., 2 classes)

#' @keywords internal
metric_ce <- ModelMetrics::ce


#' @keywords internal
metric_auc <- function(actual, predicted) {
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
metric_logLoss <- function(actual, predicted) {
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
metric_mauc <- function(actual, predicted) {
  if (NCOL(predicted) <= 2L) {
    stop("Expected a >2 column matrix of predicted class probabilities.")
  }
  ModelMetrics::mauc(
    actual = actual,
    predicted = predicted
  )$mauc
}


#' @keywords internal
metric_mlogLoss <- function(actual, predicted) {
  if (NCOL(predicted) <= 2L) {
    stop("Expected a >2 column matrix of predicted class probabilities.")
  }
  ModelMetrics::mlogLoss(
    actual = actual,
    predicted = predicted
  )
}
