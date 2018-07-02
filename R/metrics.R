# Regression -------------------------------------------------------------------

#' @keywords internal
perf_mse <- ModelMetrics::mse


#' @keywords internal
perf_rmse <- ModelMetrics::rmse


#' @keywords internal
perf_rsquared <- function(actual, predicted) {
  stats::cor(actual, predicted)^2
}


# Classification ---------------------------------------------------------------

# Binary classification (i.e., 2 classes)

#' @keywords internal
perf_ce <- ModelMetrics::ce


#' @keywords internal
perf_auc <- function(actual, predicted) {
  if (ncol(predicted) != 2) {
    stop("Expected a 2 column matrix of predicted class probabilities.")
  }
  ModelMetrics::auc(actual = actual, predicted = predicted[, 1L, drop = TRUE])
}


#' @keywords internal
perf_logLoss <- function(actual, predicted) {
  if (ncol(predicted) != 2) {
    stop("Expected a 2 column matrix of predicted class probabilities.")
  }
  ModelMetrics::logLoss(actual = actual, predicted = predicted[, 1L, drop = TRUE])
}


# Multiclass classification (i.e., >2 classes)

#' @keywords internal
perf_mauc <- function(actual, predicted) {
  if (ncol(predicted) <= 2) {
    stop("Expected a >2 column matrix of predicted class probabilities.")
  }
  ModelMetrics::mauc(actual = actual, predicted = predicted)$mauc
}


#' @keywords internal
perf_mlogLoss <- function(actual, predicted) {
  if (ncol(predicted) <= 2) {
    stop("Expected a >2 column matrix of predicted class probabilities.")
  }
  ModelMetrics::mlogLoss(actual = actual, predicted = predicted)
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
  if (object$treetype == "Regression") {
    "rmse"
  } else if (object$treetype == "Classification") {
    "error"
  } else if (object$treetype == "Probability estimation") {
    "auc"
  } else {
    stop("No support for \"ranger\" objects of type \"", object$treetype, "\".")
  }
}
