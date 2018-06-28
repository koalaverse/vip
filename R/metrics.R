#' @keywords internal
rsquared <- function(actual, predicted) stats::cor(actual, predicted)^2

#' @keywords internal
get_default_metric <- function(object) {
  UseMethod("get_default_metric")
}


#' @keywords internal
get_default_metric.ranger <- function(object) {
  if (object$treetype == "Regression") {
    "RMSE"
  } else if (object$treetype == "Classification") {
    "error"
  } else if (object$treetype == "Probability estimation") {
    "auc"
  } else {
    stop("No support for \"ranger\" objects of type \"", object$treetype, "\".")
  }
}
