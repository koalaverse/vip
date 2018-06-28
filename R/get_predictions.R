#' @keywords internal
get_predictions <- function(object) {
  UseMethod("get_predictions")
}


#' @keywords internal
get_predictions.ranger <- function(object) {
  if (object$treetype %in% c("Classification", "Regression")) {
    function(object, newdata) {
      stats::predict(object, data = newdata)$predictions
    }
  } else if (object$treetype == "Probability estimation") {
    function(object, newdata) {
      stats::predict(object, data = newdata)$predictions[, 1L]
    }
  } else {
    stop("Only classification and regression are currently supported.")
  }
}
