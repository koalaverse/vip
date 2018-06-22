#' @keywords internal
get_predictions <- function(object) {
  UseMethod("get_predictions")
}


#' @keywords internal
get_predictions.ranger <- function(object) {
  if (object$treetype == "Regression") {
    function(object, newdata) {
      stats::predict(object, data = newdata)$predictions
    }
  } else {
    stop("Only regression is currently supported for ranger objects.")
  }
}
