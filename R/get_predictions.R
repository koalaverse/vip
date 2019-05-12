#' @keywords internal
get_predictions <- function(object, type = c("raw", "prob")) {
  UseMethod("get_predictions")
}


#' @keywords internal
get_predictions.default <- function(object, type = c("raw", "prob")) {
  function(object, newdata, ...) {
    stats::predict(object, newdata = newdata)
  }
}


#' @keywords internal
get_predictions.ranger <- function(object, type = c("raw", "prob")) {
  type <- match.arg(type)
  if (object$treetype %in% c("Classification", "Regression")) {
    if (type == "prob") {
      stop("Could not obtain predicted class probabilities. Try setting ",
           "`probability = TRUE` in the call to `ranger()`.")
    }
    function(object, newdata) {
      stats::predict(object, data = newdata)$predictions
    }
  } else if (object$treetype == "Probability estimation") {
    if (type != "prob") {
      stop("Could not obtain predicted class probabilities. Try setting ",
           "`probability = TRUE` in the call to `ranger()`.")
    }
    function(object, newdata) {
      stats::predict(object, data = newdata)$predictions
    }
  } else {
    stop("Only classification and regression are currently supported.")
  }
}
