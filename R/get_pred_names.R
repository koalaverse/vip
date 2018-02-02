#' Extract Predictor Names
#'
#' Extract predictor names from a fitted model.
#'
#' @param object An appropriate fitted model object.
#'
#' @param ... Additional optional arguments.
#'
#' @keywords internal
get_pred_names <- function(object, ...) {
  UseMethod("get_pred_names")
}


#' @keywords internal
get_pred_names.default <- function(object, ...) {
  stop("Could not extract predcitor names from ", deparse(substitute(object)),
       ". Please supply the names of the predictors via the `pred.var` ",
       "argument.")
}


#' @keywords internal
get_pred_names.earth <- function(object, ...) {
  object$namesx
}


#' @keywords internal
get_pred_names.gbm <- function(object, ...) {
  object$var.names
}


#' @keywords internal
get_pred_names.H2ORegressionModel <- function(object, ...) {
  object@parameters$x
}


#' @keywords internal
get_pred_names.lm <- function(object, ...) {
  all.vars(stats::formula(object)[[3L]])
}


#' @keywords internal
get_pred_names.nls <- function(object, ...) {
  all.vars(stats::formula(object)[[3L]])
}


#' @keywords internal
get_pred_names.randomForest <- function(object, ...) {
  rownames(object$importance)
}


#' @keywords internal
get_pred_names.RandomForest <- function(object, ...) {
  all.vars(object@data@formula$input)
}


#' @keywords internal
get_pred_names.ranger <- function(object, ...) {
  names(object$variable.importance)
}


#' @keywords internal
get_pred_names.rpart <- function(object, ...) {
  names(object$variable.importance)
}


#' @keywords internal
get_pred_names.train <- function(object, ...) {
  if (!is.null(object$trainingData)) {
    xn <- names(object$trainingData)
    xn[xn != ".outcome"]
  } else {
    get_pred_names.default(object)
  }
}
