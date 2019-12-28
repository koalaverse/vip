#' Extract model formula
#'
#' Calls \code{\link[stats]{formula}} to extract the formulae from various
#' modeling objects, but returns \code{NULL} instead of an error for objects
#' that do not contain a formula component.
#'
#' @param object An appropriate fitted model object.
#'
#' @return Either a \code{\link[stats]{formula}} object or \code{NULL}.
get_formula <- function(object) {
  UseMethod("get_formula")
}


#' @keywords internal
get_formula.default <- function(object) {
  form <- tryCatch(
    expr = stats::formula(object),
    error = function(e) {
      NULL
    }
  )
}


#' @keywords internal
get_formula.constparty <- function(object) {
  get_formula.default(attr(stats::terms(object), which = "Formula_without_dot"))
}


#' Extract feature names
#'
#' Extract predictor names from a fitted model.
#'
#' @param object An appropriate fitted model object.
#'
#' @param ... Additional optional arguments.
#'
#' @keywords internal
get_feature_names <- function(object, ...) {
  UseMethod("get_feature_names")
}


#' @keywords internal
get_feature_names.default <- function(object, ...) {
  stop("Could not extract feature names from ", deparse(substitute(object)),
       ", please supply them via the `feature_names` argument.", call. = FALSE)
}


#' @keywords internal
get_feature_names.formula <- function(object, ...) {
  tryCatch(  # FIXME: IS the RHS always located in the third component?
    expr = all.vars(object[[3L]]),  # extract unique vars from RHS side of formula
    error = function(e) {  # in case formula doesn't have both a LHS and RHS, etc.
      get_feature_names.default(object)
    }
  )
}


#' @keywords internal
get_feature_names.C5.0 <- function(object, ...) {
  object$predictors
}


#' @keywords internal
get_feature_names.constparty <- function(object, ...) {
  get_feature_names(get_formula(object))
}


#' @keywords internal
get_feature_names.earth <- function(object, ...) {
  object$namesx
}


#' @keywords internal
get_feature_names.gbm <- function(object, ...) {
  object$var.names
}


#' @keywords internal
get_feature_names.glmnet <- function(object, ...) {
  object$beta@Dimnames[[1]]
}


#' @keywords internal
get_feature_names.H2ORegressionModel <- function(object, ...) {
  object@parameters$x
}


#' @keywords internal
get_feature_names.lm <- function(object, ...) {
  get_feature_names(get_formula(object))
}


#' @keywords internal
get_feature_names.nls <- function(object, ...) {
  # all.vars(stats::formula(object)[[3L]])  # returns all params
  names(object$dataClasses)
}


#' @keywords internal
get_feature_names.nnet <- function(object, ...) {
  get_feature_names(get_formula(object))
}


#' @keywords internal
get_feature_names.ppr <- function(object, ...) {
  object$xnames
}


#' @keywords internal
get_feature_names.randomForest <- function(object, ...) {
  rownames(object$importance)
}


#' @keywords internal
get_feature_names.RandomForest <- function(object, ...) {
  all.vars(object@data@formula$input)
}


#' @keywords internal
get_feature_names.ranger <- function(object, ...) {
  if (!is.null(object$forest$independent.variable.names)) {
    object$forest$independent.variable.names
  } else if (!is.null(names(object$variable.importance))) {
    names(object$variable.importance)
  } else {
    stop("Unable to recover feature names from ranger models with `importance",
         " = \"none\"` and `write.forest = FALSE`.")
  }
}


#' @keywords internal
get_feature_names.rpart <- function(object, ...) {
  # names(object$variable.importance)
  get_feature_names(get_formula(object))
}


#' @keywords internal
get_feature_names.train <- function(object, ...) {
  if (!is.null(object$trainingData)) {
    xn <- names(object$trainingData)
    xn[xn != ".outcome"]
  } else {
    get_feature_names.default(object)
  }
}
