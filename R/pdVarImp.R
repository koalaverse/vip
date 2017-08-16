#' Partial Dependence-based Variable Importance
#'
#' Compute a variable importance score for the predictor in a model using the
#' corresponding partial dependence function.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param pred.var Character string giving the names of the predictor variables
#' of interest. For reasons of computation/interpretation, this should include
#' no more than three variables.
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{\link[pdp]{partial}}.
#'
#' @export
pdVarImp <- function(object, pred.var, ...) {
  pd <- pdp::partial(object, pred.var = pred.var, ...)
  diff(range(pd$yhat))
}
