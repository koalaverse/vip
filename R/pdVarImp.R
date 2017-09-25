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
#' @param return.partial Logical indicating whether or not to return the partial
#' dependence output in an attribute called \code{"partial"}. Default is
#' \code{FALSE}.
#'
#' @param FUN Function used to measure the "flatness" of the partial dependence
#' function. If \code{NULL}, the standard deviation is used (i.e.,
#' \code{FUN = sd}).
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{\link[pdp]{partial}}.
#'
#' @export
pdVarImp <- function(object, pred.var, return.partial = FALSE, FUN = NULL, ...)
{
  FUN <- if (is.null(FUN)) {
    stats::sd
  } else {
    match.fun(FUN)
  }
  pd <- pdp::partial(object, pred.var = pred.var, ...)
  res <- FUN(pd$yhat)
  if (return.partial) {
    attr(res, "partial") <- pd
  }
  res
}
