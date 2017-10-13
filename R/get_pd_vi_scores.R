#' Variable Importance
#'
#' Compute variable importance for a set of predictors using the partial
#' dependence-based approach.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param pred.var Character string giving the names of the predictor variables
#' of interest.
#'
#' @param FUN Function used to measure the variability of the partial dependence
#' values for continuous predictors. If \code{NULL}, the standard deviation is
#' used (i.e., \code{FUN = sd}). For factors, the range statistic is used (i.e.,
#' (max - min) / 4).
#'
#' @param keep.partial Logical indicating whether or not to return the computed
#' partial dependence values for each predictor listed in \code{pred.var} in a
#' separate attribute called \code{"partial"}. Default is \code{FALSE}.
#'
#' @param ... Additional optional arguments to be passed onto
#' \code{\link{get_pd_vi_score}}.
#'
#' @keywords internal
get_pd_vi_scores <- function(object, pred.var, FUN = NULL, keep.partial = FALSE,
                             ...) {
  imp <- lapply(pred.var, function(x) {
    get_pd_vi_score(object, pred.var = x, FUN = FUN,
                    keep.partial = keep.partial, ...)
  })
  names(imp) <- pred.var
  if (!is.null(attr(imp[[1L]], "partial"))) {
    pd <- lapply(imp, FUN = function(x) attr(x, "partial"))
    names(pd) <- pred.var
    imp <- unlist(imp)
    attr(imp, "partial") <- pd
    imp
  } else {
    unlist(imp)
  }
}


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
#' @param FUN Function used to measure the variability of the partial dependence
#' values for continuous predictors. If \code{NULL}, the standard deviation is
#' used (i.e., \code{FUN = sd}). For factors, the range statistic is used (i.e.,
#' (max - min) / 4).
#'
#' @param keep.partial Logical indicating whether or not to return the computed
#' partial dependence value for the predictor listed in \code{pred.var} in a
#' separate attribute called \code{"partial"}. Default is \code{FALSE}.
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{\link[pdp]{partial}}.
#'
#' @export
get_pd_vi_score <- function(object, pred.var, FUN = NULL, keep.partial = FALSE,
                            ...) {
  FUN <- if (is.null(FUN)) {  # function to use for continuous predictors
    stats::sd
  } else {
    match.fun(FUN)
  }
  pd <- pdp::partial(object, pred.var = pred.var, ...)
  res <- if (is.factor(pd[[pred.var]])) {
    diff(range(pd[["yhat"]])) / 4
  } else {
    FUN(pd[["yhat"]])
  }
  if (keep.partial) {
    attr(res, "partial") <- pd
  }
  res
}
