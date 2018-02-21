#' Variable Importance
#'
#' Compute variable importance for a set of predictors using the partial
#' dependence-based approach.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables of interest.
#'
#' @param FUN Function used to measure the variability of the partial dependence
#' values for continuous predictors. If \code{NULL}, the standard deviation is
#' used (i.e., \code{FUN = sd}). For factors, the range statistic is used (i.e.,
#' (max - min) / 4).
#'
#' @param keep_partial Logical indicating whether or not to return the computed
#' partial dependence values for each predictor listed in \code{feature_names}
#' in a separate attribute called \code{"partial"}. Default is \code{FALSE}.
#'
#' @param ... Additional optional arguments to be passed onto
#' \code{\link{get_pd_vi_score}}.
#'
#' @keywords internal
get_pd_vi_scores <- function(object, feature_names, FUN = NULL,
                             keep_partial = FALSE, ...) {

  # Print warning message
  warning("Setting `partial = TRUE` is experimental, use at your own risk!",
          call. = FALSE)

  # Compute partial dependence-based variable importance scores
  importance_scores <- lapply(feature_names, function(x) {
    get_pd_vi_score(object, feature_names = x, FUN = FUN,
                    keep_partial = keep_partial, ...)
  })
  names(importance_scores) <- feature_names
  if (!is.null(attr(importance_scores[[1L]], "partial"))) {
    pd <- lapply(importance_scores, FUN = function(x) attr(x, "partial"))
    names(pd) <- feature_names
    importance_scores <- unlist(importance_scores)
    attr(importance_scores, "partial") <- pd
    importance_scores
  } else {
    unlist(importance_scores)
  }

}


#' Partial Dependence-based Variable Importance
#'
#' Compute a variable importance score for the predictor in a model using the
#' corresponding partial dependence function.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables of interest. For reasons of computation/interpretation, this should
#' include no more than three variables.
#'
#' @param FUN Function used to measure the variability of the partial dependence
#' values for continuous predictors. If \code{NULL}, the standard deviation is
#' used (i.e., \code{FUN = sd}). For factors, the range statistic is used (i.e.,
#' (max - min) / 4).
#'
#' @param keep_partial Logical indicating whether or not to return the computed
#' partial dependence value for the predictor listed in \code{feature_names} in
#' a separate attribute called \code{"partial"}. Default is \code{FALSE}.
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{\link[pdp]{partial}}.
#'
#' @export
get_pd_vi_score <- function(object, feature_names, FUN = NULL,
                            keep_partial = FALSE, ...) {

  # Variability importance function
  FUN <- if (is.null(FUN)) {  # function to use for continuous predictors
    stats::sd
  } else {
    match.fun(FUN)
  }

  # Compute partial dependence values
  pd <- pdp::partial(object, pred.var = feature_names, ...)

  # Compute partial dependence-based variable importance scores
  res <- if (is.factor(pd[[feature_names]])) {
    diff(range(pd[["yhat"]])) / 4
  } else {
    FUN(pd[["yhat"]])
  }
  if (keep_partial) {
    attr(res, "partial") <- pd
  }

  # Return result
  res

}
