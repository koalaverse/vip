#' Variable Importance
#'
#' Compute variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param pred.var Character string giving the names of the predictor variables
#' of interest.
#'
#' @param use.partial Logical indicating whether or not to use partial
#' dependence functions to construct variable importance scores. Default is
#' \code{FALSE}.
#'
#' @param ... Additional optional arguments.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object).
#'
#' @rdname vi
#'
#' @export
vi <- function(object, ...) {
  UseMethod("vi")
}


#' @rdname vi
#'
#' @export
vi.default <- function(object, pred.var, ...) {
  # pred.var <- all.vars(stats::formula(object)[[3L]])
  imp <- sapply(pred.var, function(x) pdVarImp(object, pred.var = x, ...))
  pred.var <- pred.var[order(imp, decreasing = TRUE)]
  tibble::tibble("Variable" = pred.var,
                 "Importance" = sort(imp, decreasing = TRUE))
}


#' @rdname vi
#'
#' @export
vi.lm <- function(object, use.partial = FALSE, ...) {
  if (use.partial) {
    pred.var <- all.vars(stats::formula(object)[[3L]])
    imp <- sapply(pred.var, function(x) pdVarImp(object, pred.var = x, ...))
    pred.var <- pred.var[order(imp, decreasing = TRUE)]
    tibble::tibble("Variable" = pred.var,
                   "Importance" = sort(imp, decreasing = TRUE))
  } else {
    coefs <- summary(object)$coefficients
    if (attr(object$terms, "intercept") == 1) {
      coefs <- coefs[-1, ]
    }
    pred.var <- rownames(coefs)
    imp <- abs(coefs[, "t value"])
    pred.var <- pred.var[order(imp, decreasing = TRUE)]
    tibble::tibble("Variable" = pred.var,
                   "Importance" = sort(imp, decreasing = TRUE))
  }

}
