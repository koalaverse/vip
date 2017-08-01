#' Variable Importance
#'
#' Compute variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object).
#'
#' @rdname vi
#'
#' @export
vi <- function(object) {
  UseMethod("vi")
}


#' @rdname vi
#'
#' @export
vi.lm <- function(object) {
  coefs <- summary(object)$coefficients
  if (attr(object$terms, "intercept") == 1) {
    coefs <- coefs[-1, ]
  }
  preds <- rownames(coefs)
  imp <- abs(coefs[, "t value"])
  preds <- preds[order(imp, decreasing = TRUE)]
  tibble::tibble("Variable" = preds,
                 "Importance" = sort(imp, decreasing = TRUE))
}
