#' Variable Importance
#'
#' Compute variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param pred.var Character string giving the names of the predictor variables
#' of interest.
#'
#' @param type Integer specifying the type of variable importance measure to
#' return for \code{\link[randomForest]{randomForest}} objects. Should be
#' \code{0} or \code{1}; see \code{\link[randomForest]{importance}} for details.
#'
#' @param partial Logical indicating whether or not to use partial dependence
#' functions to construct variable importance scores. Default is \code{FALSE}.
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
#' @param ... Additional optional arguments.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns:
#' \code{Variable} and \code{Importance}.
#'
#' @rdname vi
#'
#' @export
#'
#' @examples
#' #
#' # A projection pursuit regression example
#' #
#'
#' # Load the sample data
#' data(mtcars)
#'
#' # Fit a projection pursuit regression model
#' mtcars.ppr <- ppr(mpg ~ ., data = mtcars, nterms = 1)
#'
#' # Compute variable importance scores
#' vi(mtcars.ppr, pred.var = names(subset(mtcars, select = -mpg)))
vi <- function(object, ...) {
  UseMethod("vi")
}


#' @rdname vi
#'
#' @export
vi.default <- function(object, pred.var, FUN = NULL, keep.partial = FALSE, ...)
{
  imp <- get_pd_vi_scores(object, pred.var = pred.var, FUN = FUN,
                          keep.partial = keep.partial, ...)
  pred.var <- pred.var[order(imp, decreasing = TRUE)]
  res <- tibble::tibble("Variable" = pred.var,
                        "Importance" = sort(imp, decreasing = TRUE))
  class(res) <- c("vi", class(res))
  attr(res, "partial") <- attr(imp, "partial")
  res
}


#' @rdname vi
#'
#' @export
vi.gbm <- function(object, pred.var, partial = FALSE, FUN = NULL,
                   keep.partial = FALSE, ...) {
  if(missing(pred.var)) {
    pred.var <- object$var.names
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, pred.var = pred.var, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    vi.type <- "rel.inf"
    imp <- summary(object, plotit = FALSE, order = TRUE, ...)
    rownames(imp) <- NULL
    names(imp) <- c("Variable", "Importance")
    out <- tibble::as.tibble(imp)
    out[out$Variable %in% pred.var, ]
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.lm <- function(object, pred.var, partial = FALSE, FUN = NULL,
                  keep.partial = FALSE, ...) {
  if(missing(pred.var)) {
    pred.var <- all.vars(stats::formula(object)[[3L]])
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, pred.var = pred.var, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    vi.type <- "tstat"
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
  attr(tib, "vi.type") <- vi.type
  tib
}


#' #' @rdname vi
#' #'
#' #' @export
#' vi.earth <- function(object, pred.var, partial = FALSE, FUN = NULL,
#'                      keep.partial = FALSE, ...) {
#'   if(missing(pred.var)) {
#'     pred.var <- all.vars(stats::formula(object)[[3L]])
#'   }
#'   tib <- if (partial) {
#'     vi.type <- "partial"
#'     vi.default(object, pred.var = pred.var, FUN = NULL,
#'                keep.partial = keep.partial, ...)
#'   } else {
#'     vi.type <- "tstat"
#'     coefs <- summary(object)$coefficients
#'     if (attr(object$terms, "intercept") == 1) {
#'       coefs <- coefs[-1, ]
#'     }
#'     pred.var <- rownames(coefs)
#'     imp <- abs(coefs[, "t value"])
#'     pred.var <- pred.var[order(imp, decreasing = TRUE)]
#'     tibble::tibble("Variable" = pred.var,
#'                    "Importance" = sort(imp, decreasing = TRUE))
#'   }
#'   attr(tib, "vi.type") <- vi.type
#'   tib
#' }


#' @rdname vi
#'
#' @export
vi.randomForest <- function(object, pred.var, type = 1, partial = FALSE,
                            FUN = NULL, keep.partial = FALSE, ...) {
  if(missing(pred.var)) {
    pred.var <- rownames(object$importance)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, pred.var = pred.var, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    imp <- randomForest::importance(object, type = type, ...)
    if (dim(imp)[2L] == 0) {  # possible when importance = FALSE in RF call
      imp <- object$importance
    }
    vi.type <- colnames(imp)[1L]
    all.pred.var <- rownames(imp)
    imp <- imp[, 1L]
    all.pred.var <- all.pred.var[order(imp, decreasing = TRUE)]
    out <- tibble::tibble("Variable" = all.pred.var,
                          "Importance" = sort(imp, decreasing = TRUE))
    out[out$Variable %in% pred.var, ]
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


# #' @keywords internal
# #' @export
# print.vi <- function(x, ...) {
#   attributes(x) <- NULL
#   print(x, ...)
# }
