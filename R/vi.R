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
vi.earth <- function(object, pred.var, partial = FALSE, FUN = NULL,
                     keep.partial = FALSE, ...) {
  if (missing(pred.var)) {
    pred.var <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, pred.var = pred.var, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    vi.type <- "earth"
    imp <- earth::evimp(object, trim = FALSE, ...)  # return all importance scores
    imp <- unclass(imp)[, c("nsubsets", "gcv", "rss")]
    imp <- cbind("Variable" = rownames(imp), as.data.frame(as.matrix(imp)))
    rownames(imp) <- NULL
    tibble::as.tibble(imp)
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.gbm <- function(object, pred.var, partial = FALSE, FUN = NULL,
                   keep.partial = FALSE, ...) {
  if (missing(pred.var)) {
    pred.var <- get_pred_names(object)
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
vi.H2ORegressionModel <- function(object, pred.var, partial = FALSE, FUN = NULL,
                                  keep.partial = FALSE, ...) {
  if (missing(pred.var)) {
    pred.var <- get_pred_names(object)
  }
  tib <- if (partial) {
    # stop("`method = \"partial\" is currently not supported.")
    vi.type <- "partial"
    pd_list <- h2o::h2o.partialPlot(object, cols = pred.var, plot = FALSE,
                                    plot_stddev = FALSE, ...)
    imp <- if (is.null(FUN)) {
      unlist(lapply(pd_list, FUN = function(x) {
        if (is.numeric(x[[1L]])) {
          sd(x[["mean_response"]])
        } else {
          diff(range(x[["mean_response"]])) / 4
        }
      }))
    } else {
      unlist(lapply(pd_list, FUN = function(x) {
        FUN(x[["mean_response"]])
      }))
    }
    names(imp) <- pred.var
    imp <- sort(imp, decreasing = TRUE)
    tibble::tibble("Variable" = names(imp), "Importance" = imp)
  } else {
    vi.type <- "rel.inf"
    imp <- tibble::as.tibble(h2o::h2o.varimp(object))
    imp[3L:4L] <- NULL
    names(imp) <- c("Variable", "Importance")
    imp[imp$Variable %in% pred.var, ]
  }
  attr(tib, "vi.type") <- vi.type
  if (partial && keep.partial) {
    attr(tib, "partial") <- pd_list
  }
  tib
}


#' @rdname vi
#'
#' @export
vi.lm <- function(object, pred.var, partial = FALSE, FUN = NULL,
                  keep.partial = FALSE, ...) {
  if (missing(pred.var)) {
    pred.var <- get_pred_names(object)
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


#' @rdname vi
#'
#' @export
vi.randomForest <- function(object, pred.var, type = 1, partial = FALSE,
                            FUN = NULL, keep.partial = FALSE, ...) {
  if (missing(pred.var)) {
    pred.var <- get_pred_names(object)
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


#' @rdname vi
#'
#' @export
vi.ranger <- function(object, pred.var, type = 1, partial = FALSE,
                      FUN = NULL, keep.partial = FALSE, ...) {
  if (missing(pred.var)) {
    pred.var <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, pred.var = pred.var, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    imp <- ranger::importance(object)
    vi.type <- object$importance.mode
    all.pred.var <- names(imp)
    all.pred.var <- all.pred.var[order(imp, decreasing = TRUE)]
    out <- tibble::tibble("Variable" = all.pred.var,
                          "Importance" = sort(imp, decreasing = TRUE))
    out[out$Variable %in% pred.var, ]
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.train <- function(object, pred.var, partial = FALSE, FUN = NULL,
                     keep.partial = FALSE, ...) {
  if (missing(pred.var)) {
    pred.var <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, pred.var = pred.var, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    vi.type <- "caret"
    imp <- caret::varImp(object, ...)
    if (inherits(imp, "varImp.train")) {
      imp <- imp$importance
    }
    ord <- order(imp$Overall, decreasing = TRUE)
    tibble::tibble("Variable" = rownames(imp)[ord],
                   "Importance" = imp$Overall[ord])
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.xgb.Booster <- function(object, pred.var,
                           type = c("Gain", "Cover", "Frequency"),
                           partial = FALSE, FUN = NULL, keep.partial = FALSE,
                           ...) {
  if (missing(pred.var)) {
    pred.var <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, pred.var = pred.var, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    type <- match.arg(type)
    imp <- xgboost::xgb.importance(feature_names = pred.var, model = object)
    imp <- tibble::as.tibble(imp)
    imp <- imp[, c("Feature", type)]
    vi.type <- type
    all.pred.var <-imp$Feature
    all.pred.var <- all.pred.var[order(imp[[2L]], decreasing = TRUE)]
    out <- tibble::tibble("Variable" = all.pred.var,
                          "Importance" = sort(imp[[2L]], decreasing = TRUE))
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
