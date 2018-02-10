#' Variable Importance
#'
#' Compute variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest.
#'
#' @param truncate_feature_names Integer specifying the length at which to
#' truncate feature names. Default is \code{NULL} which results in no truncation
#' (i.e., the full name of each feature will be printed).
#'
#' @param type Integer specifying the type of variable importance measure to
#' return for \code{\link[randomForest]{randomForest}} objects. Should be
#' \code{0} or \code{1}; see \code{\link[randomForest]{importance}} for details.
#'
#' @param auc Logical indicating whether or not to compute the AUC-based
#' variable scores described in Janitza et al. (2012). Only available for
#' \code{\link[cforest]{party}} objects. See
#' \code{\link[varimpAUC]{party}} for details. Default is \code{FALSE}.
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
#' partial dependence values for each predictor listed in \code{feature_names}
#' in a separate attribute called \code{"partial"}. Default is \code{FALSE}.
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
#' vi(mtcars.ppr, feature_names = names(subset(mtcars, select = -mpg)))
vi <- function(object, ...) {
  UseMethod("vi")
}


#' @rdname vi
#'
#' @export
vi.default <- function(object, feature_names, truncate_feature_names = NULL,
                       FUN = NULL, keep.partial = FALSE, ...) {
  imp <- get_pd_vi_scores(object, feature_names = feature_names, FUN = FUN,
                          keep.partial = keep.partial, ...)
  feature_names <- feature_names[order(imp, decreasing = TRUE)]
  res <- tibble::tibble("Variable" = feature_names,
                        "Importance" = sort(imp, decreasing = TRUE))
  if (!is.null(truncate_feature_names)) {
    res$Variable <- truncate_feature_names(res$Variable,
                                           length = truncate_feature_names)
  }
  attr(res, "partial") <- attr(imp, "partial")
  res
}


#' @rdname vi
#'
#' @export
vi.earth <- function(object, feature_names, truncate_feature_names = NULL,
                     partial = FALSE, FUN = NULL, keep.partial = FALSE, ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, feature_names = feature_names, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    vi.type <- "earth"
    imp <- earth::evimp(object, trim = FALSE, ...)  # return all importance scores
    imp <- unclass(imp)[, c("nsubsets", "gcv", "rss")]
    imp <- cbind("Variable" = rownames(imp), as.data.frame(as.matrix(imp)))
    rownames(imp) <- NULL
    tibble::as.tibble(imp)
  }
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.gbm <- function(object, feature_names, truncate_feature_names = NULL,
                   partial = FALSE, FUN = NULL, keep.partial = FALSE, ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, feature_names = feature_names, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    vi.type <- "rel.inf"
    imp <- summary(object, plotit = FALSE, order = TRUE, ...)
    rownames(imp) <- NULL
    names(imp) <- c("Variable", "Importance")
    out <- tibble::as.tibble(imp)
    out[out$Variable %in% feature_names, ]
  }
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.H2ORegressionModel <- function(object, feature_names,
                                  truncate_feature_names = NULL,
                                  partial = FALSE, FUN = NULL,
                                  keep.partial = FALSE, ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    # stop("`method = \"partial\" is currently not supported.")
    vi.type <- "partial"
    pd_list <- h2o::h2o.partialPlot(object, cols = feature_names, plot = FALSE,
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
    names(imp) <- feature_names
    imp <- sort(imp, decreasing = TRUE)
    tibble::tibble("Variable" = names(imp), "Importance" = imp)
  } else {
    vi.type <- "rel.inf"
    imp <- tibble::as.tibble(h2o::h2o.varimp(object))
    imp[3L:4L] <- NULL
    names(imp) <- c("Variable", "Importance")
    imp[imp$Variable %in% feature_names, ]
  }
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
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
vi.lm <- function(object, feature_names, truncate_feature_names = NULL,
                  partial = FALSE, FUN = NULL, keep.partial = FALSE, ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, feature_names = feature_names, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    vi.type <- "tstat"
    coefs <- summary(object)$coefficients
    if (attr(object$terms, "intercept") == 1) {
      coefs <- coefs[-1, ]
    }
    feature_names <- rownames(coefs)
    imp <- abs(coefs[, "t value"])
    feature_names <- feature_names[order(imp, decreasing = TRUE)]
    tibble::tibble("Variable" = feature_names,
                   "Importance" = sort(imp, decreasing = TRUE))
  }
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.randomForest <- function(object, feature_names,
                            truncate_feature_names = NULL, type = 1,
                            partial = FALSE, FUN = NULL, keep.partial = FALSE,
                            ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, feature_names = feature_names, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    imp <- randomForest::importance(object, type = type, ...)
    if (dim(imp)[2L] == 0) {  # possible when importance = FALSE in RF call
      imp <- object$importance
    }
    vi.type <- colnames(imp)[1L]
    all.feature_names <- rownames(imp)
    imp <- imp[, 1L]
    all.feature_names <- all.feature_names[order(imp, decreasing = TRUE)]
    out <- tibble::tibble("Variable" = all.feature_names,
                          "Importance" = sort(imp, decreasing = TRUE))
    out[out$Variable %in% feature_names, ]
  }
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.RandomForest <- function(object, feature_names,
                            truncate_feature_names = NULL, auc = FALSE,
                            partial = FALSE, FUN = NULL, keep.partial = FALSE,
                            ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, feature_names = feature_names, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    imp <- if (auc) {
      party::varimpAUC(object, ...)
    } else {
      party::varimp(object, ...)
    }
    vi.type <- if (auc) {
      "AUC"
    } else {
      "Permutation"
    }
    all.feature_names <- names(imp)
    all.feature_names <- all.feature_names[order(imp, decreasing = TRUE)]
    out <- tibble::tibble("Variable" = all.feature_names,
                          "Importance" = sort(imp, decreasing = TRUE))
    out[out$Variable %in% feature_names, ]
  }
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.ranger <- function(object, feature_names, truncate_feature_names = NULL,
                      partial = FALSE, FUN = NULL, keep.partial = FALSE, ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, feature_names = feature_names, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    imp <- ranger::importance(object)
    vi.type <- object$importance.mode
    all.feature_names <- names(imp)
    all.feature_names <- all.feature_names[order(imp, decreasing = TRUE)]
    out <- tibble::tibble("Variable" = all.feature_names,
                          "Importance" = sort(imp, decreasing = TRUE))
    out[out$Variable %in% feature_names, ]
  }
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.rpart <- function(object, feature_names, truncate_feature_names = NULL,
                     partial = FALSE,FUN = NULL, keep.partial = FALSE, ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, feature_names = feature_names, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    imp <- object$variable.importance
    if (is.null(imp)) {
      stop("Cannot extract variable importance scores ",
           "from a tree with no splits.", call. = FALSE)
    }
    vi.type <- "GoodnessOfSplit"
    all.feature_names <- names(imp)
    all.feature_names <- all.feature_names[order(imp, decreasing = TRUE)]
    out <- tibble::tibble("Variable" = all.feature_names,
                          "Importance" = sort(imp, decreasing = TRUE))
    out[out$Variable %in% feature_names, ]
  }
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.train <- function(object, feature_names, truncate_feature_names = NULL,
                     partial = FALSE, FUN = NULL, keep.partial = FALSE, ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, feature_names = feature_names, FUN = NULL,
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
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
  }
  attr(tib, "vi.type") <- vi.type
  tib
}


#' @rdname vi
#'
#' @export
vi.xgb.Booster <- function(object, feature_names, truncate_feature_names = NULL,
                           type = c("Gain", "Cover", "Frequency"),
                           partial = FALSE, FUN = NULL, keep.partial = FALSE,
                           ...) {
  if (missing(feature_names)) {
    feature_names <- get_pred_names(object)
  }
  tib <- if (partial) {
    vi.type <- "partial"
    vi.default(object, feature_names = feature_names, FUN = NULL,
               keep.partial = keep.partial, ...)
  } else {
    type <- match.arg(type)
    imp <- xgboost::xgb.importance(feature_names = feature_names,
                                   model = object)
    imp <- tibble::as.tibble(imp)
    imp <- imp[, c("Feature", type)]
    vi.type <- type
    all.feature_names <-imp$Feature
    all.feature_names <- all.feature_names[order(imp[[2L]], decreasing = TRUE)]
    out <- tibble::tibble("Variable" = all.feature_names,
                          "Importance" = sort(imp[[2L]], decreasing = TRUE))
    out[out$Variable %in% feature_names, ]
  }
  if (!is.null(truncate_feature_names)) {
    tib$Variable <- truncate_feature_names(
      tib$Variable, length = truncate_feature_names
    )
  }
  attr(tib, "vi.type") <- vi.type
  tib
}
