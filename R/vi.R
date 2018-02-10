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
#' @param sort Logical indicating whether or not to order the sort the variable
#' importance scores. Default is \code{TRUE}.
#'
#' @param decreasing Logical indicating whether or not the variable importance
#' scores should be sorted in descending (\code{TRUE}) or ascending
#' (\code{FALSE}) order of importance. Default is \code{TRUE}.
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
#' @param keep_partial Logical indicating whether or not to return the computed
#' partial dependence values for each predictor listed in \code{feature_names}
#' in a separate attribute called \code{"partial"}. Default is \code{FALSE}.
#' Only used when \code{partial = TRUE}.
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
                       sort = TRUE, decreasing = TRUE, FUN = NULL,
                       keep_partial = FALSE, ...) {

  # Construct partial dependence-based variable importance scores
  imp_scores <- get_pd_vi_scores(
    object, feature_names = feature_names, FUN = FUN,
    keep_partial = keep_partial, ...
  )

  # Place variable importance scores in a tibble (the first and second columns
  # should always be labelled "Variable" and "Importance", respectively)
  tib <- tibble::tibble(
    "Variable" = feature_names,
    "Importance" = imp_scores
  )
  attr(tib, "partial") <- attr(imp_scores, "partial")  # include partial values

  # Sort variable importance scores (if requested)
  if (sort) {
    tib <- sort_importance_scores(tib, decreasing = decreasing)
  }

  # Truncate feature names (if requested)
  if (!is.null(truncate_feature_names)) {
    tib <- truncate_feature_names(tib, length = truncate_feature_names)
  }

  # Add variable importance type attribute
  attr(tib, "vi.type") <- "partial"

  # Return results
  tib

}


#' @rdname vi
#'
#' @export
vi.earth <- function(object, feature_names, truncate_feature_names = NULL,
                     sort = TRUE, decreasing = TRUE, partial = FALSE,
                     FUN = NULL, keep_partial = FALSE, ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Extract variable importance scores
  if (partial) {

    # Construct partial dependence-based variable importance scores
    vi.default(
      object, feature_names = feature_names,
      truncate_feature_names = truncate_feature_names, sort = sort,
      decreasing = decreasing, FUN = NULL, keep_partial = keep_partial, ...
    )

  } else {

    # Extract object-based variable importance scores and feature names
    imp_scores <- earth::evimp(object, trim = FALSE, ...)[, "gcv", drop = TRUE]
    feature_names <- names(imp_scores)

    # Remove "-unused" from variable importance names returned by earth::evimp
    feature_names <- gsub("-unused$", replacement = "", x = feature_names)

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = imp_scores
    )

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- "GCC"

    # Return results
    tib

  }

}


#' @rdname vi
#'
#' @export
vi.gbm <- function(object, feature_names, truncate_feature_names = NULL,
                   sort = TRUE, decreasing = TRUE, partial = FALSE, FUN = NULL,
                   keep_partial = FALSE, ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Extract variable importance scores
  if (partial) {

    # Construct partial dependence-based variable importance scores
    vi.default(
      object, feature_names = feature_names,
      truncate_feature_names = truncate_feature_names, sort = sort,
      decreasing = decreasing, FUN = NULL, keep_partial = keep_partial, ...
    )

  } else {

    # Extract object-based variable importance scores and feature names
    imp_scores <- summary(object, plotit = FALSE, order = TRUE, ...)
    feature_names <- names(imp_scores)

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = imp_scores
    )

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- "rel.inf"

    # Return results
    tib

  }

}


#' @rdname vi
#'
#' @export
vi.H2ORegressionModel <- function(object, feature_names,
                                  truncate_feature_names = NULL, sort = TRUE,
                                  decreasing = TRUE,partial = FALSE, FUN = NULL,
                                  keep_partial = FALSE, ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  if (partial) {

    # Extract list of patial dependence values (one for each feature)
    pd_list <- h2o::h2o.partialPlot(
      object, cols = feature_names, plot = FALSE, plot_stddev = FALSE, ...
    )

    # Construct partial dependence-based variable importance scores
    importance_scores <- if (is.null(FUN)) {
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

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = importance_scores
    )
    attr(tib, "partial") <- pd_list  # include partial values

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- "partial"

    # Return results
    tib

  } else {

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::as.tibble(h2o::h2o.varimp(object))
    tib[3L:4L] <- NULL
    names(tib) <- c("Variable", "Importance")

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- "rel.inf"

    # Return results
    tib

  }

}


#' @rdname vi
#'
#' @export
vi.lm <- function(object, feature_names, truncate_feature_names = NULL,
                  sort = TRUE, decreasing = TRUE, partial = FALSE, FUN = NULL,
                  keep_partial = FALSE, ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Extract variable importance scores
  if (partial) {

    # Construct partial dependence-based variable importance scores
    vi.default(
      object, feature_names = feature_names,
      truncate_feature_names = truncate_feature_names, sort = sort,
      decreasing = decreasing, FUN = NULL, keep_partial = keep_partial, ...
    )

  } else {

    # Extract object-based variable importance scores and feature names
    coefs <- summary(object)$coefficients
    if (attr(object$terms, "intercept") == 1) {
      coefs <- coefs[-1L, ]
    }
    feature_names <- rownames(coefs)
    importance_scores <- abs(coefs[, "t value"])

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = importance_scores
    )

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- "t-test"

    # Return results
    tib

  }

}


#' @rdname vi
#'
#' @export
vi.randomForest <- function(object, feature_names,
                            truncate_feature_names = NULL, sort = TRUE,
                            decreasing = TRUE, type = 1, partial = FALSE,
                            FUN = NULL, keep_partial = FALSE, ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Extract variable importance scores
  if (partial) {

    # Construct partial dependence-based variable importance scores
    vi.default(
      object, feature_names = feature_names,
      truncate_feature_names = truncate_feature_names, sort = sort,
      decreasing = decreasing, FUN = NULL, keep_partial = keep_partial, ...
    )

  } else {

    # Extract object-based variable importance scores and feature names
    importance_scores <- randomForest::importance(object, type = type, ...)
    if (dim(importance_scores)[2L] == 0) {  # possible when importance = FALSE in RF call
      importance_scores <- object$importance
    }
    importance_scores <- importance_scores[, 1L, drop = TRUE]
    feature_names <- names(importance_scores)

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = importance_scores
    )

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- colnames(importance_scores)[1L]

    # Return results
    tib

  }

}


#' @rdname vi
#'
#' @export
vi.RandomForest <- function(object, feature_names,
                            truncate_feature_names = NULL, sort = TRUE,
                            decreasing = TRUE, auc = FALSE, partial = FALSE,
                            FUN = NULL, keep_partial = FALSE, ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Extract variable importance scores
  if (partial) {

    # Construct partial dependence-based variable importance scores
    vi.default(
      object, feature_names = feature_names,
      truncate_feature_names = truncate_feature_names, sort = sort,
      decreasing = decreasing, FUN = NULL, keep_partial = keep_partial, ...
    )

  } else {

    # Extract object-based variable importance scores and feature names
    importance_scores <- if (auc) {
      party::varimpAUC(object, ...)
    } else {
      party::varimp(object, ...)
    }
    feature_names <- names(importance_scores)

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = importance_scores
    )

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- if (auc) {
      "AUC"
    } else {
      "Permutation"
    }

    # Return results
    tib

  }

}


#' @rdname vi
#'
#' @export
vi.ranger <- function(object, feature_names, truncate_feature_names = NULL,
                      sort = TRUE, decreasing = TRUE, partial = FALSE,
                      FUN = NULL, keep_partial = FALSE, ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Extract variable importance scores
  if (partial) {

    # Construct partial dependence-based variable importance scores
    vi.default(
      object, feature_names = feature_names,
      truncate_feature_names = truncate_feature_names, sort = sort,
      decreasing = decreasing, FUN = NULL, keep_partial = keep_partial, ...
    )

  } else {

    # Extract object-based variable importance scores and feature names
    importance_scores <- ranger::importance(object)
    feature_names <- names(importance_scores)

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = importance_scores
    )

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- object$importance.mode

    # Return results
    tib

  }

}


#' @rdname vi
#'
#' @export
vi.rpart <- function(object, feature_names, truncate_feature_names = NULL,
                     sort = TRUE, decreasing = TRUE, partial = FALSE,
                     FUN = NULL, keep_partial = FALSE, ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Extract variable importance scores
  if (partial) {

    # Construct partial dependence-based variable importance scores
    vi.default(
      object, feature_names = feature_names,
      truncate_feature_names = truncate_feature_names, sort = sort,
      decreasing = decreasing, FUN = NULL, keep_partial = keep_partial, ...
    )

  } else {

    # Extract object-based variable importance scores and feature names
    importance_scores <- object$variable.importance
    if (is.null(importance_scores)) {
      stop("Cannot extract variable importance scores from a tree with no ",
           "splits.", call. = FALSE)
    }
    feature_names <- names(importance_scores)

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = importance_scores
    )

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- "GoodnessOfSplit"

    # Return results
    tib

  }

}


#' @rdname vi
#'
#' @export
vi.train <- function(object, feature_names, truncate_feature_names = NULL,
                     sort = TRUE, decreasing = TRUE, partial = FALSE,
                     FUN = NULL, keep_partial = FALSE, ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Extract variable importance scores
  if (partial) {

    # Construct partial dependence-based variable importance scores
    vi.default(
      object, feature_names = feature_names,
      truncate_feature_names = truncate_feature_names, sort = sort,
      decreasing = decreasing, FUN = NULL, keep_partial = keep_partial, ...
    )

  } else {

    # Extract object-based variable importance scores and feature names
    importance_scores <- caret::varImp(object, ...)
    if (inherits(importance_scores, "varImp.train")) {
      importance_scores <- importance_scores$importance
    }
    feature_names <- rownames(importance_scores)
    importance_scores <- importance_scores$Overall

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = importance_scores
    )

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- "caret"

    # Return results
    tib

  }

}


#' @rdname vi
#'
#' @export
vi.xgb.Booster <- function(object, feature_names, truncate_feature_names = NULL,
                           sort = TRUE, decreasing = TRUE,
                           type = c("Gain", "Cover", "Frequency"),
                           partial = FALSE, FUN = NULL, keep_partial = FALSE,
                           ...) {

  # Requested feature names
  if (missing(feature_names)) {
    feature_names <- get_feature_names(object)
  }

  # Extract variable importance scores
  if (partial) {

    # Construct partial dependence-based variable importance scores
    vi.default(
      object, feature_names = feature_names,
      truncate_feature_names = truncate_feature_names, sort = sort,
      decreasing = decreasing, FUN = NULL, keep_partial = keep_partial, ...
    )

  } else {

    # Extract object-based variable importance scores and feature names
    importance_scores <- tibble::as.tibble(xgboost::xgb.importance(
      feature_names = feature_names, model = object
    ))[, c("Feature", type)]
    feature_names <- importance_scores$Feature
    importance_scores <- importance_scores[[2L]]

    # Place variable importance scores in a tibble (the first and second columns
    # should always be labelled "Variable" and "Importance", respectively)
    tib <- tibble::tibble(
      "Variable" = feature_names,
      "Importance" = importance_scores
    )

    # FIXME: What's the best way to subset by the requested feature names?

    # Sort variable importance scores (if requested)
    if (sort) {
      tib <- sort_importance_scores(tib, decreasing = decreasing)
    }

    # Truncate feature names (if requested)
    if (!is.null(truncate_feature_names)) {
      tib <- truncate_feature_names(tib, length = truncate_feature_names)
    }

    # Add variable importance type attribute
    attr(tib, "type") <- match.arg(type)

    # Return results
    tib

  }

}
