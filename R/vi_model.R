#' Model-Based Variable Importance
#'
#' Compute model-based variable importance scores for the predictors in a model.
#' (This function is meant for internal use only.)
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param auc Logical indicating whether or not to compute the AUC-based
#' variable scores described in Janitza et al. (2012). Only available for
#' \code{\link[party]{cforest}} objects. See
#' \code{\link[party]{varimpAUC}} for details. Default is \code{FALSE}.
#'
#' @param ... Additional optional arguments.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns:
#' \code{Variable} and \code{Importance}. For \code{"glm"}-like object, an
#' additional column, called \code{Sign}, is also included which includes the
#' sign (i.e., POS/NEG) of the original coefficient.
#'
#' @details Coming soon!
#'
#' @rdname vi_model
#'
#' @export
vi_model <- function(object, ...) {
  UseMethod("vi_model")
}


#' @rdname vi_model
#'
#' @export
vi_model.default <- function(object, ...) {
  stop("Model-based variable importance scores are currently not available ",
       "for objects of class ", "\"", class(object), "\".")
}


#' @rdname vi_model
#'
#' @export
vi_model.C5.0 <- function(object, ...) {

  # Consruct model-based variable importance scores
  vis <- C50::C5imp(object, ...)
  tib <- tibble::tibble(
    "Variable" = rownames(vis),
    "Importance" = vis$Overall
  )

  # Add variable importance type attribute
  dots <- list(...)
  attr(tib, "type") <- if ("metric" %in% names(dots)) {
    dots[["metric"]]
  } else {
    "usage"
  }

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.constparty <- function(object, ...) {

  # Construct model-based variable importance scores
  vis <- partykit::varimp(object, ...)
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, "type") <- "permutation"

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.earth <- function(object, ...) {

  # Construct model-based variable importance scores
  vis <- earth::evimp(object, trim = FALSE, ...)[, "gcv", drop = TRUE]
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )
  tib$Variable <- gsub("-unused$", replacement = "", x = tib$Variable)

  # Add variable importance type attribute
  attr(tib, "type") <- "GCV"

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.gbm <- function(object, ...) {

  # Construct model-based variable importance scores
  vis <- summary(object, plotit = FALSE, order = TRUE, ...)
  tib <- tibble::tibble(
    "Variable" = vis$var,
    "Importance" = vis$rel.inf
  )

  # Add variable importance type attribute
  attr(tib, "type") <- "rel.inf"

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.H2OBinomialModel <- function(object, ...) {

  # Construct model-based variable importance scores
  tib <- tibble::as.tibble(h2o::h2o.varimp(object))
  if (object@algorithm == "glm") {
    names(tib) <- c("Variable", "Importance", "Sign")
    # FIXME: Extra row at the bottom?
  } else {
    tib <- tib[1L:2L]
    names(tib) <- c("Variable", "Importance")
  }

  # Add variable importance type attribute
  attr(tib, "type") <- "h2o"

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.H2OMultinomialModel <- function(object, ...) {

  # Construct model-based variable importance scores
  tib <- tibble::as.tibble(h2o::h2o.varimp(object))
  if (object@algorithm == "glm") {
    names(tib) <- c("Variable", "Importance", "Sign")
    # FIXME: Extra row at the bottom?
  } else {
    tib <- tib[1L:2L]
    names(tib) <- c("Variable", "Importance")
  }

  # Add variable importance type attribute
  attr(tib, "type") <- "h2o"

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.H2ORegressionModel <- function(object, ...) {

  # Construct model-based variable importance scores
  tib <- tibble::as.tibble(h2o::h2o.varimp(object))
  if (object@algorithm == "glm") {
    names(tib) <- c("Variable", "Importance", "Sign")
    # FIXME: Extra row at the bottom?
  } else {
    tib <- tib[1L:2L]
    names(tib) <- c("Variable", "Importance")
  }

  # Add variable importance type attribute
  attr(tib, "type") <- "h2o"

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.lm <- function(object, ...) {

  # Construct model-based variable importance scores
  coefs <- summary(object)$coefficients
  if (attr(object$terms, "intercept") == 1) {
    coefs <- coefs[-1L, , drop = FALSE]
  }
  tib <- tibble::tibble(
    "Variable" = rownames(coefs),
    "Importance" = abs(coefs[, "t value"]),
    "Sign" = ifelse(sign(coefs[, "Estimate"]) == 1, yes = "POS", no = "NEG")
  )

  # Add variable importance type attribute
  attr(tib, "type") <- "t-test"

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.randomForest <- function(object, ...) {

  # Construct model-based variable importance scores
  vis <- randomForest::importance(object, ...)
  type <- colnames(vis)[1L]
  if (dim(vis)[2L] == 0) {  # possible when importance = FALSE in RF call
    importance_scores <- object$importance
  }
  vis <- vis[, 1L, drop = TRUE]
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, "type") <- type

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.RandomForest <- function(object, auc = FALSE, ...) {

  # Construct model-based variable importance scores
  vis <- if (auc) {
    party::varimpAUC(object)  # rm ... for now
  } else {
    party::varimp(object)  # rm ... for now
  }
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, "type") <- if (auc) {
    "AUC"
  } else {
    "Permutation"
  }

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.ranger <- function(object, ...) {

  # Construct model-based variable importance scores
  vis <- ranger::importance(object)
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, "type") <- object$importance.mode

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.rpart <- function(object, ...) {

  # Construct model-based variable importance scores
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

  # Add variable importance type attribute
  attr(tib, "type") <- "GoodnessOfSplit"

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.train <- function(object, ...) {

  # Construct model-based variable importance scores
  vis <- caret::varImp(object, ...)
  if (inherits(vis, "varImp.train")) {
    vis <- vis$importance
  }
  tib <- tibble::tibble(
    "Variable" = rownames(vis),
    "Importance" = vis$Overall
  )

  # Add variable importance type attribute
  attr(tib, "type") <- "caret"

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.xgb.Booster <- function(object, ...) {

  # Construct model-based variable importance scores
  vis <- tibble::as.tibble(xgboost::xgb.importance(
    model = object, ...
  ))[, c("Feature", "Gain")]  # FIXME: What about "Cover" and "Frequency"?
  tib <- tibble::tibble(
    "Variable" = vis$Feature,
    "Importance" = vis[[2L]]
  )

  # Add variable importance type attribute
  attr(tib, "type") <- "Gain"  # match.arg(type)

  # Return results
  tib

}
