#' ACE-based variable importance
#'
#' Compute PDP-based variable importance scores for the predictors in a model.
#' (This function is meant for internal use only.)
#'
#' @param x,formula A data frame or a matrix of features, or a formula of the
#' form \code{response ~ features}.
#'
#' @param y A vector of response values
#'
#' @param data an optional data frame, list or environment (or object coercible
#' by \code{as.data.frame} to a data frame) containing the variables in the
#' model. If not found in \code{data}, the variables are taken from
#' \code{environment(formula)}, typically the environment from which
#' \code{\link{vi_ace}} was called.
#'
#' @param ... Additional optional arguments passed onto
#' \code{\link[acepack]{ace}}.
#'
#' @details The alternating conditional expectations (ACE) algorithm finds the
#' transformations of response and features that maximize the proportion of
#' variation in the response explained by each predictor. The ACE-based variable
#' importance measure is computed by taking the square of the Pearson
#' correlation coefficient between each feature and the response.
#'
#' @rdname vi_ace
vi_ace <- function(x, y = NULL, ...) {
  UseMethod("vi_ace")
}


#' @rdname vi_ace
vi_ace.default <- function(x, y = NULL, ...) {
  if (is.data.frame(x)) {
    x <- data.matrix(x)
  }
  ace <- acepack::ace(x = x, y = y, ...)
  vis <- apply(ace$tx, MARGIN = 2, FUN = function(x) cor(x, ace$ty)^2)
  tib <- tibble::tibble(
    "Variable" = colnames(x),
    "Importance" = vis
  )
  attr(tib, which = "type") <- "ace"

  # Return results
  tib
}


#' @rdname vi_ace
vi_ace.formula <- function(formula, data, ..., na.action = stats::na.fail) {
  # Print warning message
  warning("Setting `method = \"ace\"` is experimental, use at your own risk!",
          call. = FALSE)
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, sys.parent()))) {
    m$data <- as.data.frame(data)
  }
  m$... <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval(m, sys.parent())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  y <- stats::model.extract(m, "response")
  attr(y, "na.action") <- attr(m, "na.action")
  x <- stats::model.matrix(Terms, m)
  # m <- model.frame(terms(reformulate(attributes(Terms)$term.labels)),
  #                  data.frame(m))
  # if (!is.null(y)) {
  #   m <- m[, -1, drop=FALSE]
  # }
  # for (i in seq(along = x)) {
  #   if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
  # }
  vi_ace.default(x = x, y = y, ...)
}
