#' PDP-based variable importance
#'
#' Compute PDP-based VI scores for the predictors in a model. See details below.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest.
#'
#' @param FUN Deprecated. Use \code{var_fun} instead.
#'
#' @param var_fun List with two components, \code{"cat"} and \code{"con"},
#' containing the functions to use to quantify the variability of the feature
#' effects (e.g., partial dependence values) for categorical and continuous
#' features, respectively. If \code{NULL}, the standard deviation is used for
#' continuous features. For categorical features, the range statistic is used
#' (i.e., (max - min) / 4). Only used when \code{method = "pdp"} or
#' \code{method = "ice"}.
#'
#' @param ... Additional optional arguments to be passed onto
#' \code{\link[pdp]{partial}}.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns,
#' \code{Variable} and \code{Importance}, containing the variable name and its
#' associated importance score, respectively.
#'
#' @details This approach to computing VI scores is based on quantifying the
#' relative "flatness" of the \emph{partial dependence plot} (PDP) of each
#' feature. It is model-agnostic and can be applied to any supervised learning
#' algorithm. By default, relative "flatness" is defined by computing the
#' standard deviation of the y-axis values for each PDP for numeric features;
#' for categorical features, the default is to use range divided by 4. This can
#' be changed via the `var_fun` argument. See
#' \href{https://arxiv.org/abs/1805.04755}{Greenwell et al. (2018)} for details
#' and additional examples.
#'
#' @references
#' Greenwell, B. M., Boehmke, B. C., and McCarthy, A. J. A Simple
#' and Effective Model-Based Variable Importance Measure. arXiv preprint
#' arXiv:1805.04755 (2018).
#'#' @rdname vi_pdp
#'
#' @export
vi_pdp <- function(object, ...) {
  UseMethod("vi_pdp")
}


#' @rdname vi_pdp
#'
#' @export
vi_pdp.default <- function(object, feature_names, FUN = NULL, var_fun = NULL,
                           ...) {

  # # Print warning message
  # warning("Setting `method = \"pdp\"` is experimental, use at your own risk!",
  #         call. = FALSE)

  # Catch deprecated arguments
  if (!is.null(FUN)) {
    stop("Argument `FUN` is deprecated; please use `var_fun` instead.",
         call. = FALSE)
  }

  # Check var_fun argument
  var_fun <- if (is.null(var_fun)) {
    list(
      "cat" = function(x) diff(range(x)) / 4,
      "con" = stats::sd
    )
  } else {
    check_var_fun(var_fun)
    var_fun
  }

  # Consruct PDP-based variable importance scores
  vis <- lapply(feature_names, function(x) {
    pdp_vi_score(object, feature_name = x, var_fun = var_fun, ...)
  })
  tib <- tibble::tibble(
    "Variable" = feature_names,
    "Importance" = unlist(vis)
  )
  pd <- lapply(vis, FUN = function(x) attr(x, "pdp"))
  names(pd) <- feature_names
  attr(tib, which = "pdp") <- pd
  attr(tib, which = "type") <- "pdp"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @keywords internal
pdp_vi_score <- function(object, feature_name, var_fun, ...) {

  # Only allow for a single feature
  if (length(feature_name) != 1L) {
    stop("Only a single feature allowed in `pdp_vi_score()`.", call. = FALSE)
  }

  # Compute PDP
  pd <- pdp::partial(object, pred.var = feature_name, ...)

  # Compute partial dependence-based variable importance scores
  var_fun <- if (is.factor(pd[[feature_name]])) {
    var_fun$cat  # categorical feature
  } else {
    var_fun$con  # continuous feature
  }
  res <- var_fun(pd$yhat)

  # Include PDP as an attribute
  attr(res, which = "pdp") <- pd
  attr(res, which = "type") <- "pdp"

  # Return result
  res

}
