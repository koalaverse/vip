#' Variance-based variable importance
#'
#' Compute variance-based variable importance using a simple
#' \emph{feature importance ranking measure} (FIRM) approach; for details, see
#' \href{https://arxiv.org/abs/1805.04755}{Greenwell et al. (2018)} and
#' \href{https://arxiv.org/abs/1904.03959}{Scholbeck et al. (2019)}.
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
#' (i.e., (max - min) / 4). Only applies when \code{method = "firm"}.
#'
#' @param ice Logical indicating whether or not to estimate feature effects
#' using \emph{individual conditional expectation} (ICE) curves.
#' Only applies when \code{method = "firm"}. Default is \code{FALSE}. Setting
#' \code{ice = TRUE} is preferred whenever strong interaction effects are
#' potentially present.
#'
#' @param ... Additional optional arguments to be passed onto
#' \code{\link[pdp]{partial}}.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns,
#' \code{Variable} and \code{Importance}, containing the variable name and its
#' associated importance score, respectively.
#'
#' @details This approach to computing VI scores is based on quantifying the
#' relative "flatness" of the effect of each feature. Feature effects can be
#' assessed using \emph{partial dependence plots} (PDPs) or
#' \emph{individual conditional expectation} (ICE) curves. These approaches are
#' model-agnostic and can be applied to any supervised learning algorithm. By
#' default, relative "flatness" is defined by computing the standard deviation
#' of the y-axis values for each feature effect plot for numeric features; for
#' categorical features, the default is to use range divided by 4. This can be
#' changed via the `var_fun` argument. See
#' \href{https://arxiv.org/abs/1805.04755}{Greenwell et al. (2018)} for details
#' and additional examples.
#'
#' @references
#' Greenwell, B. M., Boehmke, B. C., and McCarthy, A. J. A Simple
#' and Effective Model-Based Variable Importance Measure. arXiv preprint
#' arXiv:1805.04755 (2018).
#'
#' Scholbeck, C. A. Scholbeck, and Molnar, C.,  and Heumann C., and Bischl, B.,
#' and Casalicchio, G. Sampling, Intervention, Prediction, Aggregation: A
#' Generalized Framework for Model-Agnostic Interpretations. arXiv preprint
#' arXiv:1904.03959 (2019).
#'
#' @rdname vi_firm
#'
#' @export
vi_firm <- function(object, ...) {
  UseMethod("vi_firm")
}


#' @rdname vi_firm
#'
#' @export
vi_firm.default <- function(object, feature_names, FUN = NULL, var_fun = NULL,
                            ice = FALSE, ...) {

  # # Print warning message
  # warning("Setting `method = \"pdp\"` is experimental, use at your own risk!",
  #         call. = FALSE)

  # Check for fastshap package
  if (!requireNamespace("pdp", quietly = TRUE)) {
    stop("Package \"pdp\" needed for this function to work. ",
         "Please install it.", call. = FALSE)
  }

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
    firm(object, feature_name = x, var_fun = var_fun, ice = ice, ...)
  })
  tib <- tibble::tibble(
    "Variable" = feature_names,
    "Importance" = unlist(vis)
  )
  fe <- lapply(vis, FUN = function(x) attr(x, "effects"))
  names(fe) <- feature_names
  attr(tib, which = "effects") <- fe
  attr(tib, which = "type") <- "firm"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' Feature importance ranking measure
#'
#' Compute variable importance using a variance-based measure referred to as the
#' \emph{feature importance ranking measure} (FIRM).
#'
#' @keywords internal
#'
#' @noRd
firm <- function(object, feature_name, var_fun, ice = FALSE, ...) {

  # Only allow for a single feature
  if (length(feature_name) != 1L) {
    stop("Only a single feature allowed in `firm()`.", call. = FALSE)
  }

  # Compute feature effects
  fe <- pdp::partial(object, pred.var = feature_name, ice = ice, ...)

  # Compute partial dependence-based variable importance scores
  var_fun <- if (is.factor(fe[[feature_name]])) {
    var_fun$cat  # categorical feature
  } else {
    var_fun$con  # continuous feature
  }

  # Compute FIRM
  res <- if (isTRUE(ice)) {
    mean(tapply(fe$yhat, INDEX = fe$yhat.id, FUN = var_fun))
  } else {
    var_fun(fe$yhat)
  }

  # Include estimated feature effects as an additional attribute
  attr(res, which = "effects") <- fe

  # Return result
  res

}
