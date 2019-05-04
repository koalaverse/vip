#' ICE-Based Variable Importance
#'
#' Compute ICE-based variable importance scores for the predictors in a model.
#' (This function is meant for internal use only.)
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest.
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
#' @details Coming soon!
#'
#' @rdname vi_ice
#'
#' @export
vi_ice <- function(object, ...) {
  UseMethod("vi_ice")
}


#' @rdname vi_ice
#'
#' @export
vi_ice.default <- function(object, feature_names, var_fun = NULL, ...) {

  # Print warning message
  warning("Setting `method = \"ice\"` is experimental, use at your own risk!",
          call. = FALSE)

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

  # Consruct ICE-based variable importance scores
  vis <- lapply(feature_names, FUN = function(x) {
    ice_vi_score(object, feature_name = x, var_fun = var_fun, ...)
  })
  tib <- tibble::tibble(
    "Variable" = feature_names,
    "Importance" = unlist(vis)
  )
  ice <- lapply(vis, FUN = function(x) attr(x, "ice"))
  names(ice) <- feature_names
  attr(tib, which = "ice") <- ice
  attr(tib, which = "type") <- "ice"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @keywords internal
ice_vi_score <- function(object, feature_name, var_fun, ...) {

  # Only allow for a single feature
  if (length(feature_name) != 1L) {
    stop("Only a single feature allowed in `ice_vi_score()`.", call. = FALSE)
  }

  # Compute ICE curves
  ice <- pdp::partial(object, pred.var = feature_name, ice = TRUE, ...)

  # Compute partial dependence-based variable importance scores
  var_fun <- if (is.factor(ice[[feature_name]])) {
    var_fun$cat  # categorical feature
  } else {
    var_fun$con  # continuous feature
  }
  res <- mean(tapply(ice$yhat, INDEX = ice$yhat.id, FUN = var_fun))

  # Include ICE curves as an attribute
  attr(res, which = "ice") <- ice

  # Return result
  res

}
