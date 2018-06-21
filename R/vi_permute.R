#' Permutation-Based Variable Importance
#'
#' Compute permutation-based variable importance scores for the predictors in a
#' model. (This function is meant for internal use only.)
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest.
#'
#' @param train Non-optional data frame containing the original training data.
#'
#' @param pfun Non-optional prediction function that requires two arguments:
#' \code{object} and \code{newdata}. If specified, then the function must return
#' a vector of predictions (i.e., not a matrix or data frame). (In the future,
#' this argument may become optional.)
#'
#' @param obs Non-optional vector containing the original (i.e., training)
#' response values.
#'
#' @param metric Non-optional function for computing model performance (e.g.,
#' RMSE for regression or accuracy for binary classification). This function
#' requires two arguments, \code{pred} (for predicted values) and \code{obs}
#' (for observed values), and should return a single, numeric value.
#'
#' @param Non-optional character string specifying whether this is supervized
#' regression (\code{"regression"}) or classification (\code{"classification"}).
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns:
#' \code{Variable} and \code{Importance}. For \code{"glm"}-like object, an
#' additional column, called \code{Sign}, is also included which gives the sign
#' (i.e., POS/NEG) of the original coefficient.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @details Coming soon!
#'
#' @rdname vi_permute
#'
#' @export
vi_permute <- function(object, ...) {
  UseMethod("vi_permute")
}


#' @rdname vi_model
#'
#' @export
vi_permute.default <- function(
  object,         # fitted model object
  feature_names,  # feature names of interest
  train,          # data used to train the model          --> will get automated
  pfun,           # function(object, newdata) {}          --> will get automated
  obs,            # observed response values              --> will get automated
  metric,         # function(pred, obs) {}
  type,           # "classification" or "regression"      --> will get automated
  ...
) {

  # Issue warning until this function is complete!
  warning("This function is experimental and not yet recommended for use.")

  # if (missing(train)) {
  #   train <- get_training_data(object)
  # }

  # if (missing(feature_names)) {
  #   feature_names <- get_feature_names()
  # }

  # if (missing(type)) {
  #   type <- get_supervized_type(object)
  # }

  # if (missing(metric)) {
  #   metric <- get_default_metric(type)
  # }

  # Compute baseline metric for comparison
  baseline <- metric(pred = pfun(object, newdata = train), obs = obs)

  # Construct VI scores
  #
  # Loop through each feature and do the following:
  #
  #   1. make a copy of the training data;
  #   2. permute the values of the original feature;
  #   3. get new predictions based on permuted data set;
  #   4. record difference in accuracy.
  vis <- unlist(lapply(feature_names, FUN = function(x) {
    ptrain <- train  # make copy
    ptrain[[x]] <- sample(ptrain[[x]])  # permute values
    acc <- metric(pred = pfun(object, newdata = ptrain), obs = obs)
    baseline - acc  # FIXME: order matters here (e.g., R2 vs RMSE)!
  }))
  tib <- tibble::tibble(
    "Variable" = feature_names,
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, "type") <- "permutation"

  # Return results
  tib

}
