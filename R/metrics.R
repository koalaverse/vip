# Regression -------------------------------------------------------------------

#' @keywords internal
rsquared <- function(pred, obs) {
  stats::cor(obs, pred)^2
}


#' @keywords internal
rmse <- function(pred, obs) {
  sqrt(mean((pred - obs)^2))
}
