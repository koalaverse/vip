#' Variable Importance Plots
#'
#' Plot variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param ... Additional optional arguments to be passed onto \code{\link{vi}}.
#'
#' @rdname vip
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars.lm <- lm(mpg ~ ., data = mtcars)
#' vip(mtcars.lm) + theme_light()
#' }
vip <- function(object, ...) {
  UseMethod("vip")
}


#' @rdname vip
#'
#' @export
vip.default <- function(object, ...) {
  imp <- vi(object, ...)  # variable importance scores
  string <- "reorder(Variable, Importance)"
  ggplot2::ggplot(imp, ggplot2::aes_string(x = string, y = "Importance")) +
    ggplot2::geom_col() +
    ggplot2::xlab("") +
    ggplot2::coord_flip()
}
