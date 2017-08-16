#' Variable Importance Plots
#'
#' Plot variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param alpha Numeric value between 0 and 1 giving the trasparency of the
#' bars.
#'
#' @param color Character string specifying the color to use for the borders of
#' the bars. Could also be a function, such as
#' \code{\link[grDevices]{heat.colors}}.
#'
#' @param fill Character string specifying the color to use to fill the bars.
#' Could also be a function, such as \code{\link[grDevices]{heat.colors}}.
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
vip.default <- function(object, alpha = 1, color = "#444444", fill = "#444444",
                        ...) {
  imp <- vi(object, ...)  # variable importance scores
  string <- "reorder(Variable, Importance)"
  ggplot2::ggplot(imp, ggplot2::aes_string(x = string, y = "Importance")) +
    ggplot2::geom_col(color = color, fill = fill, alpha = alpha) +
    ggplot2::xlab("") +
    ggplot2::coord_flip()
}
