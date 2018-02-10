#' Arrange multiple grobs on a page
#'
#' See \code{\link[gridExtra]{grid.arrange}} for more details.
#'
#' @name grid.arrange
#' @rdname grid.arrange
#' @keywords internal
#' @export
#' @importFrom gridExtra grid.arrange
#' @usage grid.arrange(..., newpage = TRUE)
NULL


#' @keywords internal
truncate_feature_names <- function(x, length) {
  x$Variable <- substr(x$Variable, start = 1L, stop = length)
  x
}


#' @keywords internal
sort_importance_scores <- function(x, decreasing) {
  x[order(x$Importance, decreasing = decreasing), ]
}
