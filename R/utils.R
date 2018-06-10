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

#' @keywords internal
check_FUN <- function(x) {
  # x should be a named list of two functions with names "con" and "cat"
  if (!is.list(x)) {
    stop("FUN should be a list.", call. = FALSE)
  }
  if (length(x) != 2) {
    stop("FUN should be a list of length 2.", call. = FALSE)
  }
  if (!identical(names(x), c("con", "cat"))) {
    stop("FUN should be a list with comonents \"con\" and \"cat\".",
         call. = FALSE)
  }
  if (!all(sapply(x, is.function))) {
    stop("FUN should be a list of two functions.", call. = FALSE)
  }
}
