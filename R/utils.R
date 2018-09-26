#' Pipe operator
#'
#' See \code{\link[magrittr]{\%T>\%}} for more details.
#'
#' @name %T>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %T>%
#' @usage lhs \%T>\% rhs
NULL


#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


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

abbreviate_names <- function(x, minlength) {
  x$Variable <- abbreviate(x$Variable, minlength = minlength)
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
  if (length(x) != 2L) {
    stop("FUN should be a list of length 2.", call. = FALSE)
  }
  if (!identical(names(x), c("con", "cat"))) {
    stop("FUN should be a list with comonents \"con\" and \"cat\".",
         call. = FALSE)
  }
  if (!all(vapply(x, is.function, logical(1L)))) {
    stop("FUN should be a list of two functions.", call. = FALSE)
  }
}
