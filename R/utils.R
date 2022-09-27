#' @importFrom magrittr %T>%
#' @export
magrittr::`%T>%`


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @keywords internal
abbreviate_names <- function(x, minlength) {
  x$Variable <- abbreviate(x$Variable, minlength = minlength)
  x
}


#' @keywords internal
check_var_fun <- function(x) {
  # x should be a named list of two functions with names "con" and "cat"
  if (!is.list(x)) {
    stop("Argument `var_fun` should be a list.", call. = FALSE)
  }
  if (length(x) != 2L) {
    stop("FUN should be a list of length 2.", call. = FALSE)
  }
  if (!identical(sort(names(x)), c("cat", "con"))) {
    stop("Argument `var_fun` should be a list with comonents \"con\" and \"cat\".",
         call. = FALSE)
  }
  if (!all(vapply(x, is.function, logical(1L)))) {
    stop("Argument `var_fun` should be a list of two functions.", call. = FALSE)
  }
}


#' @keywords internal
permute_columns <- function(x, columns = NULL) {
  if (is.null(columns)) {
    stop("No columns specified for permutation.")
  }
  x[, columns] <- x[sample(nrow(x)), columns]
  x
}


#' @keywords internal
sort_importance_scores <- function(x, decreasing) {
  x[order(x$Importance, decreasing = decreasing), ]
}
