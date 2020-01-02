#' @keywords internal
get_pfun <- function(object, type = c("raw", "prob")) {
  UseMethod("get_pfun")
}


#' @keywords internal
get_pfun.default <- function(object, type = c("raw", "prob")) {
  # function(object, newdata, ...) {
  #   stats::predict(object, newdata = newdata)
  # }
  stop("Could not find a suitable prediction function for computing ",
       "permutation-based variable importance. Please supply a valid function ",
       "via the `pred_wrapper` argument; see `?vip::vi_permute` for details.",
       call. = FALSE)
}


# Package: earth ---------------------------------------------------------------

#' @keywords internal
get_pfun.earth <- function(object, type = c("raw", "prob")) {
  type <- match.arg(type)
  if (type == "raw") {
    type <- "link"
  } else {
    type <- "response"
  }
  function(object, newdata) {
    stats::predict(object, newdata = newdata, type = type)[, 1L, drop = TRUE]
  }
}


# Package: ranger --------------------------------------------------------------

#' @keywords internal
get_pfun.ranger <- function(object, type = c("raw", "prob")) {
  type <- match.arg(type)
  if (object$treetype %in% c("Classification", "Regression")) {
    if (type == "prob") {
      stop("Could not obtain predicted class probabilities. Try setting ",
           "`probability = TRUE` in the call to `ranger::ranger()`.")
    }
    function(object, newdata) {
      stats::predict(object, data = newdata)$predictions
    }
  } else if (object$treetype == "Probability estimation") {
    if (type != "prob") {
      stop("Could not obtain predicted class labels. Try setting ",
           "`probability = FALSE` in the call to `ranger::ranger()`.")
    }
    function(object, newdata) {
      stats::predict(object, data = newdata)$predictions
    }
  } else {
    stop("Only classification and regression are currently supported.")
  }
}
