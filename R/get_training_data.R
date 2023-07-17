# Error message to display when training data cannot be extracted form object
msg <- paste0(
  "The training data could not be extracted from object. You can supply the ",
  "training data using the `train` argument."
)


#' @keywords internal
get_training_data <- function(object) {
  UseMethod("get_training_data")
}


#' @keywords internal
get_training_data.default <- function(object) {

  # # Throw error message for S4 objects (for now)
  # if (isS4(object)) {
  #   stop(msg, call. = FALSE)
  # }
  #
  # # Grab the call
  # mcall <- tryCatch(stats::getCall(object), error = function(e) {
  #   stop(msg, call. = FALSE)
  # })
  #
  # # If data component of the call is NULL, then try to make sure each
  # # component is named before proceeding (taken from Advanced R, 2nd ed.)
  # if (is.null(mcall[[arg]])) {
  #   f <- tryCatch(eval(mcall[[1L]], envir = env), error = function(e) {
  #     stop(msg, call. = FALSE)
  #   })
  #   if (!is.primitive(f)) {
  #     mcall <- match.call(f, call = mcall)
  #   }
  # }
  #
  # # Grab the data component (if it exists)
  # n <- 1
  # while(length(env) != 0) {
  #   train <- tryCatch(eval(mcall[[arg]], envir = env), error = function(e) {
  #     NULL
  #   })
  #   if (!is.null(train) || identical(env, globalenv())) {
  #     break
  #   }
  #   env <- parent.frame(n)  # inspect calling environment
  #   n <- n + 1
  # }
  # if (is.null(train)) {
  #   stop(msg, call. = FALSE)
  # } else {
  #   if (!(is.data.frame(train))) {
  #     if (is.matrix(train) || is.list(train)) {
  #       train <- as.data.frame(train)
  #       # } else if (inherits(train, what = "dgCMatrix")) {
  #       #   train <- as.data.frame(data.matrix(train))
  #     } else {
  #       stop(msg, call. = FALSE)
  #     }
  #   }
  # }
  #
  # # Return original training data
  # train
  stop("Training data cannot be extracted from fitted model object. Please ",
       "supply the raw training data using the `train` argument.",
       call. = FALSE)

}


# Package: caret ---------------------------------------------------------------

#' @keywords internal
get_training_data.train <- function(object) {
  # By default, "train" object have a copy of the training data stored in
  # a component called "trainingData". Note that the returned data frame only
  # includes the feature columns
  train <- object$trainingData
  if (is.null(train)) {
    stop(msg, call. = FALSE)
  }
  train$.outcome <- NULL  # remove .outcome column
  train
}


# Package: h2o -----------------------------------------------------------------

#' @keywords internal
get_training_data.H2OBinomialModel <- function(object) {
  as.data.frame(h2o::h2o.getFrame(object@allparameters$training_frame))
}


#' @keywords internal
get_training_data.H2OMultinomialModel <- function(object) {
  as.data.frame(h2o::h2o.getFrame(object@allparameters$training_frame))
}


#' @keywords internal
get_training_data.H2ORegressionModel <- function(object) {
  as.data.frame(h2o::h2o.getFrame(object@allparameters$training_frame))
}


# Package: party ---------------------------------------------------------------

#' @keywords internal
get_training_data.BinaryTree <- function(object) {
  # WARNING: Returns feature columns only in a data frame with some additional
  # attributes
  object@data@get("input")
}


#' @keywords internal
get_training_data.RandomForest <- function(object) {
  # WARNING: Returns feature columns only in a data frame with some additional
  # attributes
  object@data@get("input")
}

library
# Package: workflow ------------------------------------------------------------

#' @keywords internal
get_training_data.workflow <- function(object) {
  stop("Training data cannot be extracted from workflow objects. Please ",
       "supply the raw training data using the `train` argument.",
       call. = FALSE)
}
