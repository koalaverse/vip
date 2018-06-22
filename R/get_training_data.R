# Error message to display when training data cannot be extracted form object
err_mssg <- paste0(
  "The training data could not be extracted from object. You can supply the ",
  "training data using the `train` argument in the call to `vi_model()`."
)


#' @keywords internal
get_training_data <- function(object) {
  UseMethod("get_training_data")
}


#' @keywords internal
get_training_data.default <- function(object) {
  if (isS4(object)) {
    stop(err_mssg)
  } else {
    train <- eval(stats::getCall(object)$data)
    if (is.null(train)) {
      stop(err_mssg)
    } else {
      if (!(is.data.frame(train))) {
        if (is.matrix(train) || is.list(train)) {
          train <- as.data.frame(train)
        } else {
          stop(err_mssg)
        }
      }
    }
  }
  train
}


#' @keywords internal
get_training_data.BinaryTree <- function(object) {
  object@data@get("input")
}


#' @keywords internal
get_training_data.cforest <- function(object) {
  stop(err_mssg)
}


#' @keywords internal
get_training_data.ctree <- function(object) {
  stop(err_mssg)
}


#' @keywords internal
get_training_data.RandomForest <- function(object) {
  object@data@get("input")
}


#' @keywords internal
get_training_data.train <- function(object) {
  # By default, "train" object store a copy of the training data in a component
  # called "trainingData"
  train <- object$trainingData
  if (is.null(train)) {
    stop(err_mssg)
  }
  train$.outcome <- NULL  # remove .outcome column
  train
}
