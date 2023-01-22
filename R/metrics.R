#' List metrics
#'
#' List all available performance metrics.
#' 
#' @return A data frame with the following columns:
#' * `metric` - the optimization or tuning metric;
#' * `description` - a brief description about the metric;
#' * `task` - whether the metric is suitable for regression or classification;
#' * `smaller_is_better` - logical indicating whether or not a smaller value of the metric is considered better.
#'
#' @export
#'
#' @examples
#' (metrics <- list_metrics())
#' metrics[metrics$task == "Multiclass classification", ]
list_metrics <- function() {
  data.frame(rbind(
    c("metric"      = "accuracy",
      "description" = "Classification accuracy",
      "task"        = "Binary/multiclass classification",
      "smaller_is_better" = FALSE
    ),
    c("metric"      = "error",
      "description" = "Misclassification error",
      "task"        = "Binary/multiclass classification",
      "smaller_is_better" = TRUE
    ),
    c("metric"      = "auc",
      "description" = "Area under (ROC) curve",
      "task"        = "Binary classification",
      "smaller_is_better" = FALSE
    ),
    c("metric"      = "logloss",
      "description" = "Log loss",
      "task"        = "Binary classification",
      "smaller_is_better" = TRUE
    ),
    c("metric"      = "mauc",
      "description" = "Multiclass area under (ROC) curve",
      "task"        = "Multiclass classification",
      "smaller_is_better" = FALSE
    ),
    # c("metric"      = "mlogloss",
    #   "description" = "Multiclass log loss",
    #   "task"        = "Multiclass classification",
    #   "smaller_is_better" = TRUE
    # ),
    c("metric"      = "mae",
      "description" = "Mean absolute error",
      "task"        = "Regression",
      "smaller_is_better" = TRUE
    ),
    c("metric"      = "mse",
      "description" = "Mean squared error",
      "task"        = "Regression",
      "smaller_is_better" = TRUE
    ),
    c("metric"      = "r2",
      "description" = "R squared",
      "task"        = "Regression",
      "smaller_is_better" = FALSE
    ),
    c("metric"      = "rsquared",
      "description" = "R squared",
      "task"        = "Regression",
      "smaller_is_better" = FALSE
    ),
    c("metric"      = "rmse",
      "description" = "Root mean squared error",
      "task"        = "Regression",
      "smaller_is_better" = TRUE
    ),
    c("metric"      = "sse",
      "description" = "Sum of squared errors",
      "task"        = "Regression",
      "smaller_is_better" = TRUE
    )
  ), stringsAsFactors = FALSE)
}


# Regression -------------------------------------------------------------------

#' Model metrics
#'
#' Common model/evaluation metrics for machine learning.
#'
#' @param actual Vector of actual target values.
#'
#' @param predicted Vector of predicted target values.
#'
#' @param na.rm Logical indicating whether or not \code{NA} values should be
#' stripped before the computation proceeds.
#'
#' @note The \code{metric_auc} and \code{metric_logLoss} functions are based on
#' code from the \href{https://cran.r-project.org/package=Metrics}{Metrics}
#' package.
#'
#' @rdname metrics
#'
#' @export
#'
#' @examples
#' x <- rnorm(10)
#' y <- rnorm(10)
#' metric_mse(x, y)
#' metric_rsquared(x, y)
metric_mse <- function(actual, predicted, na.rm = FALSE) {
  mean((predicted - actual) ^ 2, na.rm = na.rm)
}


#' @rdname metrics
#'
#' @export
metric_rmse <- function(actual, predicted, na.rm = FALSE) {
  sqrt(mean((predicted - actual) ^ 2, na.rm = na.rm))
}


#' @rdname metrics
#'
#' @export
metric_sse <- function(actual, predicted, na.rm = FALSE) {
  sum((predicted - actual) ^ 2, na.rm = na.rm)
}


#' @rdname metrics
#'
#' @export
metric_mae <- function(actual, predicted, na.rm = FALSE) {
  mean(abs(predicted - actual), na.rm = na.rm)
}


#' @rdname metrics
#'
#' @export
metric_rsquared <- function(actual, predicted, na.rm = FALSE) {
  use <- if (na.rm) "complete.obs" else "everything"
  stats::cor(x = actual, y = predicted, use = use) ^ 2
}


# Classification ---------------------------------------------------------------


# Binary or multiclass classification (i.e., >=2 classes)

#' @rdname metrics
#'
#' @export
metric_accuracy <- function(actual, predicted, na.rm = FALSE) {
  mean(actual == predicted, na.rm = FALSE)
}


#' @rdname metrics
#'
#' @export
metric_error <- function(actual, predicted, na.rm = FALSE) {
  mean(actual != predicted, na.rm = FALSE)
}


# Binary classification (i.e., 2 classes)

#' @rdname metrics
#'
#' @export
metric_auc <- function(actual, predicted) {
  if (inherits(actual, "factor")) {
    actual <- as.integer(actual) - 1L
  } else if (inherits(actual, "character")) {
    actual <- as.integer(as.factor(actual)) - 1L
  }
  r <- rank(predicted)
  n_pos <- as.numeric(sum(actual == 1))
  n_neg <- length(actual) - n_pos
  (sum(r[actual == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}


#' @rdname metrics
#'
#' @export
metric_logLoss <- function(actual, predicted) {
  score <- -(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  score[actual == predicted] <- 0
  score[is.nan(score)] <- Inf
  mean(score)
}


# Multiclass classification (i.e., >2 classes)

#' @rdname metrics
#'
#' @export
metric_mauc <- function(actual, predicted) {
  if (NCOL(predicted) <= 2L) {
    stop("Expected a >2 column matrix of predicted class probabilities.")
  }
  classes <- unique(as.character(actual))
  aucs <- sapply(seq_along(classes), FUN = function(i) {  # one vs. all
    metric_auc(
      actual = (actual == classes[i]),
      predicted = predicted[, i, drop = TRUE]
    )
  })
  mean(aucs)
}


#' #' @rdname metrics
#' #'
#' #' @export
#' metric_mlogLoss <- function(actual, predicted) {
#'   if (NCOL(predicted) <= 2L) {
#'     stop("Expected a >2 column matrix of predicted class probabilities.")
#'   }
#'   ModelMetrics::mlogLoss(
#'     actual = actual,
#'     predicted = predicted
#'   )
#' }
