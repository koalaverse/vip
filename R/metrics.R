#' List metrics
#'
#' List all available performance metrics.
#'
#' @return A data frame with the following columns:
#' * `metric` - the optimization or tuning metric;
#' * `description` - a brief description about the metric;
#' * `task` - whether the metric is suitable for regression or classification;
#' * `smaller_is_better` - logical indicating whether or not a smaller value of
#' the metric is considered better.
#' * `yardstick_function` - the name of the corresponding function from the
#' [yardstick][yardstick::yardstick] package.
#'
#' @export
#'
#' @examples
#' (metrics <- list_metrics())
#' metrics[metrics$task == "Multiclass classification", ]
list_metrics <- function() {
  data.frame(rbind(
    #
    # Classification
    #
    c("metric"      = "accuracy",
      "description" = "Classification accuracy",
      "task"        = "Binary/multiclass classification",
      "smaller_is_better" = FALSE,
      "yardstick_function" = "accuracy_vec"
    ),
    c("metric"      = "bal_accuracy",
      "description" = "Balanced classification accuracy",
      "task"        = "Binary/multiclass classification",
      "smaller_is_better" = FALSE,
      "yardstick_function" = "bal_accuracy_vec"
    ),
    c("metric"      = "youden",
      "description" = "Youden;'s index (or Youden\'s J statistic)",
      "task"        = "Binary/multiclass classification",
      "smaller_is_better" = FALSE,
      "yardstick_function" = "j_index"
    ),
    c("metric"      = "roc_auc",
      "description" = "Area under ROC curve",
      "task"        = "Binary classification",
      "smaller_is_better" = FALSE,
      "yardstick_function" = "roc_auc_vec"
    ),
    c("metric"      = "pr_auc",
      "description" = "Area under precision-recall (PR) curve",
      "task"        = "Binary classification",
      "smaller_is_better" = FALSE,
      "yardstick_function" = "pr_auc_vec"
    ),
    c("metric"      = "logloss",
      "description" = "Log loss",
      "task"        = "Binary/multiclass classification",
      "smaller_is_better" = TRUE,
      "yardstick_function" = "mn_log_loss_vec"
    ),
    c("metric"      = "brier",
      "description" = "Brier score",
      "task"        = "Binary/multiclass classification",
      "smaller_is_better" = TRUE,
      "yardstick_function" = "brier_class_vec"
    ),
    #
    # Regression
    #
    c("metric"      = "mae",
      "description" = "Mean absolute error",
      "task"        = "Regression",
      "smaller_is_better" = TRUE,
      "yardstick_function" = "mae_vec"
    ),
    c("metric"      = "mape",
      "description" = "Mean absolute percentage error",
      "task"        = "Regression",
      "smaller_is_better" = TRUE,
      "yardstick_function" = "mape_vec"
    ),
    c("metric"      = "rmse",
      "description" = "Root mean squared error",
      "task"        = "Regression",
      "smaller_is_better" = TRUE,
      "yardstick_function" = "rmse_vec"
    ),
    c("metric"      = "rsq",
      "description" = "R-squared (correlation)",
      "task"        = "Regression",
      "smaller_is_better" = FALSE,
      "yardstick_function" = "rsq_vec"
    ),
    c("metric"      = "rsq_trad",
      "description" = "R-squared (traditional)",
      "task"        = "Regression",
      "smaller_is_better" = FALSE,
      "yardstick_function" = "rsq_trad_vec"
    )
  ), stringsAsFactors = FALSE)
}


#' Get yardstick metric
#'
#' Grabs the corresponding function from yardstick based on provided string
#' description.
#'
#' @param metric String giving the name of the metric
#'
#' @return A list with two components:
#'
#' * `metric_fun` - the corresponding function from
#' [yardstick][yardstick::yardstick].
#' * `smaller_is_better` - a logical indicating whether or not a smaller value
#' of this metric is better.
#'
#' @keywords internal
#' @noRd
get_metric <- function(metric) {

  metric <- tolower(metric)  # just in case

  # Classification
  if (metric == "accuracy") {
    metric_fun <- yardstick::accuracy_vec
    smaller_is_better <- FALSE
  } else if (metric == "bal_accuracy") {
    metric_fun <- yardstick::bal_accuracy_vec
    smaller_is_better <- FALSE
  } else if (metric == "youden") {
    metric_fun <- yardstick::j_index_vec
    smaller_is_better <- FALSE
  } else if (metric == "roc_auc") {
    metric_fun <- yardstick::roc_auc_vec
    smaller_is_better <- FALSE
  } else if (metric == "pr_auc") {
    metric_fun <- yardstick::pr_auc_vec
    smaller_is_better <- FALSE
  } else if (metric == "logloss") {
    metric_fun <- yardstick::mn_log_loss_vec
    smaller_is_better <- TRUE
  } else if (metric == "brier") {
    metric_fun <- yardstick::brier_class_vec
    smaller_is_better <- TRUE

    # Regression
  } else if (metric == "rsq") {
    metric_fun <- yardstick::rsq_vec
    smaller_is_better <- FALSE
  } else if (metric == "rsq_trad") {
    metric_fun <- yardstick::rsq_trad_vec
    smaller_is_better <- FALSE
  } else if (metric == "rmse") {
    metric_fun <- yardstick::rmse_vec
    smaller_is_better <- TRUE
  } else if (metric == "mae") {
    metric_fun <- yardstick::mae_vec
    smaller_is_better <- TRUE
  } else if (metric == "mape") {
    metric_fun <- yardstick::mape_vec
    smaller_is_better <- FALSE
  } else {
    # Return informative error
    stop("Metric \"", metric, "\" is not supported; use ",
         "`vip::list_metrics()` to print a list of currently supported ",
         "metrics. Alternatively, you can pass in a `yardstick` vector ",
         "function directly (e.g., `metric = yardstick::poisson_log_loss_vec` ",
         "(just be sure to also set the `smaller_is_better` argument.",
         call. = FALSE)
  }
  list("metric_fun" = metric_fun, "smaller_is_better" = smaller_is_better)
}
