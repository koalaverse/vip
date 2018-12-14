#' Permutation-Based Variable Importance
#'
#' Compute permutation-based variable importance scores for the predictors in a
#' model. (This function is meant for internal use only.)
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param train A matrix-like R object (e.g., a data frame or matrix)
#' containing the training data.
#'
#' @param target Either a character string giving the name (or position) of the
#' target column in \code{train} or, if \code{train} only contains feature
#' columns, a vector containing the target values used to train \code{object}.
#'
#' @param metric Either a function or character string specifying the
#' performance metric to use in computing model performance (e.g., RMSE for
#' regression or accuracy for binary classification). If \code{metric} is a
#' function, then it requires two arguments, \code{actual} and \code{predicted},
#' and should return a single, numeric value. Ideally, this should be the same
#' metric that was used to train \code{object}.
#'
#' @param smaller_is_better Logical indicating whether or not a smaller value
#' of \code{metric} is better. Default is \code{NULL}. Must be supplied if
#' \code{metric} is a user-supplied function.
#'
#' @param nsim Integer specifying the number of Monte Carlo replications to
#' perform. Default is 1. If \code{nsim > 1}, the results from each replication
#' are simply averaged together (the standard deviation will also be returned).
#'
#' @param sample_size Integer specifying the size of the random sample to use
#' for each Monte Carlo repitition. Default is \code{NULL} (i.e., use all of the
#' available training data). Cannot be specified with \code{sample_frac}. Can be
#' used to reduce computation time with large data sets.
#'
#' @param sample_frac Proportion specifying the size of the random sample to use
#' for each Monte Carlo repitition. Default is \code{NULL} (i.e., use all of the
#' available training data). Cannot be specified with \code{sample_size}. Can be
#' used to reduce computation time with large data sets.
#'
#' @param reference_class Character string specifying which response category
#' represents the "reference" class (i.e., the class for which the predicted
#' class probabilities correspond to). Only needed for binary classification
#' problems.
#'
#' @param pred_fun Optional prediction function that requires two arguments,
#' \code{object} and \code{newdata}. Default is \code{NULL}. Must be supplied
#' whenever \code{metric} is a custom function.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns:
#' \code{Variable} and \code{Importance}.
#'
#' @param verbose Logical indicating whether or not to print information during
#' the construction of variable importance scores. Default is \code{FALSE}.
#'
#' @param progress Character string giving the name of the progress bar to use.
#' See \code{\link[plyr]{create_progress_bar}} for details. Default is
#' \code{"none"}.
#'
#' @param parallel Logical indicating whether or not to run \code{vi_permute()}
#' in parallel (using a backend provided by the \code{foreach} package). Default
#' is \code{FALSE}. If \code{TRUE}, an appropriate backend must be provided by
#' \code{foreach}.
#'
#' @param paropts List containing additional options to be passed onto
#' \code{foreach} when \code{parallel = TRUE}.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @details Coming soon!
#'
#' @rdname vi_permute
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(ggplot2)  # for ggtitle() function
#' library(mlbench)  # for ML benchmark data sets
#' library(nnet)     # for fitting neural networks
#'
#' # Simulate training data
#' set.seed(101)  # for reproducibility
#' trn <- as.data.frame(mlbench.friedman1(500))  # ?mlbench.friedman1
#'
#' # Inspect data
#' tibble::as.tibble(trn)
#'
#' # Fit PPR and NN models (hyperparameters were chosen using the caret package
#' # with 5 repeats of 5-fold cross-validation)
#' pp <- ppr(y ~ ., data = trn, nterms = 11)
#' set.seed(0803) # for reproducibility
#' nn <- nnet(y ~ ., data = trn, size = 7, decay = 0.1, linout = TRUE,
#'            maxit = 500)
#'
#' # Plot VI scores
#' set.seed(2021)  # for reproducibility
#' p1 <- vip(pp, method = "permute", target = "y", metric = "rsquared",
#'           pred_fun = predict) + ggtitle("PPR")
#' p2 <- vip(nn, method = "permute", target = "y", metric = "rsquared",
#'           pred_fun = predict) + ggtitle("NN")
#' grid.arrange(p1, p2, ncol = 2)
#'
#' # Mean absolute error
#' mae <- function(actual, predicted) {
#'   mean(abs(actual - predicted))
#' }
#'
#' # Permutation-based VIP with user-defined MAE metric
#' set.seed(1101)  # for reproducibility
#' vip(pp, method = "permute", target = "y", metric = mae,
#'     smaller_is_better = TRUE,
#'     pred_fun = function(object, newdata) predict(object, newdata)  # wrapper
#' ) + ggtitle("PPR")
#' }
vi_permute <- function(object, ...) {
  UseMethod("vi_permute")
}


#' @rdname vi_permute
#'
#' @export
vi_permute.default <- function(object, train, target, metric = "auto",
  smaller_is_better = NULL, nsim = 1, sample_size = NULL, sample_frac = NULL,
  reference_class = NULL, pred_fun = NULL, verbose = FALSE, progress = "none",
  parallel = FALSE, paropts = NULL, ...
) {

  # Issue warning until this function is complete!
  warning("Setting `method = \"permute\"` is experimental, use at your own ",
          "risk!", call. = FALSE)

  # Get training data, if not supplied
  if (missing(train)) {
    train <- get_training_data(object)
  }

  # Extract feature names and separate features from target (if necessary)
  if (is.character(target)) {
    feature_names <- setdiff(colnames(train), target)
    train_x <- train[, feature_names]
    train_y <- train[, target, drop = TRUE]
  } else {
    feature_names <- colnames(train)
    train_x <- train
    train_y <- target
  }

  # Sample the data?
  if (!is.null(sample_size) && !is.null(sample_frac)) {
    stop("Arguments `sample_size` and `sample_frac` cannot both be specified.")
  }
  if (!is.null(sample_size)) {
    if (sample_size <= 0 || sample_size > nrow(train)) {
      stop("Argument `sample_size` must be in (0, ", nrow(train), "].")
    }
  }
  if (!is.null(sample_frac)) {
    if (sample_frac <= 0 || sample_frac > 1) {
      stop("Argument `sample_frac` must be in (0, 1].")
    }
    sample_size <- round(nrow(train) * sample_frac, digits = 0)
  }

  # Metric
  if (is.function(metric)) {  # user-supplied function

    # If `metric` is a user-supplied function, then `smaller_is_better` cannot
    # be `NULL`.
    if (is.null(smaller_is_better) || !is.logical(smaller_is_better)) {
      stop("Please specify a logical value for `smaller_is_better`.",
           call. = FALSE)
    }

    # If `metric` is a user-supplied function, then `pred_fun` cannot
    # be `NULL`.
    if (is.null(smaller_is_better)) {
      stop("Please specify a logical value for `smaller_is_better`.",
           call. = FALSE)
    } else {
      # Check prediction function arguments
      if (!all(c("object", "newdata") %in% names(formals(pred_fun)))) {
        stop("`pred_fun()` must be a function with arguments `object` and ",
             "`newdata`.", call. = FALSE)
      }
    }

    # Check prediction function arguments
    if (!identical(c("actual", "predicted"), names(formals(metric)))) {
      stop("`metric()` must be a function with arguments `actual` and ",
           "`predicted`.", call. = FALSE)
    }

    # Performance function
    perf_fun <- metric

  } else {

    # Performance metric
    metric <- if (metric == "auto") {
      get_default_metric(object)
    } else {
      tolower(metric)
    }

    # Performance function
    perf_fun <- switch(metric,

      # Classification
      "auc" = perf_auc,            # requires predicted class probabilities
      "error" = perf_ce,           # requires predicted class labels
      "logloss" = perf_logLoss,    # requires predicted class probabilities
      "mauc" = perf_mauc,          # requires predicted class probabilities
      "mlogloss" = perf_mlogLoss,  # requires predicted class probabilities

      # Regression
      "mse" = perf_mse,
      "r2" = perf_rsquared,
      "rsquared" = perf_rsquared,
      "rmse" = perf_rmse,

      # Return informative error
      stop("Metric \"", metric, "\" is not supported.")

    )

    # Is smaller better?
    smaller_is_better <- switch(metric,

      "auto" = TRUE,

      # Classification
      "auc" = FALSE,
      "error" = TRUE,
      "logloss" = TRUE,
      "mauc" = FALSE,
      "mlogloss" = TRUE,

      # Regression
      "mse" = TRUE,
      "r2" = FALSE,
      "rsquared" = FALSE,
      "rmse" = TRUE,

      # Return informative error
      stop("Metric \"", metric, "\" is not supported.")

    )

    # Get prediction function, if not supplied
    prob_based_metrics <- c("auc", "mauc", "logloss", "mlogloss")
    if (is.null(pred_fun)) {
      type <- if (metric %in% prob_based_metrics) {
        "prob"
      } else {
        "raw"
      }
      pred_fun <- get_predictions(object, type = type)
    }

    # Determine reference class (binary classification only)
    if (is.null(reference_class) && metric %in% c("auc", "logloss")) {
      stop("Please specify the reference class via the `reference_class` ",
           "argument when using \"auc\" or \"logloss\".")
    }
    if (!is.null(reference_class) && metric %in% c("auc", "logloss")) {
      train_y <- ifelse(train_y == reference_class, yes = 1, no = 0)
    }

  }

  # Compute baseline metric for comparison
  baseline <- perf_fun(
    actual = train_y,
    predicted = pred_fun(object, newdata = train_x)
  )

  # Construct VI scores
  #
  # Loop through each feature and do the following:
  #
  #   1. make a copy of the training data;
  #   2. permute the values of the original feature;
  #   3. get new predictions based on permuted data set;
  #   4. record difference in accuracy.
  vis <- replicate(nsim,
    unlist(plyr::llply(feature_names, .progress = progress,
      .parallel = parallel, .paropts = paropts,
      .fun = function(x) {
        if (verbose && !parallel) {
          message("Computing variable importance for ", x, "...")
        }
        if (!is.null(sample_size)) {
          ids <- sample(length(train_y), size = sample_size, replace = FALSE)
          train_x <- train_x[ids, ]
          train_y <- train_y[ids]
        }
        train_x_permuted <- train_x  # make copy
        train_x_permuted[[x]] <- sample(train_x_permuted[[x]])  # permute values
        permuted <- perf_fun(
          actual = train_y,
          predicted = pred_fun(object, newdata = train_x_permuted)
        )
        if (smaller_is_better) {
          permuted - baseline
        } else {
          baseline - permuted
        }
      })
  ))
  # vis <- unlist(plyr::llply(feature_names, .progress = progress,
  #   .parallel = parallel, .paropts = paropts,
  #   .fun = function(x) {
  #     if (verbose && !parallel) {
  #       message("Computing variable importance for ", x, "...")
  #     }
  #     train_x_permuted <- train_x  # make copy
  #     train_x_permuted[[x]] <- sample(train_x_permuted[[x]])  # permute values
  #     permuted <- perf_fun(
  #       actual = train_y,
  #       predicted = pred_fun(object, newdata = train_x_permuted)
  #     )
  #     if (smaller_is_better) {
  #       permuted - baseline
  #     } else {
  #       baseline - permuted
  #     }
  #   })
  # )

  # Construct tibble of variable importance scores
  tib <- tibble::tibble(
    "Variable" = feature_names,
    "Importance" = apply(vis, MARGIN = 1, FUN = mean)
  )
  if (nsim > 1) {
    tib$StDev <- apply(vis, MARGIN = 1, FUN = sd)
  }

  # Add variable importance type attribute
  attr(tib, which = "type") <- "permutation"

  # Return results
  tib

}
