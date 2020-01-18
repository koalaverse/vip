#' Permutation-based variable importance
#'
#' Compute permutation-based variable importance scores for the predictors in a
#' model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest. If \code{NULL} (the default) then the
#' internal `get_feature_names()` function will be called to try and extract
#' them automatically. It is good practice to always specify this argument.
#'
#' @param train A matrix-like R object (e.g., a data frame or matrix)
#' containing the training data. If \code{NULL} (the default) then the
#' internal `get_training_data()` function will be called to try and extract it
#' automatically. It is good practice to always specify this argument.
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
#' metric that was used to train \code{object}. See \code{\link{list_metrics}}
#' for a list of built-in metrics.
#'
#' @param smaller_is_better Logical indicating whether or not a smaller value
#' of \code{metric} is better. Default is \code{NULL}. Must be supplied if
#' \code{metric} is a user-supplied function.
#'
#' @param type Character string specifying how to compare the baseline and
#' permuted performance metrics. Current options are \code{"difference"} (the
#' default) and \code{"ratio"}.
#'
#' @param nsim Integer specifying the number of Monte Carlo replications to
#' perform. Default is 1. If \code{nsim > 1}, the results from each replication
#' are simply averaged together (the standard deviation will also be returned).
#'
#' @param keep Logical indicating whether or not to keep the individual
#' permutation scores for all \code{nsim} repetitions. If \code{TRUE} (the
#' default) then the individual variable importance scores will be stored in an
#' attribute called \code{"raw_scores"}. (Only used when \code{nsim > 1}.)
#'
#' @param sample_size Integer specifying the size of the random sample to use
#' for each Monte Carlo repetition. Default is \code{NULL} (i.e., use all of the
#' available training data). Cannot be specified with \code{sample_frac}. Can be
#' used to reduce computation time with large data sets.
#'
#' @param sample_frac Proportion specifying the size of the random sample to use
#' for each Monte Carlo repetition. Default is \code{NULL} (i.e., use all of the
#' available training data). Cannot be specified with \code{sample_size}. Can be
#' used to reduce computation time with large data sets.
#'
#' @param reference_class Character string specifying which response category
#' represents the "reference" class (i.e., the class for which the predicted
#' class probabilities correspond to). Only needed for binary classification
#' problems.
#'
#' @param pred_fun Deprecated. Use \code{pred_wrapper} instead.
#'
#' @param pred_wrapper Prediction function that requires two arguments,
#' \code{object} and \code{newdata}. The output of this function should be
#' determined by the \code{metric} being used:
#'
#' \describe{
#'   \item{Regression}{A numeric vector of predicted outcomes.}
#'   \item{Binary classification}{A vector of predicted class labels (e.g., if
#'   using misclassification error) or a vector of predicted class probabilities
#'   for the reference class (e.g., if using log loss or AUC).}
#'   \item{Multiclass classification}{A vector of predicted class labels (e.g.,
#'   if using misclassification error) or a A matrix/data frame of predicted
#'   class probabilities for each class (e.g., if using log loss or AUC).}
#' }
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
#' @param paropts List containing additional options to be passed on to
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
#' library(nnet)     # for fitting neural networks
#'
#' # Simulate training data
#' trn <- gen_friedman(500, seed = 101)  # ?vip::gen_friedman
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
#'           pred_wrapper = predict) + ggtitle("PPR")
#' p2 <- vip(nn, method = "permute", target = "y", metric = "rsquared",
#'           pred_wrapper = predict) + ggtitle("NN")
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
#'     pred_wrapper = function(object, newdata) predict(object, newdata)
#' ) + ggtitle("PPR")
#' }
vi_permute <- function(object, ...) {
  UseMethod("vi_permute")
}


#' @rdname vi_permute
#'
#' @export
vi_permute.default <- function(
  object,
  feature_names = NULL,
  train = NULL,
  target = NULL,
  metric = NULL,
  smaller_is_better = NULL,
  type = c("difference", "ratio"),
  nsim = 1,
  keep = TRUE,
  sample_size = NULL,
  sample_frac = NULL,
  reference_class = NULL,
  pred_fun = NULL,
  pred_wrapper = NULL,
  verbose = FALSE,
  progress = "none",
  parallel = FALSE,
  paropts = NULL,
  ...
) {

  # # Issue warning until this function is complete!
  # warning("Setting `method = \"permute\"` is experimental, use at your own ",
  #         "risk!", call. = FALSE)

  # Catch deprecated arguments
  if (!is.null(pred_fun)) {
    stop("Argument `pred_fun` is deprecated; please use `pred_wrapper` ",
         "instead.", call. = FALSE)
  }

  # # Try to extract feature names if not supplied
  # if (is.null(feature_names)) {
  #   feature_names <- get_feature_names(object)
  # }

  # Try to extract training data if not supplied
  if (is.null(train)) {
    train <- get_training_data(object)
  }

  # Throw informative error messages if required arguments are missing
  if (is.null(target)) {
    stop("Could not find target. Please specify a target variable via the ",
         "`target` argument; see `?vip::vi_permute` for details.",
         call. = FALSE)
  }
  if (is.null(metric)) {
    stop("Could not find metric. Please specify a valid metric via the ",
         "`metric` argument; see `?vip::vi_permute` for details.",
         call. = FALSE)
  }
  if (is.null(pred_wrapper)) {
    stop("Could not find prediction wrapper. Please specify a valid prediction ",
         "function via the `pred_wrapper` argument; see `?vip::vi_permute` ",
         "for details.", call. = FALSE)
  }

  # Extract feature names and separate features from target (if necessary)
  if (is.character(target)) {
    if (is.null(feature_names)) {
      feature_names <- setdiff(colnames(train), target)
    }
    train_x <- train[, feature_names]
    train_y <- train[, target, drop = TRUE]
  } else {
    if (is.null(feature_names)) {
      feature_names <- colnames(train)
    }
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

    # Check prediction function arguments
    if (!all(c("object", "newdata") %in% names(formals(pred_wrapper)))) {
      stop("`pred_wrapper()` must be a function with arguments `object` and ",
           "`newdata`.", call. = FALSE)
    }

    # Check metric function arguments
    if (!all(c("actual", "predicted") %in% names(formals(metric)))) {
      stop("`metric()` must be a function with arguments `actual` and ",
           "`predicted`.", call. = FALSE)
    }

    # # Check if reference class is provided
    # if (!is.null(reference_class)) {
    #   reference_class <- train_y[1L]
    # }
    # train_y <- ifelse(train_y == reference_class, yes = 1, no = 0)

    # Performance function
    mfun <- metric

  } else {

    # Convert metric string to lowercase
    metric <- tolower(metric)

    # Get corresponding metric/performance function
    mfun <- switch(metric,

      # Classification
      "accuracy" = metric_accuracy,  # requires predicted class labels
      "error" = metric_error,        # requires predicted class labels
      "auc" = metric_auc,            # requires predicted class probabilities
      "logloss" = metric_logLoss,    # requires predicted class probabilities
      "mauc" = metric_mauc,          # requires predicted class probabilities
      # "mlogloss" = metric_mlogLoss,  # requires predicted class probabilities

      # Regression
      "mae" = metric_mae,
      "mse" = metric_mse,
      "r2" = metric_rsquared,
      "rsquared" = metric_rsquared,
      "rmse" = metric_rmse,
      "sse" = metric_sse,

      # Return informative error
      stop("Metric \"", metric, "\" is not supported; use ",
           "`vip::list_metrics()` to print a list of currently supported ",
           "metrics.", call. = FALSE)

    )

    # Is smaller better?
    smaller_is_better <- switch(metric,

      # Classification
      "accuracy" = FALSE,
      "error" = TRUE,
      "auc" = FALSE,
      "logloss" = TRUE,
      "mauc" = FALSE,
      # "mlogloss" = TRUE,

      # Regression
      "mae" = TRUE,
      "mse" = TRUE,
      "r2" = FALSE,
      "rsquared" = FALSE,
      "rmse" = TRUE,
      "sse" = TRUE,

      # Return informative error
      stop("Metric \"", metric, "\" is not supported.")

    )

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
  baseline <- mfun(
    actual = train_y,
    predicted = pred_wrapper(object, newdata = train_x)
  )

  # Type of comparison
  type <- match.arg(type)
  `%compare%` <- if (type == "difference") {
    `-`
  } else {
    `/`
  }

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
        # train_x_permuted <- train_x  # make copy
        # train_x_permuted[[x]] <- sample(train_x_permuted[[x]])  # permute values
        train_x_permuted <- permute_columns(train_x, columns = x)
        permuted <- mfun(
          actual = train_y,
          predicted = pred_wrapper(object, newdata = train_x_permuted)
        )
        if (smaller_is_better) {
          permuted %compare% baseline  # e.g., RMSE
        } else {
          baseline %compare% permuted  # e.g., R-squared
        }
      })
  ))

  # Construct tibble of variable importance scores
  tib <- tibble::tibble(
    "Variable" = feature_names,
    "Importance" = apply(vis, MARGIN = 1, FUN = mean)
  )
  if (nsim > 1) {
    tib$StDev <- apply(vis, MARGIN = 1, FUN = stats::sd)
  }

  # Add all nsim scores as an attribute
  if (nsim > 1 && keep) {
    rownames(vis) <- feature_names
    colnames(vis) <- paste0("permutation_", seq_len(ncol(vis)))
    attr(tib, which = "raw_scores") <- vis
  }

  # Add variable importance type attribute
  attr(tib, which = "type") <- "permutation"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}
