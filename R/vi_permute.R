#' Permutation-based variable importance
#'
#' Compute permutation-based variable importance scores for the predictors in a
#' model; for details on the algorithm, see Greenwell and Boehmke (2020).
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the predictor
#' variables (i.e., features) of interest. If `NULL` (the default) then they
#' will be inferred from the `train` and `target` arguments (see below). It is
#' good practice to always specify this argument.
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
#' @param reference_class Deprecated, use `event_level` instead.
#'
#' @param event_level String specifying which factor level of `truth` to
#' consider as the "event". Options are `"first"` (the default) or `"second"`.
#' This argument is only applicable for binary classification when `metric` is
#' one of `"roc_auc"`, `"pr_auc"`, or `"youden"`. This argument is passed on to
#' the corresponding [yardstick][yardstick::yardstick] metric.
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
#' @param verbose Logical indicating whether or not to print information during
#' the construction of variable importance scores. Default is `FALSE`.
#'
#' @param parallel Logical indicating whether or not to run `vi_permute()`
#' in parallel (using a backend provided by the [foreach][foreach::foreach]
#' package). Default is `FALSE`. If `TRUE`, a
#' [foreach][foreach::foreach]-compatible backend must be provided by must be
#' provided.
#'
#' @param parallelize_by Character string specifying whether to parallelize
#' across features (\code{parallelize_by = "features"}) or repetitions
#' (\code{parallelize_by = "reps"}); the latter is only useful whenever
#' \code{nsim > 1}. Default is \code{"features"}.
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{\link[foreach]{foreach}}.
#'
#' @return A tidy data frame (i.e., a [tibble][tibble::tibble] object) with two
#' columns:
#'
#' * `Variable` - the corresponding feature name;
#' * `Importance` - the associated importance, computed as the average change in
#' performance after a random permutation (or permutations, if `nsim > 1`) of
#' the feature in question.
#'
#' If `nsim > 1`, then an additional column (`StDev`) containing the standard
#' deviation of the individual permutation scores for each feature is also
#' returned; this helps assess the stability/variation of the individual
#' permutation importance for each feature.
#'
#' @importFrom foreach foreach %do% %dopar%
#'
#' @references
#' Brandon M. Greenwell and Bradley C. Boehmke, The R Journal (2020) 12:1,
#' pages 343-366.
#'
#' @rdname vi_permute
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(ggplot2)  # for ggtitle() function
#' library(ranger)   # for fitting random forests
#'
#' #
#' # Regression example
#' #
#'
#' # Simulate training data
#' trn <- gen_friedman(500, seed = 101)  # ?vip::gen_friedman
#'
#' # Inspect data
#' tibble::as_tibble(trn)
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
#'
#' #
#' # Classification example
#' #
#'
#' # Complete (i.e., imputed version of titanic data); see `?vip::titanic_mice`
#' head(t1 <- titanic_mice[[1L]])
#'
#' # Fit a (default) probability forest
#' set.seed(1150)  # for reproducibility
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
  reference_class = NULL,  # deprecated
  event_level = NULL,
  pred_wrapper = NULL,  # FIXME: Why give this a default?
  verbose = FALSE,
  parallel = FALSE,
  parallelize_by = c("features", "repetitions"),
  ...
) {

  # FIXEME: Is there a better way to fix this?
  #
  # ❯ checking R code for possible problems ... NOTE
  # vi_permute.default: no visible binding for global variable ‘j’
  # Undefined global functions or variables:
  #   j
  i <- j <- NULL

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
    if (!all(c("truth", "estimate") %in% names(formals(metric)))) {
      stop("`metric()` must be a function with arguments `truth` and ",
           "`estimate`; consider using one of the vector metric functions ",
           "from the `yardstick` package (e.g., ",
           "`metric = yardstick::huber_loss_vec`).", call. = FALSE)
    }

    # # Check if reference class is provided
    # if (!is.null(reference_class)) {
    #   reference_class <- train_y[1L]
    # }
    # train_y <- ifelse(train_y == reference_class, yes = 1, no = 0)

    # Performance function
    metric_fun <- metric

  } else {

    # Get corresponding metric/performance function
    ys_metric <- get_metric(metric)
    # metric_fun <- ys_metric[["metric_fun"]]
    smaller_is_better <- ys_metric[["smaller_is_better"]]

    # Get metric function and update `event_level` arg if needed
    metric_fun <- if (!is.null(event_level)) {
      metric_fun <- function(truth, estimate) {
        fun <- ys_metric[["metric_fun"]]
        fun(truth, estimate = estimate, event_level = event_level)
      }
    } else {
      if (metric %in% c("roc_auc", "pr_auc", "youden")) {
        warning("Consider setting the `event_level` argument when using ",
                deparse(substitute(metric)), " as the metric; see ",
                "`?vip::vi_permute` for details. Defaulting to ",
                "`event_level = \"first\"`.", call. = FALSE)
      }
      ys_metric[["metric_fun"]]
    }

    # FIXME: How to handle this with new `yardstick` integration?
    # # Determine reference class (binary classification only)
    # if (is.null(reference_class) && metric %in% c("auc", "logloss")) {
    #   stop("Please specify the reference class via the `reference_class` ",
    #        "argument when using \"auc\" or \"logloss\".")
    # }
    # if (!is.null(reference_class) && metric %in% c("auc", "logloss")) {
    #   train_y <- ifelse(train_y == reference_class, yes = 1, no = 0)
    # }

  }

  # Compute baseline metric for comparison
  baseline <- metric_fun(
    truth = train_y,
    estimate = pred_wrapper(object, newdata = train_x)
  )

  # Type of comparison
  type <- match.arg(type)
  `%compare%` <- if (type == "difference") {
    `-`
  } else {
    `/`
  }

  # Define ".do" operator
  `%do.reps%` <- `%do.features%` <- `%do%`
  if (isTRUE(parallel)) {
    parallelize_by <- match.arg(parallelize_by)
    if (parallelize_by == "reps") {
      if (nsim == 1) {
        warning("Parallelizing across repititions only works when `nsim > 1`.",
                call. = FALSE)
      } else{
        `%do.reps%` <- `%dopar%`
      }
    } else {
      `%do.features%` <- `%dopar%`
    }
  }

  # Construct VI scores
  #
  # Loop through each feature and do the following:
  #
  #   1. make a copy of the training data;
  #   2. permute the values of the original feature;
  #   3. get new predictions based on permuted data set;
  #   4. record difference in accuracy.

  vis <- foreach(i = seq_len(nsim), .combine = "cbind") %do.reps% {
    res <- foreach(j = seq_along(feature_names),
                   .combine = "rbind", ...) %do.features% {
      # if (verbose && !parallel) {
      #   message("Computing variable importance for ", x, "...")
      # }
      if (!is.null(sample_size)) {
        ids <- sample(length(train_y), size = sample_size, replace = FALSE)
        train_x <- train_x[ids, ]
        train_y <- train_y[ids]
      }
      permx <- train_x
      permx[, feature_names[j]] <- permx[sample(nrow(permx)), feature_names[j]]
      # train_x_permuted <- permute_columns(train_x, columns = feature_names[j])
      permuted <- metric_fun(
        truth = train_y,
        estimate = pred_wrapper(object, newdata = permx)
      )
      if (smaller_is_better) {
        permuted %compare% baseline  # e.g., RMSE
      } else {
        baseline %compare% permuted  # e.g., R-squared
      }
    }
  }

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
