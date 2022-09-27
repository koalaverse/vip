# Generate Friedman benchmark data
friedman1 <-  gen_friedman(seed = 101)
friedman2 <- gen_friedman(seed = 101, n_bins = 2)
friedman3 <- gen_friedman(seed = 101, n_bins = 3)

# List all available metrics
metrics <- list_metrics()
expect_true(inherits(metrics, what = "data.frame"))

# Function to run expectations
expectations <- function(object) {

  # Check class
  expect_identical(class(object),
                   target = c("vi", "tbl_df", "tbl", "data.frame"))

  # Check dimensions (should be one row for each feature)
  expect_identical(ncol(friedman1) - 1L, target = nrow(object))

  # Check top five predictors
  expect_true(all(paste0("x", 1L:5L) %in% object$Variable[1L:5L]))

}


# Regression -------------------------------------------------------------------

# Fit model
fit1 <- stats::lm(y ~ sin(x1*x2) + I((x3 - 0.5) ^ 2) + x4 + x5 +
                    x6 + x7 + x8 + x9 + x10, data = friedman1)
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)
  # predict(object, data = newdata)$predictions  # for ranger models
}

# Try all regression metrics
regression_metrics <- metrics[metrics$Task == "Regression", ]$Metric
set.seed(828)  # for reproducibility
vis <- lapply(regression_metrics, FUN = function(x) {
  vi(fit1, method = "permute", target = "y", metric = x, pred_wrapper = pfun,
     nsim = 10)
})

# Use a custom metric
rsquared <- function(actual, predicted) {
  cor(actual, predicted) ^ 2
}

# Compute permutation-based importance using R-squared (character string)
set.seed(925)  # for reproducibility
vis_rsquared <- vi_permute(
  object = fit1,
  # train = friedman1,
  target = "y",
  metric = "rsquared",
  pred_wrapper = pfun,
  sample_size = 90,
  nsim = 10
)
expectations(vis_rsquared)

# Compute permutation-based importance using R-squared (custim function)
set.seed(925)  # for reproducibility
vis_rsquared_custom <- vi_permute(
  object = fit1,
  train = subset(friedman1, select = -y),
  target = friedman1$y,
  metric = rsquared,
  smaller_is_better = FALSE,
  sample_frac = 0.9,
  pred_wrapper = pfun,
  nsim = 10
)
expectations(vis_rsquared_custom)

# Check that results are identical
expect_identical(vis_rsquared, target = vis_rsquared_custom)

# Expected errors for `vi_permute()`
expect_error(  # missing `pred_wrapper`
  vi_permute(
    object = fit1,
    train = subset(friedman1, select = -y),
    target = friedman1$y,
    metric = rsquared,
    smaller_is_better = FALSE,
    # pred_wrapper = pfun,
    sample_frac = 0.9
  )
)
expect_error(  # missing `target`
  vi_permute(
    object = fit1,
    train = subset(friedman1, select = -y),
    # target = friedman1$y,
    metric = rsquared,
    smaller_is_better = FALSE,
    pred_wrapper = pfun,
    sample_frac = 0.9
  )
)
expect_error(  # missing `smaller_is_better`
  vi_permute(
    object = fit1,
    train = subset(friedman1, select = -y),
    target = friedman1$y,
    metric = rsquared,
    # smaller_is_better = FALSE,
    pred_wrapper = pfun,
    sample_frac = 0.9
  )
)
expect_error(  # trying to set`sample_frac` and `sample_size`
  vi_permute(
    object = fit1,
    train = subset(friedman1, select = -y),
    target = friedman1$y,
    metric = rsquared,
    smaller_is_better = FALSE,
    pred_wrapper = pfun,
    sample_frac = 0.9,
    sample_size = 90
  )
)
expect_error(  # setting `sample_frac` outside of range
  vi_permute(
    object = fit1,
    train = subset(friedman1, select = -y),
    target = friedman1$y,
    metric = rsquared,
    smaller_is_better = FALSE,
    pred_wrapper = pfun,
    sample_frac = 1.9
  )
)


# Classification (binary) ------------------------------------------------------

# Fit model
fit2 <- stats::glm(y ~ sin(x1*x2) + I((x3 - 0.5) ^ 2) + x4 + x5 + x6 + x7 + x8 +
                     x9 + x10, data = friedman2, family = binomial)
pred_prob <- function(object, newdata) {
  # By default, glm() is estimating "class2" probability
  1 - predict(object, newdata = newdata, type = "response")
}
pred_label <- function(object, newdata) {
  p <- pred_prob(object, newdata = newdata)
  ifelse (p > 0.5, yes = "class1", no = "class2")
}

# Compute permutation-based importance using AUC metric
set.seed(850)  # for reproducibility
vis_auc <- vi_permute(
  object = fit2,
  # train = friedman1,
  target = "y",
  metric = "auc",
  pred_wrapper = pred_prob,
  reference_class = "class1"
)
expectations(vis_auc)

# Compute permutation-based importance using logloss metric
set.seed(901)  # for reproducibility
vis_logloss <- vi_permute(
  object = fit2,
  # train = friedman1,
  target = "y",
  metric = "logloss",
  pred_wrapper = pred_prob,
  reference_class = "class1"
)
expectations(vis_logloss)

# Compute permutation-based importance using accuracy metric
set.seed(905)  # for reproducibility
vis_accuracy <- vi_permute(
  object = fit2,
  # train = friedman1,
  target = "y",
  metric = "accuracy",
  pred_wrapper = pred_label,
  reference_class = "class1"
)
expectations(vis_accuracy)

# Compute permutation-based importance using classification error metric
set.seed(907)  # for reproducibility
vis_error <- vi_permute(
  object = fit2,
  # train = friedman1,
  target = "y",
  metric = "error",
  pred_wrapper = pred_label,
  reference_class = "class1"
)
expectations(vis_error)

# Expect error if using AUC (or logLoss) with no reference class
expect_error(
  vi_permute(
    object = fit2,
    train = friedman1,
    target = "y",
    metric = "auc",
    pred_wrapper = pred_prob
    # reference_class = "class1"
  )
)


# Classification (multiclass) --------------------------------------------------

# Exits
if (!requireNamespace("ranger", quietly = TRUE)) {
  exit_file("Package ranger missing")
}

# # Load required packages
# suppressMessages({
#   library(ranger)
# })

# Fit a (default) random forest
set.seed(928)  # for reproducibility
fit3 <- ranger::ranger(y ~ ., data = friedman3, probability = TRUE,
                       importance = "impurity")

# Prediction wrapper
pred_multi_prob <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Compute permutation-based importance using mauc metric
set.seed(932)  # for reproducibility
vis_mauc <- vi_permute(
  object = fit3,
  train = friedman3,
  target = "y",
  metric = "mauc",
  pred_wrapper = pred_multi_prob,
  reference_class = "class1",
  nsim = 10
)
expectations(vis_mauc)
