# Generate Friedman benchmark data
friedman1 <-  gen_friedman(seed = 101)
friedman2 <- gen_friedman(seed = 101, n_bins = 2)

#
# Regression
#

# Fit model
fit1 <- stats::ppr(y ~ ., data = friedman1, nterms = 11)
pfun <- function(object, newdata) {
  predict(object, newdata = newdata)
}

# Metric
rsquared <- function(actual, predicted) {
  cor(actual, predicted) ^ 2
}

# Compute permutation-based importance
set.seed(101)  # for reproducibility
vis1 <- vi_permute(
  object = fit1,
  # train = friedman1,
  target = "y",
  metric = "rsquared",
  pred_wrapper = pfun,
  sample_size = 90,
  nsim = 10
)
vis2 <- vi_permute(
  object = fit1,
  train = subset(friedman1, select = -y),
  target = friedman1$y,
  metric = rsquared,
  smaller_is_better = FALSE,
  sample_frac = 0.9,
  pred_wrapper = pfun
)

# Expectations for `vi_permute()`
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
expect_identical(
  current = class(vis1),
  target = c("vi", "tbl_df", "tbl", "data.frame")
)
expect_identical(
  current = class(vis2),
  target = c("vi", "tbl_df", "tbl", "data.frame")
)
expect_true(all(names(vis1) %in% c("Variable", "Importance", "StDev")))
expect_true(all(names(vis2) %in% c("Variable", "Importance")))
expect_identical(
  current = ncol(friedman1) - 1L,
  target = nrow(vis1)
)
expect_identical(
  current = ncol(friedman1) - 1L,
  target = nrow(vis2)
)


#
# Classification (binary)
#

# Fit model
fit2 <- stats::glm(y ~ ., data = friedman2, family = binomial)
pfun <- function(object, newdata) {
  predict(object, newdata = newdata, type = "response")
}

# Compute permutation-based importance
set.seed(101)  # for reproducibility
vis3 <- vi_permute(
  object = fit2,
  # train = friedman1,
  target = "y",
  metric = "auc",
  pred_wrapper = pfun,
  reference_class = "class1"
)
vis4 <- vi_permute(
  object = fit2,
  # train = friedman1,
  target = "y",
  metric = "auc",
  pred_wrapper = pfun,
  reference_class = "class1"
)

# Expectations for `vi_permute()`
expect_identical(
  current = class(vis3),
  target = c("vi", "tbl_df", "tbl", "data.frame")
)
expect_identical(
  current = class(vis4),
  target = c("vi", "tbl_df", "tbl", "data.frame")
)
expect_true(all(names(vis3) %in% c("Variable", "Importance")))
expect_true(all(names(vis4) %in% c("Variable", "Importance")))
expect_identical(
  current = ncol(friedman1) - 1L,
  target = nrow(vis3)
)
expect_identical(
  current = ncol(friedman1) - 1L,
  target = nrow(vis4)
)
