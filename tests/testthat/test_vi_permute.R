context("Model-agnostic variable importance")


# Function to bin a numeric vector
bin <- function(x, num_bins) {
  quantiles <- quantile(x, probs = seq(from = 0, to = 1, length = num_bins + 1))
  bins <- cut(x, breaks = quantiles, label = FALSE, include.lowest = TRUE)
  as.factor(paste0("class", bins))
}

# Simulate Friedman's data
set.seed(101)  # for reproducibility
friedman1 <- friedman2 <- friedman3 <-
  as.data.frame(mlbench::mlbench.friedman1(1000, sd = 0.1))
friedman2$y <- bin(friedman1$y, num_bins = 2)
friedman3$y <- bin(friedman1$y, num_bins = 3)


# Function: vi_permute() -------------------------------------------------------

test_that("`vi_permute()` works in regression settings.", {

  # Skips
  skip_on_cran()

  # Fit model
  fit <- stats::ppr(y ~ ., data = friedman1, nterms = 11)
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
    object = fit,
    train = friedman1,
    target = "y",
    metric = "rsquared",
    pred_wrapper = pfun
  )
  vis2 <- vi_permute(
    object = fit,
    train = subset(friedman1, select = -y),
    target = friedman1$y,
    metric = rsquared,
    smaller_is_better = FALSE,
    sample_frac = 0.9,
    pred_wrapper = pfun
  )

  # Expectations
  expect_is(vis1, class = c("vi", "tbl_df", "tbl", "data.frame"))
  expect_is(vis2, class = c("vi", "tbl_df", "tbl", "data.frame"))
  expect_true(all(names(vis1) %in% c("Variable", "Importance", "Sign")))
  expect_true(all(names(vis2) %in% c("Variable", "Importance", "Sign")))
  expect_identical(ncol(friedman1) - 1L, nrow(vis1))
  expect_identical(ncol(friedman1) - 1L, nrow(vis2))

})


test_that("`vi_permute()` works in binary classification settings.", {

  # Skips
  skip_on_cran()

  # Fit model
  fit <- ranger::ranger(y ~ ., data = friedman2, num.trees = 50,
                        probability = TRUE)
  pfun <- function(object, newdata) {
    predict(object, data = newdata)$predictions[, "class1", drop = TRUE]
  }

  # Compute permutation-based importance
  set.seed(101)  # for reproducibility
  vis <- vi_permute(
    object = fit,
    train = friedman2,
    target = "y",
    metric = "auc",
    sample_size = 900,
    reference_class = "class1",
    pred_wrapper = pfun
  )

  # Expectations
  expect_is(vis, class = c("vi", "tbl_df", "tbl", "data.frame"))
  expect_true(all(names(vis) %in% c("Variable", "Importance", "Sign")))
  expect_identical(ncol(friedman1) - 1L, nrow(vis))

})
