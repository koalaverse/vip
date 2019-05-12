context("Model-agnostic variable importance (pdp)")


# Simulate Friedman's data
set.seed(101)  # for reproducibility
friedman1 <- as.data.frame(mlbench::mlbench.friedman1(1000, sd = 0.1))


test_that("`vi_pdp()` works.", {

  # Skips
  # skip_on_cran()

  # Fit model
  fit <- stats::ppr(y ~ ., data = friedman1, nterms = 11)

  # Compute permutation-based importance
  set.seed(101)  # for reproducibility
  vis <- vi_pdp(fit, feature_names = paste0("x.", 1:10), train = friedman1)

  # Expectations
  expect_is(vis, class = c("vi", "tbl_df", "tbl", "data.frame"))
  expect_true(all(names(vis) %in% c("Variable", "Importance", "Sign")))
  expect_identical(ncol(friedman1) - 1L, nrow(vis))

})
