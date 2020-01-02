# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)
friedman3 <- gen_friedman(seed = 101, n_bins = 3)

# Tests for package ranger
if (require(ranger, quietly = TRUE)) {

  # Fit model(s)
  set.seed(101)
  fit1 <- ranger::ranger(y ~ ., data = friedman1)
  fit2 <- ranger::ranger(y ~ ., data = friedman1, importance = "impurity")

  # Compute model-based VI scores
  vis <- vi_model(fit2)

  # Expectations for `vi_model()`
  expect_error(vi_model(fit1))
  expect_identical(
    current = vis$Importance,
    target = unname(fit2$variable.importance)
  )

  # Expectations for `get_training_data()`
  expect_identical(
    current = vip:::get_training_data.default(fit1),
    target = friedman1
  )

  # Expectations for `get_feature_names()`
  expect_identical(
    current = vip:::get_feature_names.ranger(fit1),
    target = paste0("x", 1L:10L)
  )

  # Call `vip::vip()` directly
  p <- vip(fit2, method = "model", include_type = TRUE)

  # Expect `p` to be a `"gg" "ggplot"` object
  expect_identical(
    current = class(p),
    target = c("gg", "ggplot")
  )

  # Display VIPs side by side
  grid.arrange(
    vip(vis, include_type = TRUE),
    p,
    nrow = 1
  )

}
