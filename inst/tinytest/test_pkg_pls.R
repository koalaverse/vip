# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Tests for package C50
if (require(pls, quietly = TRUE) && require(caret, quietly = TRUE)) {

  # Fit model(s)
  fit <- pls::mvr(y ~ ., data = friedman1)

  # Compute VI scores
  vis <- vi_model(fit)

  # Expectations for `vi_model()`
  expect_identical(
    current = vis$Importance,
    target = caret::varImp(fit)$Overall
  )

  # Expectations for `get_training_data()`
  expect_identical(
    current = vip:::get_training_data.default(fit),
    target = friedman1
  )

  # Expectations for `get_feature_names()`
  expect_identical(
    current = vip:::get_feature_names.mvr(fit),
    target = paste0("x", 1L:10L)
  )

  # Call `vip::vip()` directly
  p <- vip(fit, method = "model", include_type = TRUE)

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
