# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Tests for package rpart
if (require(rpart, quietly = TRUE)) {

  # Fit model(s)
  fit <- rpart::rpart(y ~ ., data = friedman1)

  # Compute VI scores
  vis <- vi_model(fit)

  # Expectations for `vi_model()`
  expect_identical(
    current = vis$Importance,
    target = unname(fit$variable.importance)
  )

  # Expectations for `get_training_data()`
  expect_identical(
    current = vip:::get_training_data.default(fit),
    target = friedman1
  )

  # Expectations for `get_feature_names()`
  expect_identical(
    current = vip:::get_feature_names.rpart(fit),
    target = paste0("x", 1L:10L)
  )

  # Call `vip::vip()` directly
  p <- vip(fit, method = "model", type = 1, include_type = TRUE)

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
