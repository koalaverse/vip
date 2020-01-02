# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Tests for package C50
if (require(neuralnet, quietly = TRUE) &&
    require(NeuralNetTools, quietly = TRUE)) {

  # Fit model(s)
  set.seed(101)  # for reproducibility
  fit <- neuralnet::neuralnet(y ~ ., data = friedman1)

  # Compute VI scores
  vis1 <- vi_model(fit)
  vis2 <- vi_model(fit, type = "garson")

  # Expectations for `vi_model()`
  expect_identical(
    current = vis1$Importance,
    target = NeuralNetTools::olden(fit, bar_plot = FALSE)$importance
  )
  expect_identical(
    current = vis2$Importance,
    target = NeuralNetTools::garson(fit, bar_plot = FALSE)$rel_imp
  )

  # Expectations for `get_training_data()`
  expect_identical(
    current = vip:::get_training_data.default(fit),
    target = friedman1
  )

  # Expectations for `get_feature_names()`
  expect_identical(
    current = vip:::get_feature_names.nn(fit),
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
    vip(vis1, include_type = TRUE),
    vip(vis2, include_type = TRUE),
    p,
    nrow = 1
  )

}
