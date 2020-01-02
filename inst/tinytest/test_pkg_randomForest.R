# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)
friedman2 <- gen_friedman(seed = 101, n_bins = 2)

# Tests for package randomForest
if (require(randomForest, quietly = TRUE)) {

  # Fit model(s)
  set.seed(101)
  fit1 <- randomForest::randomForest(y ~ ., data = friedman1)
  fit2 <- randomForest::randomForest(y ~ ., data = friedman1, importance = TRUE)
  fit3 <- randomForest::randomForest(
    x = subset(friedman1, select = -y),
    y = friedman1$y
  )
  fit4 <- randomForest::randomForest(y ~ ., data = friedman2, importance = TRUE)

  # Compute VI scores
  vis1 <- vi_model(fit1)
  vis2 <- vi_model(fit2, type = 1)
  vis3 <- vi_model(fit3)
  vis4 <- vi_model(fit4, type = 2, scale = FALSE)


  # Expectations for `vi_model()`
  expect_error(vi_model(fit1, type = 1))
  expect_identical(
    current = vis1$Importance,
    target = unname(fit1$importance[, "IncNodePurity"])
  )
  expect_identical(
    current = vi_model(fit2, type = 1, scale = FALSE)$Importance,
    target = unname(fit2$importance[, "%IncMSE", drop = TRUE])
  )
  expect_identical(
    current = vis3$Importance,
    target = unname(fit3$importance[, "IncNodePurity"])
  )
  expect_identical(
    current = vis4$Importance,
    target = unname(fit4$importance[, "MeanDecreaseGini", drop = TRUE])
  )

  # Expectations for `get_training_data()`
  expect_identical(
    current = vip:::get_training_data.randomForest(fit1),
    target = friedman1
  )
  expect_identical(  # NOTE: Only x is passed in this call
    current = vip:::get_training_data.randomForest(fit3),
    target = subset(friedman1, select = -y)
  )

  # Expectations for `get_feature_names()`
  expect_identical(
    current = vip:::get_feature_names.randomForest(fit1),
    target = paste0("x", 1L:10L)
  )
  expect_identical(
    current = vip:::get_feature_names.randomForest(fit3),
    target = paste0("x", 1L:10L)
  )

  # Call `vip::vip()` directly
  p <- vip(fit4, method = "model", type = 1, include_type = TRUE)

  # Expect `p` to be a `"gg" "ggplot"` object
  expect_identical(
    current = class(p),
    target = c("gg", "ggplot")
  )

  # Display VIPs side by side
  grid.arrange(
    vip(vis1, include_type = TRUE),
    vip(vis2, include_type = TRUE),
    vip(vis4, include_type = TRUE),
    p,
    nrow = 2
  )

}
