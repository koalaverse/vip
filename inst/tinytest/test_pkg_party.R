# Generate Friedman benchmark data
friedman2 <- gen_friedman(seed = 101, n_bins = 2)

# Tests for package C50
if (require(party, quietly = TRUE) && require(varImp, quietly = TRUE)) {

  # Fit model(s)
  set.seed(101)
  fit1 <- party::cforest(y ~ ., data = friedman2)
  fit2 <- party::ctree(y ~ ., data = friedman2)

  # Compute VI scores
  set.seed(102)
  vis1 <- vi_model(fit1)
  set.seed(102)
  vis2 <- vi_model(fit1, type = "auc")
  set.seed(102)
  vis3 <- varimp(fit1)
  set.seed(102)
  vis4 <- varimpAUC(fit1)

  # Expectations for `vi_model()`
  expect_identical(
    current = vis1$Importance,
    target = unname(vis3)
  )
  expect_identical(
    current = vis2$Importance,
    target = unname(vis4)
  )

  # Expectations for `get_training_data()`
  expect_equal(
    current = vip:::get_training_data.RandomForest(fit1),
    target = subset(friedman2, select = -y),
    check.attributes = FALSE
  )
  expect_equal(
    current = vip:::get_training_data.BinaryTree(fit2),
    target = subset(friedman2, select = -y),
    check.attributes = FALSE
  )

  # Expectations for `get_feature_names()`
  expect_identical(
    current = vip:::get_feature_names.RandomForest(fit1),
    target = paste0("x", 1L:10L)
  )
  expect_identical(
    current = vip:::get_feature_names.BinaryTree(fit2),
    target = paste0("x", 1L:10L)
  )

  # Call `vip::vip()` directly
  p <- vip(fit1, method = "model", include_type = TRUE)

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
