# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Tests for package ranger
if (require(mlr3, quietly = TRUE) && require(mlr3learners, quietly = TRUE) &&
    require(ranger, quietly = TRUE)) {

  # Fit model(s)
  set.seed(101)
  task <- TaskRegr$new("friedman", backend = friedman1, target = "y")
  lrnr <- lrn("regr.ranger", importance = "impurity")
  lrnr$train(task)

  # Compute model-based VI scores
  vis <- vi_model(lrnr)

  # Expect `vi()` and `vi_model()` to both work
  expect_identical(
    current = vi(lrnr, sort = FALSE),
    target = vi_model(lrnr)
  )

  # Expectations for `vi_model()`
  expect_identical(
    current = vis$Importance,
    target = unname(lrnr$model$variable.importance)
  )

  # Expectations for `get_feature_names()`
  expect_identical(
    current = sort(vip:::get_feature_names.Learner(lrnr)),
    target = sort(paste0("x", 1L:10L))
  )

  # Call `vip::vip()` directly
  p <- vip(lrnr, method = "model", include_type = TRUE)

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
