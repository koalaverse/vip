# Exits
if (!requireNamespace("mlr3", quietly = TRUE)) {
  exit_file("Package mlr3 missing")
}
if (!requireNamespace("mlr3learners", quietly = TRUE)) {
  exit_file("Package mlr3learners missing")
}
if (!requireNamespace("ranger", quietly = TRUE)) {
  exit_file("Package ranger missing")
}

# Load required packages
suppressMessages({
  library(mlr3)
  library(mlr3learners)
  library(ranger)
})

# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Fit model(s)
set.seed(101)
task <- TaskRegr$new("friedman", backend = friedman1, target = "y")
lrnr <- lrn("regr.ranger", importance = "impurity")
expect_error(vi(lrnr))  # did not call `$traini()` yet
expect_error(vi_model(lrnr))  # did not call `$traini()` yet
lrnr$train(task)  # `vi()`, etc. should now work

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
