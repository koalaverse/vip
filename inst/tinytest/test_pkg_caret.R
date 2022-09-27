# Exits
if (!requireNamespace("caret", quietly = TRUE)) {
  exit_file("Package 'caret' missing")
}

# # Load required packages
# suppressMessages({
#   library(caret)
# })

# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Fit model(s)
fit <- caret::train(y ~ ., friedman1, method = "lm")

# Compute VI scores
vis1 <- vi_model(fit)
vis2 <- caret::varImp(fit)

# Expectations for `vi_model()`
expect_identical(
  current = vis1$Importance,
  target = vis2$importance[vis1$Variable, , drop = TRUE]
)

# Expectations for `get_training_data()`
expect_identical(
  current = vip:::get_training_data.train(fit),
  target = subset(friedman1, select = -y)
)

# Expectations for `get_feature_names()`
expect_identical(
  current = vip:::get_feature_names.train(fit),
  target = paste0("x", 1L:10L)
)
