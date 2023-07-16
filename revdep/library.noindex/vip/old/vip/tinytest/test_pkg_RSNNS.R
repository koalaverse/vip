# Exits
if (!requireNamespace("RSNNS", quietly = TRUE)) {
  exit_file("Package RSNNS missing")
}
if (!requireNamespace("NeuralNetTools", quietly = TRUE)) {
  exit_file("Package NeuralNetTools missing")
}


# # Load required packages
# suppressMessages({
#   library(RSNNS)
#   library(NeuralNetTools)
# })

# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Fit model(s)
set.seed(101)  # for reproducibility
fit <- RSNNS::mlp(
  x = subset(friedman1, select = -y),
  y = friedman1$y,
  size = 10,
  linOut = TRUE,
  maxit = 1000
)

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

# # Expectations for `get_training_data()`
# expect_identical(
#   current = vip:::get_training_data.default(fit),
#   target = friedman1
# )
#
# # Expectations for `get_feature_names()`
# expect_identical(
#   current = vip:::get_feature_names.default(fit),
#   target = paste0("x", 1L:10L)
# )

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
