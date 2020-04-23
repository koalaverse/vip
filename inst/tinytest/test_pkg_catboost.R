# Exits
if (!requireNamespace("catboost", quietly = TRUE)) {
  exit_file("Package catboost missing")
}

# # Load required packages
# suppressMessages({
#   library(catboost)
# })

# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Fit model(s)
set.seed(101)
fit <- catboost::catboost.train(
  learn_pool = catboost::catboost.load_pool(friedman1[,-1], friedman1[,1, drop = TRUE]),
  params = list(logging_level = "Silent", iterations = 10)
)

# Compute VI scores
vis_FeatureImportance_default <- vi_model(fit)
vis_FeatureImportance <- vi_model(fit, type = "FeatureImportance")
vis_PredictionValuesChange <- vi_model(fit, type = "PredictionValuesChange")
vis_LossFunctionChange <- vi_model(fit, type = "LossFunctionChange", pool = catboost::catboost.load_pool(friedman1[,-1], friedman1[,1, drop = TRUE]))
vis_Interaction <- vi_model(fit, type = "Interaction")

# Expectations for `vi_model()`
expect_identical()
expect_identical()
expect_identical()

# Expectations for `get_training_data()`
expect_error(vip:::get_training_data.default(fit))

# Expectations for `get_feature_names()`
expect_identical(
  current = vip:::get_feature_names.catboost.Model(fit),
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
  vip(vis_FeatureImportance_default, include_type = TRUE),
  vip(vis_FeatureImportance, include_type = TRUE),
  vip(vis_PredictionValuesChange, include_type = TRUE),
  vip(vis_LossFunctionChange, include_type = TRUE),
  # vip(vis_Interaction, include_type = TRUE),
  p,
  nrow = 1
)
