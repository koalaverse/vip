# Exits
if (!requireNamespace("partykit", quietly = TRUE)) {
  exit_file("Package partykit missing")
}

# Load required packages
suppressMessages({
  library(partykit)
})

# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Fit model(s)
set.seed(101)
fit1 <- partykit::ctree(y ~ ., data = friedman1)
fit2 <- partykit::cforest(y ~ ., data = friedman1)

# Compute VI scores
set.seed(102)
vis1 <- vi_model(fit1)
set.seed(102)
vis2 <- vi_model(fit2)
set.seed(102)
vis3 <- partykit::varimp(fit1)
set.seed(102)
vis4 <- partykit::varimp(fit2)

# Expectations for `vi_model()`
expect_identical(
  current = vis1$Importance[seq_along(vis3)],
  target = unname(vis3)
)
expect_identical(
  current = vis2$Importance,
  target = unname(vis4)
)

# Expectations for `get_training_data()`
expect_equal(
  current = vip:::get_training_data.default(fit1),
  target = friedman1,
  check.attributes = FALSE
)
expect_equal(
  current = vip:::get_training_data.default(fit2),
  target = friedman1,
  check.attributes = FALSE
)

# Expectations for `get_feature_names()`
expect_identical(
  current = vip:::get_feature_names.constparty(fit1),
  target = paste0("x", 1L:10L)
)
expect_identical(
  current = vip:::get_feature_names.cforest(fit2),
  target = paste0("x", 1L:10L)
)

# Call `vip::vip()` directly
p <- vip(fit2, method = "model", include_type = TRUE)

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
