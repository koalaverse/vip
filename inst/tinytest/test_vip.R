# Load Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Increase length of each feature name
for (i in seq_along(names(friedman1))) {
  if (names(friedman1)[i] != "y") {
    names(friedman1)[i] <- paste0(names(friedman1)[i], "_ABCDEFGH")
  }
}

# Fit an additive linear regression model
fit <- lm(y ~ ., data = friedman1)

# Compute VI scores
vis <- vi(fit, abbreviate_feature_names = 3, rank = TRUE)

# Expectations
expect_error(vi("a"))  # unrecognized model type
expect_true(all(vis$Importance %in% 1L:10L))
expect_true(unique(nchar(vis$Variable)) == 3L)
