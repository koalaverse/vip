# Exits
if (!requireNamespace("parsnip", quietly = TRUE)) {
  exit_file("Package parsnip missing")
}

# Load required packages
suppressMessages({
  library(parsnip)
})

# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Fit a linear model
lin <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm") %>%
  parsnip::fit(y ~ ., data = friedman1)

# Compute model-based VI scores
vis <- vi(lin, scale = TRUE)

# Expect `vi()` and `vi_model()` to both work
expect_identical(
  current = vi(lin, sort = FALSE),
  target = vi_model(lin)
)

# Check class
expect_identical(class(vis), target = c("vi", "tbl_df", "tbl", "data.frame"))

# Check dimensions (should be one row for each feature)
expect_identical(ncol(friedman1) - 1L, target = nrow(vis))

# Display VIP
vip(vis, geom = "point")

# Try permutation importance
set.seed(953)  # for reproducibility
p <- vip(
  object = lin,
  method = "permute",
  train = friedman1,
  target = "y",
  pred_wrapper = predict,
  metric = "rmse",
  nsim = 30,
  geom = "violin",
  jitter = TRUE,
  all_permutation = TRUE,
  mapping = aes(color = Variable)
)
expect_true(inherits(p, what = "ggplot"))
p  # display VIP
