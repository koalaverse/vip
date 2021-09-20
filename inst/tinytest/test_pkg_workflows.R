# Exits
if (!requireNamespace("parsnip", quietly = TRUE)) {
  exit_file("Package parsnip missing")
}
if (!requireNamespace("workflows", quietly = TRUE)) {
  exit_file("Package workflows missing")
}

# Load required packages
suppressMessages({
  library(parsnip)
  library(workflows)
})

# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Fit a linear model
mod <- parsnip::linear_reg()
wflow <- workflow() %>% add_model(mod) %>% add_formula(y ~ .)

fitted <- generics::fit(wflow, data = friedman1)

# Compute model-based VI scores
vis <- vi(fitted, scale = TRUE)

expect_error(vi(wflow), "The workflow does not have a model fit")

# Expect `vi()` and `vi_model()` to both work
expect_identical(
  current = vi(fitted, sort = FALSE),
  target = vi_model(fitted)
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
  object = fitted,
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
